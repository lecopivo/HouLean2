import Lean

namespace HouLean.Meta

open Lean

/-! # Timing Monad Transformer

A monad transformer for accumulating timing measurements across multiple
operations. Useful for profiling compilation passes and tracking performance
of individual components.

## Usage

```lean
def myCompilation : TimeItT CoreM Unit := do
  withTimeIt `parsing do
    -- parsing code
  withTimeIt `elaboration do
    -- elaboration code
  let (result, state) ← TimeItT.run myCompilation
  state.report
```

Multiple calls to the same timer name accumulate their times.
-/

/-- State for accumulating timing measurements.
    Maps timer names to their accumulated time in nanoseconds,
    and tracks which timers are currently running. -/
structure TimeItState where
  /-- Map from timer names to accumulated time in nanoseconds -/
  times : NameMap Nat := {}
  /-- Map from timer names to their start time (if currently running) -/
  running : NameMap Nat := {}
  deriving Inhabited

namespace TimeItState

/-- Format a nanosecond duration in human-readable units.
    Automatically selects ns, μs, ms, or s based on magnitude. -/
def formatNanos (nanos : Nat) : String :=
  let t := nanos.toFloat
  if t < 1e3 then s!"{t}ns"
  else if t < 1e6 then s!"{t / 1e3}μs"
  else if t < 1e9 then s!"{t / 1e6}ms"
  else s!"{t / 1e9}s"

/-- Report all accumulated timings to the info log. -/
def report (s : TimeItState) : CoreM Unit := do
  for (name, nanos) in s.times do
    logInfo m!"{name}: {formatNanos nanos}"

/-- Get the accumulated time for a specific timer in nanoseconds.
    Returns 0 if the timer has not been used. -/
def getTime (s : TimeItState) (name : Name) : Nat :=
  s.times.find? name |>.getD 0

/-- Add elapsed nanoseconds to a named timer.
    If the timer exists, the time is added to the existing total. -/
def addTime (s : TimeItState) (name : Name) (nanos : Nat) : TimeItState :=
  { s with times := s.times.alter name fun
    | some t => some (t + nanos)
    | none => some nanos }

/-- Check if a timer is currently running. -/
def isRunning (s : TimeItState) (name : Name) : Bool :=
  s.running.contains name

/-- Mark a timer as running with the given start time.
    Returns none if the timer is already running. -/
def startTimer (s : TimeItState) (name : Name) (startTime : Nat) : Option TimeItState :=
  if s.isRunning name then none
  else some { s with running := s.running.insert name startTime }

/-- Stop a running timer and accumulate elapsed time.
    Returns none if the timer is not running. -/
def stopTimer (s : TimeItState) (name : Name) (stopTime : Nat) : Option TimeItState :=
  match s.running.find? name with
  | none => none
  | some startTime =>
    let elapsed := stopTime - startTime
    some { s with
      running := s.running.erase name
      times := s.times.alter name fun
        | some t => some (t + elapsed)
        | none => some elapsed }

/-- Reset all timing data. -/
def reset (_s : TimeItState) : TimeItState := {}

/-- Merge two timing states by summing times for each timer. -/
def merge (s₁ s₂ : TimeItState) : TimeItState :=
  s₂.times.foldl (init := s₁) fun acc name nanos =>
    acc.addTime name nanos

instance : Append TimeItState where
  append := merge

end TimeItState

/-- Monad transformer for accumulating timing measurements.
    Wraps an underlying monad `m` with timing state. -/
abbrev TimeItT (m : Type → Type) := StateT TimeItState m

/-- Typeclass for monads that support timing operations.
    Provides an interface for recording and retrieving timing data. -/
class MonadTimeIt (m : Type → Type) where
  /-- Start a named timer. Throws if the timer is already running. -/
  startTimer (timerName : Name) (startTime : Nat) : m Unit
  /-- Stop a named timer and accumulate elapsed time. Throws if not running. -/
  stopTimer (timerName : Name) (stopTime : Nat) : m Unit
  /-- Retrieve all accumulated timing data as an array of (name, nanos) pairs. -/
  getTimes : m (Array (Name × Nat))

export MonadTimeIt (startTimer stopTimer getTimes)

namespace TimeItT

variable {m : Type → Type}

/-- Run a timed computation, returning the result paired with timing state. -/
def run [Monad m] (x : TimeItT m α) (s : TimeItState := {}) : m (α × TimeItState) :=
  StateT.run x s

/-- Run a timed computation, discarding the timing state. -/
def run' [Monad m] (x : TimeItT m α) (s : TimeItState := {}) : m α :=
  StateT.run' x s

/-- Run a timed computation and report results, returning only the computation result. -/
def runAndReport [Monad m] [MonadLiftT CoreM m] (x : TimeItT m α) : m α := do
  let (result, state) ← run x
  state.report
  return result

end TimeItT

/-! ## Instances -/

instance {m} [Monad m] [MonadError m] : MonadError (TimeItT m) where
  add s m st := do
    let r ← AddErrorMessageContext.add s m
    return (r, st)

/-- Base `MonadTimeIt` instance for `TimeItT`. -/
instance [Monad m] [MonadError m] : MonadTimeIt (TimeItT m) where
  startTimer name startTime := do
    let s ← get
    match s.startTimer name startTime with
    | some s' => set s'
    | none => throwError "Timer '{name}' is already running"
  stopTimer name stopTime := do
    let s ← get
    match s.stopTimer name stopTime with
    | some s' => set s'
    | none => throwError "Timer '{name}' is not running"
  getTimes := return (← get).times.toArray

/-- Lift `MonadTimeIt` through `ReaderT`. -/
instance [MonadTimeIt m] [Monad m] : MonadTimeIt (ReaderT ρ m) where
  startTimer name startTime := fun _ => MonadTimeIt.startTimer name startTime
  stopTimer name stopTime := fun _ => MonadTimeIt.stopTimer name stopTime
  getTimes := fun _ => MonadTimeIt.getTimes

/-- Lift `MonadTimeIt` through `StateT`. -/
instance [MonadTimeIt m] [Monad m] : MonadTimeIt (StateT σ m) where
  startTimer name startTime := fun s => do
    MonadTimeIt.startTimer name startTime
    return ((), s)
  stopTimer name stopTime := fun s => do
    MonadTimeIt.stopTimer name stopTime
    return ((), s)
  getTimes := fun s => do
    let times ← MonadTimeIt.getTimes
    return (times, s)

/-- Lift `MonadTimeIt` through `ExceptT`. -/
instance [MonadTimeIt m] [Monad m] : MonadTimeIt (ExceptT ε m) where
  startTimer name startTime := ExceptT.lift (MonadTimeIt.startTimer name startTime)
  stopTimer name stopTime := ExceptT.lift (MonadTimeIt.stopTimer name stopTime)
  getTimes := ExceptT.lift MonadTimeIt.getTimes

/-! ## Timing Combinators -/

/-- Time a computation, accumulating elapsed time under `timerName`.
    Multiple calls with the same name will sum their times.
    Throws an error if the timer is already running (prevents double-counting).

    Example:
    ```lean
    def process : TimeItT IO Unit := do
      withTimeIt `phase1 do
        -- first phase
      withTimeIt `phase2 do
        -- second phase
      withTimeIt `phase1 do
        -- more phase1 work (adds to existing `phase1` time)
    ```
-/
def withTimeIt [Monad m] [MonadTimeIt m] [MonadLiftT BaseIO m]
    (timerName : Name) (x : m α) : m α := do
  let start ← IO.monoNanosNow
  startTimer timerName start
  let result ← x
  let stop ← IO.monoNanosNow
  stopTimer timerName stop
  return result

/-- Report all accumulated timings to the info log. -/
def reportTimes [Monad m] [MonadTimeIt m] [MonadLiftT CoreM m] : m Unit := do
  for (name, nanos) in (← getTimes) do
    (logInfo m!"{name}: {TimeItState.formatNanos nanos}" : CoreM Unit)

end HouLean.Meta
