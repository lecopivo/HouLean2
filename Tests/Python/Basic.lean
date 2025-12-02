import HouLean.Data.Vector
import HouLean.Python.Translation

open HouLean Math

set_option pp.funBinderTypes true

def t1 (v : Float32) : Float32 := show Id _ from python%
  x = sin(v)
  return x + v

#check show Id _ from python%
  def foo(v : Float32):
    x = sin(v)
    return x + v
  return foo(10)

#check python%
  def foo{t : Type, dim : Nat}[Add t](v : Vector t dim)
    return  v + v
  return foo(
