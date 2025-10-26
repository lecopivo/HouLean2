import HouLean.Vex.VexFunction

open VEX

/-!
# Houdini VEX Attribute Functions

This file contains comprehensive bindings for Houdini's VEX attribute manipulation functions.
These functions allow you to create, read, write, query, and manipulate geometry attributes
in various contexts (detail/global, point, primitive, vertex).

## Attribute Classes

Houdini geometry has four classes of attributes:
- **Detail**: Global attributes with one value for the entire geometry
- **Point**: Attributes stored per point
- **Primitive**: Attributes stored per primitive (face, curve, etc.)
- **Vertex**: Attributes stored per vertex (primitive-point pair)

## Supported Types

VEX attributes support the following data types:
- `int`: Integer values
- `float`: Floating-point scalar values
- `vector`: 3D vectors (position, direction, normal, RGB color)
- `vector4`: 4D vectors (RGBA color, quaternions, homogeneous coordinates)
- `string`: String values
- `matrix`: 4x4 transformation matrices

## Common Patterns

### Creating and Writing Attributes
```lean
-- Create a point attribute
addpointattrib(geoself(), "myattr", 0.0)
-- Write to it
setpointattrib(geoself(), "myattr", @ptnum, 42.0)
```

### Reading Attributes
```lean
-- Read from current geometry
let val := point(0, "P", @ptnum)
-- Read from input
let val := point(@OpInput1, "myattr", ptnum)
```

### Checking Existence
```lean
if haspointattrib(0, "Cd") then
  let color := point(0, "Cd", @ptnum)
```
-/

-- ============================================================================
-- Creating Attributes
-- ============================================================================

/-- Adds an attribute to a geometry.

Creates a new attribute of the specified class and type. This is the generic
attribute creation function that works with any attribute class.

**Parameters:**
- `geohandle`: Geometry handle to write to. Use `0` or `geoself()`.
- `attribclass`: Attribute class: `"detail"`, `"primitive"`, `"point"`, or `"vertex"`.
- `name`: Name of the attribute to create.
- `defvalue`: Default value determining the attribute type.

**Returns:** `1` on success, `0` if attribute exists or creation failed.

**Example:**
```lean
-- Create a float point attribute
addattrib(geoself(), "point", "temperature", 0.0)
-- Create a vector primitive attribute
addattrib(geoself(), "primitive", "velocity", {0,0,0})
```
-/
vexfunction int addattrib(int geohandle; string attribclass; string name; int defvalue) { return 0; }
vexfunction int addattrib(int geohandle; string attribclass; string name; float defvalue) { return 0; }
vexfunction int addattrib(int geohandle; string attribclass; string name; vector defvalue) { return 0; }
vexfunction int addattrib(int geohandle; string attribclass; string name; vector4 defvalue) { return 0; }
vexfunction int addattrib(int geohandle; string attribclass; string name; string defvalue) { return 0; }
vexfunction int addattrib(int geohandle; string attribclass; string name; matrix defvalue) { return 0; }

/-- Adds a detail (global) attribute to a geometry.

Creates a new detail attribute. Detail attributes have exactly one value for
the entire geometry, useful for storing metadata or global parameters.

**Parameters:**
- `geohandle`: Geometry handle to write to. Use `0` or `geoself()`.
- `name`: Name of the detail attribute to create.
- `defvalue`: Default value determining the attribute type.

**Returns:** `1` on success, `0` if attribute exists or creation failed.

**Example:**
```lean
-- Store a version number
adddetailattrib(geoself(), "version", 1)
-- Store a description
adddetailattrib(geoself(), "description", "Generated mesh")
```
-/
vexfunction int adddetailattrib(int geohandle; string name; int defvalue) { return 0; }
vexfunction int adddetailattrib(int geohandle; string name; float defvalue) { return 0; }
vexfunction int adddetailattrib(int geohandle; string name; vector defvalue) { return 0; }
vexfunction int adddetailattrib(int geohandle; string name; vector4 defvalue) { return 0; }
vexfunction int adddetailattrib(int geohandle; string name; string defvalue) { return 0; }

/-- Adds a point attribute to a geometry.

Creates a new point attribute. Point attributes store one value per point,
the most common type of attribute for storing per-point data.

**Parameters:**
- `geohandle`: Geometry handle to write to. Use `0` or `geoself()`.
- `name`: Name of the point attribute to create.
- `defvalue`: Default value applied to all points, determines attribute type.

**Returns:** `1` on success, `0` if attribute exists or creation failed.

**Example:**
```lean
-- Create standard attributes
addpointattrib(geoself(), "Cd", {1,1,1})  -- Color
addpointattrib(geoself(), "pscale", 1.0)  -- Point scale
addpointattrib(geoself(), "id", 0)        -- Point ID
```
-/
vexfunction int addpointattrib(int geohandle; string name; int defvalue) { return 0; }
vexfunction int addpointattrib(int geohandle; string name; float defvalue) { return 0; }
vexfunction int addpointattrib(int geohandle; string name; vector defvalue) { return 0; }
vexfunction int addpointattrib(int geohandle; string name; vector4 defvalue) { return 0; }
vexfunction int addpointattrib(int geohandle; string name; string defvalue) { return 0; }
vexfunction int addpointattrib(int geohandle; string name; matrix defvalue) { return 0; }

/-- Adds a primitive attribute to a geometry.

Creates a new primitive attribute. Primitive attributes store one value per
primitive (face, curve, sphere, etc.), useful for per-face data.

**Parameters:**
- `geohandle`: Geometry handle to write to. Use `0` or `geoself()`.
- `name`: Name of the primitive attribute to create.
- `defvalue`: Default value applied to all primitives, determines attribute type.

**Returns:** `1` on success, `0` if attribute exists or creation failed.

**Example:**
```lean
-- Store material assignment per primitive
addprimattrib(geoself(), "shop_materialpath", "")
-- Store primitive IDs
addprimattrib(geoself(), "primid", 0)
```
-/
vexfunction int addprimattrib(int geohandle; string name; int defvalue) { return 0; }
vexfunction int addprimattrib(int geohandle; string name; float defvalue) { return 0; }
vexfunction int addprimattrib(int geohandle; string name; vector defvalue) { return 0; }
vexfunction int addprimattrib(int geohandle; string name; vector4 defvalue) { return 0; }
vexfunction int addprimattrib(int geohandle; string name; string defvalue) { return 0; }
vexfunction int addprimattrib(int geohandle; string name; matrix defvalue) { return 0; }

/-- Adds a vertex attribute to a geometry.

Creates a new vertex attribute. Vertex attributes store one value per vertex
(primitive-point pair), useful for per-corner data like UVs.

**Parameters:**
- `geohandle`: Geometry handle to write to. Use `0` or `geoself()`.
- `name`: Name of the vertex attribute to create.
- `defvalue`: Default value applied to all vertices, determines attribute type.

**Returns:** `1` on success, `0` if attribute exists or creation failed.

**Example:**
```lean
-- Create UV coordinates (typically stored per-vertex)
addvertexattrib(geoself(), "uv", {0,0,0})
-- Create vertex normals
addvertexattrib(geoself(), "N", {0,1,0})
```
-/
vexfunction int addvertexattrib(int geohandle; string name; int defvalue) { return 0; }
vexfunction int addvertexattrib(int geohandle; string name; float defvalue) { return 0; }
vexfunction int addvertexattrib(int geohandle; string name; vector defvalue) { return 0; }
vexfunction int addvertexattrib(int geohandle; string name; vector4 defvalue) { return 0; }
vexfunction int addvertexattrib(int geohandle; string name; string defvalue) { return 0; }

/-- Appends to a geometry's visualizer detail attribute.

Adds visualization markers to the geometry for debugging purposes. These appear
as overlays in the Houdini viewport.

**Parameters:**
- `geohandle`: Geometry handle to write to. Use `0` or `geoself()`.
- `visualizer_type`: Type of visualizer: `"marker"`, `"ray"`, `"number"`, etc.
- `attribname`: Attribute name to visualize, or empty string for position-based.
- `params`: Dictionary of visualizer-specific parameters.

**Returns:** `1` on success, `0` on failure.

**Example:**
```lean
-- Add markers at point positions
addvisualizer(geoself(), "marker", "", empty_dict)
```
-/
vexfunction int addvisualizer(int geohandle; string visualizer_type; string attribname; int params) { return 0; }

-- ============================================================================
-- Reading Attributes (Generic)
-- ============================================================================

/-- Reads the value of an attribute from geometry.

Generic function to read any attribute from any geometry element. Use the
specialized functions (`point`, `prim`, etc.) when you know the attribute class
for better performance and clarity.

**Parameters:**
- `opinput`: Input number (`0`-`3`) to read from.
- `attribclass`: Attribute class: `"detail"`, `"primitive"`, `"point"`, or `"vertex"`.
- `attribname`: Name of the attribute to read.
- `elemnum`: Element number. For detail use `0`, for others use point/prim/vertex number.
- `vtxnum`: For vertex attributes, the vertex index within primitive. Use `0` for other classes.

**Returns:** Attribute value, or `0`/empty if attribute doesn't exist or element is invalid.

**Example:**
```lean
-- Read a point attribute generically
let val := attrib(0, "point", "Cd", @ptnum, 0)
-- Better: use specific function
let val := point(0, "Cd", @ptnum)
```
-/
vexfunction int attrib(int opinput; string attribclass; string attribname; int elemnum; int vtxnum) { return 0; }
vexfunction float attrib(int opinput; string attribclass; string attribname; int elemnum; int vtxnum) { return 0.0; }
vexfunction vector attrib(int opinput; string attribclass; string attribname; int elemnum; int vtxnum) { return {0,0,0}; }
vexfunction vector4 attrib(int opinput; string attribclass; string attribname; int elemnum; int vtxnum) { return {0,0,0,0}; }
vexfunction string attrib(int opinput; string attribclass; string attribname; int elemnum; int vtxnum) { return ""; }
vexfunction matrix attrib(int opinput; string attribclass; string attribname; int elemnum; int vtxnum) { return {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}; }

/-- Returns the class of a geometry attribute.

Determines which class (detail, primitive, point, or vertex) an attribute belongs to.

**Parameters:**
- `opinput`: Input number (`0`-`3`) to read from.
- `attribname`: Name of the attribute to query.

**Returns:** `"detail"`, `"primitive"`, `"point"`, `"vertex"`, or empty string if not found.

**Example:**
```lean
let class := attribclass(0, "Cd")
-- Typically returns "point" for color
```
-/
vexfunction string attribclass(int opinput; string attribname) { return ""; }

/-- Returns the data id of a geometry attribute.

Returns a unique identifier that changes whenever the attribute data is modified.
Useful for caching and detecting when geometry has changed.

**Parameters:**
- `opinput`: Input number (`0`-`3`) to read from.
- `attribclass`: Attribute class.
- `attribname`: Name of the attribute.

**Returns:** Integer data version ID, or `-1` if attribute doesn't exist.
-/
vexfunction int attribdataid(int opinput; string attribclass; string attribname) { return 0; }

/-- Returns the size of a geometry attribute.

Returns the number of components in the attribute (e.g., 1 for float, 3 for vector).

**Parameters:**
- `opinput`: Input number (`0`-`3`) to read from.
- `attribclass`: Attribute class.
- `attribname`: Name of the attribute.

**Returns:** Number of components, or `0` if attribute doesn't exist.

**Example:**
```lean
let size := attribsize(0, "point", "P")  -- Returns 3 for position
let size := attribsize(0, "point", "Cd") -- Returns 3 for RGB color
```
-/
vexfunction int attribsize(int opinput; string attribclass; string attribname) { return 0; }

/-- Returns the type of a geometry attribute.

Returns an integer code representing the storage type of the attribute.

**Parameters:**
- `opinput`: Input number (`0`-`3`) to read from.
- `attribclass`: Attribute class.
- `attribname`: Name of the attribute.

**Returns:** Type code: `0` for int, `1` for float, `2` for string, or `-1` if not found.
-/
vexfunction int attribtype(int opinput; string attribclass; string attribname) { return 0; }

/-- Returns the transformation metadata of a geometry attribute.

Returns type information describing how the attribute should transform with geometry
(e.g., as a point, vector, normal, or color).

**Parameters:**
- `opinput`: Input number (`0`-`3`) to read from.
- `attribclass`: Attribute class.
- `attribname`: Name of the attribute.

**Returns:** Type info: `"point"`, `"vector"`, `"normal"`, `"color"`, `"quaternion"`,
           `"matrix3"`, `"matrix"`, `"none"`, or empty string if not found.

**Example:**
```lean
-- Check how an attribute transforms
let typeinfo := attribtypeinfo(0, "point", "N")  -- Returns "normal"
let typeinfo := attribtypeinfo(0, "point", "v")  -- Returns "vector"
```
-/
vexfunction string attribtypeinfo(int opinput; string attribclass; string attribname) { return ""; }

-- ============================================================================
-- Reading Detail Attributes
-- ============================================================================

/-- Reads the value of a detail attribute from geometry.

Reads a global (detail) attribute value. Detail attributes have exactly one value
for the entire geometry.

**Parameters:**
- `opinput`: Input number (`0`-`3`) to read from.
- `attribname`: Name of the detail attribute to read.
- `ignored`: Backwards compatibility parameter, always use `0`.

**Returns:** Detail attribute value, or `0`/empty if attribute doesn't exist.

**Example:**
```lean
-- Read global metadata
let version := detail(0, "version", 0)
let description := detail(0, "description", 0)
```
-/
vexfunction int detail(int opinput; string attribname; int ignored) { return 0; }
vexfunction float detail(int opinput; string attribname; int ignored) { return 0.0; }
vexfunction vector detail(int opinput; string attribname; int ignored) { return {0,0,0}; }
vexfunction vector4 detail(int opinput; string attribname; int ignored) { return {0,0,0,0}; }
vexfunction string detail(int opinput; string attribname; int ignored) { return ""; }

/-- Reads a detail attribute value from geometry.

Alternative function name for reading detail attributes. Functionally identical to `detail()`.

**Parameters:**
- `opinput`: Input number (`0`-`3`) to read from.
- `attribname`: Name of the detail attribute to read.
- `ignored`: Ignored parameter, use `0`.

**Returns:** Detail attribute value, or `0`/empty if attribute doesn't exist.
-/
vexfunction int detailattrib(int opinput; string attribname; int ignored) { return 0; }
vexfunction float detailattrib(int opinput; string attribname; int ignored) { return 0.0; }
vexfunction vector detailattrib(int opinput; string attribname; int ignored) { return {0,0,0}; }
vexfunction vector4 detailattrib(int opinput; string attribname; int ignored) { return {0,0,0,0}; }
vexfunction string detailattrib(int opinput; string attribname; int ignored) { return ""; }

/-- Returns the size of a geometry detail attribute.

Returns the number of components in a detail attribute.

**Parameters:**
- `opinput`: Input number (`0`-`3`) to read from.
- `attribname`: Name of the detail attribute.

**Returns:** Number of components (e.g., `1` for float, `3` for vector), or `0` if not found.
-/
vexfunction int detailattribsize(int opinput; string attribname) { return 0; }

/-- Returns the type of a geometry detail attribute.

Returns an integer code representing the storage type.

**Parameters:**
- `opinput`: Input number (`0`-`3`) to read from.
- `attribname`: Name of the detail attribute.

**Returns:** Integer type code, or `-1` if attribute doesn't exist.
-/
vexfunction int detailattribtype(int opinput; string attribname) { return 0; }

/-- Returns the type info of a geometry detail attribute.

Returns transformation metadata for the attribute.

**Parameters:**
- `opinput`: Input number (`0`-`3`) to read from.
- `attribname`: Name of the detail attribute.

**Returns:** Type info string, or empty string if attribute doesn't exist.
-/
vexfunction string detailattribtypeinfo(int opinput; string attribname) { return ""; }

/-- Reads the value of a detail intrinsic from geometry.

Detail intrinsics are built-in read-only properties of the geometry (e.g., `"bounds"`,
`"pointcount"`, `"primitivecount"`). These provide metadata about the geometry itself.

**Parameters:**
- `opinput`: Input number (`0`-`3`) to read from.
- `intrinsicname`: Name of the intrinsic (e.g., `"bounds"`, `"pointcount"`).

**Returns:** Intrinsic value, or `0`/empty if intrinsic doesn't exist.

**Example:**
```lean
-- Get geometry statistics
let npts := detailintrinsic(0, "pointcount")
let nprims := detailintrinsic(0, "primitivecount")
let bounds := detailintrinsic(0, "bounds")  -- Returns bounding box
```
-/
vexfunction int detailintrinsic(int opinput; string intrinsicname) { return 0; }
vexfunction float detailintrinsic(int opinput; string intrinsicname) { return 0.0; }
vexfunction vector detailintrinsic(int opinput; string intrinsicname) { return {0,0,0}; }
vexfunction vector4 detailintrinsic(int opinput; string intrinsicname) { return {0,0,0,0}; }
vexfunction string detailintrinsic(int opinput; string intrinsicname) { return ""; }
vexfunction int[] detailintrinsic(int opinput; string intrinsicname) { return #[]; }
vexfunction float[] detailintrinsic(int opinput; string intrinsicname) { return #[]; }
vexfunction vector[] detailintrinsic(int opinput; string intrinsicname) { return #[]; }
vexfunction string[] detailintrinsic(int opinput; string intrinsicname) { return #[]; }

-- ============================================================================
-- Reading Point Attributes
-- ============================================================================

/-- Reads a point attribute value from geometry.

Reads the value of an attribute at a specific point. This is the most commonly
used attribute reading function in point-based VEX contexts.

**Parameters:**
- `opinput`: Input number (`0`-`3`) to read from.
- `attribname`: Name of the point attribute to read.
- `pointnum`: Point number to read the attribute from.

**Returns:** Attribute value at the specified point, or `0`/empty if attribute
           doesn't exist or point is invalid.

**Example:**
```lean
-- Read position of a point
let pos := point(0, "P", ptnum)
-- Read color
let color := point(0, "Cd", ptnum)
-- Read neighbor's attribute
let neighbor_id := point(0, "id", neighbor_ptnum)
```
-/
vexfunction int point(int opinput; string attribname; int pointnum) { return 0; }
vexfunction float point(int opinput; string attribname; int pointnum) { return 0.0; }
vexfunction vector point(int opinput; string attribname; int pointnum) { return {0,0,0}; }
vexfunction vector4 point(int opinput; string attribname; int pointnum) { return {0,0,0,0}; }
vexfunction string point(int opinput; string attribname; int pointnum) { return ""; }
vexfunction matrix point(int opinput; string attribname; int pointnum) { return {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}; }

/-- Reads a point attribute value and outputs a success/fail flag.

Reads a point attribute and provides information about whether the read was successful.
Useful when you need to distinguish between a missing attribute and a zero value.

**Parameters:**
- `opinput`: Input number (`0`-`3`) to read from.
- `attribname`: Name of the point attribute to read.
- `pointnum`: Point number to read from.
- `success`: Output parameter - set to `1` if successful, `0` otherwise.

**Returns:** Attribute value at the specified point, or `0`/empty if read failed.

**Example:**
```lean
int success
let val := pointattrib(0, "myattr", ptnum, success)
if success then
  -- Attribute exists and was read successfully
  process(val)
```
-/
vexfunction int pointattrib(int opinput; string attribname; int pointnum; int success) { return 0; }
vexfunction float pointattrib(int opinput; string attribname; int pointnum; int success) { return 0.0; }
vexfunction vector pointattrib(int opinput; string attribname; int pointnum; int success) { return {0,0,0}; }
vexfunction vector4 pointattrib(int opinput; string attribname; int pointnum; int success) { return {0,0,0,0}; }
vexfunction string pointattrib(int opinput; string attribname; int pointnum; int success) { return ""; }

/-- Returns the size of a geometry point attribute.

Returns the number of components in a point attribute.

**Parameters:**
- `opinput`: Input number (`0`-`3`) to read from.
- `attribname`: Name of the point attribute.

**Returns:** Number of components, or `0` if attribute doesn't exist.
-/
vexfunction int pointattribsize(int opinput; string attribname) { return 0; }

/-- Returns the type of a geometry point attribute.

Returns an integer code representing the storage type.

**Parameters:**
- `opinput`: Input number (`0`-`3`) to read from.
- `attribname`: Name of the point attribute.

**Returns:** Integer type code, or `-1` if attribute doesn't exist.
-/
vexfunction int pointattribtype(int opinput; string attribname) { return 0; }

/-- Returns the type info of a geometry point attribute.

Returns transformation metadata for the attribute.

**Parameters:**
- `opinput`: Input number (`0`-`3`) to read from.
- `attribname`: Name of the point attribute.

**Returns:** Type info string, or empty string if attribute doesn't exist.
-/
vexfunction string pointattribtypeinfo(int opinput; string attribname) { return ""; }

-- ============================================================================
-- Reading Primitive Attributes
-- ============================================================================

/-- Reads a primitive attribute value from geometry.

Reads the value of an attribute at a specific primitive. Commonly used in
primitive-based VEX contexts.

**Parameters:**
- `opinput`: Input number (`0`-`3`) to read from.
- `attribname`: Name of the primitive attribute to read.
- `primnum`: Primitive number to read the attribute from.

**Returns:** Attribute value at the specified primitive, or `0`/empty if attribute
           doesn't exist or primitive is invalid.

**Example:**
```lean
-- Read primitive color
let prim_color := prim(0, "Cd", @primnum)
-- Read material path
let material := prim(0, "shop_materialpath", @primnum)
```
-/
vexfunction int prim(int opinput; string attribname; int primnum) { return 0; }
vexfunction float prim(int opinput; string attribname; int primnum) { return 0.0; }
vexfunction vector prim(int opinput; string attribname; int primnum) { return {0,0,0}; }
vexfunction vector4 prim(int opinput; string attribname; int primnum) { return {0,0,0,0}; }
vexfunction string prim(int opinput; string attribname; int primnum) { return ""; }
vexfunction matrix prim(int opinput; string attribname; int primnum) { return {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}; }

/-- Reads a primitive attribute value and outputs a success flag.

Reads a primitive attribute and provides information about whether the read was successful.

**Parameters:**
- `opinput`: Input number (`0`-`3`) to read from.
- `attribname`: Name of the primitive attribute to read.
- `primnum`: Primitive number to read from.
- `success`: Output parameter - set to `1` if successful, `0` otherwise.

**Returns:** Attribute value at the specified primitive, or `0`/empty if read failed.
-/
vexfunction int primattrib(int opinput; string attribname; int primnum; int success) { return 0; }
vexfunction float primattrib(int opinput; string attribname; int primnum; int success) { return 0.0; }
vexfunction vector primattrib(int opinput; string attribname; int primnum; int success) { return {0,0,0}; }
vexfunction vector4 primattrib(int opinput; string attribname; int primnum; int success) { return {0,0,0,0}; }
vexfunction string primattrib(int opinput; string attribname; int primnum; int success) { return ""; }

/-- Returns the size of a geometry primitive attribute.

Returns the number of components in a primitive attribute.

**Parameters:**
- `opinput`: Input number (`0`-`3`) to read from.
- `attribname`: Name of the primitive attribute.

**Returns:** Number of components, or `0` if attribute doesn't exist.
-/
vexfunction int primattribsize(int opinput; string attribname) { return 0; }

/-- Returns the type of a geometry primitive attribute.

Returns an integer code representing the storage type.

**Parameters:**
- `opinput`: Input number (`0`-`3`) to read from.
- `attribname`: Name of the primitive attribute.

**Returns:** Integer type code, or `-1` if attribute doesn't exist.
-/
vexfunction int primattribtype(int opinput; string attribname) { return 0; }

/-- Returns the type info of a geometry primitive attribute.

Returns transformation metadata for the attribute.

**Parameters:**
- `opinput`: Input number (`0`-`3`) to read from.
- `attribname`: Name of the primitive attribute.

**Returns:** Type info string, or empty string if attribute doesn't exist.
-/
vexfunction string primattribtypeinfo(int opinput; string attribname) { return ""; }

/-- Reads a primitive intrinsic from geometry.

Primitive intrinsics are built-in read-only properties of primitives (e.g., `"bounds"`,
`"measuredarea"`, `"vertexcount"`). These provide metadata about individual primitives.

**Parameters:**
- `opinput`: Input number (`0`-`3`) to read from.
- `primnum`: Primitive number to read from.
- `intrinsicname`: Name of the intrinsic to read.

**Returns:** Intrinsic value, or `0`/empty if intrinsic doesn't exist or primitive is invalid.

**Example:**
```lean
-- Get primitive properties
let area := primintrinsic(0, @primnum, "measuredarea")
let nverts := primintrinsic(0, @primnum, "vertexcount")
let bounds := primintrinsic(0, @primnum, "bounds")
```
-/
vexfunction int primintrinsic(int opinput; int primnum; string intrinsicname) { return 0; }
vexfunction float primintrinsic(int opinput; int primnum; string intrinsicname) { return 0.0; }
vexfunction vector primintrinsic(int opinput; int primnum; string intrinsicname) { return {0,0,0}; }
vexfunction vector4 primintrinsic(int opinput; int primnum; string intrinsicname) { return {0,0,0,0}; }
vexfunction string primintrinsic(int opinput; int primnum; string intrinsicname) { return ""; }

-- ============================================================================
-- Reading Vertex Attributes
-- ============================================================================

/-- Reads a vertex attribute value from geometry using linear vertex index.

Reads the value of an attribute at a specific vertex (identified by linear vertex index).
Vertex attributes are stored per vertex (primitive-point pair), commonly used for
UVs and per-corner normals.

**Parameters:**
- `opinput`: Input number (`0`-`3`) to read from.
- `attribname`: Name of the vertex attribute to read.
- `linear_vertex`: Linear vertex index to read the attribute from.

**Returns:** Attribute value at the specified vertex, or `0`/empty if attribute
           doesn't exist or vertex is invalid.

**Example:**
```lean
-- Read UV coordinates (typically stored per-vertex)
let uv := vertex(0, "uv", linear_vtx)
```
-/
vexfunction int vertex(int opinput; string attribname; int linear_vertex) { return 0; }
vexfunction float vertex(int opinput; string attribname; int linear_vertex) { return 0.0; }
vexfunction vector vertex(int opinput; string attribname; int linear_vertex) { return {0,0,0}; }
vexfunction vector4 vertex(int opinput; string attribname; int linear_vertex) { return {0,0,0,0}; }
vexfunction string vertex(int opinput; string attribname; int linear_vertex) { return ""; }

/-- Reads a vertex attribute value using primitive and vertex number.

Alternative function for reading vertex attributes using primitive/vertex pair
instead of linear index. This is often more convenient when iterating over
primitive vertices.

**Parameters:**
- `opinput`: Input number (`0`-`3`) to read from.
- `attribname`: Name of the vertex attribute to read.
- `primnum`: Primitive number containing the vertex.
- `vtxnum`: Vertex index within the primitive (0-based).

**Returns:** Attribute value at the specified vertex, or `0`/empty if attribute
           doesn't exist or vertex is invalid.

**Example:**
```lean
-- Read UV at a specific corner of a primitive
let uv := vertexattrib(0, "uv", @primnum, vtx_index)
```
-/
vexfunction int vertexattrib(int opinput; string attribname; int primnum; int vtxnum) { return 0; }
vexfunction float vertexattrib(int opinput; string attribname; int primnum; int vtxnum) { return 0.0; }
vexfunction vector vertexattrib(int opinput; string attribname; int primnum; int vtxnum) { return {0,0,0}; }
vexfunction vector4 vertexattrib(int opinput; string attribname; int primnum; int vtxnum) { return {0,0,0,0}; }
vexfunction string vertexattrib(int opinput; string attribname; int primnum; int vtxnum) { return ""; }

/-- Returns the size of a geometry vertex attribute.

Returns the number of components in a vertex attribute.

**Parameters:**
- `opinput`: Input number (`0`-`3`) to read from.
- `attribname`: Name of the vertex attribute.

**Returns:** Number of components, or `0` if attribute doesn't exist.
-/
vexfunction int vertexattribsize(int opinput; string attribname) { return 0; }

/-- Returns the type of a geometry vertex attribute.

Returns an integer code representing the storage type.

**Parameters:**
- `opinput`: Input number (`0`-`3`) to read from.
- `attribname`: Name of the vertex attribute.

**Returns:** Integer type code, or `-1` if attribute doesn't exist.
-/
vexfunction int vertexattribtype(int opinput; string attribname) { return 0; }

/-- Returns the type info of a geometry vertex attribute.

Returns transformation metadata for the attribute.

**Parameters:**
- `opinput`: Input number (`0`-`3`) to read from.
- `attribname`: Name of the vertex attribute.

**Returns:** Type info string, or empty string if attribute doesn't exist.
-/
vexfunction string vertexattribtypeinfo(int opinput; string attribname) { return ""; }

-- ============================================================================
-- Writing Attributes (Generic)
-- ============================================================================

/-- Writes an attribute value to geometry.

Generic function to write an attribute value to any class of geometry element.
The attribute must already exist (created with `addattrib` or similar).

**Parameters:**
- `geohandle`: Geometry handle to write to. Use `0` or `geoself()`.
- `attribclass`: Attribute class: `"detail"`, `"primitive"`, `"point"`, or `"vertex"`.
- `attribname`: Name of the attribute to write. Must already exist.
- `elemnum`: Element number to write to. For detail use `0`.
- `vtxnum`: For vertex attributes, the vertex index within primitive. Use `0` for other classes.
- `value`: Value to write to the attribute.

**Returns:** `1` if write was successful, `0` if attribute doesn't exist or write failed.

**Example:**
```lean
-- Generic write (prefer specific functions)
setattrib(geoself(), "point", "Cd", @ptnum, 0, {1,0,0})
-- Better: use specific function
setpointattrib(geoself(), "Cd", @ptnum, {1,0,0})
```
-/
vexfunction int setattrib(int geohandle; string attribclass; string attribname; int elemnum; int vtxnum; int value) { return 0; }
vexfunction int setattrib(int geohandle; string attribclass; string attribname; int elemnum; int vtxnum; float value) { return 0; }
vexfunction int setattrib(int geohandle; string attribclass; string attribname; int elemnum; int vtxnum; vector value) { return 0; }
vexfunction int setattrib(int geohandle; string attribclass; string attribname; int elemnum; int vtxnum; vector4 value) { return 0; }
vexfunction int setattrib(int geohandle; string attribclass; string attribname; int elemnum; int vtxnum; string value) { return 0; }
vexfunction int setattrib(int geohandle; string attribclass; string attribname; int elemnum; int vtxnum; matrix value) { return 0; }

/-- Sets the transformation type info of an attribute.

Sets the transformation metadata for an attribute, controlling how it transforms
with geometry (e.g., whether it should rotate, scale, etc. with transforms).

**Parameters:**
- `geohandle`: Geometry handle to write to. Use `0` or `geoself()`.
- `attribclass`: Attribute class.
- `attribname`: Name of the attribute to modify.
- `typeinfo`: Type info: `"point"`, `"vector"`, `"normal"`, `"color"`, `"quaternion"`,
            `"matrix3"`, `"matrix"`, or `"none"`.

**Returns:** `1` if successful, `0` if attribute doesn't exist or operation failed.

**Example:**
```lean
-- Mark velocity as a vector (transforms with rotation)
setattribtypeinfo(geoself(), "point", "v", "vector")
-- Mark normals correctly
setattribtypeinfo(geoself(), "point", "N", "normal")
-- Mark colors as non-transforming
setattribtypeinfo(geoself(), "point", "Cd", "color")
```
-/
vexfunction int setattribtypeinfo(int geohandle; string attribclass; string attribname; string typeinfo) { return 0; }

-- ============================================================================
-- Writing Detail Attributes
-- ============================================================================

/-- Sets a detail attribute value.

Writes a value to a global (detail) attribute. The attribute must already exist.

**Parameters:**
- `geohandle`: Geometry handle to write to. Use `0` or `geoself()`.
- `attribname`: Name of the detail attribute to write.
- `value`: Value to write to the attribute.

**Returns:** `1` if successful, `0` if attribute doesn't exist or write failed.

**Example:**
```lean
-- Store metadata
setdetailattrib(geoself(), "frame", @Frame)
setdetailattrib(geoself(), "description", "Processed mesh")
```
-/
vexfunction int setdetailattrib(int geohandle; string attribname; int value) { return 0; }
vexfunction int setdetailattrib(int geohandle; string attribname; float value) { return 0; }
vexfunction int setdetailattrib(int geohandle; string attribname; vector value) { return 0; }
vexfunction int setdetailattrib(int geohandle; string attribname; vector4 value) { return 0; }
vexfunction int setdetailattrib(int geohandle; string attribname; string value) { return 0; }

/-- Sets the value of a writeable detail intrinsic.

Writes to a detail intrinsic. Only certain intrinsics are writeable.

**Parameters:**
- `geohandle`: Geometry handle to write to. Use `0` or `geoself()`.
- `intrinsicname`: Name of the intrinsic to write.
- `value`: Value to write.

**Returns:** `1` if successful, `0` if intrinsic doesn't exist, is read-only, or write failed.
-/
vexfunction int setdetailintrinsic(int geohandle; string intrinsicname; int value) { return 0; }
vexfunction int setdetailintrinsic(int geohandle; string intrinsicname; float value) { return 0; }
vexfunction int setdetailintrinsic(int geohandle; string intrinsicname; vector value) { return 0; }
vexfunction int setdetailintrinsic(int geohandle; string intrinsicname; vector4 value) { return 0; }
vexfunction int setdetailintrinsic(int geohandle; string intrinsicname; string value) { return 0; }

-- ============================================================================
-- Writing Point Attributes
-- ============================================================================

/-- Sets a point attribute value.

Writes a value to a point attribute at a specific point. The attribute must already exist.
This is one of the most commonly used functions in point-based VEX.

**Parameters:**
- `geohandle`: Geometry handle to write to. Use `0` or `geoself()`.
- `attribname`: Name of the point attribute to write.
- `pointnum`: Point number to write to.
- `value`: Value to write to the attribute.

**Returns:** `1` if successful, `0` if attribute doesn't exist, point is invalid, or write failed.

**Example:**
```lean
-- Set point color
setpointattrib(geoself(), "Cd", @ptnum, {1,0,0})
-- Set point position
setpointattrib(geoself(), "P", @ptnum, new_position)
-- Modify neighbor's attribute
setpointattrib(geoself(), "selected", neighbor_pt, 1)
```
-/
vexfunction int setpointattrib(int geohandle; string attribname; int pointnum; int value) { return 0; }
vexfunction int setpointattrib(int geohandle; string attribname; int pointnum; float value) { return 0; }
vexfunction int setpointattrib(int geohandle; string attribname; int pointnum; vector value) { return 0; }
vexfunction int setpointattrib(int geohandle; string attribname; int pointnum; vector4 value) { return 0; }
vexfunction int setpointattrib(int geohandle; string attribname; int pointnum; string value) { return 0; }
vexfunction int setpointattrib(int geohandle; string attribname; int pointnum; matrix value) { return 0; }

-- ============================================================================
-- Writing Primitive Attributes
-- ============================================================================

/-- Sets a primitive attribute value.

Writes a value to a primitive attribute at a specific primitive. The attribute must already exist.

**Parameters:**
- `geohandle`: Geometry handle to write to. Use `0` or `geoself()`.
- `attribname`: Name of the primitive attribute to write.
- `primnum`: Primitive number to write to.
- `value`: Value to write to the attribute.

**Returns:** `1` if successful, `0` if attribute doesn't exist, primitive is invalid, or write failed.

**Example:**
```lean
-- Assign material per primitive
setprimattrib(geoself(), "shop_materialpath", @primnum, "/mat/metal")
-- Set primitive color
setprimattrib(geoself(), "Cd", @primnum, {0,1,0})
```
-/
vexfunction int setprimattrib(int geohandle; string attribname; int primnum; int value) { return 0; }
vexfunction int setprimattrib(int geohandle; string attribname; int primnum; float value) { return 0; }
vexfunction int setprimattrib(int geohandle; string attribname; int primnum; vector value) { return 0; }
vexfunction int setprimattrib(int geohandle; string attribname; int primnum; vector4 value) { return 0; }
vexfunction int setprimattrib(int geohandle; string attribname; int primnum; string value) { return 0; }
vexfunction int setprimattrib(int geohandle; string attribname; int primnum; matrix value) { return 0; }

/-- Sets the value of a writeable primitive intrinsic.

Writes to a primitive intrinsic. Only certain intrinsics are writeable.

**Parameters:**
- `geohandle`: Geometry handle to write to. Use `0` or `geoself()`.
- `primnum`: Primitive number to write to.
- `intrinsicname`: Name of the intrinsic to write.
- `value`: Value to write.

**Returns:** `1` if successful, `0` if intrinsic doesn't exist, is read-only,
           primitive is invalid, or write failed.
-/
vexfunction int setprimintrinsic(int geohandle; int primnum; string intrinsicname; int value) { return 0; }
vexfunction int setprimintrinsic(int geohandle; int primnum; string intrinsicname; float value) { return 0; }
vexfunction int setprimintrinsic(int geohandle; int primnum; string intrinsicname; vector value) { return 0; }
vexfunction int setprimintrinsic(int geohandle; int primnum; string intrinsicname; vector4 value) { return 0; }
vexfunction int setprimintrinsic(int geohandle; int primnum; string intrinsicname; string value) { return 0; }

-- ============================================================================
-- Writing Vertex Attributes
-- ============================================================================

/-- Sets a vertex attribute value.

Writes a value to a vertex attribute at a specific vertex. The attribute must already exist.

**Parameters:**
- `geohandle`: Geometry handle to write to. Use `0` or `geoself()`.
- `attribname`: Name of the vertex attribute to write.
- `primnum`: Primitive number containing the vertex.
- `vtxnum`: Vertex index within the primitive (0-based).
- `value`: Value to write to the attribute.

**Returns:** `1` if successful, `0` if attribute doesn't exist, vertex is invalid, or write failed.

**Example:**
```lean
-- Set UV coordinates per vertex
setvertexattrib(geoself(), "uv", @primnum, vtx, {u, v, 0})
-- Set per-corner normals
setvertexattrib(geoself(), "N", @primnum, vtx, smooth_normal)
```
-/
vexfunction int setvertexattrib(int geohandle; string attribname; int primnum; int vtxnum; int value) { return 0; }
vexfunction int setvertexattrib(int geohandle; string attribname; int primnum; int vtxnum; float value) { return 0; }
vexfunction int setvertexattrib(int geohandle; string attribname; int primnum; int vtxnum; vector value) { return 0; }
vexfunction int setvertexattrib(int geohandle; string attribname; int primnum; int vtxnum; vector4 value) { return 0; }
vexfunction int setvertexattrib(int geohandle; string attribname; int primnum; int vtxnum; string value) { return 0; }

-- ============================================================================
-- Removing Attributes
-- ============================================================================

/-- Removes a detail attribute from geometry.

Deletes a detail attribute from the geometry completely.

**Parameters:**
- `geohandle`: Geometry handle to write to. Use `0` or `geoself()`.
- `attribname`: Name of the detail attribute to remove.

**Returns:** `1` if successfully removed, `0` if it didn't exist or removal failed.
-/
vexfunction int removedetailattrib(int geohandle; string attribname) { return 0; }

/-- Removes a point attribute from geometry.

Deletes a point attribute from the geometry completely.

**Parameters:**
- `geohandle`: Geometry handle to write to. Use `0` or `geoself()`.
- `attribname`: Name of the point attribute to remove.

**Returns:** `1` if successfully removed, `0` if it didn't exist or removal failed.
-/
vexfunction int removepointattrib(int geohandle; string attribname) { return 0; }

/-- Removes a point group from geometry.

Deletes a point group from the geometry.

**Parameters:**
- `geohandle`: Geometry handle to write to. Use `0` or `geoself()`.
- `groupname`: Name of the point group to remove.

**Returns:** `1` if successfully removed, `0` if it didn't exist or removal failed.
-/
vexfunction int removepointgroup(int geohandle; string groupname) { return 0; }

/-- Removes a primitive attribute from geometry.

Deletes a primitive attribute from the geometry completely.

**Parameters:**
- `geohandle`: Geometry handle to write to. Use `0` or `geoself()`.
- `attribname`: Name of the primitive attribute to remove.

**Returns:** `1` if successfully removed, `0` if it didn't exist or removal failed.
-/
vexfunction int removeprimattrib(int geohandle; string attribname) { return 0; }

/-- Removes a primitive group from geometry.

Deletes a primitive group from the geometry.

**Parameters:**
- `geohandle`: Geometry handle to write to. Use `0` or `geoself()`.
- `groupname`: Name of the primitive group to remove.

**Returns:** `1` if successfully removed, `0` if it didn't exist or removal failed.
-/
vexfunction int removeprimgroup(int geohandle; string groupname) { return 0; }

/-- Removes a vertex attribute from geometry.

Deletes a vertex attribute from the geometry completely.

**Parameters:**
- `geohandle`: Geometry handle to write to. Use `0` or `geoself()`.
- `attribname`: Name of the vertex attribute to remove.

**Returns:** `1` if successfully removed, `0` if it didn't exist or removal failed.
-/
vexfunction int removevertexattrib(int geohandle; string attribname) { return 0; }

/-- Removes a vertex group from geometry.

Deletes a vertex group from the geometry.

**Parameters:**
- `geohandle`: Geometry handle to write to. Use `0` or `geoself()`.
- `groupname`: Name of the vertex group to remove.

**Returns:** `1` if successfully removed, `0` if it didn't exist or removal failed.
-/
vexfunction int removevertexgroup(int geohandle; string groupname) { return 0; }

-- ============================================================================
-- Attribute Query Functions
-- ============================================================================

/-- Checks whether a geometry attribute exists.

Tests if an attribute of any class exists in the geometry.

**Parameters:**
- `opinput`: Input number (`0`-`3`) to read from.
- `attribclass`: Attribute class: `"detail"`, `"primitive"`, `"point"`, or `"vertex"`.
- `attribname`: Name of the attribute to check for.

**Returns:** `1` if attribute exists, `0` otherwise.

**Example:**
```lean
if hasattrib(0, "point", "Cd") then
  -- Color attribute exists
  let color := point(0, "Cd", @ptnum)
```
-/
vexfunction int hasattrib(int opinput; string attribclass; string attribname) { return 0; }

/-- Returns if a geometry detail attribute exists.

Tests if a detail attribute exists in the geometry.

**Parameters:**
- `opinput`: Input number (`0`-`3`) to read from.
- `attribname`: Name of the detail attribute to check for.

**Returns:** `1` if attribute exists, `0` otherwise.
-/
vexfunction int hasdetailattrib(int opinput; string attribname) { return 0; }

/-- Returns if a geometry point attribute exists.

Tests if a point attribute exists in the geometry.

**Parameters:**
- `opinput`: Input number (`0`-`3`) to read from.
- `attribname`: Name of the point attribute to check for.

**Returns:** `1` if attribute exists, `0` otherwise.
-/
vexfunction int haspointattrib(int opinput; string attribname) { return 0; }

/-- Returns if a geometry primitive attribute exists.

Tests if a primitive attribute exists in the geometry.

**Parameters:**
- `opinput`: Input number (`0`-`3`) to read from.
- `attribname`: Name of the primitive attribute to check for.

**Returns:** `1` if attribute exists, `0` otherwise.
-/
vexfunction int hasprimattrib(int opinput; string attribname) { return 0; }

/-- Returns if a geometry vertex attribute exists.

Tests if a vertex attribute exists in the geometry.

**Parameters:**
- `opinput`: Input number (`0`-`3`) to read from.
- `attribname`: Name of the vertex attribute to check for.

**Returns:** `1` if attribute exists, `0` otherwise.
-/
vexfunction int hasvertexattrib(int opinput; string attribname) { return 0; }

-- ============================================================================
-- Attribute Search Functions
-- ============================================================================

/-- Finds a primitive/point/vertex by attribute value.

Searches for the first element with a matching attribute value. Useful for
finding elements by ID or name attributes.

**Parameters:**
- `opinput`: Input number (`0`-`3`) to read from.
- `attribclass`: Class to search: `"primitive"`, `"point"`, or `"vertex"`.
- `attribname`: Name of the attribute to search.
- `value`: Value to search for. Must match the attribute type.

**Returns:** Element number (point/primitive/linear vertex) of first match, or `-1` if not found.

**Example:**
```lean
-- Find point with specific ID
let pt := findattribval(0, "point", "id", 42)
-- Find primitive by name
let prim := findattribval(0, "primitive", "name", "target")
```
-/
vexfunction int findattribval(int opinput; string attribclass; string attribname; int value) { return 0; }
vexfunction int findattribval(int opinput; string attribclass; string attribname; string value) { return 0; }

/-- Returns number of elements with a certain attribute value.

Counts how many elements have a matching attribute value.

**Parameters:**
- `opinput`: Input number (`0`-`3`) to read from.
- `attribclass`: Class to search: `"primitive"`, `"point"`, or `"vertex"`.
- `attribname`: Name of the attribute to search.
- `value`: Value to count matches for.

**Returns:** Number of elements with the specified attribute value.

**Example:**
```lean
-- Count how many points are selected
let count := findattribvalcount(0, "point", "selected", 1)
```
-/
vexfunction int findattribvalcount(int opinput; string attribclass; string attribname; int value) { return 0; }
vexfunction int findattribvalcount(int opinput; string attribclass; string attribname; string value) { return 0; }

/-- Finds a point by its id attribute.

Searches for a point with a specific value in the `id` attribute. This is
faster than using `findattribval()` for the `id` attribute.

**Parameters:**
- `opinput`: Input number (`0`-`3`) to read from.
- `id`: The id value to search for.

**Returns:** Point number with the specified id, or `-1` if not found.

**Example:**
```lean
let pt := idtopoint(0, 1001)
```
-/
vexfunction int idtopoint(int opinput; int id) { return 0; }

/-- Finds a primitive by its id attribute.

Searches for a primitive with a specific value in the `id` attribute. This is
faster than using `findattribval()` for the `id` attribute.

**Parameters:**
- `opinput`: Input number (`0`-`3`) to read from.
- `id`: The id value to search for.

**Returns:** Primitive number with the specified id, or `-1` if not found.
-/
vexfunction int idtoprim(int opinput; int id) { return 0; }

/-- Finds a point by its name attribute.

Searches for a point with a specific value in the `name` attribute. This is
faster than using `findattribval()` for the `name` attribute.

**Parameters:**
- `opinput`: Input number (`0`-`3`) to read from.
- `name`: The name value to search for.

**Returns:** Point number with the specified name, or `-1` if not found.
-/
vexfunction int nametopoint(int opinput; string name) { return 0; }

/-- Finds a primitive by its name attribute.

Searches for a primitive with a specific value in the `name` attribute. This is
faster than using `findattribval()` for the `name` attribute.

**Parameters:**
- `opinput`: Input number (`0`-`3`) to read from.
- `name`: The name value to search for.

**Returns:** Primitive number with the specified name, or `-1` if not found.
-/
vexfunction int nametoprim(int opinput; string name) { return 0; }

-- ============================================================================
-- Unique Value Functions
-- ============================================================================

/-- Returns the number of unique values from an integer or string attribute.

Counts the number of distinct values across all elements for the specified attribute.

**Parameters:**
- `opinput`: Input number (`0`-`3`) to read from.
- `attribclass`: Attribute class.
- `attribname`: Name of the attribute.

**Returns:** Number of unique values, or `0` if attribute doesn't exist.

**Example:**
```lean
-- Count unique materials
let mat_count := nuniqueval(0, "primitive", "shop_materialpath")
```
-/
vexfunction int nuniqueval(int opinput; string attribclass; string attribname) { return 0; }

/-- Returns one of the set of unique values from an attribute.

Returns the Nth unique value from the attribute.

**Parameters:**
- `opinput`: Input number (`0`-`3`) to read from.
- `attribclass`: Attribute class.
- `attribname`: Name of the attribute.
- `which`: Index of the unique value to return (0-based). Must be less than `nuniqueval()`.

**Returns:** The unique value at the specified index, or `0`/empty if index is invalid.

**Example:**
```lean
-- Get all unique values
let count := nuniqueval(0, "point", "piece")
for i in 0..count do
  let piece := uniqueval(0, "point", "piece", i)
```
-/
vexfunction int uniqueval(int opinput; string attribclass; string attribname; int which) { return 0; }
vexfunction string uniqueval(int opinput; string attribclass; string attribname; int which) { return ""; }

/-- Returns the set of unique values from an attribute as an array.

Returns an array containing all unique values for the attribute, with no duplicates.

**Parameters:**
- `opinput`: Input number (`0`-`3`) to read from.
- `attribclass`: Attribute class.
- `attribname`: Name of the attribute.

**Returns:** Array of unique values. Empty if attribute doesn't exist.

**Example:**
```lean
-- Get all unique piece IDs
let pieces := uniquevals(0, "point", "piece")
for piece in pieces do
  process_piece(piece)
```
-/
vexfunction int[] uniquevals(int opinput; string attribclass; string attribname) { return #[]; }
vexfunction string[] uniquevals(int opinput; string attribclass; string attribname) { return #[]; }

-- ============================================================================
-- Attribute Interpolation Functions
-- ============================================================================

/-- Interpolates an attribute at parametric (u, v) position on a primitive.

Evaluates an attribute on a primitive at parametric coordinates, interpolating
between vertices. Commonly used for sampling attributes at arbitrary positions
on surfaces.

**Parameters:**
- `opinput`: Input number (`0`-`3`) to read from.
- `attribname`: Name of the attribute to interpolate.
- `primnum`: Primitive to evaluate on.
- `u`: U parametric coordinate on the primitive (typically 0-1).
- `v`: V parametric coordinate on the primitive (typically 0-1).

**Returns:** Interpolated attribute value at the specified parametric position.

**Example:**
```lean
-- Sample color at center of primitive
let color := primuv(0, "Cd", @primnum, 0.5, 0.5)
-- Sample at specific UV
let value := primuv(0, "density", @primnum, u, v)
```
-/
vexfunction float primuv(int opinput; string attribname; int primnum; float u; float v) { return 0.0; }
vexfunction vector primuv(int opinput; string attribname; int primnum; float u; float v) { return {0,0,0}; }
vexfunction vector4 primuv(int opinput; string attribname; int primnum; float u; float v) { return {0,0,0,0}; }

/-- Interpolates an attribute at parametric position (alternative name).

Alternative name for `primuv()` with slightly different parameter order.

**Parameters:**
- `opinput`: Input number (`0`-`3`) to read from.
- `primnum`: Primitive to evaluate on.
- `attribname`: Name of the attribute to interpolate.
- `u`: U parametric coordinate.
- `v`: V parametric coordinate.

**Returns:** Interpolated attribute value.
-/
vexfunction float prim_attribute(int opinput; int primnum; string attribname; float u; float v) { return 0.0; }
vexfunction vector prim_attribute(int opinput; int primnum; string attribname; float u; float v) { return {0,0,0}; }
vexfunction vector4 prim_attribute(int opinput; int primnum; string attribname; float u; float v) { return {0,0,0,0}; }

/-- Interpolates an attribute at UV coordinates using a UV attribute.

Uses a UV attribute (like `uv` stored on vertices/points) to determine where
to sample another attribute. Useful for texture-space lookups.

**Parameters:**
- `opinput`: Input number (`0`-`3`) to read from.
- `primnum`: Primitive to evaluate on.
- `uvname`: Name of the UV attribute to use for lookup (e.g., `"uv"`).
- `attribname`: Name of the attribute to sample.
- `uv`: UV coordinates in texture space to sample at.

**Returns:** Sampled attribute value at the specified UV coordinates.

**Example:**
```lean
-- Sample attribute at texture coordinates
let value := uvsample(0, @primnum, "uv", "Cd", {u, v, 0})
```
-/
vexfunction float uvsample(int opinput; int primnum; string uvname; string attribname; vector uv) { return 0.0; }
vexfunction vector uvsample(int opinput; int primnum; string uvname; string attribname; vector uv) { return {0,0,0}; }
vexfunction vector4 uvsample(int opinput; int primnum; string uvname; string attribname; vector uv) { return {0,0,0,0}; }

/-- Returns position derivative on a primitive at parametric (u, v) position.

Computes the rate of change of position with respect to the parametric coordinates.
Useful for computing tangents, normals, and surface orientation.

**Parameters:**
- `opinput`: Input number (`0`-`3`) to read from.
- `primnum`: Primitive to evaluate on.
- `u`: U parametric coordinate.
- `v`: V parametric coordinate.

**Returns:** Vector containing derivatives (dP/du, dP/dv, 0).

**Example:**
```lean
-- Compute tangent vectors
let deriv := primduv(0, @primnum, u, v)
let tangent_u := deriv.x
let tangent_v := deriv.y
```
-/
vexfunction vector primduv(int opinput; int primnum; float u; float v) { return {0,0,0}; }

/-- Evaluates the arc length on a primitive between two parametric positions.

Computes the arc length along a curve or surface between two parametric positions.

**Parameters:**
- `opinput`: Input number (`0`-`3`) to read from.
- `primnum`: Primitive to measure on.
- `u1`: Starting U parametric coordinate.
- `v1`: Starting V parametric coordinate.
- `u2`: Ending U parametric coordinate.
- `v2`: Ending V parametric coordinate.

**Returns:** Arc length between the two parametric positions.

**Example:**
```lean
-- Measure distance along curve
let length := primarclen(0, @primnum, 0.0, 0.0, 1.0, 0.0)
```
-/
vexfunction float primarclen(int opinput; int primnum; float u1; float v1; float u2; float v2) { return 0.0; }

/-- Evaluates arc length along a path defined by parametric coordinates.

Computes the arc length along a curve passing through multiple points.

**Parameters:**
- `opinput`: Input number (`0`-`3`) to read from.
- `primnum`: Primitive to measure on.
- `uvw`: Array of parametric coordinates defining the path.

**Returns:** Total arc length along the path.
-/
vexfunction float curvearclen(int opinput; int primnum; float uvw[]) { return 0.0; }

/-- Finds the indices and weights for interpolating at UVW coordinates.

Returns barycentric coordinates for interpolation within a primitive. Useful for
custom interpolation schemes.

**Parameters:**
- `opinput`: Input number (`0`-`3`) to read from.
- `primnum`: Primitive to query.
- `u`: U parametric coordinate.
- `v`: V parametric coordinate.
- `w`: W parametric coordinate (for volume primitives).
- `indices`: Output parameter - array of vertex indices.
- `weights`: Output parameter - array of corresponding weights.

**Returns:** Number of vertices involved in the interpolation.
-/
vexfunction int priminteriorweights(int opinput; int primnum; float u; float v; float w; int indices[]; float weights[]) { return 0; }

/-- Convert parametric UV locations between different coordinate spaces.

Converts between different parametric coordinate systems (e.g., uniform vs. arc length).

**Parameters:**
- `opinput`: Input number (`0`-`3`) to read from.
- `primnum`: Curve primitive to convert on.
- `from_space`: Source coordinate space (e.g., `"uniform"`, `"unitlen"`).
- `to_space`: Target coordinate space.
- `uv`: Parametric coordinates to convert.

**Returns:** Converted parametric coordinates.

**Example:**
```lean
-- Convert from uniform to arc-length parameterization
let uv_arclen := primuvconvert(0, @primnum, "uniform", "unitlen", {u, 0, 0})
```
-/
vexfunction vector primuvconvert(int opinput; int primnum; string from_space; string to_space; vector uv) { return {0,0,0}; }

-- ============================================================================
-- Advanced Attribute Functions
-- ============================================================================

/-- Reads an attribute value with validity check.

Reads an attribute and returns whether the read was successful, combining read
and validation in one call.

**Parameters:**
- `opinput`: Input number (`0`-`3`) to read from.
- `attribclass`: Attribute class.
- `attribname`: Name of the attribute.
- `elemnum`: Element number to read from.
- `success`: Output parameter - set to `1` if successful, `0` otherwise.

**Returns:** Attribute value, or `0`/empty if read failed.
-/
vexfunction int getattrib(int opinput; string attribclass; string attribname; int elemnum; int success) { return 0; }
vexfunction float getattrib(int opinput; string attribclass; string attribname; int elemnum; int success) { return 0.0; }
vexfunction vector getattrib(int opinput; string attribclass; string attribname; int elemnum; int success) { return {0,0,0}; }
vexfunction vector4 getattrib(int opinput; string attribclass; string attribname; int elemnum; int success) { return {0,0,0,0}; }
vexfunction string getattrib(int opinput; string attribclass; string attribname; int elemnum; int success) { return ""; }

/-- Copies attribute value into a variable and returns success flag.

Alternative attribute reading function with explicit success reporting.

**Parameters:**
- `opinput`: Input number (`0`-`3`) to read from.
- `attribname`: Name of the attribute.
- `attribclass`: Attribute class.
- `elemnum`: Element number to read from.
- `value`: Output parameter - set to the attribute value if successful.

**Returns:** `1` if attribute was successfully read, `0` otherwise.
-/
vexfunction int getattribute(int opinput; string attribname; string attribclass; int elemnum; int value) { return 0; }
vexfunction int getattribute(int opinput; string attribname; string attribclass; int elemnum; float value) { return 0; }
vexfunction int getattribute(int opinput; string attribname; string attribclass; int elemnum; vector value) { return 0; }
vexfunction int getattribute(int opinput; string attribname; string attribclass; int elemnum; vector4 value) { return 0; }
vexfunction int getattribute(int opinput; string attribname; string attribclass; int elemnum; string value) { return 0; }

-- ============================================================================
-- Point Transform Functions
-- ============================================================================

/-- Returns a point transform from a point index.

Retrieves the local-to-world transformation matrix for a point. Useful when
points have instance transforms attached.

**Parameters:**
- `opinput`: Input number (`0`-`3`) to read from.
- `ptnum`: Point number to query.

**Returns:** 4x4 transformation matrix representing the point's transform. Returns
           identity if the point has no transform.

**Example:**
```lean
-- Get point's transform matrix
let xform := pointtransform(0, @ptnum)
-- Transform a local vector to world space
let world_vec := local_vec * xform
```
-/
vexfunction matrix pointtransform(int opinput; int ptnum) { return {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}; }

/-- Returns a rigid point transform from a point index.

Retrieves a rigid (rotation + translation only, no scale/shear) transformation
for a point.

**Parameters:**
- `opinput`: Input number (`0`-`3`) to read from.
- `ptnum`: Point number to query.

**Returns:** 4x4 rigid transformation matrix. Returns identity if the point has no transform.
-/
vexfunction matrix pointtransformrigid(int opinput; int ptnum) { return {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}; }

/-- Returns an array of point transforms from point indices.

Retrieves transformation matrices for multiple points at once. More efficient
than calling `pointtransform()` in a loop.

**Parameters:**
- `opinput`: Input number (`0`-`3`) to read from.
- `ptnums`: Array of point numbers to query.

**Returns:** Array of 4x4 transformation matrices, one per point.
-/
vexfunction matrix[] pointtransforms(int opinput; int ptnums[]) { return #[]; }

/-- Returns an array of rigid point transforms from point indices.

Retrieves rigid transformation matrices for multiple points at once.

**Parameters:**
- `opinput`: Input number (`0`-`3`) to read from.
- `ptnums`: Array of point numbers to query.

**Returns:** Array of 4x4 rigid transformation matrices, one per point.
-/
vexfunction matrix[] pointtransformsrigid(int opinput; int ptnums[]) { return #[]; }

/-- Returns an array of point local transforms from point indices.

Retrieves local transformation information for multiple points.

**Parameters:**
- `opinput`: Input number (`0`-`3`) to read from.
- `ptnums`: Array of point numbers to query.

**Returns:** Array of transformation data structures.
-/
vexfunction matrix[] pointlocaltransforms(int opinput; int ptnums[]) { return #[]; }

/-- Sets the world space transform of a point.

Sets the transformation matrix for a point in world space.

**Parameters:**
- `geohandle`: Geometry handle to write to. Use `0` or `geoself()`.
- `ptnum`: Point number to modify.
- `transform`: 4x4 transformation matrix to set.

**Returns:** `1` if successful, `0` if point is invalid or operation failed.

**Example:**
```lean
-- Set point transform
let xform := matrix_from_rotation_and_translation(rot, pos)
setpointtransform(geoself(), @ptnum, xform)
```
-/
vexfunction int setpointtransform(int geohandle; int ptnum; matrix transform) { return 0; }

/-- Sets an array of point transforms at given point indices.

Sets transformation matrices for multiple points at once. More efficient than
calling `setpointtransform()` in a loop.

**Parameters:**
- `geohandle`: Geometry handle to write to. Use `0` or `geoself()`.
- `ptnums`: Array of point numbers to modify.
- `transforms`: Array of 4x4 transformation matrices, one per point.

**Returns:** `1` if successful, `0` on failure.
-/
vexfunction int setpointtransforms(int geohandle; int ptnums[]; matrix transforms[]) { return 0; }

/-- Sets an array of point local transforms at given point indices.

Sets local transformation information for multiple points.

**Parameters:**
- `geohandle`: Geometry handle to write to. Use `0` or `geoself()`.
- `ptnums`: Array of point numbers to modify.
- `transforms`: Array of local transformation data.

**Returns:** `1` if successful, `0` on failure.
-/
vexfunction int setpointlocaltransforms(int geohandle; int ptnums[]; matrix transforms[]) { return 0; }
