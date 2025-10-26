import HouLean.Vex.VexFunction

open VEX

-- ============================================================================
-- Point Management
-- ============================================================================

/- Adds a point to the geometry.

Creates a new point in the geometry at the position of an existing point.

**Parameters:**
- `geohandle`: A handle to the geometry to write to. Currently the only valid value is `0` or `geoself()`, which represents the current geometry being processed.
- `point_number`: The point number of an existing point in the geometry. The new point will be created at the same position as this point.

**Returns:** The point number of the newly created point, or `-1` if the point could not be created. -/
vexfunction int addpoint(int geohandle; int point_number) { return 0; }

/- Adds a point to the geometry.

Creates a new point in the geometry at the specified position in world space.

**Parameters:**
- `geohandle`: A handle to the geometry to write to. Currently the only valid value is `0` or `geoself()`, which represents the current geometry being processed.
- `pos`: The position in world space (as a vector) where the new point should be created.

**Returns:** The point number of the newly created point, or `-1` if the point could not be created. -/
vexfunction int addpoint(int geohandle; vector pos) { return 0; }

/- Removes a point from the geometry.

Removes the specified point from the geometry. By default, primitives that reference the point are not removed, which may leave them in an invalid state.

**Parameters:**
- `geohandle`: A handle to the geometry to write to. Currently the only valid value is `0` or `geoself()`.
- `point_number`: The point number to remove from the geometry.

**Returns:** `1` on success, `0` on failure. -/
vexfunction int removepoint(int geohandle; int point_number) { return 0; }

/- Removes a point from the geometry.

Removes the specified point from the geometry, with optional removal of connected primitives.

**Parameters:**
- `geohandle`: A handle to the geometry to write to. Currently the only valid value is `0` or `geoself()`.
- `point_number`: The point number to remove from the geometry.
- `and_prims`: If non-zero, also removes any primitives that reference this point. If zero, primitives are left in place (possibly in an invalid state).

**Returns:** `1` on success, `0` on failure. -/
vexfunction int removepoint(int geohandle; int point_number; int and_prims) { return 0; }

/- Returns the number of points in the geometry file.

Queries a geometry file on disk to determine how many points it contains.

**Parameters:**
- `filename`: The name of the geometry file to query (e.g., `"defgeo.bgeo"` or `"model.geo"`).

**Returns:** The total number of points in the geometry file. -/
vexfunction int npoints(string filename) { return 0; }

/- Returns the number of points in the input.

Queries one of the node's inputs to determine how many points it contains.

**Parameters:**
- `opinput`: The input number to read from. Valid values are `0`, `1`, `2`, or `3`, corresponding to the first through fourth inputs of the node.

**Returns:** The total number of points in the input geometry. -/
vexfunction int npoints(int opinput) { return 0; }

/- Finds the closest point in a geometry.

Finds the point in the geometry that is nearest to the given position in world space.

**Parameters:**
- `opinput`: The input number (`0`-`3`) to read from.
- `pt`: The position in world space (as a vector) to find the nearest point to.

**Returns:** The point number of the nearest point, or `-1` if no point is found. -/
vexfunction int nearpoint(int opinput; vector pt) { return 0; }

/- Finds the closest point in a geometry within a maximum distance.

Finds the nearest point within a specified search radius.

**Parameters:**
- `opinput`: The input number (`0`-`3`) to read from.
- `pt`: The position in world space to find the nearest point to.
- `maxdist`: The maximum distance to search for points. Only points within this distance from `pt` are considered. Use a large value to search the entire geometry.

**Returns:** The point number of the nearest point within `maxdist`, or `-1` if no point is found within the search radius. -/
vexfunction int nearpoint(int opinput; vector pt; float maxdist) { return 0; }

/- Finds the closest point in a point group.

Limits the search to points that are members of a specific point group.

**Parameters:**
- `opinput`: The input number (`0`-`3`) to read from.
- `ptgroup`: The name of a point group to limit the search to. Only points in this group will be considered.
- `pt`: The position in world space to find the nearest point to.

**Returns:** The point number of the nearest point in the group, or `-1` if no point is found in the group. -/
vexfunction int nearpoint(int opinput; string ptgroup; vector pt) { return 0; }

/- Finds the closest point in a point group within a maximum distance.

Combines group filtering with distance limiting.

**Parameters:**
- `opinput`: The input number (`0`-`3`) to read from.
- `ptgroup`: The name of a point group to limit the search to.
- `pt`: The position in world space to find the nearest point to.
- `maxdist`: The maximum distance to search for points. Only points in the group and within this distance are considered.

**Returns:** The point number of the nearest point in the group within `maxdist`, or `-1` if no such point exists. -/
vexfunction int nearpoint(int opinput; string ptgroup; vector pt; float maxdist) { return 0; }

/- Finds all the closest points in a geometry.

Returns multiple points within a search radius, sorted by distance from nearest to farthest.

**Parameters:**
- `opinput`: The input number (`0`-`3`) to read from.
- `pt`: The position in world space to search from.
- `maxdist`: The maximum distance to search for points. Points beyond this distance will not be included.

**Returns:** An array of point numbers, sorted by increasing distance from `pt`. The array may be empty if no points are within `maxdist`. -/
vexfunction int[] nearpoints(int opinput; vector pt; float maxdist) { return #[]; }

/- Finds all the closest points in a geometry up to a maximum count.

Limits the number of returned points to improve performance when you only need the N nearest points.

**Parameters:**
- `opinput`: The input number (`0`-`3`) to read from.
- `pt`: The position in world space to search from.
- `maxdist`: The maximum distance to search for points.
- `maxpts`: The maximum number of points to return. The function will return at most this many points, even if more exist within `maxdist`.

**Returns:** An array of up to `maxpts` point numbers, sorted by increasing distance from `pt`. -/
vexfunction int[] nearpoints(int opinput; vector pt; float maxdist; int maxpts) { return #[]; }

/- Finds all the closest points in a point group.

Returns multiple points from a group within a search radius.

**Parameters:**
- `opinput`: The input number (`0`-`3`) to read from.
- `ptgroup`: The name of a point group to limit the search to. Only points in this group are considered.
- `pt`: The position in world space to search from.
- `maxdist`: The maximum distance to search for points.

**Returns:** An array of point numbers from the group, sorted by increasing distance from `pt`. -/
vexfunction int[] nearpoints(int opinput; string ptgroup; vector pt; float maxdist) { return #[]; }

/- Finds all the closest points in a point group up to a maximum count.

Combines group filtering, distance limiting, and count limiting.

**Parameters:**
- `opinput`: The input number (`0`-`3`) to read from.
- `ptgroup`: The name of a point group to limit the search to.
- `pt`: The position in world space to search from.
- `maxdist`: The maximum distance to search for points.
- `maxpts`: The maximum number of points to return.

**Returns:** An array of up to `maxpts` point numbers from the group, sorted by increasing distance from `pt`. -/
vexfunction int[] nearpoints(int opinput; string ptgroup; vector pt; float maxdist; int maxpts) { return #[]; }

/- Returns the position of the closest point on the geometry surface.

Given a position in world space, returns the position of the closest point on the surface of the geometry. This can be used to project points onto a surface or to find the nearest surface location.

**Parameters:**
- `opinput`: The input number (`0`-`3`) to read from.
- `pt`: The position in world space to find the closest surface point to. This can be anywhere in space, not necessarily on the geometry.

**Returns:** The position (as a vector) on the geometry surface that is closest to `pt`. -/
vexfunction vector minpos(int opinput; vector pt) { return {0,0,0}; }

/- Returns the position of the closest point on the geometry within a primitive group.

Limits the surface search to primitives in a specific group.

**Parameters:**
- `opinput`: The input number (`0`-`3`) to read from.
- `primgroup`: The name of a primitive group to limit the search to. Only primitives in this group are considered when finding the closest surface point.
- `pt`: The position in world space to find the closest surface point to.

**Returns:** The position (as a vector) on the geometry surface (within the specified group) that is closest to `pt`. -/
vexfunction vector minpos(int opinput; string primgroup; vector pt) { return {0,0,0}; }

/- Returns the point number of the nth neighbor of a point.

Points are considered neighbors if they are connected by an edge (i.e., they both belong to the same primitive). This function returns a specific neighbor by index.

**Parameters:**
- `opinput`: The input number (`0`-`3`) to read from.
- `point_number`: The point whose neighbors to query.
- `neighbour_num`: The index of which neighbor to return (0-based). For example, `0` returns the first neighbor, `1` returns the second, etc.

**Returns:** The point number of the specified neighbor, or `-1` if there is no neighbor at that index (e.g., if the point has fewer neighbors than `neighbour_num + 1`). -/
vexfunction int neighbour(int opinput; int point_number; int neighbour_num) { return 0; }

/- Returns the number of points connected to the specified point.

Counts how many other points share an edge with the given point. This is useful for determining the connectivity of a point in the mesh.

**Parameters:**
- `opinput`: The input number (`0`-`3`) to read from.
- `point_number`: The point to query.

**Returns:** The number of neighboring points that are connected to this point by edges. -/
vexfunction int neighbourcount(int opinput; int point_number) { return 0; }

/- Returns an array of the point numbers of the neighbors of a point.

Returns all points that share an edge with the given point. This is more efficient than calling `neighbour()` repeatedly.

**Parameters:**
- `opinput`: The input number (`0`-`3`) to read from.
- `point_number`: The point whose neighbors to find.

**Returns:** An array of point numbers representing all edge-connected neighbors. The array will be empty if the point has no neighbors. -/
vexfunction int[] neighbours(int opinput; int point_number) { return #[]; }

/- Returns the list of primitives containing a point.

Returns all primitives that have at least one vertex referencing the given point.

**Parameters:**
- `opinput`: The input number (`0`-`3`) to read from.
- `point_number`: The point to query.

**Returns:** An array of primitive numbers that contain the specified point. The array will be empty if no primitives reference the point. -/
vexfunction int[] pointprims(int opinput; int point_number) { return #[]; }

/- Returns a linear vertex number of a point in a geometry.

Returns the first linear vertex index that references the given point. Linear vertex indices are unique across the entire geometry, unlike primitive vertex indices which are relative to a specific primitive.

**Parameters:**
- `opinput`: The input number (`0`-`3`) to read from.
- `point_number`: The point to query.

**Returns:** A linear vertex index, or `-1` if the point has no vertices (i.e., no primitives reference it). -/
vexfunction int pointvertex(int opinput; int point_number) { return 0; }

/- Returns the list of vertices connected to a point.

Returns all linear vertex indices for vertices that reference the given point. Since multiple primitives can share a point, there may be multiple vertices per point.

**Parameters:**
- `opinput`: The input number (`0`-`3`) to read from.
- `point_number`: The point to query.

**Returns:** An array of linear vertex indices that reference the specified point. The array will be empty if no vertices reference the point. -/
vexfunction int[] pointvertices(int opinput; int point_number) { return #[]; }

-- ============================================================================
-- Primitive Management
-- ============================================================================

/- Adds a primitive to the geometry.

Creates a new primitive of the specified type with no vertices. You must add vertices to the primitive using `addvertex()` after creation.

**Parameters:**
- `geohandle`: A handle to the geometry to write to. Currently the only valid value is `0` or `geoself()`.
- `type`: The type of primitive to create. Common types include `"poly"` (polygon), `"polyline"` (open polygon), `"sphere"`, `"tube"`, `"metaball"`, etc.

**Returns:** The primitive number of the created primitive, or `-1` on failure. -/
vexfunction int addprim(int geohandle; string type) { return 0; }

/- Adds a primitive to the geometry with one point.

Creates a new primitive with a single vertex referencing the specified point.

**Parameters:**
- `geohandle`: A handle to the geometry to write to.
- `type`: The type of primitive to create.
- `point0`: The point number to add as the first (and only) vertex of the primitive.

**Returns:** The primitive number of the created primitive, or `-1` on failure. -/
vexfunction int addprim(int geohandle; string type; int point0) { return 0; }

/- Adds a primitive to the geometry with two points.

Creates a new primitive with two vertices, useful for creating edges or line segments.

**Parameters:**
- `geohandle`: A handle to the geometry to write to.
- `type`: The type of primitive to create (commonly `"polyline"`).
- `point0`: The point number for the first vertex.
- `point1`: The point number for the second vertex.

**Returns:** The primitive number of the created primitive, or `-1` on failure. -/
vexfunction int addprim(int geohandle; string type; int point0; int point1) { return 0; }

/- Adds a primitive to the geometry with three points.

Creates a new primitive with three vertices, useful for creating triangles.

**Parameters:**
- `geohandle`: A handle to the geometry to write to.
- `type`: The type of primitive to create (commonly `"poly"` for a triangle).
- `point0`: The point number for the first vertex.
- `point1`: The point number for the second vertex.
- `point2`: The point number for the third vertex.

**Returns:** The primitive number of the created primitive, or `-1` on failure. -/
vexfunction int addprim(int geohandle; string type; int point0; int point1; int point2) { return 0; }

/- Adds a primitive to the geometry with four points.

Creates a new primitive with four vertices, useful for creating quadrilaterals.

**Parameters:**
- `geohandle`: A handle to the geometry to write to.
- `type`: The type of primitive to create (commonly `"poly"` for a quad).
- `point0`: The point number for the first vertex.
- `point1`: The point number for the second vertex.
- `point2`: The point number for the third vertex.
- `point3`: The point number for the fourth vertex.

**Returns:** The primitive number of the created primitive, or `-1` on failure. -/
vexfunction int addprim(int geohandle; string type; int point0; int point1; int point2; int point3) { return 0; }

/- Removes a primitive from the geometry.

Removes the specified primitive from the geometry. By default, points referenced by the primitive's vertices are not removed.

**Parameters:**
- `geohandle`: A handle to the geometry to write to. Currently the only valid value is `0` or `geoself()`.
- `prim_number`: The primitive number to remove.

**Returns:** `1` on success, `0` on failure. -/
vexfunction int removeprim(int geohandle; int prim_number) { return 0; }

/- Removes a primitive from the geometry.

Removes the specified primitive, with optional removal of points that become unused.

**Parameters:**
- `geohandle`: A handle to the geometry to write to.
- `prim_number`: The primitive number to remove.
- `and_points`: If non-zero, also removes any points that are no longer referenced by any primitive after removing this primitive. If zero, points are left in place.

**Returns:** `1` on success, `0` on failure. -/
vexfunction int removeprim(int geohandle; int prim_number; int and_points) { return 0; }

/- Returns the number of primitives in the geometry file.

Queries a geometry file on disk to determine how many primitives it contains.

**Parameters:**
- `filename`: The name of the geometry file to query (e.g., `"defgeo.bgeo"`).

**Returns:** The total number of primitives in the geometry file. -/
vexfunction int nprimitives(string filename) { return 0; }

/- Returns the number of primitives in the input.

Queries one of the node's inputs to determine how many primitives it contains.

**Parameters:**
- `opinput`: The input number (`0`-`3`) to read from.

**Returns:** The total number of primitives in the input geometry. -/
vexfunction int nprimitives(int opinput) { return 0; }

/- Converts a primitive/vertex pair into a point number.

Given a primitive and a vertex index on that primitive, returns the point number that the vertex references.

**Parameters:**
- `opinput`: The input number (`0`-`3`) to read from.
- `prim_number`: The primitive number.
- `vertex_index`: The vertex index on the primitive (0-based). For example, `0` is the first vertex, `1` is the second, etc.

**Returns:** The point number that the specified vertex references, or `-1` if the primitive or vertex index is invalid. -/
vexfunction int primpoint(int opinput; int prim_number; int vertex_index) { return 0; }

/- Returns the list of points on a primitive.

Returns an array of point numbers for all vertices on the primitive, in the order they appear on the primitive.

**Parameters:**
- `opinput`: The input number (`0`-`3`) to read from.
- `prim_number`: The primitive to query.

**Returns:** An array of point numbers referenced by the primitive's vertices, in vertex order. -/
vexfunction int[] primpoints(int opinput; int prim_number) { return #[]; }

/- Converts a primitive/vertex pair into a linear vertex.

Given a primitive and a vertex index on that primitive, returns the corresponding linear vertex index. Linear vertex indices are unique across the entire geometry.

**Parameters:**
- `opinput`: The input number (`0`-`3`) to read from.
- `prim_number`: The primitive number.
- `vertex_index`: The vertex index on the primitive (0-based).

**Returns:** The linear vertex index, or `-1` if the primitive or vertex index is invalid. -/
vexfunction int primvertex(int opinput; int prim_number; int vertex_index) { return 0; }

/- Returns the number of vertices in a primitive.

Returns how many vertices the primitive has. This is useful for iterating over all vertices of a primitive.

**Parameters:**
- `opinput`: The input number (`0`-`3`) to read from.
- `prim_number`: The primitive to query.

**Returns:** The number of vertices on the primitive. -/
vexfunction int primvertexcount(int opinput; int prim_number) { return 0; }

/- Returns the list of vertices on a primitive.

Returns an array of linear vertex indices for all vertices on the primitive, in the order they appear on the primitive.

**Parameters:**
- `opinput`: The input number (`0`-`3`) to read from.
- `prim_number`: The primitive to query.

**Returns:** An array of linear vertex indices on the primitive, in vertex order. -/
vexfunction int[] primvertices(int opinput; int prim_number) { return #[]; }

/- Returns primitives potentially intersecting a bounding box.

Performs a spatial query using an acceleration structure to find primitives whose bounding boxes overlap with the specified region. This is much faster than testing every primitive individually.

**Parameters:**
- `opinput`: The input number (`0`-`3`) to read from.
- `min`: The minimum corner of the axis-aligned bounding box (as a vector).
- `max`: The maximum corner of the axis-aligned bounding box (as a vector).

**Returns:** An array of primitive numbers whose bounding boxes potentially intersect the query box. Note that this returns candidates; some may not actually intersect. -/
vexfunction int[] primfind(int opinput; vector min; vector max) { return #[]; }

/- Returns primitives in a group potentially intersecting a bounding box.

Limits the spatial query to primitives in a specific group.

**Parameters:**
- `opinput`: The input number (`0`-`3`) to read from.
- `primgroup`: The name of a primitive group to limit the search to. Only primitives in this group are considered.
- `min`: The minimum corner of the bounding box.
- `max`: The maximum corner of the bounding box.

**Returns:** An array of primitive numbers from the specified group whose bounding boxes potentially intersect the query box. -/
vexfunction int[] primfind(int opinput; string primgroup; vector min; vector max) { return #[]; }

/- Returns the edge neighbors of a polygon.

Returns all primitives that share at least one edge with the given polygon. Two primitives share an edge if they have two consecutive vertices that reference the same two points (in opposite order).

**Parameters:**
- `opinput`: The input number (`0`-`3`) to read from.
- `prim_number`: The primitive (must be a polygon) to query.

**Returns:** An array of primitive numbers that share at least one edge with the specified primitive. -/
vexfunction int[] polyneighbours(int opinput; int prim_number) { return #[]; }

/- Rewires a vertex in a primitive to reference a different point.

Changes which point a specific vertex on a primitive references. This allows you to modify the topology of the geometry.

**Parameters:**
- `geohandle`: A handle to the geometry to write to. Currently the only valid value is `0` or `geoself()`.
- `prim_number`: The primitive containing the vertex to modify.
- `vertex_index`: The index of the vertex on the primitive to modify (0-based).
- `point_number`: The point number that the vertex should now reference.

**Returns:** `1` on success, `0` on failure. -/
vexfunction int setprimvertex(int geohandle; int prim_number; int vertex_index; int point_number) { return 0; }

/- Returns the intrinsic UV location of a point in a primitive.

For a point that is part of a primitive, returns its intrinsic UV parametric coordinates within that primitive. This is different from texture UV coordinates stored as attributes.

**Parameters:**
- `opinput`: The input number (`0`-`3`) to read from.
- `prim_number`: The primitive number.
- `point_number`: The point number that is part of the primitive.

**Returns:** A vector containing the intrinsic UV coordinates, where the x component is the U coordinate, the y component is the V coordinate, and the z component is unused. -/
vexfunction vector pointprimuv(int opinput; int prim_number; int point_number) { return {0,0,0}; }

-- ============================================================================
-- Vertex Management
-- ============================================================================

/- Adds a vertex to a primitive.

Creates a new vertex on the specified primitive that references the given point. The vertex is added at the end of the primitive's vertex list.

**Parameters:**
- `geohandle`: A handle to the geometry to write to. Currently the only valid value is `0` or `geoself()`.
- `prim_number`: The primitive to add the vertex to.
- `point_number`: The point number that the new vertex should reference.

**Returns:** The linear vertex index of the created vertex, or `-1` on failure. -/
vexfunction int addvertex(int geohandle; int prim_number; int point_number) { return 0; }

/- Removes a vertex from the geometry.

Removes a vertex from its primitive. This does not remove the point that the vertex references; it only removes the vertex itself.

**Parameters:**
- `geohandle`: A handle to the geometry to write to. Currently the only valid value is `0` or `geoself()`.
- `linear_vertex_index`: The linear vertex index to remove.

**Returns:** `1` on success, `0` on failure. -/
vexfunction int removevertex(int geohandle; int linear_vertex_index) { return 0; }

/- Returns the number of vertices in the geometry file.

Queries a geometry file on disk to determine how many vertices it contains total.

**Parameters:**
- `filename`: The name of the geometry file to query (e.g., `"defgeo.bgeo"`).

**Returns:** The total number of vertices in the geometry file. -/
vexfunction int nvertices(string filename) { return 0; }

/- Returns the number of vertices in the input.

Queries one of the node's inputs to determine how many vertices it contains total.

**Parameters:**
- `opinput`: The input number (`0`-`3`) to read from.

**Returns:** The total number of vertices in the input geometry. -/
vexfunction int nvertices(int opinput) { return 0; }

/- Returns the number of vertices in a group.

Counts how many vertices are members of the specified vertex group.

**Parameters:**
- `opinput`: The input number (`0`-`3`) to read from.
- `groupname`: The name of a vertex group to query.

**Returns:** The number of vertices in the specified vertex group. -/
vexfunction int nverticesgroup(int opinput; string groupname) { return 0; }

/- Returns the point number referenced by a linear vertex.

Given a linear vertex index, returns which point that vertex references.

**Parameters:**
- `opinput`: The input number (`0`-`3`) to read from.
- `linear_vertex_index`: The linear vertex index to query.

**Returns:** The point number that the vertex references, or `-1` if the vertex index is invalid. -/
vexfunction int vertexpoint(int opinput; int linear_vertex_index) { return 0; }

/- Returns the primitive number containing a vertex.

Given a linear vertex index, returns which primitive the vertex belongs to.

**Parameters:**
- `opinput`: The input number (`0`-`3`) to read from.
- `linear_vertex_index`: The linear vertex index to query.

**Returns:** The primitive number that contains the vertex, or `-1` if the vertex index is invalid. -/
vexfunction int vertexprim(int opinput; int linear_vertex_index) { return 0; }

/- Converts a linear vertex index into a primitive vertex number.

Given a linear vertex index (unique across the whole geometry), returns its index within its primitive (0-based position in that primitive's vertex list).

**Parameters:**
- `opinput`: The input number (`0`-`3`) to read from.
- `linear_vertex_index`: The linear vertex index to query.

**Returns:** The vertex's index within its primitive (0 for first vertex, 1 for second, etc.), or `-1` if the vertex index is invalid. -/
vexfunction int vertexprimindex(int opinput; int linear_vertex_index) { return 0; }

/- Converts a primitive/vertex pair into a linear vertex index.

Given a primitive number and a vertex index on that primitive, returns the corresponding linear vertex index.

**Parameters:**
- `opinput`: The input number (`0`-`3`) to read from.
- `prim_number`: The primitive number.
- `vertex_index`: The vertex index within the primitive (0-based).

**Returns:** The linear vertex index (unique across the whole geometry), or `-1` if the primitive or vertex index is invalid. -/
vexfunction int vertexindex(int opinput; int prim_number; int vertex_index) { return 0; }

/- Returns the next vertex sharing the same point.

Given a vertex, returns the linear index of the next vertex that references the same point. This allows iteration through all vertices connected to a single point. The vertices form a circular linked list.

**Parameters:**
- `opinput`: The input number (`0`-`3`) to read from.
- `linear_vertex_index`: The linear vertex index to query.

**Returns:** The linear vertex index of the next vertex sharing the same point. If this is the last vertex sharing the point, it wraps around to the first vertex. Returns `-1` if the vertex index is invalid. -/
vexfunction int vertexnext(int opinput; int linear_vertex_index) { return 0; }

/- Returns the previous vertex sharing the same point.

Given a vertex, returns the linear index of the previous vertex that references the same point. This is the inverse of `vertexnext()`.

**Parameters:**
- `opinput`: The input number (`0`-`3`) to read from.
- `linear_vertex_index`: The linear vertex index to query.

**Returns:** The linear vertex index of the previous vertex sharing the same point. If this is the first vertex sharing the point, it wraps around to the last vertex. Returns `-1` if the vertex index is invalid. -/
vexfunction int vertexprev(int opinput; int linear_vertex_index) { return 0; }

/- Returns the parametric coordinate of a vertex along its primitive's perimeter.

For curves and polygons, returns a value from 0 to 1 indicating where along the perimeter of the primitive the vertex lies. This is useful for operations that need to know relative position along a curve.

**Parameters:**
- `opinput`: The input number (`0`-`3`) to read from.
- `linear_vertex_index`: The linear vertex index to query.

**Returns:** The parametric coordinate (0 to 1) along the primitive's perimeter. Returns `0` if the primitive type doesn't support this operation or if the vertex index is invalid. -/
vexfunction float vertexcurveparam(int opinput; int linear_vertex_index) { return 0.0; }

/- Rewires a vertex to reference a different point.

Changes which point a vertex references. This allows you to modify the topology of the geometry.

**Parameters:**
- `geohandle`: A handle to the geometry to write to. Currently the only valid value is `0` or `geoself()`.
- `linear_vertex_index`: The linear vertex index to modify.
- `point_number`: The point number that the vertex should now reference.

**Returns:** `1` on success, `0` on failure. -/
vexfunction int setvertexpoint(int geohandle; int linear_vertex_index; int point_number) { return 0; }

-- ============================================================================
-- Edge and Group Management
-- ============================================================================

/- Tests if an edge is in a group.

Returns whether the edge between two points is a member of the specified edge group. An edge exists if the two points are consecutive vertices in at least one primitive.

**Parameters:**
- `opinput`: The input number (`0`-`3`) to read from.
- `groupname`: The name of the edge group to test membership in.
- `point1`: The first point of the edge.
- `point2`: The second point of the edge.

**Returns:** `1` if the edge is in the group, `0` if it is not in the group or if no edge exists between the points. -/
vexfunction int inedgegroup(int opinput; string groupname; int point1; int point2) { return 0; }

/- Returns the number of edges in a group.

Counts how many edges are members of the specified edge group.

**Parameters:**
- `opinput`: The input number (`0`-`3`) to read from.
- `groupname`: The name of the edge group to query.

**Returns:** The number of edges in the specified edge group. -/
vexfunction int nedgesgroup(int opinput; string groupname) { return 0; }

/- Sets edge group membership.

Adds an edge to or removes an edge from an edge group. An edge must exist (i.e., the two points must be consecutive vertices in at least one primitive) for this to succeed.

**Parameters:**
- `geohandle`: A handle to the geometry to write to. Currently the only valid value is `0` or `geoself()`.
- `groupname`: The name of the edge group to modify. The group will be created if it doesn't exist.
- `point1`: The first point of the edge.
- `point2`: The second point of the edge.
- `value`: If non-zero, adds the edge to the group. If zero, removes the edge from the group.

**Returns:** `1` on success, `0` on failure (e.g., if the edge doesn't exist). -/
vexfunction int setedgegroup(int geohandle; string groupname; int point1; int point2; int value) { return 0; }

-- ============================================================================
-- Attribute Management
-- ============================================================================

/- Removes an attribute or group from the geometry.

Deletes an attribute or group of the specified class from the geometry. This affects all elements of that class.

**Parameters:**
- `geohandle`: A handle to the geometry to write to. Currently the only valid value is `0` or `geoself()`.
- `attribclass`: The class of attribute to remove. Must be one of: `"detail"` (global attribute), `"primitive"` (per-primitive), `"point"` (per-point), or `"vertex"` (per-vertex).
- `name`: The name of the attribute or group to remove.

**Returns:** `1` if the attribute or group was successfully removed, `0` if it didn't exist or couldn't be removed. -/
vexfunction int removeattrib(int geohandle; string attribclass; string name) { return 0; }

-- ============================================================================
-- Ray Intersection
-- ============================================================================

/- Computes the first intersection of a ray with geometry.

Casts a ray from an origin point in a given direction and returns information about the first primitive hit. This is the most basic ray intersection function.

**Parameters:**
- `opinput`: The input number (`0`-`3`) to read from.
- `orig`: The origin of the ray in world space (as a vector).
- `dir`: The direction vector of the ray. Does not need to be normalized, but should not be zero.
- `P`: Output parameter - set to the world space position of the intersection point.
- `u`: Output parameter - set to the U parametric coordinate on the hit primitive.
- `v`: Output parameter - set to the V parametric coordinate on the hit primitive.

**Returns:** The primitive number that was hit, or `-1` if the ray didn't hit any geometry. -/
vexfunction int intersect(int opinput; vector orig; vector dir; vector P; float u; float v) { return 0; }

/- Computes the first intersection of a ray with geometry with tolerance.

Adds a tolerance parameter for controlling intersection precision.

**Parameters:**
- `opinput`: The input number (`0`-`3`) to read from.
- `orig`: The origin of the ray in world space.
- `dir`: The direction vector of the ray.
- `P`: Output parameter - the intersection position.
- `u`: Output parameter - the U parametric coordinate.
- `v`: Output parameter - the V parametric coordinate.
- `tol`: The tolerance for intersection testing. Use `-1` for default tolerance. Larger values make the ray "thicker" and more likely to hit.

**Returns:** The primitive number that was hit, or `-1` if no intersection was found. -/
vexfunction int intersect(int opinput; vector orig; vector dir; vector P; float u; float v; float tol) { return 0; }

/- Computes the first intersection of a ray with geometry with tolerance and maximum distance.

Adds a maximum ray distance to limit the search range.

**Parameters:**
- `opinput`: The input number (`0`-`3`) to read from.
- `orig`: The origin of the ray in world space.
- `dir`: The direction vector of the ray.
- `P`: Output parameter - the intersection position.
- `u`: Output parameter - the U parametric coordinate.
- `v`: Output parameter - the V parametric coordinate.
- `tol`: The tolerance for intersection testing. Use `-1` for default.
- `tmax`: The maximum distance along the ray to search. Intersections beyond this distance are ignored.

**Returns:** The primitive number that was hit within `tmax` distance, or `-1` if no intersection was found. -/
vexfunction int intersect(int opinput; vector orig; vector dir; vector P; float u; float v; float tol; float tmax) { return 0; }

/- Computes all intersections of a ray with geometry.

Casts a ray and returns information about all primitives hit along the ray, not just the first one. The results are ordered by distance from the ray origin.

**Parameters:**
- `opinput`: The input number (`0`-`3`) to read from.
- `orig`: The origin of the ray in world space.
- `dir`: The direction vector of the ray.
- `P`: Output parameter - an array of intersection positions in world space.
- `u`: Output parameter - an array of U parametric coordinates, one per intersection.
- `v`: Output parameter - an array of V parametric coordinates, one per intersection.

**Returns:** An array of primitive numbers that were hit, in order from nearest to farthest. The arrays `P`, `u`, and `v` will have the same length as the returned array. -/
vexfunction int[] intersect_all(int opinput; vector orig; vector dir; vector P[]; float u[]; float v[]) { return #[]; }

/- Computes all intersections of a ray with geometry with tolerance.

**Parameters:**
- `opinput`: The input number (`0`-`3`) to read from.
- `orig`: The origin of the ray.
- `dir`: The direction of the ray.
- `P`: Output parameter - array of intersection positions.
- `u`: Output parameter - array of U coordinates.
- `v`: Output parameter - array of V coordinates.
- `tol`: The tolerance for intersection testing. Use `-1` for default.

**Returns:** An array of primitive numbers that were hit, in order by distance. -/
vexfunction int[] intersect_all(int opinput; vector orig; vector dir; vector P[]; float u[]; float v[]; float tol) { return #[]; }

/- Computes all intersections of a ray with geometry with tolerance and maximum distance.

**Parameters:**
- `opinput`: The input number (`0`-`3`) to read from.
- `orig`: The origin of the ray.
- `dir`: The direction of the ray.
- `P`: Output parameter - array of intersection positions.
- `u`: Output parameter - array of U coordinates.
- `v`: Output parameter - array of V coordinates.
- `tol`: The tolerance for intersection testing. Use `-1` for default.
- `tmax`: The maximum distance along the ray to search for intersections.

**Returns:** An array of primitive numbers that were hit within `tmax` distance, in order by distance. -/
vexfunction int[] intersect_all(int opinput; vector orig; vector dir; vector P[]; float u[]; float v[]; float tol; float tmax) { return #[]; }

/- Computes ray intersection in UV space.

Intersects a ray with the geometry in UV texture space rather than world space. This is useful for texture-based operations.

**Parameters:**
- `opinput`: The input number (`0`-`3`) to read from.
- `uvname`: The name of the UV attribute to use (e.g., `"uv"`, `"uv2"`). This attribute must exist on vertices or points.
- `orig`: The origin of the ray in UV space (2D, z component ignored).
- `dir`: The direction of the ray in UV space (2D, z component ignored).
- `P`: Output parameter - the intersection position in UV space.
- `u`: Output parameter - the U parametric coordinate on the hit primitive (this is different from the UV space U).
- `v`: Output parameter - the V parametric coordinate on the hit primitive (this is different from the UV space V).

**Returns:** The primitive number that was hit, or `-1` if no intersection was found. -/
vexfunction int uvintersect(int opinput; string uvname; vector orig; vector dir; vector P; float u; float v) { return 0; }

/- Computes ray intersection in UV space with tolerance.

**Parameters:**
- `opinput`: The input number (`0`-`3`) to read from.
- `uvname`: The name of the UV attribute to use.
- `orig`: The origin of the ray in UV space.
- `dir`: The direction of the ray in UV space.
- `P`: Output parameter - the intersection position in UV space.
- `u`: Output parameter - the U parametric coordinate.
- `v`: Output parameter - the V parametric coordinate.
- `tol`: The tolerance for intersection testing. Use `-1` for default.

**Returns:** The primitive number that was hit, or `-1` if no intersection was found. -/
vexfunction int uvintersect(int opinput; string uvname; vector orig; vector dir; vector P; float u; float v; float tol) { return 0; }

/- Computes ray intersection in UV space with tolerance and maximum distance.

**Parameters:**
- `opinput`: The input number (`0`-`3`) to read from.
- `uvname`: The name of the UV attribute to use.
- `orig`: The origin of the ray in UV space.
- `dir`: The direction of the ray in UV space.
- `P`: Output parameter - the intersection position in UV space.
- `u`: Output parameter - the U parametric coordinate.
- `v`: Output parameter - the V parametric coordinate.
- `tol`: The tolerance for intersection testing. Use `-1` for default.
- `tmax`: The maximum distance along the ray in UV space to search for intersections.

**Returns:** The primitive number that was hit within `tmax` distance, or `-1` if no intersection was found. -/
vexfunction int uvintersect(int opinput; string uvname; vector orig; vector dir; vector P; float u; float v; float tol; float tmax) { return 0; }

-- ============================================================================
-- Geometry Utilities
-- ============================================================================

/- Returns a handle to the current geometry.

Returns a geometry handle that can be used with geometry modification functions like `addpoint()`, `removeprim()`, etc. This is typically used in wrangle nodes to modify the geometry being processed.

**Returns:** A handle to the current geometry. This is typically `0`, representing the geometry being processed by the current node. -/
vexfunction int geoself() { return 0; }

/- Returns an oppath string to unwrap the geometry.

Creates an operation path string that can be used to reference the input geometry in a form suitable for in-place modification. This is used internally by the geometry modification functions.

**Parameters:**
- `opinput`: The input number (`0`-`3`) to unwrap.

**Returns:** A string containing the operation path to the unwrapped geometry, in the format `"op:/path/to/node"`. -/
vexfunction string geounwrap(int opinput) { return ""; }

/- Clips a line segment to a bounding box.

Clips the line segment from `p0` to `p1` against an axis-aligned bounding box defined by `min` and `max`. Returns the clipped endpoints if any part of the line is inside the box.

**Parameters:**
- `p0`: The start point of the line segment in world space.
- `p1`: The end point of the line segment in world space.
- `min`: The minimum corner of the axis-aligned bounding box.
- `max`: The maximum corner of the axis-aligned bounding box.
- `clipped_p0`: Output parameter - set to the clipped start point. If the entire segment is inside, this equals `p0`. If the segment crosses into the box, this is the entry point.
- `clipped_p1`: Output parameter - set to the clipped end point. If the entire segment is inside, this equals `p1`. If the segment crosses out of the box, this is the exit point.

**Returns:** `1` if any part of the line segment is inside or intersects the bounding box (and the output parameters are set), `0` if the entire segment is outside the box (output parameters are undefined). -/
vexfunction int clip(vector p0; vector p1; vector min; vector max; vector clipped_p0; vector clipped_p1) { return 0; }
