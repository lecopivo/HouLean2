import HouLean.OpenCL.Compiler.Main
-- import HouLean.OpenCL.Data.Int
import HouLean.OpenCL.Basic

/-! OpenCL Work Iterm Functions

Based on: https://registry.khronos.org/OpenCL/sdk/3.0/docs/man/html/get_group_id.html
-/

namespace HouLean.OpenCL

open Compiler Qq

/-- Returns the number of dimensions in use. This is the value given to the work_dim argument specified in `clEnqueueNDRangeKernel`.

OpenCL function: `uint get_work_dim()` -/
opaque getWorkDim : OpenCLM UInt32
impl_by : getWorkDim ==> get_work_dim()


/-- Returns the number of global work-items specified for dimension identified by dimindx. This value is given by the global_work_size argument to clEnqueueNDRangeKernel.

Valid values of dimindx are 0 to get_work_dim() - 1. For other values of dimindx, get_global_size() returns 1.

OpenCL function: `size_t get_global_size(uint dimindx)` -/
opaque getGlobalSize (dimindx : UInt32) : OpenCLM UInt32
impl_by (dimindx : UInt32) : getGlobalSize dimindx ==> get_global_size(dimindx)


/-- Returns the unique global work-item ID value for dimension identified by dimindx. The global work-item ID specifies the work-item ID based on the number of global work-items specified to execute the kernel.

Valid values of dimindx are 0 to get_work_dim() - 1. For other values of dimindx, get_global_id() returns 0.

OpenCL function: `size_t get_global_id(uint dimindx)` -/
opaque getGlobalId (dimindx : UInt32) : OpenCLM UInt32
impl_by (dimindx : UInt32) : getGlobalId dimindx ==> get_global_id(n)


/-- Returns the number of local work-items specified in dimension identified by dimindx. This value is at most the value given by the local_work_size argument to clEnqueueNDRangeKernel if local_work_size is not NULL; otherwise the OpenCL implementation chooses an appropriate local_work_size value which is returned by this function.

Valid values of dimindx are 0 to get_work_dim() - 1. For other values of dimindx, get_local_size() returns 1.

OpenCL function: `size_t get_local_size(uint dimindx)` -/
opaque getLocalSize (dimindx : UInt32) : OpenCLM UInt32
impl_by (dimidx : UInt32) : getLocalSize dimidx ==> get_local_size(dimidx)


/-- Returns the same value as that returned by get_local_size(dimindx) if the kernel is executed with a uniform work-group size. If the kernel is executed with a non-uniform work-group size, returns the number of local work-items in each of the work-groups that make up the uniform region of the global range.

Valid values of dimindx are 0 to get_work_dim() - 1. For other values of dimindx, get_enqueued_local_size() returns 1.

Requires support for OpenCL 2.0 or newer.

OpenCL function: `size_t get_enqueued_local_size(uint dimindx)` -/
opaque getEnqueuedLocalSize (dimindx : UInt32) : OpenCLM UInt32

instance : OpenCLFunction getEnqueuedLocalSize where
  name := "get_enqueued_local_size"

/-- Returns the unique local work-item ID, i.e. a work-item within a specific work-group for dimension identified by dimindx.

Valid values of dimindx are 0 to get_work_dim() - 1. For other values of dimindx, get_local_id() returns 0.

OpenCL function: `size_t get_local_id(uint dimindx)` -/
opaque getLocalId (dimindx : UInt32) : OpenCLM UInt32

instance : OpenCLFunction getLocalId where
  name := "get_local_id"

/-- Returns the number of work-groups that will execute a kernel for dimension identified by dimindx.

Valid values of dimindx are 0 to get_work_dim() - 1. For other values of dimindx, get_num_groups() returns 1.

OpenCL function: `size_t get_num_groups(uint dimindx)` -/
opaque getNumGroups (dimindx : UInt32) : OpenCLM UInt32

instance : OpenCLFunction getNumGroups where
  name := "get_num_groups"

/-- Returns the work-group ID which is a number from 0 .. get_num_groups(dimindx) - 1.

Valid values of dimindx are 0 to get_work_dim() - 1. For other values, get_group_id() returns 0.

OpenCL function: `size_t get_group_id(uint dimindx)` -/
opaque getGroupId (dimindx : UInt32) : OpenCLM UInt32

instance : OpenCLFunction getGroupId where
  name := "get_group_id"

/-- Returns the offset values specified in global_work_offset argument to clEnqueueNDRangeKernel.

Valid values of dimindx are 0 to get_work_dim() - 1. For other values, get_global_offset() returns 0.

Requires support for OpenCL C 1.1 or newer.

OpenCL function: `size_t get_global_offset(uint dimindx)` -/
opaque getGlobalOffset (dimindx : UInt32) : OpenCLM UInt32

instance : OpenCLFunction getGlobalOffset where
  name := "get_global_offset"

/-- Returns the work-items 1-dimensional global ID.

For 1D work-groups, it is computed as get_global_id(0) - get_global_offset(0).
For 2D work-groups, it is computed as (get_global_id(1) - get_global_offset(1)) * get_global_size(0) + (get_global_id(0) - get_global_offset(0)).
For 3D work-groups, it is computed as ((get_global_id(2) - get_global_offset(2)) * get_global_size(1) * get_global_size(0)) + ((get_global_id(1) - get_global_offset(1)) * get_global_size(0)) + (get_global_id(0) - get_global_offset(0)).

Requires support for OpenCL 2.0 or newer.

OpenCL function: `size_t get_global_linear_id()` -/
opaque getGlobalLinearId : OpenCLM UInt32

instance : OpenCLFunction getGlobalLinearId where
  name := "get_global_linear_id"

/-- Returns the work-items 1-dimensional local ID.

For 1D work-groups, it is the same value as get_local_id(0).
For 2D work-groups, it is computed as get_local_id(1) * get_local_size(0) + get_local_id(0).
For 3D work-groups, it is computed as (get_local_id(2) * get_local_size(1) * get_local_size(0)) + (get_local_id(1) * get_local_size(0)) + get_local_id(0).

Requires support for OpenCL 2.0 or newer.

OpenCL function: `size_t get_local_linear_id()` -/
opaque getLocalLinearId : OpenCLM UInt32

instance : OpenCLFunction getLocalLinearId where
  name := "get_local_linear_id"

/-- Returns the number of work-items in the sub-group. This value is no more than the maximum sub-group size and is implementation-defined based on a combination of the compiled kernel and the dispatch dimensions.

Requires support for the cl_khr_subgroups extension or OpenCL C 3.0 with __opencl_c_subgroups feature.

OpenCL function: `uint get_sub_group_size()` -/
opaque getSubGroupSize : OpenCLM UInt32

instance : OpenCLFunction getSubGroupSize where
  name := "get_sub_group_size"

/-- Returns the maximum size of a sub-group within the dispatch. This value will be invariant for a given set of dispatch dimensions and a kernel object compiled for a given device.

Requires support for the cl_khr_subgroups extension or OpenCL C 3.0 with __opencl_c_subgroups feature.

OpenCL function: `uint get_max_sub_group_size()` -/
opaque getMaxSubGroupSize : OpenCLM UInt32

instance : OpenCLFunction getMaxSubGroupSize where
  name := "get_max_sub_group_size"

/-- Returns the number of sub-groups that the current work-group is divided into. This number will be constant for the duration of a work-group's execution.

Requires support for the cl_khr_subgroups extension or OpenCL C 3.0 with __opencl_c_subgroups feature.

OpenCL function: `uint get_num_sub_groups()` -/
opaque getNumSubGroups : OpenCLM UInt32

instance : OpenCLFunction getNumSubGroups where
  name := "get_num_sub_groups"

/-- Returns the same value as that returned by get_num_sub_groups if the kernel is executed with a uniform work-group size. If the kernel is executed with a non-uniform work-group size, returns the number of sub-groups in each of the work-groups that make up the uniform region of the global range.

Requires support for the cl_khr_subgroups extension or OpenCL C 3.0 with __opencl_c_subgroups feature.

OpenCL function: `uint get_enqueued_num_sub_groups()` -/
opaque getEnqueuedNumSubGroups : OpenCLM UInt32

instance : OpenCLFunction getEnqueuedNumSubGroups where
  name := "get_enqueued_num_sub_groups"

/-- Returns the sub-group ID which is a number from 0 .. get_num_sub_groups() - 1. For clEnqueueTask, this returns 0.

Requires support for the cl_khr_subgroups extension or OpenCL C 3.0 with __opencl_c_subgroups feature.

OpenCL function: `uint get_sub_group_id()` -/
opaque getSubGroupId : OpenCLM UInt32

instance : OpenCLFunction getSubGroupId where
  name := "get_sub_group_id"

/-- Returns the unique work-item ID within the current sub-group. The mapping from get_local_id(dimindx) to get_sub_group_local_id will be invariant for the lifetime of the work-group.

Requires support for the cl_khr_subgroups extension or OpenCL C 3.0 with __opencl_c_subgroups feature.

OpenCL function: `uint get_sub_group_local_id()` -/
opaque getSubGroupLocalId : OpenCLM UInt32

instance : OpenCLFunction getSubGroupLocalId where
  name := "get_sub_group_local_id"

end HouLean.OpenCL
