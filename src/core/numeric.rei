#*
    Numeric Value Manipulation
*#

Int8: {}
Int16: {}
Int32: {}
Int64: {}
Int128: {}

UInt8: {}
UInt16: {}
UInt32: {}
UInt64: {}
UInt128: {}

Float8: {}
Float16: {}
Float32: {}
Float64: {}
Float128: {}

#*
    Arithmetic
*#

// associated types can also be declared with the where statement for trait fns
export Add<Rhs, Res>: trait (self, rhs: Rhs) -> Res
