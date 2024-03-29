#*
    Numeric Value Manipulation
*#

# 8-bit numeric
Int8: Numeric[8]
Int16: Numeric[16]
Int32: Numeric[32]
Int64: Numeric[64]
Int128: Numeric[128]

UInt8: Numeric[8]
UInt16: Numeric[16]
UInt32: Numeric[32]
UInt64: Numeric[64]
UInt128: Numeric[128]

Float8: Numeric[8]
Float16: Numeric[16]
Float32: Numeric[32]
Float64: Numeric[64]
Float128: Numeric[128]

// USEFUL ALIASES...
export i8: Int8
export i16: Int16
export i32: Int32
export i64: Int64
export i128: Int128

export Range[Index]: {
    start: Index
    end: Index
}

#*
    Arithmetic
*#

// associated types can also be declared with the where statement for trait fns
// associative functions
// export Add<Rhs, Res>: trait (self, rhs: Rhs) -> Res
// export Mult<Rhs, Res>: trait (self, rhs: Rhs) -> Res
// export Sub<Rhs, Res>: trait (self, rhs: Rhs) -> Res
// export Div<Rhs, Res>: trait (self, rhs: Rhs) -> Res
