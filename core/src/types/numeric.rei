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

// ARITHMETIC

// associated types can also be declared with the where statement for trait fns
export Add<Rhs, Res>: trait (self, rhs: Rhs) -> Res

// examples:
/*
    data T()
    data K(i32)

    ? maybe you dont need to parameterise it like <Self, Self>, let IDE do it
    its pretty much overloadable in terms of impl anyway

    impl T: Add<Self, Self> {
        fn add(self, rhs: Self) -> Self {
            Self {}
        }
    }

    impl T: Add<K, K> {
        fn add(self, rhs: K) -> K {
            * note: copy constructor used here. You can also just return rhs to implicitly call the copy constructor
            * self and rhs are eaten up. Usually you would just @derive(Copy) to implicitly copy. It will have an overhead but ehh
            K(rhs)
        }
    }
*/
