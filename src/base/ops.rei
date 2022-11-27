#*
    Operation Traits
*#

// each base::op should only have one mapping and all defined...?
// @op = Plus
export Add: trait (self, rhs: Rhs*) -> Res*
export Sub: trait (self, rhs: Rhs*) -> Res*
export Mul: trait (self, rhs: Rhs*) -> Res*
export Div: trait (self, rhs: Rhs*) -> Res*

export PartialOrd: (Gt, Lt, Gte, Lte)

export Gt: trait (self, rhs: Rhs*) -> Res*
export Lt: trait (self, rhs: Rhs*) -> Res*
export Gte: trait (self, rhs: Rhs*) -> Res*
export Lte: trait (self, rhs: Rhs*) -> Res*

// shifts
export ShiftLeft: trait (self, rhs: Rhs*) -> Res*
export ShiftRight: trait (self, rhs: Rhs*) -> Res*

// wait are all binary operations like this?
// also you should be able to append = to them like +=
// would == mean = appended? uhh prob no

export BinaryOperation[Rhs, Res]: trait (self, rhs: Rhs) -> Res

// in that case...
// uhh

// maybe core or something?
// base can try to map operators to operations
// maybe we can set something special for it like uhh some kind of list?
// this gives it a soul maybe
// so rei base might know of fundamental maths and utf8
// that way its soul is based on the present

// so we do have to define the traits, but maybe we can use a macro for it?

export TraitSet: macro (ident) => {

}

// for op in ops => TraitSet(op), so like Add
