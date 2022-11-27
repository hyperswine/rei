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
