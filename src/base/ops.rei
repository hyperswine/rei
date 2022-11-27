#*
    Operation Traits
*#

// each base::op should only have one mapping and all defined...?
// @op = Plus
export Add: trait (self, rhs: Rhs*) -> Res* 

export PartialOrd: (Gt, Lt, Gte, Lte)
