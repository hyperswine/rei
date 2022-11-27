#*
    Useful algebraic functions & Data frames
*#

// Vec2D<T1, T2> is defined in core
// Vec2D<T> is also defined and From<[[T]]> is defined

# Companion Data
export Matrix<T>: {
    _data: Vec2D<T>
}

export Matrix<T>: {
    // => means only consider the next self contained expr, usually for one liners
    new: (_data: &[[T]]) -> Self => Self{_data}
    new: (_data: &[T], rows: Size, cols: Size) -> Self {
        let size = _data.len()
        let new_data = Vec2D()
        for i in 0..rows {
            let col = (i + 1) * cols
            let row = (i + 1) * rows
            new_data.append(_data[row..col])
        }

        Self {
            new_data
        }
    }
}

// Matrix(...) => auto deduced

// You can have multiple systems of the same name, but with differing fields and fn signatures, etc
export Matrix<T>: {
    // impl Add() {}
    // impl Subtract() {}
    // impl Multiply() {}
    // impl Divide() {}

    // Add: impl
}

// type alias
export DataFrame: Matrix<String>

// a complex + a data object type promotes the data object into a companion object
export DataFrame: {}
