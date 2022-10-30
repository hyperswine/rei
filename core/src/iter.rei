#*
    Iterator Patterns
*#

// should export(x) also make it visible here?

// iterators allow SIMD and easy parallelism when you dont need to depend on the prev values

export(prelude) Iter[Item]: trait {
    // overload 0
    map[Res]: (&self, f: Fn[Self::Item, Res]) -> Iter[Res]

    MapperIndex: (Self::Item, Index)

    // overload 1
    map[Res]: (&self, f: Fn[MapperIndex, Res]) -> Iter[Res]

    // foldr for better laziness
    // a start and one without a start? or default arg better here?
    // the start can be anything. The next is always based on accumulator?
    fold[Res, Accumulator]: (&self, start: Res = Res(), f: (Accumulator, Res) -> Res) -> Res

    filter[Res]: (&self, f: (Self::Item) -> bool) -> Iter[Res]

    first[Res]: (&self, f: (Self::Item) -> bool) -> Res
}

// NOTES:
// only each field in a mod needs a vis mod and are priv by default
// fields within fields are always public by default and cant be changed?
// yea cause of the API problem. You want to know what it does
// and you want to use the simple idiomatic way
// as highlighted or recommended

// the autocomplete by default highlights those?

// if you want to use a private thing, dont put it in a field
// flatten is a good pattern...
