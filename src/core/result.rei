// Duo types can be implicitly casted into Result<T,E> types
// Option types can also be casted into Result<T,E> types

Result[T, E]: enum {
    Ok = T
    Error = E
}

Option[T]: Result[T, ()]
OptionErr[E]: Result[(), E]
