// Duo types can be implicitly casted into Result<T,E> types
// Option types can also be casted into Result<T,E> types

export Result[T, E]: enum {
    Ok = T
    Error = E
}

export Option[T]: Result[T, ()]
export OptionErr[E]: Result[(), E]
