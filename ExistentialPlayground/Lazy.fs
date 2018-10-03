module Lazy

    let map (f : 'a -> 'b) (a : 'a Lazy) : 'b Lazy =
        lazy(f a.Value)

    let apply (f : ('a -> 'b) Lazy) (a : 'a Lazy) : 'b Lazy =
        lazy (
            let f = f.Value
            let a = a.Value
            f a
        )

    let bind (a : 'a Lazy) (f : 'a -> 'b Lazy) : 'b Lazy =
        lazy(
            let a = a.Value
            (f a).Value
        )

    type LazyBuilder () =
        member __.Return a = lazy(a)
        member __.ReturnFrom a = a
        member __.Bind (a,f) = bind a f

    let lazyB = LazyBuilder ()