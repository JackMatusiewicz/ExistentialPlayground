namespace ExistentialPlayground

type State<'s, 'a> = 's -> ('a * 's)

module State =

    let lift a : State<'s, 'a> = fun s -> (a,s)

    let map
        (f : 'a -> 'b)
        (s1 : State<'s, 'a>)
        : State<'s, 'b>
        =
        fun s ->
            let (a,s) = s1 s
            (f a, s)

    let apply
        (f : State<'s, 'a -> 'b>)
        (a : State<'s, 'a>)
        : State<'s, 'b>
        =
        fun v ->
            let (a, midS) = a v
            let (f, finS) = f midS
            (f a, finS)

    let bind
        (f : 'a -> State<'s, 'b>)
        (v : State<'s, 'a>)
        : State<'s, 'b>
        =
        fun s ->
            let (a, s) = v s
            let sb = f a
            sb s
