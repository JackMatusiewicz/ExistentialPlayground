open ExistentialPlayground

let makeTuple (a : 'a) (b : 'b) (c : 'c) = (a,b,c)

let uncurry (f : 'a -> 'b -> 'c) (a,b) = f a b

[<EntryPoint>]
let main argv =

    let lst : HList<_> =
        HList.empty
        |> HList.cons 'c'
        |> HList.cons "Hello"
        |> HList.cons 25
        |> HList.cons 25.05

    let x =
        HList.apply<_,_,_> makeTuple lst
        |> (uncurry HList.apply<_,_,_>)
        |> (uncurry HList.apply<_,_,_>)

    printfn "%A" (fst x)
    0
