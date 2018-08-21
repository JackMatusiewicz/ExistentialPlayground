open ExistentialPlayground

let makeTuple (a : 'a) (b : 'b) (c : 'c) = (a,b,c)

[<EntryPoint>]
let main argv =

    let lst : HList<int -> string -> char -> unit> =
        HList.empty
        |> HList.cons 'c'
        |> HList.cons "Hello"
        |> HList.cons 25

    let x =
        HList.apply<_,_,_> makeTuple lst
        |> (fun (f,t) -> HList.apply<_,_,_> f t)
        |> (fun (f,t) -> HList.apply<_,_,_> f t)

    printfn "%A" (fst x)
    0
