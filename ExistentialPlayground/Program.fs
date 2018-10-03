open ExistentialPlayground
open Lazy

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

    let lazy5 = lazy(printfn "This is 5"; 5)
    let lazy6 = lazy(printfn "This is 6"; 6)

    let a = lazyB {
        let! a = lazy5
        let! b = lazy6
        return a + b
    }

    printfn "%A" (fst x)
    printfn "Lazy being forced now:"
    printfn "%d" <| a.Force ()
    0
