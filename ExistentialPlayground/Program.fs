open System
open ExistentialPlayground

[<EntryPoint>]
let main argv =

    let lst : HList<int -> string -> char -> unit> =
        HList.empty
        |> HList.cons 'c'
        |> HList.cons "Hello"
        |> HList.cons 25

    printfn "%A" <| lst.GetType().ToString()

    0
