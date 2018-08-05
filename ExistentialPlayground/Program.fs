open System
open ExistentialPlayground

[<EntryPoint>]
let main argv =

    let four = TreeCata.makeLeaf 4
    let five = TreeCata.makeLeaf 5
    let nine = TreeCata.makeLeaf 9
    let c = TreeCata.makeLeaf 'c'

    let tree = Branch (Branch (four, c), Branch(five, Branch(nine, Empty)))
    let t2 = TreeCata.cata TreeCata.identity tree
    printfn "%s" <| TreeCata.cata TreeCata.toString tree
    printfn "%s" <| TreeCata.cata TreeCata.toString t2

    0
