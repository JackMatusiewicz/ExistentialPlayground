namespace ExistentialPlayground

type 'a Tree =
    | Empty
    | Leaf of LeafCrate
    | Branch of ('a Tree) * ('a Tree)

and LeafCrate =
    abstract member Bind : LeafEvaluator<'r> -> 'r

and LeafEvaluator<'r> =
    abstract member Eval<'a> : 'a -> 'r

type TreeCata<'r> =
    abstract member Empty : unit -> 'r
    abstract member Leaf<'a> : 'a -> 'r
    abstract member Branch : 'r -> 'r -> 'r

module TreeCata =

    let identityCata<'a> : TreeCata<'a Tree> =
        { new TreeCata<_> with
            member __.Empty () = Empty
            member __.Leaf x =
                { new LeafCrate with
                    member __.Bind evalr = evalr.Eval x
                } |> Leaf
            member __.Branch l r = Branch (l,r)
        }