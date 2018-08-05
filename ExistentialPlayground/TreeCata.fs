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

    let makeLeaf (x : 'a) =
        { new LeafCrate with
            member __.Bind evalr = evalr.Eval x
        } |> Leaf

    let identityCata<'a> : TreeCata<'a Tree> =
        { new TreeCata<_> with
            member __.Empty () = Empty
            member __.Leaf x = makeLeaf x
            member __.Branch l r = Branch (l,r)
        }

    let stringCata : TreeCata<string> =
        { new TreeCata<_> with
            member __.Empty () = "Empty"
            member __.Leaf x = x.ToString ()
            member __.Branch l r = sprintf "BRANCH (%s. %s)" l r
        }

    let rec cata (c : TreeCata<'a>) (tree : 'b Tree) : 'a =
        match tree with
        | Empty -> c.Empty ()
        | Leaf x ->
            x.Bind
                { new LeafEvaluator<_> with
                    member __.Eval a = c.Leaf a
                }
        | Branch (l,r) ->
            let l = cata c l
            let r = cata c r
            c.Branch l r