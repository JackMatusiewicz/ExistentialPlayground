namespace ExistentialPlayground

type 'a Foo = unit -> 'a

// An HList where we can guarantee that everything inside it is a Foo.
type FooHList<'a> =
    | FEnd of Teq<'a, unit>
    | FCons of FooHListCrate<'a>

and FooHListCrate<'a> =
    abstract member Apply<'r> : FooHListEvaluator<'a, 'r> -> 'r

and FooHListEvaluator<'a, 'r> =
    abstract member Eval<'b, 'c> : Foo<'b> -> FooHList<'c> -> Teq<'a, 'b -> 'c> -> 'r

module FooHList =

    let empty = FEnd Teq.refl

    let lift (_ : Teq<'a, 'b>) : Teq<'a FooHList, 'b FooHList> =
        Teq.believeMe

    let cons (head : Foo<'a>) (tail : FooHList<'b>) : FooHList<'a -> 'b>
        =
        { new FooHListCrate<'a -> 'b> with
            member __.Apply<'r> (evaluator : FooHListEvaluator<('a -> 'b), 'r>) =
                evaluator.Eval<'a, 'b> head tail Teq.refl
        } |> FCons

    let head (a : ('a -> 'b) FooHList) : 'a * ('b FooHList) =
        match a with
        | FEnd _ -> failwith "This is impossible, we have a proof that the HList contains at least one element"
        | FCons c ->
            c.Apply<'a * ('b FooHList)>
                { new FooHListEvaluator<'a -> 'b, 'a * ('b FooHList)> with
                    member __.Eval<'a, 'b> aFoo bTail teq =
                        failwith "todo"
                }
