namespace ExistentialPlayground

type HList<'a> =
    | End of Teq<'a, unit>
    | Cons of HListCrate<'a>

and HListCrate<'a> =
    abstract member Bind<'r> : HListEvaluator<'a, 'r> -> 'r

and HListEvaluator<'a, 'r> =
    abstract member Eval : 'b -> 'c HList -> Teq<'a, 'b -> 'c> -> 'r

module HList =

    let private lift (t : Teq<'a, 'b>) : Teq<HList<'a>, HList<'b>> =
        Teq.believeMe

    let empty : HList<unit> = Teq.refl<unit> |> End

    let cons (v : 'a) (tail : HList<'b>) : HList<'a -> 'b> =
        { new HListCrate<'a -> 'b> with
            member __.Bind<'r> (evaluator : HListEvaluator<('a -> 'b), 'r>) =
                evaluator.Eval v tail Teq.refl<'a -> 'b>
        } |> Cons

    let head<'a, 'b> (h : HList<'a -> 'b>) : 'a * HList<'b> =
        match h with
        | End _ -> failwith "Impossible"
        | Cons h ->
            h.Bind<'a * HList<'b>>
                { new HListEvaluator<'a -> 'b, 'a * HList<'b>> with
                    member __.Eval v tail teq =
                        let t2 = Teq.domain teq
                        let t3 = Teq.codomain teq |> lift
                        (v |> Teq.castTo t2), (tail |> Teq.castTo t3)
                }
