namespace ExistentialPlayground

type HList<'a> =
    | End of Teq<'a, unit>
    | Cons of HListCrate<'a>

and HListCrate<'a> =
    abstract member Apply<'r> : HListEvaluator<'a, 'r> -> 'r

and HListEvaluator<'a, 'r> =
    abstract member Eval : 'b -> 'c HList -> Teq<'a, 'b -> 'c> -> 'r

type HListFolder<'state> =
    abstract member Fold<'a> : 'state -> 'a -> 'state

module HList =

    let private lift (t : Teq<'a, 'b>) : Teq<HList<'a>, HList<'b>> =
        Teq.believeMe

    let empty : HList<unit> = Teq.refl<unit> |> End

    let cons (v : 'a) (tail : HList<'b>) : HList<'a -> 'b> =
        { new HListCrate<'a -> 'b> with
            member __.Apply<'r> (evaluator : HListEvaluator<('a -> 'b), 'r>) =
                evaluator.Eval v tail Teq.refl<'a -> 'b>
        } |> Cons

    let head<'a, 'b> (h : HList<'a -> 'b>) : 'a * HList<'b> =
        match h with
        | End _ -> failwith "Impossible - we have a proof the generic type isn't Unit"
        | Cons h ->
            h.Apply<'a * HList<'b>>
                { new HListEvaluator<'a -> 'b, 'a * HList<'b>> with
                    member __.Eval v tail teq =
                        let t2 = Teq.domain teq
                        let t3 = Teq.codomain teq |> lift
                        (v |> Teq.castFrom t2), (tail |> Teq.castFrom t3)
                }

    let apply<'a, 'b, 'c> (f : 'a -> 'c) (h : HList<'a -> 'b>) : 'c * HList<'b> =
        match h with
        | End _ -> failwith "Impossible - we have a proof the generic type isn't Unit"
        | Cons h ->
            h.Apply<'c * HList<'b>>
                { new HListEvaluator<'a -> 'b, 'c * HList<'b>> with
                    member __.Eval v tail teq =
                        let t2 = Teq.domain teq
                        let t3 = Teq.codomain teq |> lift
                        (v |> Teq.castFrom t2 |> f, tail |> Teq.castFrom t3)
                }

    let rec fold<'a, 'state>
        (list : 'a HList)
        (acc : 'state)
        (folder : 'state HListFolder)
        : 'state
        =
        match list with
        | End _ -> acc
        | Cons hc ->
            hc.Apply { new HListEvaluator<'a, 'state> with
                member __.Eval<'b, 'c> (v :'b) (tail : 'c HList) teq =
                    let newAcc = folder.Fold<'b> acc v
                    fold<'c, 'state> tail newAcc folder
            }