namespace ExistentialPlayground

type HList<'a> =
    | End of Teq<'a, unit>
    | Cons of HListCrate<'a>

and HListCrate<'a> =
    abstract member Bind<'r> : HListEvaluator<'a, 'r> -> 'r

and HListEvaluator<'a, 'r> =
    abstract member Eval<'b, 'c> : 'b -> 'c HList -> Teq<'a, 'b -> 'c> -> 'r

module HList =

    let empty : HList<unit> = Teq.refl<unit> |> End

    let cons (v : 'a) (tail : HList<'b>) : HList<'a -> 'b> =
        { new HListCrate<'a -> 'b> with
            member __.Bind<'r> (evaluator : HListEvaluator<('a -> 'b), 'r>) =
                evaluator.Eval<'a, 'b> v tail Teq.refl<'a -> 'b>
        } |> Cons
