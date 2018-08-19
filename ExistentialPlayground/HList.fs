namespace ExistentialPlayground

type HList<'a> =
    | End of Teq<'a, unit>
    | Cons of HListCrate<'a>

and HListCrate<'a> =
    abstract member Bind<'r> : HListEvaluator<'a, 'r> -> 'r

and HListEvaluator<'a, 'r> =
    abstract member Eval<'b, 'c> : 'b -> 'c HList -> Teq<'a, 'b -> 'c> -> 'r