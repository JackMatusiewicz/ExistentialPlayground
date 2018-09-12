module TypeLeak

type SimpleEvaluator<'r> =
    abstract member Eval<'a> : 'a list -> 'r
type SimpleCrate =
    abstract member Apply<'r> : SimpleEvaluator<'r> -> 'r


type SimpleDu =
    | Empty
    | Single of SimpleCrate
    | Double of SimpleCrate

//You should be able able to have both of these parameters of type 'a list, as they are different cases.
//However, the type from the above case has leaked into the lower - we can solve this with explicit <'a> on eval
let print (sc : SimpleDu) =
    match sc with
    | Empty -> "empty"
    | Single s ->
        s.Apply { new SimpleEvaluator<_> with
            member __.Eval<'a> (l : 'a list) = sprintf "%A" l
        }
    | Double sc ->
        sc.Apply { new SimpleEvaluator<_> with
            member __.Eval<'a> (l : 'a list) = sprintf "%A" l
        }