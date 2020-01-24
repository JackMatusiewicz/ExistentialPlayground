namespace ExistentialPlayground

type Teq<'a, 'b> = private Teq of ('a -> 'b) * ('b -> 'a)

module Teq =

    let private tupleBimap (f : 'a -> 'b) (g : 'c -> 'd) (a,b) = (f a), (g b)

    let refl<'a> : Teq<'a, 'a> = Teq (id, id)

    let believeMe<'a, 'b> : Teq<'a, 'b> = unbox refl<'a>

    let castFrom (Teq (f,g)) b = g b
    let castTo (Teq (f,g)) a = f a

    let domain (_ : Teq<'a -> 'b, 'c -> 'd>) : Teq<'a, 'c> = believeMe
    let codomain (_ : Teq<'a -> 'b, 'c -> 'd>) : Teq<'b, 'd> = believeMe

    // It would be more efficient to use believeMe. However, this proves it is correct.
    let mkTuple (Teq (aToC, cToA): Teq<'a,'c>) (Teq (bToD, dToB) : Teq<'b, 'd>)
        : Teq<'a * 'b,'c * 'd>
        =
        Teq ((tupleBimap aToC bToD), (tupleBimap cToA dToB))