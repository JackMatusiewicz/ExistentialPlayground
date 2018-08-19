namespace ExistentialPlayground

type Teq<'a, 'b> = private Teq of ('a -> 'b) * ('b -> 'a)

module Teq =

    let refl<'a> : Teq<'a, 'a> = Teq (id, id)

    let castTo (Teq (f,g)) b = g b
    let castFrom (Teq (f,g)) a = f a
