﻿namespace ExistentialPlayground

type Teq<'a, 'b> = private Teq of ('a -> 'b) * ('b -> 'a)

module Teq =

    let refl<'a> : Teq<'a, 'a> = Teq (id, id)

    let believeMe<'a, 'b> : Teq<'a, 'b> = unbox refl<'a>

    let castTo (Teq (f,g)) b = g b
    let castFrom (Teq (f,g)) a = f a

    let domain (_ : Teq<'a -> 'b, 'c -> 'd>) : Teq<'a, 'c> = believeMe
    let codomain (_ : Teq<'a -> 'b, 'c -> 'd>) : Teq<'b, 'd> = believeMe
