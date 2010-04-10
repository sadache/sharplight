module FunUtils
let flip f = fun b  a-> f a b
type 'a Endo= 'a -> 'a



let splitAt n xs=(LazyList.take n xs,LazyList.skip n xs)

