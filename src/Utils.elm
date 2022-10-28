module Utils exposing (const,zip)

const : a -> b -> a
const x _ = x

zip : List a -> List b -> List (a,b)
zip xs ys = List.map2 Tuple.pair xs ys
