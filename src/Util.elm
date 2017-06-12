module Util exposing (..)

import List


rotateList : Int -> List a -> List a
rotateList count list =
    let
        r =
            count % (List.length list)
    in
        List.drop r list ++ List.take r list


zipList : List a -> List b -> List ( a, b )
zipList a b =
    List.map2 (,) a b


listProduct : List a -> List b -> List ( a, b )
listProduct xs ys =
    List.concatMap (\x -> List.map (\y -> ( x, y )) ys) xs


maybeCall : Maybe a -> b -> (a -> b) -> b
maybeCall value default callback =
    case value of
        Just v ->
            callback v

        Nothing ->
            default
