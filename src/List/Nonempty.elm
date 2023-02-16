module List.Nonempty exposing
    ( ListNonempty
    , singleton, repeat, range, fromPair
    , fromTuple, toTuple
    , fromList, toList
    , appendElem, prependElem, popFirst, popLast, map, indexedMap, foldl, foldl1, foldr, foldr1, filter, filterMap
    , length, isSingleton, reverse, member, all, any, maximum, minimum, sum, product
    , append, concat, concatMap, map2, map3, map4, map5
    , sort, sortBy, sortWith
    , head, tail, rest, last, uncons, unconsLast, take, drop, takeLast, dropLast, unzip2, unzip3
    )

{-| A library for a non-empty list type.


# Definition

@docs ListNonempty


# Create

@docs singleton, repeat, range, fromPair


# Tuples

@docs fromTuple, toTuple


# Lists

@docs fromList, toList


# Transform

@docs appendElem, prependElem, popFirst, popLast, map, indexedMap, foldl, foldl1, foldr, foldr1, filter, filterMap


# Utilities

@docs length, isSingleton, reverse, member, all, any, maximum, minimum, sum, product


# Combine

@docs append, concat, concatMap, map2, map3, map4, map5


# Sort

@docs sort, sortBy, sortWith


# Deconstruct

@docs head, tail, rest, last, uncons, unconsLast, take, drop, takeLast, dropLast, unzip2, unzip3

-}

import List.Extra


{-| A list type that must contain at least one element
-}
type ListNonempty a
    = ListNonempty ( a, List a )


{-| Create a non-empty list containing just a single element

    singleton 0

-}
singleton : a -> ListNonempty a
singleton elem =
    ListNonempty ( elem, [] )


{-| Similar to `List.repeat`, except this is guaranteed to contain
at least one element, regardless of the integer provided. Otherwise
a Maybe return value would be required. If this behavior is desired,
use `List.repeat` plus `fromList`

    repeat 3 "a" |> toList == [ "a", "a", "a" ]

    repeat 0 "a" |> toList == [ "a" ]

-}
repeat : Int -> a -> ListNonempty a
repeat n elem =
    {- always has at least one element, regardless of 'n' -}
    ListNonempty ( elem, List.repeat (n - 1) elem )


{-| Works slightly differently from `List.range`. First provided number
will be the first number in the non-empty list, second provided number
will be the last number, and it can ascend or descend.

Equivalent to `List.range a b` when `a<=b`, equal to `singleton a`
if `a>b`

    range 3 6 |> toList == [ 3, 4, 5, 6 ]

    range 3 3 |> toList == [ 3 ]

    range 6 3 |> toList == [ 6, 5, 4, 3 ]

-}
range : Int -> Int -> ListNonempty Int
range a b =
    case compare a b of
        EQ ->
            ListNonempty ( a, [] )

        LT ->
            ListNonempty ( a, List.range (a + 1) b )

        GT ->
            ListNonempty ( a, List.Extra.reverseRange (a - 1) b )


{-| Try to create a non-empty list from an ordinary list.
Returns Nothing if input list is empty.

    fromList [ 0, 1, 2 ] == Just (fromPair 0 [ 1, 2 ])

    fromList [] == Nothing

-}
fromList : List a -> Maybe (ListNonempty a)
fromList elems =
    case elems of
        head_ :: tail_ ->
            Just <| ListNonempty ( head_, tail_ )

        [] ->
            Nothing


{-| Create a non-empty list from an element and a list.

    fromPair 0 [ 1, 2, 3 ] |> toList == [ 0, 1, 2, 3 ]

-}
fromPair : a -> List a -> ListNonempty a
fromPair a b =
    ListNonempty ( a, b )


{-| Create a non-empty list from a tuple of element and list

    fromTuple ( 0, [ 1, 2, 3 ] ) |> toList == [ 0, 1, 2, 3 ]

-}
fromTuple : ( a, List a ) -> ListNonempty a
fromTuple ( a, b ) =
    fromPair a b


{-| Deconstruct a non-empty list into a tuple of element and list

    fromTuple ( 0, [ 1, 2, 3 ] )
        |> toTuple
        == ( 0, [ 1, 2, 3 ] )

-}
toTuple : ListNonempty a -> ( a, List a )
toTuple (ListNonempty ( a, b )) =
    ( a, b )


{-| Convert to a list

    fromPair 0 [ 1, 2, 3 ] |> toList == [ 0, 1, 2, 3 ]

-}
toList : ListNonempty a -> List a
toList (ListNonempty ( a, b )) =
    a :: b


{-| Append a value to become the new last element.

    append 4 [ 1, 2, 3 ]
        |> toList
        == [ 1, 2, 3, 4 ]

-}
appendElem : a -> ListNonempty a -> ListNonempty a
appendElem elem (ListNonempty ( a, b )) =
    ListNonempty ( a, b ++ [ elem ] )


{-| Prepend a value to become the new first element.

    prepend 0 [ 1, 2, 3 ]
        |> toList
        == [ 0, 1, 2, 3 ]

-}
prependElem : a -> ListNonempty a -> ListNonempty a
prependElem elem elems =
    ListNonempty ( elem, toList elems )


{-| Pop the first element.

    popFirst 4 [ 1, 2, 3 ]
        |> toList
        == [ 1, 2, 3, 4 ]

-> Maybe (a, ListNonempty a)
-> (a, Maybe (ListNonempty a))
-> (a, Either (List a) (ListNonempty a))

-}
popFirst : a -> ListNonempty a -> ListNonempty a
popFirst elem (ListNonempty ( a, b )) =
    ListNonempty ( a, b ++ [ elem ] )


{-| Pop the last element.

    popLast 4 [ 1, 2, 3 ]
        |> toList
        == [ 1, 2, 3, 4 ]

-> Maybe (a, ListNonempty a)
-> (a, Maybe (ListNonempty a))
-> (a, Either (List a) (ListNonempty a))

-}
popLast : a -> ListNonempty a -> ListNonempty a
popLast elem (ListNonempty ( a, b )) =
    ListNonempty ( a, b ++ [ elem ] )


{-| Equivalent to `List.map`

    fromPair 0 [ 1, 2, 3 ]
        |> map String.fromInt
        |> toList
        == [ "0", "1", "2", "3" ]

-}
map : (a -> b) -> ListNonempty a -> ListNonempty b
map f (ListNonempty ( a, b )) =
    ListNonempty ( f a, List.map f b )


{-| Equivalent to `List.indexedMap`

    fromPair "a" [ "b", "c", "d" ]
        |> indexedMap Tuple.pair
        |> toList
        == [ ( 0, "a" ), ( 1, "b" ), ( 2, "c" ), ( 3, "d" ) ]

-}
indexedMap : (Int -> a -> b) -> ListNonempty a -> ListNonempty b
indexedMap f (ListNonempty ( a, b )) =
    ListNonempty ( f 0 a, List.indexedMap (\i -> f (i + 1)) b )


{-| Equivalent to `List.foldl`

    fromPair 1 [ 2, 3 ] |> foldl (+) 0 == 6

-}
foldl : (a -> b -> b) -> b -> ListNonempty a -> b
foldl f start elems =
    List.foldl f start (toList elems)


{-| Equivalent to `List.Extra.foldl1`

    fromPair 1 [ 2, 3 ] |> foldl (+) 0 == 6

-}
foldl1 : (a -> a -> a) -> ListNonempty a -> a
foldl1 f (ListNonempty ( a, b )) =
    List.foldl f a b


{-| Equivalent to `List.foldr`

    fromPair 1 [ 2, 3 ] |> foldr (+) 0 == 6

-}
foldr : (a -> b -> b) -> b -> ListNonempty a -> b
foldr f start elems =
    List.foldr f start (toList elems)


{-| Equivalent to `List.Extra.foldr1`

    fromPair 1 [ 2, 3 ] |> foldr (+) 0 == 6

-}
foldr1 : (a -> a -> a) -> ListNonempty a -> a
foldr1 f elems =
    elems
        |> reverse
        |> foldl1 f


{-| Similar to `List.filter`. Returns `List` because we cannot guarantee
any elements will pass the test.

    fromPair 1 [ 2, 3, 4 ] |> filter (\n -> n > 2) == [ 3, 4 ]

    fromPair 1 [ 2, 3, 4 ] |> filter (\n -> n > 20) == []

-}
filter : (a -> Bool) -> ListNonempty a -> List a
filter f elems =
    List.filter f (toList elems)


{-| Similar to `List.filterMap`. Also returns `List`.

    fromPair "3.5" [ "cat", "4", "ball" ]
        |> filterMap String.toFloat
        == [ 3.5, 4 ]

-}
filterMap : (a -> Maybe b) -> ListNonempty a -> List b
filterMap f elems =
    List.filterMap f (toList elems)


{-| Same as `List.length`

    fromPair 0 [ 1, 2, 3 ] |> length == 4

-}
length : ListNonempty a -> Int
length (ListNonempty ( _, b )) =
    1 + List.length b


{-| Whether a non-empty list has only a single element

    fromPair 0 [ 1, 2, 3 ] |> isSingleton == False

    fromPair 0 [] |> isSingleton == True

-}
isSingleton : ListNonempty a -> Bool
isSingleton (ListNonempty ( _, b )) =
    List.isEmpty b


{-| Same as `List.reverse`.

    fromPair 0 [ 1, 2, 3 ]
        |> reverse
        |> toList
        == [ 3, 2, 1, 0 ]

-}
reverse : ListNonempty a -> ListNonempty a
reverse (ListNonempty ( a, b )) =
    case List.Extra.unconsLast b of
        Just ( last_, rest_ ) ->
            ListNonempty ( last_, List.reverse <| a :: rest_ )

        Nothing ->
            ListNonempty ( a, b )


{-| Same as `List.member`

    fromPair 0 [ 1, 2, 3 ] |> member 2 == True

    fromPair 0 [ 1, 2, 3 ] |> member 5 == False

-}
member : a -> ListNonempty a -> Bool
member elem (ListNonempty ( a, b )) =
    if elem == a then
        True

    else
        List.member elem b


{-| Same as `List.all`

    fromPair 0 [ 1, 2, 3 ] |> all (\n -> n < 2) == False

    fromPair 0 [ 1, 2, 3 ] |> all (\n -> n < 5) == True

-}
all : (a -> Bool) -> ListNonempty a -> Bool
all f elems =
    List.all f (toList elems)


{-| Same as `List.any`

    fromPair 0 [ 1, 2, 3 ] |> any (\n -> n > 2) == True

    fromPair 0 [ 1, 2, 3 ] |> any (\n -> n > 5) == False

-}
any : (a -> Bool) -> ListNonempty a -> Bool
any f elems =
    List.any f (toList elems)


{-| Similar to `List.maximum`, but doesn't need to return a Maybe type

     fromPair 0 [1, 2, 3] |> maximum == 3

     fromPair 2 [] |> maximum == 2

-}
maximum : ListNonempty comparable -> comparable
maximum (ListNonempty ( a, b )) =
    List.maximum (a :: b)
        |> Maybe.withDefault a


{-| Similar to `List.minimum`, but doesn't need to return a Maybe type

     fromPair 0 [1, 2, 3] |> minimum == 0

     fromPair 2 [] |> minimum == 2

-}
minimum : ListNonempty comparable -> comparable
minimum (ListNonempty ( a, b )) =
    List.minimum (a :: b)
        |> Maybe.withDefault a


{-| Equivalent to `List.sum`

    fromPair 1 [ 2, 3 ] |> sum == 6

-}
sum : ListNonempty number -> number
sum elems =
    List.sum (toList elems)


{-| Equivalent to `List.product`

    fromPair 1 [ 2, 3 ] |> product == 6

-}
product : ListNonempty number -> number
product elems =
    List.product (toList elems)


{-| Similar to `List.append`.

    append (fromPair 0 [ 1, 2 ]) (fromPair 3 [ 4, 5 ])
        |> toList
        == [ 0, 1, 2, 3, 4, 5 ]

-}
append : ListNonempty a -> ListNonempty a -> ListNonempty a
append (ListNonempty ( a, b )) c =
    ListNonempty ( a, b ++ toList c )


{-| Similar to `List.concat`

    fromPair (fromPair 0 [ 1 ]) [ fromPair 2 [ 3 ] ]
        |> concat
        |> toList
        == [ 0, 1, 2, 3 ]

    fromPair (fromPair 0 []) []
        |> concat
        |> toList
        == [ 0 ]

-}
concat : ListNonempty (ListNonempty a) -> ListNonempty a
concat (ListNonempty ( ListNonempty ( a, b ), c )) =
    ListNonempty ( a, b ++ List.concatMap toList c )


{-| Similar to `List.concatMap`

    concatMap (\n -> fromPair n [ n ]) (fromPair 0 [ 1, 2 ])
        |> toList
        == [ 0, 0, 1, 1, 2, 2 ]

-}
concatMap : (a -> ListNonempty b) -> ListNonempty a -> ListNonempty b
concatMap f elems =
    map f elems
        |> concat


{-| Similar to `List.map2`

    map2 (\a b -> a ++ "-" ++ b)
        (fromPair "a" [ "b", "c" ])
        (fromPair "1" [ "2", "3" ])
        |> toList
        == [ "a-1", "b-2", "c-3" ]

-}
map2 : (a -> b -> result) -> ListNonempty a -> ListNonempty b -> ListNonempty result
map2 f (ListNonempty ( a1, b1 )) (ListNonempty ( a2, b2 )) =
    ListNonempty ( f a1 a2, List.map2 f b1 b2 )


{-| Similar to `List.map3`

    map3 (\a b c -> ( a, b, c ))
        (fromPair 0 [ 1 ])
        (fromPair 2 [ 3 ])
        (fromPair 4 [ 5 ])
        |> toList
        == [ ( 0, 2, 4 ), ( 1, 3, 5 ) ]

-}
map3 : (a -> b -> c -> result) -> ListNonempty a -> ListNonempty b -> ListNonempty c -> ListNonempty result
map3 f (ListNonempty ( a1, b1 )) (ListNonempty ( a2, b2 )) (ListNonempty ( a3, b3 )) =
    ListNonempty ( f a1 a2 a3, List.map3 f b1 b2 b3 )


{-| Similar to `List.map4`

    map3 (\a b c -> ( a, b, c ))
        (fromPair 0 [ 1 ])
        (fromPair 2 [ 3 ])
        (fromPair 4 [ 5 ])
        |> toList
        == [ ( 0, 2, 4 ), ( 1, 3, 5 ) ]

-}
map4 : (a -> b -> c -> d -> result) -> ListNonempty a -> ListNonempty b -> ListNonempty c -> ListNonempty d -> ListNonempty result
map4 f (ListNonempty ( a1, b1 )) (ListNonempty ( a2, b2 )) (ListNonempty ( a3, b3 )) (ListNonempty ( a4, b4 )) =
    ListNonempty ( f a1 a2 a3 a4, List.map4 f b1 b2 b3 b4 )


{-| Similar to `List.map5`

    map3 (\a b c -> ( a, b, c ))
        (fromPair 0 [ 1 ])
        (fromPair 2 [ 3 ])
        (fromPair 4 [ 5 ])
        |> toList
        == [ ( 0, 2, 4 ), ( 1, 3, 5 ) ]

-}
map5 : (a -> b -> c -> d -> e -> result) -> ListNonempty a -> ListNonempty b -> ListNonempty c -> ListNonempty d -> ListNonempty e -> ListNonempty result
map5 f (ListNonempty ( a1, b1 )) (ListNonempty ( a2, b2 )) (ListNonempty ( a3, b3 )) (ListNonempty ( a4, b4 )) (ListNonempty ( a5, b5 )) =
    ListNonempty ( f a1 a2 a3 a4 a5, List.map5 f b1 b2 b3 b4 b5 )


{-| Similar to `List.sort`

    fromPair 1 [ 3, 5, 0, 2, 4 ]
        |> sort
        |> toList
        == [ 0, 1, 2, 3, 4, 5 ]

-}
sort : ListNonempty comparable -> ListNonempty comparable
sort (ListNonempty ( a, b )) =
    case List.sort b of
        head_ :: tail_ ->
            if head_ < a then
                ListNonempty ( head_, List.sort <| a :: tail_ )

            else
                ListNonempty ( a, head_ :: tail_ )

        [] ->
            ListNonempty ( a, [] )


{-| Similar to `List.sortBy`

    fromPair { x = 1, y = 1 } [ { x = 0, y = 2 }, { x = 2, y = 0 } ]
        |> sortBy .x
        |> toList
        == [ { x = 0, y = 2 }, { x = 1, y = 1 }, { x = 2, y = 0 } ]

    fromPair { x = 1, y = 1 } [ { x = 0, y = 2 }, { x = 2, y = 0 } ]
        |> sortBy .y
        |> toList
        == [ { x = 2, y = 0 }, { x = 1, y = 1 }, { x = 0, y = 2 } ]

-}
sortBy : (a -> comparable) -> ListNonempty a -> ListNonempty a
sortBy f (ListNonempty ( a, b )) =
    case List.sortBy f b of
        head_ :: tail_ ->
            if f head_ < f a then
                ListNonempty ( head_, List.sortBy f <| a :: tail_ )

            else
                ListNonempty ( a, head_ :: tail_ )

        [] ->
            ListNonempty ( a, [] )


{-| Similar to `List.sortWith`

    fromPair 0 [ 1, 2, 3, 4 ]
        |> sortWith (\a b -> compare b a)
        |> toList
        == [ 4, 3, 2, 1, 0 ]

-}
sortWith : (a -> a -> Order) -> ListNonempty a -> ListNonempty a
sortWith f (ListNonempty ( a, b )) =
    case List.sortWith f b of
        head_ :: tail_ ->
            case f head_ a of
                LT ->
                    ListNonempty ( head_, List.sortWith f <| a :: tail_ )

                _ ->
                    ListNonempty ( a, head_ :: tail_ )

        [] ->
            ListNonempty ( a, [] )


{-| Return the first element. Unlike `List.head`, doesn't need to return a Maybe.

    fromPair 0 [ 1, 2, 3 ] |> head == 0

-}
head : ListNonempty a -> a
head (ListNonempty ( a, b )) =
    a


{-| Return everything except the first element. Unlike `List.tail`, doesn't need to return a Maybe.

    fromPair 0 [ 1, 2, 3 ] |> tail == [ 1, 2, 3 ]

-}
tail : ListNonempty a -> List a
tail (ListNonempty ( a, b )) =
    b


{-| Return the last element. Doesn't need to return a Maybe.

    fromPair 0 [ 1, 2, 3 ] |> head == 3

-}
last : ListNonempty a -> a
last (ListNonempty ( a, b )) =
    case List.Extra.unconsLast b of
        Just ( last_, _ ) ->
            last_

        Nothing ->
            a


{-| Return everything except the last element. Doesn't need to return a Maybe.

    fromPair 0 [ 1, 2, 3 ] |> rest == [ 0, 1, 2 ]

-}
rest : ListNonempty a -> List a
rest (ListNonempty ( a, b )) =
    case List.Extra.unconsLast b of
        Just ( _, rest_ ) ->
            a :: rest_

        Nothing ->
            [ a ]


{-| Equivalent to `(\elems -> (head elems, tail elems))`

    fromPair 0 [ 1, 2, 3 ] |> uncons == ( 0, [ 1, 2, 3 ] )

-}
uncons : ListNonempty a -> ( a, List a )
uncons (ListNonempty ( a, b )) =
    ( a, b )


{-| Equivalent to `(\elems -> (rest elems, last elems))`

    fromPair 0 [ 1, 2, 3 ] |> unconsLast == ( [ 0, 1, 2 ], 3 )

-}
unconsLast : ListNonempty a -> ( List a, a )
unconsLast (ListNonempty ( a, b )) =
    case List.Extra.unconsLast b of
        Just ( last_, rest_ ) ->
            ( a :: rest_, last_ )

        Nothing ->
            ( [], a )


{-| Return the first N elements. Always includes the first element, regardless of
the value of N. If this behavior is undesired, use `toList` and `List.take`

    fromPair 0 [ 1, 2, 3 ] |> take 2 |> toList == [ 0, 1 ]

    fromPair 0 [ 1, 2, 3 ] |> take 0 |> toList == [ 0 ]

-}
take : Int -> ListNonempty a -> ListNonempty a
take n (ListNonempty ( a, b )) =
    ListNonempty ( a, List.take (n - 1) b )


{-| Return all elements except for the first N elements. Always includes the last element, regardless of
the value of N. If this behavior is undesired, use `toList` and `List.drop`

    fromPair 0 [ 1, 2, 3 ] |> drop 2 |> toList == [ 2, 3 ]

    fromPair 0 [ 1, 2, 3 ] |> drop 10 |> toList == [ 3 ]

-}
drop : Int -> ListNonempty a -> ListNonempty a
drop n (ListNonempty ( a, b )) =
    if n <= 0 then
        ListNonempty ( a, b )

    else
        case b of
            head_ :: tail_ ->
                drop (n - 1) (ListNonempty ( head_, tail_ ))

            [] ->
                ListNonempty ( a, b )


{-| Return the last N elements. Always includes the last element, regardless of
the value of N. If this behavior is undesired, use `toList >> List.reverse >> List.take n >> List.reverse`

    fromPair 0 [ 1, 2, 3 ] |> takeLast 2 |> toList == [ 2, 3 ]

    fromPair 0 [ 1, 2, 3 ] |> takeLast 0 |> toList == [ 3 ]

-}
takeLast : Int -> ListNonempty a -> ListNonempty a
takeLast n elems =
    elems
        |> reverse
        |> take n
        |> reverse


{-| Return all elements except for the last N elements. Always includes the first element, regardless of
the value of N. If this behavior is undesired, use `toList >> List.reverse >> List.drop n >> List.reverse`

    fromPair 0 [ 1, 2, 3 ] |> dropLast 2 |> toList == [ 2, 3 ]

    fromPair 0 [ 1, 2, 3 ] |> dropLast 10 |> toList == [ 3 ]

-}
dropLast : Int -> ListNonempty a -> ListNonempty a
dropLast n elems =
    elems
        |> reverse
        |> drop n
        |> reverse


{-| Similar to `List.unzip`.

    fromPair ( "a", 1 ) [ ( "b", 2 ), ( "c", 3 ) ]
        |> unzip2
        == ( fromPair "a" [ "b", "c" ]
           , fromPair 1 [ 2, 3 ]
           )

-}
unzip2 : ListNonempty ( a, b ) -> ( ListNonempty a, ListNonempty b )
unzip2 (ListNonempty ( ( a1, a2 ), b )) =
    let
        ( b1, b2 ) =
            List.unzip b
    in
    ( ListNonempty ( a1, b1 )
    , ListNonempty ( a2, b2 )
    )


{-| Similar to `unzip2`, but with 3-tuples.

    fromPair ( 1, 2, 3 ) [ ( 4, 5, 6 ), ( 7, 8, 9 ) ]
        |> unzip3
        == ( fromPair 1 [ 4, 7 ]
           , fromPair 2 [ 5, 8 ]
           , fromPair 3 [ 6, 9 ]
           )

-}
unzip3 : ListNonempty ( a, b, c ) -> ( ListNonempty a, ListNonempty b, ListNonempty c )
unzip3 (ListNonempty ( ( a1, a2, a3 ), b )) =
    let
        ( b1, b2, b3 ) =
            unzip3Helper b
    in
    ( ListNonempty ( a1, b1 )
    , ListNonempty ( a2, b2 )
    , ListNonempty ( a3, b3 )
    )


unzip3Helper : List ( a, b, c ) -> ( List a, List b, List c )
unzip3Helper elems =
    case elems of
        ( head1, head2, head3 ) :: tail_ ->
            let
                ( tail1, tail2, tail3 ) =
                    unzip3Helper tail_
            in
            ( head1 :: tail1
            , head2 :: tail2
            , head3 :: tail3
            )

        [] ->
            ( [], [], [] )
