module List.Nonempty.Extra exposing
    ( unique, uniqueBy, setIf, setAt, setIfIndex, updateIf, updateAt, updateIfIndex, swapAt, maximumBy, maximumWith, minimumBy, minimumWith
    , intersperse, transpose, subsequences, permutations
    , indexedFoldl, indexedFoldr
    , scanl, scanlList, scanl1, scanr, scanrList, scanr1, mapAccuml, mapAccumr, iterate, initialize, appendList, prependList
    , splitAtTail, splitAtRest, group, groupList, groupWhile, groupWhileList, prefixes, suffixes, select, selectSplit, gatherEquals, gatherEqualsBy, gatherEqualsByList, gatherEqualsList, gatherWith, gatherWithList, frequencies, partition
    , notMember
    , lift2, lift3, lift4
    , groupsOf, groupsOfWithStep, groupsOfVarying, greedyGroupsOf, greedyGroupsOfWithStep
    )

{-| Various helper functions, perhaps too specialized for frequent use


# Basics

@docs unique, uniqueBy, setIf, setAt, setIfIndex, updateIf, updateAt, updateIfIndex, swapAt, maximumBy, maximumWith, minimumBy, minimumWith


# List transformations

@docs intersperse, transpose, subsequences, permutations


# Folds

@docs indexedFoldl, indexedFoldr


# Building lists

@docs scanl, scanlList, scanl1, scanr, scanrList, scanr1, mapAccuml, mapAccumr, iterate, initialize, appendList, prependList


# Sublists

@docs splitAtTail, splitAtRest, group, groupList, groupWhile, groupWhileList, prefixes, suffixes, select, selectSplit, gatherEquals, gatherEqualsBy, gatherEqualsByList, gatherEqualsList, gatherWith, gatherWithList, frequencies, partition


# Searching

@docs notMember


# Lift

@docs lift2, lift3, lift4


# Split to groups

@docs groupsOf, groupsOfWithStep, groupsOfVarying, greedyGroupsOf, greedyGroupsOfWithStep

-}

import List.Extra
import List.Nonempty exposing (..)


{-| Similar to `List.Extra.unique`

    fromPair 0 [ 1, 0, 2 ] |> unique == fromPair 0 [ 1, 2 ]

-}
unique : ListNonempty comparable -> ListNonempty comparable
unique elems =
    elems
        |> toList
        |> List.Extra.unique
        |> fromList
        |> Maybe.withDefault elems


{-| Similar to `List.Extra.uniqueBy`
-}
uniqueBy : (a -> comparable) -> ListNonempty a -> ListNonempty a
uniqueBy f elems =
    elems
        |> toList
        |> List.Extra.uniqueBy f
        |> fromList
        |> Maybe.withDefault elems


{-| Similar to `List.Extra.setIf`
-}
setIf : (a -> Bool) -> a -> ListNonempty a -> ListNonempty a
setIf f newElem elems =
    updateIf f (\_ -> newElem) elems


{-| Similar to `List.Extra.setAt`
-}
setAt : Int -> a -> ListNonempty a -> ListNonempty a
setAt i newElem elems =
    updateAt i (\_ -> newElem) elems


{-| `List.Extra.setIf` meets `List.Extra.updateIfIndex`
-}
setIfIndex : (Int -> Bool) -> a -> ListNonempty a -> ListNonempty a
setIfIndex fPred newElem elems =
    updateIfIndex fPred (\_ -> newElem) elems


{-| Similar to `List.Extra.updateIf`
-}
updateIf : (a -> Bool) -> (a -> a) -> ListNonempty a -> ListNonempty a
updateIf fPred fUpdate elems =
    let
        ( a, b ) =
            List.Nonempty.toTuple elems
    in
    List.Nonempty.fromTuple
        ( if fPred a then
            fUpdate a

          else
            a
        , List.Extra.updateIf fPred fUpdate b
        )


{-| Similar to `List.Extra.updateAt`
-}
updateAt : Int -> (a -> a) -> ListNonempty a -> ListNonempty a
updateAt i fUpdate elems =
    let
        ( a, b ) =
            List.Nonempty.toTuple elems
    in
    if i == 0 then
        List.Nonempty.fromTuple ( fUpdate a, b )

    else
        List.Nonempty.fromTuple ( a, List.Extra.updateAt (i - 1) fUpdate b )


{-| Similar to `List.Extra.updateIfIndex`
-}
updateIfIndex : (Int -> Bool) -> (a -> a) -> ListNonempty a -> ListNonempty a
updateIfIndex fPred fUpdate elems =
    let
        ( a, b ) =
            List.Nonempty.toTuple elems
    in
    List.Nonempty.fromTuple
        ( if fPred 0 then
            fUpdate a

          else
            a
        , List.Extra.updateIfIndex (\i -> fPred (i + 1)) fUpdate b
        )


{-| Similar to `List.Extra.swapAt`
-}
swapAt : Int -> Int -> ListNonempty a -> ListNonempty a
swapAt a b elems =
    elems
        |> List.Nonempty.toList
        |> List.Extra.swapAt a b
        |> List.Nonempty.fromList
        |> Maybe.withDefault elems


{-| Similar to `List.Extra.maximumBy`
-}
maximumBy : (a -> comparable) -> ListNonempty a -> a
maximumBy f elems =
    elems
        |> List.Nonempty.toList
        |> List.Extra.maximumBy f
        |> Maybe.withDefault (List.Nonempty.head elems)


{-| Similar to `List.Extra.maximumWith`
-}
maximumWith : (a -> a -> Order) -> ListNonempty a -> a
maximumWith f elems =
    elems
        |> List.Nonempty.toList
        |> List.Extra.maximumWith f
        |> Maybe.withDefault (List.Nonempty.head elems)


{-| Similar to `List.Extra.minimumBy`
-}
minimumBy : (a -> comparable) -> ListNonempty a -> a
minimumBy f elems =
    elems
        |> List.Nonempty.toList
        |> List.Extra.minimumBy f
        |> Maybe.withDefault (List.Nonempty.head elems)


{-| Similar to `List.Extra.minimumWith`
-}
minimumWith : (a -> a -> Order) -> ListNonempty a -> a
minimumWith f elems =
    elems
        |> List.Nonempty.toList
        |> List.Extra.minimumWith f
        |> Maybe.withDefault (List.Nonempty.head elems)


{-| Similar to `List.intersperse`

    intersperse "." (fromPair "127" [ "0", "0", "1" ])
        |> toList
        == [ "127", ".", "0", ".", "0", ".", "1" ]

-}
intersperse : a -> ListNonempty a -> ListNonempty a
intersperse sep elems =
    let
        ( a, b ) =
            List.Nonempty.toTuple elems
    in
    case b of
        [] ->
            elems

        _ ->
            List.Nonempty.fromTuple ( a, sep :: List.intersperse sep b )


{-| Similar to `List.Extra.transpose`
-}
transpose : ListNonempty (ListNonempty a) -> ListNonempty (ListNonempty a)
transpose elems =
    elems
        |> List.Nonempty.toList
        |> List.map List.Nonempty.toList
        |> List.Extra.transpose
        |> List.map List.Nonempty.fromList
        |> List.filterMap identity
        |> List.Nonempty.fromList
        |> Maybe.withDefault elems


{-| Similar to `List.Extra.subsequences`
-}
subsequences : ListNonempty a -> ListNonempty (ListNonempty a)
subsequences elems =
    elems
        |> List.Nonempty.toList
        |> List.Extra.subsequences
        |> List.map List.Nonempty.fromList
        |> List.filterMap identity
        |> List.Nonempty.fromList
        |> Maybe.withDefault (fromPair elems [])


{-| Similar to `List.Extra.permutations`
-}
permutations : ListNonempty a -> ListNonempty (ListNonempty a)
permutations elems =
    elems
        |> List.Nonempty.toList
        |> List.Extra.permutations
        |> List.map List.Nonempty.fromList
        |> List.filterMap identity
        |> List.Nonempty.fromList
        |> Maybe.withDefault (fromPair elems [])


{-| Similar to `List.Extra.indexedFoldl`
-}
indexedFoldl : (Int -> a -> b -> b) -> b -> ListNonempty a -> b
indexedFoldl f start elems =
    List.Extra.indexedFoldl f start (List.Nonempty.toList elems)


{-| Similar to `List.Extra.indexedFoldr`
-}
indexedFoldr : (Int -> a -> b -> b) -> b -> ListNonempty a -> b
indexedFoldr f start elems =
    List.Extra.indexedFoldl f start (List.Nonempty.toList elems)


{-| Similar to `List.Extra.scanl`
-}
scanl : (a -> b -> b) -> b -> ListNonempty a -> ListNonempty b
scanl f start elems =
    elems
        |> List.Nonempty.toList
        |> List.Extra.scanl f start
        |> List.Nonempty.fromList
        |> Maybe.withDefault (fromPair start [])


{-| Similar to `scanl`, except takes a `List` as input. Output is still guaranteed to be non-empty.
-}
scanlList : (a -> b -> b) -> b -> List a -> ListNonempty b
scanlList f start elems =
    elems
        |> List.Extra.scanl f start
        |> List.Nonempty.fromList
        |> Maybe.withDefault (fromPair start [])


{-| Similar to `List.Extra.scanl1` except doesn't need to return a `Maybe`. The function is `(a -> a -> a)` though, instead of `(a -> b -> b)`.
-}
scanl1 : (a -> a -> a) -> ListNonempty a -> ListNonempty a
scanl1 f elems =
    let
        ( a, b ) =
            List.Nonempty.toTuple elems
    in
    List.Extra.scanl f a b
        |> List.Nonempty.fromList
        |> Maybe.withDefault (fromPair a [])


{-| Similar to `List.Extra.scanr`
-}
scanr : (a -> b -> b) -> b -> ListNonempty a -> ListNonempty b
scanr f start elems =
    elems
        |> List.Nonempty.toList
        |> List.Extra.scanr f start
        |> List.Nonempty.fromList
        |> Maybe.withDefault (fromPair start [])


{-| Similar to `scanr`, except takes a `List` as input. Output is still guaranteed to be non-empty.
-}
scanrList : (a -> b -> b) -> b -> List a -> ListNonempty b
scanrList f start elems =
    elems
        |> List.Extra.scanr f start
        |> List.Nonempty.fromList
        |> Maybe.withDefault (fromPair start [])


{-| Similar to `List.Extra.scanr1` except doesn't need to return a `Maybe`. The function is `(a -> a -> a)` though, instead of `(a -> b -> b)`.
-}
scanr1 : (a -> a -> a) -> ListNonempty a -> ListNonempty a
scanr1 f elems =
    let
        ( a, b ) =
            List.Nonempty.toTuple elems
    in
    List.Extra.scanr f a b
        |> List.Nonempty.fromList
        |> Maybe.withDefault (fromPair a [])


{-| Similar to `List.Extra.mapAccuml`
-}
mapAccuml : (a -> b -> ( a, c )) -> a -> ListNonempty b -> ( a, ListNonempty c )
mapAccuml f start elems =
    List.Extra.mapAccuml f start (List.Nonempty.toList elems)
        |> Tuple.mapSecond List.Nonempty.fromList
        |> Tuple.mapSecond
            (Maybe.withDefault
                (List.Nonempty.singleton
                    (f start (List.Nonempty.head elems) |> Tuple.second)
                )
            )


{-| Similar to `List.Extra.mapAccumr`
-}
mapAccumr : (a -> b -> ( a, c )) -> a -> ListNonempty b -> ( a, ListNonempty c )
mapAccumr f start elems =
    List.Extra.mapAccumr f start (List.Nonempty.toList elems)
        |> Tuple.mapSecond List.Nonempty.fromList
        |> Tuple.mapSecond
            (Maybe.withDefault
                (List.Nonempty.singleton
                    (f start (List.Nonempty.head elems) |> Tuple.second)
                )
            )


{-| Similar to `List.Extra.iterate`
-}
iterate : (a -> Maybe a) -> a -> ListNonempty a
iterate f start =
    List.Extra.iterate f start
        |> List.Nonempty.fromList
        |> Maybe.withDefault (List.Nonempty.singleton start)


{-| Similar to `List.Extra.initialize`.
-}
initialize : Int -> (Int -> a) -> ListNonempty a
initialize n f =
    List.Nonempty.fromPair
        (f 0)
        (List.map
            f
            (List.range 1 n)
        )


{-| Similar to append, except taking an ordinary list as second argument.

    appendList (fromPair 0 [ 1, 2 ]) [ 3, 4, 5 ]
        |> toList
        == [ 0, 1, 2, 3, 4, 5 ]

-}
appendList : ListNonempty a -> List a -> ListNonempty a
appendList ab c =
    let
        ( a, b ) =
            List.Nonempty.toTuple ab
    in
    List.Nonempty.fromTuple ( a, b ++ c )


{-| Similar to append, except taking an ordinary list as first argument.

    appendList (fromPair 0 [ 1, 2 ]) [ 3, 4, 5 ]
        |> toList
        == [ 0, 1, 2, 3, 4, 5 ]

-}
prependList : List a -> ListNonempty a -> ListNonempty a
prependList a b =
    case a of
        head_ :: tail_ ->
            List.Nonempty.fromTuple ( head_, tail_ ++ List.Nonempty.toList b )

        [] ->
            b


{-| Similar to `List.Extra.splitAt`. Compare with `splitAtRest`.
-}
splitAtTail : Int -> ListNonempty a -> ( ListNonempty a, List a )
splitAtTail n elems =
    List.Extra.splitAt (max n 1) (List.Nonempty.toList elems)
        |> Tuple.mapFirst List.Nonempty.fromList
        |> Tuple.mapFirst (Maybe.withDefault elems)


{-| Similar to `List.Extra.splitAt`. Compare with `splitAtTail`.
-}
splitAtRest : Int -> ListNonempty a -> ( List a, ListNonempty a )
splitAtRest n elems =
    List.Extra.splitAt (min n <| List.Nonempty.length elems - 1) (List.Nonempty.toList elems)
        |> Tuple.mapSecond List.Nonempty.fromList
        |> Tuple.mapSecond (Maybe.withDefault elems)


{-| Similar to `List.Extra.group`.
-}
group : ListNonempty a -> ListNonempty (ListNonempty a)
group elems =
    elems
        |> List.Nonempty.toList
        |> List.Extra.group
        |> List.map List.Nonempty.fromTuple
        |> List.Nonempty.fromList
        |> Maybe.withDefault (List.Nonempty.singleton elems)


{-| Similar to `group`, but works on `List`.
-}
groupList : List a -> List (ListNonempty a)
groupList elems =
    elems
        |> List.Extra.group
        |> List.map List.Nonempty.fromTuple


{-| Similar to `List.Extra.groupWhile`.
-}
groupWhile : (a -> a -> Bool) -> ListNonempty a -> ListNonempty (ListNonempty a)
groupWhile f elems =
    elems
        |> List.Nonempty.toList
        |> List.Extra.groupWhile f
        |> List.map List.Nonempty.fromTuple
        |> List.Nonempty.fromList
        |> Maybe.withDefault (List.Nonempty.singleton elems)


{-| Similar to `groupWhile`, but works on `List`.
-}
groupWhileList : (a -> a -> Bool) -> List a -> List (ListNonempty a)
groupWhileList f elems =
    elems
        |> List.Extra.groupWhile f
        |> List.map List.Nonempty.fromTuple


{-| Similar to `List.Extra.inits`.
-}
prefixes : ListNonempty a -> ListNonempty (ListNonempty a)
prefixes elems =
    elems
        |> List.Nonempty.toList
        |> List.Extra.inits
        |> List.filterMap List.Nonempty.fromList
        |> List.Nonempty.fromList
        |> Maybe.withDefault (List.Nonempty.singleton elems)


{-| Similar to `List.Extra.tails`.
-}
suffixes : ListNonempty a -> ListNonempty (ListNonempty a)
suffixes elems =
    elems
        |> List.Nonempty.toList
        |> List.Extra.tails
        |> List.filterMap List.Nonempty.fromList
        |> List.Nonempty.fromList
        |> Maybe.withDefault (List.Nonempty.singleton elems)


{-| Similar to `List.Extra.select`. Output type was chosen because the output of this function is likely intended to be used as separate pieces. If output type of `ListNonempty (ListNonempty a)` is desired, use `select >> map fromTuple`.
-}
select : ListNonempty a -> ListNonempty ( a, List a )
select elems =
    elems
        |> List.Nonempty.toList
        |> List.Extra.select
        |> List.Nonempty.fromList
        |> Maybe.withDefault
            (List.Nonempty.singleton
                (List.Nonempty.toTuple elems)
            )


{-| Similar to `List.Extra.selectSplit`.
-}
selectSplit : ListNonempty a -> ListNonempty ( List a, a, List a )
selectSplit elems =
    elems
        |> List.Nonempty.toList
        |> List.Extra.selectSplit
        |> List.Nonempty.fromList
        |> Maybe.withDefault
            (List.Nonempty.singleton
                (List.Nonempty.toTuple elems
                    |> (\( a, b ) -> ( [], a, b ))
                )
            )


{-| Similar to `List.Extra.gatherEquals`.
-}
gatherEquals : ListNonempty a -> ListNonempty (ListNonempty a)
gatherEquals elems =
    elems
        |> List.Nonempty.toList
        |> List.Extra.gatherEquals
        |> List.map List.Nonempty.fromTuple
        |> List.Nonempty.fromList
        |> Maybe.withDefault (List.Nonempty.singleton elems)


{-| Similar to `gatherEquals` except operating on `List`.
-}
gatherEqualsList : List a -> List (ListNonempty a)
gatherEqualsList elems =
    elems
        |> List.Extra.gatherEquals
        |> List.map List.Nonempty.fromTuple


{-| Similar to `List.Extra.gatherEqualsBy`.
-}
gatherEqualsBy : (a -> b) -> ListNonempty a -> ListNonempty (ListNonempty a)
gatherEqualsBy f elems =
    elems
        |> List.Nonempty.toList
        |> List.Extra.gatherEqualsBy f
        |> List.map List.Nonempty.fromTuple
        |> List.Nonempty.fromList
        |> Maybe.withDefault (List.Nonempty.singleton elems)


{-| Similar to `gatherEqualsBy`, except operating on `List`.
-}
gatherEqualsByList : (a -> b) -> List a -> List (ListNonempty a)
gatherEqualsByList f elems =
    elems
        |> List.Extra.gatherEqualsBy f
        |> List.map List.Nonempty.fromTuple


{-| Similar to `List.Extra.gatherWith`.
-}
gatherWith : (a -> a -> Bool) -> ListNonempty a -> ListNonempty (ListNonempty a)
gatherWith f elems =
    elems
        |> List.Nonempty.toList
        |> List.Extra.gatherWith f
        |> List.map List.Nonempty.fromTuple
        |> List.Nonempty.fromList
        |> Maybe.withDefault
            (List.Nonempty.singleton elems)


{-| Similar to `gatherWith`, except operating on `List`.
-}
gatherWithList : (a -> a -> Bool) -> List a -> List (ListNonempty a)
gatherWithList f elems =
    elems
        |> List.Extra.gatherWith f
        |> List.map List.Nonempty.fromTuple


{-| Similar to `List.Extra.frequencies`.
-}
frequencies : ListNonempty comparable -> ListNonempty ( comparable, Int )
frequencies elems =
    elems
        |> List.Nonempty.toList
        |> List.Extra.frequencies
        |> List.Nonempty.fromList
        |> Maybe.withDefault
            (List.Nonempty.singleton
                ( List.Nonempty.head elems, 0 )
            )


{-| Equivalent to `List.partition`. We can't have any (simple) guarantees about
the lengths of the two returned lists.

    fromPair 2 [ 5, 1, 4 ]
        |> partition (\n -> n > 2)
        == ( [ 5, 4 ], [ 2, 1 ] )

-}
partition : (a -> Bool) -> ListNonempty a -> ( List a, List a )
partition f elems =
    List.partition f (List.Nonempty.toList elems)


{-| Similar to `List.Extra.notMember`.
-}
notMember : a -> ListNonempty a -> Bool
notMember elem elems =
    elems
        |> List.Nonempty.member elem
        |> not


{-| Similar to `List.Extra.lift2`.
-}
lift2 : (a -> b -> c) -> ListNonempty a -> ListNonempty b -> ListNonempty c
lift2 f elems1 elems2 =
    List.Extra.lift2
        f
        (List.Nonempty.toList elems1)
        (List.Nonempty.toList elems2)
        |> List.Nonempty.fromList
        |> Maybe.withDefault
            (List.Nonempty.singleton
                (f
                    (List.Nonempty.head elems1)
                    (List.Nonempty.head elems2)
                )
            )


{-| Similar to `List.Extra.lift3`.
-}
lift3 : (a -> b -> c -> d) -> ListNonempty a -> ListNonempty b -> ListNonempty c -> ListNonempty d
lift3 f elems1 elems2 elems3 =
    List.Extra.lift3
        f
        (List.Nonempty.toList elems1)
        (List.Nonempty.toList elems2)
        (List.Nonempty.toList elems3)
        |> List.Nonempty.fromList
        |> Maybe.withDefault
            (List.Nonempty.singleton
                (f
                    (List.Nonempty.head elems1)
                    (List.Nonempty.head elems2)
                    (List.Nonempty.head elems3)
                )
            )


{-| Similar to `List.Extra.lift4`.
-}
lift4 : (a -> b -> c -> d -> e) -> ListNonempty a -> ListNonempty b -> ListNonempty c -> ListNonempty d -> ListNonempty e
lift4 f elems1 elems2 elems3 elems4 =
    List.Extra.lift4
        f
        (List.Nonempty.toList elems1)
        (List.Nonempty.toList elems2)
        (List.Nonempty.toList elems3)
        (List.Nonempty.toList elems4)
        |> List.Nonempty.fromList
        |> Maybe.withDefault
            (List.Nonempty.singleton
                (f
                    (List.Nonempty.head elems1)
                    (List.Nonempty.head elems2)
                    (List.Nonempty.head elems3)
                    (List.Nonempty.head elems4)
                )
            )


{-| Similar to `List.Extra.groupsOf`.
-}
groupsOf : Int -> ListNonempty a -> ListNonempty (ListNonempty a)
groupsOf n elems =
    elems
        |> List.Nonempty.toList
        |> List.Extra.groupsOf (max n 1)
        |> List.filterMap List.Nonempty.fromList
        |> List.Nonempty.fromList
        |> Maybe.withDefault
            (List.Nonempty.singleton elems)


{-| Similar to `List.Extra.groupsOfWithStep`.
-}
groupsOfWithStep : Int -> Int -> ListNonempty a -> ListNonempty (ListNonempty a)
groupsOfWithStep size step elems =
    elems
        |> List.Nonempty.toList
        |> List.Extra.groupsOfWithStep (max size 1) (max step 1)
        |> List.filterMap List.Nonempty.fromList
        |> List.Nonempty.fromList
        |> Maybe.withDefault
            (List.Nonempty.singleton elems)


{-| Similar to `List.Extra.groupsOfVarying`.
-}
groupsOfVarying : ListNonempty Int -> ListNonempty a -> ListNonempty (ListNonempty a)
groupsOfVarying ns elems =
    List.Extra.groupsOfVarying
        (List.Nonempty.toList ns)
        (List.Nonempty.toList elems)
        |> List.filterMap List.Nonempty.fromList
        |> List.Nonempty.fromList
        |> Maybe.withDefault
            (List.Nonempty.singleton elems)


{-| Similar to `List.Extra.greedyGroupsOf`.
-}
greedyGroupsOf : Int -> ListNonempty a -> ListNonempty (ListNonempty a)
greedyGroupsOf size elems =
    List.Extra.greedyGroupsOf
        (max size 1)
        (List.Nonempty.toList elems)
        |> List.filterMap List.Nonempty.fromList
        |> List.Nonempty.fromList
        |> Maybe.withDefault
            (List.Nonempty.singleton elems)


{-| Similar to `List.Extra.greedyGroupsOfWithStep`.
-}
greedyGroupsOfWithStep : Int -> Int -> ListNonempty a -> ListNonempty (ListNonempty a)
greedyGroupsOfWithStep size step elems =
    elems
        |> List.Nonempty.toList
        |> List.Extra.greedyGroupsOfWithStep (max size 1) (max step 1)
        |> List.filterMap List.Nonempty.fromList
        |> List.Nonempty.fromList
        |> Maybe.withDefault
            (List.Nonempty.singleton elems)
