module NonemptyListFuzzTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (..)
import List.Nonempty exposing (..)
import Test exposing (..)


nonemptylist : Fuzzer a -> Fuzzer (ListNonempty a)
nonemptylist a =
    tuple ( a, list a ) |> Fuzz.map fromTuple


suite : Test
suite =
    describe "NonemptyList"
        [ describe "List.Nonempty.toList"
            [ fuzz (tuple ( int, list int )) "handles basic operation" <|
                \( head_, tail_ ) ->
                    fromPair head_ tail_
                        |> List.Nonempty.toList
                        |> Expect.equal (head_ :: tail_)
            ]
        , describe "List.Nonempty.maximum"
            [ fuzz (nonemptylist int) "handles basic operation" <|
                \nelist ->
                    nelist
                        |> List.Nonempty.maximum
                        |> Just
                        |> Expect.equal (nelist |> List.Nonempty.toList |> List.maximum)
            ]
        , describe "List.Nonempty.minimum"
            [ fuzz (nonemptylist int) "handles basic operation" <|
                \nelist ->
                    nelist
                        |> List.Nonempty.minimum
                        |> Just
                        |> Expect.equal (nelist |> List.Nonempty.toList |> List.minimum)
            ]
        , describe "List.Nonempty.sum"
            [ fuzz (nonemptylist int) "handles basic operation" <|
                \nelist ->
                    nelist
                        |> List.Nonempty.sum
                        |> Expect.equal (nelist |> List.Nonempty.toList |> List.sum)
            ]
        , describe "List.Nonempty.append"
            [ fuzz2 (nonemptylist int) (nonemptylist int) "handles basic operation" <|
                \nelist1 nelist2 ->
                    List.Nonempty.append
                        nelist1
                        nelist2
                        |> List.Nonempty.toList
                        |> Expect.equal
                            (List.append
                                (List.Nonempty.toList nelist1)
                                (List.Nonempty.toList nelist2)
                            )
            ]
        , describe "List.Nonempty.appendList"
            [ fuzz2 (nonemptylist int) (list int) "handles basic operation" <|
                \nelist list ->
                    List.Nonempty.appendList
                        nelist
                        list
                        |> List.Nonempty.toList
                        |> Expect.equal
                            (List.append
                                (List.Nonempty.toList nelist)
                                list
                            )
            ]
        , describe "List.Nonempty.prependList"
            [ fuzz2 (list int) (nonemptylist int) "handles basic operation" <|
                \list nelist ->
                    List.Nonempty.prependList
                        list
                        nelist
                        |> List.Nonempty.toList
                        |> Expect.equal
                            (List.append
                                list
                                (List.Nonempty.toList nelist)
                            )
            ]
        , describe "List.Nonempty.concat"
            [ fuzz (nonemptylist (nonemptylist int)) "handles basic operation" <|
                \nelistOfNelists ->
                    nelistOfNelists
                        |> List.Nonempty.concat
                        |> List.Nonempty.toList
                        |> Expect.equal
                            (nelistOfNelists
                                |> List.Nonempty.toList
                                |> List.map List.Nonempty.toList
                                |> List.concat
                            )
            ]
        , describe "List.Nonempty.intersperse"
            [ fuzz2 int (nonemptylist int) "is always of odd length" <|
                \sep elems ->
                    elems
                        |> List.Nonempty.intersperse sep
                        |> List.Nonempty.length
                        |> (\n -> modBy 2 n == 1)
                        |> Expect.equal True
            , fuzz2 int (nonemptylist int) "is correct length" <|
                \sep elems ->
                    elems
                        |> List.Nonempty.intersperse sep
                        |> List.Nonempty.length
                        |> Expect.equal
                            (List.Nonempty.length elems * 2 - 1)
            , fuzz2 int (nonemptylist int) "all odd indexes equal the separator" <|
                \sep elems ->
                    elems
                        |> List.Nonempty.intersperse sep
                        |> List.Nonempty.indexedMap Tuple.pair
                        |> List.Nonempty.partition (\( i, _ ) -> modBy 2 i == 0)
                        |> Tuple.mapFirst (List.map Tuple.second)
                        |> Tuple.mapSecond (List.map Tuple.second)
                        |> Expect.equal
                            ( toList elems
                            , List.repeat
                                (List.Nonempty.length elems - 1)
                                sep
                            )
            ]
        , describe "List.Nonempty.sort"
            [ fuzz (nonemptylist int) "handles basic operation" <|
                \nelist ->
                    nelist
                        |> List.Nonempty.sort
                        |> List.Nonempty.toList
                        |> Expect.equal (nelist |> List.Nonempty.toList |> List.sort)
            ]
        ]
