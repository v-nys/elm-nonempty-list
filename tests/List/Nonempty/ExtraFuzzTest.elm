module List.Nonempty.ExtraFuzzTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (..)
import List.Nonempty exposing (ListNonempty, fromPair, fromTuple, toList)
import List.Nonempty.Extra
import Test exposing (..)


nonemptylist : Fuzzer a -> Fuzzer (ListNonempty a)
nonemptylist a =
    tuple ( a, list a ) |> Fuzz.map fromTuple


suite : Test
suite =
    describe "List.Nonempty"
        [ describe "List.Nonempty.Extra.intersperse"
            [ fuzz2 int (nonemptylist int) "is always of odd length" <|
                \sep elems ->
                    elems
                        |> List.Nonempty.Extra.intersperse sep
                        |> List.Nonempty.length
                        |> (\n -> modBy 2 n == 1)
                        |> Expect.equal True
            , fuzz2 int (nonemptylist int) "is correct length" <|
                \sep elems ->
                    elems
                        |> List.Nonempty.Extra.intersperse sep
                        |> List.Nonempty.length
                        |> Expect.equal
                            (List.Nonempty.length elems * 2 - 1)
            , fuzz2 int (nonemptylist int) "all odd indexes equal the separator" <|
                \sep elems ->
                    elems
                        |> List.Nonempty.Extra.intersperse sep
                        |> List.Nonempty.indexedMap Tuple.pair
                        |> List.Nonempty.Extra.partition (\( i, _ ) -> modBy 2 i == 0)
                        |> Tuple.mapFirst (List.map Tuple.second)
                        |> Tuple.mapSecond (List.map Tuple.second)
                        |> Expect.equal
                            ( toList elems
                            , List.repeat
                                (List.Nonempty.length elems - 1)
                                sep
                            )
            ]
        , describe "List.Nonempty.Extra.appendList"
            [ fuzz2 (nonemptylist int) (list int) "handles basic operation" <|
                \nelist list ->
                    List.Nonempty.Extra.appendList
                        nelist
                        list
                        |> List.Nonempty.toList
                        |> Expect.equal
                            (List.append
                                (List.Nonempty.toList nelist)
                                list
                            )
            ]
        , describe "List.Nonempty.Extra.prependList"
            [ fuzz2 (list int) (nonemptylist int) "handles basic operation" <|
                \list nelist ->
                    List.Nonempty.Extra.prependList
                        list
                        nelist
                        |> List.Nonempty.toList
                        |> Expect.equal
                            (List.append
                                list
                                (List.Nonempty.toList nelist)
                            )
            ]
        ]
