module List.NonemptyFuzzTest exposing (..)

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
        , describe "List.Nonempty.sort"
            [ fuzz (nonemptylist int) "handles basic operation" <|
                \nelist ->
                    nelist
                        |> List.Nonempty.sort
                        |> List.Nonempty.toList
                        |> Expect.equal (nelist |> List.Nonempty.toList |> List.sort)
            ]
        ]
