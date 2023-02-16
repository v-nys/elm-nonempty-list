module NonemptyListTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (..)
import List.Nonempty exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "NonemptyList"
        [ describe "List.Nonempty.singleton"
            [ test "handles basic operation" <|
                \_ ->
                    List.Nonempty.singleton 4
                        |> Expect.equal
                            (fromPair 4 [])
            ]
        , describe "List.Nonempty.repeat"
            [ test "handles basic operation" <|
                \_ ->
                    List.Nonempty.repeat 4 'a'
                        |> Expect.equal
                            (fromPair 'a' [ 'a', 'a', 'a' ])
            , test "handles n=0" <|
                \_ ->
                    List.Nonempty.repeat 0 'a'
                        |> Expect.equal
                            (fromPair 'a' [])
            ]
        , describe "List.Nonempty.range"
            [ test "handles basic operation" <|
                \_ ->
                    List.Nonempty.range 3 6
                        |> Expect.equal
                            (fromPair 3 [ 4, 5, 6 ])
            , test "handles a=b" <|
                \_ ->
                    List.Nonempty.range 3 3
                        |> Expect.equal
                            (fromPair 3 [])
            , test "handles a>b" <|
                \_ ->
                    List.Nonempty.range 6 3
                        |> Expect.equal
                            (fromPair 6 [])
            ]
        , describe "List.Nonempty.fromList"
            [ test "handles basic operation" <|
                \_ ->
                    List.Nonempty.fromList [ 1, 2, 3 ]
                        |> Expect.equal
                            (Just <| fromPair 1 [ 2, 3 ])
            , test "handles empty list" <|
                \_ ->
                    List.Nonempty.fromList []
                        |> Expect.equal
                            Nothing
            ]
        , describe "List.Nonempty.fromPair"
            [ test "handles basic operation" <|
                \_ ->
                    fromPair 1 [ 2, 3 ]
                        |> Expect.equal
                            (fromPair 1 [ 2, 3 ])
            ]
        , describe "List.Nonempty.fromTuple"
            [ test "handles basic operation" <|
                \_ ->
                    fromTuple ( 1, [ 2, 3 ] )
                        |> Expect.equal
                            (fromPair 1 [ 2, 3 ])
            ]
        , describe "List.Nonempty.toList"
            [ test "handles basic operation" <|
                \_ ->
                    fromPair 1 [ 2, 3 ]
                        |> List.Nonempty.toList
                        |> Expect.equal
                            [ 1, 2, 3 ]
            ]
        , describe "List.Nonempty.map"
            [ test "handles basic operation" <|
                \_ ->
                    fromPair 1 [ 2, 3 ]
                        |> List.Nonempty.map String.fromInt
                        |> Expect.equal
                            (fromPair "1" [ "2", "3" ])
            ]
        , describe "List.Nonempty.indexedMap"
            [ test "handles basic operation" <|
                \_ ->
                    fromPair "a" [ "b", "c" ]
                        |> List.Nonempty.indexedMap Tuple.pair
                        |> Expect.equal
                            (fromPair ( 0, "a" ) [ ( 1, "b" ), ( 2, "c" ) ])
            ]
        , describe "List.Nonempty.foldl"
            [ test "handles basic operation" <|
                \_ ->
                    fromPair "a" [ "b", "c" ]
                        |> List.Nonempty.foldl (\a b -> b ++ a) ""
                        |> Expect.equal "abc"
            ]
        , describe "List.Nonempty.foldl1"
            [ test "handles basic operation" <|
                \_ ->
                    fromPair "a" [ "b", "c" ]
                        |> List.Nonempty.foldl1 (\a b -> b ++ a)
                        |> Expect.equal "abc"
            ]
        , describe "List.Nonempty.foldr"
            [ test "handles basic operation" <|
                \_ ->
                    fromPair "a" [ "b", "c" ]
                        |> List.Nonempty.foldr (\a b -> b ++ a) ""
                        |> Expect.equal "cba"
            ]
        , describe "List.Nonempty.foldr1"
            [ test "handles basic operation" <|
                \_ ->
                    fromPair "a" [ "b", "c" ]
                        |> List.Nonempty.foldr1 (\a b -> b ++ a)
                        |> Expect.equal "cba"
            ]
        , describe "List.Nonempty.length"
            [ test "handles basic operation" <|
                \_ ->
                    fromPair 0 [ 1, 2 ]
                        |> List.Nonempty.length
                        |> Expect.equal 3
            , test "handles singleton" <|
                \_ ->
                    fromPair 0 []
                        |> List.Nonempty.length
                        |> Expect.equal 1
            ]
        , describe "List.Nonempty.reverse"
            [ test "handles basic operation" <|
                \_ ->
                    fromPair 0 [ 1, 2, 3 ]
                        |> List.Nonempty.reverse
                        |> Expect.equal
                            (fromPair 3 [ 2, 1, 0 ])
            ]
        , describe "List.Nonempty.member"
            [ test "handles found in head" <|
                \_ ->
                    fromPair 0 [ 1, 2, 3 ]
                        |> List.Nonempty.member 0
                        |> Expect.equal True
            , test "handles found in tail" <|
                \_ ->
                    fromPair 0 [ 1, 2, 3 ]
                        |> List.Nonempty.member 2
                        |> Expect.equal True
            , test "handles not found" <|
                \_ ->
                    fromPair 0 [ 1, 2, 3 ]
                        |> List.Nonempty.member 5
                        |> Expect.equal False
            ]
        , describe "List.Nonempty.maximum"
            [ test "handles found in head" <|
                \_ ->
                    fromPair 3 [ 2, 1, 0 ]
                        |> List.Nonempty.maximum
                        |> Expect.equal 3
            , test "handles found in tail" <|
                \_ ->
                    fromPair 0 [ 1, 2, 3 ]
                        |> List.Nonempty.maximum
                        |> Expect.equal 3
            ]
        , describe "List.Nonempty.minimum"
            [ test "handles found in head" <|
                \_ ->
                    fromPair 0 [ 1, 2, 3 ]
                        |> List.Nonempty.minimum
                        |> Expect.equal 0
            , test "handles found in tail" <|
                \_ ->
                    fromPair 3 [ 2, 1, 0 ]
                        |> List.Nonempty.minimum
                        |> Expect.equal 0
            ]
        , describe "List.Nonempty.append"
            [ test "handles found in head" <|
                \_ ->
                    List.Nonempty.append
                        (fromPair 0 [ 1, 2 ])
                        (fromPair 3 [ 4, 5 ])
                        |> Expect.equal
                            (fromPair 0 [ 1, 2, 3, 4, 5 ])
            , test "same order as List.append" <|
                \_ ->
                    List.Nonempty.append
                        (fromPair 0 [ 1, 2 ])
                        (fromPair 3 [ 4, 5 ])
                        |> List.Nonempty.toList
                        |> Expect.equal
                            (List.append
                                [ 0, 1, 2 ]
                                [ 3, 4, 5 ]
                            )
            ]
        , describe "List.Nonempty.appendList"
            [ test "handles basic operation" <|
                \_ ->
                    List.Nonempty.appendList
                        (fromPair 0 [ 1, 2 ])
                        [ 3, 4, 5 ]
                        |> Expect.equal
                            (List.Nonempty.append
                                (fromPair 0 [ 1, 2 ])
                                (fromPair 3 [ 4, 5 ])
                            )
            , test "handles empty list" <|
                \_ ->
                    List.Nonempty.appendList
                        (fromPair 0 [ 1, 2 ])
                        []
                        |> Expect.equal
                            (fromPair 0 [ 1, 2 ])
            , test "handles minimal" <|
                \_ ->
                    List.Nonempty.appendList
                        (fromPair 0 [])
                        []
                        |> Expect.equal
                            (fromPair 0 [])
            ]
        , describe "List.Nonempty.prependList"
            [ test "handles basic operation" <|
                \_ ->
                    List.Nonempty.prependList
                        [ 0, 1, 2 ]
                        (fromPair 3 [ 4, 5 ])
                        |> Expect.equal
                            (List.Nonempty.append
                                (fromPair 0 [ 1, 2 ])
                                (fromPair 3 [ 4, 5 ])
                            )
            , test "handles empty list" <|
                \_ ->
                    List.Nonempty.prependList
                        []
                        (fromPair 0 [ 1, 2 ])
                        |> Expect.equal
                            (fromPair 0 [ 1, 2 ])
            , test "handles minimal" <|
                \_ ->
                    List.Nonempty.prependList
                        []
                        (fromPair 0 [])
                        |> Expect.equal
                            (fromPair 0 [])
            ]
        , describe "List.Nonempty.concat"
            [ test "handles basic operation" <|
                \_ ->
                    fromPair
                        (fromPair 0 [ 1 ])
                        [ fromPair 2 [ 3 ] ]
                        |> List.Nonempty.concat
                        |> Expect.equal
                            (fromPair 0 [ 1, 2, 3 ])
            , test "handles singleton" <|
                \_ ->
                    fromPair (fromPair 0 []) []
                        |> List.Nonempty.concat
                        |> Expect.equal
                            (fromPair 0 [])
            ]
        , describe "List.Nonempty.concatMap"
            [ test "handles basic operation" <|
                \_ ->
                    fromPair 0 [ 1, 2 ]
                        |> List.Nonempty.concatMap (\n -> fromPair n [ n ])
                        |> Expect.equal
                            (fromPair 0 [ 0, 1, 1, 2, 2 ])
            , test "handles singleton" <|
                \_ ->
                    fromPair 0 []
                        |> List.Nonempty.concatMap (\n -> fromPair n [])
                        |> Expect.equal
                            (fromPair 0 [])
            ]
        , describe "List.Nonempty.intersperse"
            [ test "handles basic operation" <|
                \_ ->
                    fromPair "127" [ "0", "0", "1" ]
                        |> List.Nonempty.intersperse "."
                        |> Expect.equal
                            (fromPair "127" [ ".", "0", ".", "0", ".", "1" ])
            , test "handles singleton" <|
                \_ ->
                    fromPair 0 []
                        |> List.Nonempty.intersperse -1
                        |> Expect.equal
                            (fromPair 0 [])
            ]
        , describe "List.Nonempty.map2"
            [ test "handles basic operation" <|
                \_ ->
                    List.Nonempty.map2
                        (\a b -> [ a, b ] |> String.join "-")
                        (fromPair "A1" [ "B1", "C1", "D1", "E1" ])
                        (fromPair "A2" [ "B2", "C2", "D2", "E2" ])
                        |> Expect.equal
                            (fromPair "A1-A2" [ "B1-B2", "C1-C2", "D1-D2", "E1-E2" ])
            , test "handles singletons" <|
                \_ ->
                    List.Nonempty.map2
                        (\a b -> [ a, b ] |> String.join "-")
                        (fromPair "A1" [])
                        (fromPair "A2" [])
                        |> Expect.equal
                            (fromPair "A1-A2" [])
            , test "handles first longer" <|
                \_ ->
                    List.Nonempty.map2
                        (\a b -> [ a, b ] |> String.join "-")
                        (fromPair "A1" [ "B1", "C1" ])
                        (fromPair "A2" [ "B2" ])
                        |> Expect.equal
                            (fromPair "A1-A2" [ "B1-B2" ])
            , test "handles second longer" <|
                \_ ->
                    List.Nonempty.map2
                        (\a b -> [ a, b ] |> String.join "-")
                        (fromPair "A1" [ "B1" ])
                        (fromPair "A2" [ "B2", "C2" ])
                        |> Expect.equal
                            (fromPair "A1-A2" [ "B1-B2" ])
            ]
        , describe "List.Nonempty.map3"
            [ test "handles basic operation" <|
                \_ ->
                    List.Nonempty.map3
                        (\a b c -> [ a, b, c ] |> String.join "-")
                        (fromPair "A1" [ "B1", "C1", "D1", "E1" ])
                        (fromPair "A2" [ "B2", "C2", "D2", "E2" ])
                        (fromPair "A3" [ "B3", "C3", "D3", "E3" ])
                        |> Expect.equal
                            (fromPair "A1-A2-A3" [ "B1-B2-B3", "C1-C2-C3", "D1-D2-D3", "E1-E2-E3" ])
            ]
        , describe "List.Nonempty.map4"
            [ test "handles basic operation" <|
                \_ ->
                    List.Nonempty.map4
                        (\a b c d -> [ a, b, c, d ] |> String.join "-")
                        (fromPair "A1" [ "B1", "C1", "D1", "E1" ])
                        (fromPair "A2" [ "B2", "C2", "D2", "E2" ])
                        (fromPair "A3" [ "B3", "C3", "D3", "E3" ])
                        (fromPair "A4" [ "B4", "C4", "D4", "E4" ])
                        |> Expect.equal
                            (fromPair "A1-A2-A3-A4" [ "B1-B2-B3-B4", "C1-C2-C3-C4", "D1-D2-D3-D4", "E1-E2-E3-E4" ])
            ]
        , describe "List.Nonempty.map5"
            [ test "handles basic operation" <|
                \_ ->
                    List.Nonempty.map5
                        (\a b c d e -> [ a, b, c, d, e ] |> String.join "-")
                        (fromPair "A1" [ "B1", "C1", "D1", "E1" ])
                        (fromPair "A2" [ "B2", "C2", "D2", "E2" ])
                        (fromPair "A3" [ "B3", "C3", "D3", "E3" ])
                        (fromPair "A4" [ "B4", "C4", "D4", "E4" ])
                        (fromPair "A5" [ "B5", "C5", "D5", "E5" ])
                        |> Expect.equal
                            (fromPair "A1-A2-A3-A4-A5" [ "B1-B2-B3-B4-B5", "C1-C2-C3-C4-C5", "D1-D2-D3-D4-D5", "E1-E2-E3-E4-E5" ])
            ]
        , describe "List.Nonempty.sort"
            [ test "handles basic operation" <|
                \_ ->
                    fromPair 1 [ 3, 5, 0, 2, 4 ]
                        |> List.Nonempty.sort
                        |> Expect.equal
                            (fromPair 0 [ 1, 2, 3, 4, 5 ])
            ]
        , describe "List.Nonempty.sortBy"
            [ test "handles basic operation" <|
                \_ ->
                    fromPair 1 [ 3, 5, 0, 2, 4 ]
                        |> List.Nonempty.sortBy (\n -> n * -1)
                        |> Expect.equal
                            (fromPair 5 [ 4, 3, 2, 1, 0 ])
            ]
        , describe "List.Nonempty.sortWith"
            [ test "handles basic operation" <|
                \_ ->
                    fromPair 1 [ 3, 5, 0, 2, 4 ]
                        |> List.Nonempty.sortWith (\a b -> compare b a)
                        |> Expect.equal
                            (fromPair 5 [ 4, 3, 2, 1, 0 ])
            ]
        , describe "List.Nonempty.head"
            [ test "handles basic operation" <|
                \_ ->
                    fromPair 0 [ 1, 2, 3 ]
                        |> List.Nonempty.head
                        |> Expect.equal 0
            ]
        , describe "List.Nonempty.tail"
            [ test "handles basic operation" <|
                \_ ->
                    fromPair 0 [ 1, 2, 3 ]
                        |> List.Nonempty.tail
                        |> Expect.equal [ 1, 2, 3 ]
            ]
        , describe "List.Nonempty.rest"
            [ test "handles basic operation" <|
                \_ ->
                    fromPair 0 [ 1, 2, 3 ]
                        |> List.Nonempty.rest
                        |> Expect.equal [ 0, 1, 2 ]
            ]
        , describe "List.Nonempty.last"
            [ test "handles basic operation" <|
                \_ ->
                    fromPair 0 [ 1, 2, 3 ]
                        |> List.Nonempty.last
                        |> Expect.equal 3
            ]
        , describe "List.Nonempty.uncons"
            [ test "handles basic operation" <|
                \_ ->
                    fromPair 0 [ 1, 2, 3 ]
                        |> List.Nonempty.uncons
                        |> Expect.equal ( 0, [ 1, 2, 3 ] )
            ]
        , describe "List.Nonempty.unconsLast"
            [ test "handles basic operation" <|
                \_ ->
                    fromPair 0 [ 1, 2, 3 ]
                        |> List.Nonempty.unconsLast
                        |> Expect.equal ( [ 0, 1, 2 ], 3 )
            ]
        , describe "List.Nonempty.take"
            [ test "handles basic operation" <|
                \_ ->
                    fromPair 0 [ 1, 2, 3 ]
                        |> List.Nonempty.take 2
                        |> Expect.equal
                            (fromPair 0 [ 1 ])
            , test "handles n=0" <|
                \_ ->
                    fromPair 0 [ 1, 2, 3 ]
                        |> List.Nonempty.take 0
                        |> Expect.equal
                            (fromPair 0 [])
            ]
        , describe "List.Nonempty.drop"
            [ test "handles basic operation" <|
                \_ ->
                    fromPair 0 [ 1, 2, 3 ]
                        |> List.Nonempty.drop 2
                        |> Expect.equal
                            (fromPair 2 [ 3 ])
            , test "handles n > length" <|
                \_ ->
                    fromPair 0 [ 1, 2, 3 ]
                        |> List.Nonempty.drop 10
                        |> Expect.equal
                            (fromPair 3 [])
            ]
        , describe "List.Nonempty.takeLast"
            [ test "handles basic operation" <|
                \_ ->
                    fromPair 0 [ 1, 2, 3 ]
                        |> List.Nonempty.takeLast 2
                        |> Expect.equal
                            (fromPair 2 [ 3 ])
            , test "handles n=0" <|
                \_ ->
                    fromPair 0 [ 1, 2, 3 ]
                        |> List.Nonempty.takeLast 0
                        |> Expect.equal
                            (fromPair 3 [])
            ]
        , describe "List.Nonempty.dropLast"
            [ test "handles basic operation" <|
                \_ ->
                    fromPair 0 [ 1, 2, 3 ]
                        |> List.Nonempty.dropLast 2
                        |> Expect.equal
                            (fromPair 0 [ 1 ])
            , test "handles n > length" <|
                \_ ->
                    fromPair 0 [ 1, 2, 3 ]
                        |> List.Nonempty.dropLast 10
                        |> Expect.equal
                            (fromPair 0 [])
            ]
        , describe "List.Nonempty.unzip2"
            [ test "handles basic operation" <|
                \_ ->
                    fromPair ( 101, 201 ) [ ( 102, 202 ), ( 103, 203 ) ]
                        |> List.Nonempty.unzip2
                        |> Expect.equal
                            ( fromPair 101 [ 102, 103 ]
                            , fromPair 201 [ 202, 203 ]
                            )
            , test "handles singleton" <|
                \_ ->
                    fromPair ( 101, 201 ) []
                        |> List.Nonempty.unzip2
                        |> Expect.equal
                            ( fromPair 101 []
                            , fromPair 201 []
                            )
            ]
        , describe "List.Nonempty.unzip3"
            [ test "handles basic operation" <|
                \_ ->
                    fromPair ( 101, 201, 301 ) [ ( 102, 202, 302 ), ( 103, 203, 303 ) ]
                        |> List.Nonempty.unzip3
                        |> Expect.equal
                            ( fromPair 101 [ 102, 103 ]
                            , fromPair 201 [ 202, 203 ]
                            , fromPair 301 [ 302, 303 ]
                            )
            , test "handles singleton" <|
                \_ ->
                    fromPair ( 101, 201, 301 ) []
                        |> List.Nonempty.unzip3
                        |> Expect.equal
                            ( fromPair 101 []
                            , fromPair 201 []
                            , fromPair 301 []
                            )
            ]
        ]
