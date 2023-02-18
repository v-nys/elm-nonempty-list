module List.Nonempty.ExtraTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (..)
import List.Nonempty exposing (fromPair)
import List.Nonempty.Extra
import Test exposing (..)


suite : Test
suite =
    describe "List.Nonempty"
        [ describe "List.Nonempty.Extra.appendList"
            [ test "handles basic operation" <|
                \_ ->
                    List.Nonempty.Extra.appendList
                        (fromPair 0 [ 1, 2 ])
                        [ 3, 4, 5 ]
                        |> Expect.equal
                            (List.Nonempty.append
                                (fromPair 0 [ 1, 2 ])
                                (fromPair 3 [ 4, 5 ])
                            )
            , test "handles empty list" <|
                \_ ->
                    List.Nonempty.Extra.appendList
                        (fromPair 0 [ 1, 2 ])
                        []
                        |> Expect.equal
                            (fromPair 0 [ 1, 2 ])
            , test "handles minimal" <|
                \_ ->
                    List.Nonempty.Extra.appendList
                        (fromPair 0 [])
                        []
                        |> Expect.equal
                            (fromPair 0 [])
            ]
        , describe "List.Nonempty.prependList"
            [ test "handles basic operation" <|
                \_ ->
                    List.Nonempty.Extra.prependList
                        [ 0, 1, 2 ]
                        (fromPair 3 [ 4, 5 ])
                        |> Expect.equal
                            (List.Nonempty.append
                                (fromPair 0 [ 1, 2 ])
                                (fromPair 3 [ 4, 5 ])
                            )
            , test "handles empty list" <|
                \_ ->
                    List.Nonempty.Extra.prependList
                        []
                        (fromPair 0 [ 1, 2 ])
                        |> Expect.equal
                            (fromPair 0 [ 1, 2 ])
            , test "handles minimal" <|
                \_ ->
                    List.Nonempty.Extra.prependList
                        []
                        (fromPair 0 [])
                        |> Expect.equal
                            (fromPair 0 [])
            ]
        , describe "List.Nonempty.Extra.intersperse"
            [ test "handles basic operation" <|
                \_ ->
                    fromPair "127" [ "0", "0", "1" ]
                        |> List.Nonempty.Extra.intersperse "."
                        |> Expect.equal
                            (fromPair "127" [ ".", "0", ".", "0", ".", "1" ])
            , test "handles singleton" <|
                \_ ->
                    fromPair 0 []
                        |> List.Nonempty.Extra.intersperse -1
                        |> Expect.equal
                            (fromPair 0 [])
            ]
        ]
