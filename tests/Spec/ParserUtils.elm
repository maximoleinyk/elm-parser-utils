module Spec.ParserUtils
    exposing
        ( applyDescribe
        , endDescribe
        , identityDescribe
        , maybeOneDescribe
        , oneOfDescribe
        , oneOrMoreDescribe
        , repeatDescribe
        , sequenceDescribe
        , squashDescribe
        , symbolDescribe
        , zeroOrMoreDescribe
        )

import Expect exposing (Expectation)
import ParserUtils as PU
    exposing
        ( Expecting(..)
        , Parser(..)
        , Problem
        , State(..)
        , Token(..)
        , apply
        , end
        , maybeOne
        , oneOf
        , oneOrMore
        , repeat
        , sequence
        , squash
        , symbol
        , zeroOrMore
        )
import Test exposing (Test, describe, test)


squashDescribe : Test
squashDescribe =
    describe "squash"
        [ test "should replace multiple tokens with a single one" <|
            \() ->
                let
                    initialState =
                        State
                            { source = "abc"
                            , offset = 0
                            , tokens = []
                            }

                    parser =
                        squash "squashed" <|
                            sequence
                                [ symbol (\c -> c == 'a') "a"
                                , symbol (\c -> c == 'b') "b"
                                , symbol (\c -> c == 'c') "c"
                                ]

                    actual =
                        apply initialState parser

                    expected =
                        State
                            { source = "abc"
                            , offset = 3
                            , tokens =
                                [ Token
                                    { state = "squashed"
                                    , position = 0
                                    , value = "abc"
                                    }
                                ]
                            }
                in
                Expect.equal (Ok expected) actual
        , test "should return Err if one of the low level parsers returned Err" <|
            \() ->
                let
                    initialState =
                        State
                            { source = "abc"
                            , offset = 0
                            , tokens = []
                            }

                    parser =
                        squash "squashed" <|
                            sequence
                                [ symbol (\c -> c == 'a') "a"
                                , symbol (\c -> c == 'd') "d"
                                , symbol (\c -> c == 'd') "d"
                                ]

                    actual =
                        apply initialState parser

                    expected =
                        PU.Problem
                            { latestState =
                                State
                                    { source = "abc"
                                    , offset = 1
                                    , tokens =
                                        [ Token
                                            { state = "a"
                                            , position = 0
                                            , value = "a"
                                            }
                                        ]
                                    }
                            , expecting = ExpectingList [ "d" ]
                            , offset = 0
                            }
                in
                Expect.equal (Err expected) actual
        ]


repeatDescribe : Test
repeatDescribe =
    describe "repeat"
        [ test "should return Err if nothing was parsed" <|
            \() ->
                let
                    initialState =
                        State
                            { source = "aaa"
                            , offset = 0
                            , tokens = []
                            }

                    parser =
                        repeat <| symbol (\c -> c == 'b') ()

                    actual =
                        apply initialState parser

                    expected =
                        PU.Problem
                            { latestState =
                                State
                                    { source = "aaa"
                                    , offset = 0
                                    , tokens = []
                                    }
                            , expecting = ExpectingSingle ()
                            , offset = 0
                            }
                in
                Expect.equal (Err expected) actual
        , test "should return Ok if at least once parser returned Ok" <|
            \() ->
                let
                    initialState =
                        State
                            { source = "a"
                            , offset = 0
                            , tokens = []
                            }

                    parser =
                        repeat <| symbol (\c -> c == 'a') ()

                    actual =
                        apply initialState parser

                    expected =
                        State
                            { source = "a"
                            , offset = 1
                            , tokens =
                                [ Token
                                    { state = ()
                                    , position = 0
                                    , value = "a"
                                    }
                                ]
                            }
                in
                Expect.equal (Ok expected) actual
        ]


sequenceDescribe : Test
sequenceDescribe =
    describe "sequence"
        [ test "should return Err if any of the parsers returned Err" <|
            \() ->
                let
                    initialState =
                        State
                            { source = "abc"
                            , offset = 0
                            , tokens = []
                            }

                    parser =
                        sequence
                            [ symbol (\c -> c == 'a') ()
                            , symbol (\c -> c == 'b') ()
                            , symbol (\c -> c == 'd') ()
                            ]

                    actual =
                        apply initialState parser

                    expected =
                        PU.Problem
                            { latestState =
                                State
                                    { source = "abc"
                                    , offset = 2
                                    , tokens =
                                        [ Token
                                            { state = ()
                                            , position = 1
                                            , value = "b"
                                            }
                                        , Token
                                            { state = ()
                                            , position = 0
                                            , value = "a"
                                            }
                                        ]
                                    }
                            , expecting = ExpectingSingle ()
                            , offset = 2
                            }
                in
                Expect.equal (Err expected) actual
        , test "should return Ok if all parsers returned Ok" <|
            \() ->
                let
                    initialState =
                        State
                            { source = "abc"
                            , offset = 0
                            , tokens = []
                            }

                    parser =
                        sequence
                            [ symbol (\c -> c == 'a') ()
                            , symbol (\c -> c == 'b') ()
                            , symbol (\c -> c == 'c') ()
                            ]

                    actual =
                        apply initialState parser

                    expected =
                        State
                            { source = "abc"
                            , offset = 3
                            , tokens =
                                [ Token
                                    { state = ()
                                    , position = 2
                                    , value = "c"
                                    }
                                , Token
                                    { state = ()
                                    , position = 1
                                    , value = "b"
                                    }
                                , Token
                                    { state = ()
                                    , position = 0
                                    , value = "a"
                                    }
                                ]
                            }
                in
                Expect.equal (Ok expected) actual
        ]


applyDescribe : Test
applyDescribe =
    describe "apply"
        [ test "should return Result after applying initial state" <|
            \() ->
                let
                    initialState =
                        State
                            { source = "sample"
                            , offset = 0
                            , tokens = []
                            }

                    parser =
                        Parser <| \state -> Ok state

                    actual =
                        apply initialState parser
                in
                Expect.equal (Ok initialState) actual
        ]


oneOfDescribe : Test
oneOfDescribe =
    describe "oneOf"
        [ test "parser should return Err if non of the provided parsers returned Ok" <|
            \() ->
                let
                    initialState =
                        State
                            { source = "c"
                            , offset = 0
                            , tokens = []
                            }

                    actual =
                        apply initialState <|
                            oneOf
                                [ symbol (\c -> c == 'a') "a"
                                , symbol (\c -> c == 'b') "b"
                                ]

                    expected =
                        PU.Problem
                            { latestState =
                                State
                                    { source = "c"
                                    , offset = 0
                                    , tokens = []
                                    }
                            , expecting = ExpectingList [ "b", "a" ]
                            , offset = 0
                            }
                in
                Expect.equal (Err expected) actual
        , test "parser should return Ok if one of the parsers returns Ok" <|
            \() ->
                let
                    initialState =
                        State
                            { source = "ac"
                            , offset = 0
                            , tokens = []
                            }

                    actual =
                        apply initialState <|
                            oneOf
                                [ symbol (\c -> c == 'b') ()
                                , symbol (\c -> c == 'a') ()
                                ]

                    expected =
                        State
                            { source = "ac"
                            , offset = 1
                            , tokens =
                                [ Token
                                    { state = ()
                                    , position = 0
                                    , value = "a"
                                    }
                                ]
                            }
                in
                Expect.equal (Ok expected) actual
        ]


oneOrMoreDescribe : Test
oneOrMoreDescribe =
    describe "oneOrMore"
        [ test "parser should return Err if nothing was parsed" <|
            \() ->
                let
                    initialState =
                        State
                            { source = "aaa"
                            , offset = 0
                            , tokens = []
                            }

                    parser =
                        symbol (\c -> c == 'b') ()

                    actual =
                        apply initialState <|
                            oneOrMore parser

                    expected =
                        PU.Problem
                            { latestState =
                                State
                                    { source = "aaa"
                                    , offset = 0
                                    , tokens = []
                                    }
                            , offset = 0
                            , expecting = ExpectingSingle ()
                            }
                in
                Expect.equal (Err expected) actual
        , test "parser should return Ok with a single token" <|
            \() ->
                let
                    initialState =
                        State
                            { source = "a"
                            , offset = 0
                            , tokens = []
                            }

                    parser =
                        symbol (\c -> c == 'a') ()

                    actual =
                        apply initialState <|
                            oneOrMore parser

                    expected =
                        State
                            { source = "a"
                            , offset = 1
                            , tokens =
                                [ Token
                                    { state = ()
                                    , position = 0
                                    , value = "a"
                                    }
                                ]
                            }
                in
                Expect.equal (Ok expected) actual
        , test "parser should return Ok with a two parsed tokens" <|
            \() ->
                let
                    initialState =
                        State
                            { source = "aa"
                            , offset = 0
                            , tokens = []
                            }

                    parser =
                        symbol (\c -> c == 'a') ()

                    actual =
                        apply initialState <|
                            oneOrMore parser

                    expected =
                        State
                            { source = "aa"
                            , offset = 2
                            , tokens =
                                [ Token
                                    { state = ()
                                    , position = 1
                                    , value = "a"
                                    }
                                , Token
                                    { state = ()
                                    , position = 0
                                    , value = "a"
                                    }
                                ]
                            }
                in
                Expect.equal (Ok expected) actual
        ]


symbolDescribe : Test
symbolDescribe =
    describe "symbol"
        [ test "should successfully parse next character in the sequence" <|
            \() ->
                let
                    initialState =
                        State
                            { source = "a"
                            , offset = 0
                            , tokens = []
                            }

                    parser =
                        symbol (\c -> c == 'a') ()

                    actual =
                        apply initialState parser

                    expected =
                        State
                            { source = "a"
                            , offset = 1
                            , tokens =
                                [ Token
                                    { state = ()
                                    , position = 0
                                    , value = "a"
                                    }
                                ]
                            }
                in
                Expect.equal (Ok expected) actual
        , test "parser should not parse unknown symbol" <|
            \() ->
                let
                    initialState =
                        State
                            { source = "a"
                            , offset = 0
                            , tokens = []
                            }

                    parser =
                        symbol (\c -> c == 'b') ()

                    actual =
                        apply initialState parser

                    expected =
                        PU.Problem
                            { latestState =
                                State
                                    { source = "a"
                                    , offset = 0
                                    , tokens = []
                                    }
                            , offset = 0
                            , expecting = ExpectingSingle ()
                            }
                in
                Expect.equal (Err expected) actual
        ]


identityDescribe : Test
identityDescribe =
    describe "identity"
        [ test "parsed actual should equal to initial state" <|
            \() ->
                let
                    initialState =
                        State
                            { source = "sample"
                            , offset = 0
                            , tokens = []
                            }

                    actual =
                        apply initialState PU.identity
                in
                Expect.equal (Ok initialState) actual
        ]


endDescribe : Test
endDescribe =
    describe "end"
        [ test "parser should return Ok while reaching the end of the string" <|
            \() ->
                let
                    initialState =
                        State
                            { source = ""
                            , offset = 0
                            , tokens = []
                            }

                    actual =
                        apply initialState end
                in
                Expect.equal (Ok initialState) actual
        , test "parser should return Err if there are characters left in the sequence" <|
            \() ->
                let
                    initialState =
                        State
                            { source = "sample"
                            , offset = 0
                            , tokens = []
                            }

                    actual =
                        apply initialState end

                    expected =
                        PU.Problem
                            { latestState =
                                State
                                    { source = "sample"
                                    , offset = 0
                                    , tokens = []
                                    }
                            , offset = 0
                            , expecting = ExpectingEnd
                            }
                in
                Expect.equal (Err expected) actual
        ]


maybeOneDescribe : Test
maybeOneDescribe =
    describe "maybeOne"
        [ test "parser should return Ok if nothing was parsed" <|
            \() ->
                let
                    initialState =
                        State
                            { source = "a"
                            , offset = 0
                            , tokens = []
                            }

                    parser =
                        symbol (\c -> c == 'b') ()

                    actual =
                        apply initialState <|
                            maybeOne parser
                in
                Expect.equal (Ok initialState) actual
        , test "parser should return Ok with new state" <|
            \() ->
                let
                    initialState =
                        State
                            { source = "a"
                            , offset = 0
                            , tokens = []
                            }

                    parser =
                        symbol (\c -> c == 'a') ()

                    actual =
                        apply initialState <|
                            maybeOne parser

                    expected =
                        State
                            { source = "a"
                            , offset = 1
                            , tokens =
                                [ Token
                                    { state = ()
                                    , position = 0
                                    , value = "a"
                                    }
                                ]
                            }
                in
                Expect.equal (Ok expected) actual
        ]


zeroOrMoreDescribe : Test
zeroOrMoreDescribe =
    describe "zeroOrMore"
        [ test "parser should return Ok if nothing was parsed" <|
            \() ->
                let
                    initialState =
                        State
                            { source = "sample"
                            , offset = 0
                            , tokens = []
                            }

                    parser =
                        symbol (\c -> c == 'b') ()

                    actual =
                        apply initialState <|
                            zeroOrMore parser
                in
                Expect.equal (Ok initialState) actual
        , test "parser should return Ok with a single token" <|
            \() ->
                let
                    initialState =
                        State
                            { source = "a"
                            , offset = 0
                            , tokens = []
                            }

                    parser =
                        symbol (\c -> c == 'a') ()

                    actual =
                        apply initialState <|
                            zeroOrMore parser

                    expected =
                        State
                            { source = "a"
                            , offset = 1
                            , tokens =
                                [ Token
                                    { state = ()
                                    , position = 0
                                    , value = "a"
                                    }
                                ]
                            }
                in
                Expect.equal (Ok expected) actual
        , test "parser should return Ok with a two parsed tokens" <|
            \() ->
                let
                    initialState =
                        State
                            { source = "aa"
                            , offset = 0
                            , tokens = []
                            }

                    parser =
                        symbol (\c -> c == 'a') ()

                    actual =
                        apply initialState <|
                            zeroOrMore parser

                    expected =
                        State
                            { source = "aa"
                            , offset = 2
                            , tokens =
                                [ Token
                                    { state = ()
                                    , position = 1
                                    , value = "a"
                                    }
                                , Token
                                    { state = ()
                                    , position = 0
                                    , value = "a"
                                    }
                                ]
                            }
                in
                Expect.equal (Ok expected) actual
        ]
