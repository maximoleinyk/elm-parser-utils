module Spec.BasicParsers exposing (..)

import BasicParsers exposing (..)
import Expect
import ParserUtils as PU
import Test exposing (Test, describe, test)


goodParse : PU.Parser a -> String -> Expect.Expectation
goodParse parser source =
    let
        initialState =
            PU.State
                { source = source
                , offset = 0
                , tokens = []
                }

        result =
            PU.apply initialState parser
    in
    case result of
        Ok (PU.State { tokens }) ->
            case List.head tokens of
                Nothing ->
                    Expect.fail "parse operation was successfull, however token was not added"

                Just (PU.Token next) ->
                    Expect.equal next.value source

        Err x ->
            Expect.fail "parser failed"


badParse : PU.Parser a -> String -> Expect.Expectation
badParse parser source =
    let
        initialState =
            PU.State
                { source = source
                , offset = 0
                , tokens = []
                }

        result =
            PU.apply initialState parser
    in
    case result of
        Ok (PU.State { tokens }) ->
            Expect.fail "parsed successfully"

        Err x ->
            Expect.pass


mainTest : Test
mainTest =
    describe "BasicParsers"
        [ test "ampersand" <| \() -> goodParse (ampersand ()) "&"
        , test "apostrophe" <| \() -> goodParse (apostrophe ()) "'"
        , test "at" <| \() -> goodParse (at ()) "@"
        , test "backslash" <| \() -> goodParse (backslash ()) "\\"
        , test "circumflexAccent" <| \() -> goodParse (circumflexAccent ()) "^"
        , test "closeBracket" <| \() -> goodParse (closeBracket ()) "]"
        , test "colon" <| \() -> goodParse (colon ()) ":"
        , test "comma" <| \() -> goodParse (comma ()) ","
        , describe "commaSepList"
            [ test "case 1" <| \() -> goodParse (commaSepList () (int ())) "[1,2,3]"
            , test "case 2" <| \() -> goodParse (commaSepList () (int ())) "[ 1 , 2 , 3 ]"
            , test "case 3" <| \() -> goodParse (commaSepList () (int ())) "[]"
            , test "case 4" <| \() -> goodParse (commaSepList () (int ())) "[   ]"
            , test "case 5" <| \() -> badParse (commaSepList () (int ())) "[1,]"
            , test "case 6" <| \() -> badParse (commaSepList () (int ())) "[1,2"
            ]
        , test "dollar" <| \() -> goodParse (dollar ()) "$"
        , test "doubleQuote" <| \() -> goodParse (doubleQuote ()) "\""
        , test "equals" <| \() -> goodParse (doubleQuote ()) "\""
        , test "exclamation" <| \() -> goodParse (exclamation ()) "!"
        , describe "float"
            [ test "case 1" <| \() -> goodParse (float ()) "0.0"
            , test "case 2" <| \() -> goodParse (float ()) "0.001"
            , test "case 3" <| \() -> goodParse (float ()) "1.000"
            , test "case 4" <| \() -> goodParse (float ()) "1.100"
            , test "case 5" <| \() -> goodParse (float ()) "10.001"
            , test "case 6" <| \() -> goodParse (float ()) "100.001"
            , test "case 7" <| \() -> goodParse (float ()) "-100.001"
            , test "case 8" <| \() -> goodParse (float ()) "+100.001"
            , test "case 9" <| \() -> badParse (float ()) "00.01"
            , test "case 10" <| \() -> badParse (float ()) "01.123"
            ]
        , test "fullStop" <| \() -> goodParse (fullStop ()) "."
        , test "graveAccent" <| \() -> goodParse (graveAccent ()) "`"
        , test "greaterThan" <| \() -> goodParse (greaterThan ()) ">"
        , test "hash" <| \() -> goodParse (hash ()) "#"
        , describe "int"
            [ test "case 1" <| \() -> goodParse (int ()) "0"
            , test "case 2" <| \() -> goodParse (int ()) "1"
            , test "case 3" <| \() -> goodParse (int ()) "+1"
            , test "case 4" <| \() -> goodParse (int ()) "-1"
            , test "case 5" <| \() -> goodParse (int ()) "10"
            , test "case 6" <| \() -> goodParse (int ()) "100"
            , test "case 7" <| \() -> goodParse (int ()) "-0"
            , test "case 8" <| \() -> goodParse (int ()) "+0"
            ]
        , test "keyword" <| \() -> goodParse (keyword "key" ()) "key"
        , test "leftCurlyBracket" <| \() -> goodParse (leftCurlyBracket ()) "{"
        , test "leftParenthesis" <| \() -> goodParse (leftParenthesis ()) "("
        , test "lessThan" <| \() -> goodParse (lessThan ()) "<"
        , test "minus" <| \() -> goodParse (minus ()) "-"
        , test "openBracket" <| \() -> goodParse (openBracket ()) "["
        , test "percent" <| \() -> goodParse (percent ()) "%"
        , test "plus" <| \() -> goodParse (plus ()) "+"
        , test "questionMark" <| \() -> goodParse (questionMark ()) "?"
        , test "rightCurlyBracket" <| \() -> goodParse (rightCurlyBracket ()) "}"
        , test "rightParenthesis" <| \() -> goodParse (rightParenthesis ()) ")"
        , test "semicolon" <| \() -> goodParse (semicolon ()) ";"
        , test "slash" <| \() -> goodParse (slash ()) "/"
        , test "space" <| \() -> goodParse (space ()) " "
        , test "tilde" <| \() -> goodParse (tilde ()) "~"
        , test "underscore" <| \() -> goodParse (underscore ()) "_"
        , test "verticalBar" <| \() -> goodParse (verticalBar ()) "|"
        , describe "symbol"
            [ test "case 1" <| \() -> goodParse (symbol 'a' ()) "a"
            , test "case 2" <| \() -> badParse (symbol 'b' ()) "a"
            ]
        , describe "notSymbol"
            [ test "case 1" <| \() -> goodParse (notSymbol 'b' ()) "a"
            , test "case 2" <| \() -> badParse (notSymbol 'a' ()) "a"
            ]
        ]
