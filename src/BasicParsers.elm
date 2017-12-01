module BasicParsers
    exposing
        ( ampersand
        , apostrophe
        , at
        , backslash
        , circumflexAccent
        , closeBracket
        , colon
        , comma
        , commaSepList
        , dollar
        , doubleQuote
        , equals
        , exclamation
        , float
        , fullStop
        , graveAccent
        , greaterThan
        , hash
        , int
        , keyword
        , leftCurlyBracket
        , leftParenthesis
        , lessThan
        , minus
        , notSymbol
        , openBracket
        , percent
        , plus
        , questionMark
        , rightCurlyBracket
        , rightParenthesis
        , semicolon
        , slash
        , space
        , symbol
        , tilde
        , underscore
        , verticalBar
        )

{-| A set of simple parsers.


# Character parsers

@docs space, ampersand, apostrophe, at, backslash, circumflexAccent, closeBracket, colon, comma, dollar, doubleQuote, equals, exclamation, fullStop, graveAccent, greaterThan, hash, leftCurlyBracket, leftParenthesis, lessThan, minus, openBracket, percent, plus, questionMark, rightCurlyBracket, rightParenthesis, semicolon, slash, tilde, underscore, verticalBar, space


# Number parsers

@docs int, float


# List parsers

@docs commaSepList


# Helper parsers

@docs keyword, symbol, notSymbol

-}

import ParserUtils as PU
    exposing
        ( Parser
        , apply
        , maybeOne
        , oneOf
        , oneOrMore
        , repeat
        , sequence
        , squash
        , zeroOrMore
        )


{-| Symbol parser that returns Ok if `char` parameter equals to the next
character in the sequence.
-}
symbol : Char -> a -> Parser a
symbol char context =
    PU.symbol (\c -> c == char) context


{-| Symbol parser that returns Ok if `char` parameter does not equal to
the next character in the sequence.
-}
notSymbol : Char -> a -> Parser a
notSymbol char context =
    PU.symbol (\c -> c /= char) context


{-| Parser for string keys.
-}
keyword : String -> a -> Parser a
keyword key context =
    let
        mapper =
            \char -> symbol char context

        chars =
            String.toList key
    in
    squash context <|
        sequence <|
            List.map mapper chars


{-| Parser for integer number.
-}
int : a -> Parser a
int context =
    let
        mapper =
            \d -> symbol d context

        digits =
            List.map mapper <| String.toList "1234567890"
    in
    squash context <|
        sequence
            [ maybeOne <|
                oneOf
                    [ plus context
                    , minus context
                    ]
            , repeat <| oneOf digits
            ]


{-| Parser for decimal number.
-}
float : a -> Parser a
float context =
    let
        mapper =
            \d -> symbol d context
    in
    squash context <|
        sequence
            [ maybeOne <|
                oneOf
                    [ symbol '+' context
                    , symbol '-' context
                    ]
            , oneOf
                [ symbol '0' context
                , sequence
                    [ oneOf <| List.map mapper <| String.toList "123456789"
                    , maybeOne <| repeat <| oneOf <| List.map mapper <| String.toList "1234567890"
                    ]
                ]
            , symbol '.' context
            , repeat <| oneOf <| List.map mapper <| String.toList "1234567890"
            ]


commaSepListHelper : a -> Parser a -> Parser a
commaSepListHelper context parser =
    PU.Parser <|
        \initialState ->
            let
                result =
                    apply initialState <|
                        sequence
                            [ zeroOrMore <| space context
                            , parser
                            ]
            in
            case result of
                Ok nextState ->
                    apply nextState <|
                        sequence
                            [ zeroOrMore <| space context
                            , oneOf
                                [ closeBracket context
                                , sequence
                                    [ comma context
                                    , commaSepListHelper context parser
                                    ]
                                ]
                            ]

                (Err x) as error ->
                    error


{-| Parser for list of comma separated values. Type can be defined by a parameter

    commaSepList IntList int
    commaSepList FloatList float

-}
commaSepList : a -> Parser a -> Parser a
commaSepList context parser =
    squash context <|
        sequence
            [ openBracket context
            , zeroOrMore <| space context
            , oneOf
                [ commaSepListHelper context parser
                , closeBracket context
                ]
            ]


{-| Parser for space symbol.
-}
space : a -> Parser a
space context =
    symbol ' ' context


{-| Parser for `@` symbol.
-}
at : a -> Parser a
at context =
    symbol '@' context


{-| Parser for `(` symbol.
-}
leftParenthesis : a -> Parser a
leftParenthesis context =
    symbol '(' context


{-| Parser for `)` symbol.
-}
rightParenthesis : a -> Parser a
rightParenthesis context =
    symbol ')' context


{-| Parser for `[` symbol.
-}
openBracket : a -> Parser a
openBracket context =
    symbol '[' context


{-| Parser for `]` symbol.
-}
closeBracket : a -> Parser a
closeBracket context =
    symbol ']' context


{-| Parser for `"` symbol.
-}
doubleQuote : a -> Parser a
doubleQuote context =
    symbol '"' context


{-| Parser for `+` symbol.
-}
plus : a -> Parser a
plus context =
    symbol '+' context


{-| Parser for `-` symbol.
-}
minus : a -> Parser a
minus context =
    symbol '-' context


{-| Parser for `_` symbol.
-}
underscore : a -> Parser a
underscore context =
    symbol '_' context


{-| Parser for `/` symbol.
-}
slash : a -> Parser a
slash context =
    symbol '/' context


{-| Parser for `` symbol.
-}
backslash : a -> Parser a
backslash context =
    symbol '\\' context


{-| Parser for `*` symbol.
-}
asterisk : a -> Parser a
asterisk context =
    symbol '*' context


{-| Parser for `&` symbol.
-}
ampersand : a -> Parser a
ampersand context =
    symbol '&' context


{-| Parser for `:` symbol.
-}
colon : a -> Parser a
colon context =
    symbol ':' context


{-| Parser for `;` symbol.
-}
semicolon : a -> Parser a
semicolon context =
    symbol ';' context


{-| Parser for `|` symbol.
-}
verticalBar : a -> Parser a
verticalBar context =
    symbol '|' context


{-| Parser for `~` symbol.
-}
tilde : a -> Parser a
tilde context =
    symbol '~' context


{-| Parser for `{` symbol.
-}
leftCurlyBracket : a -> Parser a
leftCurlyBracket context =
    symbol '{' context


{-| Parser for `}` symbol.
-}
rightCurlyBracket : a -> Parser a
rightCurlyBracket context =
    symbol '}' context


{-| Parser for ``` symbol.
-}
graveAccent : a -> Parser a
graveAccent context =
    symbol '`' context


{-| Parser for `?` symbol.
-}
questionMark : a -> Parser a
questionMark context =
    symbol '?' context


{-| Parser for `'` symbol.
-}
apostrophe : a -> Parser a
apostrophe context =
    symbol '\'' context


{-| Parser for `.` symbol.
-}
fullStop : a -> Parser a
fullStop context =
    symbol '.' context


{-| Parser for `,` symbol.
-}
comma : a -> Parser a
comma context =
    symbol ',' context


{-| Parser for `%` symbol.
-}
percent : a -> Parser a
percent context =
    symbol '%' context


{-| Parser for `^` symbol.
-}
circumflexAccent : a -> Parser a
circumflexAccent context =
    symbol '^' context


{-| Parser for `$` symbol.
-}
dollar : a -> Parser a
dollar context =
    symbol '$' context


{-| Parser for `#` symbol.
-}
hash : a -> Parser a
hash context =
    symbol '#' context


{-| Parser for `!` symbol.
-}
exclamation : a -> Parser a
exclamation context =
    symbol '!' context


{-| Parser for `<` symbol.
-}
lessThan : a -> Parser a
lessThan context =
    symbol '<' context


{-| Parser for `>` symbol.
-}
greaterThan : a -> Parser a
greaterThan context =
    symbol '>' context


{-| Parser for `=` symbol.
-}
equals : a -> Parser a
equals context =
    symbol '=' context
