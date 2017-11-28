module ParserUtils
    exposing
        ( Expecting(..)
        , Parser(..)
        , Problem(..)
        , State(..)
        , Token(..)
        , apply
        , end
        , identity
        , maybeOne
        , oneOf
        , oneOrMore
        , repeat
        , sequence
        , squash
        , symbol
        , zeroOrMore
        )

{-| A utility library for writing custom parsers.


# Types

@docs Parser, State, Token, Problem, Expecting


# Basic parsers

@docs identity, symbol, end


# Utility parsers

@docs maybeOne, oneOrMore, zeroOrMore, oneOf, sequence, repeat, squash, apply

-}


{-| Basic type which defined a parser.

Each parser takes some state and produces result. Result can be either
positive or negative (@see Result module). Positive means provided text
was parsed successfully, or negative - parse operation was failed. In
case of positive result there will be a new state returned (@see State).
In case of fail Problem type would be returned (@see Problem).

-}
type Parser a
    = Parser (State a -> Result (Problem a) (State a))


{-| A type which represents state of parsed text.
-}
type State a
    = State
        { source : String
        , offset : Int
        , tokens : List (Token a)
        }


{-| A type which represents parsed token.
-}
type Token a
    = Token
        { state : a
        , position : Int
        , value : String
        }


{-| Problem type which represents an error that occurred during parsing.
-}
type Problem a
    = Problem
        { latestState : State a
        , expecting : Expecting a
        , offset : Int
        }


{-| Defines a token that is expected to be parsed next in the parser sequence.
-}
type Expecting a
    = ExpectingEnd
    | ExpectingSingle a
    | ExpectingList (List a)


{-| } A parser which always returns Ok with a given state. It can be used in
cases where you just need to return some parser while nothing should be parsed.

    let
        parser =
            if condition then
                symbol (\c -> c == '.') DotToken
            else
                identity
    in
    apply state parser

-}
identity : Parser a
identity =
    Parser <| \state -> Ok state


{-| A single character parser. You can use `sequence` parser to achieve more
complicated logic (@see sequence).

    space : Parser Space
    space =
        symbol (\c -> c == ' ') Space

-}
symbol : (Char -> Bool) -> a -> Parser a
symbol predicate context =
    Parser <|
        \((State { source, offset, tokens }) as initialState) ->
            let
                newOffset =
                    offset + 1

                result =
                    String.slice offset newOffset source

                char =
                    List.head <| String.toList result

                error =
                    Err <|
                        Problem
                            { latestState = initialState
                            , expecting = ExpectingSingle context
                            , offset = offset
                            }
            in
            case char of
                Nothing ->
                    error

                Just char ->
                    if predicate char then
                        let
                            token =
                                Token
                                    { state = context
                                    , position = offset
                                    , value = result
                                    }
                        in
                        Ok
                            (State
                                { source = source
                                , offset = newOffset
                                , tokens = token :: tokens
                                }
                            )
                    else
                        error


{-| Parser that throws an error in case if we have more characters to parse
but we reached the end of the parser chain.

    let
        start =
            keyword "string" StringWord

        initialState =
            State
                { source = "string to be parsed"
                , offset = 0
                , tokens = []
                }
    in
    apply initialState <| sequence [ start, end ]

-}
end : Parser a
end =
    Parser <|
        \((State { source, offset }) as initialState) ->
            if String.length source /= offset then
                Err <|
                    Problem
                        { latestState = initialState
                        , expecting = ExpectingEnd
                        , offset = offset
                        }
            else
                Ok initialState


{-| Calls given parser exactly once. In case of fail returns initial state.

    number : Parser Token
    number =
        sequence
            [ maybeOne <|
                oneOf
                    [ plus
                    , minus
                    ]
            , digits
            ]

-}
maybeOne : Parser a -> Parser a
maybeOne parser =
    Parser <|
        \initialState ->
            case apply initialState parser of
                (Ok nextState) as result ->
                    result

                Err _ ->
                    Ok initialState


{-| Calls parser zero or more times. Always returns (Ok State).

    zeroOrMoreSpaces : Parser Spaces
    zeroOrMoreSpaces =
        zeroOrMore <| symbol (\c -> c == ' ') Spaces

-}
zeroOrMore : Parser a -> Parser a
zeroOrMore parser =
    maybeOne <| oneOrMore parser


{-| Calls parser at least once. Returns (Err Problem) if was unable to parse
at least once.

      oneOrMoreSpaces : Parser Spaces
      oneOrMoreSpaces =
          oneOrMore <| symbol (\c -> c == ' ') Spaces

-}
oneOrMore : Parser a -> Parser a
oneOrMore (Parser parser) =
    Parser <|
        \initialState ->
            let
                helperFunc minTimesMemo nextState =
                    case parser nextState of
                        Ok state ->
                            helperFunc (minTimesMemo + 1) state

                        (Err x) as error ->
                            if minTimesMemo < 1 then
                                error
                            else if minTimesMemo == 0 then
                                Ok initialState
                            else
                                Ok nextState
            in
            helperFunc 0 initialState


{-| Tries to parse one by one parsers provided in the list and fails if non of
them were successful.

    numberParser : Parser
    numberParser =
        oneOf
            [ float
            , int
            ]

-}
oneOf : List (Parser a) -> Parser a
oneOf parsers =
    Parser <|
        \((State { offset }) as initialState) ->
            let
                helper parsers result =
                    case parsers of
                        [] ->
                            Err <|
                                Problem
                                    { latestState = initialState
                                    , expecting = ExpectingList <| removeDuplicates result
                                    , offset = offset
                                    }

                        parser :: restParsers ->
                            case apply initialState parser of
                                (Ok nextState) as result ->
                                    result

                                (Err (Problem x)) as error ->
                                    if offset == x.offset then
                                        helper restParsers <| List.append result <| getExpectingValue x.expecting
                                    else
                                        error
            in
            helper parsers []


{-| Calls sequence of parsers in defined order. Returns Ok in case if all of
them returned Ok result. Returns Err if any of parsers returned false result.

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

-}
sequence : List (Parser a) -> Parser a
sequence parsers =
    Parser <|
        \initialState ->
            let
                helper latestState parsers =
                    case parsers of
                        [] ->
                            Ok latestState

                        (Parser nextParser) :: restParsers ->
                            case nextParser latestState of
                                Ok nextState ->
                                    helper nextState restParsers

                                (Err _) as result ->
                                    result
            in
            helper initialState parsers


{-| Keeps calling a given parser as many times as possible. Returns Err
if nothing was parsed.

    nonBreakingWord : Parser Symbol
    nonBreakingWord =
        let
            notSpace =
                symbol (\c -> c /= ' ') Symbol
        in
        repeat notSpace

-}
repeat : Parser a -> Parser a
repeat parser =
    Parser <|
        \initialState ->
            let
                helper =
                    \nextState prevState initialState ->
                        case apply nextState parser of
                            Ok newState ->
                                {- zeroOrMore may return Ok if nothing was parsed -}
                                if newState == prevState then
                                    Ok newState
                                else
                                    helper newState nextState initialState

                            (Err x) as error ->
                                {- if nothing else can be parsed -}
                                if nextState == initialState then
                                    error
                                else
                                    Ok nextState
            in
            helper initialState initialState initialState


{-| After parsing multiple symbols state will be updated with multiple items
aka tokens. This function allows to merged (squash) multiple tokens into a
single one.

    isNotOperator : Parser Operator
    isNotOperator =
        let
            context =
                Operator
        in
        squash context <|
            sequence
                [ keyword "is" context
                , spaces AtLeastOne
                , keyword "not" context
                ]

-}
squash : a -> Parser a -> Parser a
squash context parser =
    Parser <|
        \(State initialState) ->
            case apply (State initialState) parser of
                Ok (State nextState) ->
                    let
                        diffNumber =
                            List.length nextState.tokens - List.length initialState.tokens

                        diffTokens =
                            List.take diffNumber nextState.tokens

                        value =
                            List.foldl (\a b -> a ++ b) "" (List.map (\(Token data) -> data.value) diffTokens)

                        newOffset =
                            initialState.offset + String.length value

                        token =
                            Token
                                { state = context
                                , position = initialState.offset
                                , value = value
                                }
                    in
                    Ok
                        (State
                            { source = initialState.source
                            , offset = newOffset
                            , tokens = token :: initialState.tokens
                            }
                        )

                (Err (Problem { latestState, expecting })) as error ->
                    Err <|
                        Problem
                            { latestState = latestState
                            , expecting =
                                ExpectingList <|
                                    removeDuplicates <|
                                        getExpectingValue expecting
                            , offset = initialState.offset
                            }


{-| Utility function that calls parser with a given state.

    let
        initialState =
            State
                { source = source
                , offset = 0
                , tokens = []
                }
        parser =
            Parser <| \state -> Ok state

    in
    apply state parser

-}
apply : State a -> Parser a -> Result (Problem a) (State a)
apply state (Parser parse) =
    parse state


getExpectingValue : Expecting a -> List a
getExpectingValue expecting =
    case expecting of
        ExpectingSingle value ->
            [ value ]

        ExpectingList list ->
            list

        ExpectingEnd ->
            []


removeDuplicates : List a -> List a
removeDuplicates list =
    let
        predicate =
            \item memo ->
                if List.member item memo then
                    memo
                else
                    item :: memo
    in
    List.foldl predicate [] list
