module SqlParser exposing (..)

import Parser exposing (..)
import Set


type Token
    = Literal String
    | Placeholder String



-- parse : String -> Result (List DeadEnd) (List Token)


parse =
    run tokens


token : Parser Token
token =
    oneOf
        [ backtrackable placeholder
        , literal
        ]


tokens : Parser (List Token)
tokens =
    loop [] tokensHelper


literal : Parser Token
literal =
    chompWhile (\c -> c /= '@')
        |> getChompedString
        |> andThen (succeed << Literal)


placeholder : Parser Token
placeholder =
    succeed identity
        |. symbol "@"
        |= variable
            { start = \_ -> True
            , inner = \c -> Char.isAlphaNum c || c == '_' || c == '-'
            , reserved = Set.empty
            }
        |> andThen (succeed << Placeholder << (++) "@")


tokensHelper : List Token -> Parser (Step (List Token) (List Token))
tokensHelper ts =
    let
        checkToken tokensSoFar t =
            case t of
                Literal "" ->
                    Done (List.reverse tokensSoFar)

                _ ->
                    Loop (t :: tokensSoFar)
    in
    succeed (checkToken ts) |= token


localHelp : List String -> Parser (Step (List String) (List String))
localHelp nums =
    let
        checkNum numsSoFar num =
            if String.length num > 0 then
                Loop (num :: numsSoFar)

            else
                Done (List.reverse numsSoFar)
    in
    succeed (checkNum nums)
        |= (getChompedString <| chompWhile Char.isDigit)
        |. whitespace


whitespace : Parser ()
whitespace =
    chompWhile (\c -> c == ' ')
