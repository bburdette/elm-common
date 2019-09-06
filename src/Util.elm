module Util exposing (Size, captchaQ, deadEndToString, deadEndsToString, first, foldUntil, httpErrorString, maxInt, mbl, mblist, minInt, monthInt, paramParser, paramsParser, problemToString, rest, rslist, trueforany)

import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import Http
import ParseHelp exposing (listOf)
import Parser as P exposing ((|.), (|=), Problem(..), symbol)
import Random exposing (Seed, int, step)
import TangoColors as Color
import Time


type alias Size =
    { width : Int
    , height : Int
    }


maxInt : Int
maxInt =
    9007199254740991


minInt : Int
minInt =
    -9007199254740991


paramParser : P.Parser ( String, String )
paramParser =
    P.succeed (\a b -> ( a, b ))
        |= P.getChompedString
            (P.chompWhile (\c -> c /= '='))
        |. P.symbol "="
        |= P.getChompedString
            (P.chompWhile (\c -> c /= '&'))


paramsParser : P.Parser (Dict String String)
paramsParser =
    P.succeed (\a b -> Dict.fromList <| a :: b)
        |= paramParser
        |= listOf
            (P.succeed identity
                |. symbol "&"
                |= paramParser
            )


httpErrorString : Http.Error -> String
httpErrorString e =
    case e of
        Http.BadUrl str ->
            "badurl" ++ str

        Http.Timeout ->
            "timeout"

        Http.NetworkError ->
            "networkerror"

        Http.BadStatus x ->
            "badstatus: " ++ String.fromInt x

        Http.BadBody s ->
            "badbodyd\nstring: " ++ s


rest : List a -> List a
rest list =
    case List.tail list of
        Nothing ->
            []

        Just elts ->
            elts


first : (a -> Maybe b) -> List a -> Maybe b
first f l =
    case List.head l of
        Just e ->
            case f e of
                Just x ->
                    Just x

                Nothing ->
                    first f (rest l)

        Nothing ->
            Nothing


trueforany : (a -> Bool) -> List a -> Bool
trueforany f l =
    case List.head l of
        Just e ->
            if f e then
                True

            else
                trueforany f (rest l)

        Nothing ->
            False


mblist : List (Maybe a) -> Maybe (List a)
mblist mbs =
    Maybe.map List.reverse <|
        List.foldl
            (\mba mblst ->
                case mblst of
                    Nothing ->
                        Nothing

                    Just lst ->
                        case mba of
                            Nothing ->
                                Nothing

                            Just a ->
                                Just <| a :: lst
            )
            (Just [])
            mbs


mbl : Maybe a -> List a
mbl mba =
    case mba of
        Just x ->
            [ x ]

        Nothing ->
            []


{-| de-result a list
-}
rslist : List (Result x a) -> Result x (List a)
rslist l =
    List.foldr
        (\rn rs ->
            rs
                |> Result.andThen (\ls -> Result.map (\n -> n :: ls) rn)
        )
        (Ok [])
        l


{-| a function passed to foldUntil must return this.
-}
type Stopoid b
    = Go b
    | Stop b


{-| keep folding until a condition is met, then stop.
-}
foldUntil : (a -> b -> Stopoid b) -> b -> List a -> b
foldUntil fn initb lst =
    case lst of
        [] ->
            initb

        fst :: rst ->
            case fn fst initb of
                Stop retb ->
                    retb

                Go updb ->
                    foldUntil fn updb rst


monthInt : Time.Month -> Int
monthInt month =
    case month of
        Time.Jan ->
            1

        Time.Feb ->
            2

        Time.Mar ->
            3

        Time.Apr ->
            4

        Time.May ->
            5

        Time.Jun ->
            6

        Time.Jul ->
            7

        Time.Aug ->
            8

        Time.Sep ->
            9

        Time.Oct ->
            10

        Time.Nov ->
            11

        Time.Dec ->
            12


captchaQ : Seed -> ( Seed, String, Int )
captchaQ seed =
    let
        ( a, seed1 ) =
            step (int 0 100) seed

        ( b, seed2 ) =
            step (int 0 100) seed1
    in
    ( seed2
    , "Whats " ++ String.fromInt a ++ " + " ++ String.fromInt b ++ "?"
    , a + b
    )


deadEndsToString : List P.DeadEnd -> String
deadEndsToString deadEnds =
    String.concat (List.intersperse "; " (List.map deadEndToString deadEnds))


deadEndToString : P.DeadEnd -> String
deadEndToString deadend =
    problemToString deadend.problem ++ " at row " ++ String.fromInt deadend.row ++ ", col " ++ String.fromInt deadend.col


problemToString : P.Problem -> String
problemToString p =
    case p of
        Expecting s ->
            "expecting '" ++ s ++ "'"

        ExpectingInt ->
            "expecting int"

        ExpectingHex ->
            "expecting octal"

        ExpectingOctal ->
            "expecting octal"

        ExpectingBinary ->
            "expecting binary"

        ExpectingFloat ->
            "expecting float"

        ExpectingNumber ->
            "expecting number"

        ExpectingVariable ->
            "expecting variable"

        ExpectingSymbol s ->
            "expecting symbol '" ++ s ++ "'"

        ExpectingKeyword s ->
            "expecting keyword '" ++ s ++ "'"

        ExpectingEnd ->
            "expecting end"

        UnexpectedChar ->
            "unexpected char"

        Problem s ->
            "problem " ++ s

        BadRepeat ->
            "bad repeat"
