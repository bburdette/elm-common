module Util exposing
    ( Size
    , captchaQ
    , deadEndsToString
    , first
    , httpErrorString
    , maxInt
    , mbl
    , mblist
    , minInt
    , monthInt
    , rest
    , rslist
    , trueforany
    )

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import Http
import Parser exposing (Problem(..))
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
            "badstatus: " ++ String.fromInt x.status.code ++ ", " ++ x.status.message

        Http.BadPayload s r ->
            "badpayload\nstring: " ++ s ++ "\nresponse body: " ++ r.body


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


mbl : Maybe a -> List a
mbl mba =
    case mba of
        Just x ->
            [ x ]

        Nothing ->
            []


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


rslist : List (Result a b) -> Result a (List b)
rslist rslts =
    Result.map List.reverse <|
        List.foldl
            (\mba rslst ->
                case rslst of
                    Err e ->
                        Err e

                    Ok lst ->
                        case mba of
                            Err e ->
                                Err e

                            Ok a ->
                                Ok <| a :: lst
            )
            (Ok [])
            rslts


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


deadEndsToString : List Parser.DeadEnd -> String
deadEndsToString deadEnds =
    String.concat (List.intersperse "; " (List.map deadEndToString deadEnds))


deadEndToString : Parser.DeadEnd -> String
deadEndToString deadend =
    problemToString deadend.problem ++ " at row " ++ String.fromInt deadend.row ++ ", col " ++ String.fromInt deadend.col


problemToString : Parser.Problem -> String
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
            "expecting number"

        ExpectingNumber ->
            "expecting variable"

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
