module Common exposing (accordion, buttonStyle, countString, dateElt, edges, lightOrange, lighterBlue, maxString, menuBlue, navChoice, navbar, navbarColor, npspaces, selectionColor, selectionColorDark, tagButtonStyle, tagLikeParagraph, tagParagraph, tagPill, workaroundMultiline)

import Array
import Char
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import Html.Attributes as HA
import Json.Encode as JE
import TangoColors as Color
import Time exposing (Posix, Zone)
import Util


workaroundMultiline :
    List (Attribute msg)
    ->
        { onChange : String -> msg
        , text : String
        , placeholder : Maybe (Input.Placeholder msg)
        , label : Input.Label msg
        , spellcheck : Bool
        }
    -> Element msg
workaroundMultiline attribs mlspec =
    Input.multiline (htmlAttribute (HA.property "value" (JE.string mlspec.text)) :: attribs)
        mlspec


{-| substitute chars that look like spaces but don't
turn into newlines in an Element.paragraph.
-}
npspaces : String -> String
npspaces str =
    String.map
        (\c ->
            if c == ' ' then
                Char.fromCode 160

            else
                c
        )
        str


maxString : String -> String
maxString s =
    if String.length s > 30 then
        String.left 27 s ++ "..."

    else
        s


navChoice : Color -> mode -> (mode -> msg) -> mode -> String -> Element msg
navChoice ccolor currentmode mmsg mode caption =
    let
        txt =
            npspaces caption
    in
    if currentmode == mode then
        row
            [ Font.bold
            , onClick (mmsg mode)
            , Background.color ccolor
            , paddingEach { edges | right = 3, left = 3, top = 2, bottom = 6 }
            ]
            [ text txt ]

    else
        row
            [ onClick (mmsg mode)
            , paddingEach { edges | bottom = 6 }
            ]
            [ text txt ]


menuBlue =
    { r = 52.0 / 255.0
    , g = 101.0 / 255.0
    , b = 164.0 / 255.0
    }


selectionColor =
    rgba 0 0 0 0.4


selectionColorDark =
    rgba 0 0 0 0.7


navbarColor : Int -> Color
navbarColor level =
    let
        cf =
            \num -> 1.0 - ((0.6 ^ toFloat level) * (1.0 - num))
    in
    rgb (cf menuBlue.r) (cf menuBlue.g) (cf menuBlue.b)


edges =
    { top = 0, right = 0, left = 0, bottom = 0 }


navbar : Int -> mode -> (mode -> msg) -> List ( mode, String ) -> Element msg
navbar level currentmode mmsg choices =
    let
        ccolor =
            navbarColor (level + 1)
    in
    paragraph
        [ Background.color (navbarColor level)
        , spacing 5
        , paddingEach { edges | top = 8, bottom = 0 }
        , width fill
        ]
        -- (text (String.fromInt level) ::
        (List.intersperse
            (text " ")
            (List.map
                (\( m, s ) -> navChoice ccolor currentmode mmsg m s)
                choices
            )
        )


dateElt : Zone -> Posix -> Element a
dateElt zone date =
    text <|
        List.foldr (++)
            ""
            [ String.fromInt <| Time.toYear zone date
            , "/"
            , String.fromInt <| Util.monthInt <| Time.toMonth zone date
            , "/"
            , String.fromInt <| Time.toDay zone date
            , " "
            , String.fromInt <| Time.toHour zone date
            , ":"
            , String.fromInt <| Time.toMinute zone date
            , ":"
            , String.fromInt <| Time.toSecond zone date
            ]


countString : Int -> String -> String -> String
countString count singular plural =
    if count == 1 then
        String.fromInt count ++ " " ++ singular

    else
        String.fromInt count ++ " " ++ plural


accordion : Bool -> Bool -> msg -> String -> Element msg -> Element msg
accordion bottomone show togglemsg caption content =
    column
        [ height shrink
        , Border.solid
        , if bottomone then
            Border.width 1

          else
            Border.widthEach { bottom = 0, top = 1, left = 1, right = 1 }
        , Border.color Color.darkGrey
        , paddingXY 3 3
        , width fill
        ]
        [ row [ Font.bold, onClick togglemsg, spacing 5 ]
            [ text caption
            , el [ alignRight ] <|
                text <|
                    if show then
                        "-"

                    else
                        "+"
            ]
        , if show then
            content

          else
            Element.none
        ]


tagButtonStyle =
    [ Background.color Color.blue
    , Font.color Color.white
    , Border.color Color.darkBlue
    , paddingXY 5 3
    , Border.rounded 5
    ]


tagPill : List (Attribute msg) -> String -> Element msg
tagPill attribs tagname =
    Input.button
        (attribs
            ++ [ Font.color Color.white
               , Border.color Color.darkBlue
               , paddingXY 5 3
               , Border.rounded 5

               -- , width (maximum 80 shrink)
               ]
        )
        { label = text <| npspaces (maxString tagname)
        , onPress = Nothing
        }


tagLikeParagraph : List ( String, Color ) -> Element msg
tagLikeParagraph tags =
    paragraph
        [ scrollbars
        , height (maximum 150 shrink)
        , width fill
        , spacingXY 3 17
        , paddingXY 0 10
        ]
        (List.intersperse (text " ")
            (List.map
                (\( name, color ) ->
                    tagPill
                        [ Background.color color ]
                        name
                )
                tags
            )
        )


buttonStyle =
    [ Background.color Color.blue
    , Font.color Color.white
    , Border.color Color.darkBlue
    , paddingXY 10 5
    , Border.rounded 3
    ]


lightOrange =
    rgb255 255 181 64


lighterBlue : Color
lighterBlue =
    rgb255 171 238 255
