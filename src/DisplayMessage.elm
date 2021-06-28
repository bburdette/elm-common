module DisplayMessage exposing (..)

import Common exposing (buttonStyle)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import TangoColors as Color
import Util exposing (httpErrorString)


type alias Model =
    { message : String
    }


initialModel : String -> Model
initialModel message =
    { message = message }


type Msg
    = OkayThen


type Cmd
    = Okay


view : Model -> Element Msg
view model =
    column [ width fill, padding 10, spacing 8 ]
        [ paragraph [] [ text model.message ]
        , Input.button (buttonStyle ++ [])
            { onPress = Just OkayThen
            , label = text "okay"
            }
        ]


update : Msg -> Model -> ( Model, Cmd )
update msg model =
    case msg of
        OkayThen ->
            ( model, Okay )
