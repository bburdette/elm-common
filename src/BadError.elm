module BadError exposing (..)

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
    { errorMessage : String
    }


initialModel : String -> Model
initialModel errorMessage =
    { errorMessage = errorMessage }


type Msg
    = OkayThen


type Cmd
    = Okay


view : Model -> Element Msg
view model =
    column [ width fill ]
        [ el [] (text "uh oh, error!")
        , paragraph [] [ text model.errorMessage ]
        , Input.button (buttonStyle ++ [ width fill ])
            { onPress = Just OkayThen
            , label = text "okay"
            }
        ]


update : Msg -> Model -> ( Model, Cmd )
update msg model =
    case msg of
        OkayThen ->
            ( model, Okay )
