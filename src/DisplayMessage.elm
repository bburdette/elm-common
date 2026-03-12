module DisplayMessage exposing (..)

import Common exposing (buttonStyle)
import Element exposing (..)
import Element.Input as Input
import GenDialog as GD
import Util


type alias GDModel =
    GD.Model Model Msg ()


type alias Model =
    { message : String
    }


init : List (Attribute Msg) -> String -> Element () -> GDModel
init buttonStyle message underLay =
    { view = view buttonStyle
    , update = update
    , model = { message = message }
    , underLay = underLay
    }


type Msg
    = OkayThen


type Cmd
    = Okay


view : List (Attribute Msg) -> Maybe Util.Size -> Model -> Element Msg
view buttonStyle mbsize model =
    column
        [ width (mbsize |> Maybe.map .width |> Maybe.withDefault 500 |> px)
        , height shrink
        , spacing 10
        ]
        [ row [ centerX ] [ paragraph [] [ text model.message ] ]
        , Input.button (buttonStyle ++ [ centerX ])
            { onPress = Just OkayThen
            , label = text "okay"
            }
        ]


update : Msg -> Model -> GD.Transition Model ()
update msg model =
    case msg of
        OkayThen ->
            GD.Ok ()
