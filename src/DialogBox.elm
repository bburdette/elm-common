module DialogBox exposing (Model, Result(..), view)

import Common exposing (buttonStyle, lightOrange, tagPill)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import TangoColors as Color


type Result
    = Custom String
    | Cancel


type alias Model =
    { message : String
    , results : List Result
    }


view : Model -> Element Result
view model =
    Element.el
        [ Background.color Color.white
        , Border.color Color.red
        , Border.width 3
        , width shrink
        , height shrink
        , centerX
        , centerY
        ]
    <|
        Element.column [ width shrink ]
            [ Element.el
                [ Font.bold ]
              <|
                text model.message
            , Element.row [ spacing 3 ]
                (List.map
                    (\rs ->
                        case rs of
                            Custom txt ->
                                Input.button buttonStyle
                                    { onPress =
                                        Just <| rs
                                    , label = text txt
                                    }

                            Cancel ->
                                Input.button buttonStyle
                                    { onPress =
                                        Just <| Cancel
                                    , label = text "Cancel"
                                    }
                    )
                    model.results
                )
            ]
