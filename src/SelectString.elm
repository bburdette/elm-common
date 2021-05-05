module SelectString exposing (GDModel, Model, Msg(..), init, update, view)

-- import BisCommon as BC

import Array as A exposing (Array)
import Element as E exposing (Element)
import Element.Background as EBk
import Element.Border as EBd
import Element.Events as EE
import Element.Font as EF
import Element.Input as EI
import Element.Region
import GenDialog as GD
import TangoColors as TC
import Time exposing (Zone)
import Util


type alias GDModel =
    GD.Model Palette Model Msg Int


type alias Model =
    { choices : Array String
    , selected : Maybe Int
    , search : String
    }


type alias Palette =
    { buttonStyle : List (E.Attribute ()) }


type Msg
    = RowClick Int
    | OkClick
    | CancelClick
    | SearchChanged String
    | Noop


selectedrow : List (E.Attribute Msg)
selectedrow =
    [ EBk.color TC.lightBlue ]


view : Palette -> Model -> Element Msg
view palette model =
    let
        ls =
            String.toLower model.search

        buttonStyle =
            List.map (E.mapAttribute (\_ -> Noop)) palette.buttonStyle
    in
    E.column [ E.width <| E.px 500, E.height <| E.px 500, E.spacing 10 ]
        [ EI.text []
            { onChange = SearchChanged
            , text = model.search
            , placeholder = Nothing
            , label = EI.labelLeft [] <| E.text "search"
            }
        , E.column [ E.width E.fill, E.height <| E.px 400, E.scrollbarY, E.spacing 2 ] <|
            (A.indexedMap
                (\i s ->
                    if String.contains ls (String.toLower s) then
                        let
                            style =
                                if Just i == model.selected then
                                    selectedrow

                                else
                                    []
                        in
                        E.row ((EE.onClick <| RowClick i) :: E.height (E.px 30) :: E.width E.fill :: style) [ E.text s ]

                    else
                        E.none
                )
                model.choices
                |> A.toList
            )
        , E.row [ E.width E.fill, E.spacing 10 ]
            [ EI.button buttonStyle
                { onPress = Just OkClick, label = E.text "Ok" }
            , EI.button
                buttonStyle
                { onPress = Just CancelClick, label = E.text "Cancel" }
            ]
        ]


update : Msg -> Model -> GD.Transition Model Int
update msg model =
    case msg of
        RowClick i ->
            GD.Dialog { model | selected = Just i }

        SearchChanged s ->
            GD.Dialog { model | search = s }

        CancelClick ->
            GD.Cancel

        OkClick ->
            model.selected
                |> Maybe.map GD.Ok
                |> Maybe.withDefault GD.Cancel

        Noop ->
            GD.Dialog model


init : Model -> (Palette -> Element ()) -> GDModel
init model underLay =
    { view = view
    , update = update
    , model = model
    , underLay = underLay
    }
