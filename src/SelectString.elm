module SelectString exposing (GDModel, Model, Msg(..), init, update, view)

import BisCommon as BC
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


type alias Model a =
    { choices : List ( a, String )
    , selected : Maybe a
    , search : String
    }


type Msg a
    = RowClick a
    | OkClick
    | CancelClick
    | SearchChanged String


type alias GDModel a =
    GD.Model (Model a) (Msg a) a


selectedrow : List (E.Attribute (Msg a))
selectedrow =
    [ EBk.color TC.lightBlue ]


view : Maybe Util.Size -> Model a -> Element (Msg a)
view mbsize model =
    let
        ls =
            String.toLower model.search
    in
    E.column
        [ E.width (mbsize |> Maybe.map .width |> Maybe.withDefault 500 |> E.px)
        , E.height (mbsize |> Maybe.map .height |> Maybe.withDefault 500 |> E.px)
        , E.spacing 10
        ]
        [ EI.text []
            { onChange = SearchChanged
            , text = model.search
            , placeholder = Nothing
            , label = EI.labelLeft [] <| E.text "search"
            }
        , E.column [ E.width E.fill, E.height <| E.px 400, E.scrollbarY, E.spacing 2 ] <|
            List.map
                (\( i, s ) ->
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
        , E.row [ E.width E.fill, E.spacing 10 ]
            [ EI.button BC.buttonStyle
                { onPress = Just OkClick, label = E.text "Ok" }
            , EI.button
                BC.buttonStyle
                { onPress = Just CancelClick, label = E.text "Cancel" }
            ]
        ]


update : Msg a -> Model a -> GD.Transition (Model a) a
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


init : Model a -> Element () -> GDModel a
init model underLay =
    { view = view
    , update = update
    , model = model
    , underLay = underLay
    }
