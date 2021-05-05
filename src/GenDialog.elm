module GenDialog exposing (..)

import Common exposing (buttonStyle)
import Element as E exposing (Element)
import Element.Background as EBg
import Element.Border as EB
import Element.Events as EE
import Element.Input as EI
import Html exposing (Html)
import Html.Events as HE
import Http
import Json.Decode as JD
import Task
import Time exposing (Zone)
import Util



{-

   -- how to use:

   view : Model -> Element Msg
   view model =
       case model.dialog of
           Just dialog ->
               D.view dialog |> E.map DialogMsg

           Nothing ->
               normalview model

   -- or for a dialog centered relative to the window, not the underlying view,
   -- add it at the layout level.  TBD

-}


type Transition model return
    = Dialog model
    | Ok return
    | Cancel


type TextMsg
    = TmOk
    | TmCancel
    | TmNoop


type Msg msg
    = EltMsg msg
    | CancelClick
    | Noop


type alias Model model msg return =
    { view : model -> Element msg
    , update : msg -> model -> Transition model return
    , model : model
    , underLay : Element ()
    }


update : Msg msg -> Model model msg return -> Transition (Model model msg return) return
update msg model =
    case msg of
        EltMsg emsg ->
            case model.update emsg model.model of
                Dialog m ->
                    Dialog { model | model = m }

                Ok r ->
                    Ok r

                Cancel ->
                    Cancel

        CancelClick ->
            Cancel

        Noop ->
            Dialog model


view : Model model msg return -> Element (Msg msg)
view model =
    E.column
        [ E.height E.fill
        , E.width E.fill
        , E.inFront (overlay model)
        ]
        [ model.underLay
            |> E.map (\_ -> Noop)
        ]


layout : Model model msg return -> Html (Msg msg)
layout model =
    E.layout
        [ E.inFront (overlay model)

        -- , E.height E.fill
        -- , E.width E.fill
        ]
        (model.underLay
            |> E.map (\_ -> Noop)
        )


overlay : Model model msg return -> Element (Msg msg)
overlay model =
    E.column
        [ E.height E.fill
        , E.width E.fill
        , EBg.color <| E.rgba 0.5 0.5 0.5 0.5
        , E.inFront (dialogView model)
        , EE.onClick CancelClick
        ]
        []


dialogView : Model model msg return -> Element (Msg msg)
dialogView model =
    E.column
        [ EB.color <| E.rgb 0 0 0
        , E.centerX
        , E.centerY
        , EB.width 5
        , EBg.color <| E.rgb 1 1 1
        , E.paddingXY 10 10
        , E.spacing 5
        , E.htmlAttribute <|
            HE.custom "click"
                (JD.succeed
                    { message = Noop
                    , stopPropagation = True
                    , preventDefault = True
                    }
                )
        ]
        [ E.row [ E.centerX ] [ E.map EltMsg (model.view model.model) ]
        ]
