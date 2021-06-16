module Main exposing (..)

--import Color exposing (..)

import Browser exposing (..)
import Browser.Events exposing (onMouseDown)
import Dict exposing (..)
import Editor.Pickers.ColorPicker exposing (colorPickerView)
import Editor.Pickers.LinkPicker exposing (linkPickerView)
import Editor.Trix as Trix exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Element.Lazy as Lazy
import Element.Region as Region
import Hex exposing (fromString)
import Html as Html
import Html.Attributes as HtmlAttr
import Html.Events as HtmlEvents exposing (on, onInput)
import Html.Parser exposing (..)
import Html.Parser.Util exposing (..)


type alias Flags =
    { currentTime : Int
    , width : Int
    , height : Int
    }


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { trixEditor : Trix.Model

    --, pickedColor : Color
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        trix =
            Trix.init [] Nothing
    in
    ( { trixEditor = trix }
    , Cmd.none
    )


subscriptions model =
    Sub.batch
        [ Trix.subscriptions TrixMsg model.trixEditor
        ]


type Msg
    = TrixMsg Trix.Msg
    | NoOp


update :
    Msg
    -> Model
    ->
        ( Model
        , Cmd Msg
        )
update msg model =
    case msg of
        TrixMsg trixMsg ->
            let
                ( trix, trixCmds, mbResult ) =
                    Trix.update { outMsg = TrixMsg } trixMsg model.trixEditor
            in
            ( { model | trixEditor = trix }, trixCmds )

        NoOp ->
            ( model, Cmd.none )


view : Model -> Html.Html Msg
view model =
    Element.layout [] <|
        column
            [ spacing 15
            , padding 15
            ]
            [ Trix.view
                { outMsg = TrixMsg
                , maxHeight = 400
                , containerWidth = 700
                , linkPicker = Just { widgetView = linkPickerView [ "/home", "Contact" ] }
                , colorPicker = Just { widgetView = colorPickerView }
                , docPicker = Nothing
                , imgPicker = Nothing
                , externalCss = globalStylesheet
                , placeholder = Nothing
                , uuid = ""
                }
                model.trixEditor

            --, Trix.render model.output
            ]


globalStylesheet =
    """
@charset "UTF-8";
.trix-content{
    font-family: arial;
    font-size: 16;
}
.trix-content h1 {
  font-size:22;
  padding:5px 15px 5px 15px;
  background-color:lightBlue;
  width:100%;
  text-align:center;
}

.trix-content h2 {
  font-size:20;
  padding:5px 15px 5px 15px;
  background-color:red;
  width:100%;
  text-align:center;
}

.trix-content h3 {
  font-size:18;
  color:green;
  font-weight:bold;
}
"""
