port module Editor.Trix exposing (EditorResult(..), GlobalAttribute(..), Model, Msg, TrixDocument, currentDoc, init, loadTrixDocument, render, serializeTrixDocument, subscriptions, update, view)

--import Color exposing (..)

import Browser exposing (..)
import Browser.Events exposing (onMouseDown)
import Dict exposing (..)
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
import Html.Keyed exposing (node)
import Html.Parser exposing (..)
import Html.Parser.Util exposing (..)
import Json.Decode as D
import Json.Encode as E
import Json.Value exposing (decodeValue)
import Serialize as C exposing (..)
import Svg exposing (Svg, svg)
import Svg.Attributes exposing (cx, cy, d, points, r, rx, ry, x, x1, x2, y, y1, y2)


port activateAttribute : E.Value -> Cmd msg


port deactivateAttributes : E.Value -> Cmd msg


port insertHtml : E.Value -> Cmd msg


port loadHtml : E.Value -> Cmd msg


port getSelection : () -> Cmd msg


port selection : (E.Value -> msg) -> Sub msg


port trixReady : (E.Value -> msg) -> Sub msg


type alias Flags =
    { currentTime : Int
    , width : Int
    , height : Int
    }


type alias Model =
    { htmlContent : HtmlContent
    , selection : Maybe Selection
    , htmlOutput : HtmlOutput
    , globalAttributes : List GlobalAttribute
    , openedWidget : Maybe Widget
    }


init :
    List GlobalAttribute
    -> Maybe HtmlOutput
    -> Model
init attrs mbInput =
    { htmlContent = HtmlContent "" ""
    , selection = Nothing
    , htmlOutput = Maybe.withDefault "" mbInput
    , globalAttributes = attrs
    , openedWidget = Nothing
    }


type GlobalAttribute
    = AlignRight
    | AlignLeft
    | BackgroundColor PickedColor
    | Border
    | Font String
    | FontColor PickedColor
    | FontSize Int
    | FontAlignLeft
    | FontAlignRight
    | Center
    | Justify
    | Bold
    | Italic
    | Other String String


type alias TrixDocument =
    { globalAttributes : List GlobalAttribute
    , htmlOutput : HtmlOutput
    }


loadTrixDocument : String -> TrixDocument
loadTrixDocument s =
    case C.decodeFromString trixDocumentCodec s of
        Ok td ->
            td

        Err e ->
            --let
            --    t =
            --        Debug.log "" e
            --in
            { globalAttributes = []
            , htmlOutput = s
            }


currentDoc : Model -> String
currentDoc model =
    if (model.globalAttributes == []) && (model.htmlOutput == "") then
        ""

    else
        serializeTrixDocument { globalAttributes = model.globalAttributes, htmlOutput = model.htmlOutput }


serializeTrixDocument : TrixDocument -> String
serializeTrixDocument td =
    C.encodeToString trixDocumentCodec td


trixDocumentCodec : C.Codec e TrixDocument
trixDocumentCodec =
    C.record TrixDocument
        |> C.field .globalAttributes (C.list globalAttributeCodec)
        |> C.field .htmlOutput C.string
        |> C.finishRecord


globalAttributeCodec : C.Codec e GlobalAttribute
globalAttributeCodec =
    C.customType
        (\fAlignRight fAlignLeft fBackgroundColor fBorder fFont fFontColor fFontSize fFontAlignLeft fFontAlignRight fCenter fJustify fBold fItalic fOther value ->
            case value of
                AlignRight ->
                    fAlignRight

                AlignLeft ->
                    fAlignLeft

                BackgroundColor c ->
                    fBackgroundColor c

                Border ->
                    fBorder

                Font s ->
                    fFont s

                FontColor c ->
                    fFontColor c

                FontSize n ->
                    fFontSize n

                FontAlignLeft ->
                    fFontAlignLeft

                FontAlignRight ->
                    fFontAlignRight

                Center ->
                    fCenter

                Justify ->
                    fJustify

                Bold ->
                    fBold

                Italic ->
                    fItalic

                Other attr val ->
                    fOther attr val
        )
        |> C.variant0 AlignRight
        |> C.variant0 AlignLeft
        |> C.variant1 BackgroundColor colorCodec
        |> C.variant0 Border
        |> C.variant1 Font C.string
        |> C.variant1 FontColor colorCodec
        |> C.variant1 FontSize C.int
        |> C.variant0 FontAlignLeft
        |> C.variant0 FontAlignRight
        |> C.variant0 Center
        |> C.variant0 Justify
        |> C.variant0 Bold
        |> C.variant0 Italic
        |> C.variant2 Other C.string C.string
        |> C.finishCustomType


type alias PickedColor =
    { r : Int, g : Int, b : Int, a : Float }


colorCodec : Codec e PickedColor
colorCodec =
    C.record PickedColor
        |> C.field .r C.int
        |> C.field .g C.int
        |> C.field .b C.int
        |> C.field .a C.float
        |> C.finishRecord


type alias HtmlOutput =
    String


type alias HtmlContent =
    { html : HtmlOutput
    , text : String
    }


type alias Selection =
    { start : Int
    , end : Int
    , attrs : Dict String String
    , text : String
    }


type Widget
    = FontColorPicker
    | BackgroundColorPicker
    | InternalLinks
    | ImagePicker
    | DocPicker


subscriptions outMsg model =
    Sub.batch
        [ case model.openedWidget of
            Just FontColorPicker ->
                Browser.Events.onMouseDown (outsideTargetHandler "fontColorPicker" Close)

            Just BackgroundColorPicker ->
                Browser.Events.onMouseDown (outsideTargetHandler "backgroundColorPicker" Close)

            Just InternalLinks ->
                Browser.Events.onMouseDown (outsideTargetHandler "internalLinkPicker" Close)

            Just DocPicker ->
                Browser.Events.onMouseDown (outsideTargetHandler "docPicker" Close)

            Just ImagePicker ->
                Browser.Events.onMouseDown (outsideTargetHandler "imgPicker" Close)

            Nothing ->
                Sub.none
        , selection GotSelection
        , trixReady (always LoadContentInTrix)
        ]
        |> Sub.map outMsg


type Msg
    = GetHtmlContent HtmlContent
    | GetSelection
    | GotSelection E.Value
    | LoadContentInTrix
    | OpenWidget Widget
    | Close
    | InsertInternalLink String
    | InsertDocLink String
    | InsertImage
        { url : String
        , caption : Maybe String
        , width : Int
        , height : Int
        , alignment : String
        }
    | SetTextColor Bool PickedColor
    | SetBackgroundColor Bool PickedColor
    | SetFont Bool String
    | SetFontSize Bool Int
    | SetAlignMent GlobalAttribute
    | UndoStyle
    | SaveAndQuit
    | Quit
    | NoOp


type EditorResult a
    = EditorQuit
    | EditorData a


update :
    { outMsg : Msg -> msg }
    -> Msg
    -> Model
    ->
        ( Model
        , Cmd msg
        , Maybe (EditorResult ( HtmlOutput, List GlobalAttribute ))
        )
update config msg model =
    case msg of
        GetHtmlContent content ->
            let
                output =
                    content.html
            in
            ( { model
                | htmlContent = content
                , htmlOutput = output
              }
            , getSelection ()
            , Nothing
            )

        GetSelection ->
            ( model
            , getSelection ()
            , Nothing
            )

        GotSelection value ->
            case D.decodeValue decodeSelection value of
                Ok ( { text } as sel, ids ) ->
                    ( { model
                        | selection = Just { sel | text = String.dropRight 1 text }
                      }
                    , Cmd.none
                    , Nothing
                    )

                Err _ ->
                    ( model
                    , Cmd.none
                    , Nothing
                    )

        LoadContentInTrix ->
            --let
            --    removeHighlight node =
            --        case node of
            --            Element tag attrs nodes ->
            --                Element
            --                    tag
            --                    (List.foldr
            --                        (\( attr, value ) acc ->
            --                            (if attr == "style" then
            --                                ( attr, String.replace "background-color: highlight;" "" value )
            --                             else
            --                                ( attr, value )
            --                            )
            --                                :: acc
            --                        )
            --                        []
            --                        attrs
            --                    )
            --                    (List.map removeHighlight nodes)
            --            other ->
            --                other
            --in
            ( model
            , model.htmlOutput
                --|> List.concatMap (textBlockElementToNode (PageTreeEditor.pageIndex config.pageTreeEditor))
                ----needed to fix external link widged leaving content highlighted when swaping internal tabs
                --|> List.map removeHighlight
                --|> List.map Html.Parser.nodeToString
                --|> String.join ""
                |> (\html ->
                        E.object
                            [ ( "tagName", E.string "initial load" )
                            , ( "html", E.string html )
                            ]
                   )
                |> loadHtml
            , Nothing
            )

        OpenWidget widget ->
            ( { model
                | openedWidget =
                    if model.openedWidget == Just widget then
                        Nothing

                    else
                        Just widget
              }
            , Cmd.none
            , Nothing
            )

        Close ->
            ( { model | openedWidget = Nothing }, Cmd.none, Nothing )

        InsertInternalLink url ->
            case model.selection of
                Just { start, end, attrs, text } ->
                    if start /= end then
                        let
                            selected =
                                text

                            link =
                                "<a href=" ++ url ++ ">" ++ selected ++ "</>"

                            data =
                                E.object
                                    [ ( "selectionStart", E.int start )
                                    , ( "selectionEnd", E.int end )
                                    , ( "tagName", E.string "internal link" )
                                    , ( "html", E.string link )
                                    ]
                        in
                        ( { model | openedWidget = Nothing }
                        , insertHtml data
                        , Nothing
                        )

                    else
                        ( model, Cmd.none, Nothing )

                Nothing ->
                    ( model, Cmd.none, Nothing )

        InsertDocLink url ->
            ( model, Cmd.none, Nothing )

        InsertImage { url, caption, width, height, alignment } ->
            ( model, Cmd.none, Nothing )

        SetTextColor isGlobal color ->
            if isGlobal then
                ( { model
                    | globalAttributes =
                        updateAttrs isFontColorAttr FontColor color model.globalAttributes
                    , openedWidget = Nothing
                  }
                , Cmd.batch
                    []
                , Nothing
                )

            else
                case model.selection of
                    Just { start, end, attrs } ->
                        let
                            data =
                                E.object
                                    [ ( "selectionStart", E.int start )
                                    , ( "selectionEnd", E.int end )
                                    , ( "attribute", E.string "foregroundColor" )
                                    , ( "value", E.string (pickedColorToStr color) )
                                    ]
                        in
                        ( { model | openedWidget = Nothing }
                        , activateAttribute data
                        , Nothing
                        )

                    _ ->
                        ( model, Cmd.none, Nothing )

        SetBackgroundColor isGlobal color ->
            if isGlobal then
                ( { model
                    | globalAttributes =
                        updateAttrs isBackgroundColorAttr BackgroundColor color model.globalAttributes
                    , openedWidget = Nothing
                  }
                , Cmd.batch
                    []
                , Nothing
                )

            else
                case model.selection of
                    Just { start, end, attrs } ->
                        let
                            data =
                                E.object
                                    [ ( "selectionStart", E.int start )
                                    , ( "selectionEnd", E.int end )
                                    , ( "attribute", E.string "backgroundColor" )
                                    , ( "value", E.string (pickedColorToStr color) )
                                    ]
                        in
                        ( { model | openedWidget = Nothing }
                        , activateAttribute data
                        , Nothing
                        )

                    _ ->
                        ( model, Cmd.none, Nothing )

        SetFont isGlobal font ->
            if isGlobal then
                ( { model
                    | globalAttributes =
                        updateAttrs isFontAttr Font font model.globalAttributes
                  }
                , Cmd.batch
                    []
                , Nothing
                )

            else
                case model.selection of
                    Just { start, end, attrs } ->
                        let
                            data =
                                E.object
                                    [ ( "selectionStart", E.int start )
                                    , ( "selectionEnd", E.int end )
                                    , ( "attribute", E.string "textFont" )
                                    , ( "value", E.string font )
                                    ]
                        in
                        ( { model | openedWidget = Nothing }
                        , activateAttribute data
                        , Nothing
                        )

                    _ ->
                        ( model, Cmd.none, Nothing )

        SetFontSize isGlobal n ->
            if isGlobal then
                ( { model
                    | globalAttributes =
                        updateAttrs isFontSizeAttr FontSize n model.globalAttributes
                  }
                , Cmd.batch
                    []
                , Nothing
                )

            else
                case model.selection of
                    Just { start, end, attrs } ->
                        let
                            data =
                                E.object
                                    [ ( "selectionStart", E.int start )
                                    , ( "selectionEnd", E.int end )
                                    , ( "attribute", E.string "fontSize" )
                                    , ( "value", E.string <| String.fromInt n ++ "px" )
                                    ]
                        in
                        ( { model | openedWidget = Nothing }
                        , activateAttribute data
                        , Nothing
                        )

                    _ ->
                        ( model, Cmd.none, Nothing )

        SetAlignMent a ->
            ( { model | globalAttributes = setAlignMent model.globalAttributes a }
            , Cmd.none
            , Nothing
            )

        UndoStyle ->
            case model.selection of
                Just { start, end, attrs } ->
                    let
                        data =
                            E.object
                                [ ( "selectionStart", E.int start )
                                , ( "selectionEnd", E.int end )
                                , ( "attributes"
                                  , Dict.keys attrs
                                        |> E.list E.string
                                  )
                                ]
                    in
                    ( { model | openedWidget = Nothing }
                    , deactivateAttributes data
                    , Nothing
                    )

                _ ->
                    ( model
                    , Cmd.none
                    , Nothing
                    )

        SaveAndQuit ->
            ( model
            , Cmd.none
            , Just <|
                EditorData
                    ( model.htmlOutput
                    , model.globalAttributes
                    )
            )

        Quit ->
            ( model, Cmd.none, Just EditorQuit )

        NoOp ->
            ( model, Cmd.none, Nothing )


updateAttrs :
    (GlobalAttribute -> Bool)
    -> (a -> GlobalAttribute)
    -> a
    -> List GlobalAttribute
    -> List GlobalAttribute
updateAttrs p attrWrapper val attrs =
    let
        helper acc xs =
            case xs of
                [] ->
                    List.reverse (attrWrapper val :: acc)

                x :: xs_ ->
                    if attrWrapper val == x then
                        List.reverse acc ++ xs_

                    else if p x then
                        List.reverse (attrWrapper val :: acc) ++ xs_

                    else
                        helper (x :: acc) xs_
    in
    helper [] attrs


isFontAttr : GlobalAttribute -> Bool
isFontAttr a =
    case a of
        Font _ ->
            True

        _ ->
            False


isFontColorAttr : GlobalAttribute -> Bool
isFontColorAttr a =
    case a of
        FontColor _ ->
            True

        _ ->
            False


isBackgroundColorAttr : GlobalAttribute -> Bool
isBackgroundColorAttr a =
    case a of
        BackgroundColor _ ->
            True

        _ ->
            False


isFontSizeAttr : GlobalAttribute -> Bool
isFontSizeAttr a =
    case a of
        FontSize _ ->
            True

        _ ->
            False


setAlignMent : List GlobalAttribute -> GlobalAttribute -> List GlobalAttribute
setAlignMent attrs alignment =
    let
        alignments =
            [ FontAlignRight, FontAlignLeft, Center, Justify ]

        attrs_ =
            List.filter (\attr -> not <| List.member attr alignments) attrs
    in
    alignment :: attrs_



-------------------------------------------------------------------------------
--------------------
-- View functions --
--------------------


render : { pageIndex : Dict String String, externalCss : String } -> HtmlOutput -> List GlobalAttribute -> Element msg
render config output globalAttributes =
    Html.Parser.run output
        |> Result.withDefault []
        --|> List.map (processLinks config)
        |> List.map (toHtml config 0)
        |> Html.div
            [ HtmlAttr.class "trix-content"

            --, Attr.style "width" "100%"
            , HtmlAttr.attribute "style"
                ("width: 100%;"
                    ++ stringifyAttributes (List.concatMap globalAttrToCss globalAttributes)
                )
            ]
        |> (\r ->
                paragraph
                    [ width fill
                    ]
                    [ embeddedStyleSheet config.externalCss globalAttributes
                    , Element.html <| r
                    ]
           )



--processLinks config node =
--    let
--        processLinkAttrs processed toProcess =
--            case toProcess of
--                ( "href", url ) :: xs ->
--                    if config.editMode then
--                        List.reverse processed ++ xs
--                    else if String.startsWith "lien-interne:" url then
--                        ( "href"
--                        , String.dropLeft (String.length "lien-interne:") url
--                            |> (\cId -> Dict.get cId config.pageIndex)
--                            |> Maybe.withDefault url
--                        )
--                            :: (List.reverse processed ++ xs)
--                    else if String.startsWith "doc:" url then
--                        ( "href"
--                        , String.dropLeft (String.length "doc:") url
--                            |> percentDecode
--                            |> Maybe.withDefault ""
--                        )
--                            :: ( "target", "_blank" )
--                            :: (List.reverse processed ++ xs)
--                    else if Dict.member url config.pageIndex then
--                        ( "href", Dict.get url config.pageIndex |> Maybe.withDefault "" )
--                            :: (List.reverse processed ++ xs)
--                    else
--                        ( "href", url )
--                            :: ( "target", "_blank" )
--                            :: (List.reverse processed ++ xs)
--                other :: xs ->
--                    processLinkAttrs (other :: processed) xs
--                [] ->
--                    processed
--    in
--    case node of
--        Element "a" attrs nodes ->
--            Element "a" (processLinkAttrs [] attrs) (List.map (processLinks config) nodes)
--        Element tag attrs nodes ->
--            Element tag attrs (List.map (processLinks config) nodes)
--        Html.Parser.Text value ->
--            Html.Parser.Text value
--        Comment value ->
--            Comment value


toHtml config level node =
    case node of
        Element tag attrs nodes ->
            let
                --templateTagAttr =
                --    Dict.get tag (defaultStyleSheetCss config.defaultSyleS)
                --        |> Maybe.withDefault []
                --        |> List.map (\( a, v ) -> a ++ ":" ++ v ++ ";")
                --        |> String.join " "
                --        |> (\s -> HtmlAttr.attribute "style" s)
                attrs_ =
                    --templateTagAttr
                    --    ::
                    List.map (\( a, v ) -> HtmlAttr.attribute a v)
                        attrs
            in
            Html.node tag attrs_ (List.map (toHtml config level) nodes)

        Html.Parser.Text value ->
            Html.text value

        Comment value ->
            Html.span [] []


type alias EditorViewConfig msg =
    { containerWidth : Int
    , maxHeight : Int
    , outMsg : Msg -> msg
    , linkPicker :
        Maybe
            { widgetView :
                { handler : String -> msg
                , currentLink : Maybe String
                }
                -> Element msg
            }
    , colorPicker :
        Maybe
            { widgetView :
                { handler : String -> msg
                , attribute : String
                , currentColor : Maybe String
                }
                -> Element msg
            }
    , docPicker :
        Maybe
            { widgetView :
                { handler : String -> msg
                , currentDoc : Maybe String
                }
                -> Element msg
            }
    , imgPicker :
        Maybe
            { widgetView :
                { handler :
                    { url : String
                    , caption : Maybe String
                    , width : Int
                    , height : Int
                    , alignment : String
                    }
                    -> msg
                , currentImg :
                    Maybe
                        { url : String
                        , caption : Maybe String
                        , width : Int
                        , height : Int
                        , alignment : String
                        }
                }
                -> Element msg
            }
    , externalCss : String
    , placeholder : Maybe String
    , uuid : String
    }


view :
    EditorViewConfig msg
    -> Model
    -> Element msg
view config model =
    trixEditor config model


trixEditor :
    EditorViewConfig msg
    -> Model
    -> Element.Element msg
trixEditor config model =
    column
        [ spacing 10 ]
        [ embeddedStyleSheet config.externalCss model.globalAttributes
        , customToolbar config model
        , Element.map config.outMsg <|
            paragraph
                [ width (px config.containerWidth)
                ]
                [ Keyed.el []
                    ( config.uuid
                    , html <|
                        Html.Keyed.node "div"
                            []
                            [ ( ""
                              , Html.node
                                    "trix-toolbar"
                                    [ HtmlAttr.id "trix-toolbar"
                                    ]
                                    []
                              )
                            , ( ""
                              , Html.node "input"
                                    [ HtmlAttr.type_ "hidden"
                                    , HtmlAttr.id "reset"
                                    , HtmlAttr.value ""
                                    ]
                                    []
                              )
                            , ( ""
                              , Html.node "trix-editor"
                                    [ on "trix-change" (D.map GetHtmlContent decodeEditorMarkup)
                                    , on "trix-selection-change" (D.map (always GetSelection) (D.succeed ()))
                                    , HtmlAttr.class "trix-content"
                                    , HtmlAttr.class "trix-content-editor"
                                    , HtmlAttr.attribute "toolbar" "trix-toolbar"
                                    , HtmlAttr.attribute "input" "reset"
                                    , HtmlAttr.attribute "placeholder" (Maybe.withDefault "" config.placeholder)
                                    , HtmlAttr.style "maxHeight" (String.fromInt (config.maxHeight - 155) ++ "px")
                                    , HtmlAttr.style "overflow-y" "auto"
                                    ]
                                    []
                              )
                            ]
                    )
                ]
        ]


iconSize =
    18


customToolbar :
    EditorViewConfig msg
    -> Model
    -> Element msg
customToolbar config model =
    let
        selectionCollapsed =
            case model.selection of
                Nothing ->
                    Nothing

                Just { start, end } ->
                    Just (start == end)

        canUpdateGlobalAttr =
            case model.selection of
                Nothing ->
                    True

                Just { start, end } ->
                    start == end

        selectionAttrs =
            Maybe.map .attrs model.selection

        globalAttributesCssDict =
            List.concatMap globalAttrToCss model.globalAttributes
                |> Dict.fromList

        fontColor =
            case
                Maybe.andThen (Dict.get "foregroundColor") selectionAttrs
                    |> Maybe.map parseColor
                    |> Maybe.map pickedColorToStr
            of
                Just c ->
                    Just c

                Nothing ->
                    case
                        Dict.get "color" globalAttributesCssDict
                            |> Maybe.map parseColor
                            |> Maybe.map pickedColorToStr
                    of
                        Just c ->
                            Just c

                        Nothing ->
                            Just "rgb(0,0,0)"

        backgroundColor =
            case
                Maybe.andThen (Dict.get "backgroundColor") selectionAttrs
                    |> Maybe.map parseColor
                    |> Maybe.map pickedColorToStr
            of
                Just c ->
                    Just c

                Nothing ->
                    case
                        Dict.get "background-color" globalAttributesCssDict
                            |> Maybe.map parseColor
                            |> Maybe.map pickedColorToStr
                    of
                        Just c ->
                            Just c

                        Nothing ->
                            Just "rgb(255,255,255)"

        textFont =
            Maybe.andThen (Dict.get "textFont") selectionAttrs
                |> Maybe.withDefault "Arial"

        fontSize =
            Maybe.andThen (Dict.get "fontSize") selectionAttrs
                |> Maybe.map (String.replace "px" "")
                |> Maybe.andThen String.toInt
                |> Maybe.withDefault 16

        href =
            Maybe.andThen (Dict.get "href") selectionAttrs

        fontOptionView selectedFont f =
            Html.option
                [ HtmlAttr.value f
                , HtmlAttr.selected (selectedFont == f)
                ]
                [ Html.text f ]

        fontSizeOptionView selectedSize fs =
            let
                selected =
                    String.toInt fs
                        |> Maybe.map (\fs_ -> selectedSize == fs_)
                        |> Maybe.withDefault False
            in
            Html.option
                [ HtmlAttr.value fs
                , HtmlAttr.selected selected
                ]
                [ Html.text fs ]
    in
    row
        [ spacing 10
        , width fill
        ]
        [ case config.linkPicker of
            Nothing ->
                Element.none

            Just { widgetView } ->
                widgetViewWrapper
                    { id = "internalLinkPicker"
                    , isActive = selectionCollapsed == Just False
                    , isOpen = model.openedWidget == Just InternalLinks
                    , openMsg = config.outMsg <| OpenWidget InternalLinks
                    , label = el [] (html <| link2 iconSize)
                    , widgetView = widgetView
                    , widgetConfig =
                        { handler = config.outMsg << InsertInternalLink
                        , currentLink = href
                        }
                    }
        , case config.docPicker of
            Nothing ->
                Element.none

            Just { widgetView } ->
                widgetViewWrapper
                    { id = "docPicker"
                    , isActive = selectionCollapsed == Just False
                    , isOpen = model.openedWidget == Just DocPicker
                    , openMsg = config.outMsg <| OpenWidget DocPicker
                    , label = el [] (html <| fileText iconSize)
                    , widgetView = widgetView
                    , widgetConfig =
                        { handler = config.outMsg << InsertDocLink
                        , currentDoc = href
                        }
                    }
        , case config.imgPicker of
            Nothing ->
                Element.none

            Just { widgetView } ->
                widgetViewWrapper
                    { id = "imgPicker"
                    , isActive = selectionCollapsed == Just True
                    , isOpen = model.openedWidget == Just ImagePicker
                    , openMsg = config.outMsg <| OpenWidget ImagePicker
                    , label = el [] (html <| imageIcon iconSize)
                    , widgetView = widgetView
                    , widgetConfig =
                        { handler = config.outMsg << InsertImage
                        , currentImg = Nothing
                        }
                    }
        , case config.colorPicker of
            Nothing ->
                Element.none

            Just { widgetView } ->
                widgetViewWrapper
                    { id = "fontColorPicker"
                    , isActive = True
                    , isOpen = model.openedWidget == Just FontColorPicker
                    , openMsg = config.outMsg <| OpenWidget FontColorPicker
                    , label =
                        row [ spacing 10 ]
                            [ el [] (html <| penTool iconSize)
                            , Keyed.el
                                [ width (px 14)
                                , height (px 14)
                                , Maybe.map parseColor fontColor
                                    |> Maybe.map (\res -> rgb255 res.r res.g res.b)
                                    |> Maybe.map Background.color
                                    |> Maybe.withDefault noAttr
                                , Border.width 1
                                , Border.color (rgb 0 0 0)
                                ]
                                ( Maybe.withDefault "" fontColor
                                , Element.none
                                )
                            ]
                    , widgetView = widgetView
                    , widgetConfig =
                        { handler =
                            config.outMsg
                                << SetTextColor
                                    (selectionCollapsed
                                        == Just True
                                        || selectionCollapsed
                                        == Nothing
                                    )
                                << parseRgb
                        , attribute = "fontColor"
                        , currentColor = fontColor
                        }
                    }
        , case config.colorPicker of
            Nothing ->
                Element.none

            Just { widgetView } ->
                widgetViewWrapper
                    { id = "backgroundColorPicker"
                    , isActive = True
                    , isOpen = model.openedWidget == Just BackgroundColorPicker
                    , openMsg = config.outMsg <| OpenWidget BackgroundColorPicker
                    , label =
                        row [ spacing 10 ]
                            [ el [] (html <| droplet iconSize)
                            , Keyed.el
                                [ width (px 14)
                                , height (px 14)
                                , Maybe.map parseColor backgroundColor
                                    |> Maybe.map (\res -> rgb255 res.r res.g res.b)
                                    |> Maybe.map Background.color
                                    |> Maybe.withDefault noAttr
                                , Border.width 1
                                , Border.color (rgb 0 0 0)
                                ]
                                ( Maybe.withDefault "" backgroundColor
                                , Element.none
                                )
                            ]
                    , widgetView = widgetView
                    , widgetConfig =
                        { handler =
                            config.outMsg
                                << SetBackgroundColor
                                    (selectionCollapsed
                                        == Just True
                                        || selectionCollapsed
                                        == Nothing
                                    )
                                << parseRgb
                        , attribute = "backgroundColor"
                        , currentColor = backgroundColor
                        }
                    }
        , Element.map
            config.outMsg
          <|
            el
                []
                (html <|
                    Html.select
                        [ HtmlEvents.onInput (SetFont (selectionCollapsed == Just True || selectionCollapsed == Nothing))
                        ]
                        (List.map
                            (fontOptionView
                                textFont
                            )
                            (List.sort fonts)
                        )
                )
        , Element.map config.outMsg <|
            el
                []
                (html <|
                    Html.select
                        [ HtmlEvents.onInput
                            (\n ->
                                String.toInt n
                                    |> Maybe.withDefault 16
                                    |> SetFontSize (selectionCollapsed == Just True || selectionCollapsed == Nothing)
                            )

                        --, HtmlAttr.disabled (not <| canCustomStyleSelection model)
                        ]
                        (List.map
                            (fontSizeOptionView
                                fontSize
                            )
                            fontSizes
                        )
                )
        , Element.map config.outMsg <|
            row
                [ spacing 10 ]
                [ Input.button
                    (toogleButtonStyle (List.member FontAlignLeft model.globalAttributes) canUpdateGlobalAttr)
                    { onPress =
                        if canUpdateGlobalAttr then
                            Just (SetAlignMent FontAlignLeft)
                            --Just (SetGlobalAttribute True ( "text-align", "left" ))

                        else
                            Nothing
                    , label =
                        el [] (html <| alignLeft iconSize)
                    }
                , Input.button
                    (toogleButtonStyle (List.member Center model.globalAttributes) canUpdateGlobalAttr)
                    { onPress =
                        if canUpdateGlobalAttr then
                            Just (SetAlignMent Center)
                            --Just (SetGlobalAttribute True ( "text-align", "center" ))

                        else
                            Nothing
                    , label =
                        el [] (html <| alignCenter iconSize)
                    }
                , Input.button
                    (toogleButtonStyle
                        (List.member FontAlignRight model.globalAttributes)
                        canUpdateGlobalAttr
                    )
                    { onPress =
                        if canUpdateGlobalAttr then
                            Just (SetAlignMent FontAlignRight)
                            --Just (SetGlobalAttribute True ( "text-align", "right" ))

                        else
                            Nothing
                    , label =
                        el [] (html <| alignRight iconSize)
                    }
                , Input.button
                    (toogleButtonStyle
                        (List.member Justify model.globalAttributes)
                        canUpdateGlobalAttr
                    )
                    { onPress =
                        if canUpdateGlobalAttr then
                            Just (SetAlignMent Justify)
                            --Just (SetGlobalAttribute True ( "text-align", "justify" ))

                        else
                            Nothing
                    , label =
                        el [] (html <| alignJustify iconSize)
                    }
                ]
        , Element.map config.outMsg <|
            let
                canUndoStyle =
                    selectionAttrs /= Just Dict.empty
            in
            Input.button
                (Element.alignRight :: buttonStyle canUndoStyle)
                { onPress =
                    if canUndoStyle then
                        Just UndoStyle

                    else
                        Nothing
                , label =
                    el [] (html <| xSquare iconSize)
                }

        --config.pageList
        ]


widgetViewWrapper :
    { id : String
    , isActive : Bool
    , isOpen : Bool
    , openMsg : msg
    , label : Element msg
    , widgetView : widgetConfig -> Element msg
    , widgetConfig : widgetConfig

    --, outMsg : Msg -> msg
    }
    -> Element msg
widgetViewWrapper { id, isActive, isOpen, openMsg, label, widgetView, widgetConfig } =
    el
        [ below <|
            el
                [ Background.color (rgb 0.95 0.95 0.95) ]
                (if isOpen then
                    widgetView widgetConfig

                 else
                    Element.none
                )
        , htmlAttribute <| HtmlAttr.id id
        ]
        (Input.button
            (buttonStyle isActive)
            { onPress =
                if isActive then
                    Just openMsg

                else
                    Nothing
            , label = label
            }
        )



-------------------------------------------------------------------------------
---------------------------
-- Outside click decoder --
---------------------------


outsideTargetHandler : String -> msg -> D.Decoder msg
outsideTargetHandler targetId handler =
    D.field "target" (isOutsideTarget targetId)
        |> D.andThen
            (\isOutside ->
                if isOutside then
                    D.succeed handler

                else
                    D.fail "inside target"
            )


isOutsideTarget targetId =
    D.oneOf
        [ D.field "id" D.string
            |> D.andThen
                (\id ->
                    if targetId == id then
                        -- found match by id
                        D.succeed False

                    else
                        -- try next decoder
                        D.fail "continue"
                )
        , D.lazy (\_ -> D.field "parentNode" (isOutsideTarget targetId))

        -- fallback if all previous decoders failed
        , D.succeed True
        ]



-------------------------------------------------------------------------------


decodeSelection =
    let
        decodeAttrValue =
            D.oneOf
                [ D.string
                , D.int
                    |> D.map String.fromInt
                , D.bool
                    |> D.andThen
                        (\b ->
                            if b then
                                D.succeed "true"

                            else
                                D.succeed "false"
                        )
                , D.succeed "unknown"
                ]
    in
    D.map5 (\start end attrs ids text -> ( Selection start end attrs text, ids ))
        (D.field "start" D.int)
        (D.field "end" D.int)
        (D.field "attrs" (D.map Dict.fromList (D.keyValuePairs decodeAttrValue)))
        (D.field "attachmentsIds" (D.list D.int))
        (D.field "text" D.string)


decodeEditorMarkup : D.Decoder HtmlContent
decodeEditorMarkup =
    D.map2 HtmlContent
        (D.at [ "target", "value" ] D.string)
        (D.at [ "target", "textContent" ] D.string)



-------------------------------------------------------------------------------


parseRgb s =
    let
        extractColValue v =
            String.toInt v
                |> Maybe.withDefault 0

        extractAlphaValue v =
            String.toFloat v
                |> Maybe.withDefault 1

        prefixLength =
            if String.startsWith "rgba" s then
                5

            else
                4
    in
    case
        String.dropLeft prefixLength s
            |> String.dropRight 1
            |> String.replace " " ""
            |> String.split ","
    of
        r :: g :: b :: a :: [] ->
            { r = extractColValue r
            , g = extractColValue g
            , b = extractColValue b
            , a = extractAlphaValue a
            }

        r :: g :: b :: [] ->
            { r = extractColValue r
            , g = extractColValue g
            , b = extractColValue b
            , a = 1
            }

        _ ->
            { r = 0
            , g = 0
            , b = 0
            , a = 1
            }


rgbToHex s =
    case
        String.dropLeft 4 s
            |> String.dropRight 1
            |> String.replace " " ""
            |> String.split ","
            |> List.filterMap String.toInt
    of
        r :: g :: b :: a :: [] ->
            ((String.padLeft 2 '0' <| Hex.toString r)
                ++ (String.padLeft 2 '0' <| Hex.toString g)
                ++ (String.padLeft 2 '0' <| Hex.toString b)
            )
                |> String.toUpper

        r :: g :: b :: [] ->
            ((String.padLeft 2 '0' <| Hex.toString r)
                ++ (String.padLeft 2 '0' <| Hex.toString g)
                ++ (String.padLeft 2 '0' <| Hex.toString b)
            )
                |> String.toUpper

        _ ->
            "000000"


hexToRgb hexColor =
    let
        hexColor_ =
            String.toLower hexColor

        red =
            String.left 2 hexColor_
                |> Hex.fromString
                |> Result.withDefault 0

        green =
            String.dropLeft 2 hexColor_
                |> String.left 2
                |> Hex.fromString
                |> Result.withDefault 0

        blue =
            String.dropLeft 4 hexColor_
                |> String.left 2
                |> Hex.fromString
                |> Result.withDefault 0
    in
    { r = red
    , g = green
    , b = blue
    , a = 1
    }


parseColor s =
    if String.startsWith "#" s then
        String.dropLeft 1 s
            |> hexToRgb

    else
        parseRgb s


globalAttrToCss : GlobalAttribute -> List ( String, String )
globalAttrToCss attr =
    case attr of
        AlignRight ->
            [ ( "float", "right" ) ]

        AlignLeft ->
            [ ( "float", "left" ) ]

        BackgroundColor color ->
            [ ( "background-color", pickedColorToStr color )
            ]

        Border ->
            [ ( "border-style", "solid" )
            , ( "border-width", "1px" )
            , ( "border-color", "rgb(127,127,127)" )
            ]

        Font font ->
            [ ( "font-family", font ) ]

        FontColor { r, g, b, a } ->
            let
                ( r_, g_, b_ ) =
                    ( String.fromInt r
                    , String.fromInt g
                    , String.fromInt b
                    )
            in
            [ ( "color", "rgb(" ++ r_ ++ "," ++ g_ ++ "," ++ b_ ++ ")" ) ]

        FontSize n ->
            [ ( "font-size", String.fromInt n ++ "px" ) ]

        FontAlignLeft ->
            [ ( "text-align", "left" ) ]

        FontAlignRight ->
            [ ( "text-align", "right" ) ]

        Center ->
            [ ( "text-align", "center" ) ]

        Justify ->
            [ ( "text-align", "justify" ) ]

        Bold ->
            [ ( "font-weight", "bold" ) ]

        Italic ->
            [ ( "font-style", "italic" ) ]

        Other attribute value ->
            [ ( attribute, value ) ]


pickedColorToStr { r, g, b, a } =
    "rgba("
        ++ String.fromInt r
        ++ ","
        ++ String.fromInt g
        ++ ","
        ++ String.fromInt b
        ++ ","
        ++ String.fromFloat a
        ++ ")"


embeddedStyleSheet : String -> List GlobalAttribute -> Element msg
embeddedStyleSheet externalCss globalAttributes =
    let
        styleSheet =
            Html.div
                []
                [ Html.node "style"
                    []
                    [ Html.text <|
                        externalCss
                            ++ " .trix-content{"
                            ++ stringifyAttributes (List.concatMap globalAttrToCss globalAttributes)
                            ++ """
                                }
                            """
                            ++ """ 

                            
                            .trix-content p {
                                display: block;
                                margin: 0;
                                padding : 0;
                                line-height: 150%;
                            }
                            
                    """
                    ]
                ]
    in
    el
        []
        (html <| styleSheet)


stringifyAttributes : List ( String, String ) -> String
stringifyAttributes attributes =
    List.map (\( attr, value ) -> attr ++ ": " ++ value ++ ";") attributes
        |> String.join " "



-------------------------------------------------------------------------------


fonts =
    [ "Arial"
    , "Helvetica"
    , "Times New Roman"
    , "Times"
    , "Courier New"
    , "Courier"
    , "Verdana"
    , "Georgia"
    , "Palatino"
    , "Garamond"
    , "Bookman"
    , "Comic Sans MS"
    , "Trebuchet MS"
    , "Arial Black"
    , "Impact"
    , "Libre Baskerville"
    ]


fontSizes =
    [ "6"
    , "7"
    , "8"
    , "9"
    , "10"
    , "11"
    , "12"
    , "13"
    , "14"
    , "15"
    , "16"
    , "18"
    , "20"
    , "22"
    , "24"
    , "26"
    , "28"
    , "32"
    , "36"
    , "40"
    , "44"
    , "48"
    , "54"
    , "60"
    , "66"
    , "72"
    , "80"
    , "88"
    , "96"
    ]


chunks : Int -> List a -> List (List a)
chunks n xs =
    let
        helper acc ys =
            case ys of
                [] ->
                    List.reverse acc

                _ ->
                    helper (List.take n ys :: acc) (List.drop n ys)
    in
    helper [] xs


buttonStyle isActive =
    [ Border.rounded 5
    , Font.center
    , centerY
    , padding 5
    , focused [ Border.glow (rgb 1 1 1) 0 ]
    ]
        ++ (if isActive then
                [ Background.color (rgb 0.9 0.9 0.9)
                , mouseOver [ Font.color (rgb 255 255 255) ]
                , Border.width 1
                , Border.color (rgb 0.9 0.9 0.9)
                ]

            else
                [ Background.color (rgb 0.95 0.95 0.95)
                , Font.color (rgb 0.7 0.7 0.7)
                , htmlAttribute <| HtmlAttr.style "cursor" "default"
                , Border.width 1
                , Border.color (rgb 0.95 0.95 0.95)
                ]
           )


toogleButtonStyle isPressed isActive =
    [ Border.rounded 5
    , Font.center
    , centerY
    , padding 5
    , focused [ Border.glow (rgb 1 1 1) 0 ]
    ]
        ++ (if isActive then
                [ mouseOver
                    [ Font.color (rgb 0.3 0.3 0.3)
                    ]
                ]
                    ++ (if isPressed then
                            [ Background.color (rgb 0.9 0.9 0.9)
                            , Border.width 1
                            , Border.color (rgb 0.9 0.9 0.9)
                            , Border.shadow
                                { offset = ( 1, 1 )
                                , size = 1
                                , blur = 0.5
                                , color = rgb 0.5 0.5 0.5
                                }
                            , focused
                                [ Border.shadow
                                    { offset = ( 1, 1 )
                                    , size = 1
                                    , blur = 0.5
                                    , color = rgb 0.5 0.5 0.5
                                    }
                                ]
                            ]

                        else
                            [ Background.color (rgb 0.9 0.9 0.9)
                            , Border.width 1
                            , Border.color (rgb 0.9 0.9 0.9)
                            ]
                       )

            else
                [ Background.color (rgb 0.95 0.95 0.95)
                , Font.color (rgb 0.7 0.7 0.7)
                , htmlAttribute <| HtmlAttr.style "cursor" "default"
                , Border.width 1
                , Border.color (rgb 0.95 0.95 0.95)
                ]
           )


textInputStyle =
    [ width (px 250)
    , paddingXY 5 5
    , spacing 15
    , focused [ Border.glow (rgb 1 1 1) 0 ]
    ]


noAttr =
    htmlAttribute <| HtmlAttr.class ""



---------------------------------------------------------------------
-- Icons


customSvgFeatherIcon : Int -> String -> List (Svg msg) -> Html.Html msg
customSvgFeatherIcon size className =
    svg
        [ Svg.Attributes.class <| "feather feather-" ++ className
        , Svg.Attributes.fill "none"
        , Svg.Attributes.height (String.fromInt size)
        , Svg.Attributes.stroke "currentColor"
        , Svg.Attributes.strokeLinecap "round"
        , Svg.Attributes.strokeLinejoin "round"
        , Svg.Attributes.strokeWidth "2"
        , Svg.Attributes.viewBox "0 0 24 24"
        , Svg.Attributes.width (String.fromInt size)
        ]


xSquare : Int -> Html.Html msg
xSquare size =
    customSvgFeatherIcon size
        "x-square"
        [ Svg.rect
            [ Svg.Attributes.x "3"
            , y "3"
            , Svg.Attributes.width "18"
            , Svg.Attributes.height "18"
            , rx "2"
            , ry "2"
            ]
            []
        , Svg.line [ x1 "9", y1 "9", x2 "15", y2 "15" ] []
        , Svg.line [ x1 "15", y1 "9", x2 "9", y2 "15" ] []
        ]


alignJustify : Int -> Html.Html msg
alignJustify size =
    customSvgFeatherIcon size
        "align-justify"
        [ Svg.line [ x1 "21", y1 "10", x2 "3", y2 "10" ] []
        , Svg.line [ x1 "21", y1 "6", x2 "3", y2 "6" ] []
        , Svg.line [ x1 "21", y1 "14", x2 "3", y2 "14" ] []
        , Svg.line [ x1 "21", y1 "18", x2 "3", y2 "18" ] []
        ]


alignLeft : Int -> Html.Html msg
alignLeft size =
    customSvgFeatherIcon size
        "align-left"
        [ Svg.line [ x1 "17", y1 "10", x2 "3", y2 "10" ] []
        , Svg.line [ x1 "21", y1 "6", x2 "3", y2 "6" ] []
        , Svg.line [ x1 "21", y1 "14", x2 "3", y2 "14" ] []
        , Svg.line [ x1 "17", y1 "18", x2 "3", y2 "18" ] []
        ]


alignCenter : Int -> Html.Html msg
alignCenter size =
    customSvgFeatherIcon size
        "align-center"
        [ Svg.line
            [ x1 "18"
            , y1 "10"
            , x2 "6"
            , y2 "10"
            ]
            []
        , Svg.line [ x1 "21", y1 "6", x2 "3", y2 "6" ] []
        , Svg.line [ x1 "21", y1 "14", x2 "3", y2 "14" ] []
        , Svg.line [ x1 "18", y1 "18", x2 "6", y2 "18" ] []
        ]


alignRight : Int -> Html.Html msg
alignRight size =
    customSvgFeatherIcon size
        "align-right"
        [ Svg.line
            [ x1 "21"
            , y1 "10"
            , x2 "7"
            , y2 "10"
            ]
            []
        , Svg.line [ x1 "21", y1 "6", x2 "3", y2 "6" ] []
        , Svg.line [ x1 "21", y1 "14", x2 "3", y2 "14" ] []
        , Svg.line [ x1 "21", y1 "18", x2 "7", y2 "18" ] []
        ]


droplet : Int -> Html.Html msg
droplet size =
    customSvgFeatherIcon size
        "droplet"
        [ Svg.path [ d "M12 2.69l5.66 5.66a8 8 0 1 1-11.31 0z" ] []
        ]


penTool : Int -> Html.Html msg
penTool size =
    customSvgFeatherIcon size
        "pen-tool"
        [ Svg.path [ d "M12 19l7-7 3 3-7 7-3-3z" ] []
        , Svg.path [ d "M18 13l-1.5-7.5L2 2l3.5 14.5L13 18l5-5z" ] []
        , Svg.path [ d "M2 2l7.586 7.586" ] []
        , Svg.circle [ cx "11", cy "11", r "2" ] []
        ]


link2 : Int -> Html.Html msg
link2 size =
    customSvgFeatherIcon size
        "link-2"
        [ Svg.path [ d "M15 7h3a5 5 0 0 1 5 5 5 5 0 0 1-5 5h-3m-6 0H6a5 5 0 0 1-5-5 5 5 0 0 1 5-5h3" ] []
        , Svg.line [ x1 "8", y1 "12", x2 "16", y2 "12" ] []
        ]


imageIcon : Int -> Html.Html msg
imageIcon size =
    customSvgFeatherIcon size
        "image"
        [ Svg.rect
            [ Svg.Attributes.x "3"
            , y "3"
            , Svg.Attributes.width "18"
            , Svg.Attributes.height "18"
            , rx "2"
            , ry "2"
            ]
            []
        , Svg.circle [ cx "8.5", cy "8.5", r "1.5" ] []
        , Svg.polyline [ points "21 15 16 10 5 21" ] []
        ]


fileText : Int -> Html.Html msg
fileText size =
    customSvgFeatherIcon size
        "file-text"
        [ Svg.path
            [ d "M14 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V8z" ]
            []
        , Svg.polyline [ points "14 2 14 8 20 8" ] []
        , Svg.line [ x1 "16", y1 "13", x2 "8", y2 "13" ] []
        , Svg.line [ x1 "16", y1 "17", x2 "8", y2 "17" ] []
        , Svg.polyline [ points "10 9 9 9 8 9" ] []
        ]
