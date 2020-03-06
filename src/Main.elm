port module Main exposing (Flags, HtmlContent, Model, Msg(..), decodeDroppedFile, decodeEditorMarkup, init, main, subscriptions, trixEditor, update, view)

import Browser exposing (..)
import Browser.Events exposing (onMouseDown)
import Browser.Navigation as Nav
import Dict exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import File exposing (..)
import Hex exposing (..)
import Html as Html
import Html.Attributes as HtmlAttr
import Html.Events as HtmlEvents exposing (on, onInput)
import Html.Parser exposing (..)
import Html.Parser.Util exposing (..)
import Json.Decode as D
import Json.Encode as E
import Json.Value exposing (decodeValue)
import Url as Url exposing (..)


port activateAttribute : E.Value -> Cmd msg


port deactivateAttributes : E.Value -> Cmd msg


port insertHtml : E.Value -> Cmd msg


port getSelection : () -> Cmd msg


port selection : (E.Value -> msg) -> Sub msg


type alias Model =
    { htmlContent : HtmlContent
    , selection : Maybe Selection
    , openedWidget : Maybe Widget
    , debugEvent : Maybe E.Value
    , globalAttributes : Dict String String
    , attachmentAttributes : Dict Int (Dict String String)
    , attachmentsIds : List Int
    }


type alias Selection =
    { start : Int
    , end : Int
    , attrs : Dict String String
    }


type Widget
    = FontColorPicker
    | BackgroundColorPicker
    | InternalLinks
    | ImagePicker
    | DocPicker


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { htmlContent = HtmlContent "" ""
      , selection = Nothing
      , openedWidget = Nothing
      , debugEvent = Nothing
      , globalAttributes = Dict.empty
      , attachmentAttributes = Dict.empty
      , attachmentsIds = []
      }
    , Cmd.none
    )


subscriptions model =
    Sub.batch
        [ case model.openedWidget of
            Just FontColorPicker ->
                Browser.Events.onMouseDown (outsideTargetHandler "fontColorPicker" Close)

            Just BackgroundColorPicker ->
                Browser.Events.onMouseDown (outsideTargetHandler "backgroundColorPicker" Close)

            Just InternalLinks ->
                Browser.Events.onMouseDown (outsideTargetHandler "internalLinkPicker" Close)

            _ ->
                Sub.none
        , selection GotSelection
        ]


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


type alias HtmlContent =
    { html : String
    , text : String
    }


type Msg
    = GetHtmlContent HtmlContent
    | GetSelection
    | GotSelection E.Value
    | OpenInternalLinks
    | InsertInternalLink String
    | OpenFontColorPicker
    | OpenBackgroundColorPicker
    | SetTextColor String
    | SetBackgroundColor String
    | SetFont String
    | SetFontSize Int
    | SetGlobalAttribute Bool ( String, String )
    | UndoStyle
    | Close
    | DebugEvent E.Value
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetHtmlContent content ->
            ( { model | htmlContent = content }, getSelection () )

        GetSelection ->
            ( model, getSelection () )

        GotSelection value ->
            case D.decodeValue decodeSelection value of
                Ok ( sel, ids ) ->
                    ( { model
                        | selection = Just sel
                        , attachmentsIds = ids
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        Close ->
            ( { model
                | openedWidget = Nothing
              }
            , Cmd.none
            )

        -----------
        -- Links --
        -----------
        OpenInternalLinks ->
            ( { model
                | openedWidget =
                    if model.openedWidget == Just InternalLinks then
                        Nothing

                    else
                        Just InternalLinks
              }
            , Cmd.none
            )

        InsertInternalLink url ->
            case model.selection of
                Just { start, end, attrs } ->
                    if start /= end then
                        let
                            selected =
                                String.slice start end model.htmlContent.text

                            link =
                                "<a href=internal:" ++ url ++ ">" ++ selected ++ "</>"

                            data =
                                E.object
                                    [ ( "selectionStart", E.int start )
                                    , ( "selectionEnd", E.int end )
                                    , ( "tagName", E.string "internal link" )
                                    , ( "html", E.string link )
                                    ]
                        in
                        ( model, insertHtml data )

                    else
                        ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        OpenFontColorPicker ->
            ( { model
                | openedWidget =
                    if model.openedWidget == Just FontColorPicker then
                        Nothing

                    else
                        Just FontColorPicker
              }
            , Cmd.none
            )

        OpenBackgroundColorPicker ->
            ( { model
                | openedWidget =
                    if model.openedWidget == Just BackgroundColorPicker then
                        Nothing

                    else
                        Just BackgroundColorPicker
              }
            , Cmd.none
            )

        SetTextColor color ->
            case model.selection of
                Just { start, end, attrs } ->
                    let
                        data =
                            E.object
                                [ ( "selectionStart", E.int start )
                                , ( "selectionEnd", E.int end )
                                , ( "attribute", E.string "foregroundColor" )
                                , ( "value", E.string ("#" ++ (Dict.get color webColors |> Maybe.withDefault "000000")) )
                                ]
                    in
                    ( { model | openedWidget = Nothing }, activateAttribute data )

                _ ->
                    ( model, Cmd.none )

        SetBackgroundColor color ->
            case model.selection of
                Just { start, end, attrs } ->
                    let
                        data =
                            E.object
                                [ ( "selectionStart", E.int start )
                                , ( "selectionEnd", E.int end )
                                , ( "attribute", E.string "backgroundColor" )
                                , ( "value", E.string ("#" ++ (Dict.get color webColors |> Maybe.withDefault "000000")) )
                                ]
                    in
                    ( { model | openedWidget = Nothing }, activateAttribute data )

                _ ->
                    ( model, Cmd.none )

        SetFont font ->
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
                    ( { model | openedWidget = Nothing }, activateAttribute data )

                _ ->
                    ( model, Cmd.none )

        SetFontSize n ->
            ( model, Cmd.none )

        SetGlobalAttribute toogle attr ->
            ( { model | globalAttributes = updateAttribute toogle attr model.globalAttributes }, Cmd.none )

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
                    ( { model | openedWidget = Nothing }, deactivateAttributes data )

                _ ->
                    ( model, Cmd.none )

        DebugEvent v ->
            ( { model | debugEvent = Just v }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


view model =
    Html.div
        []
        [ Html.node "style"
            []
            [ Html.text <|
                " .trix-content{"
                    ++ stringifyAttributes model.globalAttributes
                    ++ """ 
                    
                    }
                """
                    ++ """ 

                        .trix-content-editor figure{
                            
                        }
                        .trix-content-editor figure:nth-of-type(2){
                            opacity: 0.5;
                            float: left;
                            background-color: red;
                            width: auto;
                        }
                """
            ]
        , Element.layout
            []
            (column
                [ width (maximum 1000 fill)
                , spacing 30
                ]
                [ editor model
                , renderer model
                ]
            )
        ]


stringifyAttributes : Dict String String -> String
stringifyAttributes attributes =
    Dict.toList attributes
        |> List.map (\( attr, value ) -> attr ++ ": " ++ value ++ ";")
        |> String.join " "


editor model =
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

        fontColor =
            Maybe.andThen (Dict.get "foregroundColor") selectionAttrs
                |> Maybe.map (String.dropLeft 1)
                |> Maybe.andThen (\hex -> Dict.get hex webColorsReversed)

        backgroundColor =
            Maybe.andThen (Dict.get "backgroundColor") selectionAttrs
                |> Maybe.map (String.dropLeft 1)
                |> Maybe.andThen (\hex -> Dict.get hex webColorsReversed)

        textFont =
            Maybe.andThen (Dict.get "textFont") selectionAttrs

        fontOptionView selectedFont f =
            Html.option
                [ HtmlAttr.value f
                , HtmlAttr.selected (selectedFont == (Just <| f))
                ]
                [ Html.text f ]

        customControls =
            row
                [ spacing 10 ]
                [ colorPicker
                    "fontColorPicker"
                    True
                    (model.openedWidget == Just FontColorPicker)
                    fontColor
                    OpenFontColorPicker
                    SetTextColor
                    "text"
                , colorPicker
                    "backgroundColorPicker"
                    True
                    (model.openedWidget == Just BackgroundColorPicker)
                    backgroundColor
                    OpenBackgroundColorPicker
                    SetBackgroundColor
                    "back"
                , el
                    []
                    (html <|
                        Html.select
                            [ HtmlEvents.onInput SetFont
                            , HtmlAttr.disabled (selectionCollapsed == Just True || selectionCollapsed == Nothing)
                            ]
                            (List.map
                                (fontOptionView
                                    textFont
                                )
                                (List.sort fonts)
                            )
                    )
                , linkPicker
                    "internalLinkPicker"
                    True
                    --(canStyleSelection model)
                    (model.openedWidget == Just InternalLinks)
                    Nothing
                    OpenInternalLinks
                    InsertInternalLink
                    [ "home", "contact" ]
                , Input.button
                    (buttonStyle canUpdateGlobalAttr)
                    { onPress =
                        if canUpdateGlobalAttr then
                            Just (SetGlobalAttribute True ( "text-align", "justify" ))

                        else
                            Nothing
                    , label =
                        text "justify"
                    }
                , let
                    canUndoStyle =
                        selectionAttrs /= Just Dict.empty
                  in
                  Input.button
                    (buttonStyle canUndoStyle)
                    { onPress =
                        if canUndoStyle then
                            Just UndoStyle

                        else
                            Nothing
                    , label =
                        text "undo style"
                    }

                --config.pageList
                ]
    in
    column
        [ padding 15
        , spacing 10

        --, Background.color (rgb255 123 45 70)
        ]
        [ customControls
        , paragraph [] [ html <| trixEditor ]
        ]


trixEditor =
    Html.node "trix-editor"
        [ on "trix-change" (D.map GetHtmlContent decodeEditorMarkup)
        , on "trix-selection-change" (D.map (always GetSelection) (D.succeed ()))
        , HtmlAttr.class "trix-content"
        , HtmlAttr.class "trix-content-editor"

        --, on "trix-attachment-add" (Decode.map mapAttachmentToMsg decodeDroppedFile)
        --, HtmlAttr.attribute "white-space" "normal"
        ]
        []


renderer model =
    let
        content =
            Html.Parser.run model.htmlContent.html
                |> Result.map (List.map processLinks)
                |> Result.map (List.map resetFiguresStyle)
                |> Result.map Html.Parser.Util.toVirtualDom
                |> Result.map (Html.div [ HtmlAttr.class "trix-content" ])
                |> Result.map (\r -> paragraph [] [ html r ])
                |> Result.withDefault Element.none
    in
    el
        [ padding 15 ]
        content


processLinks node =
    let
        processLinkAttrs processed toProcess =
            case toProcess of
                ( "href", url ) :: xs ->
                    if String.startsWith "internal:" url then
                        ( "href", String.dropLeft (String.length "internal:") url )
                            :: (List.reverse processed ++ xs)

                    else
                        ( "href", url )
                            :: ( "target", "blank" )
                            :: (List.reverse processed ++ xs)

                other :: xs ->
                    processLinkAttrs (other :: processed) xs

                [] ->
                    processed
    in
    case node of
        Element "a" attrs nodes ->
            Element "a" (processLinkAttrs [] attrs) (List.map processLinks nodes)

        Element tag attrs nodes ->
            Element tag attrs (List.map processLinks nodes)

        Text value ->
            Text value

        Comment value ->
            Comment value


resetFiguresStyle node =
    let
        removeAttrs attrs =
            List.foldr
                (\( attr, value ) acc ->
                    if List.member attr toRemove then
                        acc

                    else
                        ( attr, value ) :: acc
                )
                []
                attrs

        toRemove =
            [ "class", "data-trix-attachment", "data-trix-content-type", "data-trix-attributes" ]
    in
    case node of
        Element "figure" attrs nodes ->
            Element "figure" (removeAttrs attrs) (List.map resetFiguresStyle nodes)

        Element "figcaption" attrs nodes ->
            Element "figcaption" (removeAttrs attrs) (List.map resetFiguresStyle nodes)

        Element tag attrs nodes ->
            Element tag attrs (List.map resetFiguresStyle nodes)

        Text value ->
            Text value

        Comment value ->
            Comment value


updateAttribute : Bool -> ( String, String ) -> Dict String String -> Dict String String
updateAttribute toogle ( attr, value ) attributes =
    Dict.update
        attr
        (\mbVal ->
            case mbVal of
                Just val ->
                    if toogle then
                        Nothing

                    else
                        Just value

                Nothing ->
                    Just value
        )
        attributes



--mapNode : Html.Parser.Node -> (Html.Parser.Node -> Html.Parser.Node) -> Html.Parser.Node
--mapNode node =
--    case node of
--        (Html.Parser.Element tag attrs nodes) as node_ ->


decodeDroppedFile : D.Decoder ( Int, File )
decodeDroppedFile =
    D.map2 Tuple.pair
        (D.at [ "attachment", "id" ] D.int)
        (D.at [ "attachment", "file" ] File.decoder)


decodeEditorMarkup : D.Decoder HtmlContent
decodeEditorMarkup =
    D.map2 HtmlContent
        (D.at [ "target", "value" ] D.string)
        (D.at [ "target", "textContent" ] D.string)


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
    D.map4 (\start end attrs ids -> ( Selection start end attrs, ids ))
        (D.field "start" D.int)
        (D.field "end" D.int)
        (D.field "attrs" (D.map Dict.fromList (D.keyValuePairs decodeAttrValue)))
        (D.field "attachmentsIds" (D.list D.int))



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
------------------
-- Links picker --
------------------


linkPicker : String -> Bool -> Bool -> Maybe String -> Msg -> (String -> Msg) -> List String -> Element Msg
linkPicker id isActive linkPickerOpen currentLink openMsg handler pageList =
    let
        linkView page =
            row
                [ width fill
                , spacing 10
                ]
                [ el
                    [ mouseOver
                        [ Font.color (Element.rgb255 0 0 255)
                        ]
                    , pointer
                    , Events.onClick (handler page)
                    , width fill
                    , padding 10
                    , case currentLink of
                        Just link ->
                            if link == page then
                                Background.color (Element.rgb255 255 255 255)

                            else
                                noAttr

                        _ ->
                            noAttr
                    ]
                    (text page)
                ]
    in
    el
        [ below <|
            el
                [ Background.color (rgb 0.95 0.95 0.95) ]
                (if linkPickerOpen then
                    column
                        [ padding 10
                        , width fill
                        , height (maximum 200 fill)
                        , scrollbarY
                        ]
                        (List.map linkView pageList)

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
            , label =
                row
                    [ spacing 5 ]
                    [ el [] (text "lien page interne")

                    --, Icons.link
                    --    (Icons.defOptions
                    --        |> Icons.color black
                    --        |> Icons.size 20
                    --    )
                    ]
            }
        )



-------------------------------------------------------------------------------
---------------------------------------
-- Color functions  and color picker --
---------------------------------------


colorPicker :
    String
    -> Bool
    -> Bool
    -> Maybe String
    -> Msg
    -> (String -> Msg)
    -> String
    -> Element.Element Msg
colorPicker id isActive colorPickerOpen currentColor openMsg handler label =
    let
        currentColor_ =
            currentColor
                |> Maybe.andThen (\c -> Dict.get c webColors)
                |> Maybe.map hexToColor
                |> Maybe.withDefault (rgb 1 1 1)

        colorPanView ( colname, colhex ) =
            el
                [ width (px 14)
                , height (px 14)
                , Background.color (hexToColor colhex)
                , Border.width 1
                , Border.color (rgb 0 0 0)
                , pointer
                , mouseOver
                    [ Border.color (rgb 0.9 0.9 0.9) ]
                , Events.onClick (handler colname)
                ]
                Element.none

        colors =
            chunks 12 (Dict.toList webColors)
                |> List.map
                    (\r ->
                        row [ spacing 3 ]
                            (List.map colorPanView r)
                    )
    in
    el
        [ below <|
            el
                [ Background.color (rgb 0.95 0.95 0.95) ]
                (if colorPickerOpen then
                    column
                        [ spacing 3
                        , padding 10
                        ]
                        colors

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
            , label =
                row [ spacing 10 ]
                    [ el [] (text label)
                    , el
                        [ width (px 14)
                        , height (px 14)
                        , Background.color currentColor_
                        , Border.width 1
                        , Border.color (rgb 0 0 0)
                        ]
                        Element.none
                    ]
            }
        )


hexToColor : String -> Color
hexToColor hexColor =
    let
        hexColor_ =
            String.toLower hexColor

        red =
            String.left 2 hexColor_
                |> Hex.fromString
                |> Result.withDefault 0
                |> toFloat

        green =
            String.dropLeft 2 hexColor_
                |> String.left 2
                |> Hex.fromString
                |> Result.withDefault 0
                |> toFloat

        blue =
            String.dropLeft 4 hexColor_
                |> String.left 2
                |> Hex.fromString
                |> Result.withDefault 0
                |> toFloat
    in
    rgb (red / 255) (green / 255) (blue / 255)


webColorsReversed =
    Dict.toList webColors
        |> List.map (\( a, b ) -> ( b, a ))
        |> Dict.fromList


webColors =
    Dict.fromList
        [ ( "maroon", "800000" )
        , ( "dark red", "8B0000" )
        , ( "brown", "A52A2A" )
        , ( "firebrick", "B22222" )
        , ( "crimson", "DC143C" )
        , ( "red", "FF0000" )
        , ( "tomato", "FF6347" )
        , ( "coral", "FF7F50" )
        , ( "indian red", "CD5C5C" )
        , ( "light coral", "F08080" )
        , ( "dark salmon", "E9967A" )
        , ( "salmon", "FA8072" )
        , ( "light salmon", "FFA07A" )
        , ( "orange red", "FF4500" )
        , ( "dark orange", "FF8C00" )
        , ( "orange", "FFA500" )
        , ( "gold", "FFD700" )
        , ( "dark golden rod", "B8860B" )
        , ( "golden rod", "DAA520" )
        , ( "pale golden rod", "EEE8AA" )
        , ( "dark khaki", "BDB76B" )
        , ( "khaki", "F0E68C" )
        , ( "olive", "808000" )
        , ( "yellow", "FFFF00" )
        , ( "yellow green", "9ACD32" )
        , ( "dark olive green", "556B2F" )
        , ( "olive drab", "6B8E23" )
        , ( "lawn green", "7CFC00" )
        , ( "chart reuse", "7FFF00" )
        , ( "green yellow", "ADFF2F" )
        , ( "dark green", "006400" )
        , ( "green", "008000" )
        , ( "forest green", "228B22" )
        , ( "lime", "00FF00" )
        , ( "lime green", "32CD32" )
        , ( "light green", "90EE90" )
        , ( "pale green", "98FB98" )
        , ( "dark sea green", "8FBC8F" )
        , ( "medium spring green", "00FA9A" )
        , ( "spring green", "0F0FF7F" )
        , ( "sea green", "2E8B57" )
        , ( "medium aqua marine", "66CDAA" )
        , ( "medium sea green", "3CB371" )
        , ( "light sea green", "20B2AA" )
        , ( "dark slate gray", "2F4F4F" )
        , ( "teal", "008080" )
        , ( "dark cyan", "008B8B" )
        , ( "aqua", "00FFFF" )
        , ( "cyan", "00FFFF" )
        , ( "light cyan", "E0FFFF" )
        , ( "dark turquoise", "00CED1" )
        , ( "turquoise", "40E0D0" )
        , ( "medium turquoise", "48D1CC" )
        , ( "pale turquoise", "AFEEEE" )
        , ( "aqua marine", "7FFFD4" )
        , ( "powder blue", "B0E0E6" )
        , ( "cadet blue", "5F9EA0" )
        , ( "steel blue", "4682B4" )
        , ( "corn flower blue", "6495ED" )
        , ( "deep sky blue", "00BFFF" )
        , ( "dodger blue", "1E90FF" )
        , ( "light blue", "ADD8E6" )
        , ( "sky blue", "87CEEB" )
        , ( "light sky blue", "87CEFA" )
        , ( "midnight blue", "191970" )
        , ( "navy", "000080" )
        , ( "dark blue", "00008B" )
        , ( "medium blue", "0000CD" )
        , ( "blue", "0000FF" )
        , ( "royal blue", "4169E1" )
        , ( "blue violet", "8A2BE2" )
        , ( "indigo", "4B0082" )
        , ( "dark slate blue", "483D8B" )
        , ( "slate blue", "6A5ACD" )
        , ( "medium slate blue", "7B68EE" )
        , ( "medium purple", "9370DB" )
        , ( "dark magenta", "8B008B" )
        , ( "dark violet", "9400D3" )
        , ( "dark orchid", "9932CC" )
        , ( "medium orchid", "BA55D3" )
        , ( "purple", "800080" )
        , ( "thistle", "D8BFD8" )
        , ( "plum", "DDA0DD" )
        , ( "violet", "EE82EE" )
        , ( "magenta / fuchsia", "FF00FF" )
        , ( "orchid", "DA70D6" )
        , ( "medium violet red", "C71585" )
        , ( "pale violet red", "DB7093" )
        , ( "deep pink", "FF1493" )
        , ( "hot pink", "FF69B4" )
        , ( "light pink", "FFB6C1" )
        , ( "pink", "FFC0CB" )
        , ( "antique white", "FAEBD7" )
        , ( "beige", "F5F5DC" )
        , ( "bisque", "FFE4C4" )
        , ( "blanched almond", "FFEBCD" )
        , ( "wheat", "F5DEB3" )
        , ( "corn silk", "FFF8DC" )
        , ( "lemon chiffon", "FFFACD" )
        , ( "light golden rod yellow", "FAFAD2" )
        , ( "light yellow", "FFFFE0" )
        , ( "saddle brown", "8B4513" )
        , ( "sienna", "A0522D" )
        , ( "chocolate", "D2691E" )
        , ( "peru", "CD853F" )
        , ( "sandy brown", "F4A460" )
        , ( "burly wood", "DEB887" )
        , ( "tan", "D2B48C" )
        , ( "rosy brown", "BC8F8F" )
        , ( "moccasin", "FFE4B5" )
        , ( "navajo white", "FFDEAD" )
        , ( "peach puff", "FFDAB9" )
        , ( "misty rose", "FFE4E1" )
        , ( "lavender blush", "FFF0F5" )
        , ( "linen", "FAF0E6" )
        , ( "old lace", "FDF5E6" )
        , ( "papaya whip", "FFEFD5" )
        , ( "sea shell", "FFF5EE" )
        , ( "mint cream", "F5FFFA" )
        , ( "slate gray", "708090" )
        , ( "light slate gray", "778899" )
        , ( "light steel blue", "B0C4DE" )
        , ( "lavender", "E6E6FA" )
        , ( "floral white", "FFFAF0" )
        , ( "alice blue", "F0F8FF" )
        , ( "ghost white", "F8F8FF" )
        , ( "honeydew", "F0FFF0" )
        , ( "ivory", "FFFFF0" )
        , ( "azure", "F0FFFF" )
        , ( "snow", "FFFAFA" )
        , ( "black", "000000" )
        , ( "dim gray / dim grey", "696969" )
        , ( "gray / grey", "808080" )
        , ( "dark gray / dark grey", "A9A9A9" )
        , ( "silver", "C0C0C0" )
        , ( "light gray / light grey", "D3D3D3" )
        , ( "gainsboro", "DCDCDC" )
        , ( "white smoke", "F5F5F5" )
        , ( "white", "FFFFFF" )
        ]



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


textInputStyle =
    [ width (px 250)
    , paddingXY 5 5
    , spacing 15
    , focused [ Border.glow (rgb 1 1 1) 0 ]
    ]


noAttr =
    htmlAttribute <| HtmlAttr.class ""
