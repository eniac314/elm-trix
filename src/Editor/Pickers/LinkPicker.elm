module Editor.Pickers.LinkPicker exposing (linkPickerView)

import Dict exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Html.Attributes as HtmlAttr


linkPickerView pageList { handler, currentLink } =
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
        [ Background.color (rgb 0.95 0.95 0.95) ]
        (column
            [ padding 10
            , width fill
            , height (maximum 200 fill)
            , scrollbarY
            ]
            (List.map linkView pageList)
        )


noAttr =
    htmlAttribute <| HtmlAttr.class ""
