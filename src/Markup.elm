module Markup exposing (..)

import Css
import Html.Styled as Html exposing (Html, div, form, h1, h4, img, label, p, pre, span, styled, text, toUnstyled)
import Html.Styled.Attributes as Attr
import Mark
import Mark.Error
import Metadata exposing (Metadata)
import Pages.Document


markupDocument : ( String, Pages.Document.DocumentHandler Metadata (Html msg) )
markupDocument =
    Pages.Document.parser
        { -- Ideally .emu but the file watch does not pick up changes.
          extension = "md"
        , metadata = Metadata.decoder
        , body =
            \source ->
                case Mark.compile document source of
                    Mark.Success body ->
                        Html.div [] body |> Ok

                    Mark.Almost { result, errors } ->
                        viewErrors errors |> Html.text |> Ok

                    Mark.Failure errors ->
                        viewErrors errors |> Html.text |> Ok
        }


viewErrors : List Mark.Error.Error -> String
viewErrors errors =
    List.map Mark.Error.toString errors
        |> String.join "\n"


document : Mark.Document (List (Html msg))
document =
    Mark.manyOf
        [ code
        , Mark.map (Html.p []) text
        ]
        |> Mark.document identity


text : Mark.Block (List (Html msg))
text =
    Mark.textWith
        { view =
            \styles string ->
                viewText styles string
        , replacements = Mark.commonReplacements
        , inlines =
            [ Mark.annotation "link"
                (\texts url ->
                    Html.a [ Attr.href url ] (List.map (\( left, right ) -> viewText left right) texts)
                )
                |> Mark.field "url" Mark.string
            ]
        }


viewText : Mark.Styles -> String -> Html msg
viewText styles string =
    if styles.bold || styles.italic || styles.strike then
        Html.span
            [ Attr.classList
                [ ( "bold", styles.bold )
                , ( "italic", styles.italic )
                , ( "strike", styles.strike )
                ]
            ]
            [ Html.text string ]

    else
        Html.text string


code : Mark.Block (Html msg)
code =
    Mark.block "Code"
        (\str -> Html.pre [] [ Html.text str ])
        Mark.string
