module Markup exposing (..)

import Css
import Errors exposing (Error(..))
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
                    Mark.Success success ->
                        Html.div [] (success Errors.defaultError) |> Ok

                    Mark.Almost { result, errors } ->
                        viewErrors errors |> Html.text |> Ok

                    Mark.Failure errors ->
                        viewErrors errors |> Html.text |> Ok
        }


viewErrors : List Mark.Error.Error -> String
viewErrors errors =
    List.map Mark.Error.toString errors
        |> String.join "\n"


document : Mark.Document (Error -> List (Html msg))
document =
    Mark.manyOf [ text ]
        |> Mark.document
            (\parts -> \val -> List.map (\part -> part val) parts)


text : Mark.Block (Error -> Html msg)
text =
    Mark.textWith
        { view = \styles string -> renderText ( styles, string )
        , replacements = Mark.commonReplacements
        , inlines =
            [ Mark.annotation "link"
                (\texts url ->
                    Html.a [ Attr.href url ] (List.map renderText texts)
                )
                |> Mark.field "url" Mark.string
            , Mark.annotation "source"
                (\styles errCode -> Html.text "blah")
                |> Mark.field "pos" Mark.int
            ]
        }
        |> Mark.map
            (\para -> \(Error err) -> Html.p [] (Html.text (err.title ++ " : ") :: para))


{-| Render text with basic markdown styling applied.
-}
renderText : ( Mark.Styles, String ) -> Html msg
renderText ( styles, string ) =
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
