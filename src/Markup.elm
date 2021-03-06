module Markup exposing (..)

import Css
import Dict
import Errors exposing (Error, ErrorMessage)
import Html.Styled as Html exposing (Html, div, form, h1, h4, img, label, p, pre, span, styled, text, toUnstyled)
import Html.Styled.Attributes as Attr
import Json.Decode as Decode exposing (Decoder)
import Mark
import Mark.Error
import Metadata exposing (ErrorMetadata, Metadata)
import Pages.Document
import SourcePos exposing (Region, RowCol, SourceLines)
import Structure exposing (View)


markupDocument : ( String, Pages.Document.DocumentHandler Metadata (View msg) )
markupDocument =
    Pages.Document.parser
        { -- Ideally .emu but the file watch does not pick up changes.
          extension = "md"
        , metadata = Metadata.decoder
        , body =
            \src ->
                case Mark.compile document src of
                    Mark.Success success ->
                        success |> Ok

                    Mark.Almost { result, errors } ->
                        viewErrors errors |> Err

                    Mark.Failure errors ->
                        viewErrors errors |> Err
        }



-- Structural formatting of error knowledge-base articles as elm-markup.


document : Mark.Document (ErrorMessage -> Int -> List (Html msg))
document =
    Mark.manyOf
        [ errorDocs
        , quoteError
        ]
        |> Mark.document (\parts errMsg code -> List.map (\part -> part errMsg code) parts)


errorDocs : Mark.Block (ErrorMessage -> Int -> Html msg)
errorDocs =
    Mark.textWith
        { view = htmlStyleText
        , replacements = Mark.commonReplacements
        , inlines = []
        }
        |> Mark.map htmlTextsToParagraph
        |> Mark.map (\doc _ _ -> doc)


quoteError : Mark.Block (ErrorMessage -> Int -> Html msg)
quoteError =
    let
        decodeFields src prms pos =
            { src = src, params = prms, highlights = pos }

        renderFields errMsg code src prms pos =
            let
                paramsDict =
                    Decode.decodeString (Decode.dict Decode.string) prms
                        |> Result.withDefault Dict.empty

                regions =
                    Decode.decodeString (Decode.list posDecoder) pos
                        |> Result.withDefault []

                lines =
                    String.split "\n" src
                        |> List.indexedMap Tuple.pair
                        |> Dict.fromList
            in
            htmlError
                { code = code
                , title = errMsg.title
                , body = errMsg.body
                , args = paramsDict
                , sources = SourcePos.sourceLinesForRegions lines regions
                }
    in
    Mark.record "Error"
        decodeFields
        |> Mark.field "code" Mark.string
        |> Mark.field "params" Mark.string
        |> Mark.field "pos" Mark.string
        |> Mark.toBlock
        |> Mark.map (\doc errMsg code -> renderFields errMsg code doc.src doc.params doc.highlights)


posDecoder : Decoder Region
posDecoder =
    Decode.map4
        (\r1 c1 r2 c2 ->
            { start = { row = r1, col = c1 }
            , end = { row = r2, col = c2 }
            }
        )
        (Decode.field "r1" Decode.int)
        (Decode.field "c1" Decode.int)
        (Decode.field "r2" Decode.int)
        (Decode.field "c2" Decode.int)



-- Styling


viewErrors : List Mark.Error.Error -> String
viewErrors errors =
    List.map Mark.Error.toString errors
        |> String.join "\n"


htmlError : Error -> Html msg
htmlError error =
    case Mark.compile (Errors.document Errors.htmlRenderer error) error.body of
        Mark.Success success ->
            Html.styled Html.div
                [ Css.borderLeft3 (Css.px 4) Css.solid (Css.rgb 0 0 0)
                , Css.paddingLeft (Css.px 10)
                ]
                []
                success

        Mark.Almost { result, errors } ->
            viewErrors errors |> Html.text

        Mark.Failure errors ->
            viewErrors errors |> Html.text


htmlStyleText : Mark.Styles -> String -> Html msg
htmlStyleText styles string =
    if styles.bold || styles.italic || styles.strike then
        Html.styled Html.span
            [ Css.fontStyle Css.italic ]
            [ Attr.classList
                [ ( "bold", styles.bold )
                , ( "italic", styles.italic )
                , ( "strike", styles.strike )
                ]
            ]
            [ Html.text string ]

    else
        Html.text string


htmlTextsToParagraph : List (Html msg) -> Html msg
htmlTextsToParagraph texts =
    Html.p [] texts
