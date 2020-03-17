module Markup exposing (..)

import Css
import Dict
import Errors exposing (Error, SourceLines)
import Html.Styled as Html exposing (Html, div, form, h1, h4, img, label, p, pre, span, styled, text, toUnstyled)
import Html.Styled.Attributes as Attr
import Mark
import Mark.Error
import Metadata exposing (ErrorMetadata, Metadata)
import Pages.Document


example =
    { code = -1
    , title = "Unhandled Error"
    , body = """
There was an error in your code.

|> Source
    label = Look, here it is:
    pos = 0

|> Source
    label = And again here:
    pos = 1

Please fix it. This is not a helpful error message.
"""
    , args = Dict.fromList [ ( "intelligence", "clever" ) ]
    , sources =
        [ { lines = Dict.fromList [ ( 0, "Source code position 0" ) ]
          , highlight =
                Just
                    { start = { row = 0, col = 0 }
                    , end = { row = 0, col = 3 }
                    }
          }
        , { lines = Dict.fromList [ ( 0, "Source code position 1" ) ]
          , highlight =
                Just
                    { start = { row = 0, col = 0 }
                    , end = { row = 0, col = 3 }
                    }
          }
        ]
    }


markupDocument : ( String, Pages.Document.DocumentHandler Metadata (Html msg) )
markupDocument =
    Pages.Document.parser
        { -- Ideally .emu but the file watch does not pick up changes.
          extension = "md"
        , metadata = Metadata.decoder
        , body =
            \src ->
                case Mark.compile (document example) src of
                    Mark.Success success ->
                        Html.div [] success |> Ok

                    Mark.Almost { result, errors } ->
                        viewErrors errors |> Html.text |> Ok

                    Mark.Failure errors ->
                        viewErrors errors |> Html.text |> Ok
        }


viewErrors : List Mark.Error.Error -> String
viewErrors errors =
    List.map Mark.Error.toString errors
        |> String.join "\n"



-- Structural formatting of error knowledge-base articles as elm-markup.


document : Error -> Mark.Document (List (Html msg))
document error =
    let
        render parts =
            case parts of
                ErrorDocs docs ->
                    docs

                Source src ->
                    htmlError (Debug.log "error" error)

                Params _ ->
                    Html.text ""
    in
    Mark.manyOf
        [ errorDocs
        , source
        , params
        ]
        |> Mark.document (List.map render)


type Parts msg
    = ErrorDocs (Html msg)
    | Source String
    | Params (List String)


errorDocs : Mark.Block (Parts msg)
errorDocs =
    Mark.textWith
        { view = htmlStyleText
        , replacements = Mark.commonReplacements
        , inlines = []
        }
        |> Mark.map htmlTextsToParagraph
        |> Mark.map ErrorDocs


source : Mark.Block (Parts msg)
source =
    Mark.string
        |> Mark.block "Source"
            Source


params : Mark.Block (Parts msg)
params =
    Mark.tree "Params" renderList Mark.string
        |> Mark.map Params


renderList (Mark.Enumerated list) =
    let
        renderItem (Mark.Item item) =
            String.join "" item.content
    in
    List.map renderItem list.items



-- Styling


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
