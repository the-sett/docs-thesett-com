module Markup exposing (..)

import Css
import Dict
import Errors exposing (Error)
import Html.Styled as Html exposing (Html, div, form, h1, h4, img, label, p, pre, span, styled, text, toUnstyled)
import Html.Styled.Attributes as Attr
import Mark
import Mark.Error
import Metadata exposing (ErrorMetadata, Metadata)
import Pages.Document


markupDocument : ( String, Pages.Document.DocumentHandler Metadata (Html msg) )
markupDocument =
    let
        error =
            { code = -1
            , title = Errors.rudeExampleErrorMessage.title
            , body = Errors.rudeExampleErrorMessage.body
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
    in
    Pages.Document.parser
        { -- Ideally .emu but the file watch does not pick up changes.
          extension = "md"
        , metadata = Metadata.decoder
        , body =
            \source ->
                case Mark.compile (Errors.document Errors.htmlRenderer error) source of
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
