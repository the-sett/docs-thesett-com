module Markup exposing (..)

import Css
import Errors exposing (Error(..))
import Html.Styled as Html exposing (Html, div, form, h1, h4, img, label, p, pre, span, styled, text, toUnstyled)
import Html.Styled.Attributes as Attr
import Mark
import Mark.Error
import Metadata exposing (ErrorMetadata, Metadata)
import Pages.Document


markupDocument : ( String, Pages.Document.DocumentHandler Metadata (Html msg) )
markupDocument =
    Pages.Document.parser
        { -- Ideally .emu but the file watch does not pick up changes.
          extension = "md"
        , metadata = Metadata.decoder
        , body =
            \source ->
                case Mark.compile (Errors.document Errors.htmlRenderer Errors.defaultError) source of
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
