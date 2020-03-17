module Metadata exposing (ErrorMetadata, Metadata(..), decoder)

import Checker
import Data.Author
import Date exposing (Date)
import Dict exposing (Dict)
import Errors exposing (ErrorMessage)
import Json.Decode as Decode exposing (Decoder)
import L3
import List.Extra
import Pages
import Pages.ImagePath as ImagePath exposing (ImagePath)


type Metadata
    = Error ErrorMetadata
    | ErrorIndex


type alias ErrorMetadata =
    { code : Int
    , errorMsg : ErrorMessage
    }


errorCatalogue =
    Dict.empty
        |> Dict.union Checker.errorCatalogue
        |> Dict.union L3.errorCatalogue


defaultErrorMessage =
    { title = "Error Message Not Found"
    , body = "The error message being described could not be found."
    }


decoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\pageType ->
                case pageType of
                    "error-index" ->
                        Decode.succeed ErrorIndex

                    "error" ->
                        Decode.map
                            (\code ->
                                { code = code
                                , errorMsg = Dict.get code errorCatalogue |> Maybe.withDefault defaultErrorMessage
                                }
                            )
                            (Decode.field "code" Decode.int)
                            |> Decode.map Error

                    _ ->
                        Decode.fail <| "Unexpected page type " ++ pageType
            )


imageDecoder : Decoder (ImagePath Pages.PathKey)
imageDecoder =
    Decode.string
        |> Decode.andThen
            (\imageAssetPath ->
                case findMatchingImage imageAssetPath of
                    Nothing ->
                        Decode.fail "Couldn't find image."

                    Just imagePath ->
                        Decode.succeed imagePath
            )


findMatchingImage : String -> Maybe (ImagePath Pages.PathKey)
findMatchingImage imageAssetPath =
    Pages.allImages
        |> List.Extra.find
            (\image ->
                ImagePath.toString image
                    == imageAssetPath
            )
