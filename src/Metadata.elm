module Metadata exposing (ErrorMetadata, Metadata(..), decoder)

import Data.Author
import Date exposing (Date)
import Dict exposing (Dict)
import Errors exposing (Error)
import Json.Decode as Decode exposing (Decoder)
import List.Extra
import Pages
import Pages.ImagePath as ImagePath exposing (ImagePath)


type Metadata
    = Error ErrorMetadata
    | ErrorIndex


type alias ErrorMetadata =
    { title : String
    , published : Date
    , code : Int
    , error : Error
    }


errorCatalogue =
    Dict.empty


decoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\pageType ->
                case pageType of
                    "error-index" ->
                        Decode.succeed ErrorIndex

                    "error" ->
                        Decode.map4 ErrorMetadata
                            (Decode.field "title" Decode.string)
                            (Decode.field "published"
                                (Decode.string
                                    |> Decode.andThen
                                        (\isoString ->
                                            case Date.fromIsoString isoString of
                                                Ok date ->
                                                    Decode.succeed date

                                                Err error ->
                                                    Decode.fail error
                                        )
                                )
                            )
                            (Decode.succeed 0)
                            (Decode.succeed Errors.defaultError)
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
