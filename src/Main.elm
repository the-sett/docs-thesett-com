module Main exposing (..)

import Colors
import Css
import Css.Global
import Date
import Devices
import Head
import Head.Seo as Seo
import Html
import Html.Styled exposing (Html, div, toUnstyled)
import Layouts.Default
import Layouts.Zero
import Markup
import Metadata exposing (Metadata)
import Pages exposing (images, pages)
import Pages.Directory as Directory exposing (Directory)
import Pages.ImagePath as ImagePath exposing (ImagePath)
import Pages.Manifest as Manifest
import Pages.Manifest.Category
import Pages.PagePath as PagePath exposing (PagePath)
import Pages.Platform exposing (Page)
import Pages.StaticHttp as StaticHttp
import State
import Templates.Wrapped
import TheSett.Laf as Laf


main : Pages.Platform.Program Model Msg Metadata (Html Msg)
main =
    Pages.Platform.application
        { init = \_ -> init
        , subscriptions = subscriptions
        , update = update
        , onPageChange = \_ -> State.PageChanged
        , view = view
        , documents = [ Markup.markupDocument ]
        , manifest = manifest
        , canonicalSiteUrl = canonicalSiteUrl
        , internals = Pages.internals
        , generateFiles = \_ -> []
        }


type alias Model =
    State.Model


type alias Msg =
    State.Msg


init : ( Model, Cmd Msg )
init =
    ( { laf = Laf.init
      , debug = False
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


manifest : Manifest.Config Pages.PathKey
manifest =
    { backgroundColor = Nothing
    , categories = [ Pages.Manifest.Category.business ]
    , displayMode = Manifest.Standalone
    , orientation = Manifest.Portrait
    , description = "www-thesett-com Website for thesett.com"
    , iarcRatingId = Nothing
    , name = "thesett"
    , themeColor = Nothing
    , startUrl = pages.index
    , shortName = Just "thesett"
    , sourceIcon = images.iconPng
    }


canonicalSiteUrl : String
canonicalSiteUrl =
    "https://silly-spence-61a4c5.netlify.com/"


deviceConfig =
    Devices.devices


global : List Css.Global.Snippet
global =
    [ Css.Global.each
        [ Css.Global.html ]
        [ Css.backgroundColor Colors.paperWhite ]
    ]


view :
    List ( PagePath Pages.PathKey, Metadata )
    ->
        { path : PagePath Pages.PathKey
        , frontmatter : Metadata
        }
    ->
        StaticHttp.Request
            { view : Model -> Html Msg -> { title : String, body : Html.Html Msg }
            , head : List (Head.Tag Pages.PathKey)
            }
view siteMetadata page =
    let
        template =
            Layouts.Default.layout Templates.Wrapped.view deviceConfig siteMetadata page
    in
    { head = template.head
    , view =
        \model contentView ->
            let
                { title, body } =
                    template.view model contentView
            in
            { title = title
            , body =
                div []
                    [ Laf.fonts
                    , Laf.style deviceConfig
                    , Css.Global.global global
                    , body
                    ]
                    |> toUnstyled
            }
    }
        |> StaticHttp.succeed
