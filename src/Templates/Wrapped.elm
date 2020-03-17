module Templates.Wrapped exposing (view)

import Colors
import Css
import Date
import Grid
import Head
import Head.Seo as Seo
import Html.Styled exposing (Html, div, form, h1, h4, img, label, p, pre, span, styled, text, toUnstyled)
import Html.Styled.Attributes exposing (id, src)
import Html.Styled.Events exposing (onClick, onInput)
import Html.Styled.Lazy exposing (lazy2)
import Http
import Json.Decode as Decode
import Metadata exposing (Metadata)
import Pages exposing (images, pages)
import Pages.ImagePath as ImagePath exposing (ImagePath)
import Pages.PagePath as PagePath exposing (PagePath)
import Responsive exposing (ResponsiveStyle)
import State exposing (Model, Msg(..))
import Structure exposing (StaticPage, StaticView, Template, View)
import Styles exposing (lg, md, sm, xl)
import TheSett.Buttons as Buttons
import TheSett.Cards as Cards
import TheSett.Laf as Laf
import TheSett.Textfield as Textfield


view : Template Msg Model
view responsiveStyle siteMetadata page =
    { head = head page.frontmatter
    , view =
        \model contentView ->
            { title = title page.frontmatter
            , body =
                styled div
                    [ Laf.wrapper responsiveStyle ]
                    []
                    [ pageView responsiveStyle siteMetadata page model contentView ]
            }
    }


head : Metadata -> List (Head.Tag Pages.PathKey)
head metadata =
    case metadata of
        Metadata.Error meta ->
            Seo.summaryLarge
                { canonicalUrlOverride = Nothing
                , siteName = "www.thesett.com"
                , image =
                    { url = images.iconPng
                    , alt = "title"
                    , dimensions = Nothing
                    , mimeType = Nothing
                    }
                , description = "title"
                , locale = Nothing
                , title = "title"
                }
                |> Seo.article
                    { tags = []
                    , section = Nothing
                    , publishedTime = Nothing
                    , modifiedTime = Nothing
                    , expirationTime = Nothing
                    }

        Metadata.ErrorIndex ->
            Seo.summaryLarge
                { canonicalUrlOverride = Nothing
                , siteName = "www.thesett.com"
                , image =
                    { url = images.iconPng
                    , alt = "thesett logo"
                    , dimensions = Nothing
                    , mimeType = Nothing
                    }
                , description = siteTagline
                , locale = Nothing
                , title = "thesett articles"
                }
                |> Seo.website


siteTagline : String
siteTagline =
    "Knowledge articles for thesett.com"


title : Metadata -> String
title frontmatter =
    case frontmatter of
        Metadata.Error metadata ->
            "title"

        Metadata.ErrorIndex ->
            "error catalogue"


pageView :
    ResponsiveStyle
    -> List ( PagePath Pages.PathKey, Metadata )
    -> { path : PagePath Pages.PathKey, frontmatter : Metadata }
    -> Model
    -> View Msg
    -> Html Msg
pageView responsiveStyle siteMetadata page model viewForPage =
    case page.frontmatter of
        Metadata.Error metadata ->
            div [] (viewForPage metadata.errorMsg metadata.code)

        Metadata.ErrorIndex ->
            div [] []


articleImageView : ResponsiveStyle -> ImagePath Pages.PathKey -> Html msg
articleImageView responsiveStyle articleImage =
    styled img
        [ Css.pct 100 |> Css.width ]
        [ ImagePath.toString articleImage |> src
        ]
        []
