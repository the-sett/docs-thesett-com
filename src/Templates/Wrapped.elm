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
import Structure exposing (StaticPage, StaticView, Template)
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
        Metadata.Page meta ->
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
                , title = meta.title
                }
                |> Seo.website

        Metadata.Article meta ->
            Seo.summaryLarge
                { canonicalUrlOverride = Nothing
                , siteName = "www.thesett.com"
                , image =
                    { url = images.iconPng
                    , alt = meta.title
                    , dimensions = Nothing
                    , mimeType = Nothing
                    }
                , description = meta.title
                , locale = Nothing
                , title = meta.title
                }
                |> Seo.article
                    { tags = []
                    , section = Nothing
                    , publishedTime = Just (Date.toIsoString meta.published)
                    , modifiedTime = Nothing
                    , expirationTime = Nothing
                    }

        Metadata.Author meta ->
            let
                ( firstName, lastName ) =
                    case meta.name |> String.split " " of
                        [ first, last ] ->
                            ( first, last )

                        [ first, middle, last ] ->
                            ( first ++ " " ++ middle, last )

                        [] ->
                            ( "", "" )

                        _ ->
                            ( meta.name, "" )
            in
            Seo.summary
                { canonicalUrlOverride = Nothing
                , siteName = "www.thesett.com"
                , image =
                    { url = meta.avatar
                    , alt = meta.name ++ "'s articles."
                    , dimensions = Nothing
                    , mimeType = Nothing
                    }
                , description = meta.bio
                , locale = Nothing
                , title = meta.name ++ "'s articles."
                }
                |> Seo.profile
                    { firstName = firstName
                    , lastName = lastName
                    , username = Nothing
                    }

        Metadata.BlogIndex ->
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
        Metadata.Page metadata ->
            metadata.title

        Metadata.Article metadata ->
            metadata.title

        Metadata.Author author ->
            author.name

        Metadata.BlogIndex ->
            "thesett knowledge articles"


pageView :
    ResponsiveStyle
    -> List ( PagePath Pages.PathKey, Metadata )
    -> { path : PagePath Pages.PathKey, frontmatter : Metadata }
    -> Model
    -> Html Msg
    -> Html Msg
pageView responsiveStyle siteMetadata page model viewForPage =
    case page.frontmatter of
        Metadata.Page metadata ->
            viewForPage

        Metadata.Article metadata ->
            div []
                [ titleView responsiveStyle metadata.title
                , viewForPage
                ]

        Metadata.Author author ->
            viewForPage

        Metadata.BlogIndex ->
            --, Index.view siteMetadata
            div [] []


titleView : ResponsiveStyle -> String -> Html msg
titleView responsiveStyle val =
    styled div
        [ Css.textAlign Css.center ]
        []
        [ h1 [] [ text val ] ]


articleImageView : ResponsiveStyle -> ImagePath Pages.PathKey -> Html msg
articleImageView responsiveStyle articleImage =
    styled img
        [ Css.pct 100 |> Css.width ]
        [ ImagePath.toString articleImage |> src
        ]
        []
