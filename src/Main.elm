module Main exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Element exposing (Element, rgb)
import Element.Background
import Element.Border as Border
import Html.Attributes
import Palette
import Url exposing (Url)


type Model
    = Model Lesson


type alias Lesson =
    { title : String
    , summary : String
    }


type Msg
    = ChangeUrl Url
    | ClickLink UrlRequest


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url navKey =
    ( Model
        { title =
            "Using TDD to write a Leap Years function"
        , summary =
            """
This is often the first exercise I do with new teams.
You have to TDD a function that takes an integer argument
and returns a boolean. It ends up being a rather small piece
of code, just enough to show a few TDD cycles.

The first time I demonstrate this kata I usually don’t show
triangulation, and only do the four test cases listed in the
problem description. That means I go straight to using the
modulo operator when implementing the first test case. That
keeps the demo shorter, and means you don’t need to explain
triangulation as a concept.
"""
        }
    , Cmd.none
    )


view : Model -> Document msg
view model =
    let
        ideaPadLink =
            Element.link
                [ Element.alignBottom
                , Element.alignRight
                , Element.htmlAttribute (Html.Attributes.target "_blank")
                ]
                { url = "https://docs.google.com/document/d/17aUn98u1xRURT7ifPApN633IDSlXLqJMecwCh3Z_fok/edit"
                , label = Element.text "Idea Scratch Pad"
                }
    in
    { title = "Learning Hour Kit"
    , body =
        [ Element.layout
            [ Element.Background.color Palette.pageBackground
            , Element.inFront ideaPadLink
            ]
            -- layout converts from Element to Html
            (lessonBox model)
        ]
    }


lessonBox : Model -> Element a
lessonBox (Model lesson) =
    let
        summaryE =
            Element.paragraph [] [ Element.text lesson.summary ]

        titleE =
            Element.text lesson.title

        navigateE =
            Element.row [ Element.alignBottom, Element.centerX ] [ Element.text "<", Element.text ">" ]

        lessonDesc =
            Element.column
                -- these two centers the Element.text in div
                [ Element.centerX, Element.padding 40 ]
                [ Element.column [] [ titleE, summaryE ] ]
    in
    Element.el
        [ Element.centerX -- these two centers the div
        , Element.centerY
        , Border.color (rgb 0 0.7 0)
        , Element.Background.color Palette.contentBackground
        , Element.width (Element.px 800)
        ]
        (Element.column [ Element.height (Element.px 600) ]
            [ lessonDesc
            , navigateE
            ]
        )


update : Msg -> Model -> ( Model, Cmd a )
update msg model =
    ( model, Cmd.none )


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , subscriptions = \_ -> Sub.none
        , update = update
        , onUrlRequest = ClickLink
        , onUrlChange = ChangeUrl
        }
