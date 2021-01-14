module Main exposing (main)

import Browser
import Element exposing (Element, rgb)
import Element.Background
import Element.Border as Border
import Palette


type Model
    = Model Lesson


type alias Lesson =
    { title : String
    , summary : String
    }


type Msg
    = Increment
    | Decrement


main : Program () Model Msg
main =
    Browser.sandbox
        { init =
            Model
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
        , view = view
        , update = update
        }


view model =
    Element.layout [ Element.Background.color Palette.pageBackground ]
        -- layout converts from Element to Html
        (lessonBox model)


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


update : Msg -> Model -> Model
update msg model =
    model
