module Main exposing (main)

import Browser
import Element exposing (Element)
import Element.Background
import Html exposing (Html)


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
        { init = Model { title = "hejsan", summary = "lite tetx" }
        , view = view
        , update = update
        }


view model =
    Element.layout []
        -- layout converts from Element to Html
        lessonBox


lessonBox : Element a
lessonBox =
    let
        backgroundColor =
            Element.rgb255 225 225 225
    in
    Element.el
        [ Element.centerX -- these two centers the div
        , Element.centerY
        , Element.Background.color backgroundColor
        , Element.width (Element.px 800)
        , Element.height (Element.px 600)
        ]
        (Element.el
            -- these two centers the Element.text in div
            [ Element.centerX, Element.centerY ]
            (Element.text "CENTERED")
        )


update : Msg -> Model -> Model
update msg model =
    model
