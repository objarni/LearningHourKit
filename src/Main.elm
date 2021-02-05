module Main exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Element exposing (Element, rgb)
import Element.Background
import Element.Border as Border
import Element.Font as Font
import Html.Attributes
import Palette
import Url exposing (Url)
import Url.Parser as UrlP exposing ((</>))


type alias Model =
    { navKey : Nav.Key
    , page : Page
    , lessonLookup : Dict String Lesson
    }


type Page
    = LessonId String
    | LandingPage


type alias Lesson =
    { title : String
    , summary : String
    , sections : List Section
    }


type alias Section =
    { header : String
    , body : List Segment
    }


type Segment
    = Paragraph String
    | BulletList (List String)


type Msg
    = ChangeUrl Url
    | ClickLink UrlRequest


tddLesson : Lesson
tddLesson =
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
    , sections =
        [ { header = "Session Outline"
          , body =
                [ BulletList
                    [ "10 min connect: divide into pairs, write down 3 benefits of TDD"
                    , "15 min concept: demo leap years"
                    , "20 min do: leap years in pairs"
                    , "10 min reflect: summary of main idea"
                    ]
                ]
          }
        , { header = "Connect"
          , body =
                [ Paragraph """
Have everyone stand up and
stand at one side of the room.
Ask them to walk a few steps towards the other side of the
room in proportion to their answers to these questions. More
confidence means they should walk further:"""
                , BulletList
                    [ "confidence pair programming"
                    , "confidence with unit testing"
                    , "confidence with Test-Driven Development"
                    ]
                ]
          }
        ]
    }


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url navKey =
    ( { navKey = navKey
      , page = LandingPage
      , lessonLookup = Dict.fromList [ ( "tdd-introduction", tddLesson ) ]
      }
    , Cmd.none
    )


lessonRoute : UrlP.Parser (Page -> a) a
lessonRoute =
    UrlP.map LessonId (UrlP.s "lesson" </> UrlP.string)


parseUrl : String -> Page
parseUrl string =
    case Url.fromString string of
        Nothing ->
            LandingPage

        Just url ->
            Maybe.withDefault LandingPage (UrlP.parse lessonRoute url)


ideaPadLink =
    Element.link
        [ Element.alignBottom
        , Element.alignRight
        , Element.htmlAttribute (Html.Attributes.target "_blank")
        ]
        { url = "https://docs.google.com/document/d/17aUn98u1xRURT7ifPApN633IDSlXLqJMecwCh3Z_fok/edit"
        , label = Element.text "Idea Scratch Pad"
        }


view : Model -> Document msg
view { navKey, page, lessonLookup } =
    case page of
        LandingPage ->
            { title = "Olof's Learning Hour Kit"
            , body =
                [ Element.layout
                    [ Element.Background.color Palette.pageBackground
                    , Element.inFront ideaPadLink
                    ]
                    (Element.text "LANDING PAGE")
                ]
            }

        LessonId lessonId ->
            let
                lesson =
                    case Dict.get lessonId lessonLookup of
                        Just data ->
                            data

                        Nothing ->
                            Debug.todo "better modelling"
            in
            { title = "Learning Hour Kit"
            , body =
                [ Element.layout
                    [ Element.Background.color Palette.pageBackground
                    , Element.inFront ideaPadLink
                    ]
                    -- layout converts from Element to Html
                    (lessonBox lesson)
                ]
            }


paraE : String -> Element a
paraE s =
    Element.paragraph [] [ Element.text s ]


headerE : String -> Element a
headerE text =
    Element.el
        [ Font.size 24
        , Font.family
            [ Font.typeface "Open Sans"
            , Font.sansSerif
            ]
        , Element.paddingEach { top = 15, bottom = 8, right = 0, left = 0 }
        ]
        (Element.text text)


lessonBox : Lesson -> Element a
lessonBox lesson =
    let
        summaryE =
            paraE lesson.summary

        titleE =
            headerE lesson.title

        renderSegment : Segment -> Element a
        renderSegment segment =
            case segment of
                Paragraph text ->
                    paraE text

                BulletList bullets ->
                    let
                        bulletedItems =
                            List.map paraE (List.map (\s -> "* " ++ s) bullets)
                    in
                    Element.column [ Element.paddingXY 5 0 ] bulletedItems

        renderBody : List Segment -> Element a
        renderBody segments =
            Element.column []
                (List.map renderSegment segments)

        renderSection : Section -> List (Element a)
        renderSection { header, body } =
            [ headerE header, renderBody body ]

        renderSections ss =
            List.concatMap renderSection ss

        navigateE =
            Element.row [ Element.alignBottom, Element.centerX ] [ Element.text "<", Element.text ">" ]

        lessonDesc =
            Element.column
                [ Element.centerX, Element.padding 40 ]
                [ Element.column [] ([ titleE, summaryE ] ++ renderSections lesson.sections) ]
    in
    Element.el
        [ Element.centerX -- these two centers the div
        , Element.centerY
        , Border.color (rgb 0 0.7 0)
        , Element.Background.color Palette.contentBackground
        , Element.width (Element.px 800)
        ]
        (Element.column []
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
