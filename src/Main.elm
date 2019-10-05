module Main exposing (..)

import Browser
import Html exposing (Html, a, button, div, form, h1, h2, img, label, li, p, text, textarea, ul)
import Html.Attributes exposing (for, href, id, rows, src, target, value, class)
import Html.Events exposing (onClick, onInput)
import Url.Builder exposing (crossOrigin, int, string)



---- CONSTANTS ----


maxMobileWaypoints =
    3


maxAbsoluteWaypoints =
    9


mobileWaypointsMessage =
    "Mobile browsers: more than 3 waypoints are truncated"


absoluteWaypointsMessage =
    "All browsers: more than 9 waypoints are truncated"


example =
    "St. Gallen - Zurich - Berne - Basel"



---- MODEL ----


type alias Model =
    { content : String
    }


type WarningNeeded
    = Yes (List String)
    | No


init : ( Model, Cmd Msg )
init =
    ( { content = "" }, Cmd.none )



---- UPDATE ----


type Msg
    = Change String
    | LoadExample


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Change content ->
            ( { model | content = content }, Cmd.none )

        LoadExample ->
            ( { model | content = example }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Google Maps Direction Parser" ]
        , p [] [ text "Enter a list of place names, each separated with a space and a hyphen." ]
        , button [ onClick LoadExample ] [ text "Add example" ]
        , form []
            [ label [ for "content" ] [ text "Place List" ]
            , textarea [ id "content", rows 5, onInput Change, value model.content ] [ ]
            ]
        , link (splitter model)
        , validation (warning model) model
        ]


validation : WarningNeeded -> Model -> Html msg
validation warningNeeded model =
    case warningNeeded of
        Yes list ->
            div []
                [ h1 [] [ text "Google API Limitation" ]
                , p []
                    [ text "You have "
                    , text (String.fromInt (List.length (splitter model)))
                    , text " waypoints"
                    ]
                , ul [] (List.map (\l -> li [] [ text l ]) list)
                ]

        No ->
            text ""


link : List String -> Html msg
link list =
    a [ class "button", href (linkTarget list), target "_blank" ] [ text "Go" ]



---- PROGRAM ----


splitter : Model -> List String
splitter model =
    String.split " - " model.content


warning : Model -> WarningNeeded
warning model =
    if List.length (splitter model) > maxMobileWaypoints then
        if List.length (splitter model) > maxAbsoluteWaypoints then
            Yes [ mobileWaypointsMessage, absoluteWaypointsMessage ]

        else
            Yes [ mobileWaypointsMessage ]

    else
        No


linkTarget : List String -> String
linkTarget list =
    crossOrigin "https://www.google.com"
        [ "maps", "dir", "" ]
        [ int "api" 1
        , linkOrigin list
        , linkWaypoints list
        ]


linkOrigin : List String -> Url.Builder.QueryParameter
linkOrigin list =
    case list of
        [] ->
            string "origin" ""

        origin :: _ ->
            string "origin" origin


linkWaypoints : List String -> Url.Builder.QueryParameter
linkWaypoints list =
    case list of
        [] ->
            string "waypoints" ""

        _ :: waypoints ->
            string "waypoints" (String.join "|" waypoints)


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
