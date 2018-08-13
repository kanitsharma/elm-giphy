module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Navigation
import Pages.Home as Home
import Pages.About as About
import Time exposing (..)
import Process as Process
import Task as Task


main : Program Never Model Msg
main =
    Navigation.program TriggerAnimation
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type Route
    = HomeRoute
    | AboutRoute


type alias Model =
    { route : Route
    , homeModel : Home.Model
    , aboutModel : About.Model
    , transition : Bool
    }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let
        ( initHomeModel, initSubCmd ) =
            Home.init
    in
        ( Model HomeRoute initHomeModel About.init False, Cmd.map HomeMsg initSubCmd )



-- UPDATE


type Msg
    = TriggerAnimation Navigation.Location
    | HomeMsg Home.Msg
    | AboutMsg About.Msg
    | ChangeUrlMsg Route


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TriggerAnimation location ->
            ( { model | transition = True }, delay 200 (ChangeUrlMsg <| (getRoute location.hash)) )

        ChangeUrlMsg route ->
            ( { model | route = route, transition = False }, Cmd.none )

        HomeMsg homeMsg ->
            let
                ( subModel, subCmd ) =
                    Home.update homeMsg model.homeModel
            in
                ( { model | homeModel = subModel }, Cmd.map HomeMsg subCmd )

        AboutMsg aboutMsg ->
            ( { model | aboutModel = About.update aboutMsg model.aboutModel }, Cmd.none )


getRoute : String -> Route
getRoute hash =
    case hash of
        "#home" ->
            HomeRoute

        "#about" ->
            AboutRoute

        _ ->
            HomeRoute



-- Side Effects


delay : Time.Time -> Msg -> Cmd Msg
delay time msg =
    Process.sleep time
        |> Task.andThen (always <| Task.succeed msg)
        |> Task.perform identity



-- VIEW


header : Html Msg
header =
    div
        [ class "header_container" ]
        [ div
            []
            [ text "Elm-giphy" ]
        , div
            [ class "right links_container" ]
            [ viewLink "home", viewLink "about" ]
        ]


viewLink : String -> Html Msg
viewLink name =
    li [] [ a [ href ("#" ++ name), class "link" ] [ text <| String.toUpper <| name ] ]


view : Model -> Html Msg
view model =
    div [ class "main_container" ]
        [ header
        , case model.route of
            HomeRoute ->
                Html.map HomeMsg (Home.view model.homeModel model.transition)

            AboutRoute ->
                Html.map AboutMsg (About.view model.aboutModel model.transition)
        ]
