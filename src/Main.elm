module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Navigation
import Pages.Home as Home
import Pages.About as About


main : Program Never Model Msg
main =
    Navigation.program UrlChange
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
    }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let
        ( initHomeModel, initSubCmd ) =
            Home.init
    in
        ( Model HomeRoute initHomeModel About.init, Cmd.map HomeMsg initSubCmd )



-- UPDATE


type Msg
    = UrlChange Navigation.Location
    | HomeMsg Home.Msg
    | AboutMsg About.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        homeModal =
            model.homeModel

        aboutModel =
            model.aboutModel
    in
        case msg of
            UrlChange location ->
                ( { model | route = (getRoute location.hash) }, Cmd.none )

            HomeMsg homeMsg ->
                let
                    ( subModel, subCmd ) =
                        Home.update homeMsg model.homeModel
                in
                    ( { model | homeModel = subModel }, Cmd.map HomeMsg subCmd )

            AboutMsg aboutMsg ->
                ( { model | aboutModel = About.update aboutMsg aboutModel }, Cmd.none )


getRoute : String -> Route
getRoute hash =
    case hash of
        "#home" ->
            HomeRoute

        "#about" ->
            AboutRoute

        _ ->
            HomeRoute



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
                Html.map HomeMsg (Home.view model.homeModel)

            AboutRoute ->
                Html.map AboutMsg (About.view model.aboutModel)
        ]
