module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Navigation


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias HomeModel =
    { searchText : String
    , gifs : List Gif
    , showLoader : Bool
    }


type alias AboutModel =
    { showImgLoader : Bool
    }


type Route
    = HomeRoute
    | AboutRoute


type alias Gif =
    { showImgLoader : Bool
    , url : String
    }


type alias Model =
    { route : Route
    , api_key : String
    , api_url : String
    , homeModel : HomeModel
    , aboutModel : AboutModel
    }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let
        api_url =
            "https://api.giphy.com/v1/gifs/search"

        api_key =
            "TFY6tJ4s3i9MtFhW897SLn2ydN2Wa2zS"

        searchText =
            "Dogs"
    in
        ( Model
            HomeRoute
            api_key
            api_url
            (HomeModel searchText [] True)
            (AboutModel False)
        , Cmd.map HomeMsg (fetchGifs api_url searchText api_key)
        )



-- UPDATE


type Msg
    = UrlChange Navigation.Location
    | HomeMsg HomeMsg
    | AboutMsg


type HomeMsg
    = UpdateText String
    | SearchGif
    | NewGifs (Result Http.Error (List Gif))
    | StopImgLoader Int String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        homeModel =
            model.homeModel

        aboutModel =
            model.aboutModel
    in
        case msg of
            UrlChange location ->
                ( { model | route = (getRoute location.hash) }, Cmd.none )

            HomeMsg homeMsg ->
                (case homeMsg of
                    UpdateText str ->
                        ( { model | homeModel = { homeModel | searchText = str } }, Cmd.none )

                    SearchGif ->
                        ( { model | homeModel = { homeModel | showLoader = True } }, Cmd.map HomeMsg (fetchGifs model.api_url homeModel.searchText model.api_key) )

                    NewGifs (Ok gifs) ->
                        ( { model | homeModel = { homeModel | gifs = gifs, showLoader = False } }
                        , Cmd.none
                        )

                    NewGifs (Err _) ->
                        ( model, Cmd.none )

                    StopImgLoader id a ->
                        ( { model
                            | homeModel =
                                { homeModel
                                    | gifs =
                                        List.indexedMap
                                            (\i x ->
                                                case i == id of
                                                    True ->
                                                        { x | showImgLoader = False }

                                                    False ->
                                                        x
                                            )
                                            homeModel.gifs
                                }
                          }
                        , Cmd.none
                        )
                )

            AboutMsg ->
                ( model, Cmd.none )


getRoute : String -> Route
getRoute hash =
    case hash of
        "#home" ->
            HomeRoute

        "#about" ->
            AboutRoute

        _ ->
            HomeRoute



--Commands


fetchGifs : String -> String -> String -> Cmd HomeMsg
fetchGifs api query key =
    let
        url =
            api ++ "?q=" ++ query ++ "&api_key=" ++ key
    in
        Http.send NewGifs (Http.get url <| decodeGifs)



--Decoders


decodeGifs : Decode.Decoder (List Gif)
decodeGifs =
    (Decode.list >> Decode.at [ "data" ])
        (Decode.map
            (Gif True)
            (Decode.at [ "images", "original", "url" ] Decode.string)
        )



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


inputSection : String -> Html HomeMsg
inputSection searchText =
    Html.form
        [ class "input_container", onWithOptions "submit" { stopPropagation = True, preventDefault = True } (Decode.succeed SearchGif) ]
        [ input
            [ type_ "text", placeholder "Search ", onInput UpdateText, value searchText ]
            []
        , input
            [ type_ "submit", Html.Attributes.value "Search", onClick SearchGif ]
            []
        ]


onLoadSrc : (String -> msg) -> Html.Attribute msg
onLoadSrc tagger =
    on "load" (Decode.map tagger targetSrc)


targetSrc : Decode.Decoder String
targetSrc =
    Decode.at [ "target", "src" ] Decode.string


gifSection : List Gif -> Html HomeMsg
gifSection gifs =
    div [ class "overflow_container" ]
        [ div [ class "gifs" ]
            (List.indexedMap
                (\i x ->
                    div
                        [ class "gif" ]
                        [ img
                            [ src x.url
                            , onLoadSrc (StopImgLoader i)
                            , class
                                (case x.showImgLoader of
                                    True ->
                                        "hide"

                                    False ->
                                        ""
                                )
                            ]
                            []
                        , case x.showImgLoader of
                            True ->
                                div [ class "lds-hourglass" ] []

                            False ->
                                div [] []
                        ]
                )
                gifs
            )
        ]


loaderSection : Bool -> Html HomeMsg
loaderSection showLoader =
    case showLoader of
        False ->
            div [] []

        True ->
            div [ class "loader" ]
                [ div
                    [ class "centered" ]
                    [ div [ class "blob-1" ] []
                    , div [ class "blob-2" ] []
                    ]
                ]


home : HomeModel -> Html HomeMsg
home model =
    div [ class "home_container" ]
        [ inputSection model.searchText
        , gifSection model.gifs
        , loaderSection model.showLoader
        ]


about : AboutModel -> Html Msg
about model =
    div [] [ text "About" ]


view : Model -> Html Msg
view model =
    div [ class "main_container" ]
        [ header
        , case model.route of
            HomeRoute ->
                Html.map HomeMsg (home model.homeModel)

            AboutRoute ->
                about model.aboutModel
        ]
