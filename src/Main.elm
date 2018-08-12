module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Html.Lazy exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init model
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


init : Model -> ( Model, Cmd Msg )
init { api_url, searchText, api_key } =
    ( model, fetchGifs api_url searchText api_key )



-- MODEL


type alias Model =
    { searchText : String
    , gifs : List Gif
    , api_key : String
    , api_url : String
    , showLoader : Bool
    }


type alias Gif =
    { showImgLoader : Bool
    , id : String
    , url : String
    }


model : Model
model =
    Model "Dogs" [] "TFY6tJ4s3i9MtFhW897SLn2ydN2Wa2zS" "https://api.giphy.com/v1/gifs/search" False



-- UPDATE


type Msg
    = UpdateText String
    | SearchGif
    | NewGifs (Result Http.Error (List Gif))
    | StopImgLoader String String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateText str ->
            ( { model | searchText = str }, Cmd.none )

        SearchGif ->
            ( { model | showLoader = True }, fetchGifs model.api_url model.searchText model.api_key )

        NewGifs (Ok gifs) ->
            ( { model
                | gifs = gifs
                , showLoader = False
              }
            , Cmd.none
            )

        NewGifs (Err _) ->
            ( model, Cmd.none )

        StopImgLoader id a ->
            ( { model
                | gifs =
                    List.map
                        (\x ->
                            case x.id == id of
                                True ->
                                    { x | showImgLoader = False }

                                False ->
                                    x
                        )
                        model.gifs
              }
            , Cmd.none
            )



--Commands


fetchGifs : String -> String -> String -> Cmd Msg
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
        (Decode.map2
            (Gif True)
            (Decode.at [ "id" ] Decode.string)
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
        ]


inputSection : Model -> Html Msg
inputSection model =
    Html.form
        [ class "input_container", onWithOptions "submit" { stopPropagation = True, preventDefault = True } (Decode.succeed SearchGif) ]
        [ input
            [ type_ "text", placeholder "Search ", onInput UpdateText, value model.searchText ]
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


gifSection : List Gif -> Html Msg
gifSection gifs =
    div [ class "overflow_container" ]
        [ div [ class "gifs" ]
            (List.map
                (\x ->
                    div
                        [ class "gif" ]
                        [ img
                            [ src x.url
                            , onLoadSrc (StopImgLoader x.id)
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


loaderSection : Bool -> Html Msg
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


view : Model -> Html.Html Msg
view model =
    div [ class "main_container" ]
        [ header
        , inputSection model
        , lazy gifSection model.gifs
        , lazy loaderSection model.showLoader
        ]
