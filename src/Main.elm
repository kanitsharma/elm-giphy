module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


init : ( Model, Cmd Msg )
init =
    ( model, fetchGifs model.searchText )



-- MODEL


type alias Model =
    { searchText : String
    , gifs : List Gif
    , api_key : String
    , showLoader : Bool
    }


type alias Gifs =
    { gifs : List Gif
    }


type alias Gif =
    { showImgLoader : Bool
    , id : String
    , url : String
    }


model : Model
model =
    Model "Dogs" [] "TFY6tJ4s3i9MtFhW897SLn2ydN2Wa2zS" False



-- UPDATE


type Msg
    = UpdateText String
    | SearchGif
    | NewGifs (Result Http.Error Gifs)
    | StopImgLoader String String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateText str ->
            ( { model | searchText = str }, Cmd.none )

        SearchGif ->
            ( { model | showLoader = True }, fetchGifs model.searchText )

        NewGifs (Ok { gifs }) ->
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


fetchGifs : String -> Cmd Msg
fetchGifs tag =
    let
        url =
            "https://api.giphy.com/v1/gifs/search?q=" ++ tag ++ "&api_key=" ++ model.api_key
    in
        Http.send NewGifs (Http.get url <| decodeGifs)



--Decoders


decodeGifs : Decode.Decoder Gifs
decodeGifs =
    Decode.map Gifs (Decode.at [ "data" ] decodeGif)


decodeGif : Decode.Decoder (List Gif)
decodeGif =
    Decode.list
        (Decode.map2 (Gif True)
            (Decode.at [ "id" ] Decode.string)
            (Decode.at [ "images", "original", "url" ] Decode.string)
        )



-- VIEW


header : Model -> Html Msg
header model =
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


gifSection : Model -> Html Msg
gifSection model =
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
                model.gifs
            )
        ]


loaderSection : Model -> Html Msg
loaderSection model =
    case model.showLoader of
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
        [ header model
        , inputSection model
        , gifSection model
        , loaderSection model
        ]
