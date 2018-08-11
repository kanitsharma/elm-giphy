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
    ( model, Cmd.none )



-- MODEL


type alias Model =
    { searchText : String
    , gifs : Urls
    , api_key : String
    }


model : Model
model =
    Model "" ({ data = [] }) "TFY6tJ4s3i9MtFhW897SLn2ydN2Wa2zS"


type alias Urls =
    { data : List Url
    }


type alias Url =
    { url : Images
    }


type alias Images =
    { images : Image
    }


type alias Image =
    { url : String
    }


decodeGif : Decode.Decoder Urls
decodeGif =
    Decode.map Urls << Decode.field "data" <| decodeList


decodeList : Decode.Decoder (List Url)
decodeList =
    Decode.list decodeUrl


decodeUrl : Decode.Decoder Url
decodeUrl =
    Decode.map Url
        << Decode.field "images"
        << Decode.map Images
        << Decode.field "original_still"
        << Decode.map Image
        << Decode.field "url"
    <|
        Decode.string



-- UPDATE


type Msg
    = UpdateText String
    | SearchGif
    | NewGifs (Result Http.Error Urls)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateText str ->
            ( { model | searchText = str }, Cmd.none )

        SearchGif ->
            ( model, fetchGifs model.searchText )

        NewGifs (Ok urls) ->
            ( { model | gifs = urls }, Cmd.none )

        NewGifs (Err _) ->
            ( model, Cmd.none )



--Commands


fetchGifs : String -> Cmd Msg
fetchGifs tag =
    let
        url =
            "https://api.giphy.com/v1/gifs/search?q=" ++ tag ++ "&api_key=" ++ model.api_key
    in
        Http.send NewGifs << Http.get url <| decodeGif



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
    div
        [ class "input_container" ]
        [ input
            [ type_ "text", placeholder "search ", onInput UpdateText ]
            []
        , input
            [ type_ "submit", Html.Attributes.value "Search", onClick SearchGif ]
            []
        ]


gitSection : Model -> Html Msg
gitSection model =
    div [] (List.map (\x -> img [ src x.url.images.url ] []) model.gifs.data)


view : Model -> Html.Html Msg
view model =
    div [ class "main_container" ]
        [ header model
        , inputSection model
        , gitSection model
        ]
