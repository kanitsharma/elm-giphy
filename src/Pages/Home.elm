module Pages.Home exposing (Model, init, view, update, Msg)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode


-- Model


type alias Gif =
    { showImgLoader : Bool
    , url : String
    }


type alias Model =
    { searchText : String
    , gifs : List Gif
    , showLoader : Bool
    , api_key : String
    , api_url : String
    }


init : ( Model, Cmd Msg )
init =
    let
        api_url =
            "https://api.giphy.com/v1/gifs/search"

        api_key =
            "TFY6tJ4s3i9MtFhW897SLn2ydN2Wa2zS"

        searchText =
            "Dogs"
    in
        ( Model searchText [] True api_key api_url, fetchGifs api_url searchText api_key )



-- Update


type Msg
    = UpdateText String
    | SearchGif
    | NewGifs (Result Http.Error (List Gif))
    | StopImgLoader Int String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateText str ->
            ( { model | searchText = str }, Cmd.none )

        SearchGif ->
            ( { model | showLoader = True }, fetchGifs model.api_url model.searchText model.api_key )

        NewGifs (Ok gifs) ->
            ( { model | gifs = gifs, showLoader = False }
            , Cmd.none
            )

        NewGifs (Err _) ->
            ( model, Cmd.none )

        StopImgLoader id a ->
            ( { model
                | gifs =
                    List.indexedMap
                        (\i x ->
                            case i == id of
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
        (Decode.map
            (Gif True)
            (Decode.at [ "images", "original", "url" ] Decode.string)
        )



-- View


inputSection : String -> Html Msg
inputSection searchText =
    Html.form
        [ onWithOptions "submit" { stopPropagation = True, preventDefault = True } (Decode.succeed SearchGif) ]
        [ div [ class "input_container" ]
            [ input
                [ type_ "text", placeholder "Search ", onInput UpdateText, value searchText ]
                []
            , input
                [ type_ "submit", Html.Attributes.value "Search", onClick SearchGif ]
                []
            ]
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


view : Model -> Html Msg
view model =
    div [ class "home_container" ]
        [ inputSection model.searchText
        , gifSection model.gifs
        , loaderSection model.showLoader
        ]
