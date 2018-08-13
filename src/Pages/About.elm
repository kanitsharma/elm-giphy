module Pages.About exposing (Model, init, view, update, Msg)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode


type alias Model =
    { showImgLoader : Bool
    , imgSrc : String
    }


init : Model
init =
    Model True "https://upload.wikimedia.org/wikipedia/commons/thumb/f/f3/Elm_logo.svg/1200px-Elm_logo.svg.png"


type Msg
    = StopImgLoader


update : Msg -> Model -> Model
update msg model =
    case msg of
        StopImgLoader ->
            { model | showImgLoader = False }


onLoad : Msg -> Html.Attribute Msg
onLoad msg =
    on "load" << Decode.succeed <| msg


view : Model -> Bool -> Html Msg
view model transition =
    div
        [ class
            (case transition of
                True ->
                    "about_container animate"

                False ->
                    "about_container"
            )
        ]
        [ img
            [ src model.imgSrc
            , width 300
            , height 300
            , onLoad StopImgLoader
            , class
                (case model.showImgLoader of
                    True ->
                        "hide"

                    False ->
                        ""
                )
            ]
            []
        , (case model.showImgLoader of
            True ->
                div [ class "lds-hourglass center" ] []

            False ->
                div [ class "about_title" ] [ text "Made with Elm and ♥" ]
          )
        ]
