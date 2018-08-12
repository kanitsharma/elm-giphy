module Pages.About exposing (Model, init, view, update, Msg)

import Html exposing (..)
import Html.Attributes exposing (..)


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
            model


view : Model -> Html Msg
view model =
    div [ class "about_container" ]
        [ img [ src model.imgSrc, width 300, height 300 ] []
        , div [ class "about_title" ] [ text "Made with Elm and ♥" ]
        ]
