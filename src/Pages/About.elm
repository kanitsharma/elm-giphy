module Pages.About exposing (Model, init, view, update, Msg)

import Html exposing (..)


type alias Model =
    { showImgLoader : Bool
    }


init : Model
init =
    Model True


type Msg
    = StopImgLoader


update : Msg -> Model -> Model
update msg model =
    case msg of
        StopImgLoader ->
            model


view : Model -> Html Msg
view model =
    div [] [ text "About" ]
