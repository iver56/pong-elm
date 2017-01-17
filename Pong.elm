import Html exposing (..)

type alias Model =
    {}

init : Model
init =
    {}

type Msg = Tick

update : Msg -> Model -> Model
update msg model =
    case msg of
        Tick ->
            model

view : Model -> Html Msg
view model =
    text "Hello Pong!"

main =
    Html.beginnerProgram
        { model = init
        , update = update
        , view = view
        }
