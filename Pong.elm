import Html exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)


type alias Model =
    { ball : Ball
    , paddleLeft : Paddle
    , paddleRight : Paddle
    }

init : Model
init =
    { ball = initBall
    , paddleLeft = initPaddle 20
    , paddleRight = initPaddle (boardWidth - 25)
    }

type Msg = Tick

update : Msg -> Model -> Model
update msg model =
    case msg of
        Tick ->
            model


view : Model -> Html Msg
view model =
    svg
        [ width (toString boardWidth)
        , height (toString boardHeight)
        ]
        [ rect
            [ width (toString boardWidth)
            , height (toString boardHeight)
            , fill "black"
            ]
            []
        , ballView model.ball
        , paddleView model.paddleLeft
        , paddleView model.paddleRight
        ]

main =
    Html.beginnerProgram
        { model = init
        , update = update
        , view = view
        }

boardWidth =
    500

boardHeight =
    300

type alias Ball =
    { x : Float
    , y : Float
    , vx : Float
    , vy : Float
    , radius : Float
    }

type alias Paddle =
    { x : Float
    , y : Float
    , vx : Float
    , vy : Float
    , width : Float
    , height : Float
    }

initBall : Ball
initBall =
    { x = boardWidth / 2
    , y = boardHeight / 2
    , vx = 0.3
    , vy = 0.3
    , radius = 8
    }

initPaddle : Float -> Paddle
initPaddle x =
    { x = x
    , y = 0
    , vx = 0.4
    , vy = 0.4
    , width = 5
    , height = 80
    }


ballView : Ball -> Svg Msg
ballView model =
    circle
        [ cx (toString model.x)
        , cy (toString model.y)
        , r (toString model.radius)
        , fill "white"
        ]
        []

paddleView : Paddle -> Svg Msg
paddleView model =
    rect
        [ width (toString model.width)
        , height (toString model.height)
        , x (toString model.x)
        , y (toString model.y)
        , fill "white"
        ]
        []
