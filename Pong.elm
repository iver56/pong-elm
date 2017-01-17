module Main exposing (..)

import Html exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import AnimationFrame


boardWidth =
    500


boardHeight =
    300



-- MODEL


type alias Model =
    { ball : Ball
    , paddleLeft : Paddle
    , paddleRight : Paddle
    }


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


init : ( Model, Cmd Msg )
init =
    ( { ball = initBall
      , paddleLeft = initPaddle 20
      , paddleRight = initPaddle (boardWidth - 25)
      }
    , Cmd.none
    )


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



-- UPDATE


type Msg
    = Tick Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick time ->
            ( model, Cmd.none )



-- VIEW


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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    AnimationFrame.diffs Tick



-- MAIN


main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
