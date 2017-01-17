module Main exposing (..)

import Html exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import AnimationFrame
import Keyboard exposing (..)
import Set exposing (Set)


boardWidth =
    500


boardHeight =
    300



-- MODEL


type alias Model =
    { ball : Ball
    , paddleLeft : Paddle
    , paddleRight : Paddle
    , keysDown : Set KeyCode
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
      , keysDown = Set.empty
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
    | KeyUp KeyCode
    | KeyDown KeyCode


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyDown key ->
            ( { model | keysDown = Set.insert key model.keysDown }, Cmd.none )

        KeyUp key ->
            ( { model | keysDown = Set.remove key model.keysDown }, Cmd.none )

        Tick delta ->
            ( { model
                | ball = updateBall delta model.paddleLeft model.paddleRight model.ball
                , paddleLeft = updatePaddle (paddleDirectionLeft model.keysDown) delta model.paddleLeft
                , paddleRight = updatePaddle (paddleDirectionRight model.keysDown) delta model.paddleRight
              }
            , Cmd.none
            )


updatePaddle : Int -> Float -> Paddle -> Paddle
updatePaddle direction delta paddle =
    { paddle
        | y =
            clamp 0
                (boardHeight - paddle.height)
                (paddle.y + (paddle.vy * (toFloat direction)) * delta)
    }


updateBall : Float -> Paddle -> Paddle -> Ball -> Ball
updateBall delta paddleLeft paddleRight ball =
    if ball.x < -ball.radius || ball.x > boardWidth + ball.radius then
        { ball
            | x = boardWidth / 2
            , y = boardHeight / 2
        }
    else
        let
            vx =
                if within ball paddleLeft then
                    abs ball.vx
                else if within ball paddleRight then
                    -(abs ball.vx)
                else
                    ball.vx

            vy =
                if ball.y < ball.radius then
                    abs ball.vy
                else if ball.y > boardHeight - ball.radius then
                    -(abs ball.vy)
                else
                    ball.vy
        in
            { ball
                | x = ball.x + vx * delta
                , y = ball.y + vy * delta
                , vx = vx
                , vy = vy
            }


near : Float -> Float -> Float -> Bool
near a spacing b =
    b >= a - spacing && b <= a + spacing


within : Ball -> Paddle -> Bool
within ball paddle =
    near (paddle.x + paddle.width / 2) (paddle.width / 2 + ball.radius) ball.x
        && near (paddle.y + paddle.height / 2) (paddle.height / 2 + ball.radius) ball.y


paddleDirectionLeft : Set KeyCode -> Int
paddleDirectionLeft keysDown =
    if Set.member 87 keysDown then
        -1
    else if Set.member 83 keysDown then
        1
    else
        0


paddleDirectionRight : Set KeyCode -> Int
paddleDirectionRight keysDown =
    if Set.member 38 keysDown then
        -1
    else if Set.member 40 keysDown then
        1
    else
        0



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
    Sub.batch
        [ Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        , AnimationFrame.diffs Tick
        ]



-- MAIN


main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
