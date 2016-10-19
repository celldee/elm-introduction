import Html exposing (Html, div, button, text)
import Html.Events exposing (onClick)
import Html.App as App
import Svg exposing (Svg, svg, circle, line, g)
import Svg.Attributes exposing (viewBox, width, cx, cy, r, fill, x1, y1, x2, y2, stroke)
import Time exposing (Time, second)
import Date exposing (Date)
import List exposing (map)
import Date.Extra.Create exposing (getTimezoneOffset)

main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type alias Model =
  { time : Time
  , running : Bool
  }

init : (Model, Cmd Msg)
init =
  (Model 0 True, Cmd.none)


-- UPDATE

type Msg
  = Tick Time
  | ToggleRunning

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      ({ model | time = newTime }, Cmd.none)
    ToggleRunning -> 
      ({ model | running = not model.running }, Cmd.none)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  if model.running then
    (Time.every second Tick)
  else
    Sub.none


-- VIEW

view : Model -> Html Msg
view model =
  let
    date =
      Date.fromTime model.time
    offset =
      getTimezoneOffset date
    secondAngle =
      turns ((toFloat (round (Time.inSeconds model.time))) / 60 - 0.25)
    secondHandX =
      handX 40 secondAngle
    secondHandY =
      handY 40 secondAngle
    minuteAngle =
      turns (((Time.inMinutes model.time) - (toFloat offset)) / 60 - 0.25)
    minuteHandX =
      handX 30 minuteAngle
    minuteHandY =
      handY 30 minuteAngle
    hourAngle =
      turns (((Time.inHours model.time) - ((toFloat offset) / 60)) / 12 - 0.25)
    hourHandX =
      handX 20 hourAngle
    hourHandY =
      handY 20 hourAngle
  in
    div []
      [ svg [ viewBox "0 0 100 100", width "300px" ]
        [ circle [ cx "50", cy "50", r "45", fill "#0B79CE" ] []
        , g [] (map (drawTick 2 (List.length smallTicks)) smallTicks)
        , g [] (map (drawTick 5 (List.length bigTicks)) bigTicks)
        , line [ x1 "50", y1 "50", x2 secondHandX, y2 secondHandY, stroke "#023963" ] []
        , line [ x1 "50", y1 "50", x2 minuteHandX, y2 minuteHandY, stroke "#023963" ] []
        , line [ x1 "50", y1 "50", x2 hourHandX, y2 hourHandY, stroke "#023963" ] []
        ]
      , div []
        [ button [onClick ToggleRunning] [text (if model.running then "pause" else "unpause")]
        ]
      , div []
        [ text (toString date)
        ]
      ]

drawTick : Int -> Int -> Int -> Svg Msg
drawTick length count position =
  let
    angle = turns ((toFloat position) / (toFloat count))
    vx1 = handX (toFloat (45 - length)) angle
    vx2 = handX 45 angle
    vy1 = handY (toFloat (45 - length)) angle
    vy2 = handY 45 angle
  in
    line [ x1 vx1, y1 vy1, x2 vx2, y2 vy2, stroke "#023963" ] []

smallTicks : List Int
smallTicks = [0..59]

bigTicks : List Int
bigTicks = [0..11]

handX : Float -> Float -> String
handX length angle =
  toString (50 + length * cos angle)

handY : Float -> Float -> String
handY length angle =
  toString (50 + length * sin angle)
