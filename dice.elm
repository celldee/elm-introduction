import Html exposing (..)
import Html.App as App
import Html.Events exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Random

main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type alias Model =
  { dieFaces : (Int, Int)
  }

type DotPosition
  = TopLeft
  | TopRight
  | MiddleLeft
  | MiddleCenter
  | MiddleRight
  | BottomLeft
  | BottomRight

-- UPDATE

type Msg
  = Roll
  | NewFace (Int, Int)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Roll ->
      (model, Random.generate NewFace (Random.pair (Random.int 1 6) (Random.int 1 6)))

    NewFace dieFaces ->
      ({ model | dieFaces = dieFaces }, Cmd.none)



-- VIEW

view : Model -> Html Msg
view model =
  let (dieFace1, dieFace2) = model.dieFaces
  in
    div []
      [ Svg.svg [ width "230", height "120", viewBox "0 0 230 120" ]
        [ dieFaceSvg dieFace1 0
        , dieFaceSvg dieFace2 1
        ]
      , button [ onClick Roll ] [ Html.text "Roll" ]
      ]

dieFaceSvg : Int -> Int -> Svg Msg
dieFaceSvg dieFace index =
  let (vx, vy, vwidth, vheight, vrx, vry) =
    ((10 + index * (110)), 10, 100, 100, 15, 15)
  in
    g []
      [ rect [ x (toString vx), y (toString vy), width (toString vwidth), height (toString vheight), rx (toString vrx), ry (toString vry) ] [] 
      , g []
        (List.map (dotSvg vx vy vwidth vheight) (dots dieFace))
      ]

dots : Int -> List (DotPosition)
dots dieFace =
  case dieFace of
    1 -> [MiddleCenter]
    2 -> [TopLeft, BottomRight]
    3 -> [TopLeft, MiddleCenter, BottomRight]
    4 -> [TopLeft, TopRight, BottomLeft, BottomRight]
    5 -> [TopLeft, TopRight, MiddleCenter, BottomLeft, BottomRight]
    6 -> [TopLeft, TopRight, MiddleLeft, MiddleRight, BottomLeft, BottomRight]
    _ -> []

dotSvg : Int -> Int -> Int -> Int -> DotPosition -> Svg Msg
dotSvg vx vy vwidth vheight dotPosition =
  let (offset, radius) = (20, 10)
  in
    case dotPosition of
      TopLeft -> circle [ cx (toString (vx + offset)), cy (toString (vy + offset)), r (toString radius), fill "white" ] []
      TopRight -> circle [ cx (toString (vx + vwidth - offset)), cy (toString (vy + offset)), r (toString radius), fill "white" ] []
      MiddleLeft -> circle [ cx (toString (vx + offset)), cy (toString (vy + (vheight // 2))), r (toString radius), fill "white" ] []
      MiddleCenter -> circle [ cx (toString (vx + (vwidth // 2))), cy (toString (vy + (vheight // 2))), r (toString radius), fill "white" ] []
      MiddleRight -> circle [ cx (toString (vx + vwidth - offset)), cy (toString (vy + (vheight // 2))), r (toString radius), fill "white" ] []
      BottomLeft -> circle [ cx (toString (vx + offset)), cy (toString (vy + vheight - offset)), r (toString radius), fill "white" ] []
      BottomRight -> circle [ cx (toString (vx + vwidth - offset)), cy (toString (vy + vheight - offset)), r (toString radius), fill "white" ] []


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- INIT

init : (Model, Cmd Msg)
init =
  (Model (1, 1), Cmd.none)
