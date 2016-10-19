import Http
import Task
import Json.Decode as Json
import Html exposing (Html, Attribute, div, select, option, text, img, button)
import Html.App as App
import Html.Attributes exposing (src, selected, size)
import Html.Events exposing (onClick, on)
import List exposing (map, head, drop)
import Array exposing (Array, fromList, get)
import Maybe exposing (withDefault)

main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type alias Model =
  { topic : Topic
  , gifUrl : String
  , error : FetchError
  }

type Topic
  = Cats
  | Puppies
  | Babies

topics : List Topic
topics = [Cats, Puppies, Babies]

topicsArray : Array Topic
topicsArray = fromList topics

defaultTopic : Topic
defaultTopic = Cats

topicText : Topic -> String
topicText topic =
  case topic of
    Cats -> "cats"
    Puppies -> "puppies"
    Babies -> "babies"

type FetchError
  = None
  | Error Http.Error

init : (Model, Cmd Msg)
init =
  (Model defaultTopic "waiting.gif" None, getRandomGif defaultTopic)


-- UPDATE

type Msg
  = MorePlease
  | ChangeTopic Int
  | FetchSucceed String
  | FetchFail Http.Error

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MorePlease ->
      (model, getRandomGif model.topic)

    ChangeTopic topicIndex ->
      let
        topic = withDefault defaultTopic (get topicIndex topicsArray)
      in
        ({model | topic = topic, error = None}, getRandomGif topic)

    FetchSucceed newUrl ->
      ({model | gifUrl = newUrl, error = None}, Cmd.none)
      
    FetchFail error ->
      ({model | error = Error error}, Cmd.none)


getRandomGif : Topic -> Cmd Msg
getRandomGif topic =
  let
    url =
      "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=" ++ (topicText topic)
  in
    Task.perform FetchFail FetchSucceed (Http.get decodeGifUrl url)


decodeGifUrl : Json.Decoder String
decodeGifUrl =
  Json.at ["data", "image_url"] Json.string


-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ select [onChange ChangeTopic] (map (topicOption model.topic) topics)
    , button [onClick MorePlease] [text "More Please!"]
    , errorMessage model.error
    , img [src model.gifUrl] []
    ]

decodeSelectedIndex : Json.Decoder Int
decodeSelectedIndex =
  Json.at ["target", "selectedIndex"] Json.int

onChange : (Int -> Msg) -> Attribute Msg
onChange message =
  on "change" (Json.map message decodeSelectedIndex)

topicOption : Topic -> Topic -> Html Msg
topicOption current topic =
  option [selected (current == topic)] [text (topicText topic)]

errorMessage : FetchError -> Html Msg
errorMessage error =
  case error of
    None -> div [] []
    _ -> div [] 
      [ text (toString error) 
      ]


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
