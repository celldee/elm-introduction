import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import String
import Regex


main =
  App.beginnerProgram { model = model, view = view, update = update }


-- MODEL

type alias Model =
  { name : String
  , age: String
  , password : String
  , passwordAgain : String
  , validationResult: ValidationResult
  }

type ValidationResult
  = NotDone
  | Error String
  | Valid


model : Model
model =
  Model "" "" "" "" NotDone


-- UPDATE

type Msg
    = Name String
    | Age String
    | Password String
    | PasswordAgain String
    | Submit


update : Msg -> Model -> Model
update msg model =
  case msg of
    Name name ->
      { model | name = name }

    Age age ->
      { model | age = age }

    Password password ->
      { model | password = password }

    PasswordAgain password ->
      { model | passwordAgain = password }

    Submit ->
      { model | validationResult = validate model }

validate : Model -> ValidationResult
validate model =
  if not (Regex.contains (Regex.regex "^[0-9]+$") model.age) then
    Error "Age must be a number"
  else if String.length model.password < 8 then
    Error "Password must be at least 8 characters"
  else if not (Regex.contains (Regex.regex "^(?=.*[a-z])") model.password) then
    Error "Password must contain at least one lower case letter"
  else if not (Regex.contains (Regex.regex "^(?=.*[A-Z])") model.password) then
    Error "Password must contain at least one upper case letter"
  else if not (Regex.contains (Regex.regex "^(?=.*[0-9])") model.password) then
    Error "Password must contain at least one number"
  else if model.password == model.passwordAgain then
    Valid
  else
    Error "Passwords do not match!"

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ input [ type' "text", placeholder "Name", onInput Name ] []
    , input [ type' "text", placeholder "Age", onInput Age ] []
    , input [ type' "password", placeholder "Password", onInput Password ] []
    , input [ type' "password", placeholder "Re-enter Password", onInput PasswordAgain ] []
    , button [ onClick Submit ] [ text "submit" ]
    , viewValidation model.validationResult
    ]

viewValidation : ValidationResult -> Html msg
viewValidation validationResult =
  let
    (color, message) =
      case validationResult of
        NotDone -> ("", "")
        Valid -> ("green", "OK")
        Error message -> ("red", message)
  in
    div [ style [("color", color)] ] [ text message ]
