module Ast exposing (..)

import Html exposing (Html, text)
import Html.App exposing (beginnerProgram)


main =
  beginnerProgram
    {
      model = model
    , update = update
    , view = view
    }


-- MODEL

type Program =
    If Condition Program Program
  | Command Command


type Condition =
    True
  | False
  | Not Condition
  | Or Condition Condition
  | And Condition Condition
  | Less Expression Expression
  | LessEqual Expression Expression
  | Equal Expression Expression
  | GreaterEqual Expression Expression
  | Greater Expression Expression


type Expression =
    Constant Float
  | Sensor Sensor
  | Plus Expression Expression
  | Minus Expression Expression
  | Multiply Expression Expression
  | Divide Expression Expression

type Sensor =
    X
  | Y
  | Vx
  | Vy
  | O
  | W
  | Fuel


type Command =
    Skip
  | Left
  | Right
  | Thrust


type alias Model =
  {
    message : String
  , program : Program
  }


init : String -> Model
init message =
  {
    message = message
  , program = Command Skip
  }

model : Model
model = init "Hello, World!"


-- UPDATE


type Message =
  DoNothing

update : Message -> Model -> Model
update message model =
  case message of
    _ -> model


-- VIEW


view : Model -> Html Message
view model =
  text model.message
