module Ast exposing (..)

import Html exposing (Html, text)
import Html.App exposing (beginnerProgram)
import Random exposing (Generator, Seed, step, initialSeed, map, int, float)

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


intToSensor : Int -> Sensor
intToSensor n =
  case n of
    1 -> X

    2 -> Y

    3 -> Vx

    4 -> Vy

    5 -> W

    6 -> O

    _ -> Fuel


sensorGenerator : Generator Sensor
sensorGenerator = map intToSensor (int 1 7)


intToCommand : Int -> Command
intToCommand n =
  case n of
    1 -> Left

    2 -> Right

    3 -> Thrust

    _ -> Skip


commandGenerator : Generator Command
commandGenerator = map intToCommand (int 1 4)


type alias Model =
  {
    program : Program
  , seed : Seed
  }


init : Int -> Model
init n =
  let
    seed = initialSeed n

    (command, seed') = step commandGenerator seed

    program = Command command
  in
    {
      program = program
    , seed = seed'
    }

model : Model
model = init 0


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
  text (toString model.program)
