module Ast exposing (..)

import Html exposing (Html, text)
import Html.App exposing (beginnerProgram)
import Random exposing (Generator, Seed, step, initialSeed, map, int, float, pair, andThen)

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


expressionGenerator : Generator Expression
expressionGenerator =
  let
    floatToConstant : Float -> Expression
    floatToConstant f = Constant f

    constantGenerator : Generator Expression
    constantGenerator = map floatToConstant (float 0.0 1.0)

    sensorToSensorExpression : Sensor -> Expression
    sensorToSensorExpression sensor = Sensor sensor

    sensorExpressionGenerator : Generator Expression
    sensorExpressionGenerator = map sensorToSensorExpression sensorGenerator

    pairOfExpressionsToPlusExpression : (Expression, Expression) -> Expression
    pairOfExpressionsToPlusExpression (left, right) = Plus left right

    plusGenerator : Generator Expression
    plusGenerator = map pairOfExpressionsToPlusExpression (pair expressionGenerator expressionGenerator)

    pairOfExpressionsToMinusExpression : (Expression, Expression) -> Expression
    pairOfExpressionsToMinusExpression (left, right) = Minus left right

    minusGenerator : Generator Expression
    minusGenerator = map pairOfExpressionsToMinusExpression (pair expressionGenerator expressionGenerator)

    pairOfExpressionsToMultiplyExpression : (Expression, Expression) -> Expression
    pairOfExpressionsToMultiplyExpression (left, right) = Multiply left right

    multiplyGenerator : Generator Expression
    multiplyGenerator = map pairOfExpressionsToMultiplyExpression (pair expressionGenerator expressionGenerator)

    pairOfExpressionsToDivideExpression : (Expression, Expression) -> Expression
    pairOfExpressionsToDivideExpression (left, right) = Divide left right

    divideGenerator : Generator Expression
    divideGenerator = map pairOfExpressionsToDivideExpression (pair expressionGenerator expressionGenerator)

    selectExpressionGenerator : Int -> Generator Expression
    selectExpressionGenerator n =
      case n of
        1 -> divideGenerator

        2 -> multiplyGenerator

        3 -> minusGenerator

        4 -> plusGenerator

        5 -> sensorExpressionGenerator

        _ -> constantGenerator
  in
    (int 1 10) `andThen` selectExpressionGenerator


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
