module Ast exposing (..)

import Html exposing (Html, text, div, button)
import Html.App exposing (beginnerProgram)
import Html.Events exposing (onClick)
import Random exposing (Generator, Seed, step, initialSeed, map, map3, int, float, pair, andThen)

main : Platform.Program Never
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
    AlwaysTrue
  | AlwaysFalse
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


programGenerator : Generator Program
programGenerator =
  let
    ifGenerator : Generator Program
    ifGenerator =
      let
        create : Condition -> Program -> Program -> Program
        create condition left right =
          If condition left right
      in
        map3 create conditionGenerator programGenerator programGenerator


    selectProgramGenerator : Int -> Generator Program
    selectProgramGenerator n =
      case n of
        1 -> ifGenerator

        _ -> map (\command -> Command command) commandGenerator
  in
    (int 1 3) `andThen` selectProgramGenerator


conditionGenerator : Generator Condition
conditionGenerator =
  let
    comparisonGeneratorFor comparison =
      let
        create : (Expression, Expression) -> Condition
        create (left, right) = comparison left right
      in
        map create (pair expressionGenerator expressionGenerator)

    logicalGeneratorFor operator =
      let
        create :  (Condition, Condition) -> Condition
        create (left, right) = operator left right
      in
        map create (pair conditionGenerator conditionGenerator)

    selectConditionGenerator : Int -> Generator Condition
    selectConditionGenerator n =
      case n of
        1 -> (comparisonGeneratorFor Greater)

        2 -> (comparisonGeneratorFor GreaterEqual)

        3 -> (comparisonGeneratorFor Equal)

        4 -> (comparisonGeneratorFor LessEqual)

        5 -> (comparisonGeneratorFor Less)

        6 -> (logicalGeneratorFor And)

        7 -> (logicalGeneratorFor Or)

        8 -> map (\condition -> Not condition) conditionGenerator

        9 -> map (\_ -> AlwaysFalse) (int 0 1)

        _  -> map (\_ -> AlwaysTrue) (int 0 1)
  in
    (int 1 16) `andThen` selectConditionGenerator


expressionGenerator : Generator Expression
expressionGenerator =
  let
    operatorExpressionFor operator =
      let
        pairOfExpressionToOperatorExpression : (Expression, Expression) -> Expression
        pairOfExpressionToOperatorExpression (left, right) =
          operator left right
      in
        map pairOfExpressionToOperatorExpression (pair expressionGenerator expressionGenerator)

    selectExpressionGenerator : Int -> Generator Expression
    selectExpressionGenerator n =
      case n of
        1 -> operatorExpressionFor Divide

        2 -> operatorExpressionFor Multiply

        3 -> operatorExpressionFor Minus

        4 -> operatorExpressionFor Plus

        5 -> map (\sensor -> Sensor sensor) sensorGenerator

        _ -> map (\f -> Constant f) (float 0.0 1.0)
  in
    (int 1 10) `andThen` selectExpressionGenerator


sensorGenerator : Generator Sensor
sensorGenerator =
  let
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
  in
    map intToSensor (int 1 7)


commandGenerator : Generator Command
commandGenerator =
  let
    intToCommand : Int -> Command
    intToCommand n =
      case n of
        1 -> Left

        2 -> Right

        3 -> Thrust

        _ -> Skip
  in
    map intToCommand (int 1 4)


type alias Model =
  {
    program : Program
  , seed : Seed
  }


init : Int -> Model
init n =
  let
    seed = initialSeed n

    (program, seed') = step programGenerator seed
  in
    {
      program = program
    , seed = seed'
    }

model : Model
model = init 0


-- UPDATE


type Message =
    CreateRandomExpression
  | DoNothing


update : Message -> Model -> Model
update message model =
  case message of
    CreateRandomExpression ->
      let
        (program, seed') = step programGenerator model.seed
      in
        { model | program = program, seed = seed' }
    _ -> model


-- VIEW


view : Model -> Html Message
view model =
  div [] [
    button [ onClick CreateRandomExpression ] [ text "go" ]
  , text (toString model.program)
  ]
