module Ast.Generator exposing (programGenerator)

import Random exposing (Generator, Seed, step, initialSeed, map, map3, int, float, pair, andThen)
import Ast.Types exposing (..)


programGenerator : Generator Ast.Types.Program
programGenerator =
  let
    ifGenerator : Generator Ast.Types.Program
    ifGenerator =
      let
        create : Condition -> Ast.Types.Program -> Ast.Types.Program -> Ast.Types.Program
        create condition left right =
          If condition left right
      in
        map3 create conditionGenerator programGenerator programGenerator


    selectProgramGenerator : Int -> Generator Ast.Types.Program
    selectProgramGenerator n =
      case n of
        1 -> ifGenerator

        _ -> map (\command -> Command command) commandGenerator
  in
    (int 1 15) `andThen` selectProgramGenerator


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

