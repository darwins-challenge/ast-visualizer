module Ast.Message exposing (Message, createRandomExpression)


type Message =
    CreateRandomExpression
  | DoNothing


createRandomExpression : Message
createRandomExpression =
  CreateRandomExpression
