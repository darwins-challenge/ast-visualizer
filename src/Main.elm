module Ast exposing (..)

import Html exposing (Html, text, div, button)
import Html.App exposing (beginnerProgram)
import Html.Events exposing (onClick)
import Random exposing (Generator, Seed, step, initialSeed, map, map3, int, float, pair, andThen)
import Ast.Types exposing (..)
import Ast.Message exposing (Message, createRandomExpression)
import Ast.Svg exposing (programAsSvg)
import Ast.Generator exposing (programGenerator)

main : Platform.Program Never
main =
  beginnerProgram
    {
      model = model
    , update = update
    , view = view
    }


-- MODEL


type alias Model =
  {
    program : Ast.Types.Program
  , seed : Seed
  }


init : Int -> Model
init n =
  let
    seed = initialSeed n

    (program, seed') = (If (Not AlwaysFalse) (Command Thrust) (If (AlwaysTrue) (Command Skip) (Command Left)), seed)
  in
    {
      program = program
    , seed = seed'
    }

model : Model
model = init 0


-- UPDATE


update : Message -> Model -> Model
update message model =
  case message of
    createRandomExpression ->
      let
        (program, seed') = step programGenerator model.seed
      in
        { model | program = program, seed = seed' }


-- VIEW


view : Model -> Html Message
view model =
  div [] [
    button [ onClick createRandomExpression ] [ text "go" ]
  , text (toString model.seed)
  , div [] [ programAsSvg model.program ]
  ]
