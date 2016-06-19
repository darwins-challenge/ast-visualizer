module Ast.Svg exposing (programAsSvg)

import Html exposing (Html, text)
import Ast.Types
import Ast.Message exposing (Message)

programAsSvg : Ast.Types.Program -> Html Message
programAsSvg program =
  text (toString program)
