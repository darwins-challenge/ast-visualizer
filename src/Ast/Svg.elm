module Ast.Svg exposing (programAsSvg)

import Html exposing (Html, text, div)
import Ast.Types
import Ast.Message exposing (Message)
import Svg exposing (..)
import Svg.Attributes exposing (..)

programAsSvg : Ast.Types.Program -> Html Message
programAsSvg program =
  div [] [
    Svg.svg [ width "480", height "480" ] [
       g [ fill "none", stroke "black" ] [
          rect [ x "0", y "0", width "480", height "480" ] []
       ]
    ]
  , Html.text (toString program)
  ]
