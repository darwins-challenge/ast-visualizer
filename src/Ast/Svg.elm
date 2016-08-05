module Ast.Svg exposing (programAsSvg)

import Html exposing (Html, text, div)
import Ast.Types exposing (..)
import Ast.Message exposing (Message)
import Svg exposing (..)
import Svg.Attributes exposing (..)

programAsSvg : Ast.Types.Program -> Html Message
programAsSvg program =
  div [] [
    Svg.svg [ width "480", height "480" ] [
       g [ fill "none", stroke "black" ] [
         rect [ x "0", y "0", width "480", height "480" ] []
       , (asSvg (bbox program))
       ]
    ]
  , Html.text (toString program)
  ]


type alias BoundingBox = {
    x: Int
  , y: Int
  , width: Int
  , height: Int
}

asSvg : BoundingBox -> Svg Message
asSvg boundingBox =
        rect [
          x (toString boundingBox.x)
        , y (toString boundingBox.y)
        , width (toString boundingBox.width)
        , height (toString boundingBox.height)
        ] []


make_bbox : Int -> Int -> Int -> Int -> BoundingBox
make_bbox x y width height =
  { x = x, y = y, width = width, height = height }


bbox : Ast.Types.Program -> BoundingBox
bbox program =
  case program of
    If condition left right -> make_bbox 0 0 100 200
    Command command -> make_bbox 100 0 100 200
