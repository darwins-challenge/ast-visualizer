module Ast exposing (..)

import Html exposing (Html, text)
import Html.App exposing (beginnerProgram)


main =
  beginnerProgram
    { model = model
    , update = update
    , view = view
    }


-- MODEL


type alias Model = String


model : Model
model = "Hello, World!"


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
  text model
