module Darken where

import Array exposing (Array)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random exposing (Seed, int, generate)
import StartApp.Simple as StartApp
import Signal exposing (Address, message)

-- MODEL

port startTime : Float
port initialPoints : List Point

type alias Cell = Bool

type alias Grid = List (List Bool)

type alias Point = { x:Int, y:Int}

type alias Model =
  { grid : Grid,
    state : String
  }

type Action = NoOp | ActivateCell Point

emptyGrid : Grid
emptyGrid = List.repeat 5 (List.repeat 5 False)

initialModel: List Point -> Model
initialModel initialPoints =
  { grid = List.foldl toggleCell emptyGrid initialPoints,
    state = "Playing"
  }

-- UPDATE

isComplete : Grid -> Bool
isComplete grid =
  List.all (\cell -> cell == False) (List.concat grid)

isNeighbor : Point -> Point -> Bool
isNeighbor {x,y} p =
  let
    dx = abs (x - p.x)
    dy = abs (y - p.y)
  in
    if (dx == 1 && dy == 0) || (dy == 1 && dx == 0) || (dx == 0 && dy == 0) then
      True
    else
      False

toggleCell : Point -> Grid -> Grid
toggleCell point grid =
  List.indexedMap (\y row -> List.indexedMap (\x cell -> if isNeighbor {x=x, y=y} point then not cell else cell) row) grid

update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model

    ActivateCell point ->
      { model | grid = toggleCell point model.grid }

-- VIEW

--renderCell : Address Action

renderRow : Address Action -> Int -> List Cell -> List Html
renderRow address y row =
  let
    cssClass cell =
      if cell then "square on" else "square off"
    renderCell =
      (\x cell -> (div [ class (cssClass cell), onClick address (ActivateCell (Point x y)) ] [ ]))
  in
    (List.indexedMap renderCell row)

view : Address Action -> Model -> Html
view address model =
  div [ id "lightsout", class (if isComplete model.grid then "winner" else "")]
      (List.concat (List.indexedMap (renderRow address) model.grid))


main : Signal Html
main =
  let
    startTimeSeed = Random.initialSeed <| round startTime
  in
    StartApp.start { model = initialModel initialPoints, view = view, update = update }
