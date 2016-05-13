module Darken (..) where

import Html exposing (Html, div)
import Html.Attributes exposing (id, class)
import Html.Events exposing (onClick)
import Signal exposing (Address)
import StartApp.Simple as StartApp


-- MODEL


type Cell
  = On
  | Off


type alias Grid =
  List (List Cell)


type alias Point =
  { x : Int, y : Int }


type alias Model =
  { grid : Grid
  }


type Action
  = NoOp
  | ActivateCell Point


emptyGrid : Grid
emptyGrid =
  List.repeat 5 (List.repeat 5 Off)


initialModel : List Point -> Model
initialModel initialPoints =
  { grid = List.foldl pressCell emptyGrid initialPoints
  }



-- UPDATE


isComplete : Grid -> Bool
isComplete grid =
  List.all (\cell -> cell == Off) (List.concat grid)


isConnected : Point -> Point -> Bool
isConnected { x, y } p =
  let
    dx =
      abs (x - p.x)

    dy =
      abs (y - p.y)
  in
    (dx == 1 && dy == 0) || (dy == 1 && dx == 0) || (dx == 0 && dy == 0)


toggleCell : Cell -> Cell
toggleCell cell =
  case cell of
    On ->
      Off

    Off ->
      On


pressCell : Point -> Grid -> Grid
pressCell point grid =
  List.indexedMap
    (\y row ->
      List.indexedMap
        (\x cell ->
          if isConnected { x = x, y = y } point then
            toggleCell cell
          else
            cell
        )
        row
    )
    grid


update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model

    ActivateCell point ->
      { model | grid = pressCell point model.grid }



-- VIEW


renderCell : Address Action -> Int -> Int -> Cell -> Html
renderCell address x y cell =
  let
    cssClass cell =
      case cell of
        On ->
          "square on"

        Off ->
          "square off"
  in
    div [ class (cssClass cell), onClick address (ActivateCell (Point x y)) ] []


renderRow : Address Action -> Int -> List Cell -> List Html
renderRow address y row =
  List.indexedMap (\x cell -> renderCell address x y cell) row


view : Address Action -> Model -> Html
view address model =
  div
    [ id "wrapper"
    , class
        (if isComplete model.grid then
          "winner"
         else
          ""
        )
    ]
    [ div
        [ id "lightsout" ]
        (List.concat (List.indexedMap (renderRow address) model.grid))
    ]


port initialPoints : List Point
main : Signal Html
main =
  StartApp.start { model = initialModel initialPoints, view = view, update = update }
