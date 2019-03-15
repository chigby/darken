module Darken exposing (Cell(..), Grid, Model, Msg(..), Point, emptyGrid, init, isComplete, isConnected, main, pressCell, renderCell, renderRow, toggleCell, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick)



-- MODEL


type Cell
    = On
    | Off


type alias Grid =
    List (List Cell)


type alias Point =
    { x : Int, y : Int }


type alias Model =
    { grid : Grid }


type Msg
    = NoOp
    | ActivateCell Point


emptyGrid : Grid
emptyGrid =
    List.repeat 5 (List.repeat 5 Off)


init : () -> ( Model, Cmd msg )
init _ =
    ( { grid = List.foldl pressCell emptyGrid [ { x = 1, y = 1 } ] }
    , Cmd.none
    )



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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ActivateCell point ->
            ( { model | grid = pressCell point model.grid }, Cmd.none )



-- VIEW


cssClass : Cell -> String
cssClass cell =
    case cell of
        On ->
            "square on"

        Off ->
            "square off"


renderCell : Int -> Int -> Cell -> Html Msg
renderCell x y cell =
    div [ class (cssClass cell), onClick (ActivateCell (Point x y)) ] []


renderRow : Int -> List Cell -> List (Html Msg)
renderRow y row =
    List.indexedMap (\x cell -> renderCell x y cell) row


view : Model -> Html Msg
view model =
    div
        [ id "wrapper"
        , class
            (if isComplete model.grid then
                "winner"

             else
                ""
            )
        ]
        [ div [ id "lightsout" ]
            (List.concat (List.indexedMap renderRow model.grid))
        ]


main =
    Browser.document
        { init = init
        , update = update
        , view = \model -> { title = "Darken", body = [ view model ] }
        , subscriptions = \_ -> Sub.none
        }
