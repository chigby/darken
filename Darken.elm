module Darken exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (id, class)
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


init : ( Model, Cmd msg )
init =
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


renderCell : Int -> Int -> Cell -> Html Msg
renderCell x y cell =
    let
        cssClass cell =
            case cell of
                On ->
                    "square on"

                Off ->
                    "square off"
    in
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
    Html.program { init = init, update = update, view = view, subscriptions = \_ -> Sub.none }
