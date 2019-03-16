module Darken exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick)
import Random



-- MODEL


type Cell
    = On
    | Off


type alias Grid =
    List (List Cell)


type alias Point =
    { x : Int, y : Int }


type Model
    = Playing Grid
    | GameOver Grid


type Msg
    = NoOp
    | ActivateCell Point
    | NewGame (List Point)
    | Reset


emptyGrid : Grid
emptyGrid =
    List.repeat 5 (List.repeat 5 Off)


initializeGame : ( Model, Cmd Msg )
initializeGame =
    let
        model =
            Playing emptyGrid

        cmd =
            randomPoints NewGame
    in
    ( model, cmd )



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
            updateCell point model

        Reset ->
            initializeGame

        NewGame points ->
            let
                newGrid =
                    List.foldl pressCell emptyGrid points
            in
            ( Playing newGrid, Cmd.none )


updateCell : Point -> Model -> ( Model, Cmd Msg )
updateCell point model =
    case model of
        Playing grid ->
            let
                newGrid =
                    pressCell point grid

                isFinished =
                    isComplete newGrid

                newModel =
                    if isFinished then
                        GameOver newGrid

                    else
                        Playing newGrid
            in
            ( newModel, Cmd.none )

        GameOver grid ->
            ( model, Cmd.none )


randomPoints : (List Point -> Msg) -> Cmd Msg
randomPoints msg =
    Random.map2 Point (Random.int 0 4) (Random.int 0 4)
        |> Random.list 5
        |> Random.generate msg



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
    case model of
        Playing grid ->
            wrapper grid

        GameOver grid ->
            winWrapper grid


viewGrid : Grid -> Html Msg
viewGrid grid =
    div [ id "lightsout" ]
        (List.concat (List.indexedMap renderRow grid))


winWrapper : Grid -> Html Msg
winWrapper grid =
    div
        [ id "wrapper", class "winner" ]
        [ viewGrid grid
        , div [ class "winbox" ]
            [ p [] [ text "Darkness complete." ]
            , span [ onClick Reset ] [ text "Play again?" ]
            ]
        ]


wrapper : Grid -> Html Msg
wrapper grid =
    div
        [ id "wrapper" ]
        [ viewGrid grid ]


main =
    Browser.document
        { init = \() -> initializeGame
        , update = update
        , view = \model -> { title = "Darken", body = [ view model ] }
        , subscriptions = \_ -> Sub.none
        }
