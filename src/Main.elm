module Main exposing (..)

import Browser
import Css exposing (..)
import Html exposing (Html, button, div, h1, text)
import Html.Events exposing (onClick)
import List.Extra exposing (..)



-- import Random
-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    List (List ( Cell, State ))



-- diceLst : Int -> Random.Generator (List Int)
-- diceLst n =
--     Random.list n (Random.int 0 49)


setBomCell : ( Cell, State ) -> ( Cell, State )
setBomCell _ =
    ( Bom, Untouch )


setTouchedCell : ( Cell, State ) -> ( Cell, State )
setTouchedCell ( c, s ) =
    ( c, Touched )


increaseNumCell : ( Cell, State ) -> ( Cell, State )
increaseNumCell x =
    case x of
        ( Cell n, s ) ->
            ( Cell (n + 1), s )

        ( Bom, _ ) ->
            x


setBom : Int -> List ( Cell, State ) -> List ( Cell, State )
setBom x =
    if modBy 7 x == 0 then
        List.Extra.updateAt x setBomCell
            >> List.Extra.updateAt (x + 1) increaseNumCell
            >> List.Extra.updateAt (x + 8) increaseNumCell
            >> List.Extra.updateAt (x - 6) increaseNumCell
            >> List.Extra.updateAt (x + 7) increaseNumCell
            >> List.Extra.updateAt (x - 7) increaseNumCell

    else if modBy 7 x == 6 then
        List.Extra.updateAt x setBomCell
            >> List.Extra.updateAt (x - 1) increaseNumCell
            >> List.Extra.updateAt (x - 8) increaseNumCell
            >> List.Extra.updateAt (x + 6) increaseNumCell
            >> List.Extra.updateAt (x + 7) increaseNumCell
            >> List.Extra.updateAt (x - 7) increaseNumCell

    else
        List.Extra.updateAt x setBomCell
            >> List.Extra.updateAt (x + 1) increaseNumCell
            >> List.Extra.updateAt (x + 8) increaseNumCell
            >> List.Extra.updateAt (x - 6) increaseNumCell
            >> List.Extra.updateAt (x - 1) increaseNumCell
            >> List.Extra.updateAt (x - 8) increaseNumCell
            >> List.Extra.updateAt (x + 6) increaseNumCell
            >> List.Extra.updateAt (x + 7) increaseNumCell
            >> List.Extra.updateAt (x - 7) increaseNumCell


init : Model
init =
    let
        bomPos =
            [ 2, 14, 30, 40 ]
    in
    let
        board =
            List.foldl setBom (List.repeat 49 ( Cell 0, Untouch )) bomPos
    in
    List.Extra.greedyGroupsOf 7 board


type Cell
    = Bom
    | Cell Int


type State
    = Untouch
    | Touched
    | Flag



-- UPDATE


type Msg
    = Open (Cell, State)
    | OpenBom
    | SetFlag


update : Msg -> Model -> Model
update msg model =
    case msg of
        Open c ->
            (List.Extra.greedyGroupsOf 7 (List.Extra.updateIf ((==)c) setTouchedCell (List.concat model)))
        OpenBom ->
            model

        SetFlag ->
            model


colDivLst : ( Cell, State ) -> Html Msg
colDivLst cell =
    case cell of
        ( _, Flag ) ->
            button [] [ text "F" ]

        ( Cell n, Touched ) ->
            button [] [ text (String.fromInt n) ]

        ( Cell n, Untouch ) ->
            button [ onClick (Open cell) ] [ text "?" ]

        ( Bom, Untouch ) ->
            button [ onClick OpenBom ] [ text "?" ]

        ( Bom, Touched ) ->
            button [] [ text "B" ]


rowDivLst : List ( Cell, State ) -> Html Msg
rowDivLst input =
    div [] (List.map colDivLst input)



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Mine" ]
        , div [] (List.map rowDivLst model)
        ]
