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
    Board



-- diceLst : Int -> Random.Generator (List Int)
-- diceLst n =
--     Random.list n (Random.int 0 49)


setBomCell : ( Cell, State ) -> ( Cell, State )
setBomCell _ =
    ( Bom, Untouch )


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
        n =
            0
    in
    let
        l =
            List.foldl setBom (List.repeat 49 ( Cell n, Untouch )) bomPos
    in
    Board (List.Extra.greedyGroupsOf 7 l)


type Board
    = Board (List (List ( Cell, State )))


type Cell
    = Bom
    | Cell Int


type State
    = Untouch
    | Flag



-- UPDATE


type Msg
    = Increment


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            model


colDivLst : ( Cell, State ) -> Html Msg
colDivLst cell =
    case cell of
        ( Cell n, _ ) ->
            button [ onClick Increment ] [ text (String.fromInt n) ]

        ( Bom, _ ) ->
            button [ onClick Increment ] [ text "B" ]


rowDivLst : List ( Cell, State ) -> Html Msg
rowDivLst input =
    div [] (List.map colDivLst input)



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        Board c ->
            div []
                [ h1 [] [ text "Mine" ]
                , div [] (List.map rowDivLst c)
                ]
