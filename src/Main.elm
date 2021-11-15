module Main exposing (..)

import Browser
import Css exposing (..)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Random



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    Board

diceLst : Int -> Random.Generator (List Int)
diceLst n = Random.list n (Random.int 0 49)

init : Model
init =
  let bomPos = diceLst 3
  in
    let n = 0
    in
      Board (List.repeat 7 (List.repeat 7 ( Cell n, Untouch )))


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
            button [ onClick Increment ] [ text "0" ]


rowDivLst : List ( Cell, State ) -> Html Msg
rowDivLst input =
    div [] (List.map colDivLst input)



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        Board c ->
            div []
                [ div [] (List.map rowDivLst c)
                ]
