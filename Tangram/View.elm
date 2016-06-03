module Tangram.View exposing (view)

import Html exposing (Html)
import Html.App as Html
import Html.Attributes as HA
import Html.Events
import Json.Encode as JE
import Svg exposing (Svg)
import Svg.Attributes exposing (..)
import Svg.Lazy exposing (lazy, lazy3)
import Tangram.Model exposing (Model)
import Tangram.Update exposing (layoutEncoder)
import Piece.Model as Piece
import Piece.View as Piece
import Tangram.Types exposing (..)


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.h1 [] [ Html.text "Elm Tangram: drag&drop with SVG" ]
        , Html.div [] [ Html.text "Drag to move, shift-drag to rotate. Saved to localStorage." ]
        , scene model
        , resetButton
        , showLayoutButton model
        , nameView model
        , if model.showingLayout then
            exportView model
          else
            Html.text ""
          -- , debugInfo model
        ]


scene : Model -> Html.Html Msg
scene model =
    Svg.svg
        [ width <| toString model.size.width
        , height <| toString model.size.height
        , style "margin-left: 8px; margin-top: 10px"
        ]
        (lazy3 background (cursorVal model) model.size.width model.size.height
            :: (List.map (lazy pieceView) model.pieces)
        )


pieceView : ( Name, Piece.Model ) -> Svg.Svg Msg
pieceView ( name, piece ) =
    Piece.view piece |> Html.map (PieceMsg name)


background : String -> Int -> Int -> Svg.Svg Msg
background cursorV w h =
    Svg.rect
        [ width <| toString w
        , height <| toString h
        , fill "#F0F0F0"
        , cursor <| cursorV
        ]
        []


cursorVal model =
    if List.any (Piece.rotating << snd) model.pieces then
        "crosshair"
    else
        "default"


debugInfo : Model -> Html.Html Msg
debugInfo model =
    Html.div []
        [ Html.text <| "size = " ++ toString model.size
        , Html.text <| "shift = " ++ toString model.shift
        , Html.ul []
            (List.map (\item -> Html.li [] [ (Html.text << toString) item ]) model.pieces)
        ]


resetButton : Html.Html Msg
resetButton =
    Html.button [ Html.Events.onClick Reset ]
        [ Html.text "reset" ]


showLayoutButton : Model -> Html Msg
showLayoutButton model =
    let
        label =
            if model.showingLayout then
                "hide layout"
            else
                "show layout"
    in
        Html.button [ Html.Events.onClick ToggleLayout ]
            [ Html.text label ]


exportView : Model -> Html Msg
exportView model =
    Html.textarea
        [ HA.class "exportView"
        , HA.rows 4
        ]
        [ layoutEncoder model.pieces
            |> JE.encode 2
            |> Html.text
        ]


nameView : Model -> Html Msg
nameView model =
    Html.input
        [ Html.Events.onInput SetName
        , HA.value model.name
        ]
        []
