module View exposing (..)

import Html exposing (Html)
import Html.App as Html
import Svg exposing (..)
import Svg.Attributes as Svg exposing (..)
import Model exposing (Model)
import Piece.Model as Piece
import Piece.View as Piece
import Types exposing (..)


view : Model -> Html.Html Msg
view model =
  Html.div
    []
    [ Html.h1 [] [ Html.text "SVG drag and drop" ]
    , Html.div [] [ Html.text "Drag to move, shift-drag to rotate" ]
    , scene model
      --, debugInfo model
    ]


scene : Model -> Html.Html Msg
scene model =
  Svg.svg
    [ width <| toString model.size.width
    , height <| toString model.size.height
    ]
    (background model.size.width model.size.height :: (List.map pieceView model.pieces))


pieceView : ( Name, Piece.Model ) -> Svg.Svg Msg
pieceView ( name, piece ) =
  Piece.view piece |> Html.map (PieceMsg name)


background : Int -> Int -> Svg.Svg Msg
background width height =
  Svg.rect
    [ Svg.width <| toString width
    , Svg.height <| toString height
    , fill "#EEEEEE"
    ]
    []


debugInfo : Model -> Html.Html Msg
debugInfo model =
  Html.div
    []
    [ Html.text <| "size = " ++ toString model.size
    , Html.text <| "shift = " ++ toString model.shift
    , Html.ul
        []
        (List.map (\item -> Html.li [] [ (Html.text << toString) item ]) model.pieces)
    ]
