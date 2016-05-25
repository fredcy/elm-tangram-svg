module Piece.View exposing (view)

import Colors
import Piece.Model exposing (Model, getPosition, getRotation)
import Piece.Types exposing (..)


--

import Json.Decode as Json exposing ((:=))
import Mouse
import String
import Svg exposing (Svg)
import Svg.Attributes exposing (..)
import VirtualDom


view : Model -> Svg Msg
view model =
    let
        realPosition =
            getPosition model

        realRotation =
            getRotation model

        ds =
            model.drag
    in
        Svg.svg
            [ VirtualDom.on "mousedown" (Json.map DragStart offsetPosition)
            ]
            [ case model.shape of
                Triangle color scale ->
                    polygon trianglePoints color scale realRotation ( toFloat realPosition.x, toFloat realPosition.y ) ds

                Square color scale ->
                    polygon squarePoints color scale realRotation ( toFloat realPosition.x, toFloat realPosition.y ) ds

                Parallelogram color scale ->
                    polygon paraPoints color scale realRotation ( toFloat realPosition.x, toFloat realPosition.y ) ds
            ]


{-| Extract the page and offset positions from the mouse event.
-}
offsetPosition : Json.Decoder ( Position, Position )
offsetPosition =
    Json.object2 (,)
        (Json.object2 Position ("pageX" := Json.int) ("pageY" := Json.int))
        (Json.object2 Position ("offsetX" := Json.int) ("offsetY" := Json.int))


{-| Define the vertices of the shapes.  Define each such that origin at their
50% point so that rotation is natural. Triangle is defined with the hypotenuse
is horizontal.
-}
trianglePoints =
    [ ( 0, -0.5 ), ( 1, 0.5 ), ( -1, 0.5 ) ]


squarePoints =
    [ ( 0, -0.5 ), ( 0.5, 0 ), ( 0, 0.5 ), ( -0.5, 0 ) ]


paraPoints =
    [ ( 0.25, -0.25 ), ( -0.75, -0.25 ), ( -0.25, 0.25 ), ( 0.75, 0.25 ) ]



{- Do transformations explicitly in Elm rather than using the SVG `transform`
   attribute. My scheme of defining the shapes with origin at their center does
   not seem to place nice with SVG transformation, causing clipping when coords
   are negative.
-}


{-| Construct part of 'd' attribute for Svg path element
-}
ds : String -> List number -> String
ds tag values =
    tag ++ " " ++ (String.join " " (List.map toString values)) ++ " "


rotateHandle : Position -> Position -> Svg Msg
rotateHandle center end =
    Svg.svg []
        [ Svg.line
            [ x1 <| toString center.x
            , y1 <| toString center.y
            , x2 <| toString end.x
            , y2 <| toString end.y
            , style "stroke:rgb(255,0,0);stroke-width:2"
            ]
            []
        , handleArc center end
        ]



-- TODO: Having positions as both ( Float, Float ) and { x : Int, y : Int } is
-- causing grief. Can I reconcile those somehow?


{-| Draw arc at end of rotation handle. What a pain.
-}
handleArc : Position -> Position -> Svg Msg
handleArc center end =
    let
        radius =
            distance center end

        centerPt =
            toPoint center

        arcend =
            end |> toPoint |> translatePoint (scalePoint -1 centerPt) |> rotatePoint (degrees 20) |> translatePoint centerPt |> toPosition

        dVal =
            ds "M" [ end.x, end.y ]
                ++ ds "A" [ radius, radius, 0, 0, 1, toFloat arcend.x, toFloat arcend.y ]
                |> Debug.log "dVal"
    in
        Svg.path
            [ d dVal
            , style "stroke:rgb(255,0,0);stroke-width:2"
            ]
            []


polygon : List ( Float, Float ) -> Colors.Color -> Float -> Float -> ( Float, Float ) -> Maybe Drag -> Svg Msg
polygon shape color scale rotation (( px, py ) as position) drag =
    let
        vertices =
            shape |> List.map (scalePoint scale >> rotatePoint rotation >> translatePoint position)

        cursorVal =
            case drag of
                Just (Dragging _) ->
                    "move"

                Just (Rotating _) ->
                    "crosshair"

                _ ->
                    "pointer"

        handle =
            case drag of
                Just (Rotating { current }) ->
                    rotateHandle (Position (round px) (round py)) current

                _ ->
                    Svg.text ""
    in
        Svg.svg []
            [ Svg.polygon
                [ points <| pointsToString vertices
                , fill <| Colors.toCss color
                , stroke "lightgray"
                , strokeWidth (toString (6))
                , strokeLinejoin "round"
                , cursor cursorVal
                ]
                []
            , handle
            ]


rotatePoint : Float -> ( Float, Float ) -> ( Float, Float )
rotatePoint angle ( x, y ) =
    let
        x' =
            x * cos angle - y * sin angle

        y' =
            x * sin angle + y * cos angle
    in
        ( x', y' )


scalePoint : Float -> ( Float, Float ) -> ( Float, Float )
scalePoint factor ( x, y ) =
    ( x * factor, y * factor )


translatePoint : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
translatePoint ( dx, dy ) ( x, y ) =
    ( x + dx, y + dy )


{-| Construct the value needed for the SVG `points` attribute.
-}
pointsToString : List ( Float, Float ) -> String
pointsToString list =
    List.map (\( x, y ) -> toString x ++ " " ++ toString y) list |> String.join " "


toPoint : Position -> ( Float, Float )
toPoint position =
    ( toFloat position.x, toFloat position.y )


toPosition : ( Float, Float ) -> Position
toPosition ( px, py ) =
    Position (round px) (round py)
