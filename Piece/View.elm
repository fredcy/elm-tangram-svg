module Piece.View
    exposing
        ( Point
        , view
        , vertices
        , boundingBox
        , offsetPosition
        , strokeOffset
        )

import Colors
import Piece.Model exposing (Model, getPosition, getRotation)
import Piece.Types exposing (..)


--

import Json.Decode as Json
import String
import Svg exposing (Svg)
import Svg.Attributes exposing (..)
import VirtualDom


type alias Point =
    ( Float, Float )


strokeOffset : Int
strokeOffset =
    3


view : Model -> Svg Msg
view model =
    let
        realPosition =
            getPosition model

        pos =
            ( toFloat realPosition.x, toFloat realPosition.y )

        { color } =
            shapeInfo model.shape
    in
        Svg.svg [ VirtualDom.on "mousedown" (Json.map DragStart offsetPosition) ]
            [ polygon2 (vertices model) color pos model.drag ]


{-| Get final vertex locations for the shape, as points.
-}
vertices : Model -> List Point
vertices model =
    let
        { points, scale } =
            shapeInfo model.shape

        realPosition =
            getPosition model

        realRotation =
            getRotation model

        (( px, py ) as position) =
            ( toFloat realPosition.x, toFloat realPosition.y )
    in
        points |> List.map (scalePoint scale >> rotatePoint realRotation >> translatePoint position)


{-| Origin and corner of minimal bounding box.
-}
boundingBox : List Point -> ( Point, Point )
boundingBox vertices =
    let
        ( xs, ys ) =
            List.unzip vertices

        minx =
            List.minimum xs |> Maybe.withDefault 0

        maxx =
            List.maximum xs |> Maybe.withDefault 0

        miny =
            List.minimum ys |> Maybe.withDefault 0

        maxy =
            List.maximum ys |> Maybe.withDefault 0

        offset =
            toFloat strokeOffset
    in
        ( ( minx - offset, miny - offset )
        , ( maxx + offset, maxy + offset )
        )


{-| Scale and translate the points so that they just fit in a unit box.
-}
normalizeVertices : List Point -> List Point
normalizeVertices vertices =
    let
        ( ( minx, miny ), ( maxx, maxy ) ) =
            boundingBox vertices

        scale =
            Basics.max (maxx - minx) (maxy - miny)
    in
        vertices
            |> List.map (translatePoint ( -minx, -miny ))
            |> List.map (scalePoint (1 / scale))


{-| Extract the page and offset positions from the mouse event.
-}
offsetPosition : Json.Decoder ( Position, Position )
offsetPosition =
    Json.map2 (,)
        (Json.map2 Position (Json.field "pageX" Json.int) (Json.field "pageY" Json.int))
        (Json.map2 Position (Json.field "offsetX" Json.int) (Json.field "offsetY" Json.int))


{-| Define the base (unscaled, unrotated) vertices of the shapes.  Define each
such that origin is at their 50% point so that rotation is natural. Triangle is
defined with the hypotenuse on the bottom. Square and parallelogram are oriented
as in the default tangram shape.
-}
trianglePoints : List ( Float, Float )
trianglePoints =
    [ ( 0, -0.5 ), ( 1, 0.5 ), ( -1, 0.5 ) ]


squarePoints : List ( Float, Float )
squarePoints =
    [ ( 0, -0.5 ), ( 0.5, 0 ), ( 0, 0.5 ), ( -0.5, 0 ) ]


paraPoints : List ( Float, Float )
paraPoints =
    [ ( 0.25, -0.25 ), ( -0.75, -0.25 ), ( -0.25, 0.25 ), ( 0.75, 0.25 ) ]


shapeInfo : Shape -> { points : List Point, color : Colors.Color, scale : Float }
shapeInfo shape =
    case shape of
        Triangle color scale ->
            { points = trianglePoints, color = color, scale = scale }

        Square color scale ->
            { points = squarePoints, color = color, scale = scale }

        Parallelogram color scale ->
            { points = paraPoints, color = color, scale = scale }



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

        arcAngle =
            degrees 20

        arcbegin =
            end |> toPoint |> translatePoint (scalePoint -1 centerPt) |> rotatePoint (-arcAngle / 2) |> translatePoint centerPt

        arcend =
            end |> toPoint |> translatePoint (scalePoint -1 centerPt) |> rotatePoint (arcAngle / 2) |> translatePoint centerPt |> toPosition

        dVal =
            ds "M" [ Tuple.first arcbegin, Tuple.second arcbegin ]
                ++ ds "A" [ radius, radius, 0, 0, 1, toFloat arcend.x, toFloat arcend.y ]
                |> Debug.log "dVal"
    in
        Svg.path
            [ d dVal
            , style "stroke:rgb(255,0,0);stroke-width:2;fill:none"
            ]
            []


polygon : List Point -> Colors.Color -> Float -> Float -> Point -> Maybe Drag -> Svg Msg
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
                , stroke "red"
                , strokeWidth (toString (1))
                , strokeLinejoin "round"
                , cursor cursorVal
                ]
                []
            , handle
            ]


polygon2 : List Point -> Colors.Color -> Point -> Maybe Drag -> Svg Msg
polygon2 vertices color (( px, py ) as position) drag =
    let
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
                , stroke "#eeeeee"
                , strokeWidth (toString (strokeOffset * 2))
                , strokeLinejoin "round"
                , cursor cursorVal
                ]
                []
            , handle
            ]


rotatePoint : Float -> Point -> Point
rotatePoint angle ( x, y ) =
    let
        x_ =
            x * cos angle - y * sin angle

        y_ =
            x * sin angle + y * cos angle
    in
        ( x_, y_ )


scalePoint : Float -> Point -> Point
scalePoint factor ( x, y ) =
    ( x * factor, y * factor )


translatePoint : Point -> Point -> Point
translatePoint ( dx, dy ) ( x, y ) =
    ( x + dx, y + dy )


{-| Construct the value needed for the SVG `points` attribute.
-}
pointsToString : List Point -> String
pointsToString list =
    List.map (\( x, y ) -> toString x ++ " " ++ toString y) list |> String.join " "


toPoint : Position -> Point
toPoint position =
    ( toFloat position.x, toFloat position.y )


toPosition : Point -> Position
toPosition ( px, py ) =
    Position (round px) (round py)
