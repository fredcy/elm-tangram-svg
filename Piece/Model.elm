module Piece.Model exposing (Model, init, getPosition, getRotation, rotating, subscriptions)

import Mouse
import Piece.Types exposing (..)
import Svg exposing (Svg)
import Json.Encode as JE


type alias Model =
    { shape : Shape
    , position : Position
    , rotation : Rotation
    , drag : Maybe Drag
    , origin : Position
    }


encode : Model -> JE.Value
encode model =
    JE.object
        [ ( "position", encodePosition model.position )
        , ( "rotation", JE.float model.rotation )
        , ( "shape", encodeShape model.shape )
        ]


encodePosition : Position -> JE.Value
encodePosition position =
    JE.object
        [ ( "x", JE.int position.x )
        , ( "y", JE.int position.y )
        ]


encodeShape : Shape -> JE.Value
encodeShape shape =
    JE.string "TODO"


init : Shape -> Position -> Rotation -> Model
init shape position rotation =
    Model shape position (degrees rotation) Nothing (Position 0 0)


getPosition : Model -> Position
getPosition { position, drag } =
    case drag of
        Just (Dragging { start, current }) ->
            Position (position.x + current.x - start.x)
                (position.y + current.y - start.y)

        _ ->
            position


rotating : Model -> Bool
rotating model =
    case model.drag of
        Just (Rotating _) ->
            True

        _ ->
            False


{-| Calculate current rotation while in rotating state by sampling a point after
the user has dragged out a bit, creating a basis vector relative to the start
point of the drag, then calculating the vactor of the current mouse location
ralative to the drag-start. The rotation is adjusted by the difference of those
vectors, effectively giving a "handle" deterined by the sample vector. Note that
the handle rotation is about the drag-start point, not the rotational center of
the object (not sure how to reconcile mouse coords with the SVG space
coords).
-}
getRotation : Model -> Rotation
getRotation { position, rotation, drag } =
    case drag of
        Just (Rotating { start, sample, current }) ->
            case sample of
                Just samplexy ->
                    rotation - relativeRotation position samplexy current

                Nothing ->
                    rotation

        _ ->
            rotation


relativeRotation : Position -> Position -> Position -> Float
relativeRotation start sample current =
    let
        sampleAngle =
            vectorAngle (vectorDiff sample start)

        currentAngle =
            vectorAngle (vectorDiff current start)
    in
        currentAngle - sampleAngle


{-| Angle of point interpreted as vector, in radians
-}
vectorAngle : Position -> Float
vectorAngle v =
    atan2 (toFloat v.x) (toFloat v.y)


vectorDiff : Position -> Position -> Position
vectorDiff v1 v2 =
    { x = v2.x - v1.x, y = v2.y - v1.y }


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.drag of
        Nothing ->
            Sub.none

        Just _ ->
            Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd ]
