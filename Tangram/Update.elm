module Tangram.Update exposing (update, layoutEncoder, moveToOrigin)

import Window
import Json.Encode as JE
import Json.Decode as JD
import Tangram.Model exposing (..)
import Tangram.Types exposing (..)
import Task
import Piece.Model as Piece
import Piece.Update as Piece
import Piece.Types as Piece
import Piece.View as Piece
import LocalStorage


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PieceMsg name pieceMsg ->
            let
                context =
                    { shift = model.shift, size = model.size }

                ( pieces', cmds ) =
                    updatePieces name pieceMsg context model.pieces

                model' =
                    { model | pieces = pieces' }
            in
                model' ! (saveCmd model' :: cmds)

        WindowSize wsize ->
            let
                -- Allow for margin and give vertical room for additional content
                width =
                    wsize.width - 16

                height =
                    wsize.height - 300
            in
                ( { model | size = Window.Size width height }, Cmd.none )

        Error ->
            ( model, Cmd.none )

        KeyDown keycode ->
            if keycode == 16 then
                ( { model | shift = True }, Cmd.none )
            else
                ( model, Cmd.none )

        KeyUp keycode ->
            if keycode == 16 then
                ( { model | shift = False }, Cmd.none )
            else
                ( model, Cmd.none )

        Reset ->
            let
                model' =
                    { model | pieces = tangramPieces }
            in
                model' ! [ saveCmd model' ]

        GetLayout stringMaybe ->
            case stringMaybe of
                Just json ->
                    let
                        locations =
                            JD.decodeString locationsDecoder json
                    in
                        updateLocations locations model ! []

                _ ->
                    model ! []

        ToggleLayout ->
            { model | showingLayout = not model.showingLayout } ! []

        SetName name ->
            { model | name = name } ! []

        NoOp ->
            model ! []


{-| Fold over the list of components and apply the msg to the component piece
with the matching name, collecting the updated models and resulting commands
into separate lists as needed for updating the main model and batching the
commands.
-}
updatePieces :
    Name
    -> Piece.Msg
    -> Piece.Context
    -> List ( Name, Piece.Model )
    -> ( List ( Name, Piece.Model ), List (Cmd Msg) )
updatePieces name msg context items =
    let
        updatePiece (( pieceName, piece ) as item) ( items, cmds ) =
            if pieceName == name then
                let
                    ( piece', cmd ) =
                        Piece.update context msg piece
                in
                    ( ( pieceName, piece' ) :: items, Cmd.map (PieceMsg name) cmd :: cmds )
            else
                ( item :: items, cmds )
    in
        List.foldr updatePiece ( [], [] ) items


layoutEncoder : List ( Name, Piece.Model ) -> JE.Value
layoutEncoder pieces =
    let
        help ( name, piece ) =
            JE.list [ JE.string name, Piece.locationEncoder piece ]
    in
        JE.list (List.map help pieces)


updateLocations : Result String (List ( Name, Piece.Location )) -> Model -> Model
updateLocations locationsResult model =
    case locationsResult of
        Ok locations ->
            List.foldr updateLocation model locations

        _ ->
            model


locationsDecoder : JD.Decoder (List ( Name, Piece.Location ))
locationsDecoder =
    JD.list <| JD.tuple2 (,) JD.string Piece.locationDecoder


{-| Change the location of the named piece.
-}
updateLocation : ( Name, Piece.Location ) -> Model -> Model
updateLocation ( name, location ) model =
    let
        updatePiece : ( Name, Piece.Model ) -> ( Name, Piece.Model )
        updatePiece ( nameP, piece ) =
            if name == nameP then
                ( nameP, Piece.withLocation location piece )
            else
                ( nameP, piece )

        pieces' =
            List.map updatePiece model.pieces
    in
        { model | pieces = pieces' }


{-| Command to save the entire layout: positions and rotations.
-}
saveCmd : Model -> Cmd Msg
saveCmd model =
    layoutEncoder model.pieces
        |> JE.encode 0
        |> LocalStorage.set (storageName model.name)
        |> Task.perform (always Error) (always NoOp)


bounds : Model -> ( Piece.Point, Piece.Point )
bounds tangram =
    List.map (snd >> Piece.vertices) tangram.pieces
        |> List.concat
        |> Piece.boundingBox


{-| Move entire tangram as far left and upward as possible while showing all of
all pieces.
-}
moveToOrigin : Model -> Model
moveToOrigin model =
    moveToPosition { x = 0, y = 0 } model


moveToPosition : Piece.Position -> Model -> Model
moveToPosition { x, y } model =
    let
        ( ( ox, oy ), corner ) =
            bounds model

        movePiece vector ( name, piece ) =
            ( name, Piece.move vector piece )

        pieces =
            List.map (movePiece ( toFloat x - ox, toFloat y - oy )) model.pieces
    in
        { model | pieces = pieces }
