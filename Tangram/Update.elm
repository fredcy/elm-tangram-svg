module Tangram.Update exposing (update, layoutEncoder, moveToOrigin)

import List.Extra as List
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

                ( pieces_, cmds ) =
                    updatePieces name pieceMsg context model.pieces

                model_ =
                    { model | pieces = bringToTop name pieces_ }
            in
                model_ ! (saveCmd model_ :: cmds)

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
                model_ =
                    { model | pieces = tangramPieces }
            in
                model_ ! [ saveCmd model_ ]

        GetLayout stringMaybeResult ->
            case stringMaybeResult of
                Ok stringMaybe ->
                    case stringMaybe of
                        Just json ->
                            update (UpdateLocations json) model

                        _ ->
                            model ! []

                Err error ->
                    model ! []

        UpdateLocations json ->
            let
                locations =
                    JD.decodeString locationsDecoder json
            in
                updateLocations locations model ! []

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
                    ( piece_, cmd ) =
                        Piece.update context msg piece
                in
                    ( ( pieceName, piece_ ) :: items, Cmd.map (PieceMsg name) cmd :: cmds )
            else
                ( item :: items, cmds )
    in
        List.foldr updatePiece ( [], [] ) items


bringToTop : Name -> List ( Name, Piece.Model ) -> List ( Name, Piece.Model )
bringToTop name items =
    let
        pieceMaybe =
            findPiece name items
    in
        case pieceMaybe of
            Just piece ->
                removePiece name items ++ [ ( name, piece ) ]

            Nothing ->
                items


findPiece : Name -> List ( Name, Piece.Model ) -> Maybe Piece.Model
findPiece name items =
    List.find (Tuple.first >> ((==) name)) items |> Maybe.map Tuple.second


removePiece : Name -> List ( Name, Piece.Model ) -> List ( Name, Piece.Model )
removePiece name items =
    List.filter (Tuple.first >> ((/=) name)) items


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
            let
                pieces_ =
                    List.map (updatePieceLayout model.pieces) locations
            in
                { model | pieces = pieces_ }

        _ ->
            model


locationsDecoder : JD.Decoder (List ( Name, Piece.Location ))
locationsDecoder =
    JD.list <| JD.map2 (,) JD.string Piece.locationDecoder


{-| Find named piece in list of named pieces and update its location.
-}
updatePieceLayout : List ( Name, Piece.Model ) -> ( Name, Piece.Location ) -> ( Name, Piece.Model )
updatePieceLayout pieces ( name, location ) =
    let
        pieceM =
            List.find ((==) name << Tuple.first) pieces
    in
        case pieceM of
            Just ( name, piece ) ->
                ( name, Piece.withLocation location piece )

            _ ->
                Debug.crash "piece not found" name


{-| Command to save the entire layout: positions and rotations.
-}
saveCmd : Model -> Cmd Msg
saveCmd model =
    layoutEncoder model.pieces
        |> JE.encode 0
        |> LocalStorage.set (storageName model.name)
        |> Task.attempt (always NoOp)


{-| Get bounding box of entire tangram by collecting all vertices of all pieces
and then finding the extremes.
-}
bounds : Model -> ( Piece.Point, Piece.Point )
bounds tangram =
    List.map (Tuple.second >> Piece.vertices) tangram.pieces
        |> List.concat
        |> Piece.boundingBox


{-| Move entire tangram as far left and upward as possible while showing all of
all pieces.
-}
moveToOrigin : Model -> Model
moveToOrigin model =
    moveToPosition { x = 0, y = 0 } model


{-| Move all pieces by same vector so as to put the origin of bounding box at
   the given position.
-}
moveToPosition : Piece.Position -> Model -> Model
moveToPosition { x, y } model =
    let
        ( ( ox, oy ), _ ) =
            bounds model

        vector =
            ( toFloat x - ox, toFloat y - oy )

        movePiece ( name, piece ) =
            ( name, Piece.move vector piece )

        pieces =
            List.map movePiece model.pieces
    in
        { model | pieces = pieces }
