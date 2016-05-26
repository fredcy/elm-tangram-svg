module Update exposing (update)

import Window
import Json.Encode as JE
import Model exposing (..)
import Types exposing (..)
import Task
import Piece.Model as Piece
import Piece.Update as Piece
import Piece.Types as Piece
import LocalStorage


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg |> Debug.log "msg" of
        PieceMsg name pieceMsg ->
            let
                context =
                    { shift = model.shift, size = model.size }

                ( pieces', cmds ) =
                    updatePieces name pieceMsg context model.pieces

                save =
                    layoutEncoder pieces'
                        |> JE.encode 0
                        |> Debug.log "layout"
                        |> LocalStorage.set "tangram"
                        |> Task.perform (always Error) (always NoOp)
            in
                ( { model | pieces = pieces' }, Cmd.batch (save :: cmds) )

        WindowSize wsize ->
            let
                -- Allow for horizontal margin (?) and give vertical room for additional content
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
            { model | pieces = tangramPieces } ! []

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
            JE.list [ JE.string name, Piece.encoder piece ]
    in
        JE.list (List.map help pieces)
