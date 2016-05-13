module Main exposing (..)

import Html
import Html.App as Html
import Window
import Keyboard
import Piece.Model as Piece
import Piece.Update as Piece
import Piece.View as Piece
import Piece.Types as Piece
import Model exposing (..)
import Types exposing (..)
import Update exposing (..)
import View exposing (..)


main : Program Never
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


{-| Gather the subscriptions of each of the components into a single Batch.
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    let
        mapSubs ( name, piece ) =
            Piece.subscriptions piece |> Sub.map (PieceMsg name)

        reSize =
            Window.resizes WindowSize

        keyDowns =
            Keyboard.downs KeyDown

        keyUps =
            Keyboard.ups KeyUp
    in
        keyUps :: keyDowns :: reSize :: List.map mapSubs model.pieces |> Sub.batch
