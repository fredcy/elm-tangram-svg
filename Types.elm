module Types exposing (..)

import Char exposing (KeyCode)
import Window
import Piece.Types as Piece


type alias Name =
    String


type Msg
    = PieceMsg Name Piece.Msg
    | WindowSize Window.Size
    | KeyDown KeyCode
    | KeyUp KeyCode
    | Error
