module Tangram.Types exposing (..)

import Char exposing (KeyCode)
import Window
import Mouse
import Piece.Types as Piece


type alias Name =
    String


type Msg
    = PieceMsg Name Piece.Msg
    | WindowSize Window.Size
    | KeyDown KeyCode
    | KeyUp KeyCode
    | GetLayout (Maybe String)
    | ToggleLayout
    | SetName String
    | Reset
    | Error
    | NoOp
