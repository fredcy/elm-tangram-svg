module Tangram.Types exposing (..)

import Char exposing (KeyCode)
import Window
import Piece.Types as Piece
import Piece.Model as Piece


type alias Name =
    String


type alias SavedTangram =
    List ( Name, Piece.Location )


type Msg
    = PieceMsg Name Piece.Msg
    | WindowSize Window.Size
    | KeyDown KeyCode
    | KeyUp KeyCode
    | GetLayout (Maybe String)
    | UpdateLocations String
    | ToggleLayout
    | SetName String
    | Reset
    | Error
    | NoOp
