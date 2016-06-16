module Editor exposing (Model, Msg(TangramMsg), init, update, view, subscriptions)

import Html exposing (Html)
import Html.Events as HE
import Html.App
import Json.Decode as JD
import Mouse
import VirtualDom
import Tangram.Model as Tangram
import Tangram.Types as Tangram
import Tangram.Update as Tangram
import Tangram.View as Tangram
import Piece.View as Piece


type alias Model =
    { tangram : Tangram.Model
    , pointer : Mouse.Position
    }


type Msg
    = TangramMsg Tangram.Msg
    | MousePos ( Mouse.Position, Mouse.Position )
    | MoveToOrigin


init : Tangram.Model -> ( Model, Cmd Msg )
init tangram =
    { tangram = tangram
    , pointer = Mouse.Position 0 0
    }
        ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TangramMsg tmsg ->
            let
                ( tmodel, tcmd ) =
                    Tangram.update tmsg model.tangram
            in
                { model | tangram = tmodel } ! [ Cmd.map TangramMsg tcmd ]

        MousePos ( pagePos, offsetPos ) ->
            { model | pointer = offsetPos } ! []

        MoveToOrigin ->
            { model | tangram = Tangram.moveToOrigin model.tangram } ! []


view : Model -> Html Msg
view model =
    Html.svg [ VirtualDom.on "mousemove" (JD.map MousePos Piece.offsetPosition) ]
        [ Tangram.view model.tangram |> Html.App.map TangramMsg
        , Html.div []
            [ Html.button [ HE.onClick MoveToOrigin ] [ Html.text "move to origin" ] ]
        , Html.div [] [ Html.text <| toString model.pointer ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Tangram.subscriptions model.tangram |> Sub.map TangramMsg
