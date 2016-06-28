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
import String
import Task
import LocalStorage


type alias Model =
    { tangram : Tangram.Model
    , pointer : Mouse.Position
    , opening : Bool
    , savedTangrams : List Tangram.Name
    }


type Msg
    = TangramMsg Tangram.Msg
    | MousePos ( Mouse.Position, Mouse.Position )
    | MoveToOrigin
    | Reset
    | Open
    | SavedTangrams (List Tangram.Name)
    | OpenTangram Tangram.Name
    | LoadTangram (Maybe String)
    | Error LocalStorage.Error


init : Tangram.Model -> ( Model, Cmd Msg )
init tangram =
    { tangram = tangram
    , pointer = Mouse.Position 0 0
    , opening = False
    , savedTangrams = []
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

        Reset ->
            let
                ( tangram', tangramCmd ) =
                    Tangram.update Tangram.Reset model.tangram
            in
                { model | tangram = tangram' } ! [ Cmd.map TangramMsg tangramCmd ]

        Open ->
            { model | opening = True } ! [ Task.perform Error SavedTangrams getSavedTangrams ]

        SavedTangrams savedTangrams ->
            { model | savedTangrams = savedTangrams } ! []

        OpenTangram tangramName ->
            model ! [ LocalStorage.get tangramName |> Task.perform Error LoadTangram ]

        LoadTangram stringMaybe ->
            case stringMaybe of
                Just json ->
                    let
                        ( tangram', cmd ) =
                            Tangram.update (Tangram.UpdateLocations json) model.tangram
                    in
                        { model | tangram = tangram', opening = False } ! [ cmd |> Cmd.map TangramMsg ]

                _ ->
                    model ! []

        Error error ->
            model ! []


getSavedTangrams : Task.Task LocalStorage.Error (List String)
getSavedTangrams =
    LocalStorage.keys
        `Task.andThen` (List.filter (String.startsWith "tangram-") >> Task.succeed)


view : Model -> Html Msg
view model =
    Html.svg
        [-- VirtualDom.on "mousemove" (JD.map MousePos Piece.offsetPosition)
        ]
        [ Tangram.view model.tangram |> Html.App.map TangramMsg
        , Html.div []
            [ Html.button [ HE.onClick MoveToOrigin ] [ Html.text "move to origin" ]
            , Html.button [ HE.onClick Reset ] [ Html.text "reset" ]
            , Html.button [ HE.onClick Open ] [ Html.text "open" ]
            ]
          --, Html.div [] [ Html.text <| toString model.pointer ]
        , openView model
        ]


openView : Model -> Html Msg
openView model =
    case model.opening of
        True ->
            Html.ol [] (List.map savedTangramView model.savedTangrams)

        False ->
            Html.text ""


savedTangramView : Tangram.Name -> Html Msg
savedTangramView savedTangram =
    Html.li []
        [ Html.span [ HE.onClick (OpenTangram savedTangram) ] [ Html.text savedTangram ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Tangram.subscriptions model.tangram |> Sub.map TangramMsg
