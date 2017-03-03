module Editor exposing (Model, Msg(TangramMsg), init, update, view, subscriptions)

import Html exposing (Html)
import Svg
import Html.Events as HE
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
    | SavedTangrams (Result LocalStorage.Error (List Tangram.Name))
    | OpenTangram Tangram.Name
    | LoadTangram (Result LocalStorage.Error (Maybe String))


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
                ( tangram_, tangramCmd ) =
                    Tangram.update Tangram.Reset model.tangram
            in
                { model | tangram = tangram_ } ! [ Cmd.map TangramMsg tangramCmd ]

        Open ->
            { model | opening = True } ! [ Task.attempt SavedTangrams getSavedTangrams ]

        SavedTangrams savedTangramsResult ->
            case savedTangramsResult of
                Ok savedTangrams ->
                    { model | savedTangrams = savedTangrams } ! []

                Err error ->
                    model ! []

        OpenTangram tangramName ->
            model ! [ LocalStorage.get tangramName |> Task.attempt LoadTangram ]

        LoadTangram stringMaybeResult ->
            case stringMaybeResult of
                Ok stringMaybe ->
                    case stringMaybe of
                        Just json ->
                            let
                                ( tangram_, cmd ) =
                                    Tangram.update (Tangram.UpdateLocations json) model.tangram
                            in
                                { model | tangram = tangram_, opening = False } ! [ cmd |> Cmd.map TangramMsg ]

                        _ ->
                            model ! []

                Err _ ->
                    model ! []


getSavedTangrams : Task.Task LocalStorage.Error (List String)
getSavedTangrams =
    LocalStorage.keys
        |> Task.andThen (List.filter (String.startsWith "tangram-") >> Task.succeed)


view : Model -> Html Msg
view model =
    Html.div
        [-- VirtualDom.on "mousemove" (JD.map MousePos Piece.offsetPosition)
        ]
        [ Tangram.view model.tangram |> Html.map TangramMsg
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
