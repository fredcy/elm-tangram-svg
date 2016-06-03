module Editor exposing (Model, Msg(TangramMsg), init, update, view, subscriptions)

import Html exposing (Html)
import Html.App
import Window
import Tangram.Model as Tangram
import Tangram.Types as Tangram
import Tangram.Update as Tangram
import Tangram.View as Tangram


type alias Model =
    { tangram : Tangram.Model
    }


type Msg
    = TangramMsg Tangram.Msg


init : Tangram.Model -> ( Model, Cmd Msg )
init tangram =
    { tangram = tangram } ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TangramMsg tmsg ->
            let
                ( tmodel, tcmd ) =
                    Tangram.update tmsg model.tangram
            in
                { model | tangram = tmodel } ! [ Cmd.map TangramMsg tcmd ]


view : Model -> Html Msg
view model =
    Tangram.view model.tangram |> Html.App.map TangramMsg


subscriptions : Model -> Sub Msg
subscriptions model =
    Tangram.subscriptions model.tangram |> Sub.map TangramMsg
