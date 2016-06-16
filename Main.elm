module Main exposing (..)

import Html exposing (Html)
import Html.App as Html
import Editor
import Tangram.Model as Tangram
import Tangram.Types as Tangram
import Tangram.Update as Tangram


--import Tangram.View as Tangram


main : Program Never
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { tangram : Tangram.Model
    , editor : Editor.Model
    }


type Msg
    = TangramMsg Tangram.Msg
    | EditorMsg Editor.Msg


init : ( Model, Cmd Msg )
init =
    let
        ( tmodel, tcmd ) =
            Tangram.init

        ( etangram, ecmd ) =
            Tangram.init

        ( editorModel, editorCmd ) =
            Editor.init etangram
    in
        { tangram = tmodel
        , editor = editorModel
        }
            ! [ Cmd.map TangramMsg tcmd
              , Cmd.map (EditorMsg << Editor.TangramMsg) ecmd
              , Cmd.map EditorMsg editorCmd
              ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg |> Debug.log "main msg" of
        TangramMsg tmsg ->
            let
                ( tmodel, tcmd ) =
                    Tangram.update tmsg model.tangram
            in
                { model | tangram = tmodel } ! [ Cmd.map TangramMsg tcmd ]

        EditorMsg emsg ->
            let
                ( emodel, ecmd ) =
                    Editor.update emsg model.editor
            in
                { model | editor = emodel } ! [ Cmd.map EditorMsg ecmd ]


view : Model -> Html Msg
view model =
    Html.div []
        [ Editor.view model.editor |> Html.map EditorMsg
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Tangram.subscriptions model.tangram |> Sub.map TangramMsg
        , Editor.subscriptions model.editor |> Sub.map EditorMsg
        ]
