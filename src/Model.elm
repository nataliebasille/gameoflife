module Model exposing (..)

import Browser.Dom exposing (getViewportOf)
import Messages exposing (..)
import Task
import World exposing (..)


type alias Model =
    { playing : Bool
    , speedMultiplier : Int
    , world : GameBoard
    , contentViewport : ( Float, Float )
    }


getContentViewport : Cmd Message
getContentViewport =
    getViewportOf "page-content"
        |> Task.andThen (\viewport -> Task.succeed ( viewport.viewport.width, viewport.viewport.height ))
        |> Task.attempt SetContentViewport


init : () -> ( Model, Cmd Message )
init _ =
    ( { playing = False
      , speedMultiplier = 1
      , world = World.create 20 20
      , contentViewport = ( 0, 0 )
      }
    , getContentViewport
    )


update : Message -> Model -> ( Model, Cmd Message )
update msg model =
    case msg of
        ToggleCell index ->
            ( { model | playing = False, world = model.world |> World.toggleCell index }, Cmd.none )

        NextGeneration ->
            ( { model | world = model.world |> World.nextGeneration }, Cmd.none )

        StartPlaying ->
            ( { model | playing = True }, Cmd.none )

        StopPlaying ->
            ( { model | playing = False }, Cmd.none )

        SetSpeed multiplier ->
            ( { model | speedMultiplier = multiplier }, Cmd.none )

        SetContentViewport result ->
            case result of
                Ok viewport ->
                    ( { model | contentViewport = viewport }, Cmd.none )

                Err _ ->
                    ( model, getContentViewport )
