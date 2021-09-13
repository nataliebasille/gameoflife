module Messages exposing (..)

import Browser.Dom

type Message
    = ToggleCell Int
    | NextGeneration
    | StartPlaying
    | StopPlaying
    | SetSpeed Int
    | SetContentViewport (Result Browser.Dom.Error (Float, Float))