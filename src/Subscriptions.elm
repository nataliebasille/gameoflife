module Subscriptions exposing (..)

import Model exposing (..)
import Messages exposing (..)
import Time

subscriptions : Model -> Sub Message
subscriptions model =
    if model.playing then
        Time.every (1000 / toFloat model.speedMultiplier) (\_ -> NextGeneration)

    else
        Sub.none