module Main exposing (main)

import Browser
import Messages exposing (Message)
import Model exposing (Model, init, update)
import Subscriptions exposing (subscriptions)
import View exposing (view)


main : Program () Model Message
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }
