module View exposing (..)

import Css exposing (..)
import FontAwesome.Attributes as Icon
import FontAwesome.Brands as Icon
import FontAwesome.Icon as Icon exposing (Icon)
import FontAwesome.Layering as Icon
import FontAwesome.Solid as Icon
import FontAwesome.Styles as Icon
import Html
import Html.Attributes exposing (style)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, id)
import Html.Styled.Events exposing (..)
import Messages exposing (..)
import Model exposing (..)
import World exposing (CellState(..))


type alias Palette =
    { main : Color
    , light : Color
    , lighter : Color
    , dark : Color
    , darker : Color
    }


colorToString : Color -> String
colorToString color =
    "rgba(" ++ String.fromInt color.red ++ "," ++ String.fromInt color.green ++ "," ++ String.fromInt color.blue ++ "," ++ String.fromFloat color.alpha ++ ")"


theme :
    { primary : Palette
    , secondary : Palette
    , accent : Palette
    , background : Color
    }
theme =
    { primary =
        { main = hex "52206B"
        , light = hex "673983"
        , lighter = hex "8960A0"
        , dark = hex "380C50"
        , darker = hex "220234"
        }
    , secondary =
        { main = hex "37266E"
        , light = hex "503F87"
        , lighter = hex "7667A5"
        , dark = hex "201152"
        , darker = hex "110535"
        }
    , accent =
        { main = hex "812258"
        , light = hex "9D3F74"
        , lighter = hex "C06E9C"
        , dark = hex "600A3B"
        , darker = hex "3E0023"
        }
    , background = hex "EEEEEE"
    }


playButton : Model -> Html Message
playButton model =
    span
        [ css
            [ color theme.background
            , backgroundColor theme.primary.main
            , border3 (px 1) solid theme.primary.lighter
            , borderRadius (rem 0.25)
            , padding (rem 0.5)
            , fontSize (rem 1.25)
            , width (px 42)
            , height (px 42)
            , displayFlex
            , alignItems center
            , justifyContent center
            , cursor pointer
            , hover
                [ backgroundColor theme.primary.light
                ]
            ]
        , (if model.playing then
            StopPlaying

           else
            StartPlaying
          )
            |> onClick
        ]
        [ (if model.playing then
            Icon.pause

           else
            Icon.play
          )
            |> Icon.present
            |> Icon.view
            |> Html.Styled.fromUnstyled
        ]


speedSelector : Model -> Html Message
speedSelector model =
    let
        selectableSpeeds =
            [ 1, 2, 4, 8 ]

        toSpeedButton : Int -> Html Message
        toSpeedButton speed =
            div
                [ css
                    [ backgroundColor theme.primary.main
                    , displayFlex
                    , alignItems center
                    , justifyContent center
                    , hover
                        [ backgroundColor theme.primary.light
                        ]
                    ]
                , css
                    (if model.speedMultiplier == speed then
                        [ backgroundColor theme.primary.dark |> important ]

                     else
                        []
                    )
                , SetSpeed speed |> onClick
                ]
                [ text ("x" ++ String.fromInt speed) ]
    in
    div
        [ css
            [ property "display" "grid"
            , property "grid-template-columns" ("repeat(" ++ (List.length selectableSpeeds |> String.fromInt) ++ ", 42px)")
            , property "gap" "1px"
            , height (px 42)
            , backgroundColor theme.primary.lighter
            , color theme.background
            , border3 (px 1) solid theme.primary.lighter
            , borderRadius (rem 0.25)
            , cursor pointer
            , fontSize (rem 1.25)
            ]
        ]
        (selectableSpeeds
            |> List.map toSpeedButton
        )


actionBar : Model -> Html Message
actionBar model =
    div
        [ css
            [ backgroundColor theme.primary.main
            , height (pct 100)
            , property "display" "grid"
            , property "grid-template-rows" "1fr"
            , property "grid-template-columns" " max-content max-content max-content"
            , property "align-items" "center"
            , property "gap" "1rem"
            , padding2 (px 0) (px 15)
            , color theme.background
            ]
        ]
        [ span
            [ css
                [ fontWeight bold
                , letterSpacing (px 1)
                , alignSelf stretch
                , displayFlex
                , alignItems center
                , borderRight3 (px 1) solid theme.primary.lighter
                , paddingRight (px 15)
                ]
            ]
            [ text "NATALIE'S GAME OF LIFE - ELM" ]
        , playButton model
        , speedSelector model
        ]


world : Model -> Html Message
world model =
    let
        ( viewportWidth, viewportHeight ) =
            model.contentViewport

        rows =
            toFloat model.world.rows

        columns =
            toFloat model.world.columns

        squareSize =
            if viewportWidth < viewportHeight then
                (viewportWidth - rows - 1) / rows

            else
                (viewportHeight - columns - 1) / columns
    in
    div
        [ css
            [ property "display" "grid"
            , property "grid-template-rows" ("repeat(" ++ String.fromInt model.world.rows ++ ", 1fr)")
            , property "grid-template-columns" ("repeat(" ++ String.fromInt model.world.columns ++ ", 1fr)")
            , property "gap" "1px"
            , border3 (px 1) solid theme.primary.main
            , backgroundColor theme.primary.main
            , width (px (rows * squareSize))
            , height (px (columns * squareSize))
            ]
        ]
        (List.indexedMap
            (\index ->
                \cellState ->
                    div
                        [ css
                            (if cellState == Alive then
                                [ backgroundColor theme.secondary.main ]

                             else
                                [ backgroundColor theme.background ]
                            )
                        , ToggleCell index |> onClick
                        ]
                        []
            )
            model.world.board
        )


view : Model -> Html.Html Message
view model =
    Html.div
        [ style "display" "grid"
        , style "width" "100vw"
        , style "height" "100vh"
        , style "grid-template-rows" "50px 1fr "
        , style "grid-template-columns" "1fr"
        , style "background-color" (colorToString theme.background)
        ]
        [ (actionBar >> toUnstyled) model
        , div
            [ Html.Styled.Attributes.id "page-content"
            , css
                [ displayFlex
                , alignItems center
                , justifyContent center
                ]
            ]
            [ world model ]
            |> toUnstyled
        ]



-- view model =
--     div
--         [ class "page-content"
--         ]
--         [ button
--             [ (if model.playing then
--                 StopPlaying
--                else
--                 StartPlaying
--               )
--                 |> onClick
--             ]
--             [ (if model.playing then
--                 "Stop"
--                else
--                 "Play"
--               )
--                 |> text
--             ]
--         , actionBar
--         , div
--             [ class "gameboard"
--             , style "width" (String.fromInt (model.world.cellSize * model.world.rows) ++ "px")
--             , style "height" (String.fromInt (model.world.cellSize * model.world.columns) ++ "px")
--             , style "grid-template-rows" ("repeat(" ++ String.fromInt model.world.rows ++ ", 1fr)")
--             , style "grid-template-columns" ("repeat(" ++ String.fromInt model.world.columns ++ ", 1fr)")
--             ]
--             (List.indexedMap
--                 (\index ->
--                     \cellState ->
--                         div
--                             [ classList [ ( "cell", True ), ( "alive", cellState == Alive ) ]
--                             , ToggleCell index |> onClick
--                             ]
--                             []
--                 )
--                 model.world.board
--             )
--         ]
