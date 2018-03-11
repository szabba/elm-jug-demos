module Stoper exposing (main)

import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Card as Card
import Bootstrap.Card.Block as CardBlock
import Bootstrap.Grid as Grid
import Html as H exposing (Html)
import Html.Attributes as HA
import Time exposing (Time)


main : Program Never Model Msg
main =
    H.program
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type Model
    = Loaded
    | Started
    | Running { start : Time, now : Time, log : List Time }
    | Stopped { start : Time, now : Time, log : List Time }


type Msg
    = Start
    | Tick Time
    | Remember
    | Stop
    | Resume
    | Reset


view : Model -> Html Msg
view model =
    Grid.container []
        [ CDN.stylesheet
        , Grid.row []
            [ Grid.col []
                [ case model of
                    Loaded ->
                        viewWithoutState { active = True }

                    Started ->
                        viewWithoutState { active = False }

                    Running state ->
                        viewWithState { active = True } state

                    Stopped state ->
                        viewWithState { active = False } state
                ]
            ]
        ]


viewWithoutState : { active : Bool } -> Html Msg
viewWithoutState { active } =
    let
        options =
            if active then
                [ Button.onClick Start, Button.primary ]
            else
                [ Button.disabled True ]
    in
        Card.config []
            |> Card.block []
                [ CardBlock.text []
                    [ Button.button options [ H.text "Start" ] ]
                ]
            |> Card.view


viewWithState : { active : Bool } -> { now : Time, start : Time, log : List Time } -> Html Msg
viewWithState { active } state =
    let
        buttons =
            if active then
                [ Button.button [ Button.onClick Stop ] [ H.text "Stop" ]
                , Button.button [ Button.onClick Remember, Button.primary ] [ H.text "Remember" ]
                ]
            else
                [ Button.button [ Button.onClick Reset ] [ H.text "Reset" ]
                , Button.button [ Button.onClick Resume, Button.primary ] [ H.text "Resume" ]
                ]
    in
        Card.config []
            |> Card.block [] [ CardBlock.text [] buttons ]
            |> viewNow state
            |> viewLog state
            |> Card.view


viewNow : { a | now : Time, start : Time } -> Card.Config msg -> Card.Config msg
viewNow { start, now } =
    Card.block
        [ CardBlock.primary
        , CardBlock.attrs [ HA.class "font-weight-bold" ]
        ]
        [ CardBlock.text [] [ timeToEntry start now ] ]


viewLog : { a | start : Time, log : List Time } -> Card.Config msg -> Card.Config msg
viewLog { start, log } =
    let
        fullLog =
            log ++ [ start ]

        entries =
            fullLog |> List.map (timeToEntry start)
    in
        Card.block []
            [ CardBlock.text [] entries ]


timeToEntry : Time -> Time -> Html msg
timeToEntry start t =
    let
        delta =
            t - start

        hms =
            String.join ":"
                [ delta |> wholeHours |> toString
                , delta |> wholeMinutes |> toString
                , delta |> without minute |> Time.inSeconds |> toString
                ]
    in
        H.p [] [ H.text hms ]


wholeHours : Time -> Int
wholeHours t =
    t |> Time.inHours |> truncate


wholeMinutes : Time -> Int
wholeMinutes t =
    t |> without hour |> Time.inMinutes |> truncate


{-| A function that returns the part of the Time that is smaller than a unit.

    (90 * Time.second) |> without minute

should be close to 30 seconds.

-}
without : Unit -> Time -> Time
without { inUnit, unit } t =
    let
        wholeUnits =
            t |> inUnit |> truncate |> toFloat
    in
        t - wholeUnits * unit


type alias Unit =
    { inUnit : Time -> Float
    , unit : Time
    }


hour : Unit
hour =
    { inUnit = Time.inHours, unit = Time.hour }


minute : Unit
minute =
    { inUnit = Time.inMinutes, unit = Time.minute }


init : ( Model, Cmd Msg )
init =
    ( Loaded
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Started ->
            ticks

        Running _ ->
            ticks

        _ ->
            Sub.none


ticks : Sub Msg
ticks =
    Time.every (Time.second / 10) Tick


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model |> simpleUpdate msg
    , Cmd.none
    )


simpleUpdate : Msg -> Model -> Model
simpleUpdate msg model =
    case ( msg, model ) of
        ( Start, Loaded ) ->
            Started

        ( Tick t, Started ) ->
            Running
                { now = t, start = t, log = [] }

        ( Tick t, Running state ) ->
            Running
                { state | now = t }

        ( Remember, Running state ) ->
            let
                log =
                    state.now :: state.log
            in
                Running
                    { state | log = log }

        ( Stop, Started ) ->
            Loaded

        ( Stop, Running state ) ->
            Stopped state

        ( Resume, Stopped state ) ->
            Running state

        ( Reset, Stopped state ) ->
            Running
                { state | start = state.now, log = [] }

        _ ->
            model
