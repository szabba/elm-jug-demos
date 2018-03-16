module Values exposing (..)

import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Card as Card
import Bootstrap.Card.Block as CardBlock
import Bootstrap.Grid as Grid
import Dict exposing (Dict)
import Html as H exposing (Attribute, Html)
import Html.Attributes as HA
import Html.Events as HE
import Set exposing (Set)


main : Program Never Model Msg
main =
    H.beginnerProgram
        { model = init
        , update = update
        , view = view
        }


type Model
    = BestIs Answer
    | Thinking Answer (Dict String String)


type Answer
    = NoAnswer
    | Answer (Dict String Int)


type Msg
    = StartThinking
    | AddValue String
    | SetValue String String
    | RemoveValue String
    | Cancel
    | AcceptAnswer (Dict String Int)


init : Model
init =
    BestIs NoAnswer


view : Model -> Html Msg
view model =
    withWrapper
        [ case model of
            BestIs NoAnswer ->
                whatIsBest

            BestIs (Answer answer) ->
                bestIs answer

            Thinking answer dirtyAnswer ->
                thinkingSpace dirtyAnswer
        ]


withWrapper : List (Html Msg) -> Html Msg
withWrapper elements =
    Grid.container []
        [ CDN.stylesheet
        , Grid.row []
            [ Grid.col [] elements ]
        ]


whatIsBest : Html Msg
whatIsBest =
    Button.button
        [ Button.onClick StartThinking, Button.primary ]
        [ H.text "What is best in life?" ]


bestIs : Dict String Int -> Html Msg
bestIs weights =
    H.div []
        [ viewWeights weights
        , H.button [ HA.class "btn btn-primary", HE.onClick StartThinking ]
            [ H.text "Are these truly best?" ]
        ]


viewWeights : Dict String comparable -> Html msg
viewWeights weights =
    weights
        |> Dict.toList
        |> List.sortBy Tuple.second
        |> List.map viewWeight
        |> H.div []


viewWeight : ( String, a ) -> Html msg
viewWeight ( value, weight ) =
    H.p []
        [ H.span [ HA.style [ ( "display", "inline-block" ), ( "width", "10em" ) ] ]
            [ H.text value ]
        , H.text <| toString weight
        ]


thinkingSpace : Dict String String -> Html Msg
thinkingSpace weights =
    Card.config []
        |> Card.block [] [ CardBlock.text [] [ sumError weights ] ]
        |> Card.block [] [ CardBlock.text [] [ sliders weights ] ]
        |> viewValueSelector weights
        |> Card.block [] [ CardBlock.text [] [ buttons weights ] ]
        |> Card.view


sliders : Dict String String -> Html Msg
sliders weights =
    weights
        |> Dict.map valueBox
        |> Dict.toList
        |> List.map Tuple.second
        |> H.div []


sumError : Dict String String -> Html Msg
sumError weights =
    let
        sum =
            weightsSum weights
    in
    if sum /= 100 then
        H.p [ HA.class "text-danger" ]
            [ H.text <| "The weights add up to " ++ toString sum ++ " not 100!" ]
    else
        H.text ""


buttons : Dict String String -> Html Msg
buttons weights =
    let
        currentAnswer =
            weights
                |> Dict.toList
                |> List.filterMap (\( k, v ) -> v |> String.toInt |> Result.map ((,) k) |> Result.toMaybe)
                |> Dict.fromList
    in
    H.p []
        [ Button.button
            [ Button.onClick Cancel ]
            [ H.text "Cancel" ]
        , Button.button
            [ Button.disabled (weightsSum weights /= 100)
            , Button.primary
            , Button.onClick (AcceptAnswer currentAnswer)
            ]
            [ H.text "Accept" ]
        ]


weightsSum : Dict String String -> Int
weightsSum weights =
    weights
        |> Dict.values
        |> List.map (String.toInt >> Result.withDefault 0)
        |> List.sum


valueBox : String -> String -> Html Msg
valueBox value weight =
    let
        input type_ =
            H.input
                [ HE.onInput (SetValue value)
                , HA.id value
                , HA.type_ type_
                , HA.min "1"
                , HA.max "100"
                , HA.value weight
                ]
                []
    in
    H.div []
        [ H.button
            [ HA.class "btn btn-danger", HE.onClick (RemoveValue value) ]
            [ H.text "-" ]
        , H.label
            [ HA.style [ ( "width", "10em" ) ], HA.for value ]
            [ H.text value ]
        , input "range"
        , input "number"
        , valueError value weight
        ]


valueError : String -> String -> Html msg
valueError value weight =
    let
        noLessThan min n =
            if n < min then
                Err <| toString n ++ " is below " ++ toString min ++ "!"
            else
                Ok n

        noMoreThan max n =
            if n > max then
                Err <| toString n ++ " is above " ++ toString max ++ "!"
            else
                Ok n
    in
    weight
        |> String.toInt
        |> Result.mapError (\_ -> "Not a whole number!")
        |> Result.andThen (noLessThan 1)
        |> Result.andThen (noMoreThan 100)
        |> resultSwap
        |> Result.map H.text
        |> Result.map List.singleton
        |> Result.map (H.p [ HA.class "text-danger" ])
        |> Result.withDefault (H.text "")


resultSwap : Result value a -> Result a value
resultSwap res =
    case res of
        Err err ->
            Ok err

        Ok val ->
            Err val


viewValueSelector : Dict String String -> Card.Config Msg -> Card.Config Msg
viewValueSelector weights =
    let
        weightedValues =
            weights
                |> Dict.keys
                |> Set.fromList

        unweightedValues =
            Set.diff values weightedValues
    in
    Card.block []
        [ CardBlock.text [] <|
            if Set.isEmpty unweightedValues then
                [ H.text "No more values to add." ]
            else
                [ valuesSelect unweightedValues ]
        ]


valuesSelect : Set String -> Html Msg
valuesSelect values =
    values
        |> Set.toList
        |> List.map valueOption
        |> H.select []


valueOption : String -> Html Msg
valueOption value =
    H.option [ HA.value value, HE.onClick (AddValue value) ] [ H.text value ]


update : Msg -> Model -> Model
update msg model =
    case ( msg, model ) of
        ( StartThinking, BestIs answer ) ->
            Thinking answer Dict.empty

        ( AddValue value, Thinking answer weights ) ->
            let
                newWeights =
                    weights |> Dict.insert value "1"
            in
            Thinking answer newWeights

        ( SetValue value weight, Thinking answer weights ) ->
            let
                newWeights =
                    weights |> Dict.insert value weight
            in
            Thinking answer newWeights

        ( RemoveValue value, Thinking answer weights ) ->
            let
                newWeights =
                    weights |> Dict.remove value
            in
            Thinking answer newWeights

        ( AcceptAnswer answer, _ ) ->
            BestIs <| Answer answer

        ( Cancel, Thinking answer _ ) ->
            BestIs answer

        _ ->
            model


values : Set String
values =
    Set.fromList
        [ "Challenge"
        , "Comfort"
        , "Community"
        , "Freedom"
        , "Mastery"
        , "Status"
        ]
