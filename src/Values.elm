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


type alias Model =
    Dict String String


type Msg
    = AddValue String
    | SetValue String String
    | RemoveValue String
    | Reset


init : Model
init =
    Dict.empty


view : Model -> Html Msg
view model =
    withWrapper <|
        thinkingSpace model


withWrapper : Html Msg -> Html Msg
withWrapper child =
    Grid.container []
        [ CDN.stylesheet
        , Grid.row []
            [ Grid.col [] [ child ] ]
        ]


thinkingSpace : Model -> Html Msg
thinkingSpace weights =
    Card.config []
        |> Card.block []
            [ CardBlock.text []
                [ H.text "What is best in life?" ]
            ]
        |> Card.block [] [ CardBlock.text [] [ sumError weights ] ]
        |> Card.block [] [ CardBlock.text [] [ sliders weights ] ]
        |> viewValueSelector weights
        |> Card.block [] [ CardBlock.text [] [ buttons weights ] ]
        |> Card.view


viewWeights : Model -> Html Msg
viewWeights weights =
    weights
        |> Dict.toList
        |> List.sortBy Tuple.second
        |> List.map viewWeight
        |> H.div []


viewWeight : ( String, String ) -> Html Msg
viewWeight ( value, weight ) =
    H.p []
        [ H.span [ HA.style [ ( "display", "inline-block" ), ( "width", "10em" ) ] ]
            [ H.text value ]
        , H.text <| toString weight
        ]


sliders : Model -> Html Msg
sliders weights =
    weights
        |> Dict.map valueBox
        |> Dict.toList
        |> List.map Tuple.second
        |> H.div []


sumError : Model -> Html Msg
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


buttons : Model -> Html Msg
buttons model =
    H.p []
        [ Button.button
            (if Dict.isEmpty model then
                [ Button.disabled True ]
             else
                [ Button.onClick Reset
                , Button.danger
                ]
            )
            [ H.text "Reset" ]
        , Button.button
            [ if weightsSum model == 100 then
                Button.primary
              else
                Button.disabled True
            ]
            [ H.text "Accept" ]
        ]


weightsSum : Model -> Int
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
    case msg of
        AddValue value ->
            model
                |> Dict.insert value "1"

        SetValue value weight ->
            model |> Dict.insert value weight

        RemoveValue value ->
            model |> Dict.remove value

        Reset ->
            init


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
