module Values exposing (..)

import Dict exposing (Dict)


type Model
    = BestIs Answer
    | Thinking Answer (Dict String (Result String Int))


type Answer
    = NoAnswer
    | Answer (Dict String Int)


type Msg
    = StartThinking
    | SetValue String (Result String Int)
    | Cancel
    | AcceptAnswer (Dict String Int)


init : Model
init =
    BestIs NoAnswer
