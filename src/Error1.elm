module Errors exposing (..)


error x =
    if x < 0 then
        "below"
    else if x > 0 then
        "above"
    else
        True
