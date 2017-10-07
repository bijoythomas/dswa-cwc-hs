module Playground exposing (..)

import Html


escapeEarth velocity speed =
  if velocity > 11.186 then
    "Godspeed"
  else if speed == 7.67 then
    "Stay in orbit"
  else
    "Come back"

hashtag dow =
  case dow of
    "Sunday" ->
      "#Sinday"
    "Monday" ->
      "#MondayBlues"
    "Tuesday" ->
      "#TakeMeBackTuesday"
    _ ->
      "#Whatever"


main =
  Html.text (hashtag "Tuesday")