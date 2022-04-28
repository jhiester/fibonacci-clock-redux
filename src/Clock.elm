{-
  Fibonacci clock driver
  Justin Hiester
  CS 557
  Winter 2022

  This file creates an elm application that displays a Fibonacci clock.
  There is a short introduction in the readme, but for more background on
  what a Fibonacci clock is and how to read them, see this link:
  
  https://www.theguardian.com/science/alexs-adventures-in-numberland/2015/may/09/fibonacci-clock-can-you-tell-the-time-on-the-worlds-most-stylish-nerd-timepiece

  The app follows the "elm architecture" as best as I could. I am completely new
  to Elm, and to using functional programming techniques to implement something
  nontrivial. There are probably some fauxpas in here regarding both.

  The details of the implementation can be found in the ClockParts.elm file.
-}

module Clock exposing (..)

import Browser
import Element exposing (rgb255)
import Element.Background as Background
import ClockParts exposing (buildBoxes, clockDisplay, psSets)
import Html exposing (..)
import Task
import Time


-- MAIN

main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type alias Model =
  { zone : Time.Zone
  , time : Time.Posix
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( Model Time.utc (Time.millisToPosix 0)
  , Cmd.batch
      [ Task.perform AdjustTimeZone Time.here
      , Task.perform Tick Time.now
      ]
  )


-- UPDATE

type Msg
  = Tick Time.Posix
  | AdjustTimeZone Time.Zone


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      ( { model | time = newTime }
      , Cmd.none
      )

    AdjustTimeZone newZone ->
      ( { model | zone = newZone }
      , Cmd.none
      )


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 1000 Tick


-- VIEW

view : Model -> Html Msg
view model =
  let
    -- hour needs to account for minutes going over 58 to avoid going "back in time"
    hour   = 
      if (minute >= 58) 
      then 
        (1 + toFloat (Time.toHour   model.zone model.time)) 
      else 
        toFloat (Time.toHour   model.zone model.time)

    minute = toFloat (Time.toMinute model.zone model.time)

    -- find the powersets that can be used to represent the current hour
    hourSums = 
      if ((modBy 12 (round hour)) == 0)  -- zero has a special use in the app
      then 
        List.filter (\xs -> List.sum(xs) == 12) psSets
      else
        List.filter (\xs -> List.sum(xs) == (modBy 12 (round(hour)))) psSets

    -- find the powersets that can be used to represent the current minute
    minSums = List.filter (\xs -> List.sum(xs) == (modBy 12 (round(minute / 5)))) psSets
    
    {- extract an element from the set of possible hour and minute 
       representations  note: reversing the list and extracting the 
       head is done just to match my previous implementation -}
    hours = Maybe.withDefault [] (List.head (List.reverse hourSums))
    minutes = Maybe.withDefault [] (List.head (List.reverse minSums))

    --build the new display
    updatedDisplay = buildBoxes hours minutes 0

  in
    -- display the clock on a dark grey background
    Element.layout [Background.color (rgb255 51 51 51)]
        (clockDisplay updatedDisplay)