{-
  Fibonacci clock implementation
  Justin Hiester
  CS 557
  Winter 2022
-}

module ClockParts exposing (buildBoxes, clockDisplay, psSets)

import Element exposing (centerX, centerY, column, el, Element, Length, padding, px, rgb255, row)
import Element.Background as Background
import Element.Border as Border
import Html exposing (..)
import List.Extra exposing (getAt)

-- types and type aliases
type alias Box msg = Element msg
type Color = Hour | Minute | Both | Neither | Error
type alias Size = Int

-- mapings for types to values
colormap : Color -> Element.Color
colormap color =
  case color of
    Hour -> rgb255 255 99 71
    Minute -> rgb255 144 238 144
    Both -> rgb255 106 90 205
    Neither -> rgb255 128 128 128
    Error -> rgb255 212 245 66

sizemap : Size -> Length
sizemap size =
  case size of
    0 -> px 50
    1 -> px 50
    2 -> px 105
    3 -> px 160
    4 -> px 270
    _ -> px 50

-- necessary constants
fibNums: List(Int)
fibNums = [1,1,2,3,5]

psSets: List(List Int)
psSets = powerset fibNums


{-  creates a box used to display one of the fibonacci
    squares of the clock's display -}
box : Color -> Size -> Box msg
box color size = 
  el [ Background.color (colormap color)
        , Border.rounded 3
        , padding 20
        , Element.height (sizemap size)
        , Element.width (sizemap size)
        ]
        (Element.text "")

{-  partially applied box functions used to create boxes of particular
    colors, but different sizes -}
boxNeither: Size -> Box msg
boxNeither = box Neither

boxBoth: Size -> Box msg
boxBoth = box Both

boxHour: Size -> Box msg
boxHour = box Hour

boxMinute: Size -> Box msg
boxMinute = box Minute

boxError: Size -> Box msg
boxError = box Error

{-  creates boxes of specific sizes using the partially applied box
    functions for particular colors. this funciton is heavily dependent
    on the structure of the hour and minute lists used. the Int argument is
    a counter used to move through the lists to create the correctly sized
    boxes. an array would have been more appropriate for this application,
    but i had a hard time getting arrays to play nicely -}
buildBoxes: List Int -> List Int -> Int -> List(Box msg)
buildBoxes xs ys z = 
  case (xs, ys, z) of
      ([], (_), _) -> []
      ((_), [], _) -> []
      ((x::restx), (y::resty), i) -> 
        if ((x == 0) && (y == 0)) then
            (boxNeither i)::(buildBoxes restx resty (i + 1))
        else if (x == y) then 
          (boxBoth i)::(buildBoxes restx resty (i + 1)) 
        else if ((x /= 0) && (y == 0)) then
          (boxHour i)::(buildBoxes restx resty (i + 1))
        else if ((x == 0) && (y /= 0)) then
          (boxMinute i)::(buildBoxes restx resty (i + 1))
        else
          (boxError i)::(buildBoxes restx resty (i + 1))


{-  create an element representing the clock's display that can be shown
    in a browser. the display is intended to be created from a display 
    constructed from the buildBoxes function. clockDisplay uses the .getAt
    function from List.Extra to move through a list like an array. Again,
    not ideal, but it was more effective for me than trying to use arrays -}
clockDisplay : List (Box msg) -> Element msg
clockDisplay display = 
        column [centerY, centerX] [
          row [Element.width Element.fill, centerY, centerX, Element.spacing 5]
          [
              column [Element.spacing 5, centerY] [
                  row [Element.width Element.fill, centerY, centerX, Element.spacing 5] [
                      column [] [
                          Maybe.withDefault (boxError 2) (List.Extra.getAt 2 display)
                      ]
                      , column [Element.spacing 5] [
                          row [Element.width Element.fill, centerY, centerX, Element.spacing 5] [
                              Maybe.withDefault (boxError 1) (List.Extra.getAt 0 display)
                          ]
                          , row [Element.width Element.fill, centerY, centerX, Element.spacing 5] [
                              Maybe.withDefault (boxError 1) (List.Extra.getAt 1 display)
                          ]
                      ]
                  ]
                  ,row [Element.width Element.fill, centerY, centerX, Element.spacing 5] [
                      Maybe.withDefault (boxError 3) (List.Extra.getAt 3 display)
                  ]
              ]
              , column [Element.spacing 5] [
                  Maybe.withDefault (boxError 5) (List.Extra.getAt 4 display)
              ]
          ]
        ]

-- find all the powersets of a given set. inserts 0 for exluded elements as a 
-- placeholder
powerset : List Int -> List(List Int)
powerset xs =
    case xs of
    [] -> [[]]
    (x::rest) -> List.map (\ps -> x::ps) (powerset rest) ++ (List.map (\ps -> 0::ps) (powerset rest))


