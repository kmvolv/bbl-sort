
module Funcs exposing (..)
import Styles exposing (..)

import Html.Attributes exposing (style)
import Html exposing (..)
import Debug
import List exposing (..)
import List.Extra exposing (..)

-- Comparison with Maybe Int by forcefully converting it to Int
noMaybe : Maybe a -> a
noMaybe x = case x of
    Just y -> y
    Nothing -> Debug.todo "Error"

-- Obtaining value at index 'idx' of the given List
getAtIdx idx arr = 
    noMaybe(getAt idx (arr) )

-- Generate any span element with a given styling and content
genHtml styleselect content = 
    Html.span(styleselect)[text content]

-- Print the header of the page, depending on the operation chosen by the user
printChecker model =
    if model.err == True then
        genHtml (alertStyle "#f8d7da") model.msg
    else 
        if model.msg /= "" then genHtml (alertStyle "#d4edda") model.msg
        else genHtml (alertStyle "#cce5ff") "Click on any of the buttons to apply the respective operation"

-- Function to print the individual elements of the list    
printarr model idx1 idx2 =
    if (model.i == idx1 || model.i == idx2) then
        if model.i == idx2 then
                Html.span[style "display" "flex", style "flex-direction" "column"]
                [
                    if(getAtIdx idx2 model.arr == getAtIdx idx2 (sort(model.arr))) then
                        genHtml (arrElement "#AED581" True) (String.fromInt (getAtIdx idx2 model.arr))
                    else 
                        genHtml (arrElement "#FFF59D" True) (String.fromInt (getAtIdx idx2 model.arr))
                    , Html.span[style "margin-top" "10px"][genHtml encircled (String.fromInt idx2)]
                    , genHtml varPntr "i"
                ]       
        else 
            Html.span[style "display" "flex", style "flex-direction" "column"]
            [
                if(getAtIdx idx2 model.arr == getAtIdx idx2 (sort(model.arr))) then
                    genHtml (arrElement "#AED581" True) (String.fromInt (getAtIdx idx2 model.arr))
                else
                    genHtml (arrElement "#FFF59D" True) (String.fromInt (getAtIdx idx2 model.arr))
                , Html.span[style "margin-top" "10px"][genHtml encircled (String.fromInt idx2)]
                , if(model.b == idx2) then 
                    genHtml varPntr "b"
                else 
                    genHtml varPntr ""
            ]
    else
        Html.span[style "display" "flex", style "flex-direction" "column"]
        [
            if(getAtIdx idx2 model.arr == getAtIdx idx2 (sort(model.arr))) then
                genHtml (arrElement "#AED581" False) (String.fromInt (getAtIdx idx2 model.arr))
            else
                genHtml (arrElement "#FFF59D" False) (String.fromInt (getAtIdx idx2 model.arr))
            , Html.span[style "margin-top" "10px"][Html.span [style "font-size" "28px"][text (String.fromInt idx2)]]
            , if(model.b == idx2) then 
                    genHtml varPntr "b"
                else 
                    genHtml varPntr ""
        ]      
