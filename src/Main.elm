
port module Main exposing (..)
import Browser
import Html exposing (..)
import Html.Attributes exposing (style)

import List exposing (..)
import List.Extra exposing (..)

import Json.Encode as JE
import Core as ULE
import Css exposing (..)

import Styles exposing (..)
import Funcs exposing (..)

port analytics : JE.Value -> Cmd msg

-- Subscriptions 
subscriptions: Model -> Sub Msg
subscriptions model =
    Sub.none

-- For update
setFresh : Msg -> Bool
setFresh msg =
    case msg of 
        Init _ ->
            True
        _ ->
            False

-- Init
type alias Model = 
    {
        arr: List(Int),
        i: Int,
        b: Int,
        err: Bool,
        msg: String
    }

init: () -> (Model, Cmd Msg)
init _ = 
    let 
        genarr = [88, 23, 54, 17, 88]
    in
    (
        {
            arr = genarr,
            i = 0,
            b = List.length genarr,
            err = False,
            msg = ""
        }
    , Cmd.none )
    
-- View
view: Model -> Html Msg
view model =
    div[style "display" "flex", style "flex-direction" "column", style "margin-bottom" "-70px", style "margin-top" "70px"]
        [
            div[style "margin" "auto", style "display" "flex", style "flex-direction" "row"] 
                [
                    printChecker model
                    , if (sort(model.arr) == model.arr) then 
                        genHtml headerstyleSorted "Sorted!"
                    else 
                        genHtml headerstyleUnsorted "Not Sorted"  
                ]
            
            , div(arrayStyle)
                [
                    printarr model 0 0
                    , printarr model 0 1
                    , printarr model 1 2
                    , printarr model 2 3
                    , if List.length model.arr > 4 then
                        printarr model 3 4
                    else genHtml varPntr ""
                    , if List.length model.arr > 5 then
                        printarr model 4 5
                    else genHtml varPntr ""    
                ]
            , div[style "position" "relative", style "top" "10px", style "margin" "3cm 20cm", style "font-size" "30px"]
                [
                    div[] [ Html.span [style "padding-left" "3.5cm"] [text("index (i) = " ++ String.fromInt model.i)]]
                    , div[] [ Html.span [style "padding-left" "3.5cm"] [text("boundary (b) = " ++ String.fromInt model.b)]]
                    , buttonStyle Swap "Swap"
                    , buttonStyle IncI "Increment i"
                    , buttonStyle ResI "Reset i"
                    , buttonStyle DecB "Decrement b"
                ]
        ]

type Msg = Swap | IncI | ResI | DecB | Init Int

-- Update
update : Msg -> Model -> (Model,Cmd Msg)
update mesg model =
    case mesg of 
        Swap ->
            if model.i /= List.length model.arr - 1 then
                let ival = noMaybe(getAt model.i model.arr)
                    iplusval = noMaybe (getAt (model.i+1) model.arr)
                in
                ({
                    model | arr = swapAt model.i (model.i + 1) model.arr    
                    , err = if ival > iplusval then
                                        False
                                    else True
                    , msg = if ival <= iplusval then "Swapped at indices " ++ (String.fromInt model.i) ++ " and " ++ (String.fromInt (model.i + 1)) ++ ". You might be deviating from the algorithm"
                            else "Swapped at indices " ++ (String.fromInt model.i) ++ " and " ++ (String.fromInt (model.i + 1)) ++ ". Continue to the next operation."
                }, Cmd.none)
            else 
                ({
                    model | err = True
                    , msg = "There is no element to swap with, try another operation"
                }, Cmd.none)
        IncI ->
            if model.i == model.b - 1 then
                ({
                    model | msg = "You cannot increment i further"
                    , err = True
                }, Cmd.none)
            else
                let ival = noMaybe(getAt model.i model.arr)
                    iplusval = noMaybe (getAt (model.i+1) model.arr)
                in
                ({
                    model | err = if ival <= iplusval then 
                                    False
                                else True
                    , msg = if ival > iplusval then "Incremented i, you might be deviating from the algorithm"
                            else "Incremented i, continue to the next operation"
                    , i = model.i + 1
                }, Cmd.none)
                
        ResI ->
            ({
                model | err = if model.i /= model.b - 1 then 
                                    True
                                else False
                , msg = if model.i /= model.b - 1 then "Reset i, you might be deviating from the algorithm"
                        else "Reset i, continue to the next operation"
                , i = 0
            }, Cmd.none)
        DecB ->
            if model.b == 1 || model.b == model.i + 1 then
                ({
                    model | err = if model.b ==1 then False
                                    else True
                    , msg = if model.b ==1 then "Boundary has reached index 1. Bubblesort terminates here. The array should be sorted."
                    else "Cannot decrement b, try another operation"
                }, Cmd.none)
            else
                let bval = noMaybe(getAt (model.b - 1) model.arr)
                    bvalsorted = noMaybe (getAt (model.b - 1) (sort model.arr))
                in
                ({
                    model | err = if bval == bvalsorted then False
                                    else True
                    , b = model.b - 1
                    , msg = if bval /= bvalsorted then "Decremented b, you might be deviating from the algorithm"
                            else "Decremented b, continue to the next operation" 
                }, Cmd.none)
        Init i ->
            (model,Cmd.none)

-- Main
main = 
    Browser.element
        {
            init = ULE.init logger analytics init
            , view = ULE.view view
            , update = ULE.update logger analytics update setFresh Nothing Nothing
            , subscriptions = ULE.subscriptions subscriptions
        }

-- logger
logger model =
    {
        arr = model.arr
        , i = model.i
        , b = model.b
        , err = model.err
        , msg = model.msg
    }