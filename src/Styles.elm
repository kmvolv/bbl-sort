module Styles exposing (..)
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes as HA exposing (style)

-- Styling of header if array is sorted
headerstyleSorted: List(Attribute msg)
headerstyleSorted = 
    [
        style "font-size" "30px"
        , style "background-color" "#4BB543"
        , style "color" "white"
        , style "padding" "10px"
        , style "font-weight" "bold"
    ]

-- Styling of header if array is unsorted
headerstyleUnsorted: List(Attribute msg)
headerstyleUnsorted = 
    [
        style "font-size" "30px"
        , style "background-color" "#FF7900"
        , style "color" "white"
        , style "padding" "10px"
        , style "font-weight" "bold"
    ]

-- Styling for a single list element, with background color specified
arrElement: String -> Bool -> List(Attribute msg)
arrElement bkgclr selected = 
    [
        style "width" "40px"
        , style "display" "inline-block"
        , style "padding" "40px"
        , if (selected == True) then
            style "border" "5px solid red"
            else style "border" "0px"
        , style "border-radius" "100%"
        , style "font-size" "40px"
        , style "margin" "20px"
        , style "background-color" bkgclr
        , style "transition" "background-color 0.5s ease-in-out 0s"
    ]

-- To highlight list element
encircled: List(Attribute msg)
encircled = 
    [
        style "font-size" "28px"
        , style "border" "3px solid red"
        , style "border-radius" "100%"
        , style "padding" "15px"
    ]

-- Styling for 'i' and 'b' pointers
varPntr: List(Attribute msg)
varPntr = 
    [
        style "font-size" "40px"
        , style "position" "relative"
        , style "top" "20px"
    ]

-- Styling for entire list
arrayStyle: List(Attribute msg)
arrayStyle = 
    [
        style "margin" "9% 30%"
        , style "margin-bottom" "-65px"
        , style "display" "flex"
        , style "justify-content" "center"
        , style "text-align" "center"
    ]

-- Styling for 'Next' button
buttonStyle action content =
    Html.button [
        onClick action
        , HA.class "button__action--primary"
        , style "position" "relative"
        , style "top" "84px"
        , style "left" "12cm"
    ] [ text content ]

alertStyle: String -> List(Attribute msg)
alertStyle clr = 
    [
        style "font-size" "20px"
        , style "background-color" clr
        , style "padding" "10px"
        , style "margin" "0 10px"
        , style "border" "2px solid black"
    ]
    