module Homepage exposing (svg_main)

import Messaging exposing (Msg(..))
import String exposing (fromInt)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)


viewBoxWidth : Int
viewBoxWidth =
    100


viewBoxHeight : Int
viewBoxHeight =
    120


x_interval : Int
x_interval =
    25


y_interval : Int
y_interval =
    25


mask_rect : Svg Msg
mask_rect =
    rect
        [ x "0"
        , y "0"
        , width <| fromInt viewBoxWidth
        , height <| fromInt viewBoxHeight
        , fill "white"
        ]
        []


mask_def : Int -> Svg Msg
mask_def n_circles =
    defs [] [ Svg.mask [ id "mask" ] (mask_rect :: create_circles n_circles) ]


circle_N : Int -> String -> String -> String -> String -> Svg Msg
circle_N n filename s_col fill_col c_names =
    circle
        [ cx <| fromInt (x_interval * (modBy 3 n + 1))
        , cy <| fromInt (((n // 3) + 2) * y_interval)
        , r "6"
        , stroke s_col
        , fill fill_col
        , class c_names
        , onClick (GetCSV filename)
        ]
        []


circleMaskN n =
    circle_N n "" "white" "black" ""


circleTextN n filename =
    circle_N n filename "black" "transparent" "circle"


text_N : Int -> String -> Svg Msg
text_N n filename =
    text_
        [ x <| fromInt (x_interval * (modBy 3 n + 1))
        , y <| fromInt (((n // 3) + 2) * y_interval)
        , dy ".3em"
        , class "small circle-text"
        ]
        [ text filename ]


circle_group : Int -> String -> List (Svg Msg)
circle_group n filename =
    [ circleTextN n filename, text_N n filename ]


create_circles : Int -> List (Svg Msg)
create_circles n =
    List.map circleMaskN (List.range 0 (n - 1))


create_groups : List String -> List (Svg Msg)
create_groups filenames =
    List.concat (List.indexedMap circle_group filenames)


rightTurn : String
rightTurn =
    "H "
        ++ fromInt (x_interval * 3)
        ++ " c "
        ++ fromInt x_interval
        ++ " 0, "
        ++ fromInt x_interval
        ++ " "
        ++ fromInt y_interval
        ++ ", 0 "
        ++ fromInt y_interval
        ++ " "


leftTurn : String
leftTurn =
    "H "
        ++ fromInt x_interval
        ++ " c -"
        ++ fromInt x_interval
        ++ " 0, -"
        ++ fromInt x_interval
        ++ " "
        ++ fromInt y_interval
        ++ ", 0 "
        ++ fromInt y_interval
        ++ " "


chooseTurn : Int -> String
chooseTurn n =
    case modBy 2 n of
        0 ->
            rightTurn

        1 ->
            leftTurn

        _ ->
            ""


pathString : Int -> String
pathString n =
    "M 10 "
        ++ fromInt y_interval
        ++ " "
        ++ String.concat (List.map chooseTurn (List.range 0 ((n - 1) // 3 + 1)))


createPath : Int -> Svg Msg
createPath n =
    Svg.path
        [ d (pathString n)
        , Svg.Attributes.mask "url(#mask)"
        , class "path"
        ]
        []


svg_main : List String -> Svg Msg
svg_main filenames =
    svg [ width "40vw", viewBox <| "0 10 " ++ fromInt viewBoxWidth ++ " " ++ fromInt viewBoxHeight ]
        (mask_def (List.length filenames)
            :: (createPath (List.length filenames) :: create_groups filenames)
        )
