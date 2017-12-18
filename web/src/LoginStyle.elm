module LoginStyle exposing (..)

import Html exposing (Attribute)
import Html.Attributes
import Css exposing (..)


styles cssPairs =
    Css.asPairs cssPairs
        |> Html.Attributes.style


headerStyle : Attribute msg
headerStyle =
    styles
        [ paddingLeft (cm 3) ]


loginFormStyle : Attribute msg
loginFormStyle =
    styles
        [ borderRadius (px 5)
        , backgroundColor (hex "#f2f2f2")
        , padding (px 20)
        , width (px 300)
        ]


registrationFormStyle : Attribute msg
registrationFormStyle =
    styles
        [ borderRadius (px 5)
        , backgroundColor (hex "#f2f2f2")
        , padding (px 20)
        , width (px 600)
        ]


inputTextStyle : Attribute msg
inputTextStyle =
    styles
        [ display block
        , width (px 260)
        , padding2 (px 12) (px 20)
        , margin2 (px 8) (px 0)
        , border (px 0)
        , borderRadius (px 4)
        ]


buttonStyle : Attribute msg
buttonStyle =
    styles
        [ width (px 300)
        , backgroundColor (hex "#397cd5")
        , color (hex "#fff")
        , padding2 (px 14) (px 20)
        , marginTop (px 10)
        , border (px 0)
        , borderRadius (px 4)
        , fontSize (px 16)
        ]
