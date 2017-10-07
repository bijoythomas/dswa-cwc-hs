module Login exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import LoginStyle exposing (..)


type alias User =
    { username : String
    , password : String
    , loggedIn : Bool
    }


initialModel : User
initialModel =
    User "" "" False


type Msg
    = Username String
    | Password String
    | Login


view : User -> Html Msg
view user =
    if user.loggedIn then
        div [] [ button [ {- type_ "submit", -} buttonStyle, onClick Login ] [ text (toString user) ] ]
        --text ("Logged In" ++ (toString user))
    else
        div []
            [ h1 [ headerStyle ] [ text ("Login" ++ (toString user)) ]
            , Html.form [ formStyle ]
                [ div [] [ text "Username", input [ id "username", type_ "text", inputTextStyle, onInput Username ] [] ]
                , div [] [ text "Password", input [ id "password", type_ "password", inputTextStyle, onInput Password ] [] ]
                , div [] [ button [ {- type_ "submit", -} buttonStyle, onClick Login ] [ text "Login" ] ]
                ]
            ]


update : Msg -> User -> User
update message user =
    case message of
        Username name ->
            { user | username = name }

        Password password ->
            { user | password = password }

        Login ->
            { user | loggedIn = True }


main : Program Never User Msg
main =
    beginnerProgram
        { model = initialModel
        , view = view
        , update = update
        }
