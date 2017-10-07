module Login exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onSubmit, onInput, onClick)
import LoginStyle exposing (..)
import Http
import Json.Decode as Decode exposing (..)
import Json.Encode as Encode exposing (..)


type alias User =
    { username : String
    , password : String
    , loggedIn : Bool
    , errorMsg : String
    }


type alias Model =
    User


initialUser : User
initialUser =
    User "" "" False ""


init : ( User, Cmd Msg )
init =
    ( initialUser, Cmd.none )


type Msg
    = Username String
    | Password String
    | Login
    | LogOut
    | HandleLoginResponse (Result Http.Error String)


view : User -> Html Msg
view user =
    let
        -- If there is an error on authentication, show the error alert
        showError : String
        showError =
            if String.isEmpty user.errorMsg then
                "hidden"
            else
                ""

        -- If the user is logged in, show a greeting; else show login form
        content =
            if user.loggedIn then
                [ button [ buttonStyle, onClick LogOut ] [ text "Logout" ] ]
            else
                [ div [ class showError ] [ text user.errorMsg ]
                , Html.form [ formStyle, onSubmit Login ]
                    [ div [] [ text "Username", input [ id "username", type_ "text", inputTextStyle, onInput Username ] [] ]
                    , div [] [ text "Password", input [ id "password", type_ "password", inputTextStyle, onInput Password ] [] ]
                    , div [] [ button [ buttonStyle ] [ text "Login" ] ]
                    ]
                ]
    in
        div [] content


loginUrl : String
loginUrl =
    "/login"


userEncoder : User -> Encode.Value
userEncoder user =
    Encode.object
        [ ( "username", Encode.string user.username )
        , ( "password", Encode.string user.password )
        ]


tokenDecoder : Decoder String
tokenDecoder =
    Decode.field "success" Decode.string


handleLoginResponse : User -> Result Http.Error String -> ( User, Cmd Msg )
handleLoginResponse user result =
    case result of
        Ok "success" ->
            ( { user | loggedIn = True, password = "", errorMsg = "" }, Cmd.none )

        Ok _ ->
            ( { user | loggedIn = False, password = "", errorMsg = "Invalid username/password" }, Cmd.none )

        Err error ->
            ( { user | errorMsg = (toString error) }, Cmd.none )


authUser : User -> String -> Http.Request String
authUser user apiUrl =
    let
        body =
            user
                |> userEncoder
                |> Http.jsonBody
    in
        Http.post loginUrl body tokenDecoder


authUserCmd : User -> String -> Cmd Msg
authUserCmd user apiUrl =
    Http.send HandleLoginResponse (authUser user apiUrl)


update : Msg -> User -> ( User, Cmd Msg )
update message user =
    case message of
        Username name ->
            ( { user | username = name }, Cmd.none )

        Password password ->
            ( { user | password = password }, Cmd.none )

        Login ->
            ( user, authUserCmd user loginUrl )

        LogOut ->
            ( { user | username = "", password = "", loggedIn = False }, Cmd.none )

        HandleLoginResponse result ->
            handleLoginResponse user result


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }
