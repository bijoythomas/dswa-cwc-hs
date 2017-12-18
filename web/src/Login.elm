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
    , churches : List Church
    , errorMsg : String
    }


type alias Model =
    User


type alias Church =
    { venue : Int
    , church : String
    }


churchDecoder : Decoder Church
churchDecoder =
    Decode.map2 Church
        (field "venue" Decode.int)
        (field "church" Decode.string)



--(field "region" Decode.string)


churchListDecoder : Decoder (List Church)
churchListDecoder =
    Decode.list churchDecoder


initialUser : User
initialUser =
    User "" "" False [] ""


init : ( User, Cmd Msg )
init =
    ( initialUser, Cmd.none )


type Msg
    = Username String
    | Password String
    | Login
    | LogOut
    | HandleLoginResponse (Result Http.Error (List Church))


churchMenu church =
    option
        [ Html.Attributes.value (toString church.church) ]
        [ text (toString church.church) ]


churchesMenu : List Church -> List (Html msg)
churchesMenu list =
    List.map
        (\church ->
            option
                [ Html.Attributes.value church.church ]
                [ text church.church ]
        )
        list


listToMenu : List (List String) -> List (Html msg)
listToMenu list =
    let
        toSelectOption : List String -> List (Html msg) -> List (Html msg)
        toSelectOption xs acc =
            case xs of
                b :: bs ->
                    (option [ Html.Attributes.value b ] [ text b ]) :: acc

                _ ->
                    acc
    in
        List.foldr toSelectOption [] list


categoiesMenu =
    listToMenu
        [ [ "Bible Quiz", "BQ" ]
        , [ "Story", "ST" ]
        , [ "Poetry", "PE" ]
        , [ "Drawing", "DR" ]
        , [ "Essay", "ES" ]
        ]


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
                [ Html.form [ registrationFormStyle ]
                    [ label [ for "firsname" ] [ text "First Name:" ]
                    , input [ type_ "text", name "firsname", required True ] []
                    , br [] []
                    , label [ for "middlename" ] [ text "Middle Name:" ]
                    , input [ type_ "text", name "middlename", required True ] []
                    , br [] []
                    , label [ for "lastname" ] [ text "Last Name:" ]
                    , input [ type_ "text", name "lastname", required True ] []
                    , br [] []
                    , label [ for "church" ] [ text "Church:" ]
                    , select [ name "church" ] (churchesMenu user.churches)
                    , br [] []
                    , label [ for "category" ] [ text "Category:" ]
                    , select [ name "category" ] categoiesMenu
                    ]
                ]
            else
                [ div [ class showError ] [ text user.errorMsg ]
                , Html.form [ loginFormStyle, onSubmit Login ]
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


handleLoginResponse : User -> Result Http.Error (List Church) -> ( User, Cmd Msg )
handleLoginResponse user result =
    case result of
        Ok [] ->
            ( { user | loggedIn = False, churches = [], password = "", errorMsg = "Invalid username/password" }, Cmd.none )

        Ok (x :: xs) ->
            ( { user | loggedIn = True, churches = x :: xs, password = "", errorMsg = "" }, Cmd.none )

        Err error ->
            ( { user | errorMsg = (toString error) }, Cmd.none )


authUser : User -> String -> Http.Request (List Church)
authUser user apiUrl =
    let
        body =
            user
                |> userEncoder
                |> Http.jsonBody
    in
        Http.post loginUrl body churchListDecoder


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
