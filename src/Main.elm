import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, string)

-- 設計
-- アプリケーションは関数の集合体である
-- 過度な共通化は避ける
-- 共通化できるところは後からわかる
-- コンポーネント思考をしない
-- 関数は一つのことだけをする

-- model

-- update
-- クリックされる

-- view
-- top, about, portfolio, contact

-- MAIN

main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


-- MODEL

type Model
    = Failure
    | Loading
    | Succsess String


init : () -> (Model, Cmd Msg)
init _ =
    (Loading, getProfile)


--type alias Top = 
--    { className
--    ,
--    }

getProfile : Cmd Msg
getProfile =
    Http.get
    { url = "/data.json"
    , expect = Http.expectJson GotProfile profileDecoder
    }


profileDecoder : Decoder String
profileDecoder =
    field "top" (field "className" string)


-- UPDATE

type Msg
    = GotProfile (Result Http.Error String)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of 
        GotProfile result ->
            case result of
                Ok url ->
                    (Succsess url, Cmd.none)

                Err _ ->
                    (Failure, Cmd.none)


-- VIEW

view : Model -> Html Msg
view model =
    div []
        [ viewTop
        , viewSections
        , viewContacts
        ]

viewTop : Html msg
viewTop =
    div [] [ text "viewTop" ]


viewSections : Html msg
viewSections =
    div [] [ text "viewSections" ]


viewContacts : Html msg
viewContacts =
    div [] [ text "viewContacts" ]


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none