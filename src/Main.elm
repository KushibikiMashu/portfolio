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


getProfile : Cmd Msg
getProfile =
    Http.get
    { url = "http://localhost:8000/src/data.json"
    -- 後でurlは変更する。相対パスで指定できると良い
    , expect = Http.expectJson GotProfile profileDecoder
    }


profileDecoder : Decoder String
profileDecoder =
    field "main" (field "className" string)


type alias Main =
    { className : String
    , title : String
    , subtitle : String
    , avator : String
    }


type alias Sections =
    { sections : Section }


type alias Section =
    { title : String
    , items : List Item
    }


type alias Item =
    { title : String
    , description : String
    , image : String
    , link : String
    }


type alias Contacts =
    { contacts : List Contact }


type alias Contact =
    { name : String
    , icon : String
    , link : String
    }


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
    div [] [ viewApp model ]

viewApp : Model -> Html Msg
viewApp model =
    case model of
        Failure ->
            div [] [ text "Can't load JSON file."]

        Loading ->
            div [] [ text "Now Loading..." ]

        Succsess url ->
            div []
                [ viewTop model
                , viewSections
                , viewContacts
                , text url
                ]


viewTop : Model -> Html msg
viewTop model =
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