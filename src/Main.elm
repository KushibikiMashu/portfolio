import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, map3, map4, field, string, list)

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
    | Succsess Intro


init : () -> (Model, Cmd Msg)
init _ =
    (Loading, getProfile)


type alias Profile =
    { intro : Intro
    , sections : List Section
    , contacts : List Contact
    }


type alias Intro =
    { className : String
    , title : String
    , subtitle : String
    , avator : String
    }


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


type alias Contact =
    { name : String
    , icon : String
    , link : String
    }


-- UPDATE

type Msg
    = GotProfile (Result Http.Error Intro)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GotProfile result ->
            case result of
                Ok intro ->
                    (Succsess intro, Cmd.none)

                Err _ ->
                    (Failure, Cmd.none)


-- VIEW

view : Model -> Html Msg
view model =
    case model of
        Failure ->
            div [] [ text "Can't load JSON file."]

        Loading ->
            div [] [ text "Now Loading..." ]

        Succsess intro ->
            div [] [ viewApp model intro ]


viewApp : Model -> Intro -> Html Msg
viewApp model intro =
    div []
        [ viewTop model
        , viewSections
        , viewContacts
        , displayIntro intro
        ]


displayIntro : Intro -> Html msg
displayIntro intro =
    div [] 
    [ text (intro.avator ++ intro.title) ]


viewTop : Model -> Html msg
viewTop model =
    div [] [ text "viewTop" ]


viewSections : Html msg
viewSections =
    div [] [ text "viewSections" ]


viewContacts : Html msg
viewContacts =
    div [] [ text "viewContacts" ]


-- HTTP

getProfile : Cmd Msg
getProfile =
    Http.get
    { url = "http://localhost:8000/src/data.json"
    -- 後でurlは変更する。相対パスで指定できると良い
    , expect = Http.expectJson GotProfile profileDecoder
    }


profileDecoder : Decoder Intro
profileDecoder =
    field "intro" introDecoder

introDecoder : Decoder Intro
introDecoder =
    map4 Intro
        (field "className" string)
        (field "title" string)
        (field "subtitle" string)
        (field "avator" string)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none