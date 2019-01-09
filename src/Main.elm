import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, map2, map3, map4, field, string, list)

-- 設計
-- アプリケーションは関数の集合体である
-- 過度な共通化は避ける
-- 共通化できるところは後からわかる
-- コンポーネント思考をしない
-- 関数は一つのことだけをする

-- update
-- クリックされる

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
    | Succsess Profile


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
    = GotProfile (Result Http.Error Profile)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GotProfile result ->
            case result of
                Ok profile ->
                    (Succsess profile, Cmd.none)

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

        Succsess profile ->
            div [] [ viewApp model profile ]


viewApp : Model -> Profile -> Html Msg
viewApp model profile =
    let
        intro = profile.intro
        sections = profile.sections
        contacts = profile.contacts
    in
        div []
            [ viewIntro intro
            , viewSections sections
            , viewContacts contacts
            ]


viewIntro : Intro -> Html Msg
viewIntro intro =
    div [] [ text intro.title ]


viewSections : List Section -> Html Msg
viewSections sections =
    div [] 
        (List.map displaySection sections)


displaySection : Section -> Html Msg
displaySection section =
    let
        title = section.title
        items = section.items
    in
        div []
            [ div [] [ text title ]
            , div [] (List.map displayItems items)
            ]


displayItems : Item -> Html Msg
displayItems item =
    div [] [ text item.title ]


viewContacts : List Contact -> Html Msg
viewContacts contacts =
    div []
        (List.map displayContact contacts)


displayContact : Contact -> Html Msg
displayContact contact =
    div [] [ text contact.name ]


-- HTTP

getProfile : Cmd Msg
getProfile =
    Http.get
    { url = "http://localhost:8000/src/data.json"
    -- 後でurlは変更する。相対パスで指定できると良い
    , expect = Http.expectJson GotProfile profileDecoder
    }


profileDecoder : Decoder Profile
profileDecoder =
    map3 Profile
        (field "intro" introDecoder)
        (field "sections" (list sectionDecoder) )
        (field "contacts" (list contactDecoder) )


introDecoder : Decoder Intro
introDecoder =
    map4 Intro
        (field "className" string)
        (field "title" string)
        (field "subtitle" string)
        (field "avator" string)


sectionDecoder : Decoder Section
sectionDecoder =
    map2 Section
        (field "title" string)
        (field "items" (list itemDecoder) )


itemDecoder : Decoder Item
itemDecoder =
    map4 Item
        (field "title" string)
        (field "description" string)
        (field "image" string)
        (field "link" string)


contactDecoder : Decoder Contact
contactDecoder =
    map3 Contact
        (field "name" string)
        (field "icon" string)
        (field "link" string)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none