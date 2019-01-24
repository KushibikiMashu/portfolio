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
-- 多言語化対応で利用する？

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
    , icon : String
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
            , viewOthers
            , viewContacts contacts
            ]


viewIntro : Intro -> Html Msg
viewIntro intro =
    let 
        title = intro.title
        subtitle = intro.subtitle
        icon = intro.icon
    in
        div [ class "intro-container" ] 
            [ div [ class "max-w-xl mx-auto text-center pt-16" ] 
                [ div []
                    [ span [ class "intro-title" ] [ text "Love Creating" ]
                    , br [] []
                    , span [ class "intro-title" ] [ text "Web Apps" ]
                    ]
                , div [ class "absolute pin-r pin-l pin-b pb-12" ]
                    [ div [ class "py-2 md:py-4" ]
                        [ img [ class "w-24 xl:w-32", src "/assets/icon/code.svg" ] [] ]
                    , div [] 
                        [ span [ class "intro-subtitle" ] [ text "I’M MASHU" ]
                        , br [ class "block lg:hidden" ] []
                        , span [ class "intro-subtitle" ] [ text "KUSHIBIKI" ]
                        ]
                    ]
                ]
            ]


--viewIntroTitles : List String(Title型にする) -> Html Msg
--viewIntroTitles title =
--    let
--        first = List.head title
--        second = List.tail title
--        className = title.class
--    in
            
--    [ div []
--        [ span [ class "intro-title" ] [ text first ]
--        , br [ class className ] []
--        , span [ class "intro-title" ] [ text second ]
--        ]
--    ]
--"intro": {
--  "title": {
--    "className": "",
--    "words": [
--      "Love Creating",
--      "Web Apps"
--    ]
--  },
--  "subtitle": {
--    "className": "block lg:hidden",
--    "words": [
--      "I’M MASHU",
--      "KUSHIBIKI"
--    ]
--  },
--  "icon": "/assets/icon/code.svg"
--},


viewSections : List Section -> Html Msg
viewSections sections =
    div [] (List.map viewSection sections)


viewSection : Section -> Html Msg
viewSection section =
    let
        title = section.title
        items = section.items
    in
        div []
            [ div [] [ text title ]
            , div [] (List.map viewItems items)
            ]


viewItems : Item -> Html Msg
viewItems item =
    div [] [ text item.title ]


viewOthers : Html Msg
viewOthers =
    div [] []


viewOthersItem : List String -> Html Msg
viewOthersItem item =
    div [] []


viewContacts : List Contact -> Html Msg
viewContacts contacts =
    div [] 
        [ div [ class "bg-grey-lighter text-center" ] 
            [ h1 [ class "w-full py-4 text-3xl md:text-2xl text-grey-darkest" ] [ text "CONTACT" ]
            , div [] (List.map viewContact contacts)
            , div [ class "pt-4 pb-3 text-grey-darker" ]
                [ p [ class "text-base md:text-sm" ] [ text "by MASHU KUSHIBIKI" ]
                , p [ class "text-base md:text-sm" ] [ text "created with Elm" ]
                ]
            ]
        ]


viewContact : Contact -> Html Msg
viewContact contact =
    let 
        link = contact.link
        className = "text-" ++ contact.name ++ " footer-icon"
        icon = contact.icon
    in
        a [ class "no-underline", href link ]
          [ span [ class className ]
              [ i [ class icon ] [] ]
          ]


displayContact : Contact -> Html Msg
displayContact contact =
    div [] [ text contact.name ]


-- HTTP

getProfile : Cmd Msg
getProfile =
    Http.get
    { url = "http://localhost:8000/src/elm/data.json"
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
        (field "icon" string)


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