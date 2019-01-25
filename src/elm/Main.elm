import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, map2, map3, map4, map5, map6, field, string, int, list)

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
    | Succsess Portfolio


init : () -> (Model, Cmd Msg)
init _ =
    (Loading, getPortfolio)


type alias Portfolio =
    { intro : Intro
    , info : List Info
    , skills : List Skill
    , websites : List Website
    , others : List Other
    , contacts : List Contact
    }


type alias Intro =
    { className : String
    , title : String
    , subtitle : String
    , icon : String
    }


type alias Info =
    { title : String
    , description : Description
    , icon : String
    }


type alias Skill =
    { name : String
    , level : Int
    , color : String
    }


type alias Website =
    { title : String
    , description : Description
    , tech : List String
    , image : Image
    , icon : String
    , link : String
    }


type alias Other =
    { title : String
    , image : Image
    , link : String
    }


type alias Contact =
    { name : String
    , icon : String
    , color : String
    , link : String
    }


type alias Description =
    { ja : String
    , en : String
    , ch : String
    }


type alias Image =
    { src : String
    , alt : String
    }


-- UPDATE

type Msg
    = GotPortfolio (Result Http.Error Portfolio)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GotPortfolio result ->
            case result of
                Ok portfolio ->
                    (Succsess portfolio, Cmd.none)

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

        Succsess portfolio ->
            div [] [ viewApp model portfolio ]


viewApp : Model -> Portfolio -> Html Msg
viewApp model portfolio =
    let
        intro = portfolio.intro
        info = portfolio.info
        skills = portfolio.skills
        websites = portfolio.websites
        others = portfolio.others
        contacts = portfolio.contacts
    in
        div []
            [ viewIntro intro
            --, view info
            --, view skills
            --, view websites
            , viewOthers others
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
                        [ img [ class "w-24 xl:w-32", src icon ] [] ]
                    , div [] 
                        [ span [ class "intro-subtitle" ] [ text "I’M MASHU" ]
                        , br [ class "block lg:hidden" ] []
                        , span [ class "intro-subtitle" ] [ text " KUSHIBIKI" ]
                        ]
                    ]
                ]
            ]


viewDescription : Description -> Html Msg
viewDescription description =
    let
        ja = description.ja
    in
        div [ class "" ] [ text ja ]


viewLanguage : Html Msg
viewLanguage =
    div [ class "py-6" ]
        [ h1 [ class "section-title" ] [ text "LANGUAGE SKILLS" ]
        , canvas [ class "mx-auto", id "languages" ] []
        ]


viewOthers : List Other -> Html Msg
viewOthers others =
    div [ class "py-6" ] 
            [ h1 [ class "section-title" ] [ text "OTHERS" ]
            , div [ class "others-container" ] (List.map viewOther others)
            ]



    -- viewWebsite
    --div [ class "py-6" ] 
    --        [ h1 [ class "section-title" ] [ text "PORTFOLIO" ]
    --        , div [ class "card-container" ] (List.map viewOther others)
    --        ]


viewOther : Other -> Html Msg
viewOther other =
    let
        title = other.title
        imageSrc = other.image.src
        imageAlt = other.image.alt
        link = other.link

    in
        div [ class "other-item" ] 
            [ a [ class "no-underline", href link ]
                [ img [ class "other-image hover:shadow-lg", src imageSrc, alt imageAlt ] []
                ]
            ]



-- viewWebsite
        --div [ class "card hover:shadow-lg my-5 md:my-0" ]
        --    [ a [ class "no-underline", href link ]
        --        [ div [ class "flex items-center h-74px py-3 px-4" ]
        --            [ span [ class "card-avatar text-white bg-blue-darkest" ]
        --                [ i [ class "fas fa-music" ] [] ]
        --            ]
        --        ]
        --    ]


viewContacts : List Contact -> Html Msg
viewContacts contacts =
    div [] 
        [ div [ class "bg-grey-lighter text-center" ] 
            [ h1 [ class "w-full py-4 text-3xl md:text-2xl text-grey-darkest" ] [ text "CONTACT" ]
            , div [] (List.map2 viewContact contacts classes)
            , div [ class "pt-4 pb-3 text-grey-darker" ]
                [ p [ class "text-base md:text-sm" ] [ text "by MASHU KUSHIBIKI" ]
                , p [ class "text-base md:text-sm" ] [ text "created with Elm" ]
                ]
            ]
        ]


viewContact : Contact -> String -> Html Msg
viewContact contact classesa =
    let 
        icon = contact.icon
        className = "footer-icon " ++ contact.color
        link = contact.link
    in
        a [ class "no-underline", href link ]
            [ span [ class className ]
                [ i [ class icon ] [] ]
            , div [] [ text classesa ]
            ]


classes : List String
classes =
    ["a", "b", "c", "d"]

-- HTTP

getPortfolio : Cmd Msg
getPortfolio =
    Http.get
    { url = "http://localhost:8000/src/elm/data.json"
    -- 後でurlは変更する。相対パスで指定できると良い
    , expect = Http.expectJson GotPortfolio portfolioDecoder
    }


portfolioDecoder : Decoder Portfolio
portfolioDecoder =
    map6 Portfolio
        (field "intro" introDecoder)
        (field "info" (list infoDecoder) )
        (field "skills" (list skillDecoder) )
        (field "websites" (list websiteDecoder) )
        (field "others" (list otherDecoder) )
        (field "contacts" (list contactDecoder) )


introDecoder : Decoder Intro
introDecoder =
    map4 Intro
        (field "className" string)
        (field "title" string)
        (field "subtitle" string)
        (field "icon" string)


infoDecoder : Decoder Info
infoDecoder =
    map3 Info
        (field "title" string)
        (field "description" descriptionDecoder)
        (field "icon" string)


skillDecoder : Decoder Skill
skillDecoder =
    map3 Skill
        (field "name" string)
        (field "level" int)
        (field "color" string)


websiteDecoder : Decoder Website
websiteDecoder =
    map6 Website
        (field "title" string)
        (field "description" descriptionDecoder)
        (field "tech" (list string) )
        (field "image" imageDecoder)
        (field "icon" string)
        (field "link" string)


otherDecoder : Decoder Other
otherDecoder =
    map3 Other
        (field "title" string)
        (field "image" imageDecoder)
        (field "link" string)


contactDecoder : Decoder Contact
contactDecoder =
    map4 Contact
        (field "name" string)
        (field "icon" string)
        (field "color" string)
        (field "link" string)


descriptionDecoder : Decoder Description
descriptionDecoder =
    map3 Description
        (field "ja" string)
        (field "en" string)
        (field "ch" string)


imageDecoder : Decoder Image
imageDecoder =
    map2 Image
        (field "src" string)
        (field "alt" string)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none