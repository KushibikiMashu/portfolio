port module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, map2, map3, map4, map5, map6, field, string, int, list)


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


port languageSkillsToJs : List Skill -> Cmd msg


type alias App =
    { language : Language
    , portfolio : Portfolio
    }


type Language
    = English
    | Japanese
    | Chinese
    

type alias Portfolio =
    { info : List Info
    , skills : List Skill
    , websites : List Website
    , others : List Other
    , contacts : List Contact
    }


type alias Info =
    { title : String
    , description : Description
    , icon : String
    }


type alias Description =
    { ja : String
    , en : String
    , ch : String
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


type alias Image =
    { src : String
    , alt : String
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


-- UPDATE

type Msg
    = GotPortfolio (Result Http.Error Portfolio)
    | LoadAgain


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =            
    case msg of
        GotPortfolio result ->
            case result of
                Ok portfolio ->
                    ( Succsess portfolio
                    , (languageSkillsToJs portfolio.skills)
                    )

                Err _ ->
                    (Failure
                    , Cmd.none
                    )

        LoadAgain ->
            (Loading, getPortfolio)


-- VIEW

view : Model -> Html Msg
view model =
    case model of
        Failure ->
            div []
                [ text "Can't load JSON file. Something is wrong with GitHub Pages server."
                , newLine
                , button [ onClick LoadAgain ] [ text "Try again?" ]
                ]

        Loading ->
            div [] []

        Succsess portfolio ->
            div [] [ viewApp portfolio ]


viewApp : Portfolio -> Html Msg
viewApp (portfolio as p) =
        div []
            [ viewTop
            , div [ class "max-w-xl mx-auto container" ]
                [ viewFlags
                , viewInfo p.info
                , viewSkills p.skills
                , viewWebsites p.websites
                , viewOthers p.others
                ]
            , viewContacts p.contacts
            ]


viewTop : Html Msg
viewTop =
        div [ class "top-container" ] 
            [ div [ class "max-w-xl mx-auto text-center pt-16" ] 
                [ div []
                    [ span [ class "top-title" ] [ text "Love Creating" ]
                    , newLine
                    , span [ class "top-title" ] [ text "Web Apps" ]
                    ]
                , div [ class "absolute pin-r pin-l pin-b pb-12" ]
                    [ div [ class "py-2 md:py-4" ]
                        [ img [ class "w-24 xl:w-32", src "/portfolio/assets/icon/code.svg" ] [] ]
                    , div [] 
                        [ span [ class "top-subtitle" ] [ text "I’M MASHU" ]
                        , lgNewLine
                        , span [ class "top-subtitle" ] [ text " KUSHIBIKI" ]
                        ]
                    ]
                ]
            ]


newLine : Html Msg
newLine = br [] []


lgNewLine : Html Msg
lgNewLine = br [ class "block lg:hidden" ] []


viewFlags : Html Msg
viewFlags =
    div [ class "self-start pt-4 justify-center my-auto" ]
        [ ul [ class "flex justify-end px-3 pt-3 list-reset leading-narrow" ]
            (List.map2 viewFlag flagClassNames flags)
        ]


viewFlag : String -> String -> Html Msg
viewFlag className flag =
    li [ class className ] [ text flag ]


flagClassNames : List String
flagClassNames =
    [ "text-3xl pr-2"
    , "text-3xl px-2 border-l border-r border-solid border-grey-dark"
    , "text-3xl px-2"
    ]


flags : List String
flags = [ "🇯🇵", "🇬🇧", "🇨🇳" ]


viewInfo : List Info -> Html Msg
viewInfo info =
    div [ class "pb-6" ]
        [ h1 [ class "section-title" ] [ text "ABOUT" ]
        , div [ class "card-container" ] (List.map2 viewInfoItem info infoClassNames)
        ]


viewInfoItem : Info -> String -> Html Msg
viewInfoItem info infoClassName =
    let
        className = info.icon ++ " " ++ infoClassName
    in
        div [ class "card" ]
            [ div [ class "about-card-title" ] [ text info.title ]
            , i [ class className ] []
            , div [ class "px-8 py-6" ]
                [ p [ class "card-text md:h-210px" ] [ viewDescription info.description ] ]
            ]


infoClassNames : List String
infoClassNames =
    [ "py-6 w-full text-center text-4rem md:text-5xl rotate-1/8 text-red-darker"
    , "py-2 md:py-4 w-full text-4rem text-center text-blue-dark"
    , "py-2 md:py-4 w-full text-4rem text-center text-yellow-dark" 
    ]


viewDescription : Description -> Html Msg
viewDescription (description as d) =
        div [] [ text d.ja ]


viewSkills : List Skill -> Html Msg
viewSkills skills =
    div [ class "py-6" ]
        [ h1 [ class "section-title" ] 
            [ text "LANGUAGE"
            , lgNewLine
            , text " SKILLS" ]
        , canvas [ class "langage-skills-chart mx-auto", id "language-skills" ] []
        ]


viewWebsites : List Website -> Html Msg
viewWebsites websites =
    div [ class "py-6" ] 
        [ h1 [ class "section-title" ] [ text "PORTFOLIO" ]
        , div [ class "card-container" ] (List.map2 viewWebsiteItem websites websiteIconColors)
        ]


viewWebsiteItem : Website -> String -> Html Msg
viewWebsiteItem (website as w) iconColor =
    let
        imageSrc = w.image.src
        imageAlt = w.image.alt
    in
        div [ class "card hover:shadow-lg my-5 md:my-0" ]
            [ a [ class "no-underline", href w.link ]
                [ div [ class "flex items-center h-74px py-3 px-4" ]
                    [ span [ class iconColor ]
                        [ i [ class w.icon ] [] ]
                    , div [ class "portfolio-card-title" ] [ text w.title ]
                    ]
                ]
                , img [ class "w-full", src imageSrc, alt imageAlt ] []
                , div [ class "px-8 py-4" ]
                    [ p [ class "card-text" ] [ viewDescription w.description ] ]
                , div [ class "px-5 pt-2 pb-4" ] (List.map viewTech w.tech)
            ]


websiteIconColors : List String
websiteIconColors =
    [ "card-avatar text-white bg-blue-darkest"
    , "text-2.5rem text-bitcoin-chart mr-4"
    , "card-avatar text-white bg-orange-darker"
    ]


viewTech : String -> Html Msg
viewTech tech =
    let
        tag = "#" ++ tech
    in
        span [ class "tooltip" ] [ text tag ]


viewOthers : List Other -> Html Msg
viewOthers others =
    div [ class "py-6" ] 
        [ h1 [ class "section-title" ] [ text "OTHERS" ]
        , div [ class "others-container" ] (List.map viewOther others)
        ]


viewOther : Other -> Html Msg
viewOther (other as o) =
    let
        imageSrc = o.image.src
        imageAlt = o.image.alt
    in
        div [ class "other-item" ] 
            [ a [ class "no-underline", href o.link ]
                [ img [ class "other-image hover:shadow-lg", src imageSrc, alt imageAlt ] []
                ]
            , div [ class "other-text" ] [ text o.title ]
            ]


viewContacts : List Contact -> Html Msg
viewContacts contacts =
    div [] 
        [ div [ class "bg-grey-lighter text-center" ] 
            [ h1 [ class "w-full py-4 text-3xl md:text-2xl text-grey-darkest" ] [ text "CONTACT" ]
            , div [] (List.map viewContact contacts )
            , div [ class "pt-4 pb-3 text-grey-darker" ]
                [ p [ class "text-base md:text-sm" ] [ text "by MASHU KUSHIBIKI" ]
                , p [ class "text-base md:text-sm" ] [ text "created with Elm" ]
                ]
            ]
        ]


viewContact : Contact -> Html Msg
viewContact (contact as c) =
    let 
        className = "footer-icon" ++ " " ++ c.color
    in
        a [ class "no-underline", href c.link ]
            [ span [ class className ]
                [ i [ class c.icon ] [] ]
            ]


-- HTTP

getPortfolio : Cmd Msg
getPortfolio =
    Http.get
    { url = "/portfolio/src/elm/data.json"
    , expect = Http.expectJson GotPortfolio portfolioDecoder
    }


portfolioDecoder : Decoder Portfolio
portfolioDecoder =
    map5 Portfolio
        (field "info" (list infoDecoder) )
        (field "skills" (list skillDecoder) )
        (field "websites" (list websiteDecoder) )
        (field "others" (list otherDecoder) )
        (field "contacts" (list contactDecoder) )


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
