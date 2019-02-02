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


port languageSkillsToJs : List Skill -> Cmd msg


-- MODEL

type alias Model =
    { portfolio : Portfolio
    , locale : Locale
    , error : Bool
    }


emptyModel : Model
emptyModel =
    { portfolio =
        { info = []
        , skills = []
        , websites = []
        , others = []
        , contacts = []
        }
    , locale = "English"
    , error = False
    }


init : () -> ( Model, Cmd Msg )
init _ = (emptyModel, getPortfolio)


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


type alias Locale = String


-- UPDATE

type Msg
    = GotPortfolio (Result Http.Error Portfolio)
    | SetEnglish
    | SetJapanese
    | SetChinese
    | LoadAgain


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =            
    case msg of
        GotPortfolio result ->
            case result of
                Ok portfolio ->
                    ( { model | portfolio = portfolio }
                    , (languageSkillsToJs portfolio.skills)
                    )

                Err _ ->
                    ( { model | error = True }
                    , Cmd.none
                    )

        SetEnglish ->
            ( { model | locale = "English" }
            , Cmd.none
            )

        SetJapanese ->
            ( { model | locale = "Japanese" }
            , Cmd.none
            )

        SetChinese ->
            ( { model | locale = "Chinese" }
            , Cmd.none
            )

        LoadAgain ->
            ( { model | error = False }
            , getPortfolio
            )


-- VIEW

view : Model -> Html Msg
view { portfolio, locale, error } =
    if error == False then
        div [] [ viewApp portfolio locale ]

    else 
        div []
            [ text "Something is wrong with GitHub Pages."
            , newLine
            , text "Do you want to reload? Then "
            , button [ onClick LoadAgain ] [ text "Click me!" ]
            ]


viewApp : Portfolio -> String -> Html Msg
viewApp { info, websites, others, contacts } locale =
        div []
            [ viewTop
            , div [ class "max-w-xl mx-auto container" ]
                [ viewFlags
                , viewInfo info locale
                , viewSkills
                , viewWebsites websites locale
                , viewOthers others
                ]
            , viewContacts contacts
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


viewFlags : Html Msg
viewFlags =
    div [ class "self-start pt-4 justify-center my-auto" ]
        [ ul [ class "flex justify-end px-3 pt-3 list-reset leading-narrow" ]
            [   li [ class "flag mr-2", onClick SetEnglish ] [ text "🇬🇧" ]
            ,   li [ class "flag px-2 border-l border-r border-solid border-grey-dark", onClick SetJapanese ] [ text "🇯🇵" ]
            ,   li [ class "flag mx-2", onClick SetChinese ] [ text "🇨🇳" ]
            ]
        ]


viewInfo : List Info -> Locale -> Html Msg
viewInfo info locale =
    div [ class "pb-6" ]
        [ h1 [ class "section-title" ] [ text "ABOUT" ]
        , div [ class "card-container" ] (List.map3 viewInfoItem info infoClassNames (List.repeat 3 locale))
        ]


viewInfoItem : Info -> String -> Locale -> Html Msg
viewInfoItem { title, description, icon } infoClassName locale =
    let
        className = icon ++ " " ++ infoClassName
    in
        div [ class "card" ]
            [ div [ class "about-card-title" ] [ text title ]
            , i [ class className ] []
            , div [ class "px-8 py-6" ]
                [ p [ class "card-text md:h-210px" ] [ viewDescription description locale ] ]
            ]


infoClassNames : List String
infoClassNames =
    [ "py-6 w-full text-center text-4rem md:text-5xl rotate-1/8 text-red-darker"
    , "py-2 md:py-4 w-full text-4rem text-center text-blue-dark"
    , "py-2 md:py-4 w-full text-4rem text-center text-yellow-dark" 
    ]


viewDescription : Description -> Locale -> Html Msg
viewDescription { ja, en, ch } locale =
    case locale of
        "English" ->
            div [] [ text en ]
        "Japanese" ->
            div [] [ text ja ]
        "Chinese" ->
            div [] [ text ch ]
        _ ->
            div [] [ text en ]


viewSkills : Html Msg
viewSkills =
    div [ class "py-6" ]
        [ h1 [ class "section-title" ] 
            [ text "LANGUAGE"
            , lgNewLine
            , text " SKILLS" ]
        , canvas [ class "langage-skills-chart mx-auto", id "language-skills" ] []
        ]


viewWebsites : List Website -> Locale -> Html Msg
viewWebsites websites locale =
    div [ class "py-6" ] 
        [ h1 [ class "section-title" ] [ text "PORTFOLIO" ]
        , div [ class "card-container" ] (List.map3 viewWebsiteItem websites websiteIconColors (List.repeat 3 locale))
        ]


viewWebsiteItem : Website -> String -> Locale -> Html Msg
viewWebsiteItem { title, description, tech, image, icon, link } iconColor locale =
    let
        imageSrc = image.src
        imageAlt = image.alt
    in
        div [ class "card hover:shadow-lg my-5 md:my-0" ]
            [ a [ class "no-underline", href link ]
                [ div [ class "flex items-center h-74px py-3 px-4" ]
                    [ span [ class iconColor ]
                        [ i [ class icon ] [] ]
                    , div [ class "portfolio-card-title" ] [ text title ]
                    ]
                    , img [ class "w-full", src imageSrc, alt imageAlt ] []
                    , div [ class "px-8 py-4" ]
                        [ p [ class "card-text md:h-105px" ] [ viewDescription description locale ] ]
                    , div [ class "px-5 pt-2 pb-4" ] (List.map viewTech tech)
                ]
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
viewOther { title, image, link } =
    let
        imageSrc = image.src
        imageAlt = image.alt
    in
        div [ class "other-item" ] 
            [ a [ class "no-underline", href link ]
                [ img [ class "other-image hover:shadow-lg", src imageSrc, alt imageAlt ] []
                ]
            , div [ class "other-text" ] [ text title ]
            ]


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
viewContact { icon, color, link } =
    let 
        className = "footer-icon" ++ " " ++ color
    in
        a [ class "no-underline", href link ]
            [ span [ class className ]
                [ i [ class icon ] [] ]
            ]


newLine : Html Msg
newLine = br [] []


lgNewLine : Html Msg
lgNewLine = br [ class "block lg:hidden" ] []


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
