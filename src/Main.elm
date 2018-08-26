module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Events exposing (..)
import List
import Platform.Cmd
import Random


type Submission
    = Correct
    | Incorrect
    | None


type alias Model =
    { language : Language
    , options : List ButtonData
    , submission : Submission
    }


type alias Language =
    { name : String
    , description : String
    , options : List ButtonData
    }


type alias ButtonData =
    { number : Int
    , nameRomanized : String
    , nameIPA : String
    }


english : Language
english =
    { name = "English"
    , description = ""
    , options =
        List.map2
            (\i name -> { number = i, nameRomanized = name, nameIPA = name })
            (List.range 1 10)
            [ "One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten" ]
    }


tocharianB : Language
tocharianB =
    { name = "Tocharian B"
    , description = ""
    , options =
        List.map2
            (\i name -> { number = i, nameRomanized = name, nameIPA = name })
            (List.range 1 10)
            [ "ṣe", "wi", "trai", "śtwer", "piś", "ṣkas", "ṣukt", "okt", "ñu", "śak" ]
    }


irish : Language
irish =
    { name = "Irish"
    , description = ""
    , options =
        List.map2
            (\i name -> { number = i, nameRomanized = name, nameIPA = name })
            (List.range 1 10)
            [ "aon", "dó", "trí", "ceathair", "cúig", "sé", "seacht", "ocht", "naoi", "deich" ]
    }


languages =
    [ english, tocharianB, irish ]


init : () -> ( Model, Cmd Msg )
init =
    \_ ->
        ( { submission = None
          , language = tocharianB
          , options = tocharianB.options
          }
        , randomLanguage
        )


randomLanguage : Cmd Msg
randomLanguage =
    Random.generate
        (\i ->
            let
                language =
                    List.drop i languages
                        |> List.head
            in
            Maybe.withDefault english language
        )
        (Random.int 0 (List.length languages - 1))
        |> Cmd.map
            (\language ->
                SwitchLanguage
                    ( language
                    , language.options
                    )
            )



-- UPDATE


type Msg
    = SwitchLanguage ( Language, List ButtonData )
    | Submit Submission
    | Click ( Int, Int )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SwitchLanguage ( language, options ) ->
            ( { model | language = language, options = options }, Cmd.none )

        Submit newSubmission ->
            ( { model | submission = newSubmission }, Cmd.none )

        Click ( i, n ) ->
            let
                swappedOptions =
                    List.drop (i - 1) model.options
                        |> List.take (min 2 (i + 1))
                        |> List.reverse
            in
            ( { model
                | submission = None
                , options =
                    List.drop (i + 1) model.options
                        |> List.append swappedOptions
                        |> List.append (List.take (i - 1) model.options)
              }
            , Cmd.none
            )


checkOptions : List ButtonData -> Bool
checkOptions options =
    List.sortBy .number options == options


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \model -> Sub.none
        }



-- VIEW


view : Model -> Html Msg
view { options, submission } =
    div []
        [ case submission of
            Correct ->
                div []
                    [ renderButtons options
                    , text "Congratulations! You made it"
                    ]

            Incorrect ->
                div []
                    [ renderButtons options
                    , text "Sorry, that's not correct. Try again!"
                    ]

            None ->
                renderButtons options
        ]


renderButtons : List ButtonData -> Html Msg
renderButtons options =
    div []
        [ div [] (renderNumberButtons options)
        , renderSubmitButton options
        ]


renderSubmitButton : List ButtonData -> Html Msg
renderSubmitButton options =
    button
        [ onClick
            (if checkOptions options then
                Submit Correct

             else
                Submit Incorrect
            )
        ]
        [ text "Submit" ]


renderNumberButtons : List ButtonData -> List (Html Msg)
renderNumberButtons options =
    List.indexedMap
        (\i { number, nameRomanized } ->
            button
                [ onClick (Click ( i, number )) ]
                [ text nameRomanized ]
        )
        options
