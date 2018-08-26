module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import List


main =
    Browser.sandbox { init = init, update = update, view = view }


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


init : Model
init =
    { submission = None
    , language = tocharianB
    , options = tocharianB.options
    }



-- UPDATE


type Msg
    = Submit Submission
    | Click ( Int, Int )


update : Msg -> Model -> Model
update msg model =
    case msg of
        Submit newSubmission ->
            { model | submission = newSubmission }

        Click ( i, n ) ->
            let
                swappedOptions =
                    List.drop (i - 1) model.options
                        |> List.take (min 2 (i + 1))
                        |> List.reverse
            in
            { model
                | submission = None
                , options =
                    List.drop (i + 1) model.options
                        |> List.append swappedOptions
                        |> List.append (List.take (i - 1) model.options)
            }


checkOptions : List ButtonData -> Bool
checkOptions options =
    List.sortBy .number options == options



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
