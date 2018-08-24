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
    { options : List ButtonData
    , submission : Submission
    }


init : Model
init =
    { submission = None
    , options =
        [ { number = 1, nameRomanized = "One", nameIPA = "One" }
        , { number = 2, nameRomanized = "Two", nameIPA = "Two" }
        , { number = 3, nameRomanized = "Three", nameIPA = "Three" }
        , { number = 4, nameRomanized = "Four", nameIPA = "Four" }
        ]
    }


type alias ButtonData =
    { number : Int
    , nameRomanized : String
    , nameIPA : String
    }



-- UPDATE


type Msg
    = Submit Submission
    | Click ( Int, Int )


update : Msg -> Model -> Model
update msg { options, submission } =
    case msg of
        Submit newSubmission ->
            { options = options, submission = newSubmission }

        Click ( i, n ) ->
            let
                swappedOptions =
                    List.drop (i - 1) options
                        |> List.take (min 2 (i + 1))
                        |> List.reverse
            in
            { submission = None
            , options =
                List.drop (i + 1) options
                    |> List.append swappedOptions
                    |> List.append (List.take (i - 1) options)
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
