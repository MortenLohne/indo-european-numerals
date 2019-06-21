module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes as Html
import Html.Events exposing (..)
import Languages exposing (Language, Numeral, english, languages)
import List
import Platform.Cmd
import Random
import Random.List


type Submission
    = Correct
    | Incorrect
    | None


type ButtonColor
    = Red
    | Green
    | Black


colorStyle : ButtonColor -> String
colorStyle buttonColor =
    case buttonColor of
        Red ->
            "red"

        Green ->
            "green"

        Black ->
            "black"


type alias ButtonData =
    { number : Int
    , nameRomanized : String
    , nameIPA : String
    , color : ButtonColor
    }


type alias Model =
    { language : Language
    , options : List ButtonData
    , submission : Submission
    }


init : () -> ( Model, Cmd Msg )
init =
    \_ ->
        ( { submission = None
          , language = english
          , options = List.map buttonDataFromNumeral english.options
          }
        , randomLanguage
        )


buttonDataFromNumeral : Numeral -> ButtonData
buttonDataFromNumeral numeral =
    { number = numeral.number, nameRomanized = numeral.nameRomanized, nameIPA = numeral.nameIPA, color = Black }


randomLanguage : Cmd Msg
randomLanguage =
    Random.List.choose languages
        |> Random.andThen
            (\( languageMaybe, _ ) ->
                let
                    language =
                        Maybe.withDefault english languageMaybe
                in
                Random.map2 Tuple.pair
                    (Random.constant language)
                    (Random.List.shuffle (List.map buttonDataFromNumeral language.options))
            )
        |> Random.generate SwitchLanguage


type Msg
    = SwitchLanguage ( Language, List ButtonData )
    | Submit Submission
    | Hint
    | Click ( Int, Int )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SwitchLanguage ( language, options ) ->
            ( { model | language = language, options = options }, Cmd.none )

        Submit newSubmission ->
            ( { model | submission = newSubmission }, Cmd.none )

        Hint ->
            ( { model
                | options =
                    List.indexedMap
                        (\i buttonData ->
                            if (i + 1) == buttonData.number then
                                { buttonData | color = Green }

                            else
                                { buttonData | color = Red }
                        )
                        model.options
              }
            , Cmd.none
            )

        Click ( i, n ) ->
            let
                swappedOptions =
                    List.drop (i - 1) model.options
                        |> List.take (min 2 (i + 1))
                        |> List.reverse
                        |> List.map (\option -> { option | color = Black })
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
        [ div
            [ Html.style "display" "grid"
            , Html.style "max-width" "400px"
            , Html.style "grid-template-columns" "1fr 1fr 1fr"
            , Html.style "grid-template-rows" "1fr 1fr 1fr"
            , Html.style "gap" "10px"
            ]
            (renderNumberButtons options)
        , renderHintButton
        , renderSubmitButton options
        ]


renderHintButton : Html Msg
renderHintButton =
    button [ onClick Hint ] [ text "Hint" ]


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
        (\i { number, nameRomanized, color } ->
            let
                styles =
                    [ Html.style "height" "200px"
                    , Html.style "width" "200px"
                    , Html.style "font-size" "20px"
                    , Html.style "color" (colorStyle color)
                    ]
            in
            button
                (onClick
                    (Click ( i, number ))
                    :: (if i == 9 then
                            Html.style "grid-column-start" "2" :: styles

                        else
                            styles
                       )
                )
                [ text nameRomanized
                ]
        )
        options
