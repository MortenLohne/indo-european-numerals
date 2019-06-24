module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Css exposing (..)
import Html
import Html.Attributes as Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href, src)
import Html.Styled.Events exposing (..)
import Languages exposing (Language, Numeral, english, languages)
import List
import Platform.Cmd
import Random
import Random.List


buttonSize =
    150


type Submission
    = Correct
    | Incorrect
    | None


type ButtonColor
    = Red
    | Green
    | Black


colorStyle : ButtonColor -> Color
colorStyle buttonColor =
    case buttonColor of
        Red ->
            rgb 255 0 0

        Green ->
            rgb 0 255 0

        Black ->
            rgb 0 0 0


type alias ButtonData =
    { number : Int
    , nameRomanized : String
    , nameIPA : String
    , clr : ButtonColor
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
    { number = numeral.number, nameRomanized = numeral.nameRomanized, nameIPA = numeral.nameIPA, clr = Black }


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
                                { buttonData | clr = Green }

                            else
                                { buttonData | clr = Red }
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
                        |> List.map (\option -> { option | clr = Black })
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
        , view = view >> toUnstyled
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
    let
        buttons =
            renderNumberButtons options

        -- The number 10 button goes between the hint and submit buttons
        tenButton =
            buttons |> List.reverse |> List.head |> Maybe.withDefault (text "")

        firstNineButtons =
            buttons |> List.reverse |> List.tail |> Maybe.withDefault [] |> List.reverse
    in
    div
        [ css
            [ property "display" "grid"
            , maxWidth (buttonSize * 2 |> px)
            , property "grid-template-columns" "1fr 1fr 1fr"
            , property "grid-template-rows" "1fr 1fr 1fr"
            , property "grid-auto-rows" <| String.fromInt buttonSize ++ "px"
            , property "grid-auto-columns" <| String.fromInt buttonSize ++ "px"
            , property "gap" <| String.fromFloat (buttonSize / 20) ++ "px"
            , property "justify-items" "center"
            , property "align-items" "center"
            ]
        ]
        (firstNineButtons ++ [ renderHintButton, tenButton, renderSubmitButton options ])


renderHintButton : Html Msg
renderHintButton =
    styled button
        [ width (buttonSize / 2 |> px)
        , height (buttonSize / 4 |> px)
        , fontSize (buttonSize / 10 |> px)
        ]
        [ onClick Hint ]
        [ text "Hint" ]


renderSubmitButton : List ButtonData -> Html Msg
renderSubmitButton options =
    styled
        button
        [ width (buttonSize / 2 |> px)
        , height (buttonSize / 4 |> px)
        , fontSize (buttonSize / 10 |> px)
        ]
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
        (\i { number, nameRomanized, clr } ->
            let
                styles =
                    [ height (px buttonSize)
                    , width (px buttonSize)
                    , fontSize (buttonSize / 5 |> px)
                    , color (colorStyle clr)
                    ]
            in
            styled button
                (if i == 9 then
                    property "grid-column-start" "2" :: styles

                 else
                    styles
                )
                [ onClick
                    (Click ( i, number ))
                ]
                [ text nameRomanized
                ]
        )
        options
