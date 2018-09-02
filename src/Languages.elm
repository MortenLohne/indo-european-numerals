module Languages exposing (Language, Numeral, english, languages)


type alias Language =
    { name : String
    , description : String
    , options : List Numeral
    }


type alias Numeral =
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
