import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing ( onClick, on, targetValue )
import Json.Decode as Json


main : Program Never Model Msg
main =
  Html.beginnerProgram { model = initialModel, view = view, update = update }



type alias Model =
  {
    max: Int
  , fizz: Int
  , buzz: Int
  }

initialModel : Model
initialModel =
  {
    max = 20
  , fizz = 3
  , buzz = 5
  }


-- UPDATE
type Msg
  = NoMsg
  | ChangeMax String
  | ChangeFizz String
  | ChangeBuzz String

update : Msg -> Model -> Model
update msg model =
  let
    toIntOrZero = \string -> getOrElse 0 <| String.toInt string
  in
    case msg of
      ChangeMax max -> { model | max = toIntOrZero max }

      ChangeFizz fizz -> { model | fizz = toIntOrZero fizz }

      ChangeBuzz buzz -> { model | buzz = toIntOrZero buzz }

      _ -> model


getOrElse : Int -> Result a Int -> Int
getOrElse els result =
  case result of
    Ok value -> value

    Err _ -> els


view : Model -> Html Msg
view model =
  let
    fizzBuzz =
      {
        fizz = model.fizz
      , buzz = model.buzz
      }
    fizzBuzzElementList = List.range 1 model.max |> List.map (fizzBuzzContainer fizzBuzz)
  in
    div [][
      div [class "fizzbuzzInputs"] [
        fizzBuzzInput model
      , maxInput model
      ]
    , ul [class "fizzBuzzList"] fizzBuzzElementList
    ]


onChange : (String -> msg) -> Html.Attribute msg
onChange tagger =
  on "change" (Json.map tagger targetValue)


maxInput : Model -> Html Msg
maxInput model =
  div [] [
    label [] [text "1 -"]
  , input [type_ "number", value <| toString model.max, onChange ChangeMax] []
  ]


fizzBuzzInput : Model -> Html Msg
fizzBuzzInput model =
  div [class "fizzbuzzInputs"] [
    div [class "fizzContainer"] [
      label [] [text "Fizz"]
    , input [type_ "number", value <| toString model.fizz, onChange ChangeFizz] []
    ]
  , div [class "buzzContainer"] [
      label [] [text "Buzz"]
    , input [type_ "number", value <| toString model.buzz, onChange ChangeBuzz] []
    ]
  ]


fizzBuzzContainer : {fizz: Int, buzz: Int} -> Int -> Html Msg
fizzBuzzContainer fizzBuzz i =
  fizzBuzzElement <|
    if i % (fizzBuzz.fizz * fizzBuzz.buzz) == 0 then
      {
        content = "fizzbuzz"
      , class = "fizzbuzz"
      }
    else if i % fizzBuzz.fizz == 0 then
      {
        content = "fizz"
      , class = "fizz"
      }
    else if i % fizzBuzz.buzz == 0 then
      {
        content = "buzz"
      , class = "buzz"
      }
    else
      {
        content = toString i
      , class = "numberElement"
      }


fizzBuzzElement : {content: String, class: String} -> Html Msg
fizzBuzzElement fizzBuzzModel =
  li [class fizzBuzzModel.class] [
    text fizzBuzzModel.content
  ]