module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import String

-- MAIN

main =
  beginnerProgram
  { model = model
  , view = view
  , update = update
  }


-- MODEL

type alias Model = { calories: Int, newCalories: Int, item: String, items: List String }

model : Model
model =
    { calories = 0
    , newCalories = 0
    , item = ""
    , items = []
    }

-- UPDATE

type Msg
  = UpdateText String
  | AddItem
  | RemoveItem String
  | Increment
  | Decrement
  | Reset
  | SetCalories String

update : Msg -> Model -> Model
update msg model =
  case msg of

    UpdateText text ->
      { model | item = text }

    AddItem ->
      { model | items = model.item :: model.items }

    RemoveItem text ->
      { model | items = List.filter (\t -> t /= text) model.items }

    Increment ->
      { model | calories = model.calories + model.newCalories }

    Decrement ->
      { model | calories = model.calories - model.newCalories }

    Reset ->
      { model | calories = 0 }

    SetCalories step ->
      case String.toInt step of
        Err msg ->
          { model | newCalories = 0 }
        Ok val ->
          { model | newCalories = val }

-- VIEW

itemItem : String -> Html Msg
itemItem item =
    li []
        [ text item
        , button [ onClick (RemoveItem item) ] [ text "Remove" ]
        ]


itemList : List String -> Html Msg
itemList items =
    let
        children =
            List.map itemItem items
    in
        ul [] children


view : Model -> Html Msg
view model =
  div []
    [ div []
        [ input [ type_ "text", onInput UpdateText, value model.item ] []
        , button [ onClick AddItem ] [ text "Add Item" ]
        , div []
          [ itemList model.items ]
        ]
    ,  div []
        [ text "Today's Calories:" ]
    ,   div []
        [ div [] [ text (toString model.calories) ]
        , input [ placeholder "Amount of Calories", onInput SetCalories ] []
        , div [] []
        , button [ onClick Increment ] [ text "Food" ]
        , button [ onClick Decrement ] [ text "Exercise" ]
        , button [ onClick Reset ] [ text "Clear" ]
        ]
    ]
