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
    li [ style [("color", "white")]]
        [ text item
        , button [ style [("background-color", "lightgrey")
                          , ("color", "navy")
                          , ("font-size", "8px")
                          , ("border-radius", "5px")
                          , ("margin", "0 0 0 5px")]
                          , onClick (RemoveItem item) ]
                  [ text "Remove" ]
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
  div [ style [("margin", "2% auto")
                , ("display", "flex")
                , ("flex-direction", "row")
                , ("align-items", "flex-start")
                , ("justify-content", "center")
                , ("width", "85%")
                , ("color", "white")]]
    [ div []
        [ div []
            [ h1 [ style [("color", "white")]]
                [ text "Today's Calories:" ]
            ]
        , div []
            [div []
              [ h3 [ style [("color", "white")]]
                  [ text (toString model.calories) ]
              ]
              , input [ placeholder "Amount of Calories", onInput SetCalories ] []
              , div [] []
              , button [ style [("background-color", "grey")
                                , ("color", "white")
                                , ("margin", "2px")
                                , ("border-radius", "5px")]
                                , onClick Increment ]
                        [ text "Food" ]
              , button [ style [("background-color", "grey")
                                , ("color", "white")
                                , ("margin", "2px")
                                , ("border-radius", "5px")]
                                , onClick Decrement ]
                        [ text "Exercise" ]
              , button [ style [("background-color", "grey")
                                , ("color", "white")
                                , ("margin", "2px")
                                , ("border-radius", "5px")]
                                ,onClick Reset ]
                                [ text "Clear" ]
            ]
        ]
    , div [ style [("margin", "0 auto")]]
        [ div []
            [ h1 [ style [("color", "white")]]
                [ text "Today's Food and Exercise:" ]
            ]
        , div []
            [div []
                [ input [ type_ "text", onInput UpdateText, value model.item, placeholder "Food or Exercise" ] []
                , button [ style [("background-color", "grey")
                                  , ("color", "white")
                                  , ("margin", "2px")
                                  , ("border-radius", "5px")]
                                  , onClick AddItem ]
                          [ text "Add Item" ]
                , div []
                  [ itemList model.items ]
                ]
                ]
        ]
    ]
