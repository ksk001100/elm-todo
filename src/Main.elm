module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- MODEL


type alias Todo =
    { id : Int
    , body : String
    }


type alias Model =
    { input : String
    , todos : List Todo
    }


init : Model
init =
    { input = ""
    , todos = [ { id = 0, body = "test" } ]
    }



-- UPDATE


type Msg
    = Input String
    | Submit
    | Delete Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        Input input ->
            { model | input = input }

        Delete id ->
            { model | todos = deleteTodo id model.todos }

        Submit ->
            { model
                | input = ""
                , todos = { id = getNextId model.todos, body = model.input } :: model.todos
            }


getNextId : List Todo -> Int
getNextId todos =
    case List.head todos of
        Just todo ->
            todo.id + 1

        Nothing ->
            0


deleteTodo : Int -> List Todo -> List Todo
deleteTodo id todos =
    case List.Extra.findIndex (\x -> x.id == id) todos of
        Just index ->
            List.Extra.removeAt index todos

        Nothing ->
            todos


view : Model -> Html Msg
view model =
    div [ style "margin" "15px", align "center" ]
        [ Html.form [ onSubmit Submit ]
            [ input [ value model.input, onInput Input ] []
            , button
                [ disabled (String.length model.input < 1) ]
                [ text "Submit" ]
            ]
        , table
            [ style "border" "1px black solid"
            , style "width" "500px"
            , style "border-collapse" "collapse"
            , style "margin" "20px 0"
            , style "table-layout" "fixed"
            ]
            [ thead
                [ style "border" "1px solid black"
                , style "background-color" "white"
                ]
                [ tr []
                    [ th [] [ text "ID" ]
                    , th [ style "text-align" "left" ] [ text "TODO" ]
                    , th [] [ text "DEL" ]
                    ]
                ]
            , tbody [] (List.map viewTodo model.todos)
            ]
        ]


viewTodo : Todo -> Html Msg
viewTodo todo =
    tr
        [ height 50 ]
        [ td [ style "text-align" "center" ] [ text (String.fromInt todo.id) ]
        , td [ style "word-break" "break-all" ] [ text todo.body ]
        , td [ style "text-align" "center" ]
            [ button
                [ style "background-color" "#fff"
                , style "border-style" "none"
                , style "border-radius" "5px"
                , onClick (Delete todo.id)
                ]
                [ text "x" ]
            ]
        ]
