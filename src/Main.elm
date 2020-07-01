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
    div [ class "main", align "center" ]
        [ Html.form [ onSubmit Submit ]
            [ input [ value model.input, onInput Input ] []
            , button
                [ disabled (String.length model.input < 1) ]
                [ text "Submit" ]
            ]
        , table
            [ class "todo-table" ]
            [ thead
                [ class "todo-table-head" ]
                [ tr []
                    [ th [] [ text "ID" ]
                    , th [] [ text "TODO" ]
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
        [ td [ class "todo-table-col" ] [ text (String.fromInt todo.id) ]
        , td [ class "todo-table-col" ] [ text todo.body ]
        , td [ class "todo-table-col" ]
            [ button
                [ class "delete-button", onClick (Delete todo.id) ]
                [ text "x" ]
            ]
        ]