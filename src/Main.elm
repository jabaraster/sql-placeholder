port module Main exposing (..)

import Browser
import Css exposing (..)
import Html
import Html.Styled exposing (button, div, hr, text, textarea)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Json.Decode as Decode
import SqlParser



-- MODEL


type alias Model =
    { count : Int
    , sql : String
    , debug : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { count = 0
      , sql = ""
      , debug = ""
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Increment
    | Decrement
    | OnInputSql String
    | Parse
    | JsMessage String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( { model | count = model.count + 1 }, Cmd.none )

        Decrement ->
            ( { model | count = model.count - 1 }, Cmd.none )

        OnInputSql sql ->
            ( { model | sql = sql, debug = Debug.toString <| SqlParser.parse sql }, Cmd.none )

        Parse ->
            ( model, Cmd.none )

        JsMessage message ->
            -- Handle the message from JavaScript here
            ( model, Cmd.none )



-- VIEW


view : Model -> Html.Html Msg
view model =
    Html.Styled.toUnstyled <|
        div [ css [ margin (rem 1) ] ]
            [ textarea [ class "textarea", value model.sql, onInput OnInputSql ] []
            , button [ class "button", onClick Parse ] [ text "parse" ]
            , hr [] []
            , div [] [ text model.debug ]
            ]



-- SUBSCRIPTIONS


port jsToElm : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    jsToElm JsMessage



-- MAIN


main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }
