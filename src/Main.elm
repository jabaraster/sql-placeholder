port module Main exposing (..)

import Browser
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Json.Decode as Decode



-- MODEL


type alias Model =
    { count : Int }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { count = 0 }, Cmd.none )



-- UPDATE


type Msg
    = Increment
    | Decrement
    | JsMessage String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( { model | count = model.count + 1 }, Cmd.none )

        Decrement ->
            ( { model | count = model.count - 1 }, Cmd.none )

        JsMessage message ->
            -- Handle the message from JavaScript here
            ( model, Cmd.none )



-- VIEW


view : Model -> Html.Html Msg
view model =
    Html.Styled.toUnstyled <|
        div []
            [ div [] [ text (String.fromInt model.count) ]
            , button [ onClick Increment ] [ text "+" ]
            , button [ onClick Decrement ] [ text "-" ]
            ]



-- SUBSCRIPTIONS


port jsToElm : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    jsToElm JsMessage



-- MAIN


main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }
