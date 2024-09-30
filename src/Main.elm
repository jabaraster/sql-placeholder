port module Main exposing (..)

import Browser
import Bulma.Classes as B
import Css exposing (..)
import Html
import Html.Styled exposing (Html, button, div, hr, input, label, text, textarea)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Json.Decode as Decode
import List.Extra as List
import Set
import SqlParser exposing (..)
import Util exposing (ListElement(..))
import Views



-- MODEL


type alias PlaceholderValue =
    { name : String
    , value : String
    }


type alias Model =
    { sql : String
    , sqlTokens : List Token
    , placeholderValues : Maybe (List PlaceholderValue)
    , formattedSql : String
    , debug : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        initialSql =
            "select * from users where id = @id"
    in
    { sql = initialSql
    , sqlTokens = []
    , placeholderValues = Nothing
    , formattedSql = ""
    , debug = ""
    }
        |> parseCore



-- UPDATE


type Msg
    = OnInputSql String
    | Parse
    | OnInputPlaceholderValues String String
    | GotFormattedSql String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnInputSql sql ->
            ( { model | sql = sql }, Cmd.none )

        Parse ->
            parseCore model

        OnInputPlaceholderValues placeholderName value ->
            { model
                | placeholderValues =
                    flip Maybe.map
                        model.placeholderValues
                        (List.updateIf (\pv -> pv.name == placeholderName)
                            (\pv -> { pv | value = value })
                        )
            }
                |> (\m -> ( m, formatSql <| buildSql m ))

        GotFormattedSql s ->
            ( { model | formattedSql = s }, Cmd.none )


flip : (a -> b -> c) -> b -> a -> c
flip f b a =
    f a b


parseCore model =
    case SqlParser.parse model.sql of
        Ok tokens ->
            let
                ts =
                    tokens
                        |> List.filter isPlaceholder
                        |> List.map
                            (\token ->
                                case token of
                                    Placeholder name ->
                                        name

                                    _ ->
                                        ""
                            )
                        |> Set.fromList
                        |> Set.toList
                        |> List.sort
                        |> List.map (\name -> { name = name, value = "" })
            in
            ( { model | placeholderValues = Just ts, sqlTokens = tokens }, Cmd.none )

        Err err ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html.Html Msg
view model =
    Html.Styled.toUnstyled <|
        div [ css [ margin (rem 1) ] ] <|
            Views.build
                [ Single <|
                    textarea
                        [ class B.textarea
                        , css [ fontFamily monospace ]
                        , value model.sql
                        , onInput OnInputSql
                        ]
                        []
                , Single <| button [ class "button", onClick Parse ] [ text "parse" ]
                , Single <| div [ css [ fontFamily monospace ] ] [ text model.debug ]
                , Single <| hr [] []
                , Single <| label [] [ text "Placeholders" ]
                , Plural <| List.map viewPlaceholderValue <| Maybe.withDefault [] model.placeholderValues
                , Single <| hr [] []
                , Single <| label [] [ text "Executable SQL" ]
                , Single <|
                    textarea
                        [ class B.textarea
                        , css [ fontFamily monospace, fontSize (rem 0.8), Css.height (rem 30) ]
                        , readonly True
                        ]
                        [ text model.formattedSql ]
                ]


buildSql : Model -> String
buildSql model =
    String.concat <|
        List.map
            (\token ->
                case token of
                    Literal s ->
                        s

                    Placeholder name ->
                        case List.find (\pv -> pv.name == name) <| Maybe.withDefault [] model.placeholderValues of
                            Just pv ->
                                if pv.value == "" then
                                    name

                                else
                                    pv.value

                            Nothing ->
                                name
            )
            model.sqlTokens


viewPlaceholderValue : PlaceholderValue -> Html Msg
viewPlaceholderValue pv =
    div []
        [ label [ css [ display block ] ] [ text pv.name ]
        , input
            [ value pv.value
            , class B.input
            , onInput <| OnInputPlaceholderValues pv.name
            ]
            []
        ]



-- SUBSCRIPTIONS


port receiveFormattedSql : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    receiveFormattedSql GotFormattedSql


port formatSql : String -> Cmd msg



-- MAIN


main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }
