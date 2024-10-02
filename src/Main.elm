port module Main exposing (..)

import Browser
import Bulma.Classes as B
import Css exposing (..)
import Dict exposing (Dict)
import Html
import Html.Styled exposing (Html, button, div, hr, input, label, section, text, textarea)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Json.Decode as Decode
import List.Extra as List
import Set
import SqlParser exposing (..)
import Util exposing (ListElement(..))
import Views



-- MODEL


type alias Model =
    { sql : String
    , sqlTokens : List Token
    , placeholderValues : Dict String String
    , formattedSql : String
    , debug : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    { sql = ""
    , sqlTokens = []
    , placeholderValues = Dict.empty
    , formattedSql = ""
    , debug = ""
    }
        |> onInputSql "select * from users where id = @id"



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
            onInputSql sql model

        Parse ->
            ( model, Cmd.none )

        OnInputPlaceholderValues placeholderName value ->
            case Dict.get placeholderName model.placeholderValues of
                Just _ ->
                    let
                        new =
                            Dict.update
                                placeholderName
                                (\_ -> Just value)
                                model.placeholderValues
                    in
                    ( { model
                        | placeholderValues = new
                      }
                    , formatSql <| buildSql new model.sqlTokens
                    )

                Nothing ->
                    ( model, Cmd.none )

        GotFormattedSql s ->
            ( { model | formattedSql = s }, Cmd.none )


toPlaceholderValues : List Token -> Dict String String
toPlaceholderValues =
    List.foldr
        (\token acum ->
            case token of
                Placeholder name ->
                    Dict.insert name "" acum

                _ ->
                    acum
        )
        Dict.empty


buildSql : Dict String String -> List Token -> String
buildSql placeholderValues =
    String.concat
        << List.map
            (\token ->
                case token of
                    Literal s ->
                        s

                    Placeholder name ->
                        case Dict.get name placeholderValues of
                            Just val ->
                                if val == "" then
                                    name

                                else
                                    val

                            Nothing ->
                                name
            )


onInputSql sql model =
    case SqlParser.parse sql of
        Err err ->
            ( { model | sql = sql }, Cmd.none )

        Ok tokens ->
            let
                new =
                    Dict.union model.placeholderValues <| toPlaceholderValues tokens
            in
            ( { model
                | sql = sql
                , sqlTokens = tokens
                , placeholderValues = new
              }
            , formatSql <| buildSql new tokens
            )



-- VIEW


view : Model -> Html.Html Msg
view model =
    Html.Styled.toUnstyled <|
        div [ class "main-area", css [ margin (rem 1) ] ] <|
            Views.build
                [ Single <|
                    section [ class "source-sql-area", class B.content ]
                        [ label [] [ text "Source SQL" ]
                        , textarea
                            [ class B.textarea
                            , css styleTextarea
                            , value model.sql
                            , onInput OnInputSql
                            ]
                            []
                        ]
                , Single <|
                    section [ class "placeholders-area", class B.content ] <|
                        (label [ class B.pt4, class B.content ] [ text "Placeholders" ]
                            :: (List.map (viewPlaceholderValue model.placeholderValues) <|
                                    placeholderNames model.sqlTokens
                               )
                        )
                , Single <|
                    section [ class "executable-sql-area", class B.content ]
                        [ label [] [ text "Executable SQL" ]
                        , textarea
                            [ class B.textarea
                            , css  styleTextarea
                            , readonly True
                            ]
                            [ text model.formattedSql ]
                        ]
                ]

styleTextarea = [ fontFamily monospace, Css.height (pct 100) ]

viewPlaceholderValue : Dict String String -> String -> Html Msg
viewPlaceholderValue pv name =
    case Dict.get name pv of
        Nothing ->
            div [] []

        Just val ->
            div []
                [ label [ css [ display block ] ] [ text name ]
                , input
                    [ value val
                    , class B.input
                    , onInput <| OnInputPlaceholderValues name
                    ]
                    []
                ]


placeholderNames : List Token -> List String
placeholderNames =
    Tuple.first
        << List.foldr
            (\token ( acum, dupChekder ) ->
                case token of
                    Placeholder name ->
                        if Set.member name dupChekder then
                            ( acum, dupChekder )

                        else
                            ( name :: acum, Set.insert name dupChekder )

                    _ ->
                        ( acum, dupChekder )
            )
            ( [], Set.empty )



-- SUBSCRIPTIONS


port receiveFormattedSql : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    receiveFormattedSql GotFormattedSql


port formatSql : String -> Cmd msg



-- MAIN


main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }
