module App exposing (..)

import ViewHelpers exposing (..)
import Types exposing (Msg)
import Html exposing (..)
import Json.Decode as Json
import Stack exposing (..)
import Array exposing (..)
import Markdown exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)


main =
    Html.program
        { view = view
        , update = update
        , init = initialState
        , subscriptions = subscriptions
        }


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg
            else
                Json.fail "not ENTER"
    in
        on "keydown" (Json.andThen isEnter keyCode)


subscriptions model =
    Sub.none


buttonEnabled : String -> Attribute Msg
buttonEnabled input =
    case input of
        "" ->
            disabled True

        _ ->
            disabled False


view : Model -> Html Types.Msg
view model =
    div []
        [ introView
        , div [ id "interactivestack", class "row" ]
            [ div [ class "container" ]
                [ div [ class "col-sm-12" ]
                    [ h2 [ class "center" ]
                        [ text "Try the interactive Stack" ]
                    , div
                        [ id "content" ]
                        [ stackView (Stack.toList model.stack)
                        , br [] []
                        ]
                    , div [ class "row col-md-6 col-sm-12 col-md-offset-3" ]
                        [ div [ attribute "role" "form" ]
                            [ input [ id "input", type_ "text", value model.temp, onEnter Types.Push, onInput (\str -> Types.Temp str) ] []
                            , button [ buttonEnabled model.temp, class "push btn btn-default", onClick Types.Push ] [ text "Push" ]
                            ]
                        ]
                    , div [ style [ ( "margin-top", "7%" ) ], class "row" ]
                        [ div [ style [ ( "text-align", "center" ) ], class "col-md-6 col-md-offset-3 col-sm-12" ]
                            [ span [ class "h3", style [ ( "padding", "0.5em" ) ] ] [ text ("Popped: " ++ (Maybe.withDefault "" model.popped)) ]
                            , br [] []
                            , button
                                [ class "btn btn-default", onClick Types.Pop ]
                                [ text "Pop" ]
                            ]
                        ]
                    , div [ style [ ( "text-align", "center" ), ( "margin-bottom", "2%" ) ] ] [ a [ href "http://package.elm-lang.org/packages/mhoare/elm-stack/latest" ] [ text "View the elm-stack package" ] ]
                    ]
                ]
            ]
        , div [ class "container" ] [ div [ class "row" ] [ Markdown.toHtml [ class "col-sm-12" ] ViewHelpers.secondPara ] ]
        , div [ class "container" ] [ div [ class "row" ] [ Markdown.toHtml [ class "col-sm-12" ] ViewHelpers.stackOperationsPara ] ]
        , ViewHelpers.footer
        ]


filterNothing l =
    case l of
        "" ->
            Nothing

        _ ->
            Just l


renderList lst =
    ul []
        (List.map (\l -> li [] [ text l ]) lst)


toText string =
    div [] [ text string ]


update : Types.Msg -> Model -> ( Model, Cmd Types.Msg )
update msg model =
    case msg of
        Types.Pop ->
            let
                returnStack =
                    Stack.pop model.stack
            in
                ( { model | stack = Tuple.second returnStack, popped = Tuple.first returnStack, temp = "" }, Cmd.none )

        Types.Push ->
            let
                returnStack =
                    Stack.push model.temp model.stack
            in
                ( { model | stack = returnStack, temp = "" }, Cmd.none )

        Types.Temp item ->
            ( { model | temp = item }, Cmd.none )


type alias Model =
    { stack : Stack String
    , popped : Maybe String
    , temp : String
    }


initialState : ( Model, Cmd Msg )
initialState =
    ( { temp = "", stack = Stack.initialise, popped = Nothing }
    , Cmd.none
    )
