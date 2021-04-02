module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html
    exposing
        ( Attribute
        , Html
        , a
        , button
        , div
        , h1
        , i
        , input
        , li
        , main_
        , option
        , p
        , progress
        , select
        , span
        , table
        , tbody
        , td
        , text
        , th
        , thead
        , tr
        , ul
        )
import Html.Attributes as HA
    exposing
        ( class
        , disabled
        , href
        , max
        , value
        )
import Html.Events
    exposing
        ( on
        , onClick
        , onInput
        , targetValue
        )
import Http
import Json.Decode as D
import List.Extra as LE
import Query exposing (..)
import Random
import Random.List
import Route exposing (Route)
import Time
import Update.Extra as UE
import Url
import Url.Builder as B



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = UrlRequested
        }



-- MODEL


type alias Model =
    { key : Nav.Key
    , page : Page
    , members : List String
    , roles : List String
    , timeLimitSecond : Int
    , cdStatus : CountDownStatus
    }


type Page
    = NotFound
    | IllegalPage
    | TopPage
    | MemberListPage


type CountDownStatus
    = Stop
    | Pause Int Int Int
    | Count Int Int Int


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    { key = key
    , page = TopPage
    , members = []
    , roles = []
    , timeLimitSecond = 600
    , cdStatus = Stop
    }
        |> goTo (Route.parse url)



-- UPDATE


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url
    | AddInput Target
    | UpdateInput Target Int String
    | DeleteInput Target Int
    | Shuffled Url.Url Target (List String)
    | RewriteQuery Url.Url
    | Tick Time.Posix
    | CountDownStart
    | CountDownPause
    | CountDownNext
    | CountDownStop
    | UpdateTime (Maybe Int)


type Target
    = Member
    | Role


targetToModel : Target -> Model -> List String
targetToModel target model =
    case target of
        Member ->
            model.members

        Role ->
            model.roles


targetToString : Target -> String
targetToString target =
    case target of
        Member ->
            "member"

        Role ->
            "role"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    case Route.parse url of
                        Nothing ->
                            ( model, Nav.pushUrl model.key <| Url.toString url )

                        Just (Route.MemberList _ _) ->
                            ( { model | cdStatus = Stop }, Cmd.none )
                                |> UE.addCmd (Random.generate (Shuffled url Member) (Random.List.shuffle model.members))
                                -- |> UE.addCmd (Random.generate (Shuffled url Role) (Random.List.shuffle model.roles))
                                |> UE.addCmd (Nav.pushUrl model.key <| Url.toString (updateQuery url <| appQuery model))

                        Just (Route.Top _ _) ->
                            ( { model | members = [], roles = [], cdStatus = Stop }, Cmd.none )
                                |> UE.addCmd (Nav.pushUrl model.key <| Url.toString { url | query = Nothing })

                        Just Route.Reset ->
                            ( { model | members = [], roles = [] }, Cmd.none )
                                |> UE.andThen update (RewriteQuery { url | path = "/top" })

                        Just Route.UpdateQuery ->
                            ( model, Cmd.none )
                                |> UE.andThen update (RewriteQuery { url | path = "/top" })

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            goTo (Route.parse url) model

        AddInput target ->
            case target of
                Member ->
                    ( { model | members = "" :: model.members }, Cmd.none )

                Role ->
                    ( { model | roles = "" :: model.roles }, Cmd.none )

        UpdateInput target n value ->
            case target of
                Member ->
                    ( { model | members = LE.setAt n value model.members }, Cmd.none )

                Role ->
                    ( { model | roles = LE.setAt n value model.roles }, Cmd.none )

        DeleteInput target n ->
            case target of
                Member ->
                    ( { model | members = LE.removeAt n model.members }, Cmd.none )

                Role ->
                    ( { model | roles = LE.removeAt n model.roles }, Cmd.none )

        Shuffled url target list ->
            case target of
                Member ->
                    ( { model | members = list }, Cmd.none )
                        |> UE.andThen update (RewriteQuery url)

                Role ->
                    ( { model | roles = list }, Cmd.none )
                        |> UE.andThen update (RewriteQuery url)

        RewriteQuery url ->
            ( model, Nav.replaceUrl model.key <| Url.toString (updateQuery url <| appQuery model) )

        Tick _ ->
            case model.cdStatus of
                Stop ->
                    ( model, Cmd.none )

                Pause _ _ _ ->
                    ( model, Cmd.none )

                Count cycle over rest ->
                    case rest of
                        0 ->
                            ( { model | cdStatus = Count cycle (over + 1) model.timeLimitSecond }, Cmd.none )

                        _ ->
                            ( { model | cdStatus = Count cycle over (rest - 1) }, Cmd.none )

        CountDownStart ->
            if model.timeLimitSecond > 0 then
                case model.cdStatus of
                    Stop ->
                        ( { model | cdStatus = Count (List.length model.members - 1) 0 model.timeLimitSecond }, Cmd.none )

                    Pause cycle over rest ->
                        ( { model | cdStatus = Count cycle over rest }, Cmd.none )

                    Count _ _ _ ->
                        ( model, Cmd.none )

            else
                ( { model | cdStatus = Stop }, Cmd.none )

        CountDownPause ->
            case model.cdStatus of
                Stop ->
                    ( model, Cmd.none )

                Pause _ _ _ ->
                    ( model, Cmd.none )

                Count cycle over rest ->
                    ( { model | cdStatus = Pause cycle over rest }, Cmd.none )

        CountDownNext ->
            case model.cdStatus of
                Stop ->
                    ( model, Cmd.none )

                Pause cycle _ _ ->
                    if cycle == 0 then
                        ( { model | cdStatus = Stop }, Cmd.none )

                    else
                        ( { model | cdStatus = Pause (cycle - 1) 0 model.timeLimitSecond }, Cmd.none )

                Count cycle _ _ ->
                    if cycle == 0 then
                        ( { model | cdStatus = Stop }, Cmd.none )

                    else
                        ( { model | cdStatus = Count (cycle - 1) 0 model.timeLimitSecond }, Cmd.none )

        CountDownStop ->
            case model.cdStatus of
                Stop ->
                    ( model, Cmd.none )

                Pause _ _ _ ->
                    ( { model | cdStatus = Stop }, Cmd.none )

                Count _ _ _ ->
                    ( { model | cdStatus = Stop }, Cmd.none )

        UpdateTime maybeInitialTime ->
            case maybeInitialTime of
                Just timeLimitSecond ->
                    if timeLimitSecond > 0 then
                        ( { model | timeLimitSecond = timeLimitSecond }, Cmd.none )

                    else
                        ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )


goTo : Maybe Route -> Model -> ( Model, Cmd Msg )
goTo maybeRoute model =
    case maybeRoute of
        Nothing ->
            ( { model | page = NotFound }, Cmd.none )

        Just (Route.Top maybeMembers maybeRoles) ->
            ( { model
                | page = TopPage
                , members = queryValueToList maybeMembers
                , roles = queryValueToList maybeRoles
              }
            , Cmd.none
            )

        Just (Route.MemberList maybeMembers maybeRoles) ->
            case ( maybeMembers, maybeRoles ) of
                ( Just _, _ ) ->
                    ( { model
                        | page = MemberListPage
                        , members = queryValueToList maybeMembers
                        , roles = queryValueToList maybeRoles
                      }
                    , Cmd.none
                    )

                _ ->
                    ( { model | page = IllegalPage }, Cmd.none )

        Just Route.Reset ->
            ( { model | page = IllegalPage }, Cmd.none )

        Just Route.UpdateQuery ->
            ( { model | page = IllegalPage }, Cmd.none )


appQuery : Model -> List Query
appQuery model =
    [ StringListQuery "members" model.members
    , StringListQuery "roles" model.roles
    ]


queryValueToList : Maybe String -> List String
queryValueToList maybeQuery =
    case maybeQuery of
        Just query ->
            String.split "," query

        Nothing ->
            []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.cdStatus of
        Stop ->
            Sub.none

        Pause _ _ _ ->
            Sub.none

        Count _ _ _ ->
            Time.every 100 Tick



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Meeting Helper"
    , body =
        [ h1
            [ class "title", class "is-1" ]
            [ a [ href "/top" ] [ text "Meeting Preparator" ] ]
        , case model.page of
            NotFound ->
                viewNotFound

            IllegalPage ->
                viewIllegalPage

            TopPage ->
                viewTopPage model

            MemberListPage ->
                viewMemberListPage model
        ]
    }


viewNotFound : Html Msg
viewNotFound =
    text "Not found."


viewIllegalPage : Html Msg
viewIllegalPage =
    text "Illegal parameters."


viewTopPage : Model -> Html Msg
viewTopPage model =
    main_ []
        [ div
            [ class "buttons" ]
            [ resetButton
            , generateButton model
            , updateUrlButton
            ]
        , div
            [ class "columns" ]
            [ inputColumn Member model
            , inputColumn Role model
            ]
        ]


resetButton : Html msg
resetButton =
    a
        [ class "button"
        , class "is-danger"
        , href <| B.absolute [ "reset" ] []
        ]
        [ text "Reset..." ]


generateButton : Model -> Html msg
generateButton model =
    if List.length (List.filter (not << String.isEmpty) model.members) < 1 then
        button
            [ class "button"
            , class "is-info"
            , disabled True
            ]
            [ text "Generate...?" ]

    else
        a
            [ class "button"
            , class "is-info"
            , href <| B.absolute [ "result" ] []
            ]
            [ text "Generate!" ]


updateUrlButton : Html Msg
updateUrlButton =
    a
        [ class "button"
        , class "is-info"
        , href <| B.absolute [ "updatequery" ] []
        ]
        [ text "Update url" ]


inputColumn : Target -> Model -> Html Msg
inputColumn target model =
    div
        [ class "column"
        , class "is-half"
        ]
        [ div [ class "buttons" ]
            [ button
                [ class "button"
                , class "is-primary"

                -- , disabled (List.length (List.filter String.isEmpty (targetToModel target model)) > 0)
                , onClick (AddInput target)
                ]
                [ text <| "Add " ++ targetToString target ]
            ]
        , div [] [ ul [] (List.indexedMap (inputItem target) <| targetToModel target model) ]
        ]


inputItem : Target -> Int -> String -> Html Msg
inputItem target idx item =
    li []
        [ div
            [ class "columns"
            , class "is-mobile"
            , class "is-variable"
            , class "is-1"
            ]
            [ div
                [ class "column", class "is-narrow" ]
                [ button
                    [ class "button"
                    , class "is-danger"
                    , class "is-outlined"
                    , onClick (DeleteInput target idx)
                    ]
                    [ span [] [ text "Delete" ]
                    , span [ class "icon", class "is-small" ]
                        [ i [ class "fas", class "fa-times" ] [] ]
                    ]
                ]
            , div [ class "column" ]
                [ input [ value item, class "input", onInput (UpdateInput target idx) ] [] ]
            ]
        ]


onChange : (String -> msg) -> Attribute msg
onChange changeMsg =
    on "change" (D.map changeMsg targetValue)


viewMemberListPage : Model -> Html Msg
viewMemberListPage model =
    main_ []
        [ div [ class "level" ]
            [ div [ class "level-left" ]
                [ div [ class "level-item" ] [ span [ class "subtitle" ] [ text "Amount of time: " ] ]
                , div [ class "level-item" ] [ initialTimeLimitSelection model ]
                ]
            , div [ class "level-right" ]
                [ div [ class "level-item" ]
                    [ div [ class "buttons" ]
                        [ countStartPauseButton model.cdStatus
                        , countNextButton model.cdStatus
                        , countStopButton model.cdStatus
                        ]
                    ]
                ]
            ]
        , div [ class "table-container" ]
            [ table
                [ class "table", class "is-fullwidth", class "is-striped" ]
                [ thead [] [ tr [] [ th [] [ text "Rest time" ], th [] [ text "Member" ], th [] [ text "Role" ] ] ]
                , tbody [] (resultTable model)
                ]
            ]
        ]


initialTimeLimitSelection : Model -> Html Msg
initialTimeLimitSelection model =
    select
        [ value <| String.fromInt model.timeLimitSecond
        , class "select"
        , disabled (model.cdStatus /= Stop)
        , onChange (String.toInt >> UpdateTime)
        ]
        [ option [ value "100" ] [ text "10s" ]
        , option [ value "300" ] [ text "30s" ]
        , option [ value "600" ] [ text "60s" ]
        , option [ value "900" ] [ text "90s" ]
        , option [ value "1200" ] [ text "120s" ]
        , option [ value "1800" ] [ text "180s" ]
        ]


countStartPauseButton : CountDownStatus -> Html Msg
countStartPauseButton cdStatus =
    case cdStatus of
        Stop ->
            button [ class "button", class "is-info", onClick CountDownStart ] [ text "Start!" ]

        Pause _ _ _ ->
            button [ class "button", class "is-info", onClick CountDownStart ] [ text "Resume" ]

        Count _ _ _ ->
            button [ class "button", class "is-info", onClick CountDownPause ] [ text "Pause" ]


countNextButton : CountDownStatus -> Html Msg
countNextButton cdStatus =
    button
        [ class "button"
        , class "is-info"
        , onClick CountDownNext
        , disabled (cdStatus == Stop)
        ]
        [ text "Next" ]


countStopButton : CountDownStatus -> Html Msg
countStopButton cdStatus =
    button
        [ class "button"
        , class "is-danger"
        , onClick CountDownStop
        , disabled (cdStatus == Stop)
        ]
        [ text "Stop" ]


resultTable : Model -> List (Html Msg)
resultTable model =
    let
        lm =
            List.length model.members

        lr =
            List.length model.roles

        row i ( m, r ) =
            tr []
                [ td [] [ renderProgres i model ]
                , td [] [ text m ]
                , td [] [ text r ]
                ]
    in
    List.indexedMap row <|
        LE.zip
            (model.members ++ List.repeat (lr - lm) "")
            (model.roles ++ List.repeat (lm - lr) "")


renderProgres : Int -> Model -> Html msg
renderProgres i model =
    let
        viewProgres cy ov re =
            if cy == (List.length model.members - (1 + i)) then
                progress
                    [ class "progress"
                    , class <| progresClass ov re
                    , value <| String.fromInt re
                    , HA.max <| String.fromInt model.timeLimitSecond
                    ]
                    []

            else
                text ""

        progresClass ov re =
            let
                ratio =
                    toFloat re / toFloat model.timeLimitSecond
            in
            if ov == 0 then
                if ratio > 0.5 then
                    "is-info"

                else if ratio <= 0.5 && ratio > 0.2 then
                    "is-warning"

                else
                    "is-danger"

            else
                "is-danger"
    in
    case model.cdStatus of
        Stop ->
            text ""

        Pause cycle over rest ->
            viewProgres cycle over rest

        Count cycle over rest ->
            viewProgres cycle over rest
