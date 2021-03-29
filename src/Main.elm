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
import Html.Attributes
    exposing
        ( class
        , disabled
        , href
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
    , initialTime : Int
    , cdStatus : CountDownStatus
    }


type Page
    = NotFound
    | IllegalPage
    | TopPage
    | MemberListPage


type CountDownStatus
    = Stop
    | Pause Int Int
    | Count Int Int


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    { key = key
    , page = TopPage
    , members = []
    , roles = []
    , initialTime = 10
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
    | CountDownStart
    | CountDownStop
    | CountDownPause
    | Tick Time.Posix
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
                            Debug.todo "ここには来ない"

                        Just (Route.MemberList _ _) ->
                            ( model, Cmd.none )
                                |> UE.addCmd (Random.generate (Shuffled url Member) (Random.List.shuffle model.members))
                                |> UE.addCmd (Random.generate (Shuffled url Role) (Random.List.shuffle model.roles))
                                |> UE.addCmd (Nav.pushUrl model.key <| Url.toString (updateQuery model url))

                        Just (Route.Top _ _) ->
                            ( { model | members = [], roles = [] }, Cmd.none )
                                |> UE.addCmd (Nav.pushUrl model.key <| Url.toString { url | query = Nothing })

                        Just Route.Reset ->
                            ( { model | members = [], roles = [] }, Cmd.none )
                                |> UE.addCmd (Nav.pushUrl model.key <| Url.toString { url | path = "/top", query = Nothing })

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
            ( model, Nav.replaceUrl model.key <| Url.toString (updateQuery model url) )

        Tick _ ->
            case model.cdStatus of
                Stop ->
                    ( model, Cmd.none )

                Pause _ _ ->
                    ( model, Cmd.none )

                Count cycle rest ->
                    case ( cycle, rest ) of
                        ( 0, 0 ) ->
                            ( { model | cdStatus = Stop }, Cmd.none )

                        ( _, 0 ) ->
                            ( { model | cdStatus = Count (cycle - 1) model.initialTime }, Cmd.none )

                        ( _, _ ) ->
                            ( { model | cdStatus = Count cycle (rest - 1) }, Cmd.none )

        CountDownStart ->
            if model.initialTime > 0 then
                case model.cdStatus of
                    Stop ->
                        ( { model | cdStatus = Count (List.length model.members - 1) model.initialTime }, Cmd.none )

                    Pause cycle rest ->
                        ( { model | cdStatus = Count cycle rest }, Cmd.none )

                    Count _ _ ->
                        ( model, Cmd.none )

            else
                ( { model | cdStatus = Stop }, Cmd.none )

        CountDownPause ->
            case model.cdStatus of
                Stop ->
                    ( model, Cmd.none )

                Pause _ _ ->
                    ( model, Cmd.none )

                Count cycle rest ->
                    ( { model | cdStatus = Pause cycle rest }, Cmd.none )

        CountDownStop ->
            case model.cdStatus of
                Stop ->
                    ( model, Cmd.none )

                Pause _ _ ->
                    ( { model | cdStatus = Stop }, Cmd.none )

                Count _ _ ->
                    ( { model | cdStatus = Stop }, Cmd.none )

        UpdateTime maybeInitialTime ->
            case maybeInitialTime of
                Just initialTime ->
                    if initialTime > 0 then
                        ( { model | initialTime = initialTime }, Cmd.none )

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
                ( Just _, Just _ ) ->
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


queryValueToList : Maybe String -> List String
queryValueToList maybeQuery =
    case maybeQuery of
        Just query ->
            String.split "," query

        Nothing ->
            []


listToQueryValue : List String -> String
listToQueryValue list =
    list
        |> List.filter (not << String.isEmpty)
        |> String.join ","


updateQuery : Model -> Url.Url -> Url.Url
updateQuery model url =
    { url
        | query =
            Just <|
                correctQuery <|
                    B.toQuery
                        [ B.string "members" <| listToQueryValue model.members
                        , B.string "roles" <| listToQueryValue model.roles
                        ]
    }


correctQuery : String -> String
correctQuery query =
    case String.uncons query of
        Just ( '?', query_ ) ->
            query_

        _ ->
            query



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.cdStatus of
        Stop ->
            Sub.none

        Pause _ _ ->
            Sub.none

        Count _ _ ->
            Time.every 1000 Tick



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
            [ class "columns"
            , class "is-desktop"
            ]
            [ viewInputColumn Member model
            , viewInputColumn Role model
            ]
        , div []
            [ a
                [ class "button"
                , class "is-danger"
                , href <| B.absolute [ "reset" ] []
                ]
                [ text "Reset..." ]
            , if List.length (List.filter (not << String.isEmpty) model.members) < 1 then
                button
                    [ class "button"
                    , class "is-link"
                    , disabled True
                    ]
                    [ text "Generate...?" ]

              else
                a
                    [ class "button"
                    , class "is-link"
                    , href <| B.absolute [ "result" ] []
                    ]
                    [ text "Generate!" ]
            ]
        ]


viewInputColumn : Target -> Model -> Html Msg
viewInputColumn target model =
    div
        [ class "column"
        , class "is-half-desktop"
        ]
        [ button
            [ class "button"
            , class "is-primary"
            , disabled (List.length (List.filter String.isEmpty (targetToModel target model)) > 0)
            , onClick (AddInput target)
            ]
            [ text <| "Add " ++ targetToString target ]
        , div []
            [ ul []
                (List.map
                    (viewInputItem target)
                    (List.indexedMap Tuple.pair <| targetToModel target model)
                )
            ]
        ]


viewInputItem : Target -> ( Int, String ) -> Html Msg
viewInputItem target ( idx, item ) =
    li []
        [ div
            [ class "columns"
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
        [ div []
            [ span [] [ text "Time Limit: " ]
            , select
                [ value <| String.fromInt model.initialTime
                , class "select"
                , class "is-primary"
                , class "is-medium"
                , disabled (model.cdStatus /= Stop)
                , onChange (String.toInt >> UpdateTime)
                ]
                [ option [ value "10" ] [ text "10s" ]
                , option [ value "30" ] [ text "30s" ]
                , option [ value "60" ] [ text "60s" ]
                , option [ value "90" ] [ text "90s" ]
                , option [ value "120" ] [ text "120s" ]
                , option [ value "180" ] [ text "180s" ]
                ]
            ]
        , div []
            [ button
                [ class "button"
                , onClick CountDownStart
                , disabled
                    (0
                        >= model.initialTime
                        || (case model.cdStatus of
                                Count _ _ ->
                                    True

                                _ ->
                                    False
                           )
                    )
                ]
                [ text "Start!" ]
            , button
                [ class "button"
                , onClick CountDownPause
                , disabled
                    (case model.cdStatus of
                        Count _ _ ->
                            False

                        _ ->
                            True
                    )
                ]
                [ text "Pause" ]
            , button
                [ class "button"
                , onClick CountDownStop
                , disabled (model.cdStatus == Stop)
                ]
                [ text "Stop" ]
            ]
        , table
            [ class "table"
            , class "is-fullwidth"
            , class "is-striped"
            ]
            [ thead []
                [ tr []
                    [ th [] [ text "Rest time" ]
                    , th [] [ text "Member" ]
                    , th [] [ text "Role" ]
                    ]
                ]
            , tbody [] (resultTable model)
            ]
        ]


resultTable : Model -> List (Html Msg)
resultTable model =
    let
        lm =
            List.length model.members

        lr =
            List.length model.roles

        row ( i, m, r ) =
            tr []
                [ td []
                    [ let
                        viewCount cy re =
                            if cy == (List.length model.members - (1 + i)) then
                                text (String.fromInt re)

                            else
                                text ""
                      in
                      case model.cdStatus of
                        Stop ->
                            text ""

                        Pause cycle rest ->
                            viewCount cycle rest

                        Count cycle rest ->
                            viewCount cycle rest
                    ]
                , td [] [ text m ]
                , td [] [ text r ]
                ]
    in
    List.map row <|
        List.indexedMap (\i ( a, b ) -> ( i, a, b )) <|
            LE.zip
                (model.members ++ List.repeat (lr - lm) "")
                (model.roles ++ List.repeat (lm - lr) "")
