module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html
    exposing
        ( Html
        , a
        , button
        , div
        , h1
        , i
        , input
        , li
        , main_
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
        ( onClick
        , onInput
        )
import Http
import List.Extra as LE
import Random
import Random.List
import Route exposing (Route)
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
    }


type Page
    = NotFound
    | IllegalPage
    | TopPage
    | MemberListPage


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    Model key TopPage [] [] |> goTo (Route.parse url)



-- UPDATE


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url
    | Added Target
    | Input Target Int String
    | Deleted Target Int
    | Shuffled Url.Url Target (List String)
    | QueryRewritten Url.Url


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

        Added target ->
            case target of
                Member ->
                    ( { model | members = "" :: model.members }, Cmd.none )

                Role ->
                    ( { model | roles = "" :: model.roles }, Cmd.none )

        Input target n value ->
            case target of
                Member ->
                    ( { model | members = LE.setAt n value model.members }, Cmd.none )

                Role ->
                    ( { model | roles = LE.setAt n value model.roles }, Cmd.none )

        Deleted target n ->
            case target of
                Member ->
                    ( { model | members = LE.removeAt n model.members }, Cmd.none )

                Role ->
                    ( { model | roles = LE.removeAt n model.roles }, Cmd.none )

        Shuffled url target list ->
            case target of
                Member ->
                    ( { model | members = list }, Cmd.none )
                        |> UE.andThen update (QueryRewritten url)

                Role ->
                    ( { model | roles = list }, Cmd.none )
                        |> UE.andThen update (QueryRewritten url)

        QueryRewritten url ->
            ( model, Nav.replaceUrl model.key <| Url.toString (updateQuery model url) )


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
subscriptions _ =
    Sub.none



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
            , onClick (Added target)
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
                    , onClick (Deleted target idx)
                    ]
                    [ span [] [ text "Delete" ]
                    , span [ class "icon", class "is-small" ]
                        [ i [ class "fas", class "fa-times" ] [] ]
                    ]
                ]
            , div [ class "column" ]
                [ input [ value item, class "input", onInput (Input target idx) ] [] ]
            ]
        ]


viewMemberListPage : Model -> Html Msg
viewMemberListPage model =
    div []
        [ table
            [ class "table"
            , class "is-fullwidth"
            , class "is-striped"
            ]
            [ thead []
                [ tr []
                    [ th [] [ text "Role" ]
                    , th [] [ text "Member" ]
                    ]
                ]
            , tbody [] (resultTable model.members model.roles)
            ]
        ]


resultTable : List String -> List String -> List (Html Msg)
resultTable members roles =
    let
        lm =
            List.length members

        lr =
            List.length roles

        row ( m, r ) =
            tr []
                [ td [] [ text r ]
                , td [] [ text m ]
                ]
    in
    List.map row <|
        LE.zip
            (members ++ List.repeat (lr - lm) "")
            (roles ++ List.repeat (lm - lr) "")
