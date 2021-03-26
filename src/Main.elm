module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
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
    | Add Target
    | Input Target Int String
    | Delete Target Int
    | Shuffled Url.Url Target (List String)
    | RewriteQuery Url.Url


type Target
    = Member
    | Role


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Cmd.none )
                        |> UE.addCmd (Random.generate (Shuffled url Member) (Random.List.shuffle model.members))
                        |> UE.addCmd (Random.generate (Shuffled url Role) (Random.List.shuffle model.roles))
                        |> UE.addCmd (Nav.pushUrl model.key (updateQuery model url))

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            goTo (Route.parse url) model

        Add target ->
            case target of
                Member ->
                    ( { model | members = "" :: model.members }, Cmd.none )

                Role ->
                    ( { model | roles = "" :: model.roles }, Cmd.none )

        Input target n value ->
            case target of
                Member ->
                    ( { model | members = updateNth n value model.members }, Cmd.none )

                Role ->
                    ( { model | roles = updateNth n value model.roles }, Cmd.none )

        Delete target n ->
            case target of
                Member ->
                    ( { model | members = deleteNth n model.members }, Cmd.none )

                Role ->
                    ( { model | roles = deleteNth n model.roles }, Cmd.none )

        Shuffled url target list ->
            case target of
                Member ->
                    ( { model | members = list }, Cmd.none )
                        |> UE.andThen update (RewriteQuery url)

                Role ->
                    ( { model | roles = list }, Cmd.none )
                        |> UE.andThen update (RewriteQuery url)

        RewriteQuery url ->
            ( model, Nav.replaceUrl model.key (updateQuery model url) )


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


updateNth : Int -> String -> List String -> List String
updateNth n value list =
    List.take n list ++ (value :: List.drop (n + 1) list)


deleteNth : Int -> List String -> List String
deleteNth n list =
    List.take n list ++ List.drop (n + 1) list


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


correctQuery : String -> String
correctQuery query =
    case String.uncons query of
        Just ( '?', query_ ) ->
            query_

        _ ->
            query


updateQuery : Model -> Url.Url -> String
updateQuery model url =
    Url.toString
        { url
            | query =
                Just <|
                    correctQuery <|
                        B.toQuery
                            [ B.string "members" <| listToQueryValue model.members
                            , B.string "roles" <| listToQueryValue model.roles
                            ]
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Meeting Preparator"
    , body =
        [ a [ href "/top" ] [ h1 [] [ text "Meeting Preparator" ] ]
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
    div []
        [ div []
            [ button [ onClick (Add Member) ]
                [ text "Add member" ]
            , ul []
                (List.map
                    (viewInputItem (Input Member) (Delete Member))
                 <|
                    List.indexedMap Tuple.pair model.members
                )
            ]
        , div []
            [ button [ onClick (Add Role) ]
                [ text "Add role" ]
            , ul []
                (List.map
                    (viewInputItem (Input Role) (Delete Role))
                 <|
                    List.indexedMap Tuple.pair model.roles
                )
            ]
        , div []
            [ a [ href <| B.absolute [ "result" ] [] ]
                [ text "Generate!" ]
            ]
        ]


viewMemberListPage : Model -> Html Msg
viewMemberListPage model =
    div []
        [ table []
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
    case ( members, roles ) of
        ( [], [] ) ->
            []

        ( m :: ms, [] ) ->
            tr []
                [ td [] [ text "" ]
                , td [] [ text m ]
                ]
                :: resultTable ms []

        ( [], r :: rs ) ->
            tr []
                [ td [] [ text r ]
                , td [] [ text "" ]
                ]
                :: resultTable [] rs

        ( m :: ms, r :: rs ) ->
            tr []
                [ td [] [ text r ]
                , td [] [ text m ]
                ]
                :: resultTable ms rs


viewInputItem : (Int -> String -> Msg) -> (Int -> Msg) -> ( Int, String ) -> Html Msg
viewInputItem updateMsg deleteMsg ( idx, item ) =
    li []
        [ button [ onClick (deleteMsg idx) ] [ text "Del" ]
        , input [ value item, onInput (updateMsg idx) ] []
        ]
