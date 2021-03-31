module Query exposing
    ( Query(..)
    , updateQuery
    )

import Url
import Url.Builder as B


type Query
    = StringValueQuery String (Maybe String)
    | StringListQuery String (List String)
    | IntValueQuery String (Maybe Int)
    | IntListQuery String (List Int)


updateQuery : Url.Url -> List Query -> Url.Url
updateQuery url queries =
    { url
        | query =
            case List.filterMap toQueryParameter queries of
                [] ->
                    Nothing

                q :: qs ->
                    Just <| correctQuery <| B.toQuery <| (q :: qs)
    }


correctQuery : String -> String
correctQuery query =
    case String.uncons query of
        Just ( '?', query_ ) ->
            query_

        _ ->
            query



-- Generate QueryParameter


toQueryParameter : Query -> Maybe B.QueryParameter
toQueryParameter query =
    case query of
        StringValueQuery key maybeVal ->
            case maybeVal of
                Nothing ->
                    Nothing

                Just val ->
                    Just <| B.string key val

        StringListQuery key listVal ->
            case listVal of
                [] ->
                    Nothing

                v :: vs ->
                    Just <| B.string key <| stringListToQueryValue (v :: vs)

        IntValueQuery key maybeVal ->
            case maybeVal of
                Nothing ->
                    Nothing

                Just val ->
                    Just <| B.int key val

        IntListQuery key listVal ->
            case listVal of
                [] ->
                    Nothing

                v :: vs ->
                    Just <| B.string key <| intListToQueryValue (v :: vs)


stringListToQueryValue : List String -> String
stringListToQueryValue list =
    list
        |> List.filter (not << String.isEmpty)
        |> String.join ","


intListToQueryValue : List Int -> String
intListToQueryValue list =
    list
        |> List.map String.fromInt
        |> String.join ","
