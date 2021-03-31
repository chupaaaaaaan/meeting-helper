module Route exposing (..)

import Url
import Url.Parser
    exposing
        ( (</>)
        , (<?>)
        , Parser
        , map
        , oneOf
        , s
        , top
        )
import Url.Parser.Query as Q


type Route
    = Top (Maybe String) (Maybe String)
    | Reset
    | UpdateQuery
    | MemberList (Maybe String) (Maybe String)


parse : Url.Url -> Maybe Route
parse url =
    Url.Parser.parse parser url


parser : Parser (Route -> a) a
parser =
    oneOf
        [ map Top <| s "top" </> top <?> Q.string "members" <?> Q.string "roles"
        , map Reset <| s "reset"
        , map UpdateQuery <| s "updatequery"
        , map MemberList <| s "result" </> top <?> Q.string "members" <?> Q.string "roles"
        ]
