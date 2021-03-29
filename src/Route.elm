module Route exposing (..)

import Url exposing (..)
import Url.Parser exposing (..)
import Url.Parser.Query as Q


type Route
    = Top (Maybe String) (Maybe String)
    | Reset
    | MemberList (Maybe String) (Maybe String)


parse : Url -> Maybe Route
parse url =
    Url.Parser.parse parser url


parser : Parser (Route -> a) a
parser =
    oneOf
        [ map Top <| s "top" </> top <?> Q.string "members" <?> Q.string "roles"
        , map Reset <| s "reset"
        , map MemberList <| s "result" </> top <?> Q.string "members" <?> Q.string "roles"
        ]
