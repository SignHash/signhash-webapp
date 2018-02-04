module App.Routing where


import Prelude

import Control.Alt ((<|>))
import Data.Maybe (Maybe)
import Routing.Match (Match)
import Routing.Match.Class (end, lit)


data Location = Verify | Sign | Identity


type LocationChange = { new :: Location, old :: Maybe Location }


homeSlash :: Match Unit
homeSlash = lit ""


verify :: Match Location
verify =
  Verify <$ homeSlash <* end
  <|>
  Verify <$ end

sign :: Match Location
sign = Sign <$ (homeSlash *> lit "sign" <* end)

identity :: Match Location
identity = Identity <$ (homeSlash *> lit "account" <* end)


toURL :: Location -> String
toURL Verify = "/"
toURL Sign = "/sign"
toURL Identity = "/account"


routing :: Match Location
routing = identity <|> sign <|> verify
