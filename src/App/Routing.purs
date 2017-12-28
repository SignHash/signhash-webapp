module App.Routing where


import Prelude

import Control.Alt ((<|>))
import Data.Maybe (Maybe)
import Routing.Match (Match)
import Routing.Match.Class (end, lit)


data Location = Verify | Sign


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


routing :: Match Location
routing = sign <|> verify
