module Main where

import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff (Eff)
import Control.Monad.Except (runExcept)
import Data.Array (fromFoldable, head)
import Data.Either (Either(..))
import Data.Foreign (ForeignError, renderForeignError)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..), maybe)
import Data.String (joinWith)
import Lib.Files (FileData, getFiles)
import Prelude hiding (div)
import Pux (App, CoreEffects, EffModel, noEffects, start)
import Pux.DOM.Events (DOMEvent, onChange)
import Pux.DOM.HTML (HTML)
import Pux.Renderer.React (renderToDOM)
import Text.Smolder.HTML (div, input)
import Text.Smolder.HTML.Attributes (type')
import Text.Smolder.Markup (text, (!), (#!))

data Event = NewFile FileData | FileError String | NoFile

type State = {
  filename :: Maybe String
}

type WebApp = App (DOMEvent -> Event) Event State


foldp :: Event -> State -> EffModel State Event (console :: CONSOLE)
foldp NoFile state =
  noEffects $ state { filename = Nothing }
foldp (NewFile file) state =
  noEffects $ state { filename = Just file.name }
foldp (FileError err) state =
  { state, effects: [ log err *> pure Nothing ]}


handleNewFile :: DOMEvent -> Event
handleNewFile evt = case readFiles of
  Left errors -> FileError $ renderForeignErrors errors
  Right files -> maybe NoFile NewFile $ head files
  where readFiles = runExcept $ getFiles evt


renderForeignErrors :: NonEmptyList ForeignError -> String
renderForeignErrors errors =
  joinWith "\n" $ fromFoldable $ renderForeignError <$> errors


-- | Return markup from the state
view :: State -> HTML Event
view { filename } =
  div do
    input #! onChange handleNewFile ! type' "file"
    div $ text fileDescription
  where
    fileDescription =
      maybe "Please provide a file" id filename


-- | Start and render the app
main :: String -> State -> Eff (CoreEffects ( console :: CONSOLE )) WebApp
main url state = do
  app <- start
    { initialState: state
    , view
    , foldp
    , inputs: []
    }

  renderToDOM "#app" app.markup app.input

  pure app


initialState :: State
initialState = { filename: Nothing }
