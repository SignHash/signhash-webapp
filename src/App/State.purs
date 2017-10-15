module App.State where

import App.Events.Signers as Signers
import App.Events.Files as Files
import Data.Lens (Traversal')
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))


type State =
  { file :: Maybe Files.State
  , signer :: Maybe Signers.State
  }


fileProp :: Traversal' State (Maybe Files.State)
fileProp = prop (SProxy :: SProxy "file")

signerProp :: Traversal' State (Maybe Signers.State)
signerProp = prop (SProxy :: SProxy "signer")


init :: State
init =
  { file: Nothing
  , signer: Nothing
  }
