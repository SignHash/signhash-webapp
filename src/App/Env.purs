module App.Env where

type Env =
  { rpcUrl :: String,
    publicPath :: String
  }

foreign import env :: Env
