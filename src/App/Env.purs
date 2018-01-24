module App.Env where

type AppEnvConfig =
  { rpcUrl :: String,
    publicPath :: String
  }

foreign import appEnvConfig :: AppEnvConfig
