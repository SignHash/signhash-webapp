module App.Hash.Types where


type Address = String

data HashSigner = HashSigner Address | NoSigner
