module L.OS where

data OS = Darwin | Linux deriving (Show, Eq)

osFromMaybeString :: Maybe String -> OS
osFromMaybeString = maybe Darwin osFromString

osFromString :: String -> OS
osFromString "darwin" = Darwin
osFromString "linux"  = Linux
osFromString o        = error $ "unsupported os: " ++ o

