module L.OS where

import System.Info as Sys

data OS = Darwin | Linux deriving (Show, Eq)

osFromMaybeString :: Maybe String -> OS
osFromMaybeString = maybe (osFromString $ Sys.os) osFromString

osFromString :: String -> OS
osFromString "darwin" = Darwin
osFromString "linux"  = Linux
osFromString o        = error $ "unsupported os: " ++ o

systemOS :: OS
systemOS = osFromString Sys.os