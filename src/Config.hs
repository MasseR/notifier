{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
module Config
  ( Config(..)
  , loadConfig
  ) where

import           Dhall
import           GHC.Generics (Generic)
import System.FilePath
import System.Directory
import Data.Text (pack)
import Control.Exception

newtype Config = Config { feeds :: [String] } deriving Generic

instance Interpret Config

loadConfig :: IO Config
loadConfig = catch @IOException go (const (pure (Config [])))
  where
    go :: IO Config
    go = do
      path <- (</> "config.dhall") <$> getXdgDirectory XdgConfig "notifier"
      input auto (pack path)
