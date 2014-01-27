{-# LANGUAGE OverloadedStrings, TemplateHaskell, Trustworthy #-}
-- |
-- Maintainer  : Ricky Elrod <ricky@elrod.me>
-- Stability   : stable
--
-- Contains data types/constructors for individual sandbox runs.
-- For example, the compile step will produce a 'SandboxResult', which is
-- defined in this module. The execution/evaluation step will also produce a
-- 'SandboxResult'.

module Evalso.Cruncher.SandboxResult (SandboxResult (..)) where

import Control.Applicative
import Control.Lens hiding ((.=))
import Control.Monad (mzero)
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Text (Text)

-- | Describes the result we get back after performing an evaluation (or
--   compilation). This is almost always wrapped in 'IO'.
data SandboxResult = SandboxResult {
    stdout      :: Text -- ^ Standard output stream
  , stderr      :: Text -- ^ Standard error stream
  , wallTime    :: Int -- ^ How long the process took
  , exitCode    :: Int -- ^ The exit code returned by the process
  , outputFiles :: Map String ByteString  -- ^ Base64-encoded output files
} deriving (Eq, Show)

makeLenses ''SandboxResult

instance ToJSON SandboxResult where
  toJSON (SandboxResult stdout' stderr' wallTime' exitCode' outputFiles') = object
    [
      "stdout"   .= stdout'
    , "stderr"   .= stderr'
    , "wallTime" .= wallTime'
    , "exitCode" .= exitCode'
    , "outputFiles" .= outputFiles'
    ]

instance FromJSON SandboxResult where
  parseJSON (Object v) = SandboxResult <$>
                             v .: "stdout"
                         <*> v .: "stderr"
                         <*> v .: "wallTime"
                         <*> v .: "exitCode"
                         <*> v .: "outputFiles"
  parseJSON _          = mzero
