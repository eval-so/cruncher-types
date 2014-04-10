{-# LANGUAGE OverloadedStrings, TemplateHaskell, Trustworthy #-}
-- |
-- Maintainer  : Ricky Elrod <ricky@elrod.me>
-- Stability   : stable
--
-- Handles incoming requests (usually from our Yesod frontend, but could be
-- from anywhere).

module Evalso.Cruncher.Request (Request (..)) where

import Control.Applicative
import Control.Lens
import Control.Monad (mzero)
import Data.Aeson
import Data.Map (Map)
import qualified Data.Text as T

-- | Describes an incoming request to the system. Proper use of this will
--   normally lead to some kind of a 'IO' 'SandboxResult'.
data Request = Request {
    language    :: String
  , code        :: T.Text
  , inputFiles  :: Maybe (Map String Text)
  , compileOnly :: Bool
  , stdin       :: Maybe T.Text
} deriving (Eq, Show)

makeLenses ''Request

instance FromJSON Request where
  parseJSON (Object v) = Request <$>
                             v .:  "language"
                         <*> v .:  "code"
                         <*> v .:? "inputFiles"
                         <*> v .:? "compilationOnly" .!= False
                         <*> v .:? "stdin"
  parseJSON _          = mzero
