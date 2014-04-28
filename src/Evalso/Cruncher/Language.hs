{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}
-- |
-- Maintainer  : Ricky Elrod <ricky@elrod.me>
-- Stability   : experimental

module Evalso.Cruncher.Language where

import Control.Lens hiding ((.=))
import Data.Aeson
import qualified Data.Text as T

-- | Describes what a programming language looks like internally.
data Language = Language {
    _codeFilename :: String -- ^ What to store the incoming file as.
  , _compileCommand :: Maybe [T.Text] -- ^ What to compile with.
  , _compileTimeout :: Maybe Int -- ^ How many seconds to give the compilation before timeout.
  , _runCommand :: [T.Text] -- ^ What to run with.
  , _runTimeout :: Int -- ^ How many seconds to give the compilation before timeout.
  , _codemirror :: String -- ^ How does this get highlighted in CodeMirror?
  , _rpm :: String -- ^ Which RPM provides this? Used for summary/version only.
  , _displayName :: String -- ^ How should this language be displayed in UIs?
} deriving (Eq, Show)

makeLenses ''Language

instance ToJSON Language where
  toJSON (Language codeFilename' compileCommand' compileTimeout' runCommand' runTimeout' codemirror' rpm' displayName') = object
    [
      "codeFilename"   .= codeFilename'
    , "compileCommand" .= compileCommand'
    , "compileTimeout" .= compileTimeout'
    , "runCommand"     .= runCommand'
    , "runTimeout"     .= runTimeout'
    , "codemirror"     .= codemirror'
    , "rpm"            .= rpm'
    , "displayName"    .= displayName'
    ]
