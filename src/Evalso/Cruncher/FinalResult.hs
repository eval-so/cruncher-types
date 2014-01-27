{-# LANGUAGE OverloadedStrings, TemplateHaskell, Trustworthy #-}
-- |
-- Maintainer  : Ricky Elrod <ricky@elrod.me>
-- Stability   : stable
--
-- The highest level of a response that Cruncher deals with. Contains only the
-- final result of a sandbox run, including compilation and execution.

module Evalso.Cruncher.FinalResult (FinalResult (..)) where

import Evalso.Cruncher.SandboxResult

import Control.Lens hiding ((.=))
import Data.Aeson

-- | The final result for a given request.
--
--   This data type also handles error handling, in the form of types.
data FinalResult
  = FinalResult
    {
      compile     :: Maybe SandboxResult -- ^ The compilation result, if any
    , run         :: Maybe SandboxResult -- ^ The execution result, if any
    }
  | NoSuchLanguage
  | SELinuxNotEnforcing
  deriving (Eq, Show)

makeLenses ''FinalResult

instance ToJSON FinalResult where
  toJSON (FinalResult compile' run') = object
    [
      "compile"     .= compile'
    , "run"         .= run'
    ]

  -- | TODO: i18n
  toJSON (NoSuchLanguage) = object ["error" .= noSuchLanguageError]
    where
      noSuchLanguageError :: String
      noSuchLanguageError = "We do not currently support that language"

  -- | TODO: i18n
  toJSON (SELinuxNotEnforcing) = object ["error" .= selinuxError]
    where
      selinuxError :: String
      selinuxError = "Internal security error - halting request"
