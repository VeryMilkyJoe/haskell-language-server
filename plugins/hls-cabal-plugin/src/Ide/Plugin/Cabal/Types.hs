{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Ide.Plugin.Cabal.Types where

import           Control.DeepSeq        (NFData)
import           Data.Hashable
import qualified Data.Text              as T
import           Data.Typeable
import           Development.IDE        as D
import           GHC.Generics
import qualified Ide.Plugin.Cabal.Parse as Parse
import Language.LSP.Protocol.Types (CompletionItem)

data Log
  = LogFileSplitError Position
  | LogUnknownKeyWordInContextError KeyWordName
  | LogUnknownStanzaNameInContextError StanzaName
  | LogFilePathCompleterIOError FilePath IOError
  | LogUseWithStaleFastNoResult
  deriving (Show)

instance Pretty Log where
  pretty = \case
    LogFileSplitError pos -> "Position:" <+> viaShow pos
    LogUnknownKeyWordInContextError kw ->
      "Lookup failed for:" <+> viaShow kw
    LogUnknownStanzaNameInContextError sn ->
      "Lookup failed for:" <+> viaShow sn
    LogFilePathCompleterIOError fp ioErr ->
      "Filepath:" <+> viaShow fp <+> viaShow ioErr
    LogUseWithStaleFastNoResult -> "Package description couldn't be read"

data ParseCabal = ParseCabal
  deriving (Eq, Show, Typeable, Generic)
instance Hashable ParseCabal
instance NFData ParseCabal

type instance RuleResult ParseCabal = Parse.GenericPackageDescription


{- | Takes information needed to build possible completion items
and returns the list of possible completion items
-}
type Completer = Recorder (WithPriority Log) -> CompleterData -> IO [CompletionItem]

data CompleterData = CompleterData
  { ideState        :: IdeState
  , cabalPrefixInfo :: CabalPrefixInfo
  , stanzaName      :: Maybe StanzaName
  }

{- | The context a cursor can be in within a cabal file,
  we can be in stanzas or the top level,
  and additionally we can be in a context where we have already
  written a keyword but no value for it yet
-}
type Context = (StanzaContext, KeyWordContext)

-- | Context inside a cabal file, used to decide which keywords to suggest
data StanzaContext
  = -- | Top level context in a cabal file such as 'author'
    TopLevel
  | -- | Nested context in a cabal file, such as 'library',
    -- which has nested keywords, specific to the stanza
    Stanza StanzaType (Maybe StanzaName)
  deriving (Eq, Show, Read)

{- | Keyword context in cabal file
  used to decide whether to suggest values or keywords
-}
data KeyWordContext
  = -- | Key word context, where a keyword
    -- occurs right before the current word
    -- to be completed
    KeyWord KeyWordName
  | -- | Keyword context where no keyword occurs
    -- right before the current word to be completed
    None
  deriving (Eq, Show, Read)

type KeyWordName = T.Text
type StanzaName = T.Text
type StanzaType = T.Text

{- | Information about the current completion status

  Example: @"dir1/fi@ having been written to the file
  would correspond to:

  @
    completionPrefix = "dir1/fi"
    completionSuffix  = Just "\\""
    ...
  @

  We define this type instead of simply using
  VFS.PosPrefixInfo since e.g. for filepaths we
  need more than just the word before the
  cursor (as can be seen above),
  since we want to capture the whole filepath
  before the cursor.
  We also use this type to wrap all information
  necessary to complete filepaths and other values
  in a cabal file.
-}
data CabalPrefixInfo = CabalPrefixInfo
  { completionPrefix         :: T.Text
  -- ^ text prefix to complete
  , completionSuffix         :: Maybe T.Text
  -- ^ possible wrapping text, to write after
  --   the text has been completed
  , completionCursorPosition :: Position
  -- ^ the current position of the cursor in the file
  , completionRange          :: Range
  -- ^ range where completion is to be inserted
  , completionWorkingDir     :: FilePath
  -- ^ filepath of the handled cabal file
  , normalizedCabalFilePath  :: NormalizedFilePath
  }
  deriving (Eq, Show)

-- data PossibleCompletion = PossibleCompletion
--   { completionTrigger          :: T.Text
--   , completionKeyWordCompleter :: Completer
--   , completionKeyword          :: KeyWordName
--   , completionValueDescription :: T.Text
--   , completionValueCompleter   :: Completer
--   }
