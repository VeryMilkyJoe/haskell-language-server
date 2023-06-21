{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ide.Plugin.Cabal.Types where

import qualified Data.Text       as T
import           Development.IDE as D

data Log
  = LogFileSplitError Position
  | LogUnknownKeyWordInContextError KeyWordName
  | LogUnknownStanzaNameInContextError StanzaName
  | LogFilePathCompleterIOError FilePath IOError
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

{- | Takes information needed to build possible completion items
and returns the list of possible completion items
-}
type Completer = Recorder (WithPriority Log) -> CabalPrefixInfo -> IO [CabalCompletionItem]

-- | Contains information needed for a completion action
data CabalCompletionItem = CabalCompletionItem
  { itemInsert  :: T.Text
  -- ^ actual text to be written into the document
  , itemDisplay :: Maybe T.Text
  -- ^ text displayed when completion options are shown
  , itemRange   :: Range
  -- ^ range where completion is to be inserted
  }
  deriving (Eq, Show)

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
    Stanza StanzaName
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
  }
  deriving (Eq, Show)
