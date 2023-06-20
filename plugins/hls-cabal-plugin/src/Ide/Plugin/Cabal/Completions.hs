{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ide.Plugin.Cabal.Completions where

import           Control.Exception               (try)
import           Control.Exception.Extra         (evaluate)
import           Control.Monad                   (filterM, forM)
import           Control.Monad.IO.Class          (MonadIO)
import           Control.Monad.Trans.Maybe
import qualified Data.List                       as List
import           Data.Map                        (Map)
import qualified Data.Map                        as Map
import           Data.Maybe                      (fromMaybe)
import qualified Data.Text                       as T
import           Data.Text.Utf16.Rope            (Rope)
import qualified Data.Text.Utf16.Rope            as Rope
import           Development.IDE                 as D
import           Distribution.CabalSpecVersion   (CabalSpecVersion (CabalSpecV2_2),
                                                  showCabalSpecVersion)
import           Distribution.Compat.Lens        ((^.))
import           Ide.Plugin.Cabal.LicenseSuggest (licenseNames)
import qualified Language.LSP.Protocol.Lens      as JL
import qualified Language.LSP.Protocol.Types     as Compls (CompletionItem (..))
import qualified Language.LSP.Protocol.Types     as LSP
import qualified Language.LSP.VFS                as VFS
import           System.Directory                (doesDirectoryExist,
                                                  doesFileExist, listDirectory)
import qualified System.FilePath                 as FP
import qualified System.FilePath.Posix           as Posix
import qualified Text.Fuzzy.Parallel             as Fuzzy

data Log
  = LogFilePathCompleterIOError FilePath IOError
  | LogFileSplitError Position
  deriving (Show)

instance Pretty Log where
  pretty = \case
    LogFilePathCompleterIOError fp ioErr ->
      "Filepath:" <+> viaShow fp <+> viaShow ioErr
    LogFileSplitError pos ->  "Position: " <+> viaShow pos



{- | Takes information needed to build possible completion items
and returns the list of possible completion items
-}
type Completer = Recorder (WithPriority Log) -> CabalCompletionContext -> IO [CabalCompletionItem]

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
    -- occurs right before the current position,
    -- with no value associated to it
    KeyWord KeyWordName
  | -- | Keyword context where no keyword occurs
    -- right before the current position
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
data CabalCompletionContext = CabalCompletionContext
  { completionPrefix         :: T.Text
  -- ^ text prefix to complete
  , completionSuffix         :: Maybe T.Text
  -- ^ possible wrapping text, to write after
  --   the text has been completed
  , completionCursorPosition :: Position
  -- ^ the current position of the cursor in the file
  , completionRange          :: Range
  -- ^ range where completion is to be inserted
  , completionCabalFileDir   :: FilePath
  -- ^ filepath of the handled cabal file
  }
  deriving (Eq, Show)

-- ----------------------------------------------------------------
-- Public API for Completions
-- ----------------------------------------------------------------

{- | Takes information about the completion status within the file
  and finds the correct completer to be applied
-}
contextToCompleter :: Context -> Completer
-- if we are in the top level of the cabal file and not in a keyword context,
-- we can write any top level keywords or a stanza declaration
contextToCompleter (TopLevel, None) =
  constantCompleter $
    Map.keys (cabalVersionKeyword <> cabalKeywords) ++ Map.keys stanzaKeywordMap
-- if we are in a keyword context in the top level,
-- we look up that keyword in the top level context and can complete its possible values
contextToCompleter (TopLevel, KeyWord kw) =
  case Map.lookup kw (cabalVersionKeyword <> cabalKeywords) of
    Nothing -> noopCompleter
    Just l  -> l
-- if we are in a stanza and not in a keyword context,
-- we can write any of the stanza's keywords or a stanza declaration
contextToCompleter (Stanza s, None) =
  case Map.lookup s stanzaKeywordMap of
    Nothing -> noopCompleter
    Just l  -> constantCompleter $ Map.keys l ++ Map.keys stanzaKeywordMap
-- if we are in a stanza's keyword's context we can complete possible values of that keyword
contextToCompleter (Stanza s, KeyWord kw) =
  case Map.lookup s stanzaKeywordMap of
    Nothing -> noopCompleter
    Just m -> case Map.lookup kw m of
      Nothing -> noopCompleter
      Just l  -> l

{- | Takes information about the current cursor position,
  the handled cabal file and a set of possible keywords
  and creates completion suggestions that fit the current input
  from the given list
-}
mkCompletionItems :: [CabalCompletionItem] -> [LSP.CompletionItem]
mkCompletionItems l = map buildCompletion l

{- | Takes a position and a list of lines (representing a file)
  and returns the context of the current position
  can return Nothing if an error occurs
  TODO: first line can only have cabal-version: keyword
-}
getContext :: MonadIO m => Recorder (WithPriority Log) -> CabalCompletionContext -> Rope -> MaybeT m Context
getContext recorder ctx ls =
  case prevLinesM of
    Just prevLines -> do
      let lvlContext =
            if pos ^. JL.character == 0
              then TopLevel
              else currentLevel prevLines
      case lvlContext of
        TopLevel -> do
          kwContext <- MaybeT . pure $ getKeyWordContext ctx prevLines (cabalVersionKeyword <> cabalKeywords)
          pure (TopLevel, kwContext)
        Stanza s ->
          case Map.lookup s stanzaKeywordMap of
            Nothing -> do
              pure (Stanza s, None)
            Just m -> do
              kwContext <- MaybeT . pure $ getKeyWordContext ctx prevLines m
              pure (Stanza s, kwContext)
    Nothing -> do
      logWith recorder Warning $ LogFileSplitError pos
      -- basically returns nothing
      fail "Abort computation"
 where
  pos = completionCursorPosition ctx
  prevLinesM = splitAtPosition pos ls


-- ----------------------------------------------------------------
-- Helper Functions
-- ----------------------------------------------------------------

{- | Takes a position, a list of lines (representing a file)
  and a map of keywords and returns a keyword context if the
  previously written keyword matches one in the map
-}
getKeyWordContext :: CabalCompletionContext -> [T.Text] -> Map KeyWordName a -> Maybe KeyWordContext
getKeyWordContext ctx ls keywords = do
  case lastNonEmptyLineM of
    Nothing -> Just None
    Just lastLine' -> do
      let (whiteSpaces, lastLine) = T.span (== ' ') lastLine'
      let keywordIndentation = T.length whiteSpaces
      let cursorIndentation = fromIntegral (pos ^. JL.character) - (T.length $ completionPrefix ctx)
      -- in order to be in a keyword context the cursor needs
      -- to be indented more than the keyword
      if cursorIndentation > keywordIndentation
        then
         -- if the last thing written was a keyword without a value
        case List.find (`T.isPrefixOf` lastLine) (Map.keys keywords) of
          Nothing -> Just None
          Just kw -> Just $ KeyWord kw
        else Just None
 where
  pos = completionCursorPosition ctx
  lastNonEmptyLineM :: Maybe T.Text
  lastNonEmptyLineM = do
    (curLine,rest) <- List.uncons ls
    -- represents the current line while disregarding the
    -- currently written text we want to complete
    let cur = stripPartiallyWritten curLine
    List.find (not . T.null . T.stripEnd) $
      cur : rest

{- | Parse the given set of lines (starting before current cursor position
  up to the start of the file) to find the nearest stanza declaration,
  if none is found we are in the top level context.
-}
currentLevel :: [T.Text] -> StanzaContext
currentLevel [] = TopLevel
currentLevel (cur : xs)
  | Just s <- stanza = Stanza s
  | otherwise = currentLevel xs
 where
  stanza = List.find (`T.isPrefixOf` cur) (Map.keys stanzaKeywordMap)

{- | Returns a CabalCompletionItem with the given starting position
  and text to be inserted,
  where the displayed text is the same as the inserted text.
-}
makeSimpleCabalCompletionItem :: Range -> T.Text -> CabalCompletionItem
makeSimpleCabalCompletionItem r txt = CabalCompletionItem txt Nothing r

{- | Returns a CabalCompletionItem with the given starting position,
  text to be inserted and text to be displayed in the completion suggestion
-}
makeCabalCompletionItem :: Range -> T.Text -> T.Text -> CabalCompletionItem
makeCabalCompletionItem r insertTxt displayTxt =
  CabalCompletionItem insertTxt (Just displayTxt) r

{- | Get all lines before the given cursor position in the given file
  and reverse their order to traverse backwards starting from the current position
-}
splitAtPosition :: Position -> Rope -> Maybe [T.Text]
splitAtPosition pos ls = do
  split <- splitFile
  pure $ reverse $ Rope.lines $ fst split
    where
    splitFile = Rope.splitAtPosition ropePos ls
    ropePos =
      Rope.Position
      { Rope.posLine = fromIntegral $ pos ^. JL.line
      , Rope.posColumn = fromIntegral $ pos ^. JL.character
      }

-- | Takes a line of text and removes the last partially
-- written text to be completed
stripPartiallyWritten :: T.Text -> T.Text
stripPartiallyWritten = T.dropWhileEnd (\y -> (y /= ' ') && (y /= ':'))

{- Note [Using correct file path separators]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  Since cabal files only allow for posix style file paths
  we need to be careful to use the correct path separators
  whenever we work with file paths in cabal files.

  Thus we are using two different kinds of imports.
  We use "FP" for platform-compatible file paths with which
  we can query files independently of the platform.
  We use "Posix" for the posix syntax paths which need to
  be used for file path completions to be written to the cabal file.
-}

{- | Takes information about the current file's file path,
  the current cursor position in the file
  and its contents; and builds a CabalCompletionItem
  with the prefix up to that cursor position,
  checks whether a suffix needs to be completed,
  and calculates the range in the document in which to complete.
-}
getCabalCompletionContext :: FilePath -> VFS.PosPrefixInfo -> CabalCompletionContext
getCabalCompletionContext dir prefixInfo =
  CabalCompletionContext
    { completionPrefix = filepathPrefix
    , completionSuffix = Just suffix
    , completionCursorPosition = VFS.cursorPos prefixInfo
    , completionRange = Range completionStart completionEnd
    , completionCabalFileDir = dir
    }
 where
  completionEnd = VFS.cursorPos prefixInfo
  completionStart =
    Position
      (_line completionEnd)
      (_character completionEnd - (fromIntegral $ T.length filepathPrefix))
  (beforeCursorText, afterCursorText) = T.splitAt cursorColumn $ VFS.fullLine prefixInfo
  filepathPrefix = T.takeWhileEnd (not . (`elem` stopConditionChars)) beforeCursorText
  suffix =
    if apostropheOrSpaceSeparator == '\"' && even (T.count "\"" afterCursorText)
      then "\""
      else ""
  apostropheOrSpaceSeparator =
    if odd $ T.count "\"" beforeCursorText
      then '\"'
      else ' '
  cursorColumn = fromIntegral $ VFS.cursorPos prefixInfo ^. JL.character
  -- if the filepath is inside apostrophes, we parse until the apostrophe,
  -- otherwise we parse until a space occurs
  stopConditionChars = apostropheOrSpaceSeparator : [',', ':']

buildCompletion :: CabalCompletionItem -> LSP.CompletionItem
buildCompletion completionItem =
  LSP.CompletionItem
    { Compls._label = toDisplay
    , Compls._labelDetails = Nothing
    , Compls._kind = Just LSP.CompletionItemKind_Keyword
    , Compls._tags = Nothing
    , Compls._detail = Nothing
    , Compls._documentation = Nothing
    , Compls._deprecated = Nothing
    , Compls._preselect = Nothing
    , Compls._sortText = Nothing
    , Compls._filterText = Nothing
    , Compls._insertText = Nothing
    , Compls._insertTextFormat = Nothing
    , Compls._insertTextMode = Nothing
    , Compls._textEdit = Just $ LSP.InL (LSP.TextEdit (itemRange completionItem) $ itemInsert completionItem)
    , Compls._textEditText = Nothing
    , Compls._additionalTextEdits = Nothing
    , Compls._commitCharacters = Nothing
    , Compls._command = Nothing
    , Compls._data_ = Nothing
    }
 where
  toDisplay = fromMaybe (itemInsert completionItem) (itemDisplay completionItem)

-- ----------------------------------------------------------------
-- Completer API
-- ----------------------------------------------------------------

{- | Completer to be used when no completion suggestions
  are implemented for the field
-}
noopCompleter :: Completer
noopCompleter _ _ = pure []

{- | Completer to be used when a simple set of values
  can be completed for a field
-}
constantCompleter :: [T.Text] -> Completer
constantCompleter completions _ ctxInfo = do
  let scored = Fuzzy.simpleFilter 1000 10 (completionPrefix ctxInfo) completions
  let range = completionRange ctxInfo
  pure $ map (makeSimpleCabalCompletionItem range . Fuzzy.original) scored

{- | Completer to be used when a file path can be
  completed for a field, takes the file path of the directory to start from.
  Completes file paths as well as directories.
-}
filePathCompleter :: Completer
filePathCompleter recorder ctx = do
  let suffix = fromMaybe "" $ completionSuffix ctx
      complInfo = pathCompletionInfoFromCompletionContext ctx
      toMatch = fromMaybe (partialFileName complInfo) $ T.stripPrefix "./" $ partialFileName complInfo
  filePathCompletions <- listFileCompletions recorder complInfo
  let scored = Fuzzy.simpleFilter 1000 10 toMatch (map T.pack filePathCompletions)
  forM
    scored
    ( \compl' -> do
        let compl = Fuzzy.original compl'
        fullFilePath <- makeFullFilePath suffix compl complInfo
        pure $ makeCabalCompletionItem (completionRange ctx) fullFilePath fullFilePath
    )
 where
  --   Takes a suffix, a completed path and a pathCompletionInfo and
  --   generates the whole filepath including the already written prefix
  --   and the suffix in case the completed path is a filepath
  makeFullFilePath :: T.Text -> T.Text -> PathCompletionInfo -> IO T.Text
  makeFullFilePath suffix' completion' complInfo = do
    let fullPath' = partialFileDir complInfo Posix.</> T.unpack completion'
    isFilePath <- doesFileExist fullPath'
    let fullPath = if isFilePath then fullPath' ++ T.unpack suffix' else fullPath'
    pure $ T.pack fullPath

{- | Completer to be used when a directory can be completed for the field,
  takes the file path of the directory to start from.
  Only completes directories.
-}
directoryCompleter :: Completer
directoryCompleter recorder ctx = do
  let complInfo = pathCompletionInfoFromCompletionContext ctx
  directoryCompletions <- listDirectoryCompletions recorder complInfo
  let scored = Fuzzy.simpleFilter 1000 10 (partialFileName complInfo) (map T.pack directoryCompletions)
  forM
    scored
    ( \compl' -> do
        let compl = Fuzzy.original compl'
        fullDirPath <- makeFullDirPath compl complInfo
        pure $ makeCabalCompletionItem (completionRange ctx) fullDirPath fullDirPath
    )
 where
  --   Takes a directory and PathCompletionInfo and
  --   returns the whole path including the prefix that was already written
  makeFullDirPath :: T.Text -> PathCompletionInfo -> IO T.Text
  makeFullDirPath completion' complInfo = do
    let fullPath = partialFileDir complInfo Posix.</> T.unpack completion'
    pure $ T.pack fullPath

{- | Takes a path completion info and returns the list of files
  in the directory that match the path completion info.
-}
listFileCompletions :: Recorder (WithPriority Log) -> PathCompletionInfo -> IO [FilePath]
listFileCompletions recorder complInfo = do
  let complDir = mkCompletionDirectory complInfo
  try (evaluate =<< listDirectory complDir) >>= \case
    Right dirs -> do
      fixedDirs <-
        mapM
          ( \d -> do
              isDir <- doesDirectoryExist $ mkDirFromCWD complInfo d
              pure $ if isDir then Posix.addTrailingPathSeparator d else d
          )
          dirs
      pure fixedDirs
    Left (err :: IOError) -> do
      logWith recorder Warning $ LogFilePathCompleterIOError complDir err
      pure []

{- | Returns a list of all (and only) directories in the
  directory described by path completion info
-}
listDirectoryCompletions :: Recorder (WithPriority Log) -> PathCompletionInfo -> IO [FilePath]
listDirectoryCompletions recorder complInfo = do
  filepaths <- listFileCompletions recorder complInfo
  filterM (doesDirectoryExist . mkDirFromCWD complInfo) filepaths

pathCompletionInfoFromCompletionContext :: CabalCompletionContext -> PathCompletionInfo
pathCompletionInfoFromCompletionContext ctx =
  PathCompletionInfo
    { partialFileName = dirNamePrefix
    , partialFileDir = Posix.addTrailingPathSeparator $ Posix.takeDirectory prefix
    , cabalFileDir = dir
    }
 where
  prefix = T.unpack $ completionPrefix ctx
  dirNamePrefix = T.pack $ Posix.takeFileName prefix
  dir = Posix.takeDirectory $ completionCabalFileDir ctx

{- | Returns the directory, the currently handled cabal file is in.

  We let System.FilePath handle the separator syntax since this is used
  to query filepaths from the system. See Note [Using correct file path separators].
-}
mkCompletionDirectory :: PathCompletionInfo -> FilePath
mkCompletionDirectory complInfo =
  FP.addTrailingPathSeparator $
    cabalFileDir complInfo FP.</> (FP.normalise $ partialFileDir complInfo)

{- | Returns the complete filepath for the given filepath.

  Since this is used for completions we use posix separators here.
  See Note [Using correct file path separators].
-}
mkDirFromCWD :: PathCompletionInfo -> FilePath -> FilePath
mkDirFromCWD complInfo fp =
  Posix.addTrailingPathSeparator $
    mkCompletionDirectory complInfo Posix.</> Posix.normalise fp

{- | Information used to query and build file path/directory completions.

  Note that partialFileName combined with partialFileDir results in
  the original prefix.

  Example:
  On the written filepath: @dir1/fi@ the
  resulting PathCompletionInfo would be:

  @
    partialFileName = "fi"
    partialFileDir  = "dir1/dir2/fi"
    ...
  @
-}
data PathCompletionInfo = PathCompletionInfo
  { partialFileName :: T.Text
  -- ^ partly written start of next part of path
  , partialFileDir  :: FilePath
  -- ^ written part of path
  , cabalFileDir    :: FilePath
  -- ^ current working directory of the handled file
  }
  deriving (Eq, Show, Read)

-- ----------------------------------------------------------------
-- Completion Data
-- ----------------------------------------------------------------

-- | Keyword for cabal version; required to be the top line in a cabal file
cabalVersionKeyword :: Map KeyWordName Completer
cabalVersionKeyword =
  Map.singleton "cabal-version:" $
    constantCompleter $
      map (T.pack . showCabalSpecVersion) [CabalSpecV2_2 .. maxBound]

{- | Top level keywords of a cabal file.

 TODO: we could add descriptions of field values and then show them when inside the field's context
-}
cabalKeywords :: Map KeyWordName Completer
cabalKeywords =
  Map.fromList
    [ ("name:", noopCompleter) -- TODO: should complete to filename, needs meta info
    , ("version:", noopCompleter)
    , ("build-type:", constantCompleter ["Simple", "Custom", "Configure", "Make"])
    , ("license:", constantCompleter licenseNames)
    , ("license-file:", filePathCompleter)
    , ("license-files:", filePathCompleter) -- list of filenames
    , ("copyright:", noopCompleter)
    , ("author:", noopCompleter)
    , ("maintainer:", noopCompleter) -- email address, use git config?
    , ("stability:", noopCompleter)
    , ("homepage:", noopCompleter)
    , ("bug-reports:", noopCompleter)
    , ("package-url:", noopCompleter)
    , ("synopsis:", noopCompleter)
    , ("description:", noopCompleter)
    , ("category:", noopCompleter)
    , ("tested-with:", constantCompleter ["GHC"]) -- list of compilers, i.e. "GHC == 8.6.3, GHC == 8.4.4"
    , ("data-files:", filePathCompleter) -- list of filenames
    , ("data-dir:", directoryCompleter) -- directory
    , ("extra-source-files:", filePathCompleter) -- filename list
    , ("extra-doc-files:", filePathCompleter) -- filename list
    , ("extra-tmp-files:", filePathCompleter) -- filename list
    ]

-- | Map, containing all stanzas in a cabal file as keys and lists of their possible nested keywords as values
stanzaKeywordMap :: Map StanzaName (Map KeyWordName Completer)
stanzaKeywordMap =
  Map.fromList
    [
      ( "library"
      , Map.fromList $
          [ ("exposed-modules:", noopCompleter) -- identifier list
          , ("virtual-modules:", noopCompleter)
          , ("exposed:", constantCompleter ["True", "False"])
          , ("visibility:", constantCompleter ["private", "public"])
          , ("reexported-modules:", noopCompleter) -- export list, i.e. "orig-okg:Name as NewName"
          , ("signatures:", noopCompleter) -- list of signatures
          ]
            ++ libExecTestBenchCommons
      )
    ,
      ( "executable"
      , Map.fromList $
          [ ("main-is:", filePathCompleter)
          , ("scope:", constantCompleter ["public", "private"])
          ]
            ++ libExecTestBenchCommons
      )
    ,
      ( "test-suite"
      , Map.fromList $
          [ ("type:", constantCompleter ["exitcode-stdio-1.0", "detailed-0.9"])
          , ("main-is:", filePathCompleter)
          ]
            ++ libExecTestBenchCommons
      )
    ,
      ( "benchmark"
      , Map.fromList $
          [ ("type:", noopCompleter)
          , ("main-is:", filePathCompleter)
          ]
            ++ libExecTestBenchCommons
      )
    ,
      ( "foreign-library"
      , Map.fromList
          [ ("type:", constantCompleter ["native-static", "native-shared"])
          , ("options:", constantCompleter ["standalone"])
          , ("mod-def-file:", filePathCompleter)
          , ("lib-version-info:", noopCompleter)
          , ("lib-version-linux:", noopCompleter)
          ]
      )
    ,
      ( "flag"
      , Map.fromList
          [ ("description:", noopCompleter)
          , ("default:", constantCompleter ["True", "False"])
          , ("manual:", constantCompleter ["False", "True"])
          , ("lib-def-file:", noopCompleter)
          , ("lib-version-info:", noopCompleter)
          , ("lib-version-linux:", noopCompleter)
          ]
      )
    ,
      ( "source-repository"
      , Map.fromList $
          [
            ( "type:"
            , constantCompleter
                [ "darcs"
                , "git"
                , "svn"
                , "cvs"
                , "mercurial"
                , "hg"
                , "bazaar"
                , "bzr"
                , "arch"
                , "monotone"
                ]
            )
          , ("location:", noopCompleter)
          , ("module:", noopCompleter)
          , ("branch:", noopCompleter)
          , ("tag:", noopCompleter)
          , ("subdir:", directoryCompleter)
          ]
      )
    ]
 where
  libExecTestBenchCommons =
    [ ("build-depends:", noopCompleter)
    , ("other-modules:", noopCompleter)
    , ("hs-source-dirs:", directoryCompleter)
    , ("default-extensions:", noopCompleter)
    , ("other-extensions:", noopCompleter)
    , ("default-language:", constantCompleter ["GHC2021", "Haskell2010", "Haskell98"])
    , ("other-languages:", noopCompleter)
    , ("build-tool-depends:", noopCompleter)
    , ("buildable:", constantCompleter ["True", "False"])
    , ("ghc-options:", noopCompleter) -- todo: maybe there is a list of possible ghc options somewhere
    , ("ghc-prof-options:", noopCompleter)
    , ("ghc-shared-options:", noopCompleter)
    , ("ghcjs-options:", noopCompleter)
    , ("ghcjs-prof-options:", noopCompleter)
    , ("ghcjs-shared-options:", noopCompleter)
    , ("includes:", filePathCompleter) -- list of filenames
    , ("install-includes:", filePathCompleter) -- list of filenames
    , ("include-dirs:", directoryCompleter) -- list of directories
    , ("c-sources:", filePathCompleter) -- list of filenames
    , ("cxx-sources:", filePathCompleter) -- list of filenames
    , ("asm-sources:", filePathCompleter) -- list of filenames
    , ("cmm-sources:", filePathCompleter) -- list of filenames
    , ("js-sources:", filePathCompleter) -- list of filenames
    , ("extra-libraries:", noopCompleter)
    , ("extra-ghci-libraries:", noopCompleter)
    , ("extra-bundled-libraries:", noopCompleter)
    , ("extra-lib-dirs:", directoryCompleter) -- list of directories
    , ("cc-options:", noopCompleter)
    , ("cpp-options:", noopCompleter)
    , ("cxx-options:", noopCompleter)
    , ("cmm-options:", noopCompleter)
    , ("asm-options:", noopCompleter)
    , ("ld-options:", noopCompleter)
    , ("pkgconfig-depends:", noopCompleter)
    , ("frameworks:", noopCompleter)
    , ("extra-framework-dirs:", directoryCompleter) -- list of directories
    , ("mixins:", noopCompleter)
    ]

-- cabalFlagKeywords :: [(T.Text, T.Text)]
-- cabalFlagKeywords =
--   [
--     ("flag", "name"),
--     ("description:", "freeform"),
--     ("default:", "boolean"),
--     ("manual:", "boolean")
--   ]

-- cabalStanzaKeywords :: [(T.Text, T.Text)]
-- cabalStanzaKeywords =
--   [
--     ("common", "name"),
--     ("import:", "token-list")
--   ]
