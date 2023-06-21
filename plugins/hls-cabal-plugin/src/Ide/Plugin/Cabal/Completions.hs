{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ide.Plugin.Cabal.Completions where

import           Control.Monad                        (forM)
import           Control.Monad.IO.Class               (MonadIO)
import           Control.Monad.Trans.Maybe
import qualified Data.List                            as List
import           Data.Map                             (Map)
import qualified Data.Map                             as Map
import           Data.Maybe                           (fromMaybe)
import qualified Data.Text                            as T
import           Data.Text.Utf16.Rope                 (Rope)
import qualified Data.Text.Utf16.Rope                 as Rope
import           Development.IDE                      as D
import           Distribution.CabalSpecVersion        (CabalSpecVersion (CabalSpecV2_2),
                                                       showCabalSpecVersion)
import           Distribution.Compat.Lens             ((^.))
import           Ide.Plugin.Cabal.FilepathCompletions
import           Ide.Plugin.Cabal.LicenseSuggest      (licenseNames)
import           Ide.Plugin.Cabal.Types
import qualified Language.LSP.Protocol.Lens           as JL
import qualified Language.LSP.Protocol.Types          as Compls (CompletionItem (..))
import qualified Language.LSP.Protocol.Types          as LSP
import qualified Language.LSP.VFS                     as VFS
import qualified Text.Fuzzy.Parallel                  as Fuzzy

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
    Nothing -> \recorder b -> do
      logWith recorder Warning $ LogUnknownKeyWordInContextError kw
      noopCompleter recorder b
    Just l -> l
-- if we are in a stanza and not in a keyword context,
-- we can write any of the stanza's keywords or a stanza declaration
contextToCompleter (Stanza s, None) =
  case Map.lookup s stanzaKeywordMap of
    Nothing -> \recorder b -> do
      logWith recorder Warning $ LogUnknownStanzaNameInContextError s
      noopCompleter recorder b
    Just l -> constantCompleter $ Map.keys l ++ Map.keys stanzaKeywordMap
-- if we are in a stanza's keyword's context we can complete possible values of that keyword
contextToCompleter (Stanza s, KeyWord kw) =
  case Map.lookup s stanzaKeywordMap of
    Nothing -> \recorder b -> do
      logWith recorder Warning $ LogUnknownStanzaNameInContextError s
      noopCompleter recorder b
    Just m -> case Map.lookup kw m of
      Nothing -> \recorder b -> do
        logWith recorder Warning $ LogUnknownKeyWordInContextError kw
        noopCompleter recorder b
      Just l -> l

{- | Takes prefix info about the previously written text
  and a rope (representing a file), returns the corresponding context.

  Can return Nothing if an error occurs.
  TODO: first line can only have cabal-version: keyword
-}
getContext :: (MonadIO m) => Recorder (WithPriority Log) -> CabalPrefixInfo -> Rope -> MaybeT m Context
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

{- | Takes prefix info about the previously written text,
  a list of lines (representing a file) and a map of
  keywords and returns a keyword context if the
  previously written keyword matches one in the map.
-}
getKeyWordContext :: CabalPrefixInfo -> [T.Text] -> Map KeyWordName a -> Maybe KeyWordContext
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
        then -- if the last thing written was a keyword without a value
        case List.find (`T.isPrefixOf` lastLine) (Map.keys keywords) of
          Nothing -> Just None
          Just kw -> Just $ KeyWord kw
        else Just None
 where
  pos = completionCursorPosition ctx
  lastNonEmptyLineM :: Maybe T.Text
  lastNonEmptyLineM = do
    (curLine, rest) <- List.uncons ls
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
  and text to be inserted, where the displayed text is the same as the
  inserted text.
-}
makeSimpleCabalCompletionItem :: Range -> T.Text -> CabalCompletionItem
makeSimpleCabalCompletionItem r txt = CabalCompletionItem txt Nothing r

{- | Returns a CabalCompletionItem with the given starting position,
  text to be inserted and text to be displayed in the completion suggestion.
-}
mkCabalCompletionItem :: Range -> T.Text -> T.Text -> CabalCompletionItem
mkCabalCompletionItem r insertTxt displayTxt =
  CabalCompletionItem insertTxt (Just displayTxt) r

{- | Get all lines before the given cursor position in the given file
  and reverse their order to traverse backwards starting from the given position.
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

{- | Takes a line of text and removes the last partially
written word to be completed
-}
stripPartiallyWritten :: T.Text -> T.Text
stripPartiallyWritten = T.dropWhileEnd (\y -> (y /= ' ') && (y /= ':'))

{- | Takes information about the current file's file path,
  the current cursor position in the file
  and its contents; and builds a CabalCompletionItem
  with the prefix up to that cursor position,
  checks whether a suffix needs to be completed,
  and calculates the range in the document in which to complete.
-}
getCabalPrefixInfo :: FilePath -> VFS.PosPrefixInfo -> CabalPrefixInfo
getCabalPrefixInfo dir prefixInfo =
  CabalPrefixInfo
    { completionPrefix = filepathPrefix
    , completionSuffix = Just suffix
    , completionCursorPosition = VFS.cursorPos prefixInfo
    , completionRange = Range completionStart completionEnd
    , completionWorkingDir = dir
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

mkCompletionItem :: CabalCompletionItem -> LSP.CompletionItem
mkCompletionItem completionItem =
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
        fullFilePath <- mkFilePathCompletion suffix compl complInfo
        pure $ mkCabalCompletionItem (completionRange ctx) fullFilePath fullFilePath
    )

{- | Completer to be used when a directory can be completed for the field,
  takes the file path of the directory to start from.
  Only completes directories.
-}
directoryCompleter :: Completer
directoryCompleter recorder ctx = do
  let complInfo = pathCompletionInfoFromCompletionContext ctx
  directoryCompletions <- listDirectoryCompletions recorder complInfo
  let scored =
        Fuzzy.simpleFilter
          1000
          10
          (partialFileName complInfo)
          (map T.pack directoryCompletions)
  forM
    scored
    ( \compl' -> do
        let compl = Fuzzy.original compl'
        let fullDirPath = mkPathCompletion complInfo compl
        pure $ mkCabalCompletionItem (completionRange ctx) fullDirPath fullDirPath
    )

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
