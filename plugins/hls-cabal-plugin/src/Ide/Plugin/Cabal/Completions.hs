{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use fewer imports" #-}

module Ide.Plugin.Cabal.Completions where

import           Control.Applicative                  (asum)
import           Control.Monad                        (forM)
import           Control.Monad.IO.Class               (MonadIO)
import           Control.Monad.Trans.Maybe
import           Data.Function                        ((&))
import qualified Data.List                            as List
import           Data.Map                             (Map)
import qualified Data.Map                             as Map
import           Data.Maybe                           (fromMaybe)
import           Data.Ord                             (Down (..))
import qualified Data.Text                            as T
import           Data.Text.Utf16.Rope                 (Rope)
import qualified Data.Text.Utf16.Rope                 as Rope
import           Development.IDE                      as D
import           Distribution.CabalSpecVersion        (CabalSpecVersion (CabalSpecV2_2),
                                                       showCabalSpecVersion)
import           Distribution.Compat.Lens             ((^.))
import           Distribution.PackageDescription      (GenericPackageDescription (..),
                                                       Library (libBuildInfo),
                                                       hsSourceDirs)
import           Distribution.Types.CondTree          (CondTree (condTreeData))
import           Distribution.Utils.Path              (getSymbolicPath)
import           Distribution.Compat.Lens             ((?~))
import           Ide.Plugin.Cabal.FilepathCompletions
import           Ide.Plugin.Cabal.LicenseSuggest      (licenseNames)
import           Ide.Plugin.Cabal.Types
import qualified Language.LSP.Protocol.Lens           as JL
import qualified Language.LSP.Protocol.Types          as Compls (CompletionItem (..))
import qualified Language.LSP.Protocol.Types          as LSP
import qualified Language.LSP.VFS                     as VFS
import qualified System.FilePath                      as FP
import           System.FilePath                      (takeBaseName)
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
  snippetCompleter <> (constantCompleter $
    Map.keys (cabalVersionKeyword <> cabalKeywords) ++ Map.keys stanzaKeywordMap)
-- if we are in a keyword context in the top level,
-- we look up that keyword in the top level context and can complete its possible values
contextToCompleter (TopLevel, KeyWord kw) =
  case Map.lookup kw (cabalVersionKeyword <> cabalKeywords) of
    Nothing -> \recorder cData -> do
      logWith recorder Warning $ LogUnknownKeyWordInContextError kw
      noopCompleter recorder cData
    Just l -> l
-- if we are in a stanza and not in a keyword context,
-- we can write any of the stanza's keywords or a stanza declaration
contextToCompleter (Stanza s _, None) =
  case Map.lookup s stanzaKeywordMap of
    Nothing -> \recorder cData -> do
      logWith recorder Warning $ LogUnknownStanzaNameInContextError s
      noopCompleter recorder cData
    Just l -> constantCompleter $ Map.keys l ++ Map.keys stanzaKeywordMap
-- if we are in a stanza's keyword's context we can complete possible values of that keyword
contextToCompleter (Stanza s _, KeyWord kw) =
  case Map.lookup s stanzaKeywordMap of
    Nothing -> \recorder cData -> do
      logWith recorder Warning $ LogUnknownStanzaNameInContextError s
      noopCompleter recorder cData
    Just m -> case Map.lookup kw m of
      Nothing -> \recorder cData -> do
        logWith recorder Warning $ LogUnknownKeyWordInContextError kw
        noopCompleter recorder cData
      Just l -> l

{- | Takes prefix info about the previously written text
  and a rope (representing a file), returns the corresponding context.

  Can return Nothing if an error occurs.
  TODO: first line can only have cabal-version: keyword
-}
getContext :: (MonadIO m) => Recorder (WithPriority Log) -> CabalPrefixInfo -> Rope -> MaybeT m Context
getContext recorder prefInfo ls =
  case prevLinesM of
    Just prevLines -> do
      let lvlContext =
            if completionIndentation prefInfo  == 0
              then TopLevel
              else currentLevel prevLines
      case lvlContext of
        TopLevel -> do
          kwContext <- MaybeT . pure $ getKeyWordContext prefInfo prevLines (cabalVersionKeyword <> cabalKeywords)
          pure (TopLevel, kwContext)
        Stanza s n ->
          case Map.lookup s stanzaKeywordMap of
            Nothing -> do
              pure (Stanza s n, None)
            Just m -> do
              kwContext <- MaybeT . pure $ getKeyWordContext prefInfo prevLines m
              pure (Stanza s n, kwContext)
    Nothing -> do
      logWith recorder Warning $ LogFileSplitError pos
      -- basically returns nothing
      fail "Abort computation"
 where
  pos = completionCursorPosition prefInfo
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
getKeyWordContext prefInfo ls keywords = do
  case lastNonEmptyLineM of
    Nothing -> Just None
    Just lastLine' -> do
      let (whiteSpaces, lastLine) = T.span (== ' ') lastLine'
      let keywordIndentation = T.length whiteSpaces
      let cursorIndentation = completionIndentation prefInfo
      -- in order to be in a keyword context the cursor needs
      -- to be indented more than the keyword
      if cursorIndentation > keywordIndentation
        then -- if the last thing written was a keyword without a value
        case List.find (`T.isPrefixOf` lastLine) (Map.keys keywords) of
          Nothing -> Just None
          Just kw -> Just $ KeyWord kw
        else Just None
 where
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
  | Just (s, n) <- stanza = Stanza s n
  | otherwise = currentLevel xs
 where
  stanza = asum $ map checkStanza (Map.keys stanzaKeywordMap)
  checkStanza :: StanzaType -> Maybe (StanzaType, Maybe StanzaName)
  checkStanza t =
    case T.stripPrefix t (T.strip cur) of
      Just n
        | T.null n -> Just (t,Nothing)
        | otherwise -> Just (t, Just $ T.strip n)
      Nothing -> Nothing



mkDefaultCompletionItem :: T.Text -> LSP.CompletionItem
mkDefaultCompletionItem label = LSP.CompletionItem
    { Compls._label = label
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
    , Compls._textEdit = Nothing
    , Compls._textEditText = Nothing
    , Compls._additionalTextEdits = Nothing
    , Compls._commitCharacters = Nothing
    , Compls._command = Nothing
    , Compls._data_ = Nothing
    }

{- | Returns a CompletionItem with the given starting position
  and text to be inserted, where the displayed text is the same as the
  inserted text.
-}
mkSimpleCompletionItem :: Range -> T.Text -> LSP.CompletionItem
mkSimpleCompletionItem range txt = mkDefaultCompletionItem txt
  & JL.textEdit ?~ LSP.InL (LSP.TextEdit range txt)



{- | Returns a completionItem with the given starting position,
  text to be inserted and text to be displayed in the completion suggestion.
-}
mkCompletionItem :: Range -> T.Text -> T.Text -> LSP.CompletionItem
mkCompletionItem range insertTxt displayTxt = mkDefaultCompletionItem displayTxt
  & JL.textEdit ?~ LSP.InL (LSP.TextEdit range insertTxt)

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
getCabalPrefixInfo fp prefixInfo =
  CabalPrefixInfo
    { completionPrefix = completionPrefix'
    , completionSuffix = Just suffix
    , completionCursorPosition = VFS.cursorPos prefixInfo
    , completionRange = Range completionStart completionEnd
    , completionWorkingDir = FP.takeDirectory fp
    , normalizedCabalFilePath = LSP.toNormalizedFilePath fp
    , completionFileName = T.pack $ takeBaseName fp
    }
 where
  completionEnd = VFS.cursorPos prefixInfo
  completionStart =
    Position
      (_line completionEnd)
      (_character completionEnd - (fromIntegral $ T.length completionPrefix'))
  (beforeCursorText, afterCursorText) = T.splitAt cursorColumn $ VFS.fullLine prefixInfo
  completionPrefix' = T.takeWhileEnd (not . (`elem` stopConditionChars)) beforeCursorText
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

completionIndentation :: CabalPrefixInfo -> Int
completionIndentation prefInfo = fromIntegral (pos ^. JL.character) - (T.length $ completionPrefix prefInfo)
  where
    pos = completionCursorPosition prefInfo

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
constantCompleter completions _ cData  = do
  let prefInfo = cabalPrefixInfo cData
      scored = Fuzzy.simpleFilter 1000 10 (completionPrefix prefInfo) completions
      range = completionRange prefInfo
  pure $ map (mkSimpleCompletionItem range . Fuzzy.original) scored

nameCompleter :: Completer
nameCompleter _ cData = do
  let scored = Fuzzy.simpleFilter 1000 10 (completionPrefix prefInfo) [completionFileName prefInfo]
      prefInfo = cabalPrefixInfo cData
      range = completionRange prefInfo
  pure $ map (mkSimpleCompletionItem range . Fuzzy.original) scored


-- maps snippet triggerwords to match on with their completers
snippetCompleter :: Completer
snippetCompleter _ cData = do
  let scored = Fuzzy.simpleFilter 1000 10 (completionPrefix prefInfo) $ Map.keys snippetMap
  forM
    scored
    (\compl -> do
        let matched = Fuzzy.original compl
        let completion = fromMaybe [] $ Map.lookup matched snippetMap
        pure $ mkSnippetCompletion (T.unlines completion) matched
    )
      where
        prefInfo = cabalPrefixInfo cData
        mkSnippetCompletion :: T.Text -> T.Text -> LSP.CompletionItem
        mkSnippetCompletion insertText toDisplay = mkDefaultCompletionItem toDisplay
            & JL.kind ?~ LSP.CompletionItemKind_Snippet
            & JL.insertText ?~ insertText
            & JL.insertTextFormat ?~ LSP.InsertTextFormat_Snippet
        snippetMap = Map.fromList
                      [("library-snippet",
                         [ "library"
                           , "  hs-source-dirs: $1"
                           , "  exposed-modules: $0"
                           , "  build-depends: base"
                           , "  default-language: Haskell2010"
                         ]),
                        ("recommended-fields",
                          [ "cabal-version: $1"
                          , "name: " <> completionFileName prefInfo
                          , "version: 0.1.0.0"
                          , "maintainer: $4"
                          , "category: $5"
                          , "synopsis: $6"
                          , "license: $7"
                          , "build-type: Simple"
                          ]
                        )
                      ]


{- | Completer to be used when a set of values with priority weights
 attached to some values are to be completed for a field. The higher the weight,
 the higher the priority to show the value in the completion suggestion.
 If the value does not occur in the weighted map its weight is defaulted
 to zero.
-}
weightedConstantCompleter :: [T.Text] -> Map T.Text Double -> Completer
weightedConstantCompleter completions weights _ cData = do
  let scored = if perfectScore > 0
                then fmap Fuzzy.original $ Fuzzy.simpleFilter' 1000 10 prefix completions customMatch
                else topTenByWeight
      range = completionRange prefInfo
  pure $ map (mkSimpleCompletionItem range) scored
  where
    prefInfo = cabalPrefixInfo cData
    prefix = completionPrefix prefInfo
    -- this should never return Nothing since we match the word with itself
    perfectScore = fromMaybe (error "match is broken") $ Fuzzy.match prefix prefix
    customMatch :: (T.Text -> T.Text -> Maybe Int)
    customMatch toSearch searchSpace = do
      matched <- Fuzzy.match toSearch searchSpace
      let weight = fromMaybe 0 $ Map.lookup searchSpace weights
      let score = min
            perfectScore
            (round (fromIntegral matched * (1 + weight)))
      pure score
    topTenByWeight ::  [T.Text]
    topTenByWeight = take 10 $ map fst $ List.sortOn (Down . snd) $ Map.assocs weights

{- | Completer to be used when a file path can be
  completed for a field, takes the file path of  the directory to start from.
  Completes file paths as well as directories.
-}
filePathCompleter :: Completer
filePathCompleter recorder cData = do
  let prefInfo = cabalPrefixInfo cData
      suffix = fromMaybe "" $ completionSuffix prefInfo
      complInfo = pathCompletionInfoFromCabalPrefixInfo prefInfo
      toMatch = fromMaybe (partialFileName complInfo) $ T.stripPrefix "./" $ partialFileName complInfo
  filePathCompletions <- listFileCompletions recorder complInfo
  let scored = Fuzzy.simpleFilter 1000 10 toMatch (map T.pack filePathCompletions)
  forM
    scored
    ( \compl' -> do
        let compl = Fuzzy.original compl'
        fullFilePath <- mkFilePathCompletion suffix compl complInfo
        pure $ mkCompletionItem (completionRange prefInfo) fullFilePath fullFilePath
    )

{- | Completer to be used when module paths can be completed for the field.
-}
modulesCompleter :: (GenericPackageDescription -> [FilePath]) -> Completer
modulesCompleter extractionFunction recorder cData = do
  maybeGpd <- runIdeAction "cabal-plugin.modulesCompleter.parseCabal" extras
    $ useWithStaleFast ParseCabal $ normalizedCabalFilePath prefInfo
  case maybeGpd of
    Just (gpd, _) -> do
      let sourceDirs = extractionFunction gpd
      filePathCompletions <- filePathsForExposedModules sourceDirs recorder prefInfo
      pure $ map (\compl -> mkCompletionItem (completionRange prefInfo) compl compl) filePathCompletions
    Nothing -> do
      logWith recorder Debug LogUseWithStaleFastNoResult
      pure []
  where
    extras = shakeExtras (ideState cData)
    prefInfo = cabalPrefixInfo cData

exposedModuleExtraction :: GenericPackageDescription -> [FilePath]
exposedModuleExtraction gpd =
  -- we use condLibrary to get the information contained in the library stanza
  -- since the library in PackageDescription is not populated by us
  case libM of
    Just lib -> do
      map getSymbolicPath $ hsSourceDirs $ libBuildInfo $ condTreeData lib
    Nothing -> []
  where
    libM = condLibrary gpd

otherModulesExtraction :: GenericPackageDescription -> [FilePath]
otherModulesExtraction gpd =
  case libM of
    Just lib -> do
      map getSymbolicPath $ hsSourceDirs $ libBuildInfo $ condTreeData lib
    Nothing -> []
  where
    libM = condLibrary gpd

{- | Completer to be used when a directory can be completed for the field,
  takes the file path of the directory to start from.
  Only completes directories.
-}
directoryCompleter :: Completer
directoryCompleter recorder cData = do
  let prefInfo = cabalPrefixInfo cData
      complInfo = pathCompletionInfoFromCabalPrefixInfo prefInfo
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
        pure $ mkCompletionItem (completionRange prefInfo) fullDirPath fullDirPath
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
    [ ("name:", nameCompleter) -- TODO: should complete to filename, needs meta info
    , ("version:", noopCompleter)
    , ("build-type:", constantCompleter ["Simple", "Custom", "Configure", "Make"])
    , ("license:", weightedConstantCompleter licenseNames weightedLicenseNames)
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
          [ ("exposed-modules:", modulesCompleter exposedModuleExtraction) -- identifier list
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
      , Map.fromList
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

weightedLicenseNames :: Map T.Text Double
weightedLicenseNames = fmap statisticsToWeight $ Map.fromList
    [("BSD-3-Clause",9955)
    , ("MIT",3336)
    , ("GPL-3.0-only",679)
    , ("LicenseRef-OtherLicense",521)
    , ("Apache-2.0",514)
    , ("LicenseRef-GPL",443)
    , ("LicenseRef-PublicDomain",318)
    , ("MPL-2.0",288)
    , ("BSD-2-Clause",174)
    , ("GPL-2.0-only",160)
    , ("LicenseRef-LGPL",146)
    , ("LGPL-2.1-only",112)
    , ("LGPL-3.0-only",100)
    , ("AGPL-3.0-only",96)
    , ("ISC",89)
    , ("LicenseRef-Apache",45)
    , ("GPL-3.0-or-later",43)
    , ("BSD-2-Clause-Patent",33)
    , ("GPL-2.0-or-later",21)
    , ("CC0-1.0",16)
    , ("AGPL-3.0-or-later",15)
    , ("LGPL-2.1-or-later",12)
    , ("(BSD-2-Clause OR Apache-2.0)",10)
    , ("(Apache-2.0 OR MPL-2.0)",8)
    , ("LicenseRef-AGPL",6)
    , ("(BSD-3-Clause OR Apache-2.0)",4)
    , ("0BSD",3)
    , ("BSD-4-Clause",3)
    , ("LGPL-3.0-or-later",3)
    , ("LicenseRef-LGPL-2",2)
    , ("GPL-2.0-or-later AND BSD-3-Clause",2)
    , ("NONE",2)
    , ("Zlib",2)
    , ("(Apache-2.0 OR BSD-3-Clause)",2)
    , ("BSD-3-Clause AND GPL-2.0-or-later",2)
    , ("BSD-3-Clause AND GPL-3.0-or-later",2)
    ]
  where
    statisticsToWeight :: Int -> Double
    statisticsToWeight stat
      | stat < 10 = 0.1
      | stat < 20 = 0.3
      | stat < 50 = 0.4
      | stat < 100 = 0.5
      | stat < 500 = 0.6
      | stat < 650 = 0.7
      | otherwise = 0.9

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
