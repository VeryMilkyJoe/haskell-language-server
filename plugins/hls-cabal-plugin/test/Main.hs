{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedLabels         #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE TypeOperators            #-}

module Main (
    main,
) where

import           Control.Lens                         ((^.))
import           Control.Monad                        (guard)
import           Control.Monad.Trans.Maybe            (runMaybeT)
import qualified Data.ByteString                      as BS
import           Data.Either                          (isRight)
import           Data.List                            (sort)
import           Data.Row
import qualified Data.Text                            as T
import qualified Data.Text                            as Text
import qualified Data.Text.Utf16.Rope                 as Rope
import           Ide.Plugin.Cabal
import           Ide.Plugin.Cabal.Completions
import           Ide.Plugin.Cabal.FilepathCompletions
import           Ide.Plugin.Cabal.LicenseSuggest      (licenseErrorSuggestion)
import qualified Ide.Plugin.Cabal.Parse               as Lib
import           Ide.Plugin.Cabal.Types
import qualified Language.LSP.Protocol.Lens           as L
import qualified Language.LSP.VFS                     as VFS
import           System.Directory                     (getCurrentDirectory)
import           System.FilePath
import           Test.Hls

cabalPlugin :: PluginTestDescriptor Ide.Plugin.Cabal.Log
cabalPlugin = mkPluginTestDescriptor descriptor "cabal"

main :: IO ()
main = do
    defaultTestRunner $
        testGroup
            "Cabal Plugin Tests"
            [ unitTests
            , pluginTests
            ]

-- ------------------------------------------------------------------------
-- Unit Tests
-- ------------------------------------------------------------------------

unitTests :: TestTree
unitTests =
    testGroup
        "Unit Tests"
        [ cabalParserUnitTests
        , codeActionUnitTests
        , completionHelperTests
        , contextTests
        , filePathCompletionContextTests
        , pathCompleterTests
        ]

cabalParserUnitTests :: TestTree
cabalParserUnitTests =
    testGroup
        "Parsing Cabal"
        [ testCase "Simple Parsing works" $ do
            (warnings, pm) <- Lib.parseCabalFileContents =<< BS.readFile (testDataDir </> "simple.cabal")
            liftIO $ do
                null warnings @? "Found unexpected warnings"
                isRight pm @? "Failed to parse GenericPackageDescription"
        ]

codeActionUnitTests :: TestTree
codeActionUnitTests =
    testGroup
        "Code Action Tests"
        [ testCase "Unknown format" $ do
            -- the message has the wrong format
            licenseErrorSuggestion "Unknown license identifier: 'BSD3' Do you mean BSD-3-Clause?" @?= []
        , testCase "BSD-3-Clause" $ do
            take 2 (licenseErrorSuggestion "Unknown SPDX license identifier: 'BSD3' Do you mean BSD-3-Clause?")
                @?= [("BSD3", "BSD-3-Clause"), ("BSD3", "BSD-3-Clause-LBNL")]
        , testCase "MiT" $ do
            -- contains no suggestion
            take 2 (licenseErrorSuggestion "Unknown SPDX license identifier: 'MiT'")
                @?= [("MiT", "MIT"), ("MiT", "MIT-0")]
        ]

completionHelperTests :: TestTree
completionHelperTests =
    testGroup
        "Completion Helper Tests"
        [ testCase "get FilePath - partly written file path" $ do
            getFilePathCursorPrefix "src/a" 0 5 @?= "src/a"
        , testCase "get FilePath - ignores spaces" $ do
            getFilePathCursorPrefix "  src/a" 0 7 @?= "src/a"
        , testCase "get FilePath - ignores spaces and keyword" $ do
            getFilePathCursorPrefix "license-file: src/a" 0 19 @?= "src/a"
        , testCase "get FilePath - with apostrophe, ignores spaces and keyword" $ do
            getFilePathCursorPrefix "license-file: \"src/a" 0 20 @?= "src/a"
        , testCase "get FilePath - ignores list of filepaths beforehand, space separated" $ do
            getFilePathCursorPrefix "  ./text.txt file.h" 0 19 @?= "file.h"
        , testCase "get FilePath - ignores list of filepaths after, space separated" $ do
            getFilePathCursorPrefix "  ./text.t file.h" 0 10 @?= "./text.t"
        , testCase "get FilePath - ignores list of filepaths and rest of filepath after, space separated" $ do
            getFilePathCursorPrefix "  ./text.t file.h" 0 6 @?= "./te"
        , testCase "get FilePath - ignores list of filepaths beforehand, multiple space separated" $ do
            getFilePathCursorPrefix "  ./text.txt   file.h" 0 21 @?= "file.h"
        , testCase "get FilePath - ignores list of filepaths beforehand, comma separated" $ do
            getFilePathCursorPrefix "  ./text.txt, file.h" 0 20 @?= "file.h"
        , testCase "get FilePath - ignores list of filepaths beforehand, comma separated, many whitespaces" $ do
            getFilePathCursorPrefix "  ./text.txt,   file.h" 0 22 @?= "file.h"
        , testCase "get FilePath - ignores list of filepaths beforehand, comma separated, no whitespace" $ do
            getFilePathCursorPrefix "  ./text.txt,file.h" 0 19 @?= "file.h"
        , testCase "get FilePath - with apostrophes, ignores list of filepaths beforehand" $ do
            getFilePathCursorPrefix "  \"./text.txt\" \"file.h" 0 23 @?= "file.h"
        , testCase "get FilePath - ignores list of filepaths with apostrophe beforehand" $ do
            getFilePathCursorPrefix "  \"./text.txt\" file.h" 0 22 @?= "file.h"
        ]
  where
    getFilePathCursorPrefix :: T.Text -> UInt -> UInt -> T.Text
    getFilePathCursorPrefix lineString linePos charPos =
        completionPrefix . getCabalPrefixInfo "" $
            VFS.PosPrefixInfo
                { VFS.fullLine = lineString
                , VFS.prefixModule = ""
                , VFS.prefixText = ""
                , VFS.cursorPos = Position linePos charPos
                }

filePathCompletionContextTests :: TestTree
filePathCompletionContextTests =
    testGroup
        "File Path Completion Context Tests"
        [ testCase "empty file - start" $ do
            let complContext = getCabalPrefixInfo "" (simplePosPrefixInfo "" 0 0)
            completionSuffix complContext @?= Just ""
            completionPrefix complContext @?= ""
        , testCase "only whitespaces" $ do
            let complContext = getCabalPrefixInfo "" (simplePosPrefixInfo "   " 0 3)
            completionSuffix complContext @?= Just ""
            completionPrefix complContext @?= ""
        , testCase "simple filepath" $ do
            let complContext = getCabalPrefixInfo "" (simplePosPrefixInfo "   src/" 0 7)
            completionSuffix complContext @?= Just ""
            completionPrefix complContext @?= "src/"
        , testCase "simple filepath - starting apostrophe" $ do
            let complContext = getCabalPrefixInfo "" (simplePosPrefixInfo "   \"src/" 0 8)
            completionSuffix complContext @?= Just "\""
            completionPrefix complContext @?= "src/"
        , testCase "simple filepath - starting apostrophe, already closed" $ do
            let complContext = getCabalPrefixInfo "" (simplePosPrefixInfo "   \"src/\"" 0 8)
            completionSuffix complContext @?= Just ""
            completionPrefix complContext @?= "src/"
        , testCase "second filepath - starting apostrophe" $ do
            let complContext = getCabalPrefixInfo "" (simplePosPrefixInfo "fp.txt \"src/" 0 12)
            completionSuffix complContext @?= Just "\""
            completionPrefix complContext @?= "src/"
        , testCase "middle filepath - starting apostrophe" $ do
            let complContext = getCabalPrefixInfo "" (simplePosPrefixInfo "fp.txt \"src/ fp2.txt" 0 12)
            completionSuffix complContext @?= Just "\""
            completionPrefix complContext @?= "src/"
        , testCase "middle filepath - starting apostrophe, already closed" $ do
            let complContext = getCabalPrefixInfo "" (simplePosPrefixInfo "fp.t xt \"src\" fp2.txt" 0 12)
            completionSuffix complContext @?= Just ""
            completionPrefix complContext @?= "src"
        , testCase "middle filepath - starting apostrophe, already closed" $ do
            let complContext = getCabalPrefixInfo "" (simplePosPrefixInfo "\"fp.txt\" \"src fp2.txt" 0 13)
            completionSuffix complContext @?= Just "\""
            completionPrefix complContext @?= "src"
        ]
  where
    simplePosPrefixInfo :: T.Text -> UInt -> UInt -> VFS.PosPrefixInfo
    simplePosPrefixInfo lineString linePos charPos =
        VFS.PosPrefixInfo
            { VFS.fullLine = lineString
            , VFS.prefixModule = ""
            , VFS.prefixText = ""
            , VFS.cursorPos = Position linePos charPos
            }

pathCompleterTests :: TestTree
pathCompleterTests =
    testGroup
        "Path Completion Tests"
        [ fileCompleterTests
        , directoryCompleterTests
        , pathCompletionInfoFromCompletionContextTests
        , testGroup
            "Helper - List File Completion Tests"
            [ testCase "Current Directory" $ do
                testDir <- getTestDir
                compls <-
                    listFileCompletions
                        mempty
                        PathCompletionInfo
                            { partialFileName = ""
                            , partialFileDir = ""
                            , workingDir = testDir
                            }
                sort compls @?= [".hidden", "dir1/", "dir2/", "textfile.txt"]
            , testCase "In directory" $ do
                testDir <- getTestDir
                compls <-
                    listFileCompletions
                        mempty
                        PathCompletionInfo
                            { partialFileName = ""
                            , partialFileDir = "dir1/"
                            , workingDir = testDir
                            }
                sort compls @?= ["f1.txt", "f2.hs"]
            ]
        ]
  where
    simpleCabalPrefixInfo :: T.Text -> FilePath -> CabalPrefixInfo
    simpleCabalPrefixInfo prefix fp =
        CabalPrefixInfo
            { completionPrefix = prefix
            , completionSuffix = Nothing
            , completionCursorPosition = Position 0 0
            , completionRange = Range (Position 0 0) (Position 0 0)
            , completionWorkingDir = fp </> "test.cabal"
            }
    getTestDir :: IO FilePath
    getTestDir = do
        cwd <- getCurrentDirectory
        pure $ cwd </> "test/testdata/filepath-completions/"
    pathCompletionInfoFromCompletionContextTests :: TestTree
    pathCompletionInfoFromCompletionContextTests =
        testGroup
            "Completion Info to Completion Context Tests"
            [ testCase "Current Directory" $ do
                testDir <- getTestDir
                let complInfo = pathCompletionInfoFromCompletionContext $ simpleCabalPrefixInfo "" testDir
                partialFileDir complInfo @?= "./"
            , testCase "Current Directory - partly written next" $ do
                testDir <- getTestDir
                let complInfo = pathCompletionInfoFromCompletionContext $ simpleCabalPrefixInfo "di" testDir
                partialFileDir complInfo @?= "./"
                partialFileName complInfo @?= "di"
            , testCase "Current Directory - alternative writing" $ do
                testDir <- getTestDir
                let complInfo = pathCompletionInfoFromCompletionContext $ simpleCabalPrefixInfo "./" testDir
                partialFileDir complInfo @?= "./"
            , testCase "Subdirectory" $ do
                testDir <- getTestDir
                let complInfo = pathCompletionInfoFromCompletionContext $ simpleCabalPrefixInfo "dir1/" testDir
                partialFileDir complInfo @?= "dir1/"
                partialFileName complInfo @?= ""
            , testCase "Subdirectory - partly written next" $ do
                testDir <- getTestDir
                let complInfo = pathCompletionInfoFromCompletionContext $ simpleCabalPrefixInfo "dir1/d" testDir
                partialFileDir complInfo @?= "dir1/"
                partialFileName complInfo @?= "d"
            , testCase "Subdirectory - partly written next" $ do
                testDir <- getTestDir
                let complInfo = pathCompletionInfoFromCompletionContext $ simpleCabalPrefixInfo "dir1/dir2/d" testDir
                partialFileDir complInfo @?= "dir1/dir2/"
                partialFileName complInfo @?= "d"
            ]
    directoryCompleterTests :: TestTree
    directoryCompleterTests =
        testGroup
            "Directory Completer Tests"
            [ testCase "Current Directory" $ do
                testDir <- getTestDir
                completions <- completeDirectory "" testDir
                let insertCompletions = map itemInsert completions
                sort insertCompletions @?= ["./dir1/", "./dir2/"]
            , testCase "Current Directory - alternative writing" $ do
                testDir <- getTestDir
                completions <- completeDirectory "./" testDir
                let insertCompletions = map itemInsert completions
                sort insertCompletions @?= ["./dir1/", "./dir2/"]
            , testCase "Current Directory - incomplete directory path written" $ do
                testDir <- getTestDir
                completions <- completeDirectory "di" testDir
                let insertCompletions = map itemInsert completions
                sort insertCompletions @?= ["./dir1/", "./dir2/"]
            , testCase "Current Directory - incomplete filepath written" $ do
                testDir <- getTestDir
                completions <- completeDirectory "te" testDir
                let insertCompletions = map itemInsert completions
                sort insertCompletions @?= []
            , testCase "Subdirectory - no more directories found" $ do
                testDir <- getTestDir
                completions <- completeDirectory "dir1/" testDir
                let insertCompletions = map itemInsert completions
                sort insertCompletions @?= []
            , testCase "Subdirectory - available subdirectory" $ do
                testDir <- getTestDir
                completions <- completeDirectory "dir2/" testDir
                let insertCompletions = map itemInsert completions
                sort insertCompletions @?= ["dir2/dir3/"]
            , testCase "Nonexistent directory" $ do
                testDir <- getTestDir
                completions <- completeDirectory "dir2/dir4/" testDir
                let insertCompletions = map itemInsert completions
                sort insertCompletions @?= []
            ]
      where
        completeDirectory :: T.Text -> TestName -> IO [CabalCompletionItem]
        completeDirectory written dirName = directoryCompleter mempty $ simpleCabalPrefixInfo written dirName
    fileCompleterTests :: TestTree
    fileCompleterTests =
        testGroup
            "File Completer Tests"
            [ testCase "Current Directory" $ do
                testDir <- getTestDir
                completions <- completeFilePath "" testDir
                let insertCompletions = map itemInsert completions
                sort insertCompletions @?= ["./.hidden", "./dir1/", "./dir2/", "./textfile.txt"]
            , testCase "Current Directory - alternative writing" $ do
                testDir <- getTestDir
                completions <- completeFilePath "./" testDir
                let insertCompletions = map itemInsert completions
                sort insertCompletions @?= ["./.hidden", "./dir1/", "./dir2/", "./textfile.txt"]
            , testCase "Current Directory - hidden file start" $ do
                testDir <- getTestDir
                completions <- completeFilePath "." testDir
                let insertCompletions = map itemInsert completions
                sort insertCompletions @?= ["./.hidden", "./textfile.txt"]
            , testCase "Current Directory - incomplete directory path written" $ do
                testDir <- getTestDir
                completions <- completeFilePath "di" testDir
                let insertCompletions = map itemInsert completions
                sort insertCompletions @?= ["./dir1/", "./dir2/"]
            , testCase "Current Directory - incomplete filepath written" $ do
                testDir <- getTestDir
                completions <- completeFilePath "te" testDir
                let insertCompletions = map itemInsert completions
                sort insertCompletions @?= ["./textfile.txt"]
            , testCase "Subdirectory" $ do
                testDir <- getTestDir
                completions <- completeFilePath "dir1/" testDir
                let insertCompletions = map itemInsert completions
                sort insertCompletions @?= ["dir1/f1.txt", "dir1/f2.hs"]
            , testCase "Subdirectory - incomplete filepath written" $ do
                testDir <- getTestDir
                completions <- completeFilePath "dir2/dir3/MA" testDir
                let insertCompletions = map itemInsert completions
                sort insertCompletions @?= ["dir2/dir3/MARKDOWN.md"]
            , testCase "Nonexistent directory" $ do
                testDir <- getTestDir
                completions <- completeFilePath "dir2/dir4/" testDir
                let insertCompletions = map itemInsert completions
                sort insertCompletions @?= []
            ]
      where
        completeFilePath :: T.Text -> TestName -> IO [CabalCompletionItem]
        completeFilePath written dirName = filePathCompleter mempty $ simpleCabalPrefixInfo written dirName
contextTests :: TestTree
contextTests =
    testGroup
        "Context Tests"
        [ testCase "Empty File - Start" $ do
            -- for a completely empty file, the context needs to
            -- be top level without a specified keyword
            ctx <- callGetContext (Position 0 0) [""]
            ctx @?= (TopLevel, None)
        , testCase "Cabal version keyword - no value, no space after :" $ do
            -- on a file, where the keyword is already written
            -- the context should still be toplevel but the keyword should be recognized
            ctx <- callGetContext (Position 0 14) ["cabal-version:"]
            ctx @?= (TopLevel, KeyWord "cabal-version:")
        , testCase "Cabal version keyword - cursor in keyword" $ do
            -- on a file, where the keyword is already written
            -- but the cursor is in the middle of the keyword,
            -- we are not in a keyword context
            ctx <- callGetContext (Position 0 5) ["cabal-version:"]
            ctx @?= (TopLevel, None)
        , testCase "Cabal version keyword - no value, many spaces" $ do
            -- on a file, where the "cabal-version:" keyword is already written
            -- the context should still be top level but the keyword should be recognized
            ctx <- callGetContext (Position 0 45) ["cabal-version:" <> T.replicate 50 " "]
            ctx @?= (TopLevel, KeyWord "cabal-version:")
        , testCase "Cabal version keyword - keyword partly written" $ do
            -- in the first line of the file, if the keyword
            -- has not been written completely, the keyword context
            -- should still be None
            ctx <- callGetContext (Position 0 5) ["cabal"]
            ctx @?= (TopLevel, None)
        , testCase "Cabal version keyword - value partly written" $ do
            -- in the first line of the file, if the keyword
            -- has not been written completely, the keyword context
            -- should still be None
            ctx <- callGetContext (Position 0 17) ["cabal-version: 1."]
            ctx @?= (TopLevel, KeyWord "cabal-version:")
        , testCase "Inside Stanza - no keyword" $ do
            -- on a file, where the library stanza has been defined
            -- but no keyword is defined afterwards, the stanza context should be recognized
            ctx <- callGetContext (Position 3 2) libraryStanzaData
            ctx @?= (Stanza "library", None)
        , testCase "Inside Stanza - keyword, no value" $ do
            -- on a file, where the library stanza and a keyword
            -- has been defined, the keyword and stanza should be recognized
            ctx <- callGetContext (Position 4 21) libraryStanzaData
            ctx @?= (Stanza "library", KeyWord "build-depends:")
        , expectFailBecause "While not valid, it is not that important to make the code more complicated for this" $
            testCase "Cabal version keyword - no value, next line" $ do
                -- if the cabal version keyword has been written but without a value,
                -- in the next line we still should be in top level context with no keyword
                -- since the cabal version keyword and value pair need to be in the same line
                ctx <- callGetContext (Position 1 2) ["cabal-version:", ""]
                ctx @?= (TopLevel, None)
        , testCase "Non-cabal-version keyword - no value, next line indentented position" $ do
            -- if a keyword, other than the cabal version keyword has been written
            -- with no value, in the next line we still should be in top level keyword context
            -- of the keyword with no value, since its value may be written in the next line
            ctx <- callGetContext (Position 2 4) topLevelData
            ctx @?= (TopLevel, KeyWord "name:")
        , testCase "Non-cabal-version keyword - no value, next line at start" $ do
            -- if a keyword, other than the cabal version keyword has been written
            -- with no value, in the next line we still should be in top level context
            -- but not the keyword's, since it is not viable to write a value for a
            -- keyword a the start of the next line
            ctx <- callGetContext (Position 2 0) topLevelData
            ctx @?= (TopLevel, None)
        , testCase "Non-cabal-version keyword - no value, multiple lines between" $ do
            -- if a keyword, other than the cabal version keyword has been written
            -- with no value, even with multiple lines in between we can still write the
            -- value corresponding to the keyword
            ctx <- callGetContext (Position 5 4) topLevelData
            ctx @?= (TopLevel, KeyWord "name:")
        , testCase "Keyword inside stanza - cursor indented more than keyword in next line" $ do
            -- if a keyword, other than the cabal version keyword has been written
            -- in a stanza context with no value, then the value may be written in the next line,
            -- when the cursor is indented more than the keyword
            ctx <- callGetContext (Position 5 8) libraryStanzaData
            ctx @?= (Stanza "library", KeyWord "build-depends:")
        , testCase "Keyword inside stanza - cursor indented less than keyword in next line" $ do
            -- if a keyword, other than the cabal version keyword has been written
            -- in a stanza context with no value, then the value may not be written in the next line,
            -- when the cursor is indented less than the keyword
            ctx <- callGetContext (Position 5 2) libraryStanzaData
            ctx @?= (Stanza "library", None)
        , testCase "Keyword inside stanza - cursor at start of next line" $ do
            -- in a stanza context with no value the value may not be written in the next line,
            -- when the cursor is not indented and we are in the top level context
            ctx <- callGetContext (Position 5 0) libraryStanzaData
            ctx @?= (TopLevel, None)
        , testCase "Top level - cursor in later line with partially written value" $ do
            ctx <- callGetContext (Position 5 13) topLevelData
            ctx @?= (TopLevel, KeyWord "name:")
        ]
  where
    callGetContext :: Position -> [T.Text] -> IO Context
    callGetContext pos ls = do
        runMaybeT (getContext mempty (simpleCabalPrefixInfo pos) (Rope.fromText $ T.unlines ls))
            >>= \case
                Nothing -> assertFailure "Context must be found"
                Just ctx -> pure ctx

    simpleCabalPrefixInfo :: Position -> CabalPrefixInfo
    simpleCabalPrefixInfo pos =
        CabalPrefixInfo
            { completionPrefix = ""
            , completionSuffix = Nothing
            , completionCursorPosition = pos
            , completionRange = Range pos (Position 0 0)
            , completionWorkingDir = ""
            }

-- ------------------------------------------------------------------------
-- Integration Tests
-- ------------------------------------------------------------------------

pluginTests :: TestTree
pluginTests =
    testGroup
        "Plugin Tests"
        [ testGroup
            "Diagnostics"
            [ runCabalTestCaseSession "Publishes Diagnostics on Error" "" $ do
                doc <- openDoc "invalid.cabal" "cabal"
                diags <- waitForDiagnosticsFromSource doc "cabal"
                unknownLicenseDiag <- liftIO $ inspectDiagnostic diags ["Unknown SPDX license identifier: 'BSD3'"]
                liftIO $ do
                    length diags @?= 1
                    unknownLicenseDiag ^. L.range @?= Range (Position 3 24) (Position 4 0)
                    unknownLicenseDiag ^. L.severity @?= Just DiagnosticSeverity_Error
            , runCabalTestCaseSession "Clears diagnostics" "" $ do
                doc <- openDoc "invalid.cabal" "cabal"
                diags <- waitForDiagnosticsFrom doc
                unknownLicenseDiag <- liftIO $ inspectDiagnostic diags ["Unknown SPDX license identifier: 'BSD3'"]
                liftIO $ do
                    length diags @?= 1
                    unknownLicenseDiag ^. L.range @?= Range (Position 3 24) (Position 4 0)
                    unknownLicenseDiag ^. L.severity @?= Just DiagnosticSeverity_Error
                _ <- applyEdit doc $ TextEdit (Range (Position 3 20) (Position 4 0)) "BSD-3-Clause\n"
                newDiags <- waitForDiagnosticsFrom doc
                liftIO $ newDiags @?= []
            , runCabalTestCaseSession "No Diagnostics in .hs files from valid .cabal file" "simple-cabal" $ do
                hsDoc <- openDoc "A.hs" "haskell"
                expectNoMoreDiagnostics 1 hsDoc "typechecking"
                cabalDoc <- openDoc "simple-cabal.cabal" "cabal"
                expectNoMoreDiagnostics 1 cabalDoc "parsing"
            , ignoreTestBecause "Testcase is flaky for certain GHC versions (e.g. 9.2.5). See #3333 for details." $ do
                runCabalTestCaseSession "Diagnostics in .hs files from invalid .cabal file" "simple-cabal" $ do
                    hsDoc <- openDoc "A.hs" "haskell"
                    expectNoMoreDiagnostics 1 hsDoc "typechecking"
                    cabalDoc <- openDoc "simple-cabal.cabal" "cabal"
                    expectNoMoreDiagnostics 1 cabalDoc "parsing"
                    let theRange = Range (Position 3 20) (Position 3 23)
                    -- Invalid license
                    changeDoc
                        cabalDoc
                        [ TextDocumentContentChangeEvent $
                            InL $
                                #range
                                    .== theRange
                                    .+ #rangeLength
                                    .== Nothing
                                    .+ #text
                                    .== "MIT3"
                        ]
                    cabalDiags <- waitForDiagnosticsFrom cabalDoc
                    unknownLicenseDiag <- liftIO $ inspectDiagnostic cabalDiags ["Unknown SPDX license identifier: 'MIT3'"]
                    expectNoMoreDiagnostics 1 hsDoc "typechecking"
                    liftIO $ do
                        length cabalDiags @?= 1
                        unknownLicenseDiag ^. L.range @?= Range (Position 3 24) (Position 4 0)
                        unknownLicenseDiag ^. L.severity @?= Just DiagnosticSeverity_Error
            ]
        , testGroup
            "Code Actions"
            [ runCabalTestCaseSession "BSD-3" "" $ do
                doc <- openDoc "licenseCodeAction.cabal" "cabal"
                diags <- waitForDiagnosticsFromSource doc "cabal"
                reduceDiag <- liftIO $ inspectDiagnostic diags ["Unknown SPDX license identifier: 'BSD3'"]
                liftIO $ do
                    length diags @?= 1
                    reduceDiag ^. L.range @?= Range (Position 3 24) (Position 4 0)
                    reduceDiag ^. L.severity @?= Just DiagnosticSeverity_Error
                [codeAction] <- getLicenseAction "BSD-3-Clause" <$> getCodeActions doc (Range (Position 3 24) (Position 4 0))
                executeCodeAction codeAction
                contents <- documentContents doc
                liftIO $
                    contents
                        @?= Text.unlines
                            [ "cabal-version:      3.0"
                            , "name:               licenseCodeAction"
                            , "version:            0.1.0.0"
                            , "license:            BSD-3-Clause"
                            , ""
                            , "library"
                            , "    build-depends:    base"
                            , "    default-language: Haskell2010"
                            ]
            , runCabalTestCaseSession "Apache-2.0" "" $ do
                doc <- openDoc "licenseCodeAction2.cabal" "cabal"
                diags <- waitForDiagnosticsFromSource doc "cabal"
                -- test if it supports typos in license name, here 'apahe'
                reduceDiag <- liftIO $ inspectDiagnostic diags ["Unknown SPDX license identifier: 'APAHE'"]
                liftIO $ do
                    length diags @?= 1
                    reduceDiag ^. L.range @?= Range (Position 3 25) (Position 4 0)
                    reduceDiag ^. L.severity @?= Just DiagnosticSeverity_Error
                [codeAction] <- getLicenseAction "Apache-2.0" <$> getCodeActions doc (Range (Position 3 24) (Position 4 0))
                executeCodeAction codeAction
                contents <- documentContents doc
                liftIO $
                    contents
                        @?= Text.unlines
                            [ "cabal-version:      3.0"
                            , "name:               licenseCodeAction2"
                            , "version:            0.1.0.0"
                            , "license:            Apache-2.0"
                            , ""
                            , "library"
                            , "    build-depends:    base"
                            , "    default-language: Haskell2010"
                            ]
            ]
        ]
  where
    getLicenseAction :: T.Text -> [Command |? CodeAction] -> [CodeAction]
    getLicenseAction license codeActions = do
        InR action@CodeAction{_title} <- codeActions
        guard (_title == "Replace with " <> license)
        pure action

-- ------------------------------------------------------------------------
-- Runner utils
-- ------------------------------------------------------------------------

runCabalTestCaseSession :: TestName -> FilePath -> Session () -> TestTree
runCabalTestCaseSession title subdir = testCase title . runCabalSession subdir

runCabalSession :: FilePath -> Session a -> IO a
runCabalSession subdir =
    failIfSessionTimeout . runSessionWithServer cabalPlugin (testDataDir </> subdir)

testDataDir :: FilePath
testDataDir = "test" </> "testdata"

-- ------------------------------------------------------------------------
-- Test Data
-- ------------------------------------------------------------------------
libraryStanzaData :: [T.Text]
libraryStanzaData =
    [ "cabal-version:      3.0"
    , "name:               simple-cabal"
    , "library "
    , "    default-language: Haskell98"
    , "    build-depends:    "
    , "           "
    ]

topLevelData :: [T.Text]
topLevelData =
    [ "cabal-version:      3.0"
    , "name:"
    , ""
    , ""
    , ""
    , "          eee"
    ]
