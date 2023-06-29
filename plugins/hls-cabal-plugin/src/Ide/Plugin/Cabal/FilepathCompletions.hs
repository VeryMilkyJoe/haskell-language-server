{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ide.Plugin.Cabal.FilepathCompletions where

import           Control.Exception            (evaluate, try)
import           Control.Monad                (filterM)
import           Control.Monad.Extra          (concatForM, forM)
import           Data.List                    (stripPrefix)
import           Data.Maybe                   (fromMaybe)
import qualified Data.Text                    as T
import           Development.IDE.Types.Logger
import           Ide.Plugin.Cabal.Types
import           System.Directory             (doesDirectoryExist,
                                               doesFileExist, listDirectory)
import qualified System.FilePath              as FP
import           System.FilePath              (dropExtension)
import qualified System.FilePath.Posix        as Posix
import qualified Text.Fuzzy.Parallel          as Fuzzy

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
  , workingDir      :: FilePath
  -- ^ current working directory of the handled file
  }
  deriving (Eq, Show, Read)

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

pathCompletionInfoFromCabalPrefixInfo :: CabalPrefixInfo -> PathCompletionInfo
pathCompletionInfoFromCabalPrefixInfo ctx =
  PathCompletionInfo
    { partialFileName = dirNamePrefix
    , partialFileDir = Posix.addTrailingPathSeparator $ Posix.takeDirectory prefix
    , workingDir = dir
    }
 where
  prefix = T.unpack $ completionPrefix ctx
  dirNamePrefix = T.pack $ Posix.takeFileName prefix
  dir = completionWorkingDir ctx

{- | Extracts the source dirs from the library stanza in the cabal file using the GPD
  and returns a list of path completions relative to any source dir  which fit the passed prefix info.
-}
filePathsForExposedModules :: [FilePath] -> Recorder (WithPriority Log) -> CabalPrefixInfo -> IO [T.Text]
filePathsForExposedModules srcDirs recorder prefInfo = do
      concatForM
        srcDirs
        (\dir -> do
          let pInfo =
                PathCompletionInfo
                { partialFileName = T.pack $ Posix.takeFileName prefix
                , partialFileDir =  Posix.addTrailingPathSeparator $ Posix.takeDirectory prefix
                , workingDir = completionWorkingDir prefInfo FP.</> dir
                }
          completions <- listFileCompletions recorder pInfo
          validExposedCompletions <- filterM (isValidExposedModulePath pInfo) completions
          let filePathCompletions = map (fpToExposedModulePath dir) validExposedCompletions
              toMatch = fromMaybe (partialFileName pInfo) $ T.stripPrefix "./" $ partialFileName pInfo
              scored = Fuzzy.simpleFilter 1000 10 toMatch (map T.pack filePathCompletions)
          forM
            scored
            ( \compl' -> do
                let compl = Fuzzy.original compl'
                fullFilePath <- mkExposedModulePathCompletion compl pInfo
                pure fullFilePath
            )
        )
  where
    prefix =
        exposedModulePathToFp
        $ completionPrefix prefInfo
    isValidExposedModulePath :: PathCompletionInfo -> FilePath -> IO Bool
    isValidExposedModulePath pInfo path = do
      let dir = mkCompletionDirectory pInfo
      fileExists <- doesFileExist (dir FP.</> path)
      pure $ not fileExists || FP.isExtensionOf ".hs" path


fpToExposedModulePath :: FilePath -> FilePath -> FilePath
fpToExposedModulePath srcDir fp' = T.unpack $ T.intercalate "." $ fmap T.pack $  FP.splitDirectories fp
  where
    fp = fromMaybe fp' $ stripPrefix srcDir fp'

exposedModulePathToFp :: T.Text -> FilePath
exposedModulePathToFp fp = T.unpack $ T.replace "." (T.singleton FP.pathSeparator) fp


{- | Returns the directory, the currently handled cabal file is in.

  We let System.FilePath handle the separator syntax since this is used
  to query filepaths from the system. See Note [Using correct file path separators].
-}
mkCompletionDirectory :: PathCompletionInfo -> FilePath
mkCompletionDirectory complInfo =
  FP.addTrailingPathSeparator $
    workingDir complInfo FP.</> (FP.normalise $ partialFileDir complInfo)

{- | Returns the complete filepath for the given partial filepath
  by combining the current working directory of the cabal file
  with the given partly written file path.

  Since this is used for completions we use posix separators here.
  See Note [Using correct file path separators].
-}
mkDirFromCWD :: PathCompletionInfo -> FilePath -> FilePath
mkDirFromCWD complInfo fp =
  Posix.addTrailingPathSeparator $
    mkCompletionDirectory complInfo Posix.</> Posix.normalise fp

{- | Takes a directory and PathCompletionInfo and
  returns the whole path including the prefix that was already written.

  Since this is used for completions we use posix separators here.
  See Note [Using correct file path separators].
-}
mkPathCompletion :: PathCompletionInfo -> T.Text -> T.Text
mkPathCompletion complInfo completion =
  T.pack $
    partialFileDir complInfo Posix.</> T.unpack completion

{-   Takes a suffix, a completed path and a pathCompletionInfo and
   generates the whole filepath including the already written prefix,
   and the suffix in case the completed path is a filepath.
-}
mkFilePathCompletion :: T.Text -> T.Text -> PathCompletionInfo -> IO T.Text
mkFilePathCompletion suffix completion complInfo = do
  let combinedPath = T.unpack $ mkPathCompletion complInfo completion
  isFilePath <- doesFileExist combinedPath
  let completedPath = if isFilePath then combinedPath ++ T.unpack suffix else combinedPath
  pure $ T.pack completedPath

{- Takes a completed path and a pathCompletionInfo and generates the whole completed
  filepath including the already written prefix using the cabal syntax for exposed modules.

  i.e. Dir.Dir2.HaskellFile
  or   Dir.Dir2.
-}
mkExposedModulePathCompletion :: T.Text -> PathCompletionInfo -> IO T.Text
mkExposedModulePathCompletion completion complInfo = do
  let combinedPath = T.unpack $ mkPathCompletion complInfo completion
  isFilePath <- doesFileExist (workingDir complInfo FP.</> combinedPath)
  let completedPath = T.pack $ if isFilePath then dropExtension combinedPath else combinedPath ++ "."
  let exposedPath = fromMaybe completedPath $ T.stripPrefix "./" completedPath
  pure $ T.pack $ fpToExposedModulePath "" $ T.unpack exposedPath
