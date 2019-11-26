{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import           CodePruner            (getFiles, getIncludes)
import           Control.Exception     (SomeException, catch)
import           Control.Monad         (filterM, forever, when)
import           Data.Foldable         (foldlM)
import           Data.List             (group, isInfixOf, isPrefixOf, sort)
import           Data.Set              (Set, empty, insert, notMember)
import           System.Directory      (doesFileExist, removeFile)
import           System.Environment    (getArgs)
import           System.FilePath.Posix (dropExtension, normalise, takeDirectory,
                                        takeExtension, (<.>), (</>))
import           System.IO             (BufferMode (..), IOMode (..), hClose,
                                        hFlush, hPutStrLn, hSetBuffering,
                                        openFile)
import           System.IO.Strict      (hGetContents)

includeDirs :: [FilePath]
includeDirs = [
                ""
              , "platform" </> "macos"
              , "platform" </> "linux"
              , "platform" </> "windows"
              , "platform" </> "cygwin"
              ]
-- includeDirs = [ "src"
--               , "src" </> "platform" </> "macos"
--               , "src" </> "platform" </> "linux"
--               , "src" </> "platform" </> "windows"
--               , "src" </> "platform" </> "cygwin"
--               , "external"
--               , "external" </> "boost_1_55_0"
--               , "external" </> "dbio"
--               , "external" </> "include"
--               , "external" </> "cifparse"
--               ]

headerExtensions :: [String]
headerExtensions = [".hh", ".hpp", ".h", ".h++", ".hxx", ".in", ".hm", ".txx", ".ipp", ".ihh", ".ii"]

sourceExtensions :: [String]
sourceExtensions = [".cpp", ".c", ".cxx", ".c++", ".C", ".cc"]

cppExtensions :: [String]
cppExtensions = headerExtensions <> sourceExtensions

specialDir :: String
specialDir = "src"

main :: IO ()
main = do
  [src, file] <- getArgs
  allFiles <- getFiles src
  let srcDir = src </> specialDir
      srcFiles = filter (srcDir `isPrefixOf`) allFiles
  let headerFiles = filter (\s -> takeExtension s `elem` cppExtensions) srcFiles
      cmakeFiles  = filter (\s -> takeExtension s == ".cmake") allFiles
  (included, _) <- traverseIncludes srcDir ([], empty) file
  let !filesSorted = nubOrd $ fmap normalise headerFiles
      !includedSorted = nubOrd $ fmap normalise included
      !includedNotSources = filterSorted includedSorted filesSorted
      !notIncludedSources = filterSorted filesSorted includedSorted
      !notIncludedNames'  = concatMap getSources notIncludedSources
  !notIncludedNames'' <- fmap (drop (length srcDir + 1)) <$> filterM doesFileExist notIncludedNames'
  let notIncludedNames = filter (/= file) notIncludedNames''


  putStrLn $ "included: " <> show (length includedSorted)
  putStrLn $ unlines (take 10 includedSorted)

  putStrLn "included not in sources: "
  putStrLn $ unlines (take 10 includedNotSources)

  putStrLn $ "not included sources: " <> show (length notIncludedSources)
  putStrLn $ unlines (take 10 notIncludedSources)

  putStrLn $ "not included names: " <> show (length notIncludedNames)
  putStrLn $ unlines (take 10 notIncludedNames)

  putStrLn "cmake headerFiles: "
  putStrLn $ unlines cmakeFiles

  putStrLn $ "Total source files: " <> show (length filesSorted)
  putStrLn $ "Total included source files: " <> show (length includedSorted)
  putStrLn $ "Included but not in sources (wtf if not 0): " <> show (length includedNotSources)
  putStrLn $ "Not included sources: " <> show (length notIncludedSources)

  putStrLn "Removing unused sources..."
  removeSources notIncludedSources
  putStrLn "Done!"
  putStrLn "Modifying CMake files..."
  mapM_ (commentCMake notIncludedNames) cmakeFiles
  putStrLn "Done! "
  forever $ safe $ do
    [cmd, query] <- words <$> getLine
    let queries = nubOrd $ query : fmap (\s -> srcDir </> s </> query) includeDirs
    case cmd of
      "inf" -> if any (`elem` included) queries
               then putStrLn "included"
               else putStrLn "not included"
      "inc" -> do
        files <- filterM doesFileExist queries
        case files of
          [] -> putStrLn "No such file."
          [query'] -> do
            inc <- getIncludes query'
            putStrLn $ unlines inc
          x -> putStrLn "Multiple candidates: " >> putStrLn (unlines x)
      _    -> putStrLn "sorry?"

traverseIncludes :: FilePath
                 -> ([FilePath], Set FilePath)
                 -> FilePath
                 -> IO ([FilePath], Set FilePath)
traverseIncludes src (prevResults, processed) file = do
  fileIncludes <- concatMap getSources <$> getIncludes file

  let pathVariants = concatMap (\fpath -> ((src </> fpath) </>) <$> fileIncludes) (takeDirectory file : includeDirs)
      uniquePathVariants = nubOrd $ normalise <$> pathVariants

  existingFiles <- filterM doesFileExist uniquePathVariants

  let includedSources = nubOrd existingFiles
      newSources      = filter (`notMember` processed) includedSources
      newProcessed = foldr insert processed newSources

  (childrenIncludes, childrenProcessed) <- foldlM (traverseIncludes src) ([], newProcessed) newSources

  let result = newSources <> childrenIncludes <> prevResults

  pure (nubOrd result, childrenProcessed)

filterSorted :: Ord a => [a] -> [a] -> [a]
filterSorted [] _ = []
filterSorted lst [] = lst
filterSorted (x:xs) (y:ys)
  | x < y = x : filterSorted xs (y:ys)
  | x == y = filterSorted xs (y:ys)
  | otherwise = filterSorted (x:xs) ys

nubOrd :: (Eq a, Ord a) => [a] -> [a]
nubOrd = fmap head . group . sort

commentOccurences :: [String] -> [String] -> [String]
commentOccurences patterns = fmap commentLine
  where
    commentLine :: String -> String
    commentLine str = if any (`isInfixOf` str) patterns then '#':str else str

removeSources :: [String] -> IO ()
removeSources = mapM_ removeSource
  where
    removeSource :: String -> IO ()
    removeSource path = do
      let allFiles = getSources path
      existingFiles <- filterM doesFileExist allFiles
      mapM_ (safe . removeFile) existingFiles

commentCMake :: [String] -> FilePath -> IO ()
commentCMake patterns path = do
  putStrLn $ "Modifying " <> path
  hnd <- openFile path ReadMode
  !contents <- hGetContents hnd
  hClose hnd
  let !commentedContents = commentOccurences patterns $ lines contents
  res <- openFile path WriteMode
  hSetBuffering res NoBuffering
  mapM_ (hPutStrLn res) commentedContents
  hFlush res
  hClose res
  when (contents /= unlines commentedContents) $ putStrLn "File was changed."

getSources :: FilePath -> [FilePath]
getSources path = path:basePath:allFiles
  where
    basePath = dropExtension path
    allFiles = (basePath <.>) <$> sourceExtensions

safe :: IO () -> IO ()
safe act = act `catch` (appendFile "logs.txt" . (++ "\n") . show @SomeException)

duplicateFirst :: String -> String
duplicateFirst []       = []
duplicateFirst ('/':xs) = '/':'/':xs
duplicateFirst (x:xs)   = x : duplicateFirst xs
