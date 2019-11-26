{-# LANGUAGE BangPatterns #-}
module CodePruner
  (
    getFiles
  , getIncludes
  , takeWhile2
  ) where

import           Control.Monad         (filterM)
import           Data.Char             (isSpace)
import           Data.List             (dropWhileEnd, isPrefixOf)
import           System.Directory      (doesDirectoryExist, doesFileExist,
                                        listDirectory)
import           System.FilePath.Posix (splitPath, (</>))
import           System.IO             (IOMode (..), hClose, hSetEncoding,
                                        latin1, openFile)
import           System.IO.Strict      (hGetContents)

getFiles :: FilePath -> IO [String]
getFiles path = do
  isDir <- doesDirectoryExist path
  if isDir
     then getFilesPref "" (path </> "")
     else pure []

getFilesPref :: String -> FilePath -> IO [String]
getFilesPref pref path = do
  contents <- listDirectory path
  let dirname = last $ splitPath path
  -- print $ "***      dirname " <> dirname
  -- print $ "***      pref " <> pref
  files <- filterM doesFileExist $ fmap (path </>) contents
  dirs  <- filterM doesDirectoryExist $ fmap (path </>) contents
  let fullFiles = fmap (pref </>) files
      childDirs = fmap (path </>) dirs

  children <- mapM (getFilesPref dirname) childDirs
  let fullDirs = fmap (pref </>) (concat children)

  pure (fullFiles <> fullDirs)

getIncludes :: FilePath -> IO [String]
getIncludes path = do
  hnd <- openFile path ReadMode
  hSetEncoding hnd latin1
  !contents <- hGetContents hnd
  hClose hnd
  let incStrs = filter ("#include" `isPrefixOf`) . fmap (dropWhile isSpace) $ lines contents
      trimmedIncStrs  = fmap (dropWhileEnd isSpace . dropWhile isSpace . drop (length "#include")) incStrs
      bracketIncludes = fmap (takeWhileE (/= '>') . dropWhile (/= '<')) trimmedIncStrs
      quoteIncludes   = fmap (takeWhile2 (/= '"') . dropWhile (/= '"')) trimmedIncStrs
      bracketDirs = fmap (\s -> if not (null s) && head s == '<' && last s == '>' then tail . init $ s else "*** error > " <> s) bracketIncludes
      quoteDirs   = fmap (\s -> if not (null s) && head s == '"' && last s == '"' then tail . init $ s else "*** error > " <> s) quoteIncludes
      dirs = filter (\s -> not (null s) && head s /= '*') $ bracketDirs <> quoteDirs
  pure dirs

takeWhile2 :: (a -> Bool) -> [a] -> [a]
takeWhile2 pr lst = part1 ++ part2 ++ (if null tail2 then [] else [head tail2])
  where
    part1 = takeWhile pr lst
    tail1 = dropWhile pr lst
    part2 = (if null tail1 then [] else [head tail1]) ++ (takeWhile pr (if null tail1 then [] else tail tail1))
    tail2 = dropWhile pr (if null tail1 then [] else tail tail1)

takeWhileE :: (a -> Bool) -> [a] -> [a]
takeWhileE pr lst = part1 ++ (if null tail1 then [] else [head tail1])
  where
    part1 = takeWhile pr lst
    tail1 = dropWhile pr lst

