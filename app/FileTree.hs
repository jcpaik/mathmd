module FileTree where
import Data.List ( sortOn )
import Turtle hiding ( sortOn )
import qualified Control.Foldl as F
import System.FilePath (pathSeparator, takeFileName, takeBaseName)
import Data.Maybe ( catMaybes ) 

data Tree a =
      File { path :: a }
    | Directory { path :: a, children :: [Tree a] }
    deriving Foldable

isFile :: Tree a -> Bool
isFile (File _) = True
isFile (Directory _ _) = False

isDirectory :: Tree a -> Bool
isDirectory (Directory _ _) = True
isDirectory (File _) = False

-- stole from containers
foldTree :: (a -> b) -> (a -> [b] -> b) -> Tree a -> b
foldTree f g = go where
    go (File x) = f x
    go (Directory x ts) = g x (map go ts)

filterTree :: (a -> Bool) -> (a -> [Tree a] -> Bool) ->
              Tree a -> Maybe (Tree a)
filterTree ffile fdir = foldTree filterFile filterDir where
  filterFile x = if ffile x then Just (File x) else Nothing
  filterDir x maybes = let children = catMaybes maybes in
    if fdir x children then Just (Directory x children) else Nothing

headTree :: Tree a -> a
headTree (File x) = x
headTree (Directory x _) = x

sortTree :: Ord a => Tree a -> Tree a
sortTree = foldTree File 
  (\x ts -> Directory x (sortOn headTree ts))

type FileTree = Tree FilePath

filePath :: FileTree -> FilePath
filePath = headTree

fileName :: FileTree -> FilePath
fileName x = takeFileName (filePath x)

fileBaseName :: FileTree -> FilePath
fileBaseName x = takeBaseName (filePath x)

lsTree :: FilePath -> IO FileTree
lsTree src = do
  isDir <- testdir src
  isFile <- testfile src
  if isDir then do
    childPaths <- fold (ls src) F.list
    children <- mapM lsTree childPaths
    return $ Directory src children
  else if isFile then
    return $ File src
  else error "Neither a directory or file"

-- Quoting Turtle.Prelude.cptree ::
  -- The `system-filepath` library treats a path like "/tmp" as a file and not
  -- a directory and fails to strip it as a prefix from `/tmp/foo`.  Adding
  -- `(</> "")` to the end of the path makes clear that the path is a
  -- directory
stripPrefixDir :: FilePath -> FilePath -> Maybe FilePath
stripPrefixDir dir path =
  let attempt = stripPrefix dir path
      fallback = stripPrefix (dir <> [System.FilePath.pathSeparator]) path in
  attempt <|> fallback

{-
mapCpTree f srcTree dst

Copies the srcTree to dst. But instead of directly copying the files, 
applies f to generate the target file.

f srcSubTree dst takes the source subtree, target location (whether it be a file or directory)
and does some action.

Tree construction is done from root to leaves, but f is applied from leaves to root.
-}
mapCpTree :: (FileTree -> FilePath -> IO ()) -> FileTree -> FilePath -> IO ()
mapCpTree f srcTree = mapCpTree' f (filePath srcTree) srcTree

mapCpTree' :: (FileTree -> FilePath -> IO ()) -> FilePath -> FileTree -> FilePath -> IO ()
mapCpTree' f srcRoot srcSubTree dst = 
  let srcSubPath = filePath srcSubTree
      Just relPath = stripPrefixDir srcRoot srcSubPath
      dstPath = dst </> relPath in
  case srcSubTree of
    File _ -> f srcSubTree dstPath
    Directory srcPath children -> do
      mktree dstPath
      mapM_ (\t -> mapCpTree' f srcRoot t dst) children
      f srcSubTree dstPath