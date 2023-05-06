module Summary where
import FileTree
import Data.Char (isDigit)
import Data.List (isSuffixOf)
import System.FilePath ((</>), takeFileName, dropExtension)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Network.URI.Encode (encodeTextWith)

-- Assuming that FilePath = String = [Char]
indexed :: FilePath -> Bool
indexed x = indexed' (takeFileName x) where
  indexed' (d0 : d1 : '.' : ' ' : _) | isDigit d0 && isDigit d1 = True
  indexed' _ = False

isMarkdown :: FilePath -> Bool
isMarkdown x = ".md" `isSuffixOf` takeFileName x

displayName :: FilePath -> Text
displayName path =
  let fname = T.pack $ dropExtension $ takeFileName path in
  T.drop 4 fname

displayMarkdownFile :: Int -> FilePath -> Text
displayMarkdownFile depth path =
  -- replace path with any whitespace with %20 and such
  let uri = encodeTextWith (/= ' ') $ T.pack path in
  -- repeat according to level
  let indent = T.pack $ replicate (4 * depth) ' ' in
  indent <> "- [" <> displayName path <> "](" <> uri <> ")"

displayDir :: Int -> FilePath -> Text
displayDir depth path =
  -- repeat according to level
  let display = T.drop 4 $ T.pack $ takeFileName path in
  let indent = T.pack $ replicate (4 * depth) ' ' in
  indent <> "- [" <> display <> "](" <> "blank.md" <> ")"

-- Filter a FileTree with proper naming and sort the filenames
genSummaryTree :: FileTree -> FileTree
genSummaryTree tree = sorted where
  root = filePath tree
  filterFile x = indexed x && isMarkdown x
  filterDir x _ = x == root || indexed x
  Just filtered = filterTree filterFile filterDir tree
  sorted = sortTree filtered

-- From the summary tree, generate contenst of SUMMARY.md
genSummary :: FileTree -> FilePath -> Text
genSummary tree dst = T.unlines lines where
  Directory src roots = tree
  lines = [line | root <- roots, line <- loop 0 root]

  replaceTop dir =
    let Just relDir = stripPrefixDir src dir in relDir
  loop depth (File path) = [displayMarkdownFile depth $ replaceTop path]
  loop depth (Directory path children) =
    -- TODO: Locate preface if any
    displayDir depth (replaceTop path) :
      [line | child <- children, line <- loop (depth + 1) child]