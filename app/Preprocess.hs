module Preprocess where
import Data.Text (Text)
import qualified Data.Text as T

filterInlineComment :: Text -> Text
filterInlineComment line =
  let tokens = T.splitOn "%%" line in
  -- get odd-indexed ones
  let getOdd list = [t | (t, True) <-
                          zip list (cycle [True, False])] in
  T.concat $ getOdd tokens

filterComments :: Text -> Text
filterComments text =
  let lines = T.lines text in
  let loop _ [] = []
      loop True ("%%" : tl) = loop False tl
      loop True (hd : tl) = hd : loop True tl
      loop False ("%%" : tl) = loop True tl
      loop False (hd : tl) = loop False tl in
  let noBlockLines = loop True lines in
  T.unlines (map filterInlineComment noBlockLines)