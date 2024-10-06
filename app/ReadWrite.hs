module ReadWrite where
import Text.Pandoc
import Text.Pandoc.Class (trace)
import Text.Pandoc.Walk (walk)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

readerOptions :: ReaderOptions
readerOptions = def {
  readerExtensions = extensionsFromList [
    Ext_citations,
    Ext_fancy_lists,
    -- ^ upper/lower/Roman numerals
    Ext_tex_math_dollars,
    Ext_raw_html,
    Ext_raw_tex,
    Ext_strikeout,
    Ext_wikilinks_title_after_pipe,
    Ext_footnotes,
    Ext_smart,
    Ext_pipe_tables
  ]
}

readText :: Text -> PandocIO Pandoc
readText = readMarkdown readerOptions

