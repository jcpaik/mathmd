module Process where
import Text.Pandoc hiding (trace, FileTree)
import Text.Pandoc.Walk (walk)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Preprocess (filterComments)
import ReadWrite (readText)
import Turtle (FilePath, testdir)
import Control.Monad ((>=>))
import Debug.Trace (trace, traceShow, traceShowId)
import Data.Maybe (isJust)
import FileTree
import Summary
import System.FilePath ((</>), replaceExtension, takeBaseName, takeFileName, dropExtension, takeDirectory, splitDirectories)
import Data.List (isPrefixOf)
import Data.List (isSuffixOf)
import Data.List (sortOn)
import Text.Pandoc.Shared (stringify)
import Data.Char (isAlpha, isAsciiUpper)

data TheoremEnvType =
  Theorem | Lemma | Definition | Corollary |
  Conjecture | Remark | Proposition | FigureEnv | EquationEnv
  deriving (Eq, Enum, Show)

-- | Represents a single theorem environment similar to the one in LaTeX.
data TheoremEnv = TheoremEnv
    {
      theoremEnvType        :: TheoremEnvType
    , theoremEnvTag         :: Text
    , theoremEnvDescription :: [Block]
    }
  deriving Show

{- | A processor takes a tree of markdown files and its Pandoc markdown AST 
and transpiles it to a target (e.g. LaTeX or mdbook).

The order of transpilation is:
1. Pre-process markdown file with `processPreprocess`
2. Pandoc reader converts the file to a Pandoc AST
3. Detects and converts theorem environment in AST
4. Convert each inline (image, link, equation) of AST
5. Write the final AST using a Pandoc writer
-}
data Processor = Processor
    {
      processPreprocess    :: Text -> Text
    -- ^ Pre-processes each markdown file as text before being parsed by Pandoc.
    , processEquation      :: MathType -> Text -> Inline
    -- ^ Process each LaTeX equation
    , processTheoremEnv    :: TheoremEnv -> [Block]
    , processImage         :: Attr -> [Inline] -> Target -> Inline
    , processLink          :: Attr -> [Inline] -> Target -> Inline
    , processFile          :: Pandoc -> PandocIO Text
    -- ^ Writes the converted AST using a Pandoc writer
    , processDirectory     :: FileTree -> Maybe Text
    -- ^ (Optionally) generates a file for each destination folder
    , processSummary       :: FileTree -> FilePath -> IO ()
    -- ^ Given the whole source file tree, generate some summary file
    -- e.g. A `SUMMARY.md` file for mdbook, and a `main.tex` file for LaTex
    , processExtension     :: String
    -- ^ Extension of transpiled files
    }

theoremEnvTypeText :: TheoremEnvType -> Text
theoremEnvTypeText t =
  case t of
    Theorem -> "Theorem"
    Lemma -> "Lemma"
    Definition -> "Definition"
    Corollary -> "Corollary"
    Conjecture -> "Conjecture"
    Remark -> "Remark"
    Proposition -> "Proposition"
    FigureEnv -> "Figure"

theoremEnvTypeTagText :: TheoremEnvType -> Text
theoremEnvTypeTagText t =
  case t of
    Theorem -> "thm"
    Lemma -> "lem"
    Definition -> "def"
    Corollary -> "cor"
    Conjecture -> "con"
    Remark -> "rem"
    Proposition -> "pro"
    FigureEnv -> "fig"
    EquationEnv -> "eqn"

-- "Theorem" -> Just Theorem
parseTheoremEnvType :: Text -> Maybe TheoremEnvType
parseTheoremEnvType text =
  case text of
    "Theorem" -> Just Theorem
    "Lemma" -> Just Lemma
    "Definition" -> Just Definition
    "Corollary" -> Just Corollary
    "Conjecture"-> Just Conjecture
    "Remark" -> Just Remark
    "Proposition" -> Just Proposition
    "Figure" -> Just FigureEnv
    _ -> Nothing

-- "[some-thm-name]." -> Just "some-thm-name"
parseTheoremEnvTag :: Text -> Maybe Text
parseTheoremEnvTag = T.stripPrefix "[" >=> T.stripSuffix "]."

-- "#^thm-some-theorem" -> ("", Theorem, "some-theorem")
-- "05. Definitions/asdf/asdf#^thm-asdf" -> ("05. Definitions/asdf/asdf", Theorem, "asdf")
parseTheoremEnvLink :: Text -> Maybe (Text, TheoremEnvType, Text)
parseTheoremEnvLink text =
  if length tokens /= 2 then Nothing else do
      thmType <- thmTypeMaybe
      return (addr, thmType, tagBody) where
        tokens = T.splitOn "#^" text
        [addr, tag] = tokens
        (tagHead, tagBody) = T.splitAt 4 tag
        thmTypeMaybe = case tagHead of
          "thm-" -> Just Theorem
          "lem-" -> Just Lemma
          "def-" -> Just Definition
          "cor-" -> Just Corollary
          "con-" -> Just Conjecture
          "rem-" -> Just Remark
          "pro-" -> Just Proposition
          "fig-" -> Just FigureEnv
          "eqn-" -> Just EquationEnv
          _ -> Nothing

clearTags :: [Inline] -> [Inline]
clearTags [] = []
clearTags [Str str] | T.isPrefixOf "^" str = []
clearTags [x] = [x]
clearTags (Space : Str str : rest) | T.isPrefixOf "^" str = rest
clearTags (x : y) = x : clearTags y

theoremEnv :: Block -> Maybe TheoremEnv
theoremEnv (BlockQuote
  (Para ((Strong [ Str typeStr , Space , Str nameStr ]) : Space :
    restFirstPara) : restBlocks)) = do
      envType <- traceShowId $ parseTheoremEnvType typeStr
      envTag <- parseTheoremEnvTag nameStr
      Just TheoremEnv
        {
          theoremEnvType = envType
        , theoremEnvTag = envTag
        , theoremEnvDescription = tagRemoved
        }
      where
        quoted = Para restFirstPara : restBlocks
        tagRemoved = walk clearTags quoted
theoremEnv _ = Nothing

-- detects latex environments (align, equation, gather, etc.)
-- that is wrongly trapped inside $$'s due to Obsidian
trappedEnvNames :: [Text]
trappedEnvNames = [
    "align", "align*",
    "equation", "equation*",
    "gather", "gather*",
    "alignat", "alignat*"
  ]

-- note that there are other envs (bmatrix, aligned, etc.) that should be in equation
isTrappedEnv :: Text -> Bool
isTrappedEnv math =
  let math' = T.strip math in
  or [T.isPrefixOf ("\\begin{" <> envName <> "}") math' | envName <- trappedEnvNames]

injectTagToEquation :: Text -> Text -> Inline
injectTagToEquation tag math | isTrappedEnv math =
  let
    hd : tl = T.lines math
    newMath = T.unlines ("" : hd : ("\\label{eqn:" <> tag <> "}") : tl)
  in
    RawInline "tex" newMath
injectTagToEquation tag math =
  let
    header = "\\begin{equation}"
    tagLine = "\\label{eqn:" <> tag <> "}"
    footer = "\\end{equation}"
    newMath = T.unlines ["", header, tagLine, math, footer]
  in
    RawInline "tex" newMath

addEquationTag :: [Inline] -> [Inline]
addEquationTag (Space : Str tag : SoftBreak : Math DisplayMath math : tl) |
  T.isPrefixOf "^eqn-" tag =
    injectTagToEquation (T.drop 5 tag) (T.strip math) : addEquationTag tl
addEquationTag (hd : tl) = hd : addEquationTag tl
addEquationTag [] = []

processPandoc :: Processor -> Pandoc -> Pandoc
processPandoc (Processor {
    processEquation = procEq
  , processImage = procImg
  , processLink = procLink
  , processTheoremEnv = procTheoremEnv }) =
  walk procInlines . walk addEquationTag . walk blockWalker where
    procInlines (Math t txt) = procEq t txt
    procInlines (Image attr desc target) =
      procImg attr desc target
    procInlines (Link attr desc target) =
      procLink attr desc target
    procInlines x = x

    blockWalker blocks = [outBlock |
      block <- blocks, outBlock <- expandBlock block]
    expandBlock block = maybe [block]
      procTheoremEnv (theoremEnv block)

processFileWithPreprocess :: Processor -> Text -> IO Text
processFileWithPreprocess processor text =
  let filtered = processPreprocess processor text in runIOorExplode (do
    readPandoc <- readText filtered
    let writePandoc = processPandoc processor readPandoc in
      processFile processor writePandoc)

processFileTree :: Processor -> FileTree -> FilePath -> IO ()
processFileTree processor (File filePath) dst = do
  text <- TIO.readFile $ traceShowId filePath
  processed <- processFileWithPreprocess processor text
  TIO.writeFile (replaceExtension dst (processExtension processor)) processed
processFileTree processor (Directory dirPath children) dst = do
  dstTree <- lsTree (traceShowId dst)
  case processDirectory processor $ sortTree dstTree of
    Nothing -> return ()
    Just text -> TIO.writeFile (dst <> (processExtension processor)) text

-- Main function that transpiles a single file or directory
transpile :: Processor -> FilePath -> FilePath -> IO ()
transpile processor src dst = do
  isSrcDir <- testdir src
  if isSrcDir then do
    srcTreeRaw <- lsTree src
    let srcTree = genSummaryTree srcTreeRaw in do
      mapCpTree (processFileTree processor) srcTree dst
      processSummary processor srcTree dst
  else do
    srcTreeRaw <- lsTree src
    mapCpTree (processFileTree processor) srcTreeRaw dst

mdBookProcessor :: Processor
mdBookProcessor = Processor
  {
    processPreprocess = filterComments
  , processEquation = mdBookProcessEquation
  , processTheoremEnv = mdBookProcessTheoremEnv
  , processImage = Image
  , processLink = Link
  , processFile = mdBookProcessFile
  , processDirectory = const Nothing
  , processSummary = mdBookProcessSummary
  , processExtension = ".md"
  }

-- mdbook only understands things well when all the characters are escaped
mdBookProcessEquation :: MathType -> Text -> Inline
mdBookProcessEquation t txt =
  Math t (escapeAllSymbols txt) where
  escapeAllSymbols text =
    let escapes = ["{", "}", "_", "\\"]
        add ch = T.replace ch ("\\" <> ch)
        maps = map add escapes in
    foldr ($) text maps

mdBookTheoremEnvTypeText :: TheoremEnvType -> Text
mdBookTheoremEnvTypeText t =
  case t of
    Theorem -> "Theorem"
    Lemma -> "Lemma"
    Definition -> "Definition"
    Corollary -> "Corollary"
    Conjecture -> "Conjecture"
    Remark -> "Remark"
    Proposition -> "Proposition"
    FigureEnv -> "Figure"

mdBookProcessTheoremEnv :: TheoremEnv -> [Block]
mdBookProcessTheoremEnv (TheoremEnv
  { theoremEnvType = typ
  , theoremEnvTag = tag
  , theoremEnvDescription = desc }) =
    return $ BlockQuote (firstPara : restBlocks) where
      Para restFirstPara : restBlocks = desc
      typeText = theoremEnvTypeText typ
      tagText = "[" <> tag <> "]."
      header = Strong [ Str typeText , Space , Str tagText ]
      firstPara = Para (header : Space : restFirstPara)

mdBookProcessFile :: Pandoc -> PandocIO Text
mdBookProcessFile = writeMarkdown options where
  options = def {
    writerExtensions = extensionsFromList [
      Ext_tex_math_double_backslash,
      Ext_raw_html,
      Ext_strikeout
    ],
    writerWrapText = WrapPreserve
  }

mdBookProcessSummary :: FileTree -> FilePath -> IO ()
mdBookProcessSummary tree dst =
  let txt = mdBookGenSummary tree dst in
    TIO.writeFile (dst </> "SUMMARY.md") txt

-- From the summary tree, generate contenst of SUMMARY.md
mdBookGenSummary :: FileTree -> FilePath -> Text
mdBookGenSummary tree dst = T.unlines lines where
  Directory src roots = tree
  lines = [line | root <- roots, line <- loop 0 root]

  replaceTop dir =
    let Just relDir = stripPrefixDir src dir in relDir
  loop depth (File path) = [displayMarkdownFile depth $ replaceTop path]
  loop depth (Directory path children) =
    -- TODO: Locate preface if any
    displayDir depth (replaceTop path) :
      [line | child <- children, line <- loop (depth + 1) child]

latexProcessor :: Processor
latexProcessor = Processor
  {
    processPreprocess = filterComments
  , processEquation = latexProcessEquation
  , processTheoremEnv = latexProcessTheoremEnv
  , processImage = latexProcessImage
  , processLink = latexProcessLink
  , processFile = latexProcessFile
  , processDirectory = latexProcessDirectory
  , processSummary = \a b -> return ()
  , processExtension = ".tex"
  }

latexProcessEquation :: MathType -> Text -> Inline
latexProcessEquation DisplayMath txt |
  isTrappedEnv txt = RawInline "tex" $ T.strip txt
latexProcessEquation mt t = Math mt t

latexTheoremEnvTypeName :: TheoremEnvType -> Text
latexTheoremEnvTypeName t =
  case t of
    Theorem -> "theorem"
    Lemma -> "lemma"
    Definition -> "definition"
    Corollary -> "corollary"
    Conjecture -> "conjecture"
    Remark -> "remark"
    Proposition -> "proposition"
    FigureEnv -> "figure"
    EquationEnv -> "equation"

latexTheoremEnvTypeHeader :: TheoremEnvType -> Text
latexTheoremEnvTypeHeader t =
  case t of
    Theorem -> "thm:"
    Lemma -> "lem:"
    Definition -> "def:"
    Corollary -> "cor:"
    Conjecture -> "con:"
    Remark -> "rem:"
    Proposition -> "pro:"
    FigureEnv -> "fig:"
    EquationEnv -> "eqn:"

latexProcessTheoremEnv :: TheoremEnv -> [Block]
-- Handle figures separately
latexProcessTheoremEnv (TheoremEnv FigureEnv envTag envDesc) =
  [Para $ [envStart] ++ [image] ++ caption ++ [envEnd]] where
    envStartTex = "\\begin{figure}\n\\centering\n"
    envStart = RawInline "tex" envStartTex

    -- Figure environment should have two paragraphs,
    -- one for description and one for image
    [Para inlines, Para [image@(Image _ _ _)]] = envDesc
    caption = [RawInline "tex" "\n\\caption{"] ++
      inlines ++
      [RawInline "tex" "}\n"]

    label = "\\label{fig:" <> envTag <> "}\n"
    envEndTex = label <> "\\end{figure}"
    envEnd = RawInline "tex" envEndTex
latexProcessTheoremEnv TheoremEnv
  {
    theoremEnvType = envType
  , theoremEnvTag = envTag
  , theoremEnvDescription = envDesc
  } = [envStart] ++ envDesc ++ [envEnd] where
    envName = latexTheoremEnvTypeName envType
    envStart = Plain [RawInline "tex" $ "\\begin{" <> envName <> "}"]
    envLabel = "\\label{" <> theoremEnvTypeTagText envType <> ":" <>
               envTag <> "}\n"
    envEndTex = envLabel <> "\\end{" <> envName <> "}"
    envEnd = Plain [RawInline "tex" envEndTex]

latexProcessImage :: Attr -> [Inline] -> Target -> Inline
latexProcessImage _ [Str attrStr] (path, "") |
  T.last attrStr == '%' =
  Image attr [] (path, "") where
    attr = ( "" , [] , [ ( "width" , attrStr ) ] )
-- Change .svg file to .pdf file if there is no size specification
latexProcessImage _ _ (path, "") |
  T.takeEnd 4 path == ".svg" =
  Image attr [] (T.dropEnd 4 path <> ".pdf", "") where
    attr = ( "" , [] , [] )
latexProcessImage a i t = Image a i t

latexProcessLink :: Attr -> [Inline] -> Target -> Inline
-- [[target]]
latexProcessLink attr desc (target, "wikilink") |
  stringify desc == target =
    latexProcessSingleLink target
-- [[target|desc]]
latexProcessLink attr desc (target, "wikilink") =
  RawInline "tex" $ stringify desc <>
  " (" <> stringify (latexProcessSingleLink target) <> ")"
-- Leave rest intact
latexProcessLink attr desc target = Link attr desc target

latexProcessSingleLink :: Text -> Inline
-- [[#^thm-something]]
latexProcessSingleLink link | isJust (parseTheoremEnvLink link) =
  let Just (path, envType, envName) = parseTheoremEnvLink link
      thmTypeText = theoremEnvTypeText envType
      thmRefText = theoremEnvTypeTagText envType <> ":" <> envName in
      RawInline "tex" $ "\\Cref{" <> thmRefText <> "}"
-- [[@citation]]
latexProcessSingleLink link | T.head link == '@' =
  RawInline "tex" $ "\\cite{" <> T.tail link <> "}"
-- [[filename.md]] or [[filename]]
latexProcessSingleLink link =
  RawInline "tex" $ "\\Cref{" <> latexLabelOfFile (T.unpack link) <> "}"

latexProcessFile :: Pandoc -> PandocIO Text
latexProcessFile pd = writeLaTeX options mpd where
  options = def {
    writerExtensions = extensionsFromList [
      Ext_tex_math_dollars,
      Ext_raw_html,
      Ext_raw_tex,
      Ext_strikeout
    ],
    writerCiteMethod = Biblatex,
    writerWrapText = WrapPreserve
  }
  mpd = walk modHeader $ walk modProof $ walk modProofOption pd
  modHeader (Header n t xs) = Header (n+2) t xs
  modHeader x = x
  modProof (Emph [Str "Proof."]) = RawInline "tex" "\\begin{proof}\n"
  modProof (Str "□") = RawInline "tex" "\n\\end{proof}"
  modProof x = x
  modProofOption x@(Emph [Str "Proof."] : Space : Str "(of" : rest) =
    let
      isClosingInline (Str str) = T.last str == ')'
      isClosingInline _ = False
      (inBracket, outBracket) = break isClosingInline rest
    in
      case outBracket of
        (Str str : Space : other) ->
          [RawInline "tex" "\\begin{proof}[Proof of"] ++
          inBracket ++ [Str (T.init str)] ++
          [RawInline "tex" "]\n"] ++ other
        _ -> x
  modProofOption x = x

latexSections :: [Text]
latexSections = ["chapter", "section", "subsection", "subsubsection"]

-- `01. Section Title/00. Preface.md` => `section-title`
-- `01. Section Title/00. Preface` => `section-title`
-- `01. Section Title/01. Subsection Title.md` => `subsection-title`
-- `01. Section Title/01. Subsection Title` => `subsection-title`
-- TODO: come up with a better labeling strategy
latexLabelOfFile :: String -> Text
latexLabelOfFile p =
  let
    path = takeFileName $ dropPreface p
    makeTitle t = T.replace " " "-" (T.toLower $ T.drop 4 $ T.pack t)
  in
    "sec:" <> makeTitle path

dropPreface :: String -> String
dropPreface p
  | "00. " `isPrefixOf` takeFileName p = takeDirectory p
  | ".tex" `isSuffixOf` p || ".md" `isSuffixOf` p = dropExtension p
  | otherwise = p

-- File name to the corresponding line in latex
-- `00. something.tex` => `\\input{00. something.tex}`
-- `AA/BB/03. something.tex` => `\\subsection{something}\n\\input{AA/BB/03. something.tex}`

latexLineOfFile :: String -> Text
-- Zero indexed -> no header
latexLineOfFile p =
  let
    sectionName = latexSections !! (length (splitDirectories p) - 2)
    sectionTitle =
        T.replace " Q" " $\\mathcal{Q}$" $ -- hack for one document
        T.dropWhile (\c -> c == '_' || c == ' ') $
        T.drop 4 $
        T.pack (takeBaseName p)
    sectionHeader = ("\\" <> sectionName <> "{" <> sectionTitle <> "}" :: Text)

    sectionTag = latexLabelOfFile p
    -- TODO: change label head according to section/subsection/etc.
    sectionLabel = ("\\label{" <> sectionTag <> "}" :: Text)
    sectionInput = ("\\input{" <> T.pack p <> "}" :: Text)
  in
    if "00. " `isPrefixOf` takeBaseName p
      then sectionInput
      else T.unlines [sectionHeader, sectionLabel, sectionInput]

latexProcessDirectory :: FileTree -> Maybe Text
latexProcessDirectory (Directory dirPath children) = Just $
  T.unlines l where
    childPaths = filter (isSuffixOf ".tex") $ map filePath children
    isAppendixPath p = isAsciiUpper $ head (takeBaseName p)
    (sectionPaths, appendixPaths) = break isAppendixPath childPaths
    l = map latexLineOfFile sectionPaths ++
      if null appendixPaths then []
      else "\\appendix" : map latexLineOfFile appendixPaths