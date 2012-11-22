{-# LANGUAGE RecordWildCards #-}

import           System.Console.CmdArgs
import           Control.Arrow
import           Text.BlogLiterately
import           Data.List
import qualified System.IO.UTF8 as U (readFile)
import           Text.Blaze.Html.Renderer.String (renderHtml)
import           Text.Pandoc

-- | Replace a sublist with another list.
replace :: (Eq a) => [a] -> [a] -> [a] -> [a]
replace [] _ _ = []
replace l@(x:xs) i j = if i `isPrefixOf` l
                       then j ++ replace (drop (length i) l) i j
                       else x : replace xs i j

-- | Map over all blocks in the Pandoc document.
mapBlocks :: (Block -> Block) -> Pandoc -> Pandoc
mapBlocks f (Pandoc m blocks) = Pandoc m $ map f blocks

-- | Create a BlogLiterally transformer arrow that maps over all blocks in the
--   document.
makeXF :: (Block -> Block) -> Transform
makeXF f = Transform (const . arr . mapBlocks $ f) (const True)

-- | Remove Literate Haskell's bird-tracks from the output.  I prefer not to see
--   them.
removeBirdTracks :: Block -> Block
removeBirdTracks (RawBlock "html" src) =
  RawBlock "html"
    $ (\x -> replace x "\n<span style=\"color: gray;\">ghci"
                      "<span style=\"color: gray;\">ghci")
    $ (\x -> replace x "<span class=\"fu\"></span><span class=\"ot\"> "
                      "<span class=\"fu\"></span><span class=\"ot\">")
    $ (\x -> replace x "<span class=\"fu\">&gt;</span>"
                      "<span class=\"fu\"></span>")
    $ (\x -> replace x "<span class=\"fu\">&gt;</span> "
                      "<span class=\"fu\"></span>")
    $ src
removeBirdTracks b = b

-- | If a code block contains "-- HIDE" anywhere within it, drop that block
--   from the output.
hiddenBlocks :: Block -> Block
hiddenBlocks b@(CodeBlock _ src) =
  if "-- HIDE" `isInfixOf` src then Null else b
hiddenBlocks b = b

-- | If any line that begins with "\#", change it to "#".  This is necessary
--   because Markdown uses # for headers, but Literate Haskell doesn't accept
--   them.
fixHeaders :: String -> String
fixHeaders = unlines . map (\l -> if "\\#" `isPrefixOf` l
                                  then drop 1 l else l) . lines

fixupTables :: String -> String
fixupTables txt = replace txt "<table>"
                              "<table style=\"width: 70%; margin: 20px\">"

-- | This and the next function replace the versions provided by BlogLiterally
--   so that I can run `fixHeaders` before `readMarkdown`.
xformDoc' :: BlogLiterately -> [Transform] -> (String -> IO String)
xformDoc' bl xforms = runKleisli $
     arr     fixLineEndings
  >>> arr     fixHeaders
  >>> arr     (readMarkdown parseOpts)

  >>> runTransforms xforms bl

  >>> arr     (writeHtml writeOpts)
  >>> arr     renderHtml
  >>> arr     fixupTables
  where
    writeOpts = defaultWriterOptions
                { writerReferenceLinks = True
                , writerHTMLMathMethod =
                  MathJax "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML" }
    parseOpts = defaultParserState
                { stateLiterateHaskell = True }

blogLiteratelyCustom' :: [Transform] -> IO ()
blogLiteratelyCustom' ts = do
  bl <- cmdArgs blOpts
  let (BlogLiterately{..}) = bl

  prefs <- getStylePrefs style
  let hsHighlight' = case hsHighlight of
          HsColourInline _ -> HsColourInline prefs
          _                -> hsHighlight
      bl' = bl { hsHighlight = hsHighlight' }

  let bl'' = bl' { password = password }

  html <- xformDoc' bl'' ts =<< U.readFile file
  postIt bl'' html

main :: IO ()
main = blogLiteratelyCustom' [ ghciXF
                             , imagesXF
                             , makeXF hiddenBlocks
                             , highlightXF
                             , makeXF removeBirdTracks
                             , centerImagesXF ]

-- Main.hs ends here
