{-# LANGUAGE RecordWildCards #-}

import System.Console.CmdArgs
import Control.Arrow
import Text.BlogLiterately

import qualified System.IO.UTF8 as U (readFile)
import Text.Pandoc
import Data.List
import Text.Blaze.Html.Renderer.String (renderHtml)

replace :: (Eq a) => [a] -> [a] -> [a] -> [a]
replace [] _ _ = []
replace l@(x:xs) i j = if i `isPrefixOf` l
                       then j ++ replace (drop (length i) l) i j
                       else x : replace xs i j

debirdXF :: Transform
debirdXF = Transform (const (arr removeAllTracks)) (const True)

removeAllTracks :: Pandoc -> Pandoc
removeAllTracks (Pandoc m blocks) =
    Pandoc m $ map removeBirdTracks blocks

removeBirdTracks :: Block -> Block
removeBirdTracks (RawBlock "html" src) =
  let src'  = (replace src "<span style=\"\">&gt;</span>"
                       "<span style=\"\"></span>")
  in RawBlock "html" src'
removeBirdTracks b = b

hidecodeXF :: Transform
hidecodeXF = Transform (const (arr hideCodeBlocks)) (const True)

hideCodeBlocks :: Pandoc -> Pandoc
hideCodeBlocks (Pandoc m blocks) =
    Pandoc m $ map hiddenBlocks blocks

hiddenBlocks :: Block -> Block
hiddenBlocks b@(CodeBlock _ src) =
  if "-- HIDE" `isInfixOf` src then Null else b
hiddenBlocks b = b

fixHeaders :: String -> String
fixHeaders [] = []
fixHeaders ('\\':'#':cs) = '#':cs
fixHeaders (c:cs) = c:fixHeaders cs

xformDoc' :: BlogLiterately -> [Transform] -> (String -> IO String)
xformDoc' bl xforms = runKleisli $
       arr     fixHeaders
    >>> arr     (readMarkdown parseOpts)

    >>> runTransforms xforms bl

    >>> arr     (writeHtml writeOpts)
    >>> arr     renderHtml
  where
    writeOpts = defaultWriterOptions
                { writerReferenceLinks = True }
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
                             , hidecodeXF
                             , highlightXF
                             , debirdXF
                             , centerImagesXF ]

-- literate.hs ends here
