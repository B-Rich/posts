import Control.Arrow
import Text.BlogLiterately

import Text.Pandoc (Pandoc(..), Block (RawBlock))
import Data.List

debirdXF :: Transform
debirdXF = Transform (const (arr removeAllTracks)) (const True)

removeAllTracks :: Pandoc -> Pandoc
removeAllTracks (Pandoc m blocks) =
    Pandoc m $ map removeBirdTracks blocks

replace :: (Eq a) => [a] -> [a] -> [a] -> [a]
replace [] _ _ = []
replace l@(x:xs) i j = if i `isPrefixOf` l
                       then j ++ replace (drop (length i) l) i j
                       else x : replace xs i j

removeBirdTracks :: Block -> Block
removeBirdTracks (RawBlock "html" src) =
  let src'  = (replace src "<span style=\"\">&gt;</span>"
                       "<span style=\"\"></span>")
  in RawBlock "html" src'
removeBirdTracks b = b

main :: IO ()
main = blogLiteratelyCustom [ ghciXF
                            , imagesXF
                            , highlightXF
                            , debirdXF
                            , centerImagesXF ]

-- literate.hs ends here
