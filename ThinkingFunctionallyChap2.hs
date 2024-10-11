import Data.Char (toUpper)

-- Exercise C
modernise :: String -> String
modernise = unwords . map (\(c : cs) -> toUpper c : cs) . words
