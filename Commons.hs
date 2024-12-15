module Commons where

import qualified Data.Attoparsec.Text as At
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)

inp :: At.Parser a -> IO a
inp parser = do
    input <- getArgs >>= TIO.readFile . head
    let result = At.eitherResult $ At.feed (At.parse parser input) mempty
    case result of
        Left l -> error l
        Right r -> return r
