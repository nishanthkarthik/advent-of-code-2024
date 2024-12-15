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

data V2 a = V2 a a deriving (Eq, Ord, Show, Functor)

instance Num a => Num (V2 a) where
    (+) (V2 a b) (V2 c d) = V2 (a + c) (b + d)
    (*) (V2 a b) (V2 c d) = V2 (a * c) (b * d)
    abs v = abs <$> v
    signum v = signum <$> v
    fromInteger a = fromInteger <$> V2 a a
    negate v = negate <$> v

type V2i = V2 Int
