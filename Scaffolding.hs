import System.Environment (getArgs)
import System.IO (writeFile, readFile)
import System.Directory (createDirectoryIfMissing)
import Text.Printf (printf)

main :: IO ()
main = do
    pwd <- head <$> getArgs
    day <- (!! 1) <$> getArgs
    createDirectoryIfMissing True (printf "%s/inputs/%s" pwd day)
    writeFile (printf "%s/inputs/%s/input.txt" pwd day) "replace-me\n"
    writeFile (printf "%s/inputs/%s/test.txt" pwd day) "replace-me\n"
    readFile "deps/DayTemplate.hs" >>= writeFile (printf "%s/Day%s.hs" pwd day)
