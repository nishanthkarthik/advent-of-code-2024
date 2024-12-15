import System.Environment (getArgs)
import System.IO (writeFile)
import System.Directory (createDirectoryIfMissing)
import Text.Printf (printf)

main :: IO ()
main = do
    pwd <- head <$> getArgs
    day <- (!! 1) <$> getArgs
    createDirectoryIfMissing True (printf "%s/inputs/%s" pwd day)
    writeFile (printf "%s/inputs/%s/input.txt" pwd day) "replace-me\n"
    writeFile (printf "%s/inputs/%s/test.txt" pwd day) "replace-me\n"
    writeFile (printf "%s/Day%s.hs" pwd day) "\n\nmain :: IO ()\n"
    appendFile (printf "%s/Day%s.hs" pwd day) "main = do\n"
    appendFile (printf "%s/Day%s.hs" pwd day) "    return ()\n"
