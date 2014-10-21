import TrelloParse (parseTrello)

import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main = do
    input <- T.getContents
    let result = parseTrello "stdin" input
    case result of
        Left failure -> do
            T.putStr input
            putStr "\n\n\n"
            putStrLn "Warning: Parse Failed."
            print failure
        Right output ->
            T.putStr output
