module Main where
import Negatron
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import System.Exit
import System.Environment

exit :: Int -> IO ()
exit c
    | c == 0    = exitWith (ExitSuccess)
    | otherwise = exitWith (ExitFailure c)

main :: IO ()
main = do
    args <- getArgs
    let fileName = if length args >= 1
        then args !! 0
        else "test.nega"
    program <- T.readFile fileName
    let parseTree = Right $ parse . alexScanTokens $ T.unpack program
    case parseTree of
        Left  err -> putStrLn $ err
        Right ast -> do
            returnCode <- peval ast
            exit returnCode
        -- Right ast -> putStrLn $ show ast

