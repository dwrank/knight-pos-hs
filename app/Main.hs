module Main (main) where

import System.Environment
import Data.Char (isDigit)
import Control.Monad (when)

import KnightPos

parsePos :: String -> Maybe KnightPos
parsePos arg = do
    let s = filter (\x -> isDigit x) arg
    let l = map (\x -> read [x]::Int) $ s
    return (head l, head $ tail l)

run :: Maybe KnightPos -> Maybe KnightPos -> [KnightPosChain]
run Nothing _ = []
run _ Nothing = []
run (Just start) (Just end) = findMoves start end

main :: IO ()
main = do
    args <- getArgs
    when (length args > 1) $ do
        let start = parsePos $ args !! 0
        let end = parsePos $ args !! 1
        print $ run start end
