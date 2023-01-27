module KnightPos
    (
       KnightPos,
       KnightPosChain,
       findMoves
    ) where

import Control.Monad (guard)

    {--
type KnightPos = (Int, Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c, r) = do
    (c', r') <- [(c+2,r+1),(c+2,r-1),(c-2,r+1),(c-2,r-1),
                 (c+1,r+2),(c+1,r-2),(c-1,r+2),(c-1,r-2)]
    guard (c' `elem` [1..8] && r' `elem` [1..8])
    return (c', r')

in3 :: KnightPos -> [KnightPos]
in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` in3 start
--}

type KnightPos = (Int, Int)
type KnightPosChain = [KnightPos]

moveKnight :: KnightPosChain -> [KnightPosChain]
moveKnight [] = []
moveKnight ((c,r):prev) = do
    -- List monad do notation:
    --   return x = [x]
    --   xs >>= f = concat (map f xs)
    -- xs
    (c', r') <- [(c+2,r+1),(c+2,r-1),(c-2,r+1),(c-2,r-1),
                 (c+1,r+2),(c+1,r-2),(c-1,r+2),(c-1,r-2)]
    -- f
    guard (c' `elem` [1..8] && r' `elem` [1..8]) -- returns [] if false
    return $ (c',r') : (c,r) : prev

in3 :: KnightPosChain -> [KnightPosChain]
in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight

findMoves :: KnightPos -> KnightPos -> [KnightPosChain]
findMoves start end = do
    let chains = in3 [start]
    [reverse chain | chain <- chains, end == head chain]

