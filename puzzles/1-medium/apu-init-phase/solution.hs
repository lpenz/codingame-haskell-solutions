import System.IO
import Control.Monad
import qualified Data.List as L
import Data.Ord

-- Debug:

import Debug.Trace 

traceshow :: (Show a) => a -> b -> b
traceshow a = trace (show a)

traceshow0 :: (Show a) => a -> a
traceshow0 a = traceshow a a

-- Node type:

newtype Node = Node (Int, Int) deriving (Eq, Ord)
instance Show Node where
   show n = show (x n) ++ " " ++ show (y n)

unNode :: Node -> (Int, Int)
unNode (Node t) = t

x :: Node -> Int
x = fst . unNode

y :: Node -> Int
y = snd . unNode

-- Input processing:

inputproc :: Int -> Int -> [[Bool]] -> [Node]
inputproc width height bm = L.map (Node . fst) $ L.filter snd wcoords
    where
        wcoords = L.zip coords $ L.concat bm
        coords = L.map ( \ i -> (i `mod` width, i `div` width)) [0..(width*height - 1)]

-- Output calculation:

putneigh :: [Node] -> (Node -> Int) -> (Node -> Int) -> Node -> Node
putneigh nodes f g node = ans
    where
        neighs = L.filter (\ n -> f node == f n && g n > g node) nodes
        nearest = L.minimumBy (comparing f) neighs
        ans = if L.null neighs
                    then Node (-1, -1)
                    else nearest

-- Main:

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    width <- fmap read getLine :: IO Int
    height <- fmap read getLine :: IO Int
    lines <- replicateM height $ fmap (L.map (== '0')) getLine :: IO [[Bool]]
    let nodes = inputproc width height lines
    forM_ nodes $ \ node -> do
        putStr $ show node ++ " "
        putStr $ show $ putneigh nodes y x node
        putStr " "
        putStr $ show $ putneigh nodes x y node
        putStr "\n"
