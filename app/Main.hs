module Main where

import Control.Monad (unless, when)

main :: IO ()
main = do
    let b1 = fromXWMV 100 50 1 0
        b2 = fromXWMV 300 150 100000 (-5)
    update b1 b2 0 0

data Block =
    Block Double Double Double Double
    deriving (Show)

fromXWMV :: Double -> Double -> Double -> Double -> Block
fromXWMV x w m v = Block x w m v

toXWMV :: Block -> (Double, Double, Double, Double)
toXWMV (Block x w m v) = (x, w, m, v)

getX :: Block -> Double
getX (Block x _ _ _) = x

getW :: Block -> Double
getW (Block _ w _ _) = w

updateX :: Block -> Block
updateX (Block x w m v) = Block (x + v) w m v

colliding :: Block -> Block -> Bool
colliding (Block x1 w1 _ _) (Block x2 w2 _ _) =
    not $ x1 + w1 < x2 || x1 > x2 + w2

bounce :: Block -> Block -> (Block, Block)
bounce b1@(Block x1 w1 m1 v1) b2@(Block x2 w2 m2 v2) =
    (fromXWMV x1 w1 m1 n1, fromXWMV x2 w2 m2 n2)
  where
    sumM = m1 + m2
    n1 = (m1 - m2) / sumM * v1 + (2 * m2 / sumM) * v2
    n2 = (2 * m1 / sumM) * v1 + (m2 - m1) / sumM * v2

update :: Block -> Block -> Integer -> Integer -> IO ()
update b1 b2 col times
    -- putStrLn $ unlines [show b1, show b2]
 = do
    let newB1@(Block x1 w1 m1 v1) = updateX b1
        newB2@(Block x2 w2 m2 v2) = updateX b2
    putStrLn $ unlines [show v1, show v2]
    putStrLn $ unlines [show x1, show x2]
    -- when (times > 10000) $ error (show col)
    if (v1 > 0 && v2 > 0 && v2 > v1)
        then print col
        else if (x1 < 0)
                 then update (fromXWMV 0 w1 m1 (-v1)) newB2 (col + 1) (0)
                 else if colliding newB1 newB2
                          then do
                              let (t1, t2) = bounce newB1 newB2
                              update t1 t2 (col + 1) (0)
                          else update newB1 newB2 col (times + 1)
