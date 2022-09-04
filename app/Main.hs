module Main where

import Control.Monad (unless, when)

main :: IO ()
main = do
    let digits = 5
        steps = 10000
        b1 = fromXWMV 100 50 1 0
        b2 = fromXWMV 300 150 (100 ^ digits) (-(5 / steps))
    update b1 b2 0

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

updatePosition :: Block -> Block
updatePosition (Block x w m v) = Block (x + v) w m v

reverseVelocity :: Block -> Block
reverseVelocity (Block x w m v) = Block x w m (-v)

colliding :: Block -> Block -> Bool
colliding (Block x1 w1 _ _) (Block x2 w2 _ _) =
    not $ x1 + w1 < x2 || x1 > x2 + w2

bounce :: Block -> Block -> (Block, Block)
bounce b1@(Block x1 w1 ma va) b2@(Block x2 w2 mb vb) =
    (fromXWMV x1 w1 ma n1, fromXWMV x2 w2 mb n2)
  where
    sumM = ma + mb
    n1 = ((ma - mb) / sumM) * va + ((2 * mb) / sumM) * vb
    n2 = ((2 * ma) / sumM) * va + ((mb - ma) / sumM) * vb

update :: Block -> Block -> Integer -> IO ()
update b1 b2 col = do
    let areColliding = colliding b1 b2
        (t1@(Block x1 _ _ v1), t2@(Block x2 w2 _ v2)) =
            if areColliding
                then bounce b1 b2
                else (b1, b2)
        hitWall = x1 < 0
        newCol = col + boolToInteger areColliding + boolToInteger hitWall
        newB1 =
            updatePosition
                (if hitWall
                     then reverseVelocity t1
                     else t1)
        newB2 = updatePosition t2
    when (x1 > x2 + w2) $ error "Left block on the wrong side"
    if (v1 >= 0 && v2 >= 0 && v2 >= v1)
        then print newCol
        else update newB1 newB2 newCol

boolToInteger :: Bool -> Integer
boolToInteger = toInteger . fromEnum
