import Data.Bits((.&.),(.|.),shiftL,shiftR)
import Data.List(group,sort,nub,partition,splitAt)
import Data.Map (Map,empty,insert,member,size,foldWithKey)
import Data.Functor.Identity
import Control.Monad.Trans.State

data Side = Side Int Bool deriving Eq 
data Cube = Cube Side Side deriving (Show,Eq)

instance Show Side where
    show (Side code flippable) = ((bits code) >>= show) ++ ps where
        ps | flippable = ""
           | otherwise = "-"

instance Ord Side where
    compare (Side a _) (Side b _) = compare a b

instance Ord Cube where
    compare (Cube aUp aDown) (Cube bUp bDown) = compare (aUp,aDown) (bUp,bDown)

bits n = reverse $ bits' 12 n where
    bits' 0 k = []
    bits' m k = (k `mod` 2):bits' (m-1) (k `div` 2) 

--up:            / H
--         10 11/0 
--      9      /    1
--     8      /     2
--    7      /     3
--        6 /5  4      
--       L /
--down:
--           L / 
--       7  6 /5
--    8      /    4
--   9      /    3  
--  10     /    2
--      11/0 1   
--       / H
       

done = Cube (Side 3510 True) (Side 3510 True)
cube0 = Cube (Side 3135 True) (Side 3510 True)

--Rotates side clockwise
rotateSide :: Side -> Side
rotateSide (Side code flippable) = Side code' flippable' where
    code' = (code `shiftR` 1) .|. ((code .&. 1) `shiftL` 11)
    flippable' = (not flippable) || even code

--List of distinct side rotations
distinctRotations :: Side -> [Side]
distinctRotations side = side:(takeWhile (/=side) $ tail $ iterate rotateSide side)

--Checks if side is flippable
isFlippable (Side _ flippable) = flippable

--List of flippable distinct side rotations
flippableRotations :: Side -> [Side]
flippableRotations = filter isFlippable . distinctRotations

--Flips cube around axis 11/0 -- 6/5
flipCube :: Cube -> [Cube]
flipCube (Cube up down) | isFlippable up = [Cube up' down'] 
                        | otherwise = [] where
    toPair (Side code _) = (code `shiftR` 6, code .&. 63)
    fromPair (l,h) = Side ((l `shiftL` 6) .|. h) True
    (upL, upH) = toPair up
    (dwL, dwH) = toPair down
    up' = fromPair (upL, dwH)
    down' = fromPair (dwL, upH)     

--'Minified' representation of cube 
minifyCube :: Cube -> Cube
minifyCube (Cube up down) = Cube up' down' where
    [up', down'] = sort [minimum $ distinctRotations up, minimum $ distinctRotations down]

--Get minified cubes one flip from given
derivedCubes :: Cube -> [Cube]
derivedCubes (Cube up down) = nub [minifyCube flp | up' <- flippableRotations up, down' <- flippableRotations down, flp <- flipCube (Cube up' down')]


step :: [Cube] -> State (Map Cube [Cube]) [Cube]
step [] = do
    return []
step (cube:cs) = do
    --current map of cube to it's derived cubes
    mp <- get
    if (cube `member` mp)
        then (step cs)
        else do
    let derived = derivedCubes cube
    let (seen, new) = partition (`member` mp) derived
    put (insert cube derived mp)
    rest <- step cs
    return (new++rest)

steps :: Cube -> [([Cube], Map Cube [Cube])]
steps cube = iterate fn ([cube], empty) where
    fn (cs,mp) = runState (step (take 10 cs)) mp
