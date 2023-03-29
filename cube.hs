import Data.Bits((.&.),(.|.),shiftL,shiftR)
import Data.List(group,sort,nub,partition,splitAt,find)
import Data.Map (Map,empty,insert,member,keys)
import Data.Functor.Identity
import Control.Monad.Trans.State

data Piece = Triangle | Kite deriving (Eq, Ord, Enum, Show)
data Side = Side {pieces :: [Piece], isCut :: Bool} deriving Eq
data Cube = Cube {up :: Side, down :: Side} deriving (Show,Eq)

instance Show Side where
    show (Side pieces isCut) = (show pieces) ++ ps where
        ps | isCut = "-"
           | otherwise = ""

instance Ord Side where
    compare (Side a _) (Side b _) = compare a b

instance Ord Cube where
    compare (Cube aUp aDown) (Cube bUp bDown) = compare (aUp,aDown) (bUp,bDown)

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

construct (up,upCt) (down,downCt) | (length kites)==(length triangles) && (length kites) == 8 = Cube (Side up upCt) (Side down downCt)
                                  | otherwise = error "Invalid cube" where
    (kites, triangles) = partition (==Kite) (up ++ down)
       
--Complete cube
done = minifyCube $ construct 
    ([Kite,Triangle,Kite,Triangle,Kite,Triangle,Kite,Triangle], False) 
    ([Kite,Triangle,Kite,Triangle,Kite,Triangle,Kite,Triangle], False)
--Some random state
cube0 = minifyCube $ construct 
    ([Triangle,Triangle,Triangle,Triangle,Kite,Kite,Kite,Kite], False)
    ([Kite,Triangle,Kite,Triangle,Kite,Triangle,Kite,Triangle], False)
--Cube 8 steps from done
devil = minifyCube $ construct 
    ([Triangle,Triangle,Kite,Triangle,Kite,Kite,Triangle,Kite], False) 
    ([Triangle,Kite,Triangle,Kite,Triangle,Kite,Triangle,Kite], False)

--Rotates side clockwise
rotateSide::Side -> Side
rotateSide (Side pieces isCut) | isCut = Side pieces False
                               | otherwise = Side (p':ps') (p'==Kite) where
    p' = last pieces
    ps' = init pieces

--List of distinct side rotations
distinctRotations :: Side -> [Side]
distinctRotations side = side:(takeWhile (/=side) $ tail $ iterate rotateSide side)

splitSide :: Side -> [([Piece],[Piece])]
splitSide (Side pieces isCut) | isCut || hCount /= halfSize = []
                              | otherwise = [(reverse h, l)] where
    (h,l,hCount) = head $ dropWhile (\(_,_,c) -> c<halfSize) $ iterate (\(cur,(r:rs),c) -> (r:cur,rs,c+1+fromEnum r)) ([],pieces,0)
    halfSize = 6

--List of flippable distinct side rotations
flippableRotations :: Side -> [([Piece],[Piece])]
flippableRotations = (>>= splitSide) . distinctRotations

flipPair :: ([Piece],[Piece]) -> ([Piece],[Piece]) -> Cube
flipPair (upH,upL) (downH, downL) = Cube (Side (downL++upH) False) (Side (upL ++ downH) False)

--Flips cube around axis 11/0 -- 6/5
flipCube :: Cube -> [Cube]
flipCube (Cube up down) = do
    upSplit <- splitSide up
    downSplit <- splitSide down
    return $ flipPair upSplit downSplit

--'Minified' representation of cube 
minifyCube :: Cube -> Cube
minifyCube (Cube up down) = Cube up' down' where
    [up', down'] = sort [minimum $ distinctRotations up, minimum $ distinctRotations down]

--Get minified cubes one flip from given
derivedCubes :: Cube -> [Cube]
derivedCubes (Cube up down) = nub [minifyCube $ flipPair up' down' | up' <- flippableRotations up, down' <- flippableRotations down]


--Gets cubes derived from given cubes with path
traceStep :: [[Cube]] -> State (Map Cube [Cube]) [[Cube]]
traceStep [] = do
    return []
traceStep (path:cs) = do
    let cube = head path
    mp <- get
    if (cube `member` mp)
        then (traceStep cs)
        else do
    let derived = derivedCubes cube
    let new = map (:path) $ filter (not.(`member` mp)) derived
    put (insert cube derived mp)
    rest <- traceStep cs
    return (new ++ rest)

--Trace of cube transitions between two states
trace :: Cube -> Cube -> Maybe [Cube]
trace from to = find fn $ fst $ trace' ([[from]], empty) where
    trace' (cs,mp) | (any fn cs) || null cs = (cs, mp)
                   | otherwise = trace' $ runState (traceStep cs) mp
    fn = (==to).head

steps :: Cube -> [([Cube], Map Cube [Cube])]
steps cube = iterate fn ([cube], empty) where
    fn (cs,mp) = runState (step cs) mp
    --Gets cubes derived from given cubes and updates transitions graph
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
        let new = filter (not.(`member` mp)) derived
        put (insert cube derived mp)
        rest <- step cs
        return (new++rest)


