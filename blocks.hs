module Main (
    main
    )
    where

import List

data BlockE = Hole | Pin | Blank
    deriving (Show,Eq)

type Block = [BlockE]

b1,b1r,b2,b2r,b3,b3r,b4,b4r,b5,b5r :: Block
b1 = [Hole,Hole,Blank,Blank,Hole]
b1r = reverse b1
b2 = [Pin,Hole,Pin,Blank,Blank]
b2r = reverse b2
b3 = [Pin,Blank,Hole,Blank,Hole]
b3r = reverse b3
b4 = [Hole,Pin,Hole,Blank,Blank]
b4r = reverse b4
b5 = [Hole,Pin,Blank,Hole,Blank]
b5r = reverse b5

b6,b6r,b7,b7r,b8,b8r,b9,b9r,b10,b10r :: Block
b6 = [Pin,Blank,Pin,Blank,Blank]
b6r = reverse b6
b7 = [Blank,Pin,Blank,Blank,Pin]
b7r = reverse b7
b8 = [Pin,Blank,Hole,Blank,Blank]
b8r = reverse b8
b9 = [Blank,Pin,Blank,Hole,Blank]
b9r = reverse b9
b10 = [Blank,Pin,Blank,Pin,Hole]
b10r = reverse b10

type Blocks = [Block]

type Solution = ([Block],[Block])

blocks :: [(Block,Block)]
blocks = [ (b1,b1r),
           (b2,b2r),
           (b3,b3r),
           (b4,b4r),
           (b5,b5r),
           (b6,b6r),
           (b7,b7r),
           (b8,b8r),
           (b9,b9r),
           (b10,b10r) ]

-- Main just shows the first solution from the solution list
-- main = putStrLn $ show $ head $ (construct blocks)

main :: IO()
main = do
            putStrLn "Printing Solutions:\n"
            mainAll' 1 (construct blocks)

mainAll' :: Int -> [Solution] -> IO()
mainAll' n []     = putStrLn "\nAll Solutions Printed"
mainAll' n (x:xs) = do
                      putStrLn $ "Solution " ++ (show n) ++ ": " ++ (show x)
                      mainAll' (n+1) xs

-- We now construct our solution by fusing the generator and the tester
-- into a single smart constructor of the solution list

-- construct takes the block list, and a pair accumulator into which we're
-- gonna put our solution piece by piece.

construct :: [(Block,Block)] -> [Solution]
construct bs = construct' (bs,([],[]))

-- we represent a solution as a pair of lists of blocks,
-- the first element for the vertical blocks and the second for the
-- horizontal blocks.
--
-- construct' is the function that does the actual work.
-- if we find an empty list of blocks then we have a solution!
-- if we still have blocks to put in, we do some work:
--
-- we take the block from the block list, and add it to our solution
-- only if it is a valid move (see later).
-- the two list comprehensions are there because our block list
-- is saved as a block with its reverse.

construct' :: ([(Block,Block)],(Blocks,Blocks)) -> [Solution]
construct' ([],(v,h)) = [(v,h)]
construct' (bs,(v,h)) = concat $ map construct' $ nub ([(remove b bs,(h,v++[b])) | (b,b') <- bs , valid (v++[b],h)] 
                                                        ++
                                                       [(remove b' bs,(h,v++[b'])) | (b,b') <- bs , valid (v++[b'],h)])

-- valid only does the work that hasn't been done in a previous test
--
-- i.e. when we try and add a block to a potential solution we know
-- due to the way the constructor works, that we need only test those
-- parts which have recently been added (i.e. where the last added
-- block overlaps with those already there).

-- it should be noted that the valid test always looks at the first
-- block list because we swap the elements of the potential solution
-- every time we add a block (this allows us to have only one
-- construct' and valid function, rather than a contruct and valid
-- for adding vertical blocks, and one for horizontal blocks.

valid :: (Blocks,Blocks) -> Bool
valid (bs,[])  = True
valid (bs,bs') = allBlank $ map addP $ zip (last bs) (map (!!i) bs')
                     where
                         i = (length bs) - 1

-- everything else is pretty obvious.

allBlank :: [Bool] -> Bool
allBlank = all (==True)

addP :: (BlockE,BlockE) -> Bool
addP (Blank,Blank) = True
addP (Pin,Hole) = True
addP (Hole,Pin) = True
addP (_,_) = False

remove :: Block -> [(Block,Block)] -> [(Block,Block)]
remove r ((b,b'):bs)
    | r == b    = bs
    | r == b'   = bs
    | otherwise = (b,b') : (remove r bs)

