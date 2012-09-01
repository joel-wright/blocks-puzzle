module Main (
    main
    )
    where

import List

data BlockE = Hole | Pin | Blank
    deriving (Show,Eq)

type Block = [BlockE]

b1,b2,b3,b4,b5,b6,b7,b8,b9,b10 :: Block
b1 =  [Hole,Hole,Blank,Blank,Hole]
b2 =  [Pin,Hole,Pin,Blank,Blank]
b3 =  [Pin,Blank,Hole,Blank,Hole]
b4 =  [Hole,Pin,Hole,Blank,Blank]
b5 =  [Hole,Pin,Blank,Hole,Blank]
b6 =  [Pin,Blank,Pin,Blank,Blank]
b7 =  [Blank,Pin,Blank,Blank,Pin]
b8 =  [Pin,Blank,Hole,Blank,Blank]
b9 =  [Blank,Pin,Blank,Hole,Blank]
b10 = [Blank,Pin,Blank,Pin,Hole]

type Blocks = [Block]
type Solution = ([Block],[Block])

blocks :: [Block]
blocks = [b1,b2,b3,b4,b5,b6,b7,b8,b9,b10]

-- Main just shows the first solution from the solution list
main :: IO()
main = mainAll' 1 (take 1 $ construct blocks)

mainAll' :: Int -> [Solution] -> IO()
mainAll' n []     = return ()
mainAll' n (x:xs) = do
                      putStrLn $ "Solution " ++ (show n) ++ ":\n\n" ++ (showSolution x)
                      mainAll' (n+1) xs


-- A quick function to convince myself that the multiple solutions 
-- produced are mirrors/rotations of a single solution.
removeMirrors :: Solution -> [Solution] -> [Solution]
removeMirrors s ss = filter (\s' -> not(elem s' mss)) ss
    where
        bsl = fst(s)
        bsr = snd(s)
        mss = [((reverse (map reverse bsr)),reverse((map reverse bsl))),
               ((reverse (map reverse bsl)),reverse((map reverse bsr))),
               (reverse(bsl),(map reverse bsr)),
               ((map reverse bsl),reverse(bsr)),
               (reverse(bsr),(map reverse bsl)),
               ((map reverse bsr),reverse(bsl)),
               (bsr,bsl)]

-- We now construct our solution by generating only valid solutions
--
-- construct takes the block list, and a pair accumulator into which we're
-- gonna put our solution piece by piece.
construct :: [Block] -> [Solution]
construct bs = construct' (bs,([],[]))

-- we now represent a solution as a pair of lists of blocks,
-- the first element for the vertical blocks and the second for the
-- horizontal blocks.
--
-- construct' is the function that does the actual work.
-- if we find an empty list of blocks then we have a solution!
-- if we still have blocks to put in, we do some work:
--
-- we take the block from the block list, and add it to our solution
-- only if it is a valid move (see later).
construct' :: (Blocks,(Blocks,Blocks)) -> [Solution]
construct' ([],(v,h)) = [(v,h)]
construct' (bs,(v,h)) = concat $ map construct' $
                            [(delete b bs,(h,v++[b])) | b <- bs, valid (v++[b],h)] 
                             ++
                            [(delete b bs,(h,v++[(reverse b)])) | b <- bs, valid (v++[(reverse b)],h)]

-- valid only does the work that hasn't been done in a previous test
-- 
-- i.e. when we try and add a block to a potential solution we know
-- due to the way the constructor works, that we need only test those
-- parts which have recently been added (where the last added
-- block overlaps with those already present).

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

-- everything else is obvious.
allBlank :: [Bool] -> Bool
allBlank = all (==True)

addP :: (BlockE,BlockE) -> Bool
addP (Blank,Blank) = True
addP (Pin,Hole) = True
addP (Hole,Pin) = True
addP (_,_) = False

showSolution :: Solution -> String
showSolution (blks1,blks2) = "Layer 1:\n\n" ++ (showBlocks blks1) ++ "\n" ++
                             "Layer 2:\n\n" ++ (showBlocks blks2)

showBlocks :: Blocks -> String
showBlocks [] = ""
showBlocks (b:bs) = (show b) ++ "\n" ++ showBlocks bs

