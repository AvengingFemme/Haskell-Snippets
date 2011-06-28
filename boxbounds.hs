-- File file file file
-- Supposed to let you find the minimum box size for shipping porpoises
import List

my6970box = [3.25, 18.125, 12.25]

availableFlatRateBoxes = [[8+5/8,5+3/8+1+5/8], [11,8+1/2,5+1/2], [13+5/8, 11+7/8,3+3/8], [12,12,5+1/2]]

-- fitBox is the box we're trying to put inside another box
-- testBox is the box we're trying to fit inside
fitBoxInto :: [Double] -> [Double] -> Bool
fitBoxInto fitBox testBox
	| length fitBox /= length testBox = False -- if they're differently dimensioned, not going to fit
	| otherwise = foldl (&&) True [ if x < y then True else False | x <- sort fitBox, y <- sort testBox ]

fitIntoBoxes fitBox listOfTestBoxes = filter (fitBoxInto fitBox) listOfTestBoxes

main = do
	putStrLn (show (fitIntoBoxes my6970box availableFlatRateBoxes))
