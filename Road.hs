type Path = [(Char, Int)]

data Section = Section { a :: Int, b :: Int, c :: Int }
    deriving (Show)

roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c) = 
    let priceA = sum $ map snd pathA
        priceB = sum $ map snd pathB
        forwardPriceToA = priceA + a
        crossPriceToA = priceB + b + c
        forwardPriceToB = priceB + b
        crossPriceToB = priceA + a + c
        newPathToA = if forwardPriceToA <= crossPriceToA
                        then ('A',a):pathA
                        else ('C',c):('B',b):pathB
        newPathToB = if forwardPriceToB <= crossPriceToB
                        then ('B',b):pathB
                        else ('C',c):('A',a):pathA
    in  (newPathToA, newPathToB)

main :: IO ()
main = do
    let initialPaths = ([], [])
        sections = [Section 10 20 30, Section 5 10 15, Section 20 30 10]
        finalPaths = foldl roadStep initialPaths sections
    print finalPaths
