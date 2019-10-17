import Data.Fixed
import System.IO (hFlush, stdout)
import System.Environment (getArgs)

data Comp = Ave1 | Ave2 | Equal deriving (Show, Eq)

type Size = Fixed E2

changeResolution :: (HasResolution a) => Fixed a -> Float
changeResolution = fromRational . toRational

ave1 :: Size -> Size -> Size
ave1 a b = a/2 + b/2 

ave2 :: Size -> Size -> Size
ave2 a b = a + (b - a) / 2

ave :: Float -> Float -> Float
ave a b = (a + b) / 2

comp :: [Size] -> [(Size, Size, Float, Size, Size, Comp)]
comp []     = []
comp (x:xs) = (loop x xs) ++ (comp xs)
 where
  loop :: Size -> [Size] -> [(Size, Size, Float, Size, Size, Comp)]
  loop a []     = []
  loop a (b:bs) = do
   let av1 = ave1 b a
   let av2 = ave2 b a 
   let av  = ave (changeResolution b) (changeResolution a)
   let cp  = if av1 == av2 then Equal else if (abs (av-(changeResolution av1))) < (abs (av-(changeResolution av2))) then Ave1 else Ave2
   (a, b, av, av1, av2, cp):(loop a bs)

selector :: Comp -> [Size] -> [(Size, Size, Float, Size, Size, Comp)]
selector dcp list = (filter (\(a, b, av, av1, av2, cp) -> cp == dcp).comp) list

main = do
 [a, b]<- getArgs
-- let list = [(read a :: Size), (read a :: Float) + 0.01..(read b :: Size)]
 let list = [(read a :: Size), (read a :: Size) + 0.01 .. (read b :: Size)]
 putStr "print detail? [y/N]"
 hFlush stdout
 ans <- getLine
 if ans == "y" then printDetail list >> printOutOfAverage (comp list) >> printResult list else printOutOfAverage (comp list) >> printResult list
  where
   printDetail list = do
    putStrLn "a/2 + b/2"
    mapM_ print $ selector Ave1 list
    putStrLn "a + (b-a)/2"
    mapM_ print $ selector Ave2 list
   printResult list = do
    putStrLn "===== reslut ====="
    putStrLn "(all, equal, a/2 + b/2, a + (b-a)/2)"
    print $ ((length.comp) list, (length.selector Equal) list, (length.selector Ave1) list, (length.selector Ave2) list)
   printOutOfAverage []                              = putStrLn "out of end"
   printOutOfAverage ((a, b, av, av1, av2, cp):list) = 
    if av1 < a || b < av1 || av2 < a || b < av2 then print (a,b,av,av1,av2,cp) >> printOutOfAverage list
    else                                                                          printOutOfAverage list

