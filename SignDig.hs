import Data.Fixed
import System.IO (hFlush, stdout)
import System.Environment (getArgs)
import Graphics.Gnuplot.Simple
import Data.Time

data Comp = Ave1 | Ave2 | Equal deriving (Show, Eq)

type SizeE3 = Fixed E2

changeResolution :: (HasResolution a) => Fixed a -> Double
changeResolution = fromRational . toRational

revChangeResolution :: (HasResolution a) => Double -> Fixed a 
revChangeResolution = fromRational . toRational

ave1 :: SizeE3 -> SizeE3 -> SizeE3
ave1 a b = a/2 + b/2 

ave1' :: Double -> Double -> Double 
ave1' a b = a/2 + b/2 

ave2 :: SizeE3 -> SizeE3 -> SizeE3
ave2 a b = a + (b - a) / 2

ave2' :: Double -> Double -> Double
ave2' a b = a + (b - a) / 2

ave :: SizeE3 -> SizeE3 -> SizeE3 
ave a b = (a + b) / 2

ave' :: Double -> Double -> Double
ave' a b = (a + b) / 2

comp :: [SizeE3] -> [(SizeE3, SizeE3, Double, SizeE3, SizeE3, Comp)]
comp []     = []
comp (x:xs) = (loop x xs) ++ (comp xs)
 where
  loop :: SizeE3 -> [SizeE3] -> [(SizeE3, SizeE3, Double, SizeE3, SizeE3, Comp)]
  loop a []     = []
  loop a (b:bs) = do
   let av1 = ave1 a b
   let av2 = ave2 a b
   let av  = ave' (changeResolution a) (changeResolution b)
   let cp  = if av1 == av2 then Equal else if (abs (av-(changeResolution av1))) < (abs (av-(changeResolution av2))) then Ave1 else Ave2
   (a, b, av, av1, av2, cp):(loop a bs)

selector :: Comp -> [SizeE3] -> [(SizeE3, SizeE3, Double, SizeE3, SizeE3, Comp)]
selector dcp list = (filter (\(a, b, av, av1, av2, cp) -> cp == dcp).comp) list

customPlotPaths (a,b) funcs legend rat png = plotPathsStyle attribute plotstyle
 where
  line      = linearScale 1000 (a,b)
  w         = map (`fmap` line) funcs
  z         = map (zip line) w
  ratio     = if rat == "" then 1 else read rat
  fontType  = "IPAGothic"
  tics      = Custom "tics" ["font", "\"" ++ fontType ++ ",8\""] -- 目盛りフォントの変更
  xlabel    = Custom "xlabel" ["font", "\"" ++ fontType ++ ",8\""] -- xlabelのフォント
  ylabel    = Custom "ylabel" ["font", "\"" ++ fontType ++ ",8\""] -- ylabelのフォント
  keyFont   = Custom "ylabel" ["font", "\"" ++ fontType ++ ",8\""] -- 凡例のフォントの変更
  titleFont = Custom "ylabel" ["font", "\"" ++ fontType ++ ",8\""] -- タイトルのフォント変更
  key       = [] --凡例の位置等
  label     = [YLabel "誤差" , XLabel "b"] -- 軸の見出し
  save      = [PNG png] -- 出力する形式(PNG)と名前
  title     = [Title ("a = " ++ (show a) ++ "とb (" ++ (show a) ++ "," ++ (show b) ++ ")の平均の相対誤差") ] -- グラフのタイトル
  size      = [Aspect (Ratio ratio)] -- グラフの形 縦/横
  font      = [tics, xlabel, ylabel, keyFont, titleFont]
  attribute = save ++ key ++ label ++ title ++ size ++ font
  plotstyle = [x|i <-[0..(length z-1)],let x = (defaultStyle {lineSpec = CustomStyle [LineTitle (legend!!i),LineWidth 2.0]},z!!i)]

main = do
 [a, b]<- getArgs
 let list = [(read a :: SizeE3), (read a :: SizeE3) + 0.01 .. (read b :: SizeE3)]
 putStr "print detail? [y/N]"
 hFlush stdout
 ans <- getLine
 if ans == "y" then printDetail list >> putStrLn "start" >>  printOutOfAverage (comp list) >> printResult list
 else putStrLn "start" >> printOutOfAverage (comp list) >> printResult list
 putStr "filename :"
 hFlush stdout
 gfn <- getLine
 putStr "ratio :"
 hFlush stdout
 grt <- getLine
 let dba = read a :: Double
 decidePdfFile gfn >>= customPlotPaths (dba, (read b :: Double)) [func dba{-, func1 dba, func2 dba}-}] ["ave", "ave1", "ave2"] grt
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
   func :: Double -> Double -> Double
   func a b = ((ave' a b) - changeResolution (ave (revChangeResolution a) (revChangeResolution b))) / ave' a b
   func1 :: Double -> Double -> Double
   func1 a b = ((ave1' a b) - changeResolution (ave1 (revChangeResolution a) (revChangeResolution b))) / ave1' a b
   func2 :: Double -> Double -> Double
   func2 a b = ((ave2' a b) - changeResolution (ave2 (revChangeResolution a) (revChangeResolution b))) / ave2' a b
   decidePdfFile :: String -> IO String
   decidePdfFile []    = formatTime defaultTimeLocale "%Y-%m-%d-%T.png" <$> getZonedTime 
   decidePdfFile input = return input
