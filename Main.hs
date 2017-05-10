{-# LANGUAGE OverloadedStrings #-}
import Reflex
import Reflex.Dom
import Control.Monad.Trans (liftIO)
import Data.Map as DM (Map, fromList)
import Data.Text (Text, pack)
import Data.Traversable (forM)
import Data.Time.Clock (getCurrentTime)
import Data.List (transpose)

kprod :: Num a => [[a]] -> [[a]] -> [[a]]
kprod xs ys = 
    let m0 = flip $ fmap.fmap.(*)
        m1 = flip $ fmap.fmap.m0
    in concatMap (fmap concat.transpose) $ m1 xs ys

cellSize :: Int
cellSize = 5

cellAttrs :: Map Text Text
cellAttrs = 
    fromList [ ( "x",            "0.05")
             , ( "y",            "0.05")
             , ( "width",        "0.9")
             , ( "height",       "0.9")
             , ( "style",        "fill:blue")
             ] 

groupAttrs :: (Int,Int) -> Map Text Text
groupAttrs (x,y) = 
    fromList [ ("transform", 
                pack $    "scale (" ++ show cellSize ++ ", " ++ show cellSize ++ ") " 
                       ++ "translate (" ++ show x ++ ", " ++ show y ++ ")" 
               )
             ] 

boardAttrs :: Int -> Int -> Map Text Text
boardAttrs w h = fromList 
                 [ ("width" , pack $ show $ w * cellSize)
                 , ("height", pack $ show $ h * cellSize)
                 ]

showCell :: MonadWidget t m => Int -> (Int,Int) -> m ()
showCell x (y,on) = 
    if (on==1) then
        elSvgns "g"  (constDyn $ groupAttrs (x,y)) $ 
            elSvgns "rect" (constDyn $ cellAttrs) $ 
                return ()
    else
        return ()

showRow :: MonadWidget t m => (Int,[Int]) -> m ()
showRow (x,r) = mapM_ (showCell x) $ zip [0..] r 

showMatrix :: MonadWidget t m => [[Int]] -> m ()
showMatrix m = mapM_ showRow $ zip [0..] m 

showFractal :: MonadWidget t m => [[Int]] -> m ()
showFractal seed = do
  let fractals = iterate (kprod seed) seed
      shown = fractals !! 3
      w = length $ head shown
      h = length shown
  elSvgns "svg" (constDyn $ boardAttrs w h) $ showMatrix shown

main :: IO ()
main = mainWidget $ do 
  let vicsekSeed = 
          [[0, 1, 0]
          ,[1, 1, 1]
          ,[0, 1, 0]
          ]
  showFractal vicsekSeed

  el "br" $ return ()
  let sirpinskiSeed = 
          [[1, 1, 1]
          ,[1, 0, 1]
          ,[1, 1, 1]
          ]
  showFractal sirpinskiSeed

elSvgns :: MonadWidget t m => Text -> Dynamic t (Map Text Text) -> m a -> m a
elSvgns t m ma = do
    (el, val) <- elDynAttrNS' (Just "http://www.w3.org/2000/svg") t m ma
    return val
