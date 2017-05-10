{-# LANGUAGE OverloadedStrings #-}
import Reflex
import Reflex.Dom
import Data.Map as DM (Map, fromList)
import Data.Text (Text, pack)
import Data.List (transpose)

-- show the Vicsek and Sierpinski fractals
main :: IO ()
main = mainWidget $ do 
  elAttr "h1" ("style" =: "color:black") $ text "Kroneker Product Based Fractals" 
  el "br" $ return ()
  elAttr "h2" ("style" =: "color:brown") $ text "Vicsek Fractal" 
  showFractal [[0, 1, 0] ,[1, 1, 1] ,[0, 1, 0] ]
  el "br" $ return ()
  elAttr "h2" ("style" =: "color:brown") $ text "Sierpinski Fractal" 
  showFractal [[1, 1, 1] ,[1, 0, 1] ,[1, 1, 1] ]

-- size in pixels of an individual cell
cellSize :: Int
cellSize = 8

-- given a "seed" matrix, generate and display a fractal.
showFractal :: MonadWidget t m => [[Int]] -> m ()
showFractal seed = do
  let boardAttrs w h = 
         fromList [ ("width" , pack $ show $ w * cellSize)
                  , ("height", pack $ show $ h * cellSize)
                  ]
      fractals = iterate (kronekerProduct seed) seed
      shown = fractals !! 3 -- the fourth fractal (starting from 0)
      w = length $ head shown
      h = length shown
  elSvgns "svg" (constDyn $ boardAttrs w h) $ showMatrix shown

-- Compute the Kroneker product of two matrices.
kronekerProduct :: Num a => [[a]] -> [[a]] -> [[a]]
kronekerProduct xs ys = 
    let m0 = flip $ fmap.fmap.(*)
        m1 = flip $ fmap.fmap.m0
    in concat $ fmap (fmap concat.transpose) $ m1 xs ys

-- Show an entire matrix
showMatrix :: MonadWidget t m => [[Int]] -> m ()
showMatrix m = mapM_ showRow $ zip [0..] m 

-- Show a single horizontal row of a matrix
showRow :: MonadWidget t m => (Int,[Int]) -> m ()
showRow (x,r) = mapM_ (showCell x) $ zip [0..] r 

-- Show a circle in a box moved to the correct location on screen
showCell :: MonadWidget t m => Int -> (Int,Int) -> m ()
showCell x (y,on) = 
  let boxAttrs (x,y) = -- place box on screen
        fromList [ ("transform", 
                    pack $    "scale (" ++ show cellSize ++ ", " ++ show cellSize ++ ") " 
                           ++ "translate (" ++ show x ++ ", " ++ show y ++ ")" 
                   )
                 ] 

      cellAttrs = -- draw circle in box.
        fromList [ ( "cx",      "0.5")
                 , ( "cy",      "0.5")
                 , ( "r",       "0.45")
                 , ( "style",   "fill:green")
                 ] 

  in if (on==1) then
       elSvgns "g"  (constDyn $ boxAttrs (x,y)) $ 
         elSvgns "circle" (constDyn $ cellAttrs) $ 
           return ()
     else
       return ()

-- Wrapper around elDynAttrNS'
elSvgns :: MonadWidget t m => Text -> Dynamic t (Map Text Text) -> m a -> m a
elSvgns t m ma = do
    (el, val) <- elDynAttrNS' (Just "http://www.w3.org/2000/svg") t m ma
    return val
