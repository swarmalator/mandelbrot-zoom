import Graphics.Gloss
import Graphics.Gloss.Raster.Field
import Graphics.Gloss.Data.ViewPort
import Data.Complex
import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSV
import qualified Graphics.Gloss.Data.Point.Arithmetic as PA

mandelbrot :: Complex Float -> Complex Float -> Int -> (Complex Float, Int)
mandelbrot z _ 0 = (z, 0)
mandelbrot z c n
  | magnitude z > 2 = (z, n)
  | otherwise = mandelbrot (z * z + c) c (n - 1)
  

rgbcolor :: Float -> Float -> Float -> Color
rgbcolor r g b = makeColor r g b 1.0

smoothcolor :: Complex Float -> Int -> Int -> Color
smoothcolor _ 0 _ = black
smoothcolor z n max = (uncurryRGB rgbcolor) $ hsv deg 1 1 
  where deg = 360 * ((fromIntegral n) - 1.0 + (log (log (magnitude z))) / (log 2)) / (fromIntegral max)
  
colorof :: Int -> Point -> Color
colorof max (x,y) = let (z, n) = mandelbrot 0 (x :+ y) max
                in smoothcolor z n max

getpicture :: ViewPort -> Float -> (Picture,Point) -> (Picture,Point)
getpicture vp _ pd@(pic,pt) = let npt = invertViewPort vp (1,1)
                                  adjpt = (PA.- (1/300 PA.* t)).(300/scl PA.*)
                                  adjpic = (translate (-tx) (-ty)).(scale (300 /scl) (300/scl))
                                  t@(tx,ty) = viewPortTranslate vp
                                  scl = 300 * (viewPortScale vp)
                                  max = (round $ 4 * (log scl))
                              in case (pt /= npt) of
                                True -> (adjpic $ makePicture 600 600 5 5 ((colorof max).adjpt), npt)
                                False -> pd

main :: IO ()
main = simulate
  (InWindow "Mandelbrot" (600,600) (20,20))
  black
  1
  (Circle 0, (0,0))
  (\(p,_ ) -> p)
  getpicture
