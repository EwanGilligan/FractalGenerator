module Plot
    ( drawPlot
    )
where

import           Fractals                       ( Coordinate )
import           Graphics.GD
import           Control.Monad

type Window = (Coordinate, Coordinate)

white = rgb 255 255 255

drawPlot :: (Coordinate -> Color) -> Window -> Size -> FilePath -> IO ()
drawPlot coordToColour win dim filepath = do
    im <- newImage dim
    fillImage white im
    mapM_ (\p -> setPixel p (coordToColour (mapPixelToCoord p win dim)) im)
        $ allPixels dim
    savePngFile filepath im

mapPixelToCoord :: Point -> Window -> Size -> Coordinate
mapPixelToCoord (x, y) ((lx, rx), (by, ty)) (w, h) =
    let xscale = fromIntegral x / fromIntegral w
        yscale = fromIntegral y / fromIntegral h
    in  ((rx - lx) * xscale + lx, (ty - by) * yscale + by) 

allPixels :: Size -> [Point]
allPixels (w, h) = [ (x, y) | x <- [0 .. (w - 1)], y <- [0 .. (h - 1)] ]
