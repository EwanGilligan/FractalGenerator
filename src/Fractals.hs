module Fractals
    ( julia
    , mandlebrot
    )
where

import           Data.Complex
import           Graphics.GD 

iterateSet
    :: Int
    -> Complex Double -- c
    -> Complex Double -- z
    -> Int
    -> Int
iterateSet maxIter c z iter = if iter > maxIter
    then 0
    else
        let z' = z ^ 2 + c
        in  if magnitude z' > 2
                then iter
                else iterateSet maxIter c z' (iter + 1)

julia :: Complex Double -> Complex Double -> Int
julia c z = iterateSet 64 c z 0

mandlebrot :: Complex Double -> Complex Double -> Int
mandlebrot c z = iterateSet 127 c z 0

drawMandlebrot :: Point -> Color
drawMandlebrot (x, y) = pointColour 127 $ mandlebrot (fromIntegral x :+ fromIntegral y) (0 :+ 0)

pointColour ::Int -> Int -> Color
pointColour _ 0 = rgb 255 255 255
pointColour maxIter x = let colour = iterToColour maxIter x in rgb colour colour colour   

iterToColour :: Int -> Int -> Int
iterToColour maxIter iter = floor (255.0 - 255.0 * fromIntegral iter / fromIntegral maxIter)
