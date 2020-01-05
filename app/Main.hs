module Main where

import Fractals
import Plot


main :: IO ()
main = drawPlot drawMandlebrot ((-2, 1),(-1, 1)) (6000, 4000) "mandlebrot.png"
