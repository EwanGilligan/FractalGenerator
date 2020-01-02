module JuliaSet
    ( julia
    , maxIter
    )
where

import           Data.Complex

maxIter = 64 

julia
    :: Complex Double -- c
    -> Complex Double -- z
    -> Int
    -> Int
julia c z iter = if iter > maxIter
    then 0
    else
        let z' = z ^ 2 + c
        in  if magnitude z' > 2 then iter else julia c z' (iter + 1)
