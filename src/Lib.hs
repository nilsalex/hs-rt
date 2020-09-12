module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Vec3 = Vec3
    { vecX :: Double
    , vecY :: Double
    , vecZ :: Double
    }

data Ray = Ray
    { origin    :: Vec3
    , direction :: Vec3
    }

data Object = Sphere Vec3 Double


