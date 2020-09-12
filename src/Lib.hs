module Lib
    ( someFunc
    ) where

import Data.List (sort, sortBy)
import Data.Maybe (mapMaybe)

myCamera :: Camera
myCamera = Camera (Vec3 0 100 (-100)) (Vec3 0 50 (-50)) (Vec3 0 35.36 35.36) (Vec3 (-50) 0 0)

myScene :: Scene
myScene = Scene [ Sphere (Vec3 0 0 100) 50 (Color 255 0 0)
                , Sphere (Vec3 100 0 150) 60 (Color 0 0 255)
                ]

someFunc :: IO ()
someFunc = putStrLn . ppmFromLists $ image 500 500 myCamera myScene

data Vec3 = Vec3
    { vecX :: Double
    , vecY :: Double
    , vecZ :: Double
    }

data Ray = Ray
    { rayOrigin    :: Vec3
    , rayDirection :: Vec3
    }

data Object = Sphere
    { sphereCenter :: Vec3
    , sphereRadius :: Double
    , sphereColor  :: Color
    }

newtype Scene = Scene { unScene :: [Object] }

data Color = Color
    { red   :: Int
    , green :: Int
    , blue  :: Int
    }

data Camera = Camera
    { cameraOrigin :: Vec3
    , screenOrigin :: Vec3
    , screenUp     :: Vec3
    , screenLeft   :: Vec3
    }

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x:_) = Just x

prod3 :: Vec3 -> Vec3 -> Double
prod3 (Vec3 x y z) (Vec3 x' y' z') = x*x' + y*y' + z*z'

add3 :: Vec3 -> Vec3 -> Vec3
add3 (Vec3 x y z) (Vec3 x' y' z') = Vec3 (x+x') (y+y') (z+z')

sub3 :: Vec3 -> Vec3 -> Vec3
sub3 (Vec3 x y z) (Vec3 x' y' z') = Vec3 (x-x') (y-y') (z-z')

intersection :: Ray -> Object -> Maybe (Double, Color)
intersection (Ray orig dir) (Sphere center radius color)
    | discriminant < 0 = Nothing
    | otherwise        = fmap (\l -> (l, color)) solution
  where
    o = sub3 orig center
    d = dir
    r = radius
    od = prod3 o d
    dNormSquared = prod3 d d
    oNormSquared = prod3 o o
    discriminant = od * od - dNormSquared * (oNormSquared - r*r)
    lambda1  = ((-od) - sqrt discriminant) / dNormSquared
    lambda2  = ((-od) + sqrt discriminant) / dNormSquared
    solution = headMaybe . sort . filter (>0) $ [lambda1,lambda2]

trace :: Scene -> Ray -> Maybe Color
trace (Scene objects) ray = fmap snd . headMaybe . sortBy (\(a,_) (b,_) -> compare a b) $ intersections
  where
    intersections = mapMaybe (intersection ray) objects

pixel :: Vec3 -> Vec3 -> Scene -> Color
pixel orig pix scene =
    case trace scene ray of
      Just color -> color
      Nothing    -> Color 0 0 0
  where
    dir = sub3 pix orig
    ray = Ray orig dir

image :: Int -> Int -> Camera -> Scene -> [[Color]]
image xRes yRes (Camera orig screenOrig screenUp screenLeft) scene =
    fmap (\y -> fmap (\x -> pixel orig scene x y) [-(xRes `div` 2) .. xRes `div` 2]) [-(yRes `div` 2) .. yRes `div` 2]
  where
    

ppmFromLists :: [[Color]] -> String
ppmFromLists xs@(x:_) = "P3" <> "\n" <> show cols <> " " <> show rows <> "\n" <> "255" <> "\n" <> content
  where
    cols = length x
    rows = length xs
    content = unlines $ fmap unwords $ fmap (fmap (\(Color r g b) -> show r <> " " <> show g <> " " <> show b)) xs
