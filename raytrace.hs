data Vec3 = Vec3 { x :: Float, y :: Float, z :: Float } deriving (Show)
type Row = [Vec3]
type Image = [Row]

add :: Vec3 -> Vec3 -> Vec3
add v1 v2 = Vec3 (x v1 + x v2) (y v1 + y v2) (z v1 + z v2)

negate :: Vec3 -> Vec3
negate v = Vec3 (-x v) (-y v) (-z v)

multiply :: Vec3 -> Vec3 -> Vec3
multiply v1 v2 = Vec3 (x v1 * x v2) (y v1 * y v2) (z v1 * z v2)

divide :: Vec3 -> Vec3 -> Vec3
divide v1 v2 = Vec3 (x v1 / x v2) (y v1 / y v2) (z v1 / z v2)

scale :: Vec3 -> Float -> Vec3
scale v t = Vec3 (t * x v) (t * y v) (t * z v)

dot :: Vec3 -> Vec3 -> Float
dot v1 v2 = (x v1) * (x v2) + (y v1) * (y v2) + (z v1) * (z v2)

cross :: Vec3 -> Vec3 -> Vec3
cross v1 v2 = Vec3 ((y v1) * (z v2) - (z v1) * (y v2))
                   ((z v1) * (x v2) - (x v1) * (z v2))
                   ((x v1) * (y v2) - (y v1) * (x v2))

squared_length :: Vec3 -> Float
squared_length v = (x v) * (x v) + (y v) * (y v) + (z v) * (z v)

len = sqrt . squared_length

normalize :: Vec3 -> Vec3
normalize v = let l = len(v)
              in Vec3 (x v / l) (y v / l) (z v / l)

convert :: Integer -> Integer -> Float
convert x nx = 255.99 * fromIntegral x / fromIntegral nx

data Ray = Ray { origin :: Vec3
               , direction :: Vec3 } deriving (Show)

pointAt :: Ray -> Float -> Vec3
pointAt r t = add (origin r) (scale (direction r) t)

color :: Ray -> Vec3
color r = let t = 0.5 * (y (normalize (direction r)) + 1.0)
          in scale (add (scale (Vec3 1.0 1.0 1.0) (1.0-t)) (scale (Vec3 0.5 0.7 1.0) t)) 255.99

lower_left = Vec3 (-2.0) (-1.0) (-1.0)
horizontal = Vec3  4.0  0.0  0.0
vertical   = Vec3  0.0  2.0  0.0
imgOrigin = Vec3  0.0  0.0  0.0

genImage :: Integer -> Integer -> Image
genImage nx ny = [
                  [ color (Ray imgOrigin (add (add lower_left 
                                                   (scale horizontal (fromIntegral x / fromIntegral nx)))
                                              (scale vertical (fromIntegral y / fromIntegral ny)))) 
                          | x <- [0 .. nx-1]]
                        | y <- [ny - yi + 1 | yi <- [1 .. ny]]
                      ];

strVec3 :: Vec3 -> String
strVec3 v = show (floor (x v)) ++ " " ++ show (floor (y v)) ++ " " ++ show (floor (z v))

strRow :: Row -> String
strRow [] = ""
strRow (p:ps) = strVec3 p ++ "\n" ++ strRow ps

strImage :: Image -> String
strImage [] = ""
strImage (r:rs) = strRow r ++ strImage rs

main = do{
   putStrLn "P3";
   putStrLn "200 100";
   putStrLn "255";
   putStrLn (strImage (genImage 200 100));
}
