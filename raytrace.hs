import Data.Maybe

data Vec3 = Vec3 { x :: Float, y :: Float, z :: Float } deriving (Show)
type Row = [Vec3]
type Image = [Row]

add :: Vec3 -> Vec3 -> Vec3
add v1 v2 = Vec3 (x v1 + x v2) (y v1 + y v2) (z v1 + z v2)

negate3 :: Vec3 -> Vec3
negate3 v = Vec3 (-x v) (-y v) (-z v)

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
color r = let t = getClosestHit [(Sphere (Vec3 0.0 0.0 (-1.0)) 0.5), (Sphere (Vec3 0.0 (-100.5) (-1.0)) 100.0)] r 0.0 10000.0
          in if (isJust t)
             then scale (add (normalize (add (pointAt r (time (fromJust t))) (Vec3 0.0 0.0 1.0))) (Vec3 1.0 1.0 1.0)) (0.5 * 255.99)
             else let t = 0.5 * (y (normalize (direction r)) + 1.0)
                  in scale (add (scale (Vec3 1.0 1.0 1.0) (1.0-t)) (scale (Vec3 0.5 0.7 1.0) t)) 255.99

data Hitable = Sphere { center :: Vec3 , radius :: Float }

data HitRecord = HitRecord { time :: Float, position :: Vec3, normal :: Vec3 }

hit :: Hitable -> Ray -> Float -> Float -> Maybe HitRecord
hit s r tMin tMax = let oc = add (origin r) (negate3 (center s))
                        a = dot (direction r) (direction r) 
                        b = dot oc (direction r)
                        c = (dot oc oc) - (radius s) * (radius s)
                        discriminant = b * b - a * c 
                    in if discriminant > 0
                       then let tmp = sqrt(b * b - a * c)
                                sol1 = (-b - tmp) / a
                                sol2 = (-b + tmp) / a
                            in if sol1 < tMax && sol1 > tMin
                               then Just (HitRecord sol1 (pointAt r sol1) (scale (add (pointAt r sol1) (center s)) (1 / radius s)))
                               else if sol2 < tMax && sol2 > tMin
                                    then Just (HitRecord sol2 (pointAt r sol2) (scale (add (pointAt r sol2) (center s)) (1 / radius s)))
                                    else Nothing
                       else Nothing

getClosestHit :: [Hitable] -> Ray -> Float -> Float -> Maybe HitRecord
getClosestHit [] _ _ _ = Nothing
getClosestHit (x:xs) r tMin tMax = let this = (hit x r tMin tMax)
                                   in if isNothing this
                                      then getClosestHit xs r tMin tMax
                                   else let other = getClosestHit xs r tMin (min (time (fromJust this)) tMax)
                                        in if isNothing other
                                           then this
                                           else if (time (fromJust this)) < (time (fromJust other)) then this else other 


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
