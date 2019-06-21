import Data.Maybe
import System.Random

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

lerp :: Vec3 -> Vec3 -> Float -> Vec3
lerp x y t = add (scale x (1-t)) (scale y t)

hitColor :: StdGen -> (Maybe HitRecord) -> Ray -> [Hitable] -> Vec3
hitColor _ Nothing r _ = lerp (Vec3 1.0 1.0 1.0) (Vec3 0.5 0.7 1.0) (0.5 * ((y (normalize (direction r))) + 1))
hitColor r (Just rec) _ w = let (u, r') = randomInUnitSphere r
                                target = add (add (position rec) (normal rec)) u
                            in scale (color_ (Ray (position rec) (add target (negate3 (position rec)))) r') 0.5

randomInUnitSphere :: StdGen -> (Vec3, StdGen)
randomInUnitSphere r = let range = (-1.0, 1.0)
                           (x, r') = randomR range r
                           (y, r'') = randomR range r'
                           (z, r''') = randomR range r''
                           v = Vec3 x y z
                       in if squared_length v < 1.0 then (v, r''') else randomInUnitSphere r'''

color_ :: Ray -> StdGen -> Vec3
color_ r g = let world = [(Sphere (Vec3 0.0 0.0 (-1.0)) 0.5), (Sphere (Vec3 0.0 (-100.5) (-1.0)) 100.0)]
            in (hitColor g (getClosestHit world r 0 10000000) r world)

color :: Ray -> StdGen -> Vec3
color r g = let v = color_ r g
            in scale (Vec3 (sqrt $ x v) (sqrt $ y v) (sqrt $ z v)) 255.99


average3_ :: [Vec3] -> Vec3 -> Int -> Vec3
average3_ [] a n = scale a (1.0 / fromIntegral n)
average3_ (x:xs) a n = average3_ xs (add a x) (n+1)

average3 :: [Vec3] -> Vec3
average3 [] = Vec3 0.0 0.0 0.0
average3 xs = average3_ xs (Vec3 0.0 0.0 0.0) 0

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

closestTime :: Float -> Maybe HitRecord -> Float
closestTime t Nothing = t
closestTime t (Just x) = min t (time x)

closestHit :: Maybe HitRecord -> Maybe HitRecord -> Maybe HitRecord
closestHit Nothing Nothing = Nothing
closestHit (Just x) Nothing = (Just x)
closestHit Nothing (Just x) = (Just x)
closestHit (Just x) (Just y) = if (time x) < (time y) then (Just x) else (Just y)

getClosestHit :: [Hitable] -> Ray -> Float -> Float -> Maybe HitRecord
getClosestHit [] _ _ _ = Nothing
getClosestHit (x:xs) r tMin tMax = let this = (hit x r tMin tMax)
                                   in if isNothing this
                                      then getClosestHit xs r tMin tMax
                                   else let other = getClosestHit xs r tMin (closestTime tMax this)
                                        in closestHit this other

data Camera = Camera { lower_left :: Vec3, horizontal :: Vec3, vertical :: Vec3, imgOrigin :: Vec3 }
camera = Camera (Vec3 (-2.0) (-1.0) (-1.0)) (Vec3 4.0 0.0 0.0) (Vec3 0.0 2.0 0.0) (Vec3 0.0 0.0 0.0)

getRay :: Camera -> Float -> Float -> Ray
getRay c u v = Ray (imgOrigin c) (add (lower_left c) (add (scale (horizontal c) u) (add (scale (vertical c) v) (negate3 (imgOrigin c)))))

rands :: StdGen -> [StdGen]
rands r = let (r', r'') = split r in (r':rands r'')

coords :: StdGen -> Integer -> Integer -> Integer -> [(StdGen, Integer, Integer)]
coords g x nx ny = if x == nx
                   then if ny == 0
                        then []
                        else coords (fst $ split g) 0 nx (ny-1)
                   else ((g, x, ny):coords (fst $ split g) (x+1) nx ny)

genImage :: StdGen -> Integer -> Integer -> IO Image
genImage r nx ny = let (r1, r2) = split r
                 in return [
                      [ average3 [color (getRay camera (((fromIntegral x) + u) / fromIntegral nx) (((fromIntegral y) + v) / fromIntegral ny)) r'
                                 | (u, v) <- take 10 (zip (randoms r1 :: [Float]) (randoms r2 :: [Float]))]]
                      | (r', x, y) <- coords r 0 nx ny
                    ];

strVec3 :: Vec3 -> String
strVec3 v = show (floor (x v)) ++ " " ++ show (floor (y v)) ++ " " ++ show (floor (z v))

strRow :: Row -> String
strRow [] = ""
strRow (p:ps) = strVec3 p ++ "\n" ++ strRow ps

strImage :: Image -> String
strImage [] = ""
strImage (r:rs) = strRow r ++ strImage rs

main = do {
   putStrLn "P3";
   putStrLn "200 100";
   putStrLn "255";
   stdGen <- getStdGen;
   img <- genImage stdGen 200 100;
   putStrLn $ strImage img
}
