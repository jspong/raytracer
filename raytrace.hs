import Data.Maybe
import System.Random

-- Vec3 operations

data Vec3 = Vec3 { x :: Float, y :: Float, z :: Float } deriving (Show)
type Image = [Vec3]

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
normalize v = scale v (1 / len v)

lerp :: Vec3 -> Vec3 -> Float -> Vec3
lerp x y t = add (scale x (1-t)) (scale y t)

average3_ :: [Vec3] -> Vec3 -> Int -> Vec3
average3_ [] a n = scale a (1.0 / fromIntegral n)
average3_ (x:xs) a n = average3_ xs (add a x) (n+1)

average3 :: [Vec3] -> Vec3
average3 [] = Vec3 0.0 0.0 0.0
average3 xs = average3_ xs (Vec3 0.0 0.0 0.0) 0

-- Ray operations

data Ray = Ray { origin :: Vec3
               , direction :: Vec3 } deriving (Show)

pointAt :: Ray -> Float -> Vec3
pointAt r t = add (origin r) (scale (direction r) t)

-- Collision Physics

data Hitable = Sphere { center :: Vec3 , radius :: Float , material :: Material}

data HitRecord = HitRecord { time :: Float, position :: Vec3, normal :: Vec3 , material_ :: Material }

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
                               then let p = pointAt r sol1
                                        normal = scale (add p (negate3 (center s))) (1.0 / (radius s))
                                    in Just (HitRecord sol1 p normal (material s))
                               else if sol2 < tMax && sol2 > tMin
                                    then let p = pointAt r sol2
                                             normal = scale (add p (negate3 (center s))) (1.0 / (radius s))
                                         in Just (HitRecord sol2 p normal (material s))
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

-- Image Generation

coords :: StdGen -> Integer -> Integer -> Integer -> [(StdGen, Integer, Integer)]
coords g x nx ny = if x == nx
                   then if ny == 0
                        then []
                        else coords (fst $ split g) 0 nx (ny-1)
                   else ((g, x, ny):coords (fst $ split g) (x+1) nx ny)

toCoord :: Integer -> Integer -> Float -> Float
toCoord x nx u = ((fromIntegral x) + u) / (fromIntegral nx)

genImage :: StdGen -> Integer -> Integer -> [Hitable] -> Image
genImage g nx ny world = [ let us = randoms (snd $ split g') :: [Float]
                               vs = randoms (snd $ split (snd $ split g')) :: [Float]
                     in average3 [ color (getRay camera (toCoord x nx u) (toCoord y ny v)) g' world | (u, v) <- take 10 (zip us vs) ]
                   | (g', x, y) <- coords g 0 nx ny ];

strVec3 :: Vec3 -> String
strVec3 v = show (floor (x v)) ++ " " ++ show (floor (y v)) ++ " " ++ show (floor (z v))

strImage :: Image -> String
strImage [] = ""
strImage (x:xs) = strVec3 x ++ "\n" ++ strImage xs

-- Materials

data Material = Lambertian { albedo :: Vec3 }
              | Metal { albedo :: Vec3 , fuzz :: Float }
              | Dialectric { ri :: Float }

randomInUnitSphere :: StdGen -> (Vec3, StdGen)
randomInUnitSphere g = let range = (-1.0, 1.0)
                           (x, g') = randomR range g
                           (y, g'') = randomR range g'
                           (z, g''') = randomR range g''
                           v = Vec3 x y z
                       in if squared_length v < 1.0 then (v, g''') else randomInUnitSphere g'''

reflect :: Vec3 -> Vec3 -> Vec3
reflect v n = add v (scale n (-2 * dot v n))

refract :: Vec3 -> Vec3 -> Float -> Maybe Vec3
refract v n ni_over_nt = let uv = normalize v
                             dt = dot uv n
                             discriminant = 1.0 - ni_over_nt * ni_over_nt * (1 - dt * dt)
                         in if discriminant > 0
                            then let a = scale (add uv (scale n (-1.0 * dt))) ni_over_nt
                                     b = scale n (-1.0 * sqrt discriminant)
                                 in Just(add a b)
                            else Nothing

schlick :: Float -> Float -> Float
schlick c i = let r0 = (1.0 - i) / (1.0 + i)
                  r1 = r0 * r0
              in r1 + (1.0 - r1) * ((1.0 - c) ** 5.0)

glass :: StdGen -> HitRecord -> (Maybe Vec3) -> Vec3 -> Float -> Float -> (Maybe (Vec3, Ray), StdGen)
glass g hr Nothing reflected _ _ = (Just (Vec3 1.0 1.0 1.0, Ray (position hr) reflected), g)
glass g hr (Just refracted) reflected cosine i = let (ref, g') = random g
                                                     dir = if ref < schlick cosine i then reflected else refracted
                                                 in (Just (Vec3 1.0 1.0 1.0, Ray (position hr) dir), g')

scatter :: StdGen -> Material -> Ray -> HitRecord -> (Maybe (Vec3, Ray), StdGen)
scatter g (Lambertian a) r hr = let (u, g') = randomInUnitSphere g
                                    target = add (add (position hr) (normal hr)) u
                                    scattered = Ray (position hr) (add target (negate3 (position hr)))
                                in (Just (a, scattered), g')
scatter g (Metal a f) r hr = let reflected = reflect (normalize $ direction r) (normal hr)
                                 (u, g') = randomInUnitSphere g
                                 scattered = Ray (position hr) (add reflected (scale u f))
                                 d = dot reflected (normal hr)
                             in (if d > 0 then Just (a, scattered) else Nothing, g')
scatter g (Dialectric i) r hr = let reflected = reflect (direction r) (normal hr)
                                    discriminant = dot (direction r) (normal hr)
                                    outward_normal = if discriminant > 0
                                                     then scale (normal hr) (-1.0)
                                                     else normal hr
                                    ni_over_nt = if discriminant > 0 then i else 1.0 / i
                                    cosine = if discriminant > 0
                                             then i * (dot (direction r) (normal hr)) / len (direction r)
                                             else (-1.0) * (dot (direction r) (normal hr)) / len (direction r)
                                    refracted = refract (direction r) outward_normal ni_over_nt
                                in glass g hr refracted reflected cosine i

-- Rendering Colors

render :: StdGen -> Maybe (Vec3, Ray) -> [Hitable] -> Integer -> (Vec3, StdGen)
render g Nothing _ _ = (Vec3 0.0 0.0 0.0, g)
render g (Just (v, r)) world d = let (c, g') = color_ r g world (d-1)
                                 in (multiply v c, g')

hitColor :: StdGen -> (Maybe HitRecord) -> Ray -> [Hitable] -> Integer -> (Vec3, StdGen)
hitColor g _ _ _ 0 = (Vec3 0.0 0.0 0.0, g)
hitColor g Nothing r _ _ = (lerp (Vec3 1.0 1.0 1.0) (Vec3 0.5 0.7 1.0) (0.5 * ((y (normalize (direction r))) + 1)), g)
hitColor g (Just rec) r w d = let (scattered, g') = scatter g (material_ rec) r rec
                              in render g' scattered w d

color_ :: Ray -> StdGen -> [Hitable] -> Integer -> (Vec3, StdGen)
color_ r g world d = hitColor g (getClosestHit world r 0.00001 1000000) r world d

color :: Ray -> StdGen -> [Hitable] -> Vec3
color r g world = let (v, _) = color_ r g world 50
                    in scale (Vec3 (sqrt $ x v) (sqrt $ y v) (sqrt $ z v)) 255.99

data Camera = Camera { lower_left :: Vec3, horizontal :: Vec3, vertical :: Vec3, imgOrigin :: Vec3 }
camera = Camera (Vec3 (-2.0) (-1.0) (-1.0)) (Vec3 4.0 0.0 0.0) (Vec3 0.0 2.0 0.0) (Vec3 0.0 0.0 0.0)

getRay :: Camera -> Float -> Float -> Ray
getRay c u v = Ray (imgOrigin c) (add (lower_left c) (add (scale (horizontal c) u) (add (scale (vertical c) v) (negate3 (imgOrigin c)))))

rands :: StdGen -> [StdGen]
rands g = let (g', g'') = split g in (g':rands g'')

main = do {
   putStrLn "P3";
   putStrLn "400 200";
   putStrLn "255";
   stdGen <- getStdGen;
   world <- return [(Sphere (Vec3 0.0 0.0 (-1.0)) 0.5 (Lambertian $ Vec3 0.8 0.3 0.3)),
                    (Sphere (Vec3 0.0 (-100.5) (-1.0)) 100.0 (Lambertian $ Vec3 0.8 0.8 0.0)),
                    (Sphere (Vec3 1.0 0.0 (-1.0)) 0.5 (Metal (Vec3 0.8 0.6 0.2) 0.3)),
                    (Sphere (Vec3 (-1.0) 0.0 (-1.0)) 0.5 (Dialectric 1.5)),
                    (Sphere (Vec3 (-1.0) 0.0 (-1.0)) (-0.45) (Dialectric 1.5))];
   putStrLn $ strImage $ genImage stdGen 400 200 world ;
}
