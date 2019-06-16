type Vec3 = (Float, Float, Float)
type Row = [Vec3]
type Image = [Row]

add :: Vec3 -> Vec3 -> Vec3
add (a1, b1, c1) (a2, b2, c2) = (a1+a2, b1+b2, c1+c2)

negate :: Vec3 -> Vec3
negate (a,b,c) = (-a, -b, -c)

multiply :: Vec3 -> Vec3 -> Vec3
multiply (a1, b1, c1) (a2, b2, c2) = (a1*a2, b1*b2, c1*c2)

divide :: Vec3 -> Vec3 -> Vec3
divide (a1, b1, c1) (a2, b2, c2) = (a1/a2, b1/b2, c1/c2)

scale :: Vec3 -> Float -> Vec3
scale (a, b, c) x = (a*x, b*x, c*x)

dot :: Vec3 -> Vec3 -> Float
dot (a1, b1, c1) (a2, b2, c2) = a1*a2 + b1*b2 + c1*c2

cross :: Vec3 -> Vec3 -> Vec3
cross (a1, b1, c1) (a2, b2, c2) = (b1*c2-c1*b2, c1*a2-a1*c2, a1*b2-a2*b1)

squared_length :: Vec3 -> Float
squared_length (a, b, c) = a*a + b*b + c*c

len = sqrt . squared_length

normalize :: Vec3 -> Vec3
normalize (a, b, c) = (a / l, b / l, c / l) where l = len (a,b,c)


convert :: Integer -> Integer -> Float
convert x nx = 255.99 * fromIntegral x / fromIntegral nx

genImage :: Integer -> Integer -> Image
genImage nx ny = [
   [ (convert x nx, convert y ny, convert 1 5 ) | x <- [0 .. nx-1]]
   | y <- [ny - yi + 1 | yi <- [1 .. ny]]
                 ]

strVec3 :: Vec3 -> String
strVec3 (r, g, b) = show (floor r) ++ " " ++ show (floor g) ++ " " ++ show (floor b)

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
