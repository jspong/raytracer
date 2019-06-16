type Pixel = (Float, Float, Float)
type Row = [Pixel]
type Image = [Row]

convert :: Integer -> Integer -> Float
convert x nx = 255.99 * fromIntegral x / fromIntegral nx

genImage :: Integer -> Integer -> Image
genImage nx ny = [
   [ (convert x nx, convert y ny, convert 1 5 ) | x <- [0 .. nx-1]]
   | y <- [ny - yi + 1 | yi <- [1 .. ny]]
                 ]

strPixel :: Pixel -> String
strPixel (r, g, b) = show (floor r) ++ " " ++ show (floor g) ++ " " ++ show (floor b)

strRow :: Row -> String
strRow [] = ""
strRow (p:ps) = strPixel p ++ "\n" ++ strRow ps

strImage :: Image -> String
strImage [] = ""
strImage (r:rs) = strRow r ++ strImage rs

main = do{
   putStrLn "P3";
   putStrLn "200 100";
   putStrLn "255";
   putStrLn (strImage (genImage 200 100));
}
