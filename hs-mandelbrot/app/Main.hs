width :: Int
width = 180

height :: Int
height = 40

maxIterations :: Int
maxIterations = 250

xBounds :: Bounds
xBounds = Bounds (-2.0, 2.0)

yBounds :: Bounds
yBounds = Bounds (-1.0, 1.0)

center :: Complex
center = Complex (0.0, 0.0)

xScale :: Double
xScale = (bMax xBounds + bMin xBounds) / fromIntegral width

yScale :: Double
yScale = (bMax yBounds + bMin yBounds) / fromIntegral height

main :: IO ()
main = putStr plot
  where
    plot =
        foldl (\list row -> list ++ row ++ ['\n']) [] grid

template :: [(Integer, [(Integer, Complex)])]
template =
    zip
        [0 .. toInteger height]
        ( replicate
            height
            (zip [0 .. toInteger width] (replicate width origin))
        )

grid :: [[Char]]
grid =
    map
        ( \(y, row) ->
            ( map
                ( \(x, _) ->
                    if ( mandelbrot
                            ( Complex
                                ( fromIntegral x * xScale + bMin xBounds + re center
                                , fromIntegral y * yScale + bMin yBounds + im center
                                )
                            )
                            origin
                            0
                       )
                        then '*'
                        else '?'
                )
                row
            )
        )
        template

mandelbrot :: Complex -> Complex -> Int -> Bool
mandelbrot c z i
    | i >= maxIterations = True
    | re z3 * re z3 + im z3 * im z3 > 4.0 = False
    | otherwise = mandelbrot c z3 (i + 1)
  where
    z2 =
        Complex
            ( re z * re z - im z * im z
            , 2.0 * re z * im z
            )
    z3 =
        Complex
            ( re z2 + re c
            , im z2 + im c
            )

-- Complex Type & functions
data Complex = Complex (Double, Double)

re :: Complex -> Double
re (Complex (val, _)) = val

im :: Complex -> Double
im (Complex (_, val)) = val

origin :: Complex
origin = Complex (0.0, 0.0)

-- Bounds Type & functions
data Bounds = Bounds (Double, Double)

bMin :: Bounds -> Double
bMin (Bounds (m, _)) = m

bMax :: Bounds -> Double
bMax (Bounds (_, m)) = m
