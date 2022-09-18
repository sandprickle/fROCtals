width :: Int
width = 180

height :: Int
height = 40

maxIterations :: Int
maxIterations = 1000

xBounds :: Bounds
xBounds = Bounds (-2.0, 2.0)

yBounds :: Bounds
yBounds = Bounds (-1.0, 1.0)

center :: Complex
center = Complex (-0.5, 0.0)

xScale :: Double
xScale =
    (upperBound xBounds - lowerBound xBounds) / fromIntegral width

yScale :: Double
yScale =
    (upperBound yBounds - lowerBound yBounds) / fromIntegral height

main :: IO ()
main = putStr plot
  where
    plot =
        foldl (\list row -> list ++ row ++ ['\n']) [] grid

template :: [(Integer, [(Integer, Complex)])]
template =
    zip
        [0 ..]
        ( replicate
            height
            (zip [0 ..] (replicate width origin))
        )

grid :: [[Char]]
grid =
    map
        ( \(y, row) ->
            ( map
                ( \(x, _item) ->
                    mandelbrot
                        ( Complex
                            ( fromIntegral x
                                * xScale
                                + lowerBound xBounds
                                + re center
                            , fromIntegral y
                                * yScale
                                + lowerBound yBounds
                                + im center
                            )
                        )
                        origin
                        0
                )
                row
            )
        )
        template

mandelbrot :: Complex -> Complex -> Int -> Char
mandelbrot c z i
    | i >= maxIterations = '*'
    | re z3 * re z3 + im z3 * im z3 > 4.0 = ' '
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
re (Complex (real, _)) = real

im :: Complex -> Double
im (Complex (_, imaginary)) = imaginary

origin :: Complex
origin = Complex (0.0, 0.0)

-- Bounds Type & functions
data Bounds = Bounds (Double, Double)

lowerBound :: Bounds -> Double
lowerBound (Bounds (lower, _)) = lower

upperBound :: Bounds -> Double
upperBound (Bounds (_, upper)) = upper
