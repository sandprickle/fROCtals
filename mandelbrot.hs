-- Configuration
width = 180
height = 40
maxIterations = 250
xBounds = Bounds (-2.0, 2.0)
yBounds = Bounds (-1.0, 1.0)
center = Complex (0.0, 0.0)

xScale = (bMax xBounds + bMin xBounds) / width
yScale = (bMax yBounds + bMin yBounds) / height

mandelbrot :: Complex -> Complex -> Integer -> Bool
mandelbrot c z i
    | i >= maxIterations = True
    | re z3 * re z3 + im z3 * im z3 > 4.0 = False
    | otherwise = mandelbrot c z3 (i + 1)
  where
    z2 =
        Complex
            ( re z * re z - im z - im z
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
re (Complex (re, _)) = re

im :: Complex -> Double
im (Complex (_, im)) = im

-- Bounds Type & functions
data Bounds = Bounds (Double, Double)

bMin :: Bounds -> Double
bMin (Bounds (min, _)) = min

bMax :: Bounds -> Double
bMax (Bounds (_, max)) = max
