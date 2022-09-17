-- Configuration
width = 180
height = 40
maxIterations = 250
xBounds = (-2.0, 2.0)
yBounds = (-1.0, 1.0)
center = Complex (0.0, 0.0)

xScale = ( snd xBounds + fst xBounds ) / width
yScale = ( snd yBounds + fst yBounds ) / height


mandelbrot :: Complex-> Complex -> Integer -> Bool
mandelbrot c z i  
    | i <= maxIterations = True
    | otherwise = False
    



data Complex = Complex (Double, Double)

real :: Complex -> Double
real (Complex (re, _)) = re

imaginary :: Complex -> Double
imaginary (Complex (_, im)) = im
