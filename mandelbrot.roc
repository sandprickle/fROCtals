app "roc-mandelbrot"
    packages { pf: "cli-platform/main.roc" }
    imports [pf.Stdout, pf.Task.{ Task, await }]
    provides [main] to pf

cfg : {
    width : Nat,
    height : Nat,
    maxIterations : Nat,
    xBounds : { min : F64, max : F64 },
    yBounds : { min : F64, max : F64 },
    center : Complex,
}
cfg = {
    width: 180,
    height: 40,
    maxIterations: 1_000,
    xBounds: { min: -2.0, max: 2.0 },
    yBounds: { min: -1.0, max: 1.0 },
    center: { re: -0.5, im: 0.0 },
}

main : List Str -> Task {} [] [Write [Stdout]]
main = \_ ->
    Stdout.line (Str.joinWith plot "\n")

plot : List Str
plot =
    row <- List.map points

    chars = List.map row \val ->
        if val then
            "*"
        else
            " "

    Str.joinWith chars ""

points : List (List Bool)
points =
    template =
        List.repeat { re: 0.0, im: 0.0 } cfg.width
        |> List.repeat cfg.height

    row, y <- List.mapWithIndex template

    _, x <- List.mapWithIndex row

    xScale =
        (cfg.xBounds.max - cfg.xBounds.min)
        / Num.toFrac cfg.width

    yScale =
        (cfg.yBounds.max - cfg.yBounds.min)
        / Num.toFrac cfg.height

    currentPoint = {
        re: Num.toFrac x * xScale + cfg.xBounds.min + cfg.center.re,
        im: Num.toFrac y * yScale + cfg.yBounds.min + cfg.center.im,
    }

    mandelbrot currentPoint { re: 0.0, im: 0.0 } 0

Complex : {
    re : F64,
    im : F64,
}

mandelbrot : Complex, Complex, Nat -> Bool
mandelbrot = \c, z, i ->
    if i >= cfg.maxIterations then
        True
    else
        z2 = {
            re: z.re * z.re - z.im * z.im,
            im: 2.0 * z.re * z.im,
        }
        z3 = {
            re: z2.re + c.re,
            im: z2.im + c.im,
        }

        if z3.re * z3.re + z3.im * z3.im > 4.0 then
            False
        else
            mandelbrot c z3 (i + 1)
