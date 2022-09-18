const CONFIG: Config = Config {
    x_bounds: Bounds {
        min: -2.0,
        max: 2.0,
    },
    y_bounds: Bounds {
        min: -1.0,
        max: 1.0,
    },
    center: Complex { re: -0.5, im: 0.0 },
    max_iterations: 1_000,
    height: 40,
    width: 180,
};

const X_SCALE: f64 = (CONFIG.x_bounds.max - CONFIG.x_bounds.min) / (CONFIG.width as f64);
const Y_SCALE: f64 = (CONFIG.y_bounds.max - CONFIG.y_bounds.min) / (CONFIG.height as f64);

fn main() {
    let mut grid = [['?'; CONFIG.width]; CONFIG.height];

    for (row_i, row) in grid.into_iter().enumerate() {
        for (col_i, _) in row.into_iter().enumerate() {
            let c = Complex {
                re: col_i as f64 * X_SCALE + CONFIG.x_bounds.min + CONFIG.center.re,
                im: row_i as f64 * Y_SCALE + CONFIG.y_bounds.min + CONFIG.center.im,
            };

            grid[row_i][col_i] = if mandelbrot(c) { '*' } else { ' ' }
        }
    }

    for row in grid {
        println!("{}", String::from_iter(row))
    }
}

fn mandelbrot(c: Complex) -> bool {
    let mut z = Complex { re: 0.0, im: 0.0 };
    let mut iter_count = 0;

    while z.re + z.im <= 4.0 && iter_count < CONFIG.max_iterations {
        z = Complex {
            re: z.re * z.re - z.im * z.im,
            im: 2.0 * z.re * z.im,
        };

        z = Complex {
            re: z.re + c.re,
            im: z.im + c.im,
        };

        iter_count += 1;
    }

    iter_count == CONFIG.max_iterations
}
struct Complex {
    re: f64,
    im: f64,
}

struct Bounds {
    min: f64,
    max: f64,
}

struct Config {
    x_bounds: Bounds,
    y_bounds: Bounds,
    center: Complex,
    max_iterations: usize,
    height: usize,
    width: usize,
}
