const cfg = {
  xBounds: { min: -2.0, max: 2.0 },
  yBounds: { min: -1.0, max: 1.0 },
  center: { re: -0.5, im: 0 },
  maxIterations: 1000,
  height: 40,
  width: 180,
}

const xScale = (cfg.xBounds.max - cfg.xBounds.min) / cfg.width
const yScale = (cfg.yBounds.max - cfg.yBounds.min) / cfg.height

function mandelbrot(c) {
  let z = { re: 0, im: 0 }
  let iterCount = 0

  while (z.re + z.im <= 4 && iterCount < cfg.maxIterations) {
    z = {
      re: z.re * z.re - z.im * z.im,
      im: 2 * z.re * z.im,
    }
    z = {
      re: z.re + c.re,
      im: z.im + c.im,
    }
    iterCount++
  }

  return (iterCount === cfg.maxIterations)
}

function plot() {
  let grid = new Array(cfg.height);

  for (let row = 0; row < cfg.height; row++) {
    grid[row] = new Array(cfg.width)

    for (let col = 0; col < cfg.width; col++) {
      c = {
        re: col * xScale + cfg.xBounds.min + cfg.center.re,
        im: row * yScale + cfg.yBounds.min + cfg.center.im,
      }
      grid[row][col] = mandelbrot(c) ? "*" : " "
    }
  }

  for (let row = 0; row < cfg.height; row++) {
    grid[row] = grid[row].join('')
  }

  return grid.join("\n")
}

console.log(plot())
