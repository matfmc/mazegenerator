# mazegenerator

<!-- badges: start -->
[![R-CMD-check](https://github.com/matfmc/mazegenerator/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/matfmc/mazegenerator/actions/workflows/R-CMD-check.yaml)
[![pkgdown](https://github.com/matfmc/mazegenerator/actions/workflows/pkgdown.yaml/badge.svg)](https://matfmc.github.io/mazegenerator/)
[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE.md)
<!-- badges: end -->

**mazegenerator** is a small educational R package for generating, visualizing,
animating, and solving rectangular mazes. Maze generation uses randomized
depth-first search with backtracking; pathfinding uses A-star search.

![Animated generation of a maze](man/figures/animation.gif)

## Installation

Install the development version from GitHub:

```r
install.packages("remotes")
remotes::install_github("matfmc/mazegenerator")
```

The core package uses only base R. Animated GIF export additionally requires
[`magick`](https://docs.ropensci.org/magick/):

```r
install.packages("magick")
```

## Generate and plot a maze

```r
library(mazegenerator)

maze <- generate_maze(rows = 20, cols = 20, seed = 99)
plot_maze(maze)
```

![A generated maze](man/figures/maze.png)

Generation is reproducible when `seed` is supplied. The returned `maze` object
contains the traversal history and the passages carved between cells.

## Solve it

```r
path <- solve_maze(maze)
plot_maze(maze, solution = path)
```

![A solved maze](man/figures/solved-maze.png)

`solve_maze()` can also find a path through a numeric or logical matrix:

```r
board <- matrix(
  c(1, 1, 0, 0,
    0, 1, 1, 0,
    0, 0, 1, 1,
    0, 0, 0, 1),
  nrow = 4,
  byrow = TRUE
)

solve_maze(board, start = c(1, 1), end = c(4, 4))
```

## Animate the generation

```r
small_maze <- generate_maze(12, 12, seed = 7)
animate_maze(small_maze, "maze-animation.gif", fps = 12)
```

For a large maze, use `every` to render fewer frames:

```r
animate_maze(maze, "maze-animation.gif", every = 4)
```

## How it works

The generator starts in one cell and repeatedly chooses a random unvisited
neighbor. When it reaches a dead end, it backtracks until another unvisited
neighbor is available. This produces a *perfect maze*: all cells are connected
and there is exactly one simple path between any two cells.

See the [algorithm article](https://matfmc.github.io/mazegenerator/articles/how-it-works.html)
for a visual explanation and the complete reproducible example.

## License

MIT © 2026 Matheus. See [LICENSE.md](LICENSE.md).
