test_that("maze plotting writes a graphics file", {
  maze <- generate_maze(4, 4, seed = 1)
  solution <- solve_maze(maze)
  output <- tempfile(fileext = ".png")

  png(output, width = 500, height = 500)
  expect_invisible(plot_maze(maze, solution = solution, show_path = TRUE))
  dev.off()

  expect_true(file.exists(output))
  expect_gt(file.info(output)$size, 0)
})

test_that("animation gives an actionable dependency message", {
  maze <- generate_maze(2, 2, seed = 1)
  if (!requireNamespace("magick", quietly = TRUE)) {
    expect_error(animate_maze(maze), "Package `magick` is required")
  } else {
    skip("Dependency is installed; full animation is exercised by examples.")
  }
})
