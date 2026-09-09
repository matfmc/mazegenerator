test_that("tracing records every expansion and is off by default", {
  maze <- generate_maze(10, 10, seed = 7)

  expect_null(attr(solve_maze(maze), "search_trace"))

  solution <- solve_maze(maze, trace = TRUE)
  trace <- attr(solution, "search_trace")

  expect_s3_class(trace, "maze_search_trace")
  expect_equal(trace$rows, maze$rows)
  expect_equal(trace$cols, maze$cols)
  expect_equal(trace$start, maze$start)
  expect_equal(trace$end, maze$end)
  expect_gt(length(trace$steps), 0)
  expect_equal(trace$steps[[length(trace$steps)]]$current, maze$end)
})

test_that("the trace replays into a consistent open and closed set", {
  maze <- generate_maze(12, 12, seed = 99)
  solution <- solve_maze(maze, trace = TRUE)
  trace <- attr(solution, "search_trace")

  closed <- matrix(FALSE, nrow = trace$rows, ncol = trace$cols)
  open <- matrix(FALSE, nrow = trace$rows, ncol = trace$cols)
  open[trace$start[1L], trace$start[2L]] <- TRUE

  for (step in trace$steps) {
    current <- step$current
    # every expanded cell must be on the frontier and not already expanded
    expect_true(open[current[1L], current[2L]])
    expect_false(closed[current[1L], current[2L]])
    open[current[1L], current[2L]] <- FALSE
    closed[current[1L], current[2L]] <- TRUE
    if (nrow(step$opened) > 0L) {
      expect_false(any(closed[step$opened]))
      open[step$opened] <- TRUE
    }
  }

  # the whole solution lies inside the explored region
  expect_true(all(closed[as.matrix(solution)]))
})

test_that("cell_fill shades cells without breaking the plot", {
  maze <- generate_maze(6, 6, seed = 1)
  fill <- data.frame(row = 1:3, column = 1L, color = "#bae6fd")

  file <- tempfile(fileext = ".png")
  grDevices::png(file, width = 200, height = 200)
  expect_silent(plot_maze(maze, cell_fill = fill, main = "custom title"))
  grDevices::dev.off()
  expect_gt(file.size(file), 0)

  expect_error(
    plot_maze(maze, cell_fill = data.frame(row = 1, column = 1)),
    "cell_fill"
  )
})

test_that("animate_solution validates its inputs", {
  skip_if_not_installed("magick")

  maze <- generate_maze(6, 6, seed = 1)

  expect_error(animate_solution("not a maze"), "generate_maze")
  expect_error(
    animate_solution(maze, solution = solve_maze(maze)),
    "trace = TRUE"
  )
  expect_error(
    animate_solution(maze, output_file = file.path(tempdir(), "no-dir", "x.gif")),
    "does not exist"
  )
})

test_that("animate_solution writes a GIF", {
  skip_if_not_installed("magick")

  maze <- generate_maze(6, 6, seed = 1)
  output <- tempfile(fileext = ".gif")

  expect_message(
    animate_solution(maze, output_file = output, width = 150L, height = 150L,
                     hold = 2L, progress = FALSE),
    "Animation saved"
  )
  expect_true(file.exists(output))
  expect_gt(file.size(output), 0)
})
