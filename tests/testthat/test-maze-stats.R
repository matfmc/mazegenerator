.build_maze <- function(rows, cols, start, end, cell_order, algorithm = "dfs", open_walls = 0) {
  # `cell_order` is a list of c(row, column) cells describing a walk that
  # carves a passage between each consecutive pair -- enough to hand-build a
  # small maze with a fully known, deterministic structure.
  passages <- vapply(
    seq_len(length(cell_order) - 1L),
    function(i) mazegenerator:::.edge_key(cell_order[[i]], cell_order[[i + 1L]]),
    character(1)
  )
  structure(
    list(
      rows = rows,
      cols = cols,
      start = as.integer(start),
      end = as.integer(end),
      algorithm = algorithm,
      traversal = NULL,
      visitation_order = NULL,
      carve_order = passages,
      passages = passages,
      seed = NULL,
      open_walls = open_walls,
      walls_removed = 0L
    ),
    class = "maze"
  )
}

test_that("maze_stats requires a maze object", {
  expect_error(maze_stats(matrix(1, 2, 2)), "generate_maze")
})

test_that("dead_ends of a 1xN corridor is 2", {
  cells <- lapply(1:5, function(c) c(1L, c))
  maze <- .build_maze(1, 5, c(1, 1), c(1, 5), cells)
  stats <- maze_stats(maze)

  expect_s3_class(stats, "maze_stats")
  expect_equal(stats$dead_ends, 2L)
  expect_equal(stats$dead_end_pct, 2 / 5)
  expect_equal(stats$degree1, 2L)
  expect_equal(stats$degree2, 3L)
  expect_equal(stats$degree3, 0L)
  expect_equal(stats$degree4, 0L)
  expect_equal(stats$corridor_mean, 4)
  expect_equal(stats$corridor_max, 4)
  expect_equal(stats$diameter, 4L)
  expect_equal(stats$solution_length, 5L)
  expect_equal(stats$solution_pct, 1)
  # A straight-line corridor never turns, so every step continues in the
  # same direction as the one before it.
  expect_equal(stats$river, 1)
})

test_that("known values on a small hand-built serpentine maze", {
  # A 3x3 Hamiltonian serpentine path through every cell, with two turns
  # per "row" of the S-shape and no branching at all.
  cells <- list(
    c(1, 1), c(1, 2), c(1, 3),
    c(2, 3), c(2, 2), c(2, 1),
    c(3, 1), c(3, 2), c(3, 3)
  )
  maze <- .build_maze(3, 3, c(1, 1), c(3, 3), cells)
  stats <- maze_stats(maze)

  expect_equal(stats$rows, 3L)
  expect_equal(stats$cols, 3L)
  expect_equal(stats$algorithm, "dfs")
  expect_equal(stats$open_walls, 0)

  expect_equal(stats$dead_ends, 2L)
  expect_equal(stats$dead_end_pct, 2 / 9)
  expect_equal(stats$degree1, 2L)
  expect_equal(stats$degree2, 7L)
  expect_equal(stats$degree3, 0L)
  expect_equal(stats$degree4, 0L)
  expect_equal(stats$degree1 + stats$degree2 + stats$degree3 + stats$degree4, 9L)

  expect_equal(stats$corridor_mean, 8)
  expect_equal(stats$corridor_max, 8)
  expect_equal(stats$diameter, 8L)
  expect_equal(stats$solution_length, 9L)
  expect_equal(stats$solution_pct, 1)

  # Directions along the walk: right, right, down, left, left, down, right,
  # right -- 7 transitions, 3 of which continue straight.
  expect_equal(stats$river, 3 / 7)
})

test_that("as.data.frame.maze_stats produces a one-row data frame", {
  maze <- generate_maze(8, 8, seed = 3)
  stats <- maze_stats(maze)
  df <- as.data.frame(stats)

  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 1L)
  expect_true(all(c(
    "rows", "cols", "algorithm", "open_walls", "dead_ends", "dead_end_pct",
    "corridor_mean", "corridor_max", "degree1", "degree2", "degree3", "degree4",
    "diameter", "solution_length", "solution_pct", "river"
  ) %in% names(df)))
})

test_that("maze_stats invariants hold across perfect and braided mazes", {
  for (seed in c(1L, 7L, 42L)) {
    for (open_walls in c(0, 0.3)) {
      maze <- generate_maze(10, 10, seed = seed, open_walls = open_walls)
      stats <- maze_stats(maze)

      total_cells <- maze$rows * maze$cols
      expect_equal(
        stats$degree1 + stats$degree2 + stats$degree3 + stats$degree4,
        total_cells
      )
      expect_true(stats$dead_end_pct >= 0 && stats$dead_end_pct <= 1)
      expect_true(stats$solution_pct >= 0 && stats$solution_pct <= 1)
      expect_true(stats$solution_length >= 1L)
      expect_true(stats$diameter >= 0L)
      expect_true(stats$diameter <= total_cells - 1L)
      expect_true(stats$corridor_mean >= 1 || is.na(stats$corridor_mean))
      expect_true(stats$corridor_max >= stats$corridor_mean || is.na(stats$corridor_max))
      if (!is.na(stats$river)) {
        expect_true(stats$river >= 0 && stats$river <= 1)
      }
    }
  }
})

test_that("print.maze_stats runs without error and returns its argument invisibly", {
  maze <- generate_maze(6, 6, seed = 11)
  stats <- maze_stats(maze)
  expect_output(print(stats), "<maze_stats>", fixed = TRUE)
  expect_identical(print(stats), stats)
})
