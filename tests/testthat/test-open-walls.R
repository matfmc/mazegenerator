test_that("open_walls = 0 reproduces default behaviour byte-for-byte", {
  m1 <- generate_maze(15, 15, seed = 42)
  m2 <- generate_maze(15, 15, seed = 42, open_walls = 0)

  expect_identical(m1$passages, m2$passages)
  expect_identical(m1$traversal, m2$traversal)
  expect_identical(m1$end, m2$end)
})

test_that("open_walls = 0 keeps the maze perfect", {
  rows <- 10L
  cols <- 8L
  m <- generate_maze(rows, cols, seed = 1, open_walls = 0)

  expect_equal(length(m$passages), rows * cols - 1L)
  expect_equal(m$walls_removed, 0L)
})

test_that("open_walls = 1 opens every internal wall", {
  rows <- 6L
  cols <- 7L
  m <- generate_maze(rows, cols, seed = 2, open_walls = 1)

  total_candidates <- rows * (cols - 1L) + (rows - 1L) * cols
  expect_equal(length(m$passages), total_candidates)

  # Every internal adjacent pair must have a passage.
  for (r in seq_len(rows)) {
    for (c in seq_len(cols)) {
      if (c < cols) {
        key <- mazegenerator:::.edge_key(c(r, c), c(r, c + 1L))
        expect_true(key %in% m$passages)
      }
      if (r < rows) {
        key <- mazegenerator:::.edge_key(c(r, c), c(r + 1L, c))
        expect_true(key %in% m$passages)
      }
    }
  }
})

test_that("intermediate open_walls yields a count strictly between bounds", {
  rows <- 12L
  cols <- 12L
  m <- generate_maze(rows, cols, seed = 3, open_walls = 0.3)

  perfect_count <- rows * cols - 1L
  total_candidates <- rows * (cols - 1L) + (rows - 1L) * cols

  expect_gt(length(m$passages), perfect_count)
  expect_lt(length(m$passages), total_candidates)
  expect_equal(m$walls_removed, length(m$passages) - perfect_count)
})

test_that("generation is reproducible for a fixed seed and open_walls", {
  m1 <- generate_maze(10, 10, seed = 99, open_walls = 0.25)
  m2 <- generate_maze(10, 10, seed = 99, open_walls = 0.25)
  m3 <- generate_maze(10, 10, seed = 99, open_walls = 0.6)

  expect_identical(m1$passages, m2$passages)
  expect_false(identical(m1$passages, m3$passages))
})

test_that("vectorised wall-candidate keys match .edge_key()", {
  rows <- 5L
  cols <- 5L
  keys <- mazegenerator:::.wall_candidate_keys(rows, cols)

  expected <- character()
  for (r in seq_len(rows)) {
    for (c in seq_len(cols)) {
      if (c < cols) {
        expected <- c(expected, mazegenerator:::.edge_key(c(r, c), c(r, c + 1L)))
      }
      if (r < rows) {
        expected <- c(expected, mazegenerator:::.edge_key(c(r, c), c(r + 1L, c)))
      }
    }
  }

  expect_setequal(keys, expected)
  expect_equal(length(keys), length(expected))
})

test_that("open_walls validates its input", {
  expect_error(generate_maze(5, 5, open_walls = -0.1), "between 0 and 1")
  expect_error(generate_maze(5, 5, open_walls = 1.5), "between 0 and 1")
  expect_error(generate_maze(5, 5, open_walls = "a"), "between 0 and 1")
  expect_error(generate_maze(5, 5, open_walls = NA), "between 0 and 1")
  expect_error(generate_maze(5, 5, open_walls = c(0.1, 0.2)), "between 0 and 1")
})

test_that("solve_maze() still works on a braided maze", {
  maze <- generate_maze(10, 10, seed = 5, open_walls = 0.2)
  path <- solve_maze(maze)

  expect_true(nrow(path) >= 2L)
  expect_identical(c(path$row[1L], path$column[1L]), as.integer(maze$start))
  expect_identical(
    c(path$row[nrow(path)], path$column[nrow(path)]),
    as.integer(maze$end)
  )

  for (i in seq_len(nrow(path) - 1L)) {
    from <- c(path$row[i], path$column[i])
    to <- c(path$row[i + 1L], path$column[i + 1L])
    key <- mazegenerator:::.edge_key(from, to)
    expect_true(key %in% maze$passages)
  }
})
