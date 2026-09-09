.reachable_from <- function(rows, cols, start, passages) {
  visited <- matrix(FALSE, nrow = rows, ncol = cols)
  visited[start[1L], start[2L]] <- TRUE
  queue <- list(start)
  head_idx <- 1L
  while (head_idx <= length(queue)) {
    current <- queue[[head_idx]]
    head_idx <- head_idx + 1L
    neighbors <- mazegenerator:::.cell_neighbors(current, rows, cols)
    if (nrow(neighbors) > 0L) {
      for (i in seq_len(nrow(neighbors))) {
        neighbor <- as.integer(neighbors[i, ])
        if (!visited[neighbor[1L], neighbor[2L]] &&
            mazegenerator:::.edge_key(current, neighbor) %in% passages) {
          visited[neighbor[1L], neighbor[2L]] <- TRUE
          queue[[length(queue) + 1L]] <- neighbor
        }
      }
    }
  }
  visited
}

algorithms <- c(
  "dfs", "prim", "kruskal", "wilson", "eller",
  "division", "aldous_broder", "hunt_kill", "binary_tree", "sidewinder", "growing_tree"
)

for (algo in algorithms) {

  test_that(sprintf("%s produces a perfect maze", algo), {
    rows <- 9L
    cols <- 8L
    maze <- generate_maze(rows, cols, seed = 11, algorithm = algo)

    expect_equal(maze$algorithm, algo)
    expect_length(unique(maze$passages), rows * cols - 1L)
    expect_equal(length(maze$passages), length(unique(maze$passages)))

    reachable <- .reachable_from(rows, cols, maze$start, maze$passages)
    expect_true(all(reachable))
  })

  test_that(sprintf("%s carve_order has no duplicates and matches passages", algo), {
    maze <- generate_maze(6, 7, seed = 5, algorithm = algo)

    expect_length(maze$carve_order, length(unique(maze$carve_order)))
    expect_setequal(maze$carve_order, maze$passages)
  })

  test_that(sprintf("%s is reproducible for a fixed seed and varies across seeds", algo), {
    m1 <- generate_maze(8, 8, seed = 123, algorithm = algo)
    m2 <- generate_maze(8, 8, seed = 123, algorithm = algo)
    m3 <- generate_maze(8, 8, seed = 456, algorithm = algo)

    expect_identical(m1$passages, m2$passages)
    expect_identical(m1$carve_order, m2$carve_order)
    expect_false(identical(m1$passages, m3$passages))
  })

  test_that(sprintf("%s handles non-rectangular and degenerate sizes", algo), {
    for (dims in list(c(3, 11), c(11, 3), c(1, 9), c(9, 1), c(1, 1))) {
      rows <- dims[1L]
      cols <- dims[2L]
      maze <- generate_maze(rows, cols, seed = 7, algorithm = algo)

      expect_length(unique(maze$passages), rows * cols - 1L)
      reachable <- .reachable_from(rows, cols, maze$start, maze$passages)
      expect_true(all(reachable))
    }
  })

  test_that(sprintf("%s end cell is in bounds and reachable", algo), {
    rows <- 10L
    cols <- 6L
    maze <- generate_maze(rows, cols, seed = 21, algorithm = algo)

    expect_true(maze$end[1L] >= 1L && maze$end[1L] <= rows)
    expect_true(maze$end[2L] >= 1L && maze$end[2L] <= cols)

    reachable <- .reachable_from(rows, cols, maze$start, maze$passages)
    expect_true(reachable[maze$end[1L], maze$end[2L]])
  })

  test_that(sprintf("braiding works on top of %s", algo), {
    rows <- 15L
    cols <- 15L
    maze <- generate_maze(rows, cols, seed = 33, algorithm = algo, open_walls = 0.3)

    perfect_count <- rows * cols - 1L
    expect_gt(length(maze$passages), perfect_count)
    expect_gt(maze$walls_removed, 0L)
    expect_equal(maze$walls_removed, length(maze$passages) - perfect_count)
    # carve_order stays the perfect-maze passage set, unaffected by braiding.
    expect_length(maze$carve_order, perfect_count)
  })
}

test_that("dfs keeps traversal and visitation_order; other algorithms set them to NULL", {
  dfs_maze <- generate_maze(5, 5, seed = 1, algorithm = "dfs")
  expect_false(is.null(dfs_maze$traversal))
  expect_false(is.null(dfs_maze$visitation_order))

  for (algo in c("prim", "kruskal", "wilson", "eller")) {
    maze <- generate_maze(5, 5, seed = 1, algorithm = algo)
    expect_null(maze$traversal)
    expect_null(maze$visitation_order)
  }
})

test_that("plot_maze(show_path = TRUE) errors for non-dfs mazes", {
  for (algo in c("prim", "kruskal", "wilson", "eller")) {
    maze <- generate_maze(5, 5, seed = 1, algorithm = algo)
    expect_error(plot_maze(maze, show_path = TRUE), "algorithm = \"dfs\"")
    expect_error(plot_maze(maze, show_nodes = TRUE), "algorithm = \"dfs\"")
  }

  dfs_maze <- generate_maze(5, 5, seed = 1, algorithm = "dfs")
  output <- tempfile(fileext = ".png")
  grDevices::png(output, width = 300, height = 300)
  expect_invisible(plot_maze(dfs_maze, show_path = TRUE, show_nodes = TRUE))
  grDevices::dev.off()
})

test_that("an invalid algorithm value errors", {
  expect_error(generate_maze(5, 5, algorithm = "nonexistent"), "should be one of")
  expect_error(generate_maze(5, 5, algorithm = c("dfs", "prim")))
})

test_that("dfs default algorithm behaviour is unchanged", {
  maze <- generate_maze(6, 6, seed = 42)
  expect_equal(maze$algorithm, "dfs")
  expect_setequal(unique(maze$carve_order), maze$passages)
})

test_that("aldous_broder and hunt_kill keep traversal and visitation_order", {
  for (algo in c("aldous_broder", "hunt_kill")) {
    maze <- generate_maze(5, 5, seed = 1, algorithm = algo)
    expect_false(is.null(maze$traversal))
    expect_false(is.null(maze$visitation_order))
    expect_true(length(maze$traversal) >= length(maze$visitation_order))
    expect_equal(length(maze$visitation_order), 25L)
  }
})

test_that("binary_tree biases the top row and leftmost column into corridors", {
  rows <- 8L
  cols <- 8L
  maze <- generate_maze(rows, cols, seed = 3, algorithm = "binary_tree")

  top_row_edges <- vapply(
    seq_len(cols - 1L),
    function(c) mazegenerator:::.edge_key(c(1L, c), c(1L, c + 1L)),
    character(1L)
  )
  expect_true(all(top_row_edges %in% maze$passages))

  left_col_edges <- vapply(
    seq_len(rows - 1L),
    function(r) mazegenerator:::.edge_key(c(r, 1L), c(r + 1L, 1L)),
    character(1L)
  )
  expect_true(all(left_col_edges %in% maze$passages))
})

test_that("sidewinder makes the top row one continuous corridor", {
  rows <- 8L
  cols <- 8L
  maze <- generate_maze(rows, cols, seed = 4, algorithm = "sidewinder")

  top_row_edges <- vapply(
    seq_len(cols - 1L),
    function(c) mazegenerator:::.edge_key(c(1L, c), c(1L, c + 1L)),
    character(1L)
  )
  expect_true(all(top_row_edges %in% maze$passages))
})

test_that("recursive division produces a perfect maze", {
  rows <- 9L
  cols <- 11L
  maze <- generate_maze(rows, cols, seed = 9, algorithm = "division")

  expect_length(unique(maze$passages), rows * cols - 1L)
  reachable <- .reachable_from(rows, cols, maze$start, maze$passages)
  expect_true(all(reachable))
})

test_that("growing_tree selection changes the resulting texture", {
  rows <- 10L
  cols <- 10L

  newest_maze <- generate_maze(rows, cols, seed = 55, algorithm = "growing_tree",
                                selection = "newest")
  random_maze <- generate_maze(rows, cols, seed = 55, algorithm = "growing_tree",
                                selection = "random")

  expect_length(unique(newest_maze$passages), rows * cols - 1L)
  expect_length(unique(random_maze$passages), rows * cols - 1L)
  expect_true(all(.reachable_from(rows, cols, newest_maze$start, newest_maze$passages)))
  expect_true(all(.reachable_from(rows, cols, random_maze$start, random_maze$passages)))

  expect_false(identical(newest_maze$passages, random_maze$passages))
})

test_that("growing_tree selection is reproducible for a fixed seed", {
  for (sel in c("newest", "random", "oldest", "mixed")) {
    m1 <- generate_maze(7, 7, seed = 21, algorithm = "growing_tree", selection = sel)
    m2 <- generate_maze(7, 7, seed = 21, algorithm = "growing_tree", selection = sel)
    expect_identical(m1$passages, m2$passages)
  }
})

test_that("growing_tree with selection = \"newest\" reproduces dfs", {
  maze <- generate_maze(6, 6, seed = 77, algorithm = "growing_tree", selection = "newest")
  dfs_maze <- generate_maze(6, 6, seed = 77, algorithm = "dfs")
  expect_setequal(maze$passages, dfs_maze$passages)
})

test_that("selection is ignored (with a warning) for non-growing_tree algorithms", {
  expect_warning(
    generate_maze(5, 5, seed = 1, algorithm = "dfs", selection = "random"),
    "growing_tree"
  )
  expect_silent(generate_maze(5, 5, seed = 1, algorithm = "dfs"))
})

test_that("an invalid selection value errors", {
  expect_error(
    generate_maze(5, 5, algorithm = "growing_tree", selection = "nonexistent"),
    "should be one of"
  )
})
