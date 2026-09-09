.count_dead_ends <- function(maze) {
  rows <- maze$rows
  cols <- maze$cols
  cells <- expand.grid(row = seq_len(rows), col = seq_len(cols))
  degrees <- vapply(seq_len(nrow(cells)), function(i) {
    cell <- c(cells$row[i], cells$col[i])
    neighbors <- mazegenerator:::.cell_neighbors(cell, rows, cols)
    sum(vapply(seq_len(nrow(neighbors)), function(k) {
      mazegenerator:::.edge_key(cell, neighbors[k, ]) %in% maze$passages
    }, logical(1L)))
  }, integer(1L))
  sum(degrees == 1L)
}

.reachable_count <- function(maze, from) {
  rows <- maze$rows
  cols <- maze$cols
  visited <- matrix(FALSE, nrow = rows, ncol = cols)
  visited[from[1L], from[2L]] <- TRUE
  queue <- list(as.integer(from))
  head_idx <- 1L
  while (head_idx <= length(queue)) {
    current <- queue[[head_idx]]
    head_idx <- head_idx + 1L
    neighbors <- mazegenerator:::.cell_neighbors(current, rows, cols)
    if (nrow(neighbors) == 0L) next
    for (i in seq_len(nrow(neighbors))) {
      neighbor <- as.integer(neighbors[i, ])
      if (!visited[neighbor[1L], neighbor[2L]] &&
          mazegenerator:::.edge_key(current, neighbor) %in% maze$passages) {
        visited[neighbor[1L], neighbor[2L]] <- TRUE
        queue[[length(queue) + 1L]] <- neighbor
      }
    }
  }
  sum(visited)
}

.is_fully_connected <- function(maze) {
  .reachable_count(maze, c(1L, 1L)) == maze$rows * maze$cols
}

test_that("braid(p = 1) removes dead ends and keeps the maze connected", {
  maze <- generate_maze(6, 6, seed = 42)
  before <- .count_dead_ends(maze)
  expect_gt(before, 0)

  braided <- braid(maze, p = 1)

  expect_s3_class(braided, "maze")
  expect_true(.is_fully_connected(braided))
  expect_equal(.count_dead_ends(braided), 0)
  expect_gt(braided$walls_removed, maze$walls_removed)
  expect_gt(length(braided$carve_order), length(maze$carve_order))
  expect_true(all(maze$passages %in% braided$passages))
})

test_that("braid(p = 0) is a no-op", {
  maze <- generate_maze(6, 6, seed = 7)
  braided <- braid(maze, p = 0)

  expect_equal(braided$passages, maze$passages)
  expect_equal(braided$carve_order, maze$carve_order)
  expect_equal(braided$walls_removed, maze$walls_removed)
})

test_that("a braided maze is still fully connected and solvable", {
  maze <- generate_maze(8, 8, seed = 3, algorithm = "prim")
  braided <- braid(maze, p = 1)

  expect_true(.is_fully_connected(braided))
  path <- solve_maze(braided)
  expect_true(nrow(path) >= 1)
  expect_equal(as.integer(path[1, ]), as.integer(braided$start))
  expect_equal(as.integer(path[nrow(path), ]), as.integer(braided$end))
})

test_that("sparsify reduces the reachable cell count", {
  maze <- generate_maze(10, 10, seed = 11)
  before <- .reachable_count(maze, maze$start)

  sparse <- sparsify(maze, p = 1)

  expect_s3_class(sparse, "maze")
  after <- .reachable_count(sparse, maze$start)
  expect_lt(after, before)
  expect_true(all(sparse$passages %in% maze$passages))
})

test_that("sparsify(p = 0) is a no-op on passages", {
  maze <- generate_maze(6, 6, seed = 21)
  sparse <- sparsify(maze, p = 0)

  expect_equal(sparse$passages, maze$passages)
})

test_that("set_endpoints('longest') is at least as long as the default end", {
  maze <- generate_maze(10, 10, seed = 5, algorithm = "kruskal")
  default_path <- solve_maze(maze)

  longest <- set_endpoints(maze, "longest")
  longest_path <- solve_maze(longest)

  expect_true(nrow(longest_path) >= nrow(default_path))
})

test_that("set_endpoints('corners') yields exactly the corners", {
  maze <- generate_maze(7, 5, seed = 9)
  corners <- set_endpoints(maze, "corners")

  expect_equal(corners$start, c(1L, 1L))
  expect_equal(corners$end, c(7L, 5L))
})

test_that("set_endpoints('farthest') keeps start and updates end", {
  maze <- generate_maze(8, 8, seed = 13, algorithm = "wilson")
  result <- set_endpoints(maze, "farthest")

  expect_equal(result$start, maze$start)
  expect_true(is.integer(result$end))
})

test_that("set_endpoints('random') yields two distinct in-bounds cells", {
  maze <- generate_maze(6, 6, seed = 17)
  result <- set_endpoints(maze, "random")

  expect_false(identical(result$start, result$end))
  expect_true(all(result$start >= 1L & result$start <= 6L))
  expect_true(all(result$end >= 1L & result$end <= 6L))
})

test_that("explicit start/end override the chosen strategy", {
  maze <- generate_maze(6, 6, seed = 23)
  result <- set_endpoints(maze, strategy = "corners", start = c(2L, 3L), end = c(4L, 5L))

  expect_equal(result$start, c(2L, 3L))
  expect_equal(result$end, c(4L, 5L))
})

test_that("set_endpoints validates explicit positions", {
  maze <- generate_maze(4, 4, seed = 29)
  expect_error(set_endpoints(maze, start = c(0L, 1L)))
  expect_error(set_endpoints(maze, end = c(10L, 10L)))
})

test_that("plot_maze runs without error on braid/sparsify/set_endpoints results", {
  maze <- generate_maze(8, 8, seed = 31)

  braided <- braid(maze, p = 1)
  sparse <- sparsify(maze, p = 0.5)
  reendpointed <- set_endpoints(maze, "longest")

  pdf(NULL)
  on.exit(dev.off(), add = TRUE)

  expect_no_error(plot_maze(braided))
  expect_no_error(plot_maze(sparse))
  expect_no_error(plot_maze(reendpointed, solution = solve_maze(reendpointed)))
})
