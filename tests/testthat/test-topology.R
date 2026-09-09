.reachable_from_masked <- function(rows, cols, start, passages, mask = NULL, wrap = "none") {
  visited <- matrix(FALSE, nrow = rows, ncol = cols)
  visited[start[1L], start[2L]] <- TRUE
  queue <- list(start)
  head_idx <- 1L
  while (head_idx <= length(queue)) {
    current <- queue[[head_idx]]
    head_idx <- head_idx + 1L
    neighbors <- mazegenerator:::.cell_neighbors(current, rows, cols, mask, wrap)
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

.circle_mask <- function(n) {
  center <- (n + 1) / 2
  dist <- outer(seq_len(n), seq_len(n), function(r, c) sqrt((r - center)^2 + (c - center)^2))
  dist <= n / 2
}

topology_algorithms <- c("dfs", "prim", "kruskal", "wilson", "aldous_broder", "hunt_kill", "growing_tree")

# ---- mask ------------------------------------------------------------------

for (algo in topology_algorithms) {

  test_that(sprintf("%s: masked maze only visits TRUE cells and stays a spanning tree", algo), {
    mask <- .circle_mask(11)
    maze <- generate_maze(mask = mask, algorithm = algo, seed = 7)

    expect_equal(maze$rows, 11L)
    expect_equal(maze$cols, 11L)
    expect_true(mask[maze$start[1L], maze$start[2L]])
    expect_true(mask[maze$end[1L], maze$end[2L]])

    # No passage touches a FALSE cell.
    for (key in maze$passages) {
      cells <- strsplit(key, "\\|")[[1L]]
      for (cell_str in cells) {
        rc <- as.integer(strsplit(cell_str, ",")[[1L]])
        expect_true(mask[rc[1L], rc[2L]])
      }
    }

    expect_length(unique(maze$passages), sum(mask) - 1L)
    expect_equal(length(maze$passages), length(unique(maze$passages)))

    reachable <- .reachable_from_masked(maze$rows, maze$cols, maze$start, maze$passages, mask)
    expect_true(all(reachable[mask]))
    expect_false(any(reachable[!mask]))
  })
}

test_that("mask can be inferred from dim() and coerced from a non-logical matrix", {
  mask <- matrix(c(1, 1, 0, 1, 1, 1, 0, 1, 1), nrow = 3L)
  maze <- generate_maze(mask = mask, algorithm = "dfs", seed = 1)
  expect_equal(maze$rows, 3L)
  expect_equal(maze$cols, 3L)
  expect_true(is.logical(maze$mask))
})

test_that("mask dimensions must agree with explicit rows/cols", {
  mask <- matrix(TRUE, nrow = 4L, ncol = 4L)
  expect_error(generate_maze(rows = 5L, cols = 4L, mask = mask), "rows")
})

test_that("start snaps to the first TRUE cell when left at the default", {
  mask <- matrix(FALSE, nrow = 5L, ncol = 5L)
  mask[3L:5L, 3L:5L] <- TRUE
  maze <- generate_maze(mask = mask, algorithm = "prim", seed = 2)
  expect_true(mask[maze$start[1L], maze$start[2L]])
})

test_that("an explicit start on a FALSE cell errors", {
  mask <- matrix(FALSE, nrow = 5L, ncol = 5L)
  mask[3L:5L, 3L:5L] <- TRUE
  expect_error(generate_maze(mask = mask, start = c(1L, 1L), algorithm = "prim"), "TRUE")
})

test_that("a mask with fewer than 2 TRUE cells errors", {
  mask <- matrix(FALSE, nrow = 3L, ncol = 3L)
  mask[1L, 1L] <- TRUE
  expect_error(generate_maze(mask = mask, algorithm = "dfs"), "at least 2")
})

test_that("a disconnected mask errors", {
  mask <- matrix(FALSE, nrow = 5L, ncol = 5L)
  mask[1L, 1L] <- TRUE
  mask[5L, 5L] <- TRUE
  expect_error(generate_maze(mask = mask, algorithm = "dfs"), "connected")
})

test_that("mask combined with a row-structured algorithm errors", {
  mask <- .circle_mask(9)
  for (algo in c("eller", "binary_tree", "sidewinder", "division")) {
    expect_error(generate_maze(mask = mask, algorithm = algo))
  }
})

# ---- wrap --------------------------------------------------------------

test_that("wrap = 'horizontal' produces a passage across the seam across many seeds", {
  found_seam <- FALSE
  for (seed in 1:25) {
    maze <- generate_maze(6, 6, seed = seed, algorithm = "prim", wrap = "horizontal")
    seam <- vapply(seq_len(maze$rows), function(r) {
      mazegenerator:::.edge_key(c(r, 1L), c(r, maze$cols)) %in% maze$passages
    }, logical(1L))
    if (any(seam)) {
      found_seam <- TRUE
      break
    }
  }
  expect_true(found_seam)
})

test_that("wrap keeps the maze connected", {
  for (wrap in c("horizontal", "vertical", "both")) {
    maze <- generate_maze(6, 7, seed = 3, algorithm = "dfs", wrap = wrap)
    # The base spanning tree uses plain (non-wrapped) adjacency, so it is
    # connected on its own; any seam passages `wrap` adds are bonus loops.
    reachable <- .reachable_from_masked(maze$rows, maze$cols, maze$start, maze$passages)
    expect_true(all(reachable))
    expect_gte(length(unique(maze$passages)), maze$rows * maze$cols - 1L)
    expect_equal(length(maze$passages), length(unique(maze$passages)))
  }
})

test_that("wrap combined with a row-structured algorithm errors", {
  for (algo in c("eller", "binary_tree", "sidewinder", "division")) {
    expect_error(generate_maze(6, 6, algorithm = algo, wrap = "horizontal"))
  }
})

# ---- rooms ---------------------------------------------------------------

test_that("rooms are fully opened and the maze stays connected", {
  rooms <- list(c(2L, 2L, 4L, 4L), c(7L, 7L, 9L, 9L))
  maze <- generate_maze(10, 10, seed = 4, algorithm = "prim", rooms = rooms)

  for (room in rooms) {
    row0 <- room[1L]; col0 <- room[2L]; row1 <- room[3L]; col1 <- room[4L]
    for (r in row0:row1) {
      for (c in col0:(col1 - 1L)) {
        expect_true(mazegenerator:::.edge_key(c(r, c), c(r, c + 1L)) %in% maze$passages)
      }
    }
    for (c in col0:col1) {
      for (r in row0:(row1 - 1L)) {
        expect_true(mazegenerator:::.edge_key(c(r, c), c(r + 1L, c)) %in% maze$passages)
      }
    }
  }

  reachable <- .reachable_from_masked(maze$rows, maze$cols, maze$start, maze$passages)
  expect_true(all(reachable))
})

test_that("rooms out of bounds or outside the mask error", {
  expect_error(generate_maze(10, 10, rooms = list(c(8L, 8L, 12L, 12L)), algorithm = "prim"))

  mask <- .circle_mask(10)
  expect_error(
    generate_maze(mask = mask, rooms = list(c(1L, 1L, 2L, 2L)), algorithm = "prim"),
    "mask"
  )
})

test_that("rooms combined with a row-structured algorithm errors", {
  rooms <- list(c(2L, 2L, 4L, 4L))
  for (algo in c("eller", "binary_tree", "sidewinder", "division")) {
    expect_error(generate_maze(10, 10, algorithm = algo, rooms = rooms))
  }
})

# ---- plot_maze / solve_maze compatibility ---------------------------------

test_that("plot_maze and solve_maze run without error on a masked maze", {
  mask <- .circle_mask(11)
  maze <- generate_maze(mask = mask, algorithm = "prim", seed = 9)

  pdf(NULL)
  on.exit(dev.off(), add = TRUE)

  expect_no_error(plot_maze(maze))
  expect_no_error(solve_maze(maze))
})

test_that("plot_maze and solve_maze run without error on a wrapped maze", {
  maze <- generate_maze(8, 8, seed = 9, algorithm = "dfs", wrap = "both")

  pdf(NULL)
  on.exit(dev.off(), add = TRUE)

  expect_no_error(plot_maze(maze))
  expect_no_error(solve_maze(maze))
})
