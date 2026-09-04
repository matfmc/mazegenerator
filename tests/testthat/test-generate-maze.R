test_that("generation visits every cell through adjacent moves", {
  maze <- generate_maze(4, 5, start = c(2, 3), seed = 42)

  expect_s3_class(maze, "maze")
  expect_equal(maze$start, c(2L, 3L))
  expect_length(maze$visitation_order, 20)

  visited <- vapply(maze$visitation_order, paste, character(1), collapse = ",")
  expect_length(unique(visited), 20)

  traversal <- do.call(rbind, maze$traversal)
  distances <- rowSums(abs(traversal[-1, , drop = FALSE] -
    traversal[-nrow(traversal), , drop = FALSE]))
  expect_true(all(distances == 1))
})

test_that("a perfect maze has cells minus one passages", {
  maze <- generate_maze(7, 3, seed = 123)
  expect_length(maze$passages, 7 * 3 - 1)
})

test_that("a one-cell maze is supported", {
  maze <- generate_maze(1, 1, seed = 1)
  expect_equal(maze$start, c(1L, 1L))
  expect_equal(maze$end, c(1L, 1L))
  expect_length(maze$passages, 0)
})

test_that("generation validates dimensions and start coordinates", {
  expect_error(generate_maze(0, 2), "greater than zero")
  expect_error(generate_maze(2.5, 2), "whole number")
  expect_error(generate_maze(2, 2, start = c(3, 1)), "boundaries")
  expect_error(generate_maze(2, 2, start = "top-left"), "c\\(row, column\\)")
})

test_that("a seed makes generation reproducible", {
  first <- generate_maze(5, 5, seed = 2026)
  second <- generate_maze(5, 5, seed = 2026)
  expect_equal(first$traversal, second$traversal)
})
