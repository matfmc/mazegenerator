#' Plot a maze
#'
#' Draws a maze with base R graphics. Generation traversal or a solved path can
#' optionally be overlaid on the passages.
#'
#' @param maze A `maze` object returned by [generate_maze()].
#' @param solution Optional two-column matrix or data frame returned by
#'   [solve_maze()].
#' @param show_path Draw the complete generation traversal.
#' @param show_nodes Number the cells in the generation traversal.
#' @param wall_color,background_color Colors used for walls and cells.
#' @param wall_width Width of wall lines.
#' @param path_color,solution_color Colors used for optional path overlays.
#' @param path_width Width of path overlays.
#' @param mark_endpoints Mark and label the start and end cells.
#' @param ... Additional graphical parameters passed by `plot.maze()`.
#'
#' @return The maze, invisibly.
#' @export
#'
#' @examples
#' maze <- generate_maze(10, 10, seed = 42)
#' plot_maze(maze)
plot_maze <- function(maze, solution = NULL, show_path = FALSE,
                      show_nodes = FALSE, wall_color = "#171717",
                      background_color = "white", wall_width = 3,
                      path_color = "#8b5cf6", solution_color = "#f59e0b",
                      path_width = 2, mark_endpoints = TRUE) {
  if (!.is_maze(maze)) {
    stop("`maze` must be an object returned by `generate_maze()`.", call. = FALSE)
  }

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)
  graphics::par(mar = c(1, 1, 2.5, 1))
  graphics::plot.new()
  graphics::plot.window(
    xlim = c(0.5, maze$cols + 0.5),
    ylim = c(maze$rows + 0.5, 0.5),
    asp = 1,
    xaxs = "i",
    yaxs = "i"
  )
  graphics::rect(
    0.5, 0.5, maze$cols + 0.5, maze$rows + 0.5,
    col = background_color,
    border = NA
  )

  if (maze$cols > 1L) {
    for (column in seq_len(maze$cols - 1L)) {
      for (row in seq_len(maze$rows)) {
        if (!.has_passage(maze, c(row, column), c(row, column + 1L))) {
          graphics::segments(
            column + 0.5, row - 0.5,
            column + 0.5, row + 0.5,
            col = wall_color, lwd = wall_width
          )
        }
      }
    }
  }

  if (maze$rows > 1L) {
    for (row in seq_len(maze$rows - 1L)) {
      for (column in seq_len(maze$cols)) {
        if (!.has_passage(maze, c(row, column), c(row + 1L, column))) {
          graphics::segments(
            column - 0.5, row + 0.5,
            column + 0.5, row + 0.5,
            col = wall_color, lwd = wall_width
          )
        }
      }
    }
  }

  graphics::rect(
    0.5, 0.5, maze$cols + 0.5, maze$rows + 0.5,
    border = wall_color,
    lwd = wall_width
  )

  if (isTRUE(show_path)) {
    .draw_coordinate_path(maze$traversal, path_color, path_width)
  }

  if (isTRUE(show_nodes)) {
    coords <- do.call(rbind, maze$visitation_order)
    graphics::points(coords[, 2L], coords[, 1L], pch = 19, cex = 0.45, col = path_color)
    graphics::text(
      coords[, 2L], coords[, 1L], labels = seq_len(nrow(coords)),
      pos = 3, cex = 0.55, col = path_color
    )
  }

  if (!is.null(solution)) {
    solution <- .as_coordinate_matrix(solution, maze$rows, maze$cols, "solution")
    .draw_coordinate_path(split(solution, row(solution)), solution_color, path_width)
  }

  if (isTRUE(mark_endpoints)) {
    graphics::points(maze$start[2L], maze$start[1L], pch = 19, cex = 1.2, col = "#16a34a")
    graphics::points(maze$end[2L], maze$end[1L], pch = 15, cex = 1.1, col = "#dc2626")
    graphics::text(maze$start[2L], maze$start[1L], "start", pos = 3, cex = 0.7, col = "#166534")
    graphics::text(maze$end[2L], maze$end[1L], "end", pos = 1, cex = 0.7, col = "#991b1b")
  }

  graphics::title(main = sprintf("%d x %d maze", maze$rows, maze$cols))
  invisible(maze)
}

#' @export
plot.maze <- function(x, ...) {
  plot_maze(x, ...)
}

.has_passage <- function(maze, from, to) {
  .edge_key(from, to) %in% maze$passages
}

.draw_coordinate_path <- function(path, color, width) {
  if (length(path) < 2L) {
    return(invisible(NULL))
  }
  coordinates <- do.call(rbind, path)
  graphics::lines(
    x = coordinates[, 2L],
    y = coordinates[, 1L],
    col = grDevices::adjustcolor(color, alpha.f = 0.8),
    lwd = width
  )
  invisible(NULL)
}

.as_coordinate_matrix <- function(x, rows, cols, name) {
  if (is.data.frame(x)) {
    x <- as.matrix(x)
  }
  if (!is.matrix(x) || ncol(x) != 2L || !is.numeric(x) || nrow(x) < 1L ||
      anyNA(x) || any(!is.finite(x)) || any(x != floor(x))) {
    stop(sprintf("`%s` must be a numeric two-column matrix or data frame.", name), call. = FALSE)
  }
  x <- matrix(as.integer(x), ncol = 2L, dimnames = list(NULL, c("row", "column")))
  if (any(x[, 1L] < 1L | x[, 1L] > rows | x[, 2L] < 1L | x[, 2L] > cols)) {
    stop(sprintf("`%s` contains a position outside the boundaries.", name), call. = FALSE)
  }
  x
}
