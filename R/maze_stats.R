#' Compute comparative statistics for a maze
#'
#' Summarizes the structural character of a maze: how many dead ends it has,
#' how long its corridors are, how branchy it is, how "wide" it is from
#' corner to corner, how much of the grid its shortest solution covers, and
#' how straight its corridors tend to run. Useful for comparing mazes
#' produced by different [generate_maze()] algorithms or `open_walls`
#' settings.
#'
#' A *corridor* here is a maximal run of cells between two "special" cells
#' (a dead end, a branch, or an intersection -- any cell whose number of open
#' passages, its degree, is not exactly 2), following the unique route
#' through the intervening degree-2 cells. In a perfect maze every corridor
#' has exactly two such endpoints. A braided maze (see `open_walls` in
#' [generate_maze()]) can additionally contain a loop of degree-2 cells with
#' no special cell on it at all; each such loop is counted as one more
#' corridor whose length is its own edge count.
#'
#' @param maze A `maze` object returned by [generate_maze()].
#'
#' @return An object of class `maze_stats` (a list, printed with its own
#'   `print` method, and convertible to a one-row data frame with
#'   `as.data.frame()`) containing:
#'   \describe{
#'     \item{rows, cols}{The maze's dimensions.}
#'     \item{algorithm}{The generation algorithm recorded on `maze`.}
#'     \item{open_walls}{The `open_walls` probability recorded on `maze`.}
#'     \item{dead_ends, dead_end_pct}{The number of degree-1 cells, and that
#'       count as a fraction of all cells.}
#'     \item{corridor_mean, corridor_max}{The mean and maximum corridor
#'       length (in edges), across every corridor as defined above.
#'       `NA` for a 1x1 maze, which has no corridors.}
#'     \item{degree1, degree2, degree3, degree4}{The number of cells with
#'       exactly 1, 2, 3, or 4 open passages. These four counts sum to
#'       `rows * cols` (every generated maze keeps every cell connected, so
#'       degree 0 does not occur except in a degenerate 1x1 maze).}
#'     \item{diameter}{The length, in steps, of the longest shortest path
#'       between any two cells in the maze -- its graph diameter. Computed
#'       by a double-sweep BFS: farthest cell from an arbitrary corner, then
#'       farthest cell from that cell, then the shortest-path length between
#'       those two. This is exact for a perfect maze (a tree) but only an
#'       approximation, typically a slight underestimate, for a braided
#'       maze with loops.}
#'     \item{solution_length, solution_pct}{The number of cells on the
#'       shortest path from `maze$start` to `maze$end` (via
#'       `solve_maze(maze, algorithm = "bfs")`), and that count as a
#'       fraction of all cells.}
#'     \item{river}{A straightness measure in `[0, 1]`, `NA` if the maze has
#'       no corridor with at least two edges. Within a corridor, each step
#'       after the first either continues in the same direction as the step
#'       before it or turns; `river` is the fraction of those steps that
#'       continue straight, averaged over every corridor with at least two
#'       edges (each corridor contributes one fraction, regardless of its
#'       length). A `river` near 1 means the maze is dominated by long,
#'       straight corridors; a `river` near 0 means corridors turn at nearly
#'       every cell.}
#'   }
#' @export
#'
#' @examples
#' maze <- generate_maze(15, 15, seed = 42)
#' maze_stats(maze)
#'
#' # Braiding a maze (adding loops) tends to shorten its corridors and
#' # increase its branching.
#' braided <- generate_maze(15, 15, seed = 42, open_walls = 0.3)
#' as.data.frame(maze_stats(braided))
maze_stats <- function(maze) {
  if (!.is_maze(maze)) {
    stop("`maze` must be an object returned by `generate_maze()`.", call. = FALSE)
  }

  rows <- maze$rows
  cols <- maze$cols
  total_cells <- rows * cols
  passages <- maze$passages

  open_neighbors <- function(cell) {
    neighbors <- .cell_neighbors(cell, rows, cols)
    if (nrow(neighbors) == 0L) {
      return(neighbors)
    }
    keep <- vapply(seq_len(nrow(neighbors)), function(i) {
      .edge_key(cell, neighbors[i, ]) %in% passages
    }, logical(1))
    neighbors[keep, , drop = FALSE]
  }

  degree <- matrix(0L, nrow = rows, ncol = cols)
  for (r in seq_len(rows)) {
    for (c in seq_len(cols)) {
      degree[r, c] <- nrow(open_neighbors(c(r, c)))
    }
  }

  degree_counts <- vapply(1:4, function(d) sum(degree == d), integer(1))

  dead_ends <- sum(degree == 1L)
  dead_end_pct <- dead_ends / total_cells

  # ---- Corridor decomposition -------------------------------------------
  visited_edges <- character(0)
  corridor_lengths <- numeric(0)
  corridor_straightness <- numeric(0)

  special_cells <- which(degree != 2L, arr.ind = TRUE)

  if (nrow(special_cells) > 0L) {
    for (i in seq_len(nrow(special_cells))) {
      origin <- as.integer(special_cells[i, ])
      neighbors <- open_neighbors(origin)
      if (nrow(neighbors) == 0L) next

      for (j in seq_len(nrow(neighbors))) {
        first <- as.integer(neighbors[j, ])
        start_edge <- .edge_key(origin, first)
        if (start_edge %in% visited_edges) next

        prev <- origin
        curr <- first
        visited_edges <- c(visited_edges, start_edge)
        directions <- list(curr - prev)
        length_count <- 1L

        while (degree[curr[1L], curr[2L]] == 2L) {
          step_neighbors <- open_neighbors(curr)
          next_cell <- NULL
          for (k in seq_len(nrow(step_neighbors))) {
            candidate <- as.integer(step_neighbors[k, ])
            if (!all(candidate == prev)) {
              next_cell <- candidate
              break
            }
          }
          if (is.null(next_cell)) break
          edge_key <- .edge_key(curr, next_cell)
          if (edge_key %in% visited_edges) break
          visited_edges <- c(visited_edges, edge_key)
          directions[[length(directions) + 1L]] <- next_cell - curr
          prev <- curr
          curr <- next_cell
          length_count <- length_count + 1L
        }

        corridor_lengths <- c(corridor_lengths, length_count)
        if (length(directions) >= 2L) {
          transitions <- length(directions) - 1L
          straight <- sum(vapply(2:length(directions), function(k) {
            all(directions[[k]] == directions[[k - 1L]])
          }, logical(1)))
          corridor_straightness <- c(corridor_straightness, straight / transitions)
        }
      }
    }
  }

  # Edges left over belong to a loop of degree-2 cells with no special cell
  # on it at all -- only possible on a heavily braided maze. Each connected
  # component of such edges is one more corridor, its length its edge
  # count; it contributes no `river` measurement since it has no natural
  # starting cell.
  remaining <- setdiff(passages, visited_edges)
  if (length(remaining) > 0L) {
    parse_edge <- function(key) {
      parts <- strsplit(key, "|", fixed = TRUE)[[1]]
      list(
        from = as.integer(strsplit(parts[1L], ",")[[1]]),
        to = as.integer(strsplit(parts[2L], ",")[[1]])
      )
    }
    cell_key <- function(cell) paste(cell[1L], cell[2L], sep = ",")
    edges <- lapply(remaining, parse_edge)
    from_keys <- vapply(edges, function(e) cell_key(e$from), character(1))
    to_keys <- vapply(edges, function(e) cell_key(e$to), character(1))
    all_keys <- unique(c(from_keys, to_keys))
    # Simple union-find keyed by cell label.
    root_of <- stats::setNames(all_keys, all_keys)
    find <- function(x) {
      while (root_of[[x]] != x) {
        x <- root_of[[x]]
      }
      x
    }
    for (i in seq_along(edges)) {
      ra <- find(from_keys[i])
      rb <- find(to_keys[i])
      if (ra != rb) root_of[[ra]] <- rb
    }
    edge_roots <- vapply(seq_along(edges), function(i) find(from_keys[i]), character(1))
    corridor_lengths <- c(corridor_lengths, as.numeric(table(edge_roots)))
  }

  if (total_cells <= 1L || length(corridor_lengths) == 0L) {
    corridor_mean <- NA_real_
    corridor_max <- NA_real_
  } else {
    corridor_mean <- mean(corridor_lengths)
    corridor_max <- max(corridor_lengths)
  }

  river <- if (length(corridor_straightness) == 0L) {
    NA_real_
  } else {
    mean(corridor_straightness)
  }

  # ---- Diameter: double-sweep BFS approximation --------------------------
  if (total_cells <= 1L) {
    diameter <- 0L
  } else {
    sweep_a <- .farthest_cell(rows, cols, c(1L, 1L), passages)
    sweep_b <- .farthest_cell(rows, cols, sweep_a, passages)
    diameter_path <- solve_maze(maze, start = sweep_a, end = sweep_b, algorithm = "bfs")
    diameter <- nrow(diameter_path) - 1L
  }

  # ---- Solution coverage --------------------------------------------------
  solution_path <- solve_maze(maze, algorithm = "bfs")
  solution_length <- nrow(solution_path)
  solution_pct <- solution_length / total_cells

  structure(
    list(
      rows = rows,
      cols = cols,
      algorithm = maze$algorithm,
      open_walls = maze$open_walls,
      dead_ends = as.integer(dead_ends),
      dead_end_pct = dead_end_pct,
      corridor_mean = corridor_mean,
      corridor_max = corridor_max,
      degree1 = as.integer(degree_counts[[1L]]),
      degree2 = as.integer(degree_counts[[2L]]),
      degree3 = as.integer(degree_counts[[3L]]),
      degree4 = as.integer(degree_counts[[4L]]),
      diameter = as.integer(diameter),
      solution_length = as.integer(solution_length),
      solution_pct = solution_pct,
      river = river
    ),
    class = "maze_stats"
  )
}

#' @export
print.maze_stats <- function(x, ...) {
  cat(sprintf("<maze_stats> %d x %d (%s)\n", x$rows, x$cols, x$algorithm))
  if (isTRUE(x$open_walls > 0)) {
    cat(sprintf("  open_walls:  %g\n", x$open_walls))
  }
  cat(sprintf("  dead ends:   %d (%.1f%%)\n", x$dead_ends, 100 * x$dead_end_pct))
  cat(sprintf(
    "  degree 1/2/3/4: %d / %d / %d / %d\n",
    x$degree1, x$degree2, x$degree3, x$degree4
  ))
  cat(sprintf(
    "  corridors:   mean %s, max %s\n",
    if (is.na(x$corridor_mean)) "NA" else sprintf("%.2f", x$corridor_mean),
    if (is.na(x$corridor_max)) "NA" else x$corridor_max
  ))
  cat(sprintf("  diameter:    %d\n", x$diameter))
  cat(sprintf(
    "  solution:    %d cells (%.1f%% of maze)\n",
    x$solution_length, 100 * x$solution_pct
  ))
  cat(sprintf(
    "  river:       %s\n",
    if (is.na(x$river)) "NA" else sprintf("%.3f", x$river)
  ))
  invisible(x)
}

#' @export
as.data.frame.maze_stats <- function(x, row.names = NULL, optional = FALSE, ...) {
  data.frame(
    rows = x$rows,
    cols = x$cols,
    algorithm = x$algorithm,
    open_walls = x$open_walls,
    dead_ends = x$dead_ends,
    dead_end_pct = x$dead_end_pct,
    corridor_mean = x$corridor_mean,
    corridor_max = x$corridor_max,
    degree1 = x$degree1,
    degree2 = x$degree2,
    degree3 = x$degree3,
    degree4 = x$degree4,
    diameter = x$diameter,
    solution_length = x$solution_length,
    solution_pct = x$solution_pct,
    river = x$river,
    stringsAsFactors = FALSE
  )
}
