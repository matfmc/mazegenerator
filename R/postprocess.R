#' Braid a maze by removing dead ends
#'
#' Scans a `maze` object for dead ends -- cells of degree 1 in the passage
#' graph, i.e. cells reachable through exactly one open wall -- and, with
#' probability `p`, knocks out one more wall from each, creating a loop.
#' When a dead end has a neighbor that is itself a dead end, that wall is
#' preferred, since carving it removes two dead ends for the price of one
#' wall; otherwise a random closed wall out of the cell is chosen instead.
#'
#' @details
#' This is deliberately different from -- and complements -- the
#' `open_walls` argument of [generate_maze()]. `open_walls` blindly removes
#' any internal wall of the maze at random, with no regard for where dead
#' ends are. `braid()` is targeted: it only ever opens walls that eliminate
#' (or shorten) dead ends, which is the classic "braiding" operation used to
#' turn a perfect maze into a braided one with fewer, shorter dead ends and
#' more loops. You can use both: generate with `open_walls` for random loops
#' and then call `braid()` to specifically clear out remaining dead ends, or
#' vice versa.
#'
#' The newly opened walls are appended to `carve_order` (so animations and
#' replays of maze construction remain sensible) and counted in
#' `walls_removed`, on top of any walls already removed by `open_walls` at
#' generation time.
#'
#' @param maze A `maze` object, as returned by [generate_maze()].
#' @param p Probability, in `[0, 1]`, that any given dead end has a wall
#'   knocked out. At the default of `1`, every dead end that has an
#'   available wall to open is braided away. At `0`, `braid()` is a no-op.
#'
#' @return A modified `maze` object of class `maze`, with `passages`,
#'   `carve_order`, and `walls_removed` updated to reflect the newly opened
#'   walls. All other fields (`rows`, `cols`, `start`, `end`, `algorithm`,
#'   etc.) are unchanged.
#' @export
#' @seealso [generate_maze()] for the `open_walls` argument, which removes
#'   walls at random rather than targeting dead ends; [sparsify()] for the
#'   opposite operation of carving *more* dead ends out of a maze.
#'
#' @examples
#' maze <- generate_maze(10, 10, seed = 42)
#' braided <- braid(maze)
#' braided$walls_removed
#' plot_maze(braided)
braid <- function(maze, p = 1) {
  if (!.is_maze(maze)) {
    stop("`maze` must be a `maze` object.", call. = FALSE)
  }
  p <- .probability(p, "p")

  rows <- maze$rows
  cols <- maze$cols
  passages <- maze$passages
  opened <- character()

  cells <- expand.grid(row = seq_len(rows), col = seq_len(cols))
  visit_order <- sample.int(nrow(cells))

  for (idx in visit_order) {
    cell <- c(cells$row[idx], cells$col[idx])

    if (.postprocess_degree(cell, rows, cols, passages) != 1L) {
      next
    }
    if (stats::runif(1L) >= p) {
      next
    }

    neighbors <- .cell_neighbors(cell, rows, cols)
    if (nrow(neighbors) == 0L) {
      next
    }

    is_wall <- vapply(seq_len(nrow(neighbors)), function(i) {
      !(.edge_key(cell, neighbors[i, ]) %in% passages)
    }, logical(1L))
    candidates <- neighbors[is_wall, , drop = FALSE]
    if (nrow(candidates) == 0L) {
      next
    }

    leads_to_dead_end <- vapply(seq_len(nrow(candidates)), function(i) {
      .postprocess_degree(as.integer(candidates[i, ]), rows, cols, passages) == 1L
    }, logical(1L))
    if (any(leads_to_dead_end)) {
      candidates <- candidates[leads_to_dead_end, , drop = FALSE]
    }

    pick <- if (nrow(candidates) == 1L) 1L else sample.int(nrow(candidates), 1L)
    neighbor <- as.integer(candidates[pick, ])
    key <- .edge_key(cell, neighbor)

    passages <- c(passages, key)
    opened <- c(opened, key)
  }

  maze$passages <- passages
  maze$carve_order <- c(maze$carve_order, opened)
  maze$walls_removed <- maze$walls_removed + length(opened)
  maze
}

#' Sparsify a maze by carving out dead ends
#'
#' The opposite of [braid()]: repeatedly erases dead-end cells by removing
#' their single connecting passage, turning them into isolated cells.
#' Erasing a dead end can turn its former neighbor into a new dead end, so
#' the process repeats in passes until either no dead ends remain, a pass
#' erases nothing, or a safety cap on the number of passes is reached.
#'
#' @details
#' The resulting maze is generally **not** a perfect maze anymore: some
#' cells become fully isolated (unreachable from every other cell), so
#' `solve_maze()` may fail to find a path between an isolated cell and
#' anything else. Choosing `start`/`end` cells that remain connected (for
#' example with [set_endpoints()], or by checking [solve_maze()] succeeds)
#' is the caller's responsibility.
#'
#' `carve_order` is left untouched: it still records how the *original*
#' maze was carved, but since `sparsify()` removes some of those passages,
#' `carve_order` (or `unique()` of it) no longer reproduces the final set of
#' open walls, and replaying it (e.g. via `animate_maze()`) would show walls
#' that `sparsify()` has since closed back up. `walls_removed` is adjusted
#' down when an erased passage happened to be one of the "extra" loop walls
#' counted in it, so it keeps tracking the number of walls open beyond a
#' spanning tree of the maze.
#'
#' @param maze A `maze` object, as returned by [generate_maze()].
#' @param p Probability, in `[0, 1]`, that any given dead end is erased in
#'   a pass. Higher values cull more of the maze; `0` is a no-op.
#'
#' @return A modified `maze` object of class `maze`, with `passages` and
#'   `walls_removed` updated. `carve_order` and all other fields are
#'   unchanged (see Details).
#' @export
#' @seealso [braid()] for the opposite operation.
#'
#' @examples
#' maze <- generate_maze(10, 10, seed = 42)
#' sparse <- sparsify(maze, p = 0.5)
#' sparse$passages
sparsify <- function(maze, p = 0.5) {
  if (!.is_maze(maze)) {
    stop("`maze` must be a `maze` object.", call. = FALSE)
  }
  p <- .probability(p, "p")

  rows <- maze$rows
  cols <- maze$cols
  passages <- maze$passages
  extra_pool <- setdiff(passages, maze$carve_order)
  erased <- character()

  total_cells <- rows * cols
  cells <- expand.grid(row = seq_len(rows), col = seq_len(cols))

  for (iteration in seq_len(total_cells)) {
    degrees <- vapply(seq_len(nrow(cells)), function(i) {
      .postprocess_degree(c(cells$row[i], cells$col[i]), rows, cols, passages)
    }, integer(1L))
    dead_idx <- which(degrees == 1L)
    if (length(dead_idx) == 0L) {
      break
    }
    dead_idx <- sample(dead_idx)

    erased_this_pass <- FALSE
    for (i in dead_idx) {
      cell <- c(cells$row[i], cells$col[i])
      if (.postprocess_degree(cell, rows, cols, passages) != 1L) {
        next
      }
      if (stats::runif(1L) >= p) {
        next
      }

      neighbors <- .cell_neighbors(cell, rows, cols)
      is_passage <- vapply(seq_len(nrow(neighbors)), function(k) {
        .edge_key(cell, neighbors[k, ]) %in% passages
      }, logical(1L))
      if (!any(is_passage)) {
        next
      }

      neighbor <- as.integer(neighbors[which(is_passage)[1L], ])
      key <- .edge_key(cell, neighbor)

      passages <- setdiff(passages, key)
      erased <- c(erased, key)
      erased_this_pass <- TRUE
    }

    if (!erased_this_pass) {
      break
    }
  }

  maze$passages <- passages
  if (length(erased) > 0L) {
    removed_extra <- sum(erased %in% extra_pool)
    maze$walls_removed <- max(0L, maze$walls_removed - removed_extra)
  }
  maze
}

#' Choose new start/end cells for a maze
#'
#' Re-labels the `start` and `end` cells recorded on a `maze` object,
#' without touching its passages. Several strategies are available for
#' picking cells automatically, and an explicit `start` and/or `end`
#' overrides the chosen strategy for that endpoint.
#'
#' @param maze A `maze` object, as returned by [generate_maze()].
#' @param strategy One of `"longest"`, `"farthest"`, `"corners"`, or
#'   `"random"`:
#'   \itemize{
#'     \item `"longest"` (the default): finds the true longest shortest path
#'       in the maze (its diameter) via double breadth-first search --
#'       BFS from an arbitrary cell locates a farthest cell `A`, then BFS
#'       from `A` locates a farthest cell `B` from it; `start = A`,
#'       `end = B`.
#'     \item `"farthest"`: keeps the maze's existing `start` and sets `end`
#'       to the farthest cell from it. This is the same rule
#'       [generate_maze()] uses by default for non-`"dfs"` algorithms,
#'       exposed here so it can be reapplied after the maze has changed
#'       (e.g. after [braid()] or [sparsify()]).
#'     \item `"corners"`: `start = c(1, 1)`, `end = c(rows, cols)`.
#'     \item `"random"`: two distinct, uniformly chosen cells.
#'   }
#' @param start,end Optional `c(row, column)` positions. When supplied,
#'   they override `strategy` for that endpoint (validated to lie inside
#'   the maze).
#'
#' @return A modified `maze` object of class `maze`, with `start` and/or
#'   `end` replaced. All other fields, including `passages`, are unchanged.
#' @export
#' @seealso [generate_maze()], [solve_maze()]
#'
#' @examples
#' maze <- generate_maze(10, 10, seed = 42)
#' longest <- set_endpoints(maze, "longest")
#' corners <- set_endpoints(maze, "corners")
#' fixed <- set_endpoints(maze, start = c(1, 1), end = c(2, 2))
set_endpoints <- function(maze, strategy = c("longest", "corners", "random", "farthest"),
                           start = NULL, end = NULL) {
  if (!.is_maze(maze)) {
    stop("`maze` must be a `maze` object.", call. = FALSE)
  }
  strategy <- match.arg(strategy)

  rows <- maze$rows
  cols <- maze$cols

  computed <- NULL
  if (is.null(start) || is.null(end)) {
    computed <- switch(
      strategy,
      corners = list(start = c(1L, 1L), end = c(rows, cols)),
      random = {
        cells <- expand.grid(row = seq_len(rows), col = seq_len(cols))
        picked <- sample.int(nrow(cells), 2L)
        list(
          start = as.integer(cells[picked[1L], ]),
          end = as.integer(cells[picked[2L], ])
        )
      },
      farthest = {
        origin <- maze$start
        list(start = origin, end = .farthest_cell(rows, cols, origin, maze$passages))
      },
      longest = {
        a <- .farthest_cell(rows, cols, c(1L, 1L), maze$passages)
        b <- .farthest_cell(rows, cols, a, maze$passages)
        list(start = a, end = b)
      }
    )
  }

  final_start <- if (!is.null(start)) {
    .validate_position(start, rows, cols, "start")
  } else {
    as.integer(computed$start)
  }
  final_end <- if (!is.null(end)) {
    .validate_position(end, rows, cols, "end")
  } else {
    as.integer(computed$end)
  }

  maze$start <- final_start
  maze$end <- final_end
  maze
}

# ---- Shared helpers -------------------------------------------------------

.postprocess_degree <- function(cell, rows, cols, passages) {
  neighbors <- .cell_neighbors(cell, rows, cols)
  if (nrow(neighbors) == 0L) {
    return(0L)
  }
  sum(vapply(seq_len(nrow(neighbors)), function(i) {
    .edge_key(cell, neighbors[i, ]) %in% passages
  }, logical(1L)))
}
