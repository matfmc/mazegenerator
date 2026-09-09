#' Solve a maze or walkable matrix with graph search
#'
#' For a `maze` object, the function follows its passages. A numeric or logical
#' matrix can also be supplied, in which case cells matching `walkable` may be
#' crossed.
#'
#' @param x A `maze` object or a matrix.
#' @param start,end Integer vectors `c(row, column)`. For a maze, they default to
#'   its recorded start and end. They are required for a matrix.
#' @param walkable Values considered walkable when `x` is a matrix.
#' @param diagonal Allow diagonal movement for a matrix. Maze passages are always
#'   orthogonal. Note that `algorithm` values that require uniform step costs
#'   (`"bfs"` and `"bidirectional"`) reject `diagonal = TRUE` on a matrix
#'   (diagonal moves cost `sqrt(2)`, not 1); use `"dijkstra"` or `"astar"`
#'   instead. `algorithm = "wall_follower"` also rejects `diagonal = TRUE`
#'   because following a wall relies on the four cardinal turn directions.
#' @param trace Record the state of the search so that it can be replayed. The
#'   trace is attached to the result as the `search_trace` attribute and is what
#'   [animate_solution()] renders. Each recorded step also carries `side`,
#'   `"forward"` or `"backward"` for the three bidirectional algorithms (which
#'   frontier that expansion belongs to) and `"none"` for every other
#'   algorithm.
#' @param algorithm Search algorithm to use, one of `"astar"` (the default),
#'   `"dijkstra"`, `"bfs"`, `"greedy"`, `"bidirectional"`,
#'   `"bidirectional_dijkstra"`, `"bidirectional_astar"`, `"wall_follower"`,
#'   or `"dead_end_filling"`:
#'   \itemize{
#'     \item `"astar"`: goal-directed A* search. Its heuristic biases
#'       expansion towards `end`, so it typically expands the fewest cells.
#'     \item `"dijkstra"`: uniform-cost search — the special case A* reduces
#'       to when the heuristic is dropped — expanding uniformly outward from
#'       `start` regardless of where `end` is.
#'     \item `"bfs"`: the unweighted special case of Dijkstra, a plain
#'       FIFO-queue breadth-first search, only valid when every step has the
#'       same cost (always true for a maze; true for a matrix unless
#'       `diagonal = TRUE`).
#'     \item `"greedy"`: Greedy Best-First search. Implemented by reusing the
#'       A* machinery with the path-cost-so-far (`g`) term forced to zero, so
#'       expansion is driven purely by the heuristic distance to `end`. It is
#'       usually fast but, unlike `"astar"`, is not guaranteed to find the
#'       shortest path.
#'     \item `"bidirectional"`: breadth-first search run simultaneously from
#'       `start` and from `end`, alternating one expansion per side, until
#'       the two frontiers meet. Requires uniform step costs, like `"bfs"`.
#'     \item `"bidirectional_dijkstra"`: bidirectional Dijkstra. Like
#'       `"bidirectional"` but weighted, so it also works with the
#'       non-uniform step costs `diagonal = TRUE` gives a matrix. Critically,
#'       it does not stop at the first cell the two frontiers have in
#'       common: with non-uniform costs the first meeting is not necessarily
#'       on the shortest path, so it keeps track of the best complete path
#'       found so far (`mu`) and only stops once neither frontier can
#'       possibly improve on it.
#'     \item `"bidirectional_astar"`: NBA* (New Bidirectional A*, Pijls &
#'       Post, 2009), a goal-directed version of `"bidirectional_dijkstra"`.
#'       The forward frontier's heuristic aims at `end` and the backward
#'       frontier's aims at `start` (see `heuristic`). Running plain A* from
#'       both ends and stopping at the first meeting point is *not* a
#'       correct shortest-path algorithm, even with admissible heuristics,
#'       because a heuristic admissible for one direction is generally not
#'       consistent with the other; NBA* avoids this with per-side pruning
#'       rules tied to the shared best-known path length `mu`, without
#'       needing to evaluate the heuristic across both frontiers at once.
#'     \item `"wall_follower"`: the classic hand-on-the-wall maze-walking
#'       rule (see `hand`). It never looks at the whole maze, only at which
#'       of the cardinal directions around the current cell are open, and so
#'       it does not generally find the shortest path. On a maze with loops
#'       (see `open_walls` in [generate_maze()]) it can fail to reach `end`
#'       entirely; see `hand` below for what happens in that case.
#'     \item `"dead_end_filling"`: iteratively "fills in" every dead end
#'       (any non-start, non-end cell with at most one still-open
#'       connection) until nothing more can be removed. On a perfect maze
#'       this leaves exactly the solution corridor, which is then walked
#'       directly. On a braided maze (loops present) dead-end filling alone
#'       cannot resolve the remaining loop(s), so the surviving cells are
#'       searched with breadth-first search to recover the shortest path.
#'   }
#' @param heuristic The heuristic A*-family algorithms (`"astar"`,
#'   `"greedy"`, `"bidirectional_astar"`) use to estimate the remaining
#'   distance to the goal. Ignored (with a warning if set explicitly) by
#'   every other algorithm — `"dijkstra"` and `"bidirectional_dijkstra"`
#'   always use a heuristic of zero, and `"bfs"`, `"bidirectional"`,
#'   `"wall_follower"`, and `"dead_end_filling"` use none at all. One of:
#'   \itemize{
#'     \item `"auto"` (the default): `"manhattan"` for orthogonal movement
#'       (a maze, or a matrix with `diagonal = FALSE`), `"euclidean"` for a
#'       matrix with `diagonal = TRUE`. This is the heuristic every one of
#'       these algorithms has always used; passing `"auto"` (or omitting
#'       `heuristic`) reproduces that exact behaviour.
#'     \item `"manhattan"`: `abs(dx) + abs(dy)`. Admissible for orthogonal
#'       movement, but **not** for `diagonal = TRUE`: it charges 2 for a
#'       diagonal step that actually costs `sqrt(2)`, so it can overestimate
#'       the true remaining distance and A* is no longer guaranteed to
#'       return the shortest path. `solve_maze()` errors if `"manhattan"` is
#'       requested together with `diagonal = TRUE`.
#'     \item `"euclidean"`: straight-line distance. Admissible for
#'       `diagonal = TRUE` (it never overestimates the true cost) but looser
#'       than `"octile"`.
#'     \item `"octile"`: `(dx + dy) + (sqrt(2) - 2) * min(dx, dy)`, the tight
#'       admissible heuristic for a grid where orthogonal steps cost 1 and
#'       diagonal steps cost `sqrt(2)` — exactly this package's cost model
#'       for a matrix with `diagonal = TRUE`.
#'     \item `"chebyshev"`: `max(dx, dy)`. Admissible only when diagonal
#'       moves cost the same as orthogonal ones, which is **not** this
#'       package's cost model (diagonal moves cost `sqrt(2)` here). Provided
#'       for experimentation; it will not generally yield shortest paths in
#'       this package and is not a safe default.
#'     \item `"zero"`: always 0, which turns A* into Dijkstra (and
#'       `"bidirectional_astar"` into `"bidirectional_dijkstra"`).
#'     \item a function `function(position, target)` returning a single
#'       finite, non-negative number, for a custom heuristic. `position` and
#'       `target` are each `c(row, column)`; `solve_maze()` errors if the
#'       function ever returns something else.
#'   }
#' @param hand Which wall the `"wall_follower"` algorithm keeps a hand on,
#'   one of `"left"` (the default) or `"right"`. Ignored by every other
#'   algorithm. Following a wall only reaches `end` when `end` actually lies
#'   along some wall reachable from `start`, which is always true on a
#'   perfect maze but not guaranteed on a braided one. To avoid looping
#'   forever on a maze with cycles, the walk is capped at `4 * rows * cols`
#'   steps; if it has not reached `end` by then, `solve_maze()` emits a
#'   warning and returns a zero-row result (as if produced by
#'   [.no_cells()], adapted to a data frame).
#'
#' @return A data frame with `row` and `column`, ordered from start to end
#'   (or, only for `"wall_follower"` when it fails to reach `end`, a data
#'   frame with zero rows — see `hand`). When `trace = TRUE` it carries a
#'   `search_trace` attribute: a list with the grid dimensions, the `start`
#'   and `end` cells, the `algorithm` used, and one entry per expansion in
#'   `steps`, each holding the expanded cell (`current`) and the cells that
#'   it added to the frontier (`opened`). For `"wall_follower"`, each step is
#'   one move of the walk: `current` is the cell just arrived at and
#'   `opened` holds the single next cell the rule decides to move to (or no
#'   cells when the walk is stuck or finished). For `"dead_end_filling"`,
#'   the trace begins with one step per cell filled in (`current` is the
#'   filled cell, `opened` is always empty), followed by the ordinary
#'   breadth-first-search steps (in the same shape as `algorithm = "bfs"`)
#'   over the cells that survived filling.
#' @export
#'
#' @examples
#' maze <- generate_maze(10, 10, seed = 42)
#' path <- solve_maze(maze)
#' plot_maze(maze, solution = path)
#'
#' # Compare how many cells each algorithm expands on the same maze.
#' expansions <- vapply(c("astar", "dijkstra", "bfs"), function(algo) {
#'   solved <- solve_maze(maze, algorithm = algo, trace = TRUE)
#'   length(attr(solved, "search_trace")$steps)
#' }, integer(1))
#' expansions
solve_maze <- function(x, start = NULL, end = NULL, walkable = 1,
                       diagonal = FALSE, trace = FALSE,
                       algorithm = c("astar", "dijkstra", "bfs", "greedy",
                                     "bidirectional", "bidirectional_dijkstra",
                                     "bidirectional_astar", "wall_follower",
                                     "dead_end_filling"),
                       heuristic = "auto",
                       hand = c("left", "right")) {
  algorithm <- match.arg(algorithm)
  hand <- match.arg(hand)
  heuristic_missing <- missing(heuristic)
  if (.is_maze(x)) {
    rows <- x$rows
    cols <- x$cols
    if (is.null(start)) start <- x$start
    if (is.null(end)) end <- x$end
    start <- .validate_position(start, rows, cols, "start")
    end <- .validate_position(end, rows, cols, "end")
    neighbor_function <- function(position) {
      candidates <- .cell_neighbors(position, rows, cols)
      keep <- apply(candidates, 1L, function(candidate) {
        .has_passage(x, position, candidate)
      })
      candidates[keep, , drop = FALSE]
    }
    step_cost <- function(from, to) 1
    diagonal <- FALSE
  } else if (is.matrix(x)) {
    rows <- nrow(x)
    cols <- ncol(x)
    if (is.null(start) || is.null(end)) {
      stop("`start` and `end` are required when `x` is a matrix.", call. = FALSE)
    }
    start <- .validate_position(start, rows, cols, "start")
    end <- .validate_position(end, rows, cols, "end")
    diagonal <- isTRUE(diagonal)

    if (algorithm %in% c("bfs", "bidirectional") && diagonal) {
      stop(
        sprintf("`algorithm = \"%s\"` assumes uniform step costs, but ", algorithm),
        "`diagonal = TRUE` gives diagonal moves a cost of sqrt(2) while ",
        "orthogonal moves cost 1. Use `algorithm = \"dijkstra\"` or ",
        "`algorithm = \"astar\"` instead.",
        call. = FALSE
      )
    }
    if (algorithm == "wall_follower" && diagonal) {
      stop(
        "`algorithm = \"wall_follower\"` assumes orthogonal movement and does ",
        "not support `diagonal = TRUE`.",
        call. = FALSE
      )
    }

    if (!(x[start[1L], start[2L]] %in% walkable) ||
        !(x[end[1L], end[2L]] %in% walkable)) {
      stop("The start and end cells must both be walkable.", call. = FALSE)
    }

    neighbor_function <- function(position) {
      directions <- matrix(
        c(-1L, 0L, 1L, 0L, 0L, -1L, 0L, 1L),
        ncol = 2L,
        byrow = TRUE
      )
      if (diagonal) {
        directions <- rbind(
          directions,
          c(-1L, -1L), c(-1L, 1L), c(1L, -1L), c(1L, 1L)
        )
      }
      candidates <- sweep(directions, 2L, position, `+`)
      inside <- candidates[, 1L] >= 1L & candidates[, 1L] <= rows &
        candidates[, 2L] >= 1L & candidates[, 2L] <= cols
      candidates <- candidates[inside, , drop = FALSE]
      candidates[x[cbind(candidates[, 1L], candidates[, 2L])] %in% walkable, , drop = FALSE]
    }
    step_cost <- function(from, to) {
      if (all(abs(from - to) == 1L)) sqrt(2) else 1
    }
  } else {
    stop("`x` must be a maze object or a matrix.", call. = FALSE)
  }

  # `heuristic` only has meaning for the A*-family algorithms; every other
  # algorithm either has no heuristic (bfs, bidirectional, wall_follower,
  # dead_end_filling) or always uses a heuristic of zero (dijkstra and
  # bidirectional_dijkstra). This mirrors how `generate_maze()` handles
  # `selection` being irrelevant outside `algorithm = "growing_tree"`.
  heuristic_free_algorithms <- c(
    "bfs", "dijkstra", "bidirectional", "bidirectional_dijkstra",
    "wall_follower", "dead_end_filling"
  )
  if (algorithm %in% heuristic_free_algorithms && !heuristic_missing) {
    warning(
      sprintf("`heuristic` is not used by `algorithm = \"%s\"`; ignoring.", algorithm),
      call. = FALSE
    )
  }

  if (algorithm == "bfs") {
    .breadth_first(rows, cols, start, end, neighbor_function, trace = isTRUE(trace))
  } else if (algorithm == "bidirectional") {
    .bidirectional_bfs(rows, cols, start, end, neighbor_function, trace = isTRUE(trace))
  } else if (algorithm == "bidirectional_dijkstra") {
    .bidirectional_dijkstra(rows, cols, start, end, neighbor_function, step_cost,
                             trace = isTRUE(trace))
  } else if (algorithm == "bidirectional_astar") {
    base_heuristic <- .make_heuristic(heuristic, diagonal)
    heuristic_f <- function(position) base_heuristic(position, end)
    heuristic_b <- function(position) base_heuristic(position, start)
    .nba_star(rows, cols, start, end, neighbor_function, step_cost,
              heuristic_f, heuristic_b, trace = isTRUE(trace))
  } else if (algorithm == "wall_follower") {
    .wall_follower(rows, cols, start, end, neighbor_function, hand = hand, trace = isTRUE(trace))
  } else if (algorithm == "dead_end_filling") {
    .dead_end_filling(rows, cols, start, end, neighbor_function, trace = isTRUE(trace))
  } else {
    # astar, dijkstra, and greedy all share the same A* machinery: dijkstra
    # drops the heuristic (h = 0) and greedy drops the accumulated cost
    # (g = 0), so no separate search loop is needed for either.
    search_heuristic <- if (algorithm == "dijkstra") {
      function(position) 0
    } else {
      base_heuristic <- .make_heuristic(heuristic, diagonal)
      function(position) base_heuristic(position, end)
    }
    search_step_cost <- if (algorithm == "greedy") {
      function(from, to) 0
    } else {
      step_cost
    }
    .a_star(rows, cols, start, end, neighbor_function, search_heuristic, search_step_cost,
            trace = isTRUE(trace), algorithm = algorithm)
  }
}

.a_star <- function(rows, cols, start, end, neighbor_function, heuristic,
                    step_cost, trace = FALSE, algorithm = "astar") {
  g_score <- matrix(Inf, nrow = rows, ncol = cols)
  f_score <- matrix(Inf, nrow = rows, ncol = cols)
  closed <- matrix(FALSE, nrow = rows, ncol = cols)
  parent_row <- matrix(NA_integer_, nrow = rows, ncol = cols)
  parent_col <- matrix(NA_integer_, nrow = rows, ncol = cols)

  g_score[start[1L], start[2L]] <- 0
  f_score[start[1L], start[2L]] <- heuristic(start)
  open <- matrix(start, nrow = 1L, dimnames = list(NULL, c("row", "column")))
  steps <- if (trace) list() else NULL

  while (nrow(open) > 0L) {
    priorities <- f_score[cbind(open[, 1L], open[, 2L])]
    current_index <- which.min(priorities)
    current <- as.integer(open[current_index, ])
    open <- open[-current_index, , drop = FALSE]

    if (all(current == end)) {
      if (trace) {
        steps[[length(steps) + 1L]] <- .search_step(current, .no_cells())
      }
      path <- .reconstruct_path(parent_row, parent_col, start, end)
      if (trace) {
        attr(path, "search_trace") <- .search_trace(rows, cols, start, end, steps, algorithm)
      }
      return(path)
    }
    if (closed[current[1L], current[2L]]) {
      next
    }
    closed[current[1L], current[2L]] <- TRUE

    opened <- if (trace) list() else NULL
    neighbors <- neighbor_function(current)
    if (nrow(neighbors) == 0L) {
      if (trace) {
        steps[[length(steps) + 1L]] <- .search_step(current, .no_cells())
      }
      next
    }

    for (i in seq_len(nrow(neighbors))) {
      neighbor <- as.integer(neighbors[i, ])
      if (closed[neighbor[1L], neighbor[2L]]) {
        next
      }
      tentative <- g_score[current[1L], current[2L]] + step_cost(current, neighbor)
      if (tentative < g_score[neighbor[1L], neighbor[2L]]) {
        parent_row[neighbor[1L], neighbor[2L]] <- current[1L]
        parent_col[neighbor[1L], neighbor[2L]] <- current[2L]
        g_score[neighbor[1L], neighbor[2L]] <- tentative
        f_score[neighbor[1L], neighbor[2L]] <- tentative + heuristic(neighbor)

        already_open <- nrow(open) > 0L && any(
          open[, 1L] == neighbor[1L] & open[, 2L] == neighbor[2L]
        )
        if (!already_open) {
          open <- rbind(open, neighbor)
          if (trace) {
            opened[[length(opened) + 1L]] <- neighbor
          }
        }
      }
    }

    if (trace) {
      steps[[length(steps) + 1L]] <- .search_step(
        current,
        if (length(opened) == 0L) .no_cells() else do.call(rbind, opened)
      )
    }
  }

  stop("No path exists between `start` and `end`.", call. = FALSE)
}

.no_cells <- function() {
  matrix(integer(0), ncol = 2L, dimnames = list(NULL, c("row", "column")))
}

.search_step <- function(current, opened, side = "none") {
  list(
    current = as.integer(current),
    opened = matrix(
      as.integer(opened),
      ncol = 2L,
      dimnames = list(NULL, c("row", "column"))
    ),
    side = side
  )
}

# Returns the `side` recorded on a search step ("forward", "backward", or
# "none"), defaulting to "none" for a step read back from a trace recorded
# before `side` existed (a step is a plain list, so a missing element reads
# back as NULL rather than erroring).
.step_side <- function(step) {
  side <- step$side
  if (is.null(side)) "none" else side
}

.search_trace <- function(rows, cols, start, end, steps, algorithm = "astar") {
  structure(
    list(
      rows = rows,
      cols = cols,
      start = as.integer(start),
      end = as.integer(end),
      algorithm = algorithm,
      steps = steps
    ),
    class = "maze_search_trace"
  )
}

.is_search_trace <- function(x) {
  inherits(x, "maze_search_trace")
}

.reconstruct_path <- function(parent_row, parent_col, start, end) {
  path <- list(end)
  current <- end
  while (!all(current == start)) {
    previous <- c(
      parent_row[current[1L], current[2L]],
      parent_col[current[1L], current[2L]]
    )
    if (anyNA(previous)) {
      stop("Could not reconstruct the path.", call. = FALSE)
    }
    current <- as.integer(previous)
    path[[length(path) + 1L]] <- current
  }
  coordinates <- do.call(rbind, rev(path))
  data.frame(row = coordinates[, 1L], column = coordinates[, 2L])
}

.breadth_first <- function(rows, cols, start, end, neighbor_function, trace = FALSE,
                           algorithm = "bfs") {
  visited <- matrix(FALSE, nrow = rows, ncol = cols)
  parent_row <- matrix(NA_integer_, nrow = rows, ncol = cols)
  parent_col <- matrix(NA_integer_, nrow = rows, ncol = cols)

  visited[start[1L], start[2L]] <- TRUE
  queue <- matrix(start, nrow = 1L, dimnames = list(NULL, c("row", "column")))
  head_index <- 1L
  steps <- if (trace) list() else NULL

  while (head_index <= nrow(queue)) {
    current <- as.integer(queue[head_index, ])
    head_index <- head_index + 1L

    if (all(current == end)) {
      if (trace) {
        steps[[length(steps) + 1L]] <- .search_step(current, .no_cells())
      }
      path <- .reconstruct_path(parent_row, parent_col, start, end)
      if (trace) {
        attr(path, "search_trace") <- .search_trace(rows, cols, start, end, steps, algorithm)
      }
      return(path)
    }

    opened <- if (trace) list() else NULL
    neighbors <- neighbor_function(current)
    if (nrow(neighbors) == 0L) {
      if (trace) {
        steps[[length(steps) + 1L]] <- .search_step(current, .no_cells())
      }
      next
    }

    for (i in seq_len(nrow(neighbors))) {
      neighbor <- as.integer(neighbors[i, ])
      if (visited[neighbor[1L], neighbor[2L]]) {
        next
      }
      visited[neighbor[1L], neighbor[2L]] <- TRUE
      parent_row[neighbor[1L], neighbor[2L]] <- current[1L]
      parent_col[neighbor[1L], neighbor[2L]] <- current[2L]
      queue <- rbind(queue, neighbor)
      if (trace) {
        opened[[length(opened) + 1L]] <- neighbor
      }
    }

    if (trace) {
      steps[[length(steps) + 1L]] <- .search_step(
        current,
        if (length(opened) == 0L) .no_cells() else do.call(rbind, opened)
      )
    }
  }

  stop("No path exists between `start` and `end`.", call. = FALSE)
}

# ---- Greedy best-first, bidirectional BFS, wall follower, dead-end filling ----
#
# Greedy best-first search does not need a dedicated loop: it is expressed in
# `solve_maze()` by calling `.a_star()` with `step_cost` forced to zero, so
# `f_score` reduces to the heuristic alone. The remaining three algorithms
# below are structurally different from a frontier search and get their own
# implementations.

.bidirectional_bfs <- function(rows, cols, start, end, neighbor_function, trace = FALSE) {
  visited_f <- matrix(FALSE, nrow = rows, ncol = cols)
  visited_b <- matrix(FALSE, nrow = rows, ncol = cols)
  parent_f_row <- matrix(NA_integer_, nrow = rows, ncol = cols)
  parent_f_col <- matrix(NA_integer_, nrow = rows, ncol = cols)
  parent_b_row <- matrix(NA_integer_, nrow = rows, ncol = cols)
  parent_b_col <- matrix(NA_integer_, nrow = rows, ncol = cols)

  visited_f[start[1L], start[2L]] <- TRUE
  visited_b[end[1L], end[2L]] <- TRUE

  queue_f <- matrix(start, nrow = 1L, dimnames = list(NULL, c("row", "column")))
  queue_b <- matrix(end, nrow = 1L, dimnames = list(NULL, c("row", "column")))
  head_f <- 1L
  head_b <- 1L
  steps <- if (trace) list() else NULL
  meeting <- if (all(start == end)) as.integer(start) else NULL

  while (is.null(meeting) && (head_f <= nrow(queue_f) || head_b <= nrow(queue_b))) {
    if (head_f <= nrow(queue_f)) {
      current <- as.integer(queue_f[head_f, ])
      head_f <- head_f + 1L
      neighbors <- neighbor_function(current)
      opened <- if (trace) list() else NULL
      if (nrow(neighbors) > 0L) {
        for (i in seq_len(nrow(neighbors))) {
          neighbor <- as.integer(neighbors[i, ])
          if (!visited_f[neighbor[1L], neighbor[2L]]) {
            visited_f[neighbor[1L], neighbor[2L]] <- TRUE
            parent_f_row[neighbor[1L], neighbor[2L]] <- current[1L]
            parent_f_col[neighbor[1L], neighbor[2L]] <- current[2L]
            queue_f <- rbind(queue_f, neighbor)
            if (trace) opened[[length(opened) + 1L]] <- neighbor
            if (visited_b[neighbor[1L], neighbor[2L]]) {
              meeting <- neighbor
            }
          }
        }
      }
      if (trace) {
        steps[[length(steps) + 1L]] <- .search_step(
          current, if (length(opened) == 0L) .no_cells() else do.call(rbind, opened),
          side = "forward"
        )
      }
      if (!is.null(meeting)) break
    }

    if (head_b <= nrow(queue_b)) {
      current <- as.integer(queue_b[head_b, ])
      head_b <- head_b + 1L
      neighbors <- neighbor_function(current)
      opened <- if (trace) list() else NULL
      if (nrow(neighbors) > 0L) {
        for (i in seq_len(nrow(neighbors))) {
          neighbor <- as.integer(neighbors[i, ])
          if (!visited_b[neighbor[1L], neighbor[2L]]) {
            visited_b[neighbor[1L], neighbor[2L]] <- TRUE
            parent_b_row[neighbor[1L], neighbor[2L]] <- current[1L]
            parent_b_col[neighbor[1L], neighbor[2L]] <- current[2L]
            queue_b <- rbind(queue_b, neighbor)
            if (trace) opened[[length(opened) + 1L]] <- neighbor
            if (visited_f[neighbor[1L], neighbor[2L]]) {
              meeting <- neighbor
            }
          }
        }
      }
      if (trace) {
        steps[[length(steps) + 1L]] <- .search_step(
          current, if (length(opened) == 0L) .no_cells() else do.call(rbind, opened),
          side = "backward"
        )
      }
      if (!is.null(meeting)) break
    }
  }

  if (is.null(meeting)) {
    stop("No path exists between `start` and `end`.", call. = FALSE)
  }

  path <- .stitch_bidirectional_path(
    meeting, start, end, parent_f_row, parent_f_col, parent_b_row, parent_b_col
  )
  if (trace) {
    attr(path, "search_trace") <- .search_trace(rows, cols, start, end, steps, "bidirectional")
  }
  path
}

# Reconstructs a path from a bidirectional search's meeting cell out to
# `start` (via the forward parent pointers) and `end` (via the backward
# ones), splicing the two halves together. Shared by `.bidirectional_bfs()`,
# `.bidirectional_dijkstra()`, and `.nba_star()`.
.stitch_bidirectional_path <- function(meeting, start, end, parent_f_row, parent_f_col,
                                       parent_b_row, parent_b_col) {
  forward_path <- list(meeting)
  current <- meeting
  while (!all(current == start)) {
    previous <- c(parent_f_row[current[1L], current[2L]], parent_f_col[current[1L], current[2L]])
    if (anyNA(previous)) {
      stop("Could not reconstruct the path.", call. = FALSE)
    }
    current <- as.integer(previous)
    forward_path[[length(forward_path) + 1L]] <- current
  }
  forward_path <- rev(forward_path)

  backward_path <- list()
  current <- meeting
  while (!all(current == end)) {
    previous <- c(parent_b_row[current[1L], current[2L]], parent_b_col[current[1L], current[2L]])
    if (anyNA(previous)) {
      stop("Could not reconstruct the path.", call. = FALSE)
    }
    current <- as.integer(previous)
    backward_path[[length(backward_path) + 1L]] <- current
  }

  coordinates <- do.call(rbind, c(forward_path, backward_path))
  data.frame(row = coordinates[, 1L], column = coordinates[, 2L])
}

# Resolves the `heuristic` argument of `solve_maze()` -- a name or a
# user-supplied function -- into a function(position, target) usable by both
# a forward search (target = end) and a backward one (target = start; used
# by "bidirectional_astar"'s NBA* implementation). "auto" reproduces exactly
# what `solve_maze()` always did before `heuristic` existed: manhattan for
# orthogonal movement, euclidean once `diagonal = TRUE` makes manhattan
# inadmissible.
.make_heuristic <- function(heuristic, diagonal) {
  diagonal <- isTRUE(diagonal)

  if (is.function(heuristic)) {
    user_fn <- heuristic
    return(function(position, target) {
      value <- user_fn(position, target)
      if (!is.numeric(value) || length(value) != 1L || !is.finite(value) || value < 0) {
        stop(
          "A custom `heuristic` function must return a single finite, ",
          "non-negative number for every `(position, target)` pair.",
          call. = FALSE
        )
      }
      as.numeric(value)
    })
  }

  if (!is.character(heuristic) || length(heuristic) != 1L || is.na(heuristic)) {
    stop(
      "`heuristic` must be a single string (one of \"auto\", \"manhattan\", ",
      "\"euclidean\", \"chebyshev\", \"octile\", \"zero\") or a ",
      "function(position, target).",
      call. = FALSE
    )
  }

  name <- match.arg(heuristic, c("auto", "manhattan", "euclidean", "chebyshev", "octile", "zero"))
  if (identical(name, "auto")) {
    name <- if (diagonal) "euclidean" else "manhattan"
  }

  if (identical(name, "manhattan") && diagonal) {
    stop(
      "`heuristic = \"manhattan\"` is inadmissible with `diagonal = TRUE`: a ",
      "diagonal step costs sqrt(2) but manhattan distance charges 2 for it, ",
      "so it can overestimate the true remaining distance and A* is no ",
      "longer guaranteed to return the shortest path. Use `\"octile\"` (the ",
      "tight admissible heuristic for this package's orthogonal-cost-1 / ",
      "diagonal-cost-sqrt(2) grid) or `\"euclidean\"` (admissible but ",
      "looser) instead.",
      call. = FALSE
    )
  }

  switch(
    name,
    manhattan = function(position, target) sum(abs(position - target)),
    euclidean = function(position, target) sqrt(sum((position - target)^2)),
    chebyshev = function(position, target) max(abs(position - target)),
    octile = function(position, target) {
      delta <- abs(position - target)
      dx <- delta[1L]
      dy <- delta[2L]
      (dx + dy) + (sqrt(2) - 2) * min(dx, dy)
    },
    zero = function(position, target) 0
  )
}

# Bidirectional Dijkstra. Grows two uniform-cost frontiers, one from `start`
# and one from `end`, alternating expansion towards whichever side currently
# holds fewer open cells. Unlike `.bidirectional_bfs()` it does NOT stop at
# the first cell the two frontiers have in common: with non-uniform step
# costs (a matrix with `diagonal = TRUE`, where a diagonal step costs
# sqrt(2) and an orthogonal one costs 1) the first shared cell is not
# necessarily on the shortest path. Instead it tracks `mu`, the length of
# the best complete path found so far, updating it every time a relaxed edge
# touches a cell the other side has already reached, and only stops once
# neither open list's smallest tentative distance can still beat `mu`.
.bidirectional_dijkstra <- function(rows, cols, start, end, neighbor_function, step_cost,
                                    trace = FALSE) {
  start <- as.integer(start)
  end <- as.integer(end)
  algorithm <- "bidirectional_dijkstra"

  if (all(start == end)) {
    path <- data.frame(row = start[1L], column = start[2L])
    if (trace) {
      attr(path, "search_trace") <- .search_trace(
        rows, cols, start, end,
        list(.search_step(start, .no_cells(), side = "forward")),
        algorithm
      )
    }
    return(path)
  }

  g_f <- matrix(Inf, nrow = rows, ncol = cols)
  g_b <- matrix(Inf, nrow = rows, ncol = cols)
  closed_f <- matrix(FALSE, nrow = rows, ncol = cols)
  closed_b <- matrix(FALSE, nrow = rows, ncol = cols)
  parent_f_row <- matrix(NA_integer_, nrow = rows, ncol = cols)
  parent_f_col <- matrix(NA_integer_, nrow = rows, ncol = cols)
  parent_b_row <- matrix(NA_integer_, nrow = rows, ncol = cols)
  parent_b_col <- matrix(NA_integer_, nrow = rows, ncol = cols)

  g_f[start[1L], start[2L]] <- 0
  g_b[end[1L], end[2L]] <- 0
  open_f <- matrix(start, nrow = 1L, dimnames = list(NULL, c("row", "column")))
  open_b <- matrix(end, nrow = 1L, dimnames = list(NULL, c("row", "column")))

  mu <- Inf
  meeting <- NULL
  steps <- if (trace) list() else NULL

  update_mu <- function(node, g_here, g_other_mat) {
    other <- g_other_mat[node[1L], node[2L]]
    if (is.finite(other)) {
      candidate <- g_here + other
      if (candidate < mu) {
        mu <<- candidate
        meeting <<- node
      }
    }
  }

  while (nrow(open_f) > 0L && nrow(open_b) > 0L) {
    key_f <- min(g_f[cbind(open_f[, 1L], open_f[, 2L])])
    key_b <- min(g_b[cbind(open_b[, 1L], open_b[, 2L])])
    if (key_f + key_b >= mu) {
      break
    }

    expand_forward <- nrow(open_f) <= nrow(open_b)

    if (expand_forward) {
      priorities <- g_f[cbind(open_f[, 1L], open_f[, 2L])]
      idx <- which.min(priorities)
      x <- as.integer(open_f[idx, ])
      open_f <- open_f[-idx, , drop = FALSE]
      if (closed_f[x[1L], x[2L]]) next
      closed_f[x[1L], x[2L]] <- TRUE
      update_mu(x, g_f[x[1L], x[2L]], g_b)

      neighbors <- neighbor_function(x)
      opened <- if (trace) list() else NULL
      if (nrow(neighbors) > 0L) {
        for (i in seq_len(nrow(neighbors))) {
          y <- as.integer(neighbors[i, ])
          if (closed_f[y[1L], y[2L]]) next
          tentative <- g_f[x[1L], x[2L]] + step_cost(x, y)
          if (tentative < g_f[y[1L], y[2L]]) {
            g_f[y[1L], y[2L]] <- tentative
            parent_f_row[y[1L], y[2L]] <- x[1L]
            parent_f_col[y[1L], y[2L]] <- x[2L]
            already_open <- nrow(open_f) > 0L &&
              any(open_f[, 1L] == y[1L] & open_f[, 2L] == y[2L])
            if (!already_open) {
              open_f <- rbind(open_f, y)
              if (trace) opened[[length(opened) + 1L]] <- y
            }
            update_mu(y, tentative, g_b)
          }
        }
      }
      if (trace) {
        steps[[length(steps) + 1L]] <- .search_step(
          x, if (length(opened) == 0L) .no_cells() else do.call(rbind, opened),
          side = "forward"
        )
      }
    } else {
      priorities <- g_b[cbind(open_b[, 1L], open_b[, 2L])]
      idx <- which.min(priorities)
      x <- as.integer(open_b[idx, ])
      open_b <- open_b[-idx, , drop = FALSE]
      if (closed_b[x[1L], x[2L]]) next
      closed_b[x[1L], x[2L]] <- TRUE
      update_mu(x, g_b[x[1L], x[2L]], g_f)

      neighbors <- neighbor_function(x)
      opened <- if (trace) list() else NULL
      if (nrow(neighbors) > 0L) {
        for (i in seq_len(nrow(neighbors))) {
          y <- as.integer(neighbors[i, ])
          if (closed_b[y[1L], y[2L]]) next
          tentative <- g_b[x[1L], x[2L]] + step_cost(x, y)
          if (tentative < g_b[y[1L], y[2L]]) {
            g_b[y[1L], y[2L]] <- tentative
            parent_b_row[y[1L], y[2L]] <- x[1L]
            parent_b_col[y[1L], y[2L]] <- x[2L]
            already_open <- nrow(open_b) > 0L &&
              any(open_b[, 1L] == y[1L] & open_b[, 2L] == y[2L])
            if (!already_open) {
              open_b <- rbind(open_b, y)
              if (trace) opened[[length(opened) + 1L]] <- y
            }
            update_mu(y, tentative, g_f)
          }
        }
      }
      if (trace) {
        steps[[length(steps) + 1L]] <- .search_step(
          x, if (length(opened) == 0L) .no_cells() else do.call(rbind, opened),
          side = "backward"
        )
      }
    }
  }

  if (is.null(meeting)) {
    stop("No path exists between `start` and `end`.", call. = FALSE)
  }

  path <- .stitch_bidirectional_path(
    meeting, start, end, parent_f_row, parent_f_col, parent_b_row, parent_b_col
  )
  if (trace) {
    attr(path, "search_trace") <- .search_trace(rows, cols, start, end, steps, algorithm)
  }
  path
}

# NBA* (New Bidirectional A*; Pijls & Post, 2009), used by
# `algorithm = "bidirectional_astar"`. Grows a forward A* frontier (heuristic
# aimed at `end`) and a backward one (heuristic aimed at `start`)
# simultaneously, alternating towards whichever side holds fewer open cells.
# Simply running A* from both ends and stopping at the first meeting point is
# NOT a correct shortest-path algorithm even when both heuristics are
# individually admissible and consistent, because a heuristic consistent for
# one direction is generally not consistent with the other. NBA* avoids
# front-to-front heuristic evaluation and instead prunes each popped node `x`
# against the shared best-known path length `mu`: on the forward side, `x`
# is skipped (never expanded) once `f_f(x) >= mu` OR
# `g_f(x) + f_min_b - h_b(x) >= mu`, where `f_min_b` is the current smallest
# f-value on the backward open list and `h_b(x)` is the backward heuristic
# evaluated at `x` (i.e. the estimated distance from `x` back to `start`);
# the backward side mirrors this. The search stops once either open list is
# exhausted or its `f_min` can no longer beat `mu`.
.nba_star <- function(rows, cols, start, end, neighbor_function, step_cost,
                      heuristic_f, heuristic_b, trace = FALSE) {
  start <- as.integer(start)
  end <- as.integer(end)
  algorithm <- "bidirectional_astar"

  if (all(start == end)) {
    path <- data.frame(row = start[1L], column = start[2L])
    if (trace) {
      attr(path, "search_trace") <- .search_trace(
        rows, cols, start, end,
        list(.search_step(start, .no_cells(), side = "forward")),
        algorithm
      )
    }
    return(path)
  }

  g_f <- matrix(Inf, nrow = rows, ncol = cols)
  g_b <- matrix(Inf, nrow = rows, ncol = cols)
  f_f <- matrix(Inf, nrow = rows, ncol = cols)
  f_b <- matrix(Inf, nrow = rows, ncol = cols)
  closed_f <- matrix(FALSE, nrow = rows, ncol = cols)
  closed_b <- matrix(FALSE, nrow = rows, ncol = cols)
  parent_f_row <- matrix(NA_integer_, nrow = rows, ncol = cols)
  parent_f_col <- matrix(NA_integer_, nrow = rows, ncol = cols)
  parent_b_row <- matrix(NA_integer_, nrow = rows, ncol = cols)
  parent_b_col <- matrix(NA_integer_, nrow = rows, ncol = cols)

  g_f[start[1L], start[2L]] <- 0
  g_b[end[1L], end[2L]] <- 0
  f_f[start[1L], start[2L]] <- heuristic_f(start)
  f_b[end[1L], end[2L]] <- heuristic_b(end)

  open_f <- matrix(start, nrow = 1L, dimnames = list(NULL, c("row", "column")))
  open_b <- matrix(end, nrow = 1L, dimnames = list(NULL, c("row", "column")))

  mu <- Inf
  meeting <- NULL
  steps <- if (trace) list() else NULL

  update_mu <- function(node, g_here, g_other_mat) {
    other <- g_other_mat[node[1L], node[2L]]
    if (is.finite(other)) {
      candidate <- g_here + other
      if (candidate < mu) {
        mu <<- candidate
        meeting <<- node
      }
    }
  }

  while (nrow(open_f) > 0L && nrow(open_b) > 0L) {
    f_min_f <- min(f_f[cbind(open_f[, 1L], open_f[, 2L])])
    f_min_b <- min(f_b[cbind(open_b[, 1L], open_b[, 2L])])
    if (f_min_f >= mu || f_min_b >= mu) {
      break
    }

    expand_forward <- nrow(open_f) <= nrow(open_b)

    if (expand_forward) {
      priorities <- f_f[cbind(open_f[, 1L], open_f[, 2L])]
      idx <- which.min(priorities)
      x <- as.integer(open_f[idx, ])
      open_f <- open_f[-idx, , drop = FALSE]
      if (closed_f[x[1L], x[2L]]) next

      prune <- f_f[x[1L], x[2L]] >= mu ||
        (g_f[x[1L], x[2L]] + f_min_b - heuristic_b(x)) >= mu
      if (prune) {
        if (trace) {
          steps[[length(steps) + 1L]] <- .search_step(x, .no_cells(), side = "forward")
        }
        next
      }

      closed_f[x[1L], x[2L]] <- TRUE
      neighbors <- neighbor_function(x)
      opened <- if (trace) list() else NULL
      if (nrow(neighbors) > 0L) {
        for (i in seq_len(nrow(neighbors))) {
          y <- as.integer(neighbors[i, ])
          if (closed_f[y[1L], y[2L]]) next
          tentative <- g_f[x[1L], x[2L]] + step_cost(x, y)
          if (tentative < g_f[y[1L], y[2L]]) {
            g_f[y[1L], y[2L]] <- tentative
            f_f[y[1L], y[2L]] <- tentative + heuristic_f(y)
            parent_f_row[y[1L], y[2L]] <- x[1L]
            parent_f_col[y[1L], y[2L]] <- x[2L]
            already_open <- nrow(open_f) > 0L &&
              any(open_f[, 1L] == y[1L] & open_f[, 2L] == y[2L])
            if (!already_open) {
              open_f <- rbind(open_f, y)
              if (trace) opened[[length(opened) + 1L]] <- y
            }
            update_mu(y, tentative, g_b)
          }
        }
      }
      if (trace) {
        steps[[length(steps) + 1L]] <- .search_step(
          x, if (length(opened) == 0L) .no_cells() else do.call(rbind, opened),
          side = "forward"
        )
      }
    } else {
      priorities <- f_b[cbind(open_b[, 1L], open_b[, 2L])]
      idx <- which.min(priorities)
      x <- as.integer(open_b[idx, ])
      open_b <- open_b[-idx, , drop = FALSE]
      if (closed_b[x[1L], x[2L]]) next

      prune <- f_b[x[1L], x[2L]] >= mu ||
        (g_b[x[1L], x[2L]] + f_min_f - heuristic_f(x)) >= mu
      if (prune) {
        if (trace) {
          steps[[length(steps) + 1L]] <- .search_step(x, .no_cells(), side = "backward")
        }
        next
      }

      closed_b[x[1L], x[2L]] <- TRUE
      neighbors <- neighbor_function(x)
      opened <- if (trace) list() else NULL
      if (nrow(neighbors) > 0L) {
        for (i in seq_len(nrow(neighbors))) {
          y <- as.integer(neighbors[i, ])
          if (closed_b[y[1L], y[2L]]) next
          tentative <- g_b[x[1L], x[2L]] + step_cost(x, y)
          if (tentative < g_b[y[1L], y[2L]]) {
            g_b[y[1L], y[2L]] <- tentative
            f_b[y[1L], y[2L]] <- tentative + heuristic_b(y)
            parent_b_row[y[1L], y[2L]] <- x[1L]
            parent_b_col[y[1L], y[2L]] <- x[2L]
            already_open <- nrow(open_b) > 0L &&
              any(open_b[, 1L] == y[1L] & open_b[, 2L] == y[2L])
            if (!already_open) {
              open_b <- rbind(open_b, y)
              if (trace) opened[[length(opened) + 1L]] <- y
            }
            update_mu(y, tentative, g_f)
          }
        }
      }
      if (trace) {
        steps[[length(steps) + 1L]] <- .search_step(
          x, if (length(opened) == 0L) .no_cells() else do.call(rbind, opened),
          side = "backward"
        )
      }
    }
  }

  if (is.null(meeting)) {
    stop("No path exists between `start` and `end`.", call. = FALSE)
  }

  path <- .stitch_bidirectional_path(
    meeting, start, end, parent_f_row, parent_f_col, parent_b_row, parent_b_col
  )
  if (trace) {
    attr(path, "search_trace") <- .search_trace(rows, cols, start, end, steps, algorithm)
  }
  path
}

# Directions in clockwise order (N, E, S, W); turning is just moving along
# this cycle: +1 turns clockwise ("right"), -1 turns counter-clockwise
# ("left"), +2 turns all the way around.
.wall_follower_directions <- list(c(-1L, 0L), c(0L, 1L), c(1L, 0L), c(0L, -1L))

.wall_follower <- function(rows, cols, start, end, neighbor_function, hand = "left",
                           trace = FALSE) {
  turn <- function(dir, delta) ((dir - 1L + delta) %% 4L) + 1L

  can_move <- function(position, dir_index) {
    target <- position + .wall_follower_directions[[dir_index]]
    if (target[1L] < 1L || target[1L] > rows || target[2L] < 1L || target[2L] > cols) {
      return(FALSE)
    }
    neighbors <- neighbor_function(position)
    if (nrow(neighbors) == 0L) {
      return(FALSE)
    }
    any(neighbors[, 1L] == target[1L] & neighbors[, 2L] == target[2L])
  }

  start <- as.integer(start)
  end <- as.integer(end)

  if (all(start == end)) {
    path <- data.frame(row = start[1L], column = start[2L])
    if (trace) {
      attr(path, "search_trace") <- .search_trace(
        rows, cols, start, end, list(.search_step(start, .no_cells())), "wall_follower"
      )
    }
    return(path)
  }

  facing <- NA_integer_
  for (d in 1:4) {
    if (can_move(start, d)) {
      facing <- d
      break
    }
  }

  steps <- if (trace) list() else NULL

  if (is.na(facing)) {
    warning(
      "`algorithm = \"wall_follower\"` found no open passage out of `start`; ",
      "returning a zero-row result.",
      call. = FALSE
    )
    path <- as.data.frame(.no_cells())
    if (trace) {
      attr(path, "search_trace") <- .search_trace(rows, cols, start, end, steps, "wall_follower")
    }
    return(path)
  }

  current <- start
  visited_path <- list(current)
  cap <- 4L * rows * cols
  count <- 0L

  while (!all(current == end) && count < cap) {
    count <- count + 1L
    order <- if (identical(hand, "right")) {
      c(turn(facing, 1L), facing, turn(facing, -1L), turn(facing, 2L))
    } else {
      c(turn(facing, -1L), facing, turn(facing, 1L), turn(facing, 2L))
    }

    next_cell <- NULL
    for (d in order) {
      if (can_move(current, d)) {
        next_cell <- as.integer(current + .wall_follower_directions[[d]])
        facing <- d
        break
      }
    }

    if (trace) {
      steps[[length(steps) + 1L]] <- .search_step(
        current,
        if (is.null(next_cell)) {
          .no_cells()
        } else {
          matrix(next_cell, nrow = 1L, dimnames = list(NULL, c("row", "column")))
        }
      )
    }

    if (is.null(next_cell)) {
      break
    }
    current <- next_cell
    visited_path[[length(visited_path) + 1L]] <- current
  }

  if (!all(current == end)) {
    warning(
      sprintf(
        "`algorithm = \"wall_follower\"` did not reach `end` within %d steps ",
        cap
      ),
      "(the maze may contain loops; see `open_walls` in `generate_maze()`); ",
      "returning a zero-row result.",
      call. = FALSE
    )
    path <- as.data.frame(.no_cells())
    if (trace) {
      attr(path, "search_trace") <- .search_trace(rows, cols, start, end, steps, "wall_follower")
    }
    return(path)
  }

  coordinates <- do.call(rbind, visited_path)
  path <- data.frame(row = coordinates[, 1L], column = coordinates[, 2L])
  if (trace) {
    attr(path, "search_trace") <- .search_trace(rows, cols, start, end, steps, "wall_follower")
  }
  path
}

.dead_end_filling <- function(rows, cols, start, end, neighbor_function, trace = FALSE) {
  start <- as.integer(start)
  end <- as.integer(end)
  filled <- matrix(FALSE, nrow = rows, ncol = cols)
  fill_order <- if (trace) list() else NULL

  degree <- function(cell) {
    neighbors <- neighbor_function(cell)
    if (nrow(neighbors) == 0L) {
      return(0L)
    }
    sum(!filled[cbind(neighbors[, 1L], neighbors[, 2L])])
  }

  repeat {
    changed <- FALSE
    for (r in seq_len(rows)) {
      for (c in seq_len(cols)) {
        if (filled[r, c]) next
        if (r == start[1L] && c == start[2L]) next
        if (r == end[1L] && c == end[2L]) next
        if (degree(c(r, c)) <= 1L) {
          filled[r, c] <- TRUE
          changed <- TRUE
          if (trace) {
            fill_order[[length(fill_order) + 1L]] <- as.integer(c(r, c))
          }
        }
      }
    }
    if (!changed) break
  }

  # Whatever survives filling contains the solution: a single corridor for a
  # perfect maze, or that corridor plus any un-fillable loop(s) for a braided
  # maze. Either way, a breadth-first search restricted to the surviving
  # cells recovers the shortest path.
  reduced_neighbor_function <- function(position) {
    neighbors <- neighbor_function(position)
    if (nrow(neighbors) == 0L) {
      return(neighbors)
    }
    neighbors[!filled[cbind(neighbors[, 1L], neighbors[, 2L])], , drop = FALSE]
  }

  path <- .breadth_first(
    rows, cols, start, end, reduced_neighbor_function,
    trace = trace, algorithm = "dead_end_filling"
  )

  if (trace) {
    bfs_trace <- attr(path, "search_trace")
    fill_steps <- lapply(fill_order, function(cell) .search_step(cell, .no_cells()))
    combined_steps <- c(fill_steps, bfs_trace$steps)
    attr(path, "search_trace") <- .search_trace(
      rows, cols, start, end, combined_steps, "dead_end_filling"
    )
  }

  path
}
