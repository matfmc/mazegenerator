#' Generate a random maze
#'
#' Carves a perfect rectangular maze -- every cell is reachable and there is
#' exactly one simple path between any two cells -- using one of several
#' classic generation algorithms.
#'
#' @param rows Number of rows in the maze.
#' @param cols Number of columns in the maze.
#' @param start Integer vector `c(row, column)` giving the starting cell.
#' @param seed Optional integer seed for reproducible generation.
#' @param verbose Show generation progress in the console.
#' @param open_walls Probability, in `[0, 1]`, that any given internal wall
#'   between two adjacent cells is removed after the maze is generated. At the
#'   default of `0`, the maze is unchanged (a perfect maze: every cell is
#'   reachable and there is exactly one simple path between any two cells).
#'   Values above `0` "braid" the maze by knocking out extra walls, creating
#'   loops so that some cells have more than one route between them.
#' @param algorithm The carving algorithm to use, one of `"dfs"`, `"prim"`,
#'   `"kruskal"`, `"wilson"`, `"eller"`, `"division"`, `"aldous_broder"`,
#'   `"hunt_kill"`, `"binary_tree"`, `"sidewinder"` or `"growing_tree"`:
#'   \itemize{
#'     \item `"dfs"` (the default): randomized depth-first search with
#'       backtracking. Produces long, winding corridors with few short dead
#'       ends.
#'     \item `"prim"`: randomized Prim's algorithm, growing a single tree
#'       from `start` by repeatedly carving a random frontier wall. Produces
#'       short, bushy dead ends and a more uniformly branching texture than
#'       `"dfs"`.
#'     \item `"kruskal"`: randomized Kruskal's algorithm, carving walls in
#'       random order via a union-find of connected components. Produces a
#'       texture with many short, evenly scattered dead ends and no bias
#'       toward `start`.
#'     \item `"wilson"`: loop-erased random walks generating a uniform
#'       spanning tree, so every perfect maze on the grid is equally likely.
#'       Slower to converge for large grids but free of any algorithmic bias.
#'     \item `"eller"`: a row-by-row, constant-memory algorithm. Produces a
#'       texture similar to `"kruskal"` while only ever holding one row of
#'       state.
#'     \item `"division"`: recursive division, the only subtractive
#'       algorithm here -- it starts from a fully open room and recursively
#'       splits it with a wall (leaving a single gap) until every chamber is
#'       one cell. Produces long straight walls and a distinctly
#'       architectural, room-like texture.
#'     \item `"aldous_broder"`: a naive random walk that carves into any
#'       unvisited cell it stumbles onto, forming a uniform spanning tree
#'       (like `"wilson"`, unbiased) but typically slower to finish since it
#'       keeps wandering over already-carved territory.
#'     \item `"hunt_kill"`: random walks that carve until they get stuck,
#'       then "hunt" for the first unvisited cell adjacent to the carved
#'       area to start the next walk. Produces winding corridors similar to
#'       `"dfs"` but with fewer very long dead ends.
#'     \item `"binary_tree"`: for every cell, randomly carves toward its
#'       north or west neighbor only. Extremely fast and simple, but
#'       strongly biased: the top row and leftmost column are always open
#'       corridors, and every other cell has a diagonal drift toward the
#'       top-left corner.
#'     \item `"sidewinder"`: like `"binary_tree"` but row-by-row -- carves
#'       east along a run of cells, then closes the run out with a single
#'       north carve from a random member of it. The top row is always one
#'       continuous corridor; every other row still has a (milder) northward
#'       bias.
#'     \item `"growing_tree"`: keeps a pool of "active" cells and repeatedly
#'       carves from one of them (chosen using `selection`) to a random
#'       unvisited neighbor. A general-purpose algorithm whose texture
#'       depends entirely on `selection`.
#'   }
#'
#'   `mask`, `wrap` and `rooms` (below) are only supported for `"dfs"`,
#'   `"prim"`, `"kruskal"`, `"wilson"`, `"aldous_broder"`, `"hunt_kill"` and
#'   `"growing_tree"` -- every algorithm that carves by walking the passage
#'   graph one neighbor at a time. `"eller"`, `"binary_tree"`, `"sidewinder"`
#'   (row-by-row algorithms) and `"division"` (which recursively splits a
#'   rectangular room) all assume a complete rectangular grid; combining any
#'   of them with a non-default `mask`, `wrap` or `rooms` is an error.
#' @param selection Only used when `algorithm = "growing_tree"`; ignored
#'   (with a warning if explicitly set) otherwise. Controls which active
#'   cell the growing tree carves from next, one of `"newest"`, `"random"`,
#'   `"oldest"` or `"mixed"`:
#'   \itemize{
#'     \item `"newest"`: always the most recently added cell. This
#'       reproduces `"dfs"`.
#'     \item `"random"`: a uniformly random active cell. This approximates
#'       `"prim"`.
#'     \item `"oldest"`: always the least recently added cell, carving
#'       breadth-first. Produces long, straight corridors radiating from
#'       `start`.
#'     \item `"mixed"`: flips a coin each step between `"newest"` and
#'       `"random"`, blending the two textures.
#'   }
#' @param mask Optional logical matrix of dimension `c(rows, cols)` (or
#'   coercible to logical) marking which cells are part of the maze.
#'   `TRUE` cells are carvable; `FALSE` cells are excluded entirely (solid
#'   rock), which lets the maze take a non-rectangular shape -- a circle,
#'   a heart, a logo, text. If `mask` is supplied and `rows`/`cols` are left
#'   at their defaults, they are inferred from `dim(mask)`; if both are
#'   given explicitly they must agree with `dim(mask)`. The mask must have
#'   at least two `TRUE` cells, and every `TRUE` cell must be reachable from
#'   every other `TRUE` cell through 4-connected `TRUE` neighbors (a maze on
#'   a disconnected mask is meaningless and errors). `start` must land on a
#'   `TRUE` cell; if `start` is left at its default and it happens to fall
#'   on a `FALSE` cell, it is silently snapped to the first `TRUE` cell
#'   instead of erroring. Only supported for the neighbor-walking algorithms
#'   listed under `algorithm`. The mask is stored on the returned `maze`
#'   object.
#' @param wrap Toroidal/cylindrical wrapping of the grid's edges, one of
#'   `"none"` (the default), `"horizontal"` (column `cols` becomes adjacent
#'   to column 1, in every row), `"vertical"` (row `rows` becomes adjacent
#'   to row 1, in every column) or `"both"`. The maze is still carved as an
#'   ordinary (mask-aware) perfect maze on the flat rectangle first, so it
#'   stays fully connected and solvable by plain grid-adjacency tools like
#'   [solve_maze()]; `wrap` then randomly opens some bonus passages across
#'   the seam(s) on top of that, the same way `open_walls` adds bonus loops
#'   elsewhere. The resulting maze is no longer planar in the usual sense.
#'   [plot_maze()] always draws the maze as a flat rectangle with a solid
#'   outer border, so a seam passage is never drawn as an opening or a
#'   corridor in the picture -- it is simply invisible there, even though it
#'   is a real, walkable passage in `maze$passages`. This is expected;
#'   inspect `maze$passages` (edge keys of the form `"r1,c1|r2,c2"`) to see
#'   the wrap connections directly. Only supported for the neighbor-walking
#'   algorithms listed under `algorithm`.
#' @param rooms Optional list of integer vectors `c(row0, col0, row1, col1)`,
#'   each describing a rectangle (inclusive, 1-indexed) that is fully opened
#'   -- every internal wall inside it removed -- before carving begins, so
#'   the carving algorithm treats the room as one already-connected region
#'   (roguelike-style open chambers linked by carved corridors). Each
#'   rectangle must lie inside the maze boundaries and, if `mask` is given,
#'   entirely inside the `TRUE` region of `mask`. Because a room's interior
#'   is fully open, a maze with `rooms` is generally no longer a perfect
#'   maze (it can contain loops). Only supported for the neighbor-walking
#'   algorithms listed under `algorithm`.
#'
#' @return An object of class `maze` containing its dimensions, start and end
#'   cells, `algorithm` (the algorithm used), `carve_order` (a character
#'   vector of edge keys in the order the walls were carved -- `unique()` of
#'   which equals the perfect-maze passage set), and `passages` (the full set
#'   of open walls, including any added by `open_walls`). For
#'   `algorithm = "dfs"`, `"aldous_broder"` and `"hunt_kill"`, `traversal` and
#'   `visitation_order` additionally record the generation walk as a list of
#'   `c(row, column)` positions; for every other algorithm they are `NULL`.
#'   It also contains `open_walls`, the probability used, and
#'   `walls_removed`, the number of internal walls that were actually
#'   opened. When `open_walls > 0` the maze is no longer perfect: loops
#'   exist, and there can be more than one route between two cells.
#' @export
#'
#' @examples
#' maze <- generate_maze(10, 10, seed = 42)
#' maze
#'
#' prim_maze <- generate_maze(10, 10, seed = 42, algorithm = "prim")
#'
#' division_maze <- generate_maze(10, 10, seed = 42, algorithm = "division")
#' growing_tree_maze <- generate_maze(10, 10, seed = 42,
#'   algorithm = "growing_tree", selection = "mixed")
#'
#' # A circular maze via a mask built from Euclidean distance to the center.
#' n <- 21
#' center <- (n + 1) / 2
#' dist_from_center <- outer(seq_len(n), seq_len(n), function(r, c) {
#'   sqrt((r - center)^2 + (c - center)^2)
#' })
#' circle_mask <- dist_from_center <= n / 2
#' circular_maze <- generate_maze(mask = circle_mask, algorithm = "prim", seed = 1)
#'
#' # A wrapped (cylindrical) maze and a maze with two open rooms.
#' wrapped_maze <- generate_maze(10, 10, seed = 1, wrap = "horizontal")
#' room_maze <- generate_maze(20, 20, seed = 1,
#'   rooms = list(c(2, 2, 5, 5), c(12, 12, 16, 16)))
generate_maze <- function(rows = 20L, cols = rows, start = c(1L, 1L),
                          seed = NULL, verbose = FALSE, open_walls = 0,
                          algorithm = c("dfs", "prim", "kruskal", "wilson", "eller",
                                        "division", "aldous_broder", "hunt_kill",
                                        "binary_tree", "sidewinder", "growing_tree"),
                          selection = c("newest", "random", "oldest", "mixed"),
                          mask = NULL, wrap = c("none", "horizontal", "vertical", "both"),
                          rooms = NULL) {
  rows_missing <- missing(rows)
  cols_missing <- missing(cols)
  start_missing <- missing(start)
  wrap <- match.arg(wrap)

  if (!is.null(mask)) {
    if (!is.matrix(mask)) {
      stop("`mask` must be a matrix.", call. = FALSE)
    }
    mask_dims <- dim(mask)
    if (!rows_missing && .positive_integer(rows, "rows") != mask_dims[1L]) {
      stop("`rows` does not match `nrow(mask)`.", call. = FALSE)
    }
    if (!cols_missing && .positive_integer(cols, "cols") != mask_dims[2L]) {
      stop("`cols` does not match `ncol(mask)`.", call. = FALSE)
    }
    if (rows_missing) rows <- mask_dims[1L]
    if (cols_missing) cols <- mask_dims[2L]
  }

  rows <- .positive_integer(rows, "rows")
  cols <- .positive_integer(cols, "cols")

  if (!is.null(mask)) {
    mask <- .validate_mask(mask, rows, cols)
  }

  if (!is.null(mask) && start_missing && !mask[start[1L], start[2L]]) {
    true_cells <- which(mask, arr.ind = TRUE)
    true_cells <- true_cells[order(true_cells[, 1L], true_cells[, 2L]), , drop = FALSE]
    start <- as.integer(true_cells[1L, ])
  }

  start <- .validate_position(start, rows, cols, "start")
  if (!is.null(mask) && !mask[start[1L], start[2L]]) {
    stop("`start` must be a `TRUE` cell in `mask`.", call. = FALSE)
  }

  open_walls <- .probability(open_walls, "open_walls")
  algorithm <- match.arg(algorithm)
  selection_missing <- missing(selection)
  selection <- match.arg(selection)

  if (algorithm != "growing_tree" && !selection_missing) {
    warning(
      "`selection` is only used when `algorithm = \"growing_tree\"`; ignoring.",
      call. = FALSE
    )
  }

  if (!is.null(rooms)) {
    rooms <- .validate_rooms(rooms, rows, cols, mask)
  }

  topology_used <- !is.null(mask) || wrap != "none" || !is.null(rooms)
  unsupported_algorithms <- c("eller", "division", "binary_tree", "sidewinder")
  if (topology_used && algorithm %in% unsupported_algorithms) {
    stop(
      sprintf(
        "`mask`, `wrap` and `rooms` are not supported with `algorithm = \"%s\"` (it assumes a complete rectangular grid); use \"dfs\", \"prim\", \"kruskal\", \"wilson\", \"aldous_broder\", \"hunt_kill\" or \"growing_tree\" instead.",
        algorithm
      ),
      call. = FALSE
    )
  }

  if (!is.null(seed)) {
    seed <- .whole_number(seed, "seed")
    set.seed(seed)
  }

  # `wrap` is deliberately NOT threaded into the core carving algorithms
  # below: the spanning tree they build always uses plain rectangular
  # (mask-aware) adjacency, so the maze stays fully connected and
  # solvable by ordinary grid-neighbor logic (e.g. `solve_maze()`, which
  # has no notion of a wrapped edge). Wrapping is instead layered on
  # afterwards as bonus loop passages across the seam(s) -- see below --
  # the same way `open_walls` adds bonus loops, rather than as load-bearing
  # tree edges that could strand parts of the maze from non-wrap-aware
  # consumers.
  result <- switch(
    algorithm,
    dfs = .generate_dfs(rows, cols, start, verbose, mask),
    prim = .generate_prim(rows, cols, start, verbose, mask),
    kruskal = .generate_kruskal(rows, cols, verbose, mask),
    wilson = .generate_wilson(rows, cols, start, verbose, mask),
    eller = .generate_eller(rows, cols, verbose),
    division = .generate_division(rows, cols, verbose),
    aldous_broder = .generate_aldous_broder(rows, cols, start, verbose, mask),
    hunt_kill = .generate_hunt_kill(rows, cols, start, verbose, mask),
    binary_tree = .generate_binary_tree(rows, cols, verbose),
    sidewinder = .generate_sidewinder(rows, cols, verbose),
    growing_tree = .generate_growing_tree(rows, cols, start, selection, verbose, mask)
  )

  carve_order <- result$carve_order

  room_edges <- character()
  if (!is.null(rooms)) {
    room_edges <- setdiff(.room_open_edges(rooms), carve_order)
    carve_order <- c(room_edges, carve_order)
  }

  passages <- carve_order

  if (algorithm == "dfs") {
    end <- as.integer(utils::tail(result$visitation_order, 1L)[[1L]])
  } else {
    end <- .farthest_cell(rows, cols, start, passages, mask)
  }

  walls_removed <- 0L

  if (wrap != "none") {
    seam_keys <- setdiff(
      .wall_candidate_keys(rows, cols, mask, wrap),
      .wall_candidate_keys(rows, cols, mask, "none")
    )
    seam_keys <- setdiff(seam_keys, passages)
    if (length(seam_keys) > 0L) {
      opened_seam <- seam_keys[stats::runif(length(seam_keys)) < 0.5]
      if (length(opened_seam) > 0L) {
        passages <- c(passages, opened_seam)
        carve_order <- c(carve_order, opened_seam)
        walls_removed <- walls_removed + length(opened_seam)
      }
    }
  }

  if (open_walls > 0) {
    candidate_keys <- .wall_candidate_keys(rows, cols, mask, "none")
    walls <- setdiff(candidate_keys, passages)
    if (length(walls) > 0L) {
      opened <- walls[stats::runif(length(walls)) < open_walls]
      if (length(opened) > 0L) {
        passages <- c(passages, opened)
        walls_removed <- walls_removed + length(opened)
      }
    }
  }

  structure(
    list(
      rows = rows,
      cols = cols,
      start = start,
      end = end,
      algorithm = algorithm,
      traversal = result$traversal,
      visitation_order = result$visitation_order,
      carve_order = carve_order,
      passages = passages,
      seed = seed,
      open_walls = open_walls,
      walls_removed = walls_removed,
      mask = mask,
      wrap = wrap,
      rooms = rooms
    ),
    class = "maze"
  )
}

#' @export
print.maze <- function(x, ...) {
  cat(sprintf("<maze> %d x %d\n", x$rows, x$cols))
  cat(sprintf("  algorithm: %s\n", x$algorithm))
  cat(sprintf("  start: (%d, %d)\n", x$start[1L], x$start[2L]))
  cat(sprintf("  end:   (%d, %d)\n", x$end[1L], x$end[2L]))
  if (!is.null(x$mask)) {
    cat(sprintf("  cells: %d (masked out of %d)\n", sum(x$mask), x$rows * x$cols))
  } else {
    cat(sprintf("  cells: %d\n", x$rows * x$cols))
  }
  if (!is.null(x$wrap) && !identical(x$wrap, "none")) {
    cat(sprintf("  wrap: %s\n", x$wrap))
  }
  if (!is.null(x$rooms) && length(x$rooms) > 0L) {
    cat(sprintf("  rooms: %d\n", length(x$rooms)))
  }
  if (isTRUE(x$walls_removed > 0)) {
    cat(sprintf("  loops: %d walls removed (p = %g)\n", x$walls_removed, x$open_walls))
  }
  invisible(x)
}

# ---- DFS ---------------------------------------------------------------

.generate_dfs <- function(rows, cols, start, verbose, mask = NULL, wrap = "none") {
  visited <- matrix(FALSE, nrow = rows, ncol = cols)
  visited[start[1L], start[2L]] <- TRUE
  visited_count <- 1L

  current <- start
  stack <- list(start)
  traversal <- list(start)
  visitation_order <- list(start)
  total_cells <- if (is.null(mask)) rows * cols else sum(mask)

  while (visited_count < total_cells) {
    candidates <- .cell_neighbors(current, rows, cols, mask, wrap)
    keep <- !visited[cbind(candidates[, 1L], candidates[, 2L])]
    candidates <- candidates[keep, , drop = FALSE]

    if (nrow(candidates) > 0L) {
      current <- candidates[sample.int(nrow(candidates), 1L), ]
      current <- as.integer(current)
      stack[[length(stack) + 1L]] <- current
      visited[current[1L], current[2L]] <- TRUE
      visited_count <- visited_count + 1L
      visitation_order[[length(visitation_order) + 1L]] <- current
    } else {
      stack <- stack[-length(stack)]
      current <- stack[[length(stack)]]
    }

    traversal[[length(traversal) + 1L]] <- current

    if (isTRUE(verbose)) {
      cat(sprintf("\rGenerating maze: %5.1f%%", 100 * visited_count / total_cells))
    }
  }

  if (isTRUE(verbose)) {
    cat("\nMaze generation complete.\n")
  }

  list(
    carve_order = .carve_order_from_traversal(traversal),
    traversal = traversal,
    visitation_order = visitation_order
  )
}

.carve_order_from_traversal <- function(traversal) {
  if (length(traversal) < 2L) {
    return(character())
  }
  keys <- vapply(
    seq_len(length(traversal) - 1L),
    function(i) .edge_key(traversal[[i]], traversal[[i + 1L]]),
    character(1L)
  )
  unique(keys)
}

# ---- Prim ---------------------------------------------------------------

.generate_prim <- function(rows, cols, start, verbose, mask = NULL, wrap = "none") {
  total_cells <- if (is.null(mask)) rows * cols else sum(mask)
  in_tree <- matrix(FALSE, nrow = rows, ncol = cols)
  in_tree[start[1L], start[2L]] <- TRUE
  in_tree_count <- 1L

  carve_order <- character(total_cells - 1L)
  carved <- 0L

  # Frontier walls stored as a matrix of from_row, from_col, to_row, to_col,
  # where `from` is already in the tree and `to` may or may not be.
  frontier <- .neighbor_walls(start, rows, cols, mask, wrap)

  while (in_tree_count < total_cells) {
    n <- nrow(frontier)
    pick <- if (n == 1L) 1L else sample.int(n, 1L)
    wall <- frontier[pick, ]
    frontier <- frontier[-pick, , drop = FALSE]

    to <- c(wall[3L], wall[4L])
    if (!in_tree[to[1L], to[2L]]) {
      from <- c(wall[1L], wall[2L])
      carved <- carved + 1L
      carve_order[carved] <- .edge_key(from, to)

      in_tree[to[1L], to[2L]] <- TRUE
      in_tree_count <- in_tree_count + 1L

      new_walls <- .neighbor_walls(to, rows, cols, mask, wrap)
      keep <- !in_tree[cbind(new_walls[, 3L], new_walls[, 4L])]
      new_walls <- new_walls[keep, , drop = FALSE]
      if (nrow(new_walls) > 0L) {
        frontier <- rbind(frontier, new_walls)
      }

      if (isTRUE(verbose)) {
        cat(sprintf("\rGenerating maze: %5.1f%%", 100 * in_tree_count / total_cells))
      }
    }
  }

  if (isTRUE(verbose)) {
    cat("\nMaze generation complete.\n")
  }

  list(carve_order = carve_order[seq_len(carved)], traversal = NULL, visitation_order = NULL)
}

.neighbor_walls <- function(position, rows, cols, mask = NULL, wrap = "none") {
  neighbors <- .cell_neighbors(position, rows, cols, mask, wrap)
  if (nrow(neighbors) == 0L) {
    return(matrix(integer(0), ncol = 4L))
  }
  cbind(
    matrix(position[1L], nrow = nrow(neighbors), ncol = 1L),
    matrix(position[2L], nrow = nrow(neighbors), ncol = 1L),
    neighbors
  )
}

# ---- Kruskal --------------------------------------------------------------

.generate_kruskal <- function(rows, cols, verbose, mask = NULL, wrap = "none") {
  walls <- .wall_candidates(rows, cols, mask, wrap)
  total_walls <- nrow(walls)
  total_cells <- rows * cols
  target_cells <- if (is.null(mask)) total_cells else sum(mask)

  if (total_walls == 0L) {
    return(list(carve_order = character(), traversal = NULL, visitation_order = NULL))
  }

  order <- sample.int(total_walls)
  parent <- seq_len(total_cells)
  rank <- integer(total_cells)

  carve_order <- character(target_cells - 1L)
  carved <- 0L

  for (i in order) {
    from_id <- (walls[i, 2L] - 1L) * rows + walls[i, 1L]
    to_id <- (walls[i, 4L] - 1L) * rows + walls[i, 3L]

    roots <- .union_find_union(parent, rank, from_id, to_id)
    parent <- roots$parent
    rank <- roots$rank

    if (roots$merged) {
      carved <- carved + 1L
      carve_order[carved] <- .edge_key(walls[i, 1L:2L], walls[i, 3L:4L])

      if (isTRUE(verbose)) {
        cat(sprintf("\rGenerating maze: %5.1f%%", 100 * carved / (target_cells - 1L)))
      }

      if (carved == target_cells - 1L) {
        break
      }
    }
  }

  if (isTRUE(verbose)) {
    cat("\nMaze generation complete.\n")
  }

  list(carve_order = carve_order[seq_len(carved)], traversal = NULL, visitation_order = NULL)
}

.union_find_find <- function(parent, x) {
  root <- x
  while (parent[root] != root) {
    root <- parent[root]
  }
  while (parent[x] != root) {
    next_x <- parent[x]
    parent[x] <- root
    x <- next_x
  }
  list(parent = parent, root = root)
}

.union_find_union <- function(parent, rank, x, y) {
  found_x <- .union_find_find(parent, x)
  parent <- found_x$parent
  root_x <- found_x$root

  found_y <- .union_find_find(parent, y)
  parent <- found_y$parent
  root_y <- found_y$root

  if (root_x == root_y) {
    return(list(parent = parent, rank = rank, merged = FALSE))
  }

  if (rank[root_x] < rank[root_y]) {
    tmp <- root_x
    root_x <- root_y
    root_y <- tmp
  }
  parent[root_y] <- root_x
  if (rank[root_x] == rank[root_y]) {
    rank[root_x] <- rank[root_x] + 1L
  }

  list(parent = parent, rank = rank, merged = TRUE)
}

# ---- Wilson ---------------------------------------------------------------

.generate_wilson <- function(rows, cols, start, verbose, mask = NULL, wrap = "none") {
  carvable <- if (is.null(mask)) matrix(TRUE, nrow = rows, ncol = cols) else mask
  total_cells <- sum(carvable)
  in_tree <- matrix(FALSE, nrow = rows, ncol = cols)
  in_tree[start[1L], start[2L]] <- TRUE
  in_tree_count <- 1L

  carve_order <- character(total_cells - 1L)
  carved <- 0L

  # Direction codes recorded per unvisited cell during the current walk:
  # 1 = up, 2 = down, 3 = left, 4 = right.
  direction <- matrix(0L, nrow = rows, ncol = cols)

  while (in_tree_count < total_cells) {
    remaining <- which(!in_tree & carvable, arr.ind = TRUE)
    if (nrow(remaining) == 0L) {
      break
    }
    start_idx <- if (nrow(remaining) == 1L) 1L else sample.int(nrow(remaining), 1L)
    origin <- as.integer(remaining[start_idx, ])

    current <- origin
    while (!in_tree[current[1L], current[2L]]) {
      neighbors <- .cell_neighbors(current, rows, cols, mask, wrap)
      next_idx <- if (nrow(neighbors) == 1L) 1L else sample.int(nrow(neighbors), 1L)
      next_cell <- as.integer(neighbors[next_idx, ])

      code <- if (next_cell[1L] < current[1L]) 1L
        else if (next_cell[1L] > current[1L]) 2L
        else if (next_cell[2L] < current[2L]) 3L
        else 4L
      direction[current[1L], current[2L]] <- code

      current <- next_cell
    }

    # Retrace from origin along the recorded directions, carving as we go.
    # This automatically erases loops since a revisited cell's direction was
    # overwritten by the later pass through it.
    cell <- origin
    while (!in_tree[cell[1L], cell[2L]]) {
      code <- direction[cell[1L], cell[2L]]
      next_cell <- switch(
        code,
        c(cell[1L] - 1L, cell[2L]),
        c(cell[1L] + 1L, cell[2L]),
        c(cell[1L], cell[2L] - 1L),
        c(cell[1L], cell[2L] + 1L)
      )

      carved <- carved + 1L
      carve_order[carved] <- .edge_key(cell, next_cell)

      in_tree[cell[1L], cell[2L]] <- TRUE
      in_tree_count <- in_tree_count + 1L
      direction[cell[1L], cell[2L]] <- 0L

      cell <- next_cell
    }

    if (isTRUE(verbose)) {
      cat(sprintf("\rGenerating maze: %5.1f%%", 100 * in_tree_count / total_cells))
    }
  }

  if (isTRUE(verbose)) {
    cat("\nMaze generation complete.\n")
  }

  list(carve_order = carve_order[seq_len(carved)], traversal = NULL, visitation_order = NULL)
}

# ---- Eller ------------------------------------------------------------

.generate_eller <- function(rows, cols, verbose) {
  total_cells <- rows * cols
  carve_order <- character(total_cells - 1L)
  carved <- 0L

  next_set_id <- 1L
  row_sets <- rep(0L, cols)

  for (r in seq_len(rows)) {
    unset <- row_sets == 0L
    if (any(unset)) {
      n_unset <- sum(unset)
      row_sets[unset] <- seq.int(next_set_id, next_set_id + n_unset - 1L)
      next_set_id <- next_set_id + n_unset
    }

    is_last_row <- r == rows

    if (cols > 1L) {
      for (c in seq_len(cols - 1L)) {
        should_merge <- is_last_row || stats::runif(1L) < 0.5
        if (should_merge && row_sets[c] != row_sets[c + 1L]) {
          carved <- carved + 1L
          carve_order[carved] <- .edge_key(c(r, c), c(r, c + 1L))

          old_set <- row_sets[c + 1L]
          new_set <- row_sets[c]
          row_sets[row_sets == old_set] <- new_set
        }
      }
    }

    if (!is_last_row) {
      next_row_sets <- rep(0L, cols)
      sets_in_row <- unique(row_sets)
      for (set_id in sets_in_row) {
        members <- which(row_sets == set_id)
        if (length(members) == 1L) {
          n_down <- 1L
        } else {
          n_down <- sample.int(length(members), 1L)
        }
        chosen <- if (length(members) == 1L) members else sort(sample(members, n_down))

        for (c in chosen) {
          carved <- carved + 1L
          carve_order[carved] <- .edge_key(c(r, c), c(r + 1L, c))
          next_row_sets[c] <- set_id
        }
      }
      row_sets <- next_row_sets
    }

    if (isTRUE(verbose)) {
      cat(sprintf("\rGenerating maze: %5.1f%%", 100 * r / rows))
    }
  }

  if (isTRUE(verbose)) {
    cat("\nMaze generation complete.\n")
  }

  list(carve_order = carve_order[seq_len(carved)], traversal = NULL, visitation_order = NULL)
}

# ---- Recursive Division ----------------------------------------------------

.generate_division <- function(rows, cols, verbose) {
  total_cells <- rows * cols
  total_splits <- max(total_cells - 1L, 0L)
  carve_order <- character(total_splits)
  carved <- 0L

  # Chambers are (row0, row1, col0, col1) inclusive cell ranges. Every
  # internal edge starts open (the grid begins as one fully open room) and
  # each split records the single gap edge that keeps the two resulting
  # chambers connected, so the recorded edges form a spanning tree.
  stack <- list(c(1L, rows, 1L, cols))

  while (length(stack) > 0L) {
    chamber <- stack[[length(stack)]]
    stack <- stack[-length(stack)]

    row0 <- chamber[1L]; row1 <- chamber[2L]
    col0 <- chamber[3L]; col1 <- chamber[4L]
    height <- row1 - row0 + 1L
    width <- col1 - col0 + 1L

    if (height < 2L && width < 2L) {
      next
    }

    horizontal <- if (width < 2L) {
      TRUE
    } else if (height < 2L) {
      FALSE
    } else if (height > width) {
      TRUE
    } else if (width > height) {
      FALSE
    } else {
      stats::runif(1L) < 0.5
    }

    if (horizontal) {
      wall_row <- row0 - 1L + sample.int(row1 - row0, 1L)
      gap_col <- if (col1 == col0) col0 else sample(col0:col1, 1L)

      carved <- carved + 1L
      carve_order[carved] <- .edge_key(c(wall_row, gap_col), c(wall_row + 1L, gap_col))

      stack[[length(stack) + 1L]] <- c(row0, wall_row, col0, col1)
      stack[[length(stack) + 1L]] <- c(wall_row + 1L, row1, col0, col1)
    } else {
      wall_col <- col0 - 1L + sample.int(col1 - col0, 1L)
      gap_row <- if (row1 == row0) row0 else sample(row0:row1, 1L)

      carved <- carved + 1L
      carve_order[carved] <- .edge_key(c(gap_row, wall_col), c(gap_row, wall_col + 1L))

      stack[[length(stack) + 1L]] <- c(row0, row1, col0, wall_col)
      stack[[length(stack) + 1L]] <- c(row0, row1, wall_col + 1L, col1)
    }

    if (isTRUE(verbose)) {
      cat(sprintf("\rGenerating maze: %5.1f%%", 100 * carved / max(total_splits, 1L)))
    }
  }

  if (isTRUE(verbose)) {
    cat("\nMaze generation complete.\n")
  }

  list(carve_order = carve_order[seq_len(carved)], traversal = NULL, visitation_order = NULL)
}

# ---- Aldous-Broder ----------------------------------------------------------

.generate_aldous_broder <- function(rows, cols, start, verbose, mask = NULL, wrap = "none") {
  total_cells <- if (is.null(mask)) rows * cols else sum(mask)
  visited <- matrix(FALSE, nrow = rows, ncol = cols)
  visited[start[1L], start[2L]] <- TRUE
  visited_count <- 1L

  carve_order <- character(total_cells - 1L)
  carved <- 0L

  current <- start
  traversal <- list(start)
  visitation_order <- list(start)

  while (visited_count < total_cells) {
    neighbors <- .cell_neighbors(current, rows, cols, mask, wrap)
    next_cell <- as.integer(neighbors[sample.int(nrow(neighbors), 1L), ])

    if (!visited[next_cell[1L], next_cell[2L]]) {
      carved <- carved + 1L
      carve_order[carved] <- .edge_key(current, next_cell)
      visited[next_cell[1L], next_cell[2L]] <- TRUE
      visited_count <- visited_count + 1L
      visitation_order[[length(visitation_order) + 1L]] <- next_cell

      if (isTRUE(verbose)) {
        cat(sprintf("\rGenerating maze: %5.1f%%", 100 * visited_count / total_cells))
      }
    }

    current <- next_cell
    traversal[[length(traversal) + 1L]] <- current
  }

  if (isTRUE(verbose)) {
    cat("\nMaze generation complete.\n")
  }

  list(
    carve_order = carve_order[seq_len(carved)],
    traversal = traversal,
    visitation_order = visitation_order
  )
}

# ---- Hunt-and-Kill ----------------------------------------------------------

.generate_hunt_kill <- function(rows, cols, start, verbose, mask = NULL, wrap = "none") {
  total_cells <- if (is.null(mask)) rows * cols else sum(mask)
  visited <- matrix(FALSE, nrow = rows, ncol = cols)
  visited[start[1L], start[2L]] <- TRUE
  visited_count <- 1L

  carve_order <- character(total_cells - 1L)
  carved <- 0L

  current <- start
  traversal <- list(start)
  visitation_order <- list(start)

  while (visited_count < total_cells) {
    neighbors <- .cell_neighbors(current, rows, cols, mask, wrap)
    keep <- !visited[cbind(neighbors[, 1L], neighbors[, 2L])]
    candidates <- neighbors[keep, , drop = FALSE]

    if (nrow(candidates) > 0L) {
      next_cell <- as.integer(candidates[sample.int(nrow(candidates), 1L), ])
      carved <- carved + 1L
      carve_order[carved] <- .edge_key(current, next_cell)
      visited[next_cell[1L], next_cell[2L]] <- TRUE
      visited_count <- visited_count + 1L

      current <- next_cell
      traversal[[length(traversal) + 1L]] <- current
      visitation_order[[length(visitation_order) + 1L]] <- current
    } else {
      # Hunt: scan the grid in row-major order for the first unvisited cell
      # that is adjacent to the carved area, and link it in.
      linked <- FALSE
      for (r in seq_len(rows)) {
        for (c in seq_len(cols)) {
          if (!is.null(mask) && !mask[r, c]) {
            next
          }
          if (!visited[r, c]) {
            cell <- c(r, c)
            cell_neighbors <- .cell_neighbors(cell, rows, cols, mask, wrap)
            visited_neighbor <- visited[cbind(cell_neighbors[, 1L], cell_neighbors[, 2L])]

            if (any(visited_neighbor)) {
              linkable <- cell_neighbors[visited_neighbor, , drop = FALSE]
              link_to <- as.integer(linkable[sample.int(nrow(linkable), 1L), ])

              carved <- carved + 1L
              carve_order[carved] <- .edge_key(cell, link_to)
              visited[r, c] <- TRUE
              visited_count <- visited_count + 1L

              current <- cell
              traversal[[length(traversal) + 1L]] <- current
              visitation_order[[length(visitation_order) + 1L]] <- current
              linked <- TRUE
              break
            }
          }
        }
        if (linked) {
          break
        }
      }
      if (!linked) {
        break
      }
    }

    if (isTRUE(verbose)) {
      cat(sprintf("\rGenerating maze: %5.1f%%", 100 * visited_count / total_cells))
    }
  }

  if (isTRUE(verbose)) {
    cat("\nMaze generation complete.\n")
  }

  list(
    carve_order = carve_order[seq_len(carved)],
    traversal = traversal,
    visitation_order = visitation_order
  )
}

# ---- Binary Tree ------------------------------------------------------------

.generate_binary_tree <- function(rows, cols, verbose) {
  total_cells <- rows * cols
  carve_order <- character(max(total_cells - 1L, 0L))
  carved <- 0L
  processed <- 0L

  for (r in seq_len(rows)) {
    for (c in seq_len(cols)) {
      has_north <- r > 1L
      has_west <- c > 1L

      carve_to <- if (has_north && has_west) {
        if (stats::runif(1L) < 0.5) c(r - 1L, c) else c(r, c - 1L)
      } else if (has_north) {
        c(r - 1L, c)
      } else if (has_west) {
        c(r, c - 1L)
      } else {
        NULL
      }

      if (!is.null(carve_to)) {
        carved <- carved + 1L
        carve_order[carved] <- .edge_key(c(r, c), carve_to)
      }

      processed <- processed + 1L
      if (isTRUE(verbose)) {
        cat(sprintf("\rGenerating maze: %5.1f%%", 100 * processed / total_cells))
      }
    }
  }

  if (isTRUE(verbose)) {
    cat("\nMaze generation complete.\n")
  }

  list(carve_order = carve_order[seq_len(carved)], traversal = NULL, visitation_order = NULL)
}

# ---- Sidewinder -------------------------------------------------------------

.generate_sidewinder <- function(rows, cols, verbose) {
  total_cells <- rows * cols
  carve_order <- character(max(total_cells - 1L, 0L))
  carved <- 0L

  for (r in seq_len(rows)) {
    run_start <- 1L
    at_north <- r == 1L

    for (c in seq_len(cols)) {
      at_east <- c == cols
      close_out <- at_east || (!at_north && stats::runif(1L) < 0.5)

      if (!close_out) {
        carved <- carved + 1L
        carve_order[carved] <- .edge_key(c(r, c), c(r, c + 1L))
      } else {
        if (!at_north) {
          member_col <- if (run_start == c) run_start else sample(run_start:c, 1L)
          carved <- carved + 1L
          carve_order[carved] <- .edge_key(c(r, member_col), c(r - 1L, member_col))
        }
        run_start <- c + 1L
      }
    }

    if (isTRUE(verbose)) {
      cat(sprintf("\rGenerating maze: %5.1f%%", 100 * r / rows))
    }
  }

  if (isTRUE(verbose)) {
    cat("\nMaze generation complete.\n")
  }

  list(carve_order = carve_order[seq_len(carved)], traversal = NULL, visitation_order = NULL)
}

# ---- Growing Tree -----------------------------------------------------------

.generate_growing_tree <- function(rows, cols, start, selection, verbose, mask = NULL, wrap = "none") {
  total_cells <- if (is.null(mask)) rows * cols else sum(mask)
  visited <- matrix(FALSE, nrow = rows, ncol = cols)
  visited[start[1L], start[2L]] <- TRUE
  visited_count <- 1L

  carve_order <- character(total_cells - 1L)
  carved <- 0L

  active <- list(start)

  while (length(active) > 0L) {
    n_active <- length(active)
    idx <- switch(
      selection,
      newest = n_active,
      oldest = 1L,
      random = if (n_active == 1L) 1L else sample.int(n_active, 1L),
      mixed = if (stats::runif(1L) < 0.5) {
        n_active
      } else if (n_active == 1L) {
        1L
      } else {
        sample.int(n_active, 1L)
      }
    )

    cell <- active[[idx]]
    neighbors <- .cell_neighbors(cell, rows, cols, mask, wrap)
    keep <- !visited[cbind(neighbors[, 1L], neighbors[, 2L])]
    candidates <- neighbors[keep, , drop = FALSE]

    if (nrow(candidates) > 0L) {
      next_cell <- as.integer(candidates[sample.int(nrow(candidates), 1L), ])
      carved <- carved + 1L
      carve_order[carved] <- .edge_key(cell, next_cell)

      visited[next_cell[1L], next_cell[2L]] <- TRUE
      visited_count <- visited_count + 1L
      active[[length(active) + 1L]] <- next_cell

      if (isTRUE(verbose)) {
        cat(sprintf("\rGenerating maze: %5.1f%%", 100 * visited_count / total_cells))
      }
    } else {
      active <- active[-idx]
    }
  }

  if (isTRUE(verbose)) {
    cat("\nMaze generation complete.\n")
  }

  list(carve_order = carve_order[seq_len(carved)], traversal = NULL, visitation_order = NULL)
}

# ---- Shared helpers -------------------------------------------------------

.farthest_cell <- function(rows, cols, start, passages, mask = NULL, wrap = "none") {
  total_cells <- if (is.null(mask)) rows * cols else sum(mask)
  if (total_cells == 1L) {
    return(as.integer(start))
  }

  distance <- matrix(-1L, nrow = rows, ncol = cols)
  distance[start[1L], start[2L]] <- 0L
  queue <- list(start)
  head_idx <- 1L
  farthest <- as.integer(start)
  farthest_distance <- 0L

  passage_set <- passages

  while (head_idx <= length(queue)) {
    current <- queue[[head_idx]]
    head_idx <- head_idx + 1L
    current_distance <- distance[current[1L], current[2L]]

    neighbors <- .cell_neighbors(current, rows, cols, mask, wrap)
    if (nrow(neighbors) > 0L) {
      for (i in seq_len(nrow(neighbors))) {
        neighbor <- as.integer(neighbors[i, ])
        if (distance[neighbor[1L], neighbor[2L]] == -1L &&
            .edge_key(current, neighbor) %in% passage_set) {
          new_distance <- current_distance + 1L
          distance[neighbor[1L], neighbor[2L]] <- new_distance
          queue[[length(queue) + 1L]] <- neighbor
          if (new_distance > farthest_distance) {
            farthest_distance <- new_distance
            farthest <- neighbor
          }
        }
      }
    }
  }

  farthest
}

.cell_neighbors <- function(position, rows, cols, mask = NULL, wrap = "none") {
  directions <- matrix(
    c(-1L, 0L, 1L, 0L, 0L, -1L, 0L, 1L),
    ncol = 2L,
    byrow = TRUE
  )
  cells <- sweep(directions, 2L, position, `+`)

  if (wrap %in% c("vertical", "both")) {
    cells[, 1L] <- ((cells[, 1L] - 1L) %% rows) + 1L
  }
  if (wrap %in% c("horizontal", "both")) {
    cells[, 2L] <- ((cells[, 2L] - 1L) %% cols) + 1L
  }

  # Defend against degenerate wrap on a 1-wide/1-tall grid, which would
  # otherwise wrap a cell onto itself.
  self_loop <- cells[, 1L] == position[1L] & cells[, 2L] == position[2L]

  in_bounds <- cells[, 1L] >= 1L & cells[, 1L] <= rows &
    cells[, 2L] >= 1L & cells[, 2L] <= cols

  cells <- cells[in_bounds & !self_loop, , drop = FALSE]

  if (!is.null(mask) && nrow(cells) > 0L) {
    cells <- cells[mask[cbind(cells[, 1L], cells[, 2L])], , drop = FALSE]
  }

  cells
}

.cell_key <- function(position) {
  paste(position[1L], position[2L], sep = ",")
}

.edge_key <- function(from, to) {
  paste(sort(c(.cell_key(from), .cell_key(to))), collapse = "|")
}

.path_edges <- function(path) {
  if (length(path) < 2L) {
    return(character())
  }
  unique(vapply(
    seq_len(length(path) - 1L),
    function(i) .edge_key(path[[i]], path[[i + 1L]]),
    character(1L)
  ))
}

.is_maze <- function(x) {
  inherits(x, "maze") &&
    is.numeric(x$rows) && is.numeric(x$cols) &&
    (is.null(x$traversal) || is.list(x$traversal)) && is.character(x$passages)
}

.whole_number <- function(x, name) {
  if (length(x) != 1L || !is.numeric(x) || is.na(x) || !is.finite(x) || x != floor(x)) {
    stop(sprintf("`%s` must be one whole number.", name), call. = FALSE)
  }
  as.integer(x)
}

.positive_integer <- function(x, name) {
  value <- .whole_number(x, name)
  if (value < 1L) {
    stop(sprintf("`%s` must be greater than zero.", name), call. = FALSE)
  }
  value
}

.probability <- function(x, name) {
  if (length(x) != 1L || !is.numeric(x) || is.na(x) || !is.finite(x) ||
      x < 0 || x > 1) {
    stop(sprintf("`%s` must be a single number between 0 and 1.", name), call. = FALSE)
  }
  as.numeric(x)
}

.wall_candidates <- function(rows, cols, mask = NULL, wrap = "none") {
  right <- matrix(integer(0), ncol = 4L)
  if (cols > 1L) {
    r <- rep(seq_len(rows), times = cols - 1L)
    c1 <- rep(seq_len(cols - 1L), each = rows)
    right <- cbind(r, c1, r, c1 + 1L)
  }

  down <- matrix(integer(0), ncol = 4L)
  if (rows > 1L) {
    r <- rep(seq_len(rows - 1L), times = cols)
    c1 <- rep(seq_len(cols), each = rows - 1L)
    down <- cbind(r, c1, r + 1L, c1)
  }

  wrap_h <- matrix(integer(0), ncol = 4L)
  if (wrap %in% c("horizontal", "both") && cols > 1L) {
    r <- seq_len(rows)
    wrap_h <- cbind(r, cols, r, 1L)
  }

  wrap_v <- matrix(integer(0), ncol = 4L)
  if (wrap %in% c("vertical", "both") && rows > 1L) {
    c1 <- seq_len(cols)
    wrap_v <- cbind(rows, c1, 1L, c1)
  }

  out <- rbind(right, down, wrap_h, wrap_v)
  colnames(out) <- c("from_row", "from_col", "to_row", "to_col")

  if (nrow(out) > 0L) {
    keys <- .edge_key_vec(out[, 1L], out[, 2L], out[, 3L], out[, 4L])
    out <- out[!duplicated(keys), , drop = FALSE]

    if (!is.null(mask) && nrow(out) > 0L) {
      keep <- mask[cbind(out[, 1L], out[, 2L])] & mask[cbind(out[, 3L], out[, 4L])]
      out <- out[keep, , drop = FALSE]
    }
  }

  out
}

.wall_candidate_keys <- function(rows, cols, mask = NULL, wrap = "none") {
  walls <- .wall_candidates(rows, cols, mask, wrap)
  if (nrow(walls) == 0L) {
    return(character())
  }
  .edge_key_vec(walls[, 1L], walls[, 2L], walls[, 3L], walls[, 4L])
}

.edge_key_vec <- function(from_r, from_c, to_r, to_c) {
  key1 <- paste(from_r, from_c, sep = ",")
  key2 <- paste(to_r, to_c, sep = ",")
  lo <- pmin(key1, key2)
  hi <- pmax(key1, key2)
  paste(lo, hi, sep = "|")
}

.validate_mask <- function(mask, rows, cols) {
  if (!is.matrix(mask)) {
    stop("`mask` must be a matrix.", call. = FALSE)
  }
  if (nrow(mask) != rows || ncol(mask) != cols) {
    stop("`mask` dimensions must match `rows` and `cols`.", call. = FALSE)
  }

  mask <- matrix(as.logical(mask), nrow = rows, ncol = cols)
  if (anyNA(mask)) {
    stop("`mask` must not contain `NA` values.", call. = FALSE)
  }
  if (sum(mask) < 2L) {
    stop("`mask` must have at least 2 `TRUE` cells.", call. = FALSE)
  }
  if (!.mask_is_connected(mask)) {
    stop(
      "`mask` must be 4-connected: every `TRUE` cell must be reachable from every other `TRUE` cell.",
      call. = FALSE
    )
  }

  mask
}

.mask_is_connected <- function(mask) {
  true_cells <- which(mask, arr.ind = TRUE)
  total <- nrow(true_cells)
  if (total <= 1L) {
    return(TRUE)
  }

  rows <- nrow(mask)
  cols <- ncol(mask)
  origin <- as.integer(true_cells[1L, ])

  visited <- matrix(FALSE, nrow = rows, ncol = cols)
  visited[origin[1L], origin[2L]] <- TRUE
  queue <- list(origin)
  head_idx <- 1L
  count <- 1L

  while (head_idx <= length(queue)) {
    current <- queue[[head_idx]]
    head_idx <- head_idx + 1L

    neighbors <- .cell_neighbors(current, rows, cols, mask)
    if (nrow(neighbors) > 0L) {
      for (i in seq_len(nrow(neighbors))) {
        candidate <- as.integer(neighbors[i, ])
        if (!visited[candidate[1L], candidate[2L]]) {
          visited[candidate[1L], candidate[2L]] <- TRUE
          count <- count + 1L
          queue[[length(queue) + 1L]] <- candidate
        }
      }
    }
  }

  count == total
}

.validate_rooms <- function(rooms, rows, cols, mask) {
  if (!is.list(rooms)) {
    stop("`rooms` must be a list of `c(row0, col0, row1, col1)` vectors.", call. = FALSE)
  }

  validated <- vector("list", length(rooms))
  for (i in seq_along(rooms)) {
    room <- rooms[[i]]
    if (length(room) != 4L || !is.numeric(room) || anyNA(room) ||
        any(!is.finite(room)) || any(room != floor(room))) {
      stop(
        sprintf("`rooms[[%d]]` must be `c(row0, col0, row1, col1)` using whole numbers.", i),
        call. = FALSE
      )
    }
    room <- as.integer(room)
    row0 <- room[1L]; col0 <- room[2L]; row1 <- room[3L]; col1 <- room[4L]

    if (row0 > row1 || col0 > col1) {
      stop(
        sprintf("`rooms[[%d]]` must have `row0 <= row1` and `col0 <= col1`.", i),
        call. = FALSE
      )
    }
    if (row0 < 1L || row1 > rows || col0 < 1L || col1 > cols) {
      stop(sprintf("`rooms[[%d]]` must lie inside the maze boundaries.", i), call. = FALSE)
    }
    if (!is.null(mask) && !all(mask[row0:row1, col0:col1])) {
      stop(sprintf("`rooms[[%d]]` must lie entirely inside `mask`.", i), call. = FALSE)
    }

    validated[[i]] <- c(row0, col0, row1, col1)
  }

  validated
}

.room_open_edges <- function(rooms) {
  if (length(rooms) == 0L) {
    return(character())
  }

  keys <- character()
  for (room in rooms) {
    row0 <- room[1L]; col0 <- room[2L]; row1 <- room[3L]; col1 <- room[4L]

    if (col1 > col0) {
      for (r in row0:row1) {
        for (c in col0:(col1 - 1L)) {
          keys <- c(keys, .edge_key(c(r, c), c(r, c + 1L)))
        }
      }
    }
    if (row1 > row0) {
      for (c in col0:col1) {
        for (r in row0:(row1 - 1L)) {
          keys <- c(keys, .edge_key(c(r, c), c(r + 1L, c)))
        }
      }
    }
  }

  unique(keys)
}

.validate_position <- function(x, rows, cols, name) {
  if (length(x) != 2L || !is.numeric(x) || anyNA(x) ||
      any(!is.finite(x)) || any(x != floor(x))) {
    stop(sprintf("`%s` must be `c(row, column)` using whole numbers.", name), call. = FALSE)
  }
  x <- as.integer(x)
  if (x[1L] < 1L || x[1L] > rows || x[2L] < 1L || x[2L] > cols) {
    stop(sprintf("`%s` must be inside the maze boundaries.", name), call. = FALSE)
  }
  x
}
