#!/usr/bin/env Rscript
# Renders the mazes2/ batch: 11 generation algorithms (plus 4 growing_tree
# selection variants = 14 mazes total), each solved with all 9 solving
# algorithms (wall_follower rendered for both hands => 10 solve GIFs per
# maze), at 25x25, seed 42, open_walls = 0 (perfect mazes), with start and
# end pinned to diagonally opposite corners -- (1,1) and (rows, cols) -- via
# set_endpoints(strategy = "corners").
#
# The script is resumable: any maze.png / maze.rds / generation.gif /
# solve_*.gif that already exists on disk is left untouched and skipped.
# index.csv and README.md are always rewritten from a fresh pass (cheap:
# only rendering GIFs/PNGs is expensive, and those are skipped when present).
#
# Usage:
#   "/c/Program Files/R/R-4.6.0/bin/Rscript" scripts/generate_mazes2.R

pkgload::load_all(".", quiet = TRUE)

# ---- Sharding --------------------------------------------------------------
# `--shard=i/n` renders only every n-th maze starting at i, so n instances can
# run concurrently over disjoint maze directories. A shard never writes
# index.csv / README.md (it only sees part of the batch); run the script once
# with no --shard afterwards to emit those -- that pass re-solves each maze
# (cheap) and skips every already-rendered GIF.
.args <- commandArgs(trailingOnly = TRUE)
.shard_arg <- grep("^--shard=", .args, value = TRUE)
SHARD <- if (length(.shard_arg) > 0L) sub("^--shard=", "", .shard_arg[1L]) else NULL
WRITE_INDEX <- is.null(SHARD)

out_root <- "mazes2"
if (!dir.exists(out_root)) dir.create(out_root)

ROWS <- 25L
COLS <- 25L
SEED <- 42L
START <- c(1L, 1L)
WIDTH <- 800L
HEIGHT <- 800L
TARGET_FRAMES <- 115  # aim each GIF's rendered-frame count near this

tune_every <- function(n_steps, target = TARGET_FRAMES) {
  if (is.null(n_steps) || n_steps <= 0) return(1L)
  max(1L, round(n_steps / target))
}

frame_step_count <- function(total_steps, every) {
  if (total_steps == 0L) return(0L)
  length(unique(c(seq.int(1L, total_steps, by = every), total_steps)))
}

# ---- Batch definition ------------------------------------------------------

gen_algorithms <- c(
  "dfs", "prim", "kruskal", "wilson", "eller", "division",
  "aldous_broder", "hunt_kill", "binary_tree", "sidewinder"
)

mazes_to_build <- lapply(gen_algorithms, function(a) list(dir = a, algorithm = a, selection = NA_character_))
growing_tree_selections <- c("newest", "random", "oldest", "mixed")
mazes_to_build <- c(
  mazes_to_build,
  lapply(growing_tree_selections, function(sel) {
    list(dir = paste0("growing_tree_", sel), algorithm = "growing_tree", selection = sel)
  })
)

if (!is.null(SHARD)) {
  .parts <- as.integer(strsplit(SHARD, "/", fixed = TRUE)[[1L]])
  if (length(.parts) != 2L || any(is.na(.parts)) || .parts[1L] < 1L ||
      .parts[2L] < 1L || .parts[1L] > .parts[2L]) {
    stop("`--shard=` must look like `--shard=2/6` (1 <= i <= n).", call. = FALSE)
  }
  .keep <- which((seq_along(mazes_to_build) - 1L) %% .parts[2L] == (.parts[1L] - 1L))
  mazes_to_build <- mazes_to_build[.keep]
  cat(sprintf("Shard %d/%d: %d maze(s) -- %s\n", .parts[1L], .parts[2L],
              length(mazes_to_build),
              paste(vapply(mazes_to_build, function(s) s$dir, character(1)), collapse = ", ")))
}

solvers <- list(
  list(name = "astar",              algorithm = "astar",             hand = NULL),
  list(name = "dijkstra",           algorithm = "dijkstra",          hand = NULL),
  list(name = "bfs",                algorithm = "bfs",               hand = NULL),
  list(name = "greedy",             algorithm = "greedy",            hand = NULL),
  list(name = "bidirectional",      algorithm = "bidirectional",     hand = NULL),
  list(name = "bidirectional_dijkstra", algorithm = "bidirectional_dijkstra", hand = NULL),
  list(name = "bidirectional_astar",    algorithm = "bidirectional_astar",    hand = NULL),
  list(name = "wall_follower_left", algorithm = "wall_follower",     hand = "left"),
  list(name = "wall_follower_right",algorithm = "wall_follower",     hand = "right"),
  list(name = "dead_end_filling",   algorithm = "dead_end_filling",  hand = NULL)
)

optimal_solvers <- c("astar", "dijkstra", "bfs", "bidirectional",
                      "bidirectional_dijkstra", "bidirectional_astar",
                      "dead_end_filling")
nonoptimal_solvers <- c("greedy", "wall_follower_left", "wall_follower_right")

index_rows <- list()
failures <- list()

cat(sprintf("=== mazegenerator batch: %d mazes x %d solvers ===\n", length(mazes_to_build), length(solvers)))

for (spec in mazes_to_build) {
  dir_name <- spec$dir
  out_dir <- file.path(out_root, dir_name)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  cat(sprintf("\n--- %s (algorithm=%s%s) ---\n", dir_name, spec$algorithm,
              if (!is.na(spec$selection)) sprintf(", selection=%s", spec$selection) else ""))

  maze_rds_path <- file.path(out_dir, "maze.rds")
  if (file.exists(maze_rds_path)) {
    maze <- readRDS(maze_rds_path)
    cat("  maze.rds exists, loaded from disk\n")
  } else {
    gen_args <- list(
      rows = ROWS, cols = COLS, start = START, seed = SEED,
      open_walls = 0, algorithm = spec$algorithm
    )
    if (!is.na(spec$selection)) gen_args$selection <- spec$selection
    maze <- do.call(generate_maze, gen_args)
    # Endpoints pinned to diagonally opposite corners: (1,1) -> (rows, cols),
    # the two vertices furthest apart on the grid.
    maze <- set_endpoints(maze, strategy = "corners")
    saveRDS(maze, maze_rds_path)
    cat(sprintf("  generated maze, endpoints (%d,%d)->(%d,%d), saved maze.rds\n",
                maze$start[1L], maze$start[2L], maze$end[1L], maze$end[2L]))
  }

  # ---- static PNG -----------------------------------------------------
  png_path <- file.path(out_dir, "maze.png")
  if (file.exists(png_path)) {
    cat("  maze.png exists, skipping\n")
  } else {
    grDevices::png(png_path, width = WIDTH, height = HEIGHT, bg = "white")
    tryCatch(plot_maze(maze), finally = grDevices::dev.off())
    cat("  wrote maze.png\n")
  }

  # ---- generation GIF ---------------------------------------------------
  gen_gif_path <- file.path(out_dir, "generation.gif")
  total_carve_steps <- length(maze$carve_order)
  gen_every <- tune_every(total_carve_steps)
  gen_frames <- frame_step_count(total_carve_steps, gen_every)
  if (file.exists(gen_gif_path)) {
    cat("  generation.gif exists, skipping\n")
  } else {
    animate_maze(maze, output_file = gen_gif_path, every = gen_every,
                 width = WIDTH, height = HEIGHT, progress = FALSE)
    cat(sprintf("  wrote generation.gif (every=%d, frames=%d)\n", gen_every, gen_frames))
  }

  # ---- maze_stats() once per maze ---------------------------------------
  stats <- maze_stats(maze)

  # ---- solves -------------------------------------------------------------
  for (solver in solvers) {
    solve_gif_path <- file.path(out_dir, sprintf("solve_%s.gif", solver$name))

    solved <- NULL
    failure_reason <- NA_character_
    result <- tryCatch({
      args <- list(maze, algorithm = solver$algorithm, trace = TRUE)
      if (!is.null(solver$hand)) args$hand <- solver$hand
      withCallingHandlers(
        do.call(solve_maze, args),
        warning = function(w) {
          failure_reason <<- conditionMessage(w)
          invokeRestart("muffleWarning")
        }
      )
    }, error = function(e) {
      failure_reason <<- conditionMessage(e)
      NULL
    })

    if (!is.null(result) && nrow(result) > 0L) {
      solved <- result
    } else {
      if (is.na(failure_reason)) failure_reason <- "empty path"
      failures[[length(failures) + 1L]] <- list(
        maze = dir_name, solver = solver$name, reason = failure_reason
      )
      cat(sprintf("  [%s] FAILED: %s\n", solver$name, failure_reason))
    }

    path_length <- if (!is.null(solved)) nrow(solved) else NA_integer_
    trace <- if (!is.null(solved)) attr(solved, "search_trace") else NULL
    expansions <- if (!is.null(trace)) length(trace$steps) else NA_integer_
    every <- if (!is.null(trace)) tune_every(expansions) else NA_integer_
    frames_rendered <- if (!is.null(trace)) frame_step_count(expansions, every) else NA_integer_

    if (!is.null(solved)) {
      if (file.exists(solve_gif_path)) {
        cat(sprintf("  solve_%s.gif exists, skipping\n", solver$name))
      } else {
        animate_solution(maze, solution = solved, output_file = solve_gif_path,
                          every = every, width = WIDTH, height = HEIGHT, progress = FALSE,
                          forward_color = "#0ea5e9", backward_color = "#f43f5e")
        cat(sprintf("  wrote solve_%s.gif (every=%d, frames=%d, path_length=%d, expansions=%d)\n",
                     solver$name, every, frames_rendered, path_length, expansions))
      }
    }

    index_rows[[length(index_rows) + 1L]] <- data.frame(
      algorithm = spec$algorithm,
      selection = spec$selection,
      open_walls = maze$open_walls,
      passages = length(maze$passages),
      walls_removed = maze$walls_removed,
      path_length = path_length,
      search_algorithm = solver$name,
      expansions = expansions,
      frames_rendered = frames_rendered,
      every = every,
      dead_ends = stats$dead_ends,
      corridor_mean = stats$corridor_mean,
      corridor_max = stats$corridor_max,
      diameter = stats$diameter,
      river = stats$river,
      stringsAsFactors = FALSE
    )
  }
}

if (!WRITE_INDEX) {
  cat(sprintf("\n=== Shard %s done (index.csv / README.md skipped). ===\n", SHARD))
  if (length(failures) > 0L) {
    cat(sprintf("%d solver failure(s) in this shard.\n", length(failures)))
  }
  quit(save = "no", status = 0L)
}

index_df <- do.call(rbind, index_rows)
write.csv(index_df, file.path(out_root, "index.csv"), row.names = FALSE)
cat(sprintf("\nWrote %s (%d rows)\n", file.path(out_root, "index.csv"), nrow(index_df)))

# ---- README.md -------------------------------------------------------------

fmt <- function(x, digits = 2) {
  if (is.na(x)) return("NA")
  if (is.numeric(x) && x != round(x)) return(sprintf(paste0("%.", digits, "f"), x))
  as.character(x)
}

maze_dirs <- vapply(mazes_to_build, function(s) s$dir, character(1))

# Table 1: per-maze structural stats (one row per maze; distinct rows in index_df)
struct_rows <- index_df[!duplicated(index_df[c("algorithm", "selection")]), ]
struct_rows <- struct_rows[match(vapply(mazes_to_build, function(s) if (is.na(s$selection)) s$algorithm else paste0(s$algorithm, "_", s$selection), character(1)),
                                  paste0(struct_rows$algorithm, ifelse(is.na(struct_rows$selection), "", paste0("_", struct_rows$selection)))), ]

# path length (bfs, i.e. shortest/optimal) per maze for the structural table
bfs_rows <- index_df[index_df$search_algorithm == "bfs", ]

struct_table <- c(
  "| maze | dead_ends | corridor_mean | corridor_max | diameter | river | solution_length (bfs) |",
  "|---|---|---|---|---|---|---|"
)
for (i in seq_len(nrow(struct_rows))) {
  r <- struct_rows[i, ]
  key_algo <- r$algorithm
  key_sel <- r$selection
  bfs_match <- bfs_rows[bfs_rows$algorithm == key_algo &
                           ((is.na(key_sel) & is.na(bfs_rows$selection)) | (!is.na(key_sel) & bfs_rows$selection == key_sel)), ]
  sol_len <- if (nrow(bfs_match) > 0) bfs_match$path_length[1] else NA
  maze_label <- if (is.na(key_sel)) key_algo else sprintf("%s (%s)", key_algo, key_sel)
  struct_table <- c(struct_table, sprintf(
    "| %s | %d | %s | %s | %d | %s | %s |",
    maze_label, r$dead_ends, fmt(r$corridor_mean), fmt(r$corridor_max), r$diameter, fmt(r$river, 3), fmt(sol_len)
  ))
}

# Table 2: solver performance -- rows = maze, columns = expansions per solver (path_length in parens)
solver_names <- vapply(solvers, function(s) s$name, character(1))
perf_header <- c(
  paste0("| maze | ", paste(solver_names, collapse = " | "), " |"),
  paste0("|---|", paste(rep("---", length(solver_names)), collapse = "|"), "|")
)
perf_table <- perf_header
for (spec in mazes_to_build) {
  key_algo <- spec$algorithm
  key_sel <- spec$selection
  maze_label <- if (is.na(key_sel)) key_algo else sprintf("%s (%s)", key_algo, key_sel)
  cells <- vapply(solver_names, function(sn) {
    row <- index_df[index_df$algorithm == key_algo &
                       ((is.na(key_sel) & is.na(index_df$selection)) | (!is.na(key_sel) & index_df$selection == key_sel)) &
                       index_df$search_algorithm == sn, ]
    if (nrow(row) == 0 || is.na(row$path_length[1])) {
      "FAILED"
    } else {
      sprintf("%d exp / %d len", row$expansions[1], row$path_length[1])
    }
  }, character(1))
  perf_table <- c(perf_table, paste0("| ", maze_label, " | ", paste(cells, collapse = " | "), " |"))
}

failure_lines <- if (length(failures) > 0) {
  vapply(failures, function(f) sprintf("- **%s** on maze `%s`: %s", f$solver, f$maze, f$reason), character(1))
} else {
  "- None. Every solver reached `end` on every maze in this batch."
}

readme <- c(
  "# Maze batch outputs (mazes2)",
  "",
  sprintf(
    "All mazes are %dx%d, generated with `seed = %d`, `open_walls = 0` (perfect mazes -- no loops). Start and end are pinned to diagonally opposite corners -- (1, 1) and (%d, %d) -- via `set_endpoints(strategy = \"corners\")`, so every solve runs corner to corner across the full diagonal of the grid. This batch compares the package's %d generation algorithms (with `growing_tree` rendered once per `selection` value: newest, random, oldest, mixed, for %d mazes total) against all 9 solving algorithms (`wall_follower` rendered for both `left` and `right` hands, for 10 solve GIFs per maze).",
    ROWS, COLS, SEED, ROWS, COLS, length(gen_algorithms), length(mazes_to_build)
  ),
  "",
  "Each maze directory contains `maze.png`, `maze.rds`, `generation.gif`, and one `solve_<solver>.gif` per solver (`solve_wall_follower_left.gif` / `solve_wall_follower_right.gif` for the two hands).",
  "",
  sprintf(
    "**Which solvers are optimal:** `%s` are guaranteed to return a shortest path on these perfect mazes. **Which are not:** `greedy` (Greedy Best-First; driven purely by the heuristic, can overshoot) and `wall_follower` (both hands; a purely local hand-on-the-wall rule with no guarantee of shortest path, and on a maze with loops it can fail to reach `end` at all -- though these are all perfect mazes, so every wall reachable from `start` does eventually lead to `end`).",
    paste(optimal_solvers, collapse = "`, `")
  ),
  "",
  "## Solver failures",
  "",
  failure_lines,
  "",
  "## Structural stats (from `maze_stats()`), one row per maze",
  "",
  struct_table,
  "",
  "## Solver performance -- expansions and path length, one row per maze",
  "",
  perf_table,
  ""
)
writeLines(readme, file.path(out_root, "README.md"))
cat(sprintf("Wrote %s\n", file.path(out_root, "README.md")))

cat("\n=== Done. ===\n")
if (length(failures) > 0) {
  cat(sprintf("%d solver failure(s) recorded (see README.md).\n", length(failures)))
}
