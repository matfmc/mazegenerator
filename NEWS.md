# mazegenerator (development version)

- `solve_maze()` gained a `heuristic` argument for the A*-family algorithms
  (`"astar"`, `"greedy"`, and the new `"bidirectional_astar"`): `"auto"`
  (the default, and exactly what these algorithms always used before --
  manhattan for orthogonal movement, euclidean once `diagonal = TRUE`),
  `"manhattan"`, `"euclidean"`, `"chebyshev"`, `"octile"`, `"zero"` (which
  turns A* into Dijkstra), or a custom `function(position, target)`.
  `"manhattan"` now errors when combined with `diagonal = TRUE`, since it is
  inadmissible there (it charges 2 for a diagonal step that costs
  `sqrt(2)`) and can make A* return a non-shortest path; `"octile"` is the
  tight admissible replacement for that case. `heuristic` is ignored (with a
  warning if set explicitly) by every algorithm that does not use one.
- `solve_maze()` gained two more search algorithms: `"bidirectional_dijkstra"`
  (bidirectional Dijkstra) and `"bidirectional_astar"` (NBA*, New
  Bidirectional A*, Pijls & Post 2009). Both correctly track the best
  complete path found so far (`mu`) instead of stopping at the first cell
  the two frontiers have in common, which is required for correctness once
  step costs are non-uniform (a matrix with `diagonal = TRUE`) or the two
  heuristics are not mutually consistent.
- A traced search step now carries `side` (`"forward"`, `"backward"`, or
  `"none"`), recording which frontier of a bidirectional search it belongs
  to; every other algorithm reports `"none"`. A trace recorded before this
  change (or any step list missing the field) still reads back fine --
  `side` defaults to `"none"`.
- `animate_solution()` gained `forward_color` and `backward_color`: for a
  trace that carries `side` information, cells touched by the forward and
  backward searches of a bidirectional trace are now shaded separately
  instead of both being lumped into `explored_color` / `frontier_color`. A
  trace from a single-directional algorithm renders exactly as before.
- `generate_maze()` gained six more carving algorithms: `"division"`
  (recursive division), `"aldous_broder"` (Aldous-Broder), `"hunt_kill"`
  (hunt-and-kill), `"binary_tree"`, `"sidewinder"`, and `"growing_tree"`
  (with a new `selection` argument -- `"newest"`, `"random"`, `"oldest"`,
  or `"mixed"` -- controlling which active cell it carves from next).
  `traversal` and `visitation_order` are now also recorded for
  `"aldous_broder"` and `"hunt_kill"`, in addition to `"dfs"`.
- `generate_maze()` gained `mask`, `wrap`, and `rooms` for non-rectangular
  and non-planar topology: `mask` carves a maze through an arbitrary
  connected shape (a circle, a heart, a logo, text); `wrap` opens bonus
  passages across the grid's edges for toroidal/cylindrical mazes; `rooms`
  fully opens rectangular chambers before carving, for roguelike-style
  layouts. These are only supported by the neighbor-walking algorithms
  (`"dfs"`, `"prim"`, `"kruskal"`, `"wilson"`, `"aldous_broder"`,
  `"hunt_kill"`, and `"growing_tree"`).
- Added `braid()` and `sparsify()` to adjust a maze's loopiness after
  generation: `braid()` targets dead ends specifically and knocks out a
  wall to remove or shorten them, while `sparsify()` does the opposite,
  carving dead ends away until cells become isolated. Both complement
  `open_walls`, which removes walls at random with no regard for dead
  ends.
- Added `set_endpoints()` to re-label a maze's `start` and `end` cells
  after the fact, with `"longest"`, `"farthest"`, `"corners"`, and
  `"random"` strategies (or explicit positions), useful after `braid()` or
  `sparsify()` change the maze's connectivity.
- Added `maze_stats()`, which computes comparative structural statistics
  for a maze -- dead-end count, corridor length, degree distribution,
  diameter, solution coverage, and a corridor-straightness ("river")
  measure -- with `print` and `as.data.frame` methods for easy comparison
  across algorithms and settings.
- `solve_maze()` gained four more search algorithms: `"greedy"` (greedy
  best-first search), `"bidirectional"` (bidirectional breadth-first
  search), `"wall_follower"` (the classic hand-on-the-wall rule, with a
  new `hand` argument for `"left"` or `"right"`), and
  `"dead_end_filling"` (iteratively fills in dead ends before falling
  back to breadth-first search on what remains).
- `generate_maze()` gained `algorithm`, selecting between five carving
  algorithms: `"dfs"` (the previous behaviour and still the default),
  `"prim"`, `"kruskal"`, `"wilson"`, and `"eller"`. All five produce perfect
  mazes, with distinct textures: DFS gives long corridors, Prim and Kruskal
  give short bushy dead ends, Wilson draws uniformly from all possible mazes,
  and Eller works row by row in constant memory.
- Every maze now records `carve_order`, the passages in the order they were
  opened, and `algorithm`. `carve_order` is defined for all five algorithms
  and replaces the DFS-only `traversal` as the basis of `animate_maze()`,
  which therefore now animates every algorithm.
- `traversal` and `visitation_order` are `NULL` for every algorithm other than
  `"dfs"`, since no single contiguous walk exists for them. `plot_maze()`
  raises an informative error when `show_path` or `show_nodes` is requested
  for such a maze.
- The `end` cell is chosen as the cell furthest from `start` for the four new
  algorithms. `"dfs"` keeps its previous rule (the last cell visited), so
  existing seeds still reproduce exactly -- note that this frequently places
  `end` close to `start`, which makes DFS mazes a poor showcase for comparing
  search algorithms.
- `solve_maze()` gained `algorithm`, selecting between `"astar"` (the previous
  behaviour and still the default), `"dijkstra"` (the same search without the
  heuristic, expanding uniformly outward), and `"bfs"` (a true FIFO
  breadth-first search). All three return a shortest path; they differ in how
  much of the maze they expand to find it. `"bfs"` requires uniform step
  costs and so rejects `diagonal = TRUE` on a matrix.
- The `search_trace` attribute records the `algorithm` used, and
  `animate_solution()` names that algorithm in its frame titles instead of
  always labelling them `A*`.
- `generate_maze()` gained `open_walls`, a probability that each internal
  wall is independently removed after generation, braiding the maze with
  loops instead of leaving it perfect.
- `solve_maze()` gained `trace = TRUE`, which records every A-star expansion
  and the cells it pushed onto the frontier as a `search_trace` attribute.
- Added `animate_solution()`, which replays a traced search as an animated GIF:
  expanded cells, frontier, and the cell being expanded are shaded per frame,
  and the closing frames hold the solved path.
- `plot_maze()` gained `cell_fill`, to shade individual cells underneath the
  walls, and `main`, to override the plot title.

# mazegenerator 0.1.0

- Reorganized the original scripts as an installable R package.
- Added a small public API for generation, plotting, animation, and solving.
- Removed reliance on global variables and code that ran when files were loaded.
- Corrected the A-star neighborhood calculation and boundary handling.
- Added documentation, examples, tests, and automated package checks.
