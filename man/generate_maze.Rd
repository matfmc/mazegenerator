\name{generate_maze}
\alias{generate_maze}
\alias{print.maze}
\title{Generate a random maze}
\usage{
generate_maze(rows = 20L, cols = rows, start = c(1L, 1L),
  seed = NULL, verbose = FALSE, open_walls = 0,
  algorithm = c("dfs", "prim", "kruskal", "wilson", "eller",
    "division", "aldous_broder", "hunt_kill", "binary_tree",
    "sidewinder", "growing_tree"),
  selection = c("newest", "random", "oldest", "mixed"),
  mask = NULL, wrap = c("none", "horizontal", "vertical", "both"),
  rooms = NULL)

\method{print}{maze}(x, ...)
}
\arguments{
\item{rows}{Number of rows in the maze.}
\item{cols}{Number of columns in the maze.}
\item{start}{Integer vector \code{c(row, column)} giving the starting cell.}
\item{seed}{Optional integer seed for reproducible generation.}
\item{verbose}{Show generation progress in the console.}
\item{open_walls}{Probability, in \code{[0, 1]}, that any given internal wall
between two adjacent cells is removed after the maze is generated. At the
default of \code{0}, the maze is unchanged (a perfect maze: every cell is
reachable and there is exactly one simple path between any two cells).
Values above \code{0} "braid" the maze by knocking out extra walls, creating
loops so that some cells have more than one route between them.}
\item{algorithm}{The carving algorithm to use, one of \code{"dfs"},
\code{"prim"}, \code{"kruskal"}, \code{"wilson"}, \code{"eller"},
\code{"division"}, \code{"aldous_broder"}, \code{"hunt_kill"},
\code{"binary_tree"}, \code{"sidewinder"} or \code{"growing_tree"}:
\itemize{
  \item \code{"dfs"} (the default): randomized depth-first search with
    backtracking. Produces long, winding corridors with few short dead
    ends.
  \item \code{"prim"}: randomized Prim's algorithm, growing a single tree
    from \code{start} by repeatedly carving a random frontier wall.
    Produces short, bushy dead ends and a more uniformly branching
    texture than \code{"dfs"}.
  \item \code{"kruskal"}: randomized Kruskal's algorithm, carving walls in
    random order via a union-find of connected components. Produces a
    texture with many short, evenly scattered dead ends and no bias
    toward \code{start}.
  \item \code{"wilson"}: loop-erased random walks generating a uniform
    spanning tree, so every perfect maze on the grid is equally likely.
    Slower to converge for large grids but free of any algorithmic bias.
  \item \code{"eller"}: a row-by-row, constant-memory algorithm. Produces
    a texture similar to \code{"kruskal"} while only ever holding one row
    of state.
  \item \code{"division"}: recursive division, the only subtractive
    algorithm here -- it starts from a fully open room and recursively
    splits it with a wall (leaving a single gap) until every chamber is
    one cell. Produces long straight walls and a distinctly architectural,
    room-like texture.
  \item \code{"aldous_broder"}: a naive random walk that carves into any
    unvisited cell it stumbles onto, forming a uniform spanning tree
    (like \code{"wilson"}, unbiased) but typically slower to finish since
    it keeps wandering over already-carved territory.
  \item \code{"hunt_kill"}: random walks that carve until they get stuck,
    then "hunt" for the first unvisited cell adjacent to the carved area
    to start the next walk. Produces winding corridors similar to
    \code{"dfs"} but with fewer very long dead ends.
  \item \code{"binary_tree"}: for every cell, randomly carves toward its
    north or west neighbor only. Extremely fast and simple, but strongly
    biased: the top row and leftmost column are always open corridors,
    and every other cell has a diagonal drift toward the top-left corner.
  \item \code{"sidewinder"}: like \code{"binary_tree"} but row-by-row --
    carves east along a run of cells, then closes the run out with a
    single north carve from a random member of it. The top row is always
    one continuous corridor; every other row still has a (milder)
    northward bias.
  \item \code{"growing_tree"}: keeps a pool of "active" cells and
    repeatedly carves from one of them (chosen using \code{selection}) to
    a random unvisited neighbor. A general-purpose algorithm whose
    texture depends entirely on \code{selection}.
}

\code{mask}, \code{wrap} and \code{rooms} (below) are only supported for
\code{"dfs"}, \code{"prim"}, \code{"kruskal"}, \code{"wilson"},
\code{"aldous_broder"}, \code{"hunt_kill"} and \code{"growing_tree"} --
every algorithm that carves by walking the passage graph one neighbor at a
time. \code{"eller"}, \code{"binary_tree"}, \code{"sidewinder"}
(row-by-row algorithms) and \code{"division"} (which recursively splits a
rectangular room) all assume a complete rectangular grid; combining any of
them with a non-default \code{mask}, \code{wrap} or \code{rooms} is an
error.}
\item{selection}{Only used when \code{algorithm = "growing_tree"}; ignored
(with a warning if explicitly set) otherwise. Controls which active cell
the growing tree carves from next, one of \code{"newest"}, \code{"random"},
\code{"oldest"} or \code{"mixed"}:
\itemize{
  \item \code{"newest"}: always the most recently added cell. This
    reproduces \code{"dfs"}.
  \item \code{"random"}: a uniformly random active cell. This
    approximates \code{"prim"}.
  \item \code{"oldest"}: always the least recently added cell, carving
    breadth-first. Produces long, straight corridors radiating from
    \code{start}.
  \item \code{"mixed"}: flips a coin each step between \code{"newest"}
    and \code{"random"}, blending the two textures.
}}
\item{mask}{Optional logical matrix of dimension \code{c(rows, cols)} (or
coercible to logical) marking which cells are part of the maze. \code{TRUE}
cells are carvable; \code{FALSE} cells are excluded entirely (solid rock),
which lets the maze take a non-rectangular shape -- a circle, a heart, a
logo, text. If \code{mask} is supplied and \code{rows}/\code{cols} are left
at their defaults, they are inferred from \code{dim(mask)}; if both are
given explicitly they must agree with \code{dim(mask)}. The mask must have
at least two \code{TRUE} cells, and every \code{TRUE} cell must be
reachable from every other \code{TRUE} cell through 4-connected \code{TRUE}
neighbors (a maze on a disconnected mask is meaningless and errors).
\code{start} must land on a \code{TRUE} cell; if \code{start} is left at
its default and it happens to fall on a \code{FALSE} cell, it is silently
snapped to the first \code{TRUE} cell instead of erroring. Only supported
for the neighbor-walking algorithms listed under \code{algorithm}. The mask
is stored on the returned \code{maze} object.}
\item{wrap}{Toroidal/cylindrical wrapping of the grid's edges, one of
\code{"none"} (the default), \code{"horizontal"} (column \code{cols}
becomes adjacent to column 1, in every row), \code{"vertical"} (row
\code{rows} becomes adjacent to row 1, in every column) or \code{"both"}.
The maze is still carved as an ordinary (mask-aware) perfect maze on the
flat rectangle first, so it stays fully connected and solvable by plain
grid-adjacency tools like \code{solve_maze()}; \code{wrap} then randomly
opens some bonus passages across the seam(s) on top of that, the same way
\code{open_walls} adds bonus loops elsewhere. The resulting maze is no
longer planar in the usual sense. \code{plot_maze()} always draws the maze
as a flat rectangle with a solid outer border, so a seam passage is never
drawn as an opening or a corridor in the picture -- it is simply invisible
there, even though it is a real, walkable passage in \code{maze$passages}.
This is expected; inspect \code{maze$passages} (edge keys of the form
\code{"r1,c1|r2,c2"}) to see the wrap connections directly. Only supported
for the neighbor-walking algorithms listed under \code{algorithm}.}
\item{rooms}{Optional list of integer vectors
\code{c(row0, col0, row1, col1)}, each describing a rectangle (inclusive,
1-indexed) that is fully opened -- every internal wall inside it removed --
before carving begins, so the carving algorithm treats the room as one
already-connected region (roguelike-style open chambers linked by carved
corridors). Each rectangle must lie inside the maze boundaries and, if
\code{mask} is given, entirely inside the \code{TRUE} region of
\code{mask}. Because a room's interior is fully open, a maze with
\code{rooms} is generally no longer a perfect maze (it can contain loops).
Only supported for the neighbor-walking algorithms listed under
\code{algorithm}.}
\item{x}{A \code{maze} object.}
\item{...}{Additional arguments, currently unused.}
}
\value{
An object of class \code{maze} containing its dimensions, start and end
cells, \code{algorithm} (the algorithm used), \code{carve_order} (a
character vector of edge keys in the order the walls were carved --
\code{unique()} of which equals the perfect-maze passage set), and
\code{passages} (the full set of open walls, including any added by
\code{open_walls}). For \code{algorithm = "dfs"}, \code{"aldous_broder"}
and \code{"hunt_kill"}, \code{traversal} and \code{visitation_order}
additionally record the generation walk as a list of \code{c(row, column)}
positions; for every other algorithm they are \code{NULL}. It also
contains \code{open_walls}, the probability used, and
\code{walls_removed}, the number of internal walls that were actually
opened. When \code{open_walls > 0} the maze is no longer perfect: loops
exist, and there can be more than one route between two cells. The print
method returns the object invisibly.
}
\description{
Carves a perfect rectangular maze -- every cell is reachable and there is
exactly one simple path between any two cells -- using one of several
classic generation algorithms.
}
\examples{
maze <- generate_maze(10, 10, seed = 42)
maze

prim_maze <- generate_maze(10, 10, seed = 42, algorithm = "prim")

division_maze <- generate_maze(10, 10, seed = 42, algorithm = "division")
growing_tree_maze <- generate_maze(10, 10, seed = 42,
  algorithm = "growing_tree", selection = "mixed")

# A circular maze via a mask built from Euclidean distance to the center.
n <- 21
center <- (n + 1) / 2
dist_from_center <- outer(seq_len(n), seq_len(n), function(r, c) {
  sqrt((r - center)^2 + (c - center)^2)
})
circle_mask <- dist_from_center <= n / 2
circular_maze <- generate_maze(mask = circle_mask, algorithm = "prim", seed = 1)

# A wrapped (cylindrical) maze and a maze with two open rooms.
wrapped_maze <- generate_maze(10, 10, seed = 1, wrap = "horizontal")
room_maze <- generate_maze(20, 20, seed = 1,
  rooms = list(c(2, 2, 5, 5), c(12, 12, 16, 16)))
}
