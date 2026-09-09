\name{solve_maze}
\alias{solve_maze}
\title{Solve a maze or walkable matrix with graph search}
\usage{
solve_maze(x, start = NULL, end = NULL, walkable = 1,
  diagonal = FALSE, trace = FALSE,
  algorithm = c("astar", "dijkstra", "bfs", "greedy",
    "bidirectional", "bidirectional_dijkstra",
    "bidirectional_astar", "wall_follower", "dead_end_filling"),
  heuristic = "auto",
  hand = c("left", "right"))
}
\arguments{
\item{x}{A \code{maze} object or a matrix.}
\item{start, end}{Integer vectors \code{c(row, column)}. For a maze, they
default to its recorded start and end. They are required for a matrix.}
\item{walkable}{Values considered walkable when \code{x} is a matrix.}
\item{diagonal}{Allow diagonal movement for a matrix. Maze passages are always
orthogonal. Note that \code{algorithm} values that require uniform step
costs (\code{"bfs"} and \code{"bidirectional"}) reject \code{diagonal =
TRUE} on a matrix (diagonal moves cost \code{sqrt(2)}, not 1); use
\code{"dijkstra"}, \code{"astar"}, \code{"bidirectional_dijkstra"}, or
\code{"bidirectional_astar"} instead, all of which handle non-uniform step
costs correctly. \code{algorithm = "wall_follower"} also rejects
\code{diagonal = TRUE} because following a wall relies on the four
cardinal turn directions.}
\item{trace}{Record the state of the search so that it can be replayed. The
trace is attached to the result as the \code{search_trace} attribute and is
what \code{animate_solution()} renders. Each recorded step also carries
\code{side}, \code{"forward"} or \code{"backward"} for the three
bidirectional algorithms (which frontier that expansion belongs to) and
\code{"none"} for every other algorithm.}
\item{algorithm}{Search algorithm to use, one of \code{"astar"} (the
default), \code{"dijkstra"}, \code{"bfs"}, \code{"greedy"},
\code{"bidirectional"}, \code{"bidirectional_dijkstra"},
\code{"bidirectional_astar"}, \code{"wall_follower"}, or
\code{"dead_end_filling"}:
\itemize{
  \item \code{"astar"}: goal-directed A* search. Its heuristic biases
    expansion towards \code{end}, so it typically expands the fewest
    cells.
  \item \code{"dijkstra"}: uniform-cost search -- the special case A*
    reduces to when the heuristic is dropped -- expanding uniformly
    outward from \code{start} regardless of where \code{end} is.
  \item \code{"bfs"}: the unweighted special case of Dijkstra, a plain
    FIFO-queue breadth-first search, only valid when every step has the
    same cost (always true for a maze; true for a matrix unless
    \code{diagonal = TRUE}).
  \item \code{"greedy"}: Greedy Best-First search. Implemented by reusing
    the A* machinery with the path-cost-so-far (\code{g}) term forced to
    zero, so expansion is driven purely by the heuristic distance to
    \code{end}. It is usually fast but, unlike \code{"astar"}, is not
    guaranteed to find the shortest path.
  \item \code{"bidirectional"}: breadth-first search run simultaneously
    from \code{start} and from \code{end}, alternating one expansion per
    side, until the two frontiers meet. Requires uniform step costs, like
    \code{"bfs"}.
  \item \code{"bidirectional_dijkstra"}: bidirectional Dijkstra. Like
    \code{"bidirectional"} but weighted, so it also works with the
    non-uniform step costs \code{diagonal = TRUE} gives a matrix.
    Critically, it does not stop at the first cell the two frontiers have
    in common: with non-uniform costs the first meeting is not
    necessarily on the shortest path, so it keeps track of the best
    complete path found so far (\code{mu}) and only stops once neither
    frontier can possibly improve on it.
  \item \code{"bidirectional_astar"}: NBA* (New Bidirectional A*, Pijls &
    Post, 2009), a goal-directed version of
    \code{"bidirectional_dijkstra"}. The forward frontier's heuristic aims
    at \code{end} and the backward frontier's aims at \code{start} (see
    \code{heuristic}). Running plain A* from both ends and stopping at the
    first meeting point is \emph{not} a correct shortest-path algorithm,
    even with admissible heuristics, because a heuristic admissible for
    one direction is generally not consistent with the other; NBA* avoids
    this with per-side pruning rules tied to the shared best-known path
    length \code{mu}, without needing to evaluate the heuristic across
    both frontiers at once.
  \item \code{"wall_follower"}: the classic hand-on-the-wall
    maze-walking rule (see \code{hand}). It never looks at the whole
    maze, only at which of the cardinal directions around the current
    cell are open, and so it does not generally find the shortest path.
    On a maze with loops (see \code{open_walls} in
    \code{generate_maze()}) it can fail to reach \code{end} entirely; see
    \code{hand} below for what happens in that case.
  \item \code{"dead_end_filling"}: iteratively "fills in" every dead end
    (any non-start, non-end cell with at most one still-open connection)
    until nothing more can be removed. On a perfect maze this leaves
    exactly the solution corridor, which is then walked directly. On a
    braided maze (loops present) dead-end filling alone cannot resolve
    the remaining loop(s), so the surviving cells are searched with
    breadth-first search to recover the shortest path.
}}
\item{heuristic}{The heuristic A*-family algorithms (\code{"astar"},
\code{"greedy"}, \code{"bidirectional_astar"}) use to estimate the
remaining distance to the goal. Ignored (with a warning if set explicitly)
by every other algorithm -- \code{"dijkstra"} and
\code{"bidirectional_dijkstra"} always use a heuristic of zero, and
\code{"bfs"}, \code{"bidirectional"}, \code{"wall_follower"}, and
\code{"dead_end_filling"} use none at all. One of:
\itemize{
  \item \code{"auto"} (the default): \code{"manhattan"} for orthogonal
    movement (a maze, or a matrix with \code{diagonal = FALSE}),
    \code{"euclidean"} for a matrix with \code{diagonal = TRUE}. This is
    the heuristic every one of these algorithms has always used; passing
    \code{"auto"} (or omitting \code{heuristic}) reproduces that exact
    behaviour.
  \item \code{"manhattan"}: \code{abs(dx) + abs(dy)}. Admissible for
    orthogonal movement, but \strong{not} for \code{diagonal = TRUE}: it
    charges 2 for a diagonal step that actually costs \code{sqrt(2)}, so
    it can overestimate the true remaining distance and A* is no longer
    guaranteed to return the shortest path. \code{solve_maze()} errors if
    \code{"manhattan"} is requested together with \code{diagonal = TRUE}.
  \item \code{"euclidean"}: straight-line distance. Admissible for
    \code{diagonal = TRUE} (it never overestimates the true cost) but
    looser than \code{"octile"}.
  \item \code{"octile"}: \code{(dx + dy) + (sqrt(2) - 2) * min(dx, dy)},
    the tight admissible heuristic for a grid where orthogonal steps cost
    1 and diagonal steps cost \code{sqrt(2)} -- exactly this package's
    cost model for a matrix with \code{diagonal = TRUE}.
  \item \code{"chebyshev"}: \code{max(dx, dy)}. Admissible only when
    diagonal moves cost the same as orthogonal ones, which is \strong{not}
    this package's cost model (diagonal moves cost \code{sqrt(2)} here).
    Provided for experimentation; it will not generally yield shortest
    paths in this package and is not a safe default.
  \item \code{"zero"}: always 0, which turns A* into Dijkstra (and
    \code{"bidirectional_astar"} into \code{"bidirectional_dijkstra"}).
  \item a function \code{function(position, target)} returning a single
    finite, non-negative number, for a custom heuristic. \code{position}
    and \code{target} are each \code{c(row, column)}; \code{solve_maze()}
    errors if the function ever returns something else.
}}
\item{hand}{Which wall the \code{"wall_follower"} algorithm keeps a hand
on, one of \code{"left"} (the default) or \code{"right"}. Ignored by every
other algorithm. Following a wall only reaches \code{end} when \code{end}
actually lies along some wall reachable from \code{start}, which is always
true on a perfect maze but not guaranteed on a braided one. To avoid
looping forever on a maze with cycles, the walk is capped at
\code{4 * rows * cols} steps; if it has not reached \code{end} by then,
\code{solve_maze()} emits a warning and returns a zero-row result (as if
produced by \code{.no_cells()}, adapted to a data frame).}
}
\value{
A data frame with \code{row} and \code{column}, ordered from start to end
(or, only for \code{"wall_follower"} when it fails to reach \code{end}, a
data frame with zero rows -- see \code{hand}). When \code{trace = TRUE} it
carries a \code{search_trace} attribute: a list with the grid dimensions,
the \code{start} and \code{end} cells, the \code{algorithm} used, and one
entry per expansion in \code{steps}, each holding the expanded cell
(\code{current}), the cells that it added to the frontier (\code{opened}),
and \code{side} (\code{"forward"}, \code{"backward"}, or \code{"none"};
see \code{trace}). For \code{"wall_follower"}, each step is one move of the
walk: \code{current} is the cell just arrived at and \code{opened} holds
the single next cell the rule decides to move to (or no cells when the
walk is stuck or finished). For \code{"dead_end_filling"}, the trace
begins with one step per cell filled in (\code{current} is the filled
cell, \code{opened} is always empty), followed by the ordinary
breadth-first-search steps (in the same shape as \code{algorithm = "bfs"})
over the cells that survived filling.
}
\description{
For a \code{maze} object, the function follows its passages. A numeric or
logical matrix can also be supplied, in which case cells matching
\code{walkable} may be crossed.
}
\seealso{
\code{\link{animate_solution}} to render a traced search as a GIF.
}
\examples{
maze <- generate_maze(10, 10, seed = 42)
path <- solve_maze(maze)
plot_maze(maze, solution = path)

# Compare how many cells each algorithm expands on the same maze.
expansions <- vapply(c("astar", "dijkstra", "bfs"), function(algo) {
  solved <- solve_maze(maze, algorithm = algo, trace = TRUE)
  length(attr(solved, "search_trace")$steps)
}, integer(1))
expansions
}
