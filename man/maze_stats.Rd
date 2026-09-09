\name{maze_stats}
\alias{maze_stats}
\alias{print.maze_stats}
\alias{as.data.frame.maze_stats}
\title{Compute comparative statistics for a maze}
\usage{
maze_stats(maze)

\method{print}{maze_stats}(x, ...)

\method{as.data.frame}{maze_stats}(x, row.names = NULL, optional = FALSE, ...)
}
\arguments{
\item{maze}{A \code{maze} object returned by \code{generate_maze()}.}
\item{x}{A \code{maze_stats} object, as returned by \code{maze_stats()}.}
\item{...}{Additional arguments, currently unused.}
\item{row.names, optional}{Passed on for compatibility with
\code{as.data.frame()}; not otherwise used.}
}
\value{
An object of class \code{maze_stats} (a list, printed with its own
\code{print} method, and convertible to a one-row data frame with
\code{as.data.frame()}) containing:
\describe{
  \item{rows, cols}{The maze's dimensions.}
  \item{algorithm}{The generation algorithm recorded on \code{maze}.}
  \item{open_walls}{The \code{open_walls} probability recorded on
    \code{maze}.}
  \item{dead_ends, dead_end_pct}{The number of degree-1 cells, and that
    count as a fraction of all cells.}
  \item{corridor_mean, corridor_max}{The mean and maximum corridor
    length (in edges), across every corridor as defined above.
    \code{NA} for a 1x1 maze, which has no corridors.}
  \item{degree1, degree2, degree3, degree4}{The number of cells with
    exactly 1, 2, 3, or 4 open passages. These four counts sum to
    \code{rows * cols} (every generated maze keeps every cell connected,
    so degree 0 does not occur except in a degenerate 1x1 maze).}
  \item{diameter}{The length, in steps, of the longest shortest path
    between any two cells in the maze -- its graph diameter. Computed
    by a double-sweep BFS: farthest cell from an arbitrary corner, then
    farthest cell from that cell, then the shortest-path length between
    those two. This is exact for a perfect maze (a tree) but only an
    approximation, typically a slight underestimate, for a braided
    maze with loops.}
  \item{solution_length, solution_pct}{The number of cells on the
    shortest path from \code{maze$start} to \code{maze$end} (via
    \code{solve_maze(maze, algorithm = "bfs")}), and that count as a
    fraction of all cells.}
  \item{river}{A straightness measure in \code{[0, 1]}, \code{NA} if the
    maze has no corridor with at least two edges. Within a corridor, each
    step after the first either continues in the same direction as the
    step before it or turns; \code{river} is the fraction of those steps
    that continue straight, averaged over every corridor with at least
    two edges (each corridor contributes one fraction, regardless of its
    length). A \code{river} near 1 means the maze is dominated by long,
    straight corridors; a \code{river} near 0 means corridors turn at
    nearly every cell.}
}

The \code{print} method returns \code{x} invisibly; \code{as.data.frame()}
returns a one-row \code{data.frame} with one column per element above.
}
\description{
Summarizes the structural character of a maze: how many dead ends it has,
how long its corridors are, how branchy it is, how "wide" it is from
corner to corner, how much of the grid its shortest solution covers, and
how straight its corridors tend to run. Useful for comparing mazes
produced by different \code{generate_maze()} algorithms or
\code{open_walls} settings.
}
\details{
A \emph{corridor} here is a maximal run of cells between two "special"
cells (a dead end, a branch, or an intersection -- any cell whose number
of open passages, its degree, is not exactly 2), following the unique
route through the intervening degree-2 cells. In a perfect maze every
corridor has exactly two such endpoints. A braided maze (see
\code{open_walls} in \code{generate_maze()}) can additionally contain a
loop of degree-2 cells with no special cell on it at all; each such loop
is counted as one more corridor whose length is its own edge count.
}
\examples{
maze <- generate_maze(15, 15, seed = 42)
maze_stats(maze)

# Braiding a maze (adding loops) tends to shorten its corridors and
# increase its branching.
braided <- generate_maze(15, 15, seed = 42, open_walls = 0.3)
as.data.frame(maze_stats(braided))
}
