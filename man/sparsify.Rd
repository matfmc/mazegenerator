\name{sparsify}
\alias{sparsify}
\title{Sparsify a maze by carving out dead ends}
\usage{
sparsify(maze, p = 0.5)
}
\arguments{
\item{maze}{A \code{maze} object, as returned by \code{generate_maze()}.}
\item{p}{Probability, in \code{[0, 1]}, that any given dead end is erased
in a pass. Higher values cull more of the maze; \code{0} is a no-op.}
}
\value{
A modified \code{maze} object of class \code{maze}, with \code{passages}
and \code{walls_removed} updated. \code{carve_order} and all other fields
are unchanged (see Details).
}
\description{
The opposite of \code{braid()}: repeatedly erases dead-end cells by
removing their single connecting passage, turning them into isolated
cells. Erasing a dead end can turn its former neighbor into a new dead
end, so the process repeats in passes until either no dead ends remain, a
pass erases nothing, or a safety cap on the number of passes is reached.
}
\details{
The resulting maze is generally \strong{not} a perfect maze anymore: some
cells become fully isolated (unreachable from every other cell), so
\code{solve_maze()} may fail to find a path between an isolated cell and
anything else. Choosing \code{start}/\code{end} cells that remain
connected (for example with \code{set_endpoints()}, or by checking
\code{solve_maze()} succeeds) is the caller's responsibility.

\code{carve_order} is left untouched: it still records how the
\emph{original} maze was carved, but since \code{sparsify()} removes some
of those passages, \code{carve_order} (or \code{unique()} of it) no longer
reproduces the final set of open walls, and replaying it (e.g. via
\code{animate_maze()}) would show walls that \code{sparsify()} has since
closed back up. \code{walls_removed} is adjusted down when an erased
passage happened to be one of the "extra" loop walls counted in it, so it
keeps tracking the number of walls open beyond a spanning tree of the
maze.
}
\examples{
maze <- generate_maze(10, 10, seed = 42)
sparse <- sparsify(maze, p = 0.5)
sparse$passages
}
\seealso{
\code{\link{braid}} for the opposite operation.
}
