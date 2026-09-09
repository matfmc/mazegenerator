\name{animate_solution}
\alias{animate_solution}
\title{Animate a maze search}
\usage{
animate_solution(maze, solution = NULL,
  output_file = "maze-solution.gif", fps = 10, every = 1L,
  width = 800L, height = 800L, wall_width = 3,
  explored_color = "#bae6fd", frontier_color = "#fdba74",
  current_color = "#8b5cf6", forward_color = "#0ea5e9",
  backward_color = "#f43f5e", solution_color = "#f59e0b", hold = 12L,
  progress = interactive())
}
\arguments{
\item{maze}{A \code{maze} object returned by \code{generate_maze()}.}
\item{solution}{Optional result of \code{solve_maze()} called with
\code{trace = TRUE}. When omitted the maze is solved again with tracing
enabled.}
\item{output_file}{Destination path for the GIF.}
\item{fps}{Frames per second.}
\item{every}{Render every nth search step. Increase this for large mazes.}
\item{width, height}{Frame dimensions in pixels.}
\item{wall_width}{Width of wall lines.}
\item{explored_color}{Fill for cells that have been expanded. Used for
every algorithm except that, for a trace carrying \code{side} information
(see Details), cells touched by either bidirectional frontier are shaded
with \code{forward_color} / \code{backward_color} instead.}
\item{frontier_color}{Fill for cells waiting in the open set. Same caveat
as \code{explored_color}.}
\item{current_color}{Fill for the cell being expanded.}
\item{forward_color}{Fill for cells (explored or still on the frontier)
belonging to the forward search -- the one growing from \code{start} -- of
a bidirectional trace. Ignored for a trace with no \code{side}
information.}
\item{backward_color}{Fill for cells belonging to the backward search --
the one growing from \code{end} -- of a bidirectional trace. Ignored for a
trace with no \code{side} information.}
\item{solution_color}{Color of the path drawn in the closing frames.}
\item{hold}{Number of closing frames showing the solved path.}
\item{progress}{Show a progress bar in interactive sessions.}
}
\value{The normalized output path, invisibly.}
\description{
Replays the search recorded by \code{solve_maze(trace = TRUE)} and writes it
to an animated GIF. Each frame shades the cells already expanded, the cells
waiting on the frontier, and the cell being expanded, so the characteristic
shape of each search becomes visible: A-star's bias towards the goal,
Dijkstra's uniform spread, and breadth-first search's expanding rings. The
frame titles name whichever algorithm produced the trace. The final frames
hold the solved path. The optional \code{magick} package is required only
when this function is called.

For a trace produced by one of the three bidirectional algorithms
(\code{"bidirectional"}, \code{"bidirectional_dijkstra"},
\code{"bidirectional_astar"}), each step also records which frontier it
belongs to (see the \code{side} field documented in \code{solve_maze()}),
and cells touched by the forward search (growing from \code{start}) and the
backward search (growing from \code{end}) are shaded with
\code{forward_color} and \code{backward_color} instead of the usual
\code{explored_color} / \code{frontier_color} pair. A trace from any other
algorithm carries no side information and renders exactly as before.
}
\seealso{
\code{\link{animate_maze}} for the generation animation.
}
\examples{
if (requireNamespace("magick", quietly = TRUE)) {
  maze <- generate_maze(8, 8, seed = 42)
  animate_solution(maze, output_file = tempfile(fileext = ".gif"))
}
}
