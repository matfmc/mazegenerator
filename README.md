# Maze Generator

A sophisticated maze generator built in R that creates mazes using a random walk algorithm. The generator includes visualization capabilities for both static mazes and animated maze generation processes.

## Features

- Generate mazes of any size
- Visualize maze generation process through animation
- Customizable wall thickness and path appearance
- Option to display node numbers and path progression
- Export capabilities for both static and animated mazes

## Installation

```R
# Required packages
install.packages("magick")
```

## Usage
### Basic Maze Generation
```R
# Create a 20x20 maze
source('mazer2.r')
board <- matrix(0, 20, 20)
maze_path <- mazefy(1,1,board , verbose = TRUE)
plot_maze(maze_path, board)

```

## Create Animation
```R
# Create Animation
# Generate an animated visualization of the maze creation process
create_maze_animation(maze_path, board, "maze_animation.gif")
```
