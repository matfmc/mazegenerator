# Maze Generator

A  maze generator built in R that creates mazes using a random walk algorithm. The generator includes visualization capabilities for both static mazes and animated maze generation processes.

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
![Rplot01](https://github.com/user-attachments/assets/56d8cbe3-9e09-418e-ba10-23caff0e96c4)

## Create Animation
```R
# Create Animation
# Generate an animated visualization of the maze creation process
create_maze_animation(maze_path, board, "maze_animation.gif")
```


https://github.com/user-attachments/assets/375dd1d3-905c-432d-a5e3-383691f1ab4b

