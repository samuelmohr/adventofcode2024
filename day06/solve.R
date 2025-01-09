library(data.table)

read_input <- function(filename) {
    lines <- data.table::fread(filename, header = FALSE, sep = "\n")
    data <- setDT(lines)[, tstrsplit(V1, split = "(?=.)", perl = TRUE)]
    data[]
}

directions <- c("N", "E", "S", "W")

update_direction <- function(current_direction) {
    idx <- match(current_direction, directions)
    idx <- (idx) %% length(directions)
    return(directions[idx + 1])
}

update_position <- function(position, direction) {
    if (direction == "N") {
        position[1] <- position[1] - 1
    } else if (direction == "E") {
        position[2] <- position[2] + 1
    } else if (direction == "S") {
        position[1] <- position[1] + 1
    } else if (direction == "W") {
        position[2] <- position[2] - 1
    }
    return(position)
}

state_machine <- function(map, initial_position, initial_direction) {
    position <- initial_position
    direction <- initial_direction
    print("Guard starts …")

    in_bounds <- function(position) {
        return(position[1] >= 1 && position[1] <= nrow(map) && position[2] >= 1 && position[2] <= ncol(map))
    }
    visited_positions <- data.table(x = numeric(), y = numeric(), direction = character())
    circled <- FALSE
    
    while (TRUE) {
        visited_positions <- rbind(visited_positions, data.table(x = position[1], y = position[2], direction = direction))
        next_position <- update_position(position, direction)
        if (!in_bounds(next_position)) {
            print("Guard moved out of bounds")
            break
        }
        if (map[next_position[1], next_position[2]] == "#") {
            direction <- update_direction(direction)
        } else {
            position <- next_position
        }
        if (any(visited_positions$x == position[1] & visited_positions$y == position[2] & visited_positions$direction == direction)) {
            print("Guard circled")
            circled <- TRUE
            break
        }
    }
    return(list(visited_positions, circled))
}

solve1 <- function(data) {
    map <- data |> as.matrix()
    initial_position <- which(map == "^", arr.ind = TRUE) |> as.vector()
    initial_direction <- "N"

    positions <- state_machine(map, initial_position, initial_direction)[[1]][, .(x, y)]
    return(nrow(unique(positions)))
}

solve2 <- function(data) {
    map <- data |> as.matrix()
    initial_position <- which(map == "^", arr.ind = TRUE) |> as.vector()
    initial_direction <- "N"

    positions <- unique(state_machine(map, initial_position, initial_direction)[[1]][, .(x, y)])
    print(sprintf("Checking alternative %d positions …", nrow(positions)))
    alternatives <- 0
    for (i in 1:nrow(positions)) {
        position <- positions[i, ] 
        if (identical(as.numeric(position), as.numeric(initial_position))) {
            next
        }
        map[position$x, position$y] <- "#"
        alt_positions <- state_machine(map, initial_position, initial_direction)[[2]]
        if (alt_positions) {
            alternatives <- alternatives + 1
        }
        map[position$x, position$y] <- "."
    }
    return(alternatives)
}
