library(data.table)

read_input <- function(filename) {
    lines <- readLines(filename)
    split <- which(lines == "")[1]

    map <- setDT(data.table::fread(text = paste(lines[1:(split - 1)], collapse = "\n"), header = FALSE, sep = "\n"))[, tstrsplit(V1, split = "(?=.)", perl = TRUE)]
    sequence <- paste(lines[(split + 1):length(lines)], collapse = "")
    
    return(list(map, sequence))
}

state_machine <- function(boxes, borders, robot, sequence) {
    position <- robot
    n <- nchar(sequence)
    pb <- txtProgressBar(min = 0, max = n, style = 3)

    for (i in 1:nchar(sequence)) {
        direction <- c(0, 0)
        if (substr(sequence, i, i) == "^") {
            direction <- c(-1, 0)
        } else if (substr(sequence, i, i) == "v") {
            direction <- c(1, 0)
        } else if (substr(sequence, i, i) == "<") {
            direction <- c(0, -1)
        } else if (substr(sequence, i, i) == ">") {
            direction <- c(0, 1)
        }
        next_position <- position + direction

        check_position <- next_position
        while(TRUE) {
            if(boxes[row == check_position[1] & col == check_position[2], .N] > 0) {
                check_position <- check_position + direction
            } else if(borders[row == check_position[1] & col == check_position[2], .N] > 0) {
                break
            } else {
                if(any(next_position != check_position)) {
                    boxes[row == next_position[1] & col == next_position[2], c("row", "col") := list(check_position[1], check_position[2])]
                }
                position <- next_position
                break
            }
        }
        setTxtProgressBar(pb, i)
    }
    close(pb)
    return(boxes)
}


solve1 <- function(data) {
    boxes <- data.table(which(data[[1]] == "O", arr.ind = TRUE))
    borders <- data.table(which(data[[1]] == "#", arr.ind = TRUE))
    robot <- which(data[[1]] == "@", arr.ind = TRUE)

    moved_boxes <- state_machine(boxes, borders, robot, data[[2]])
    moved_boxes[, GPS := (row - 1) * 100 + col - 1]
    
    return(moved_boxes[, sum(GPS)])
}


state_machine_double <- function(boxes, borders, robot, sequence) {
    position <- robot
    boxes[, row := as.numeric(row)]
    boxes[, col := as.numeric(col)]
    boxes[, active := FALSE]
    n <- nchar(sequence)
    pb <- txtProgressBar(min = 0, max = n, style = 3)

    for (i in 1:nchar(sequence)) {
        direction <- c(0, 0)
        check_position <- position
        horizontal <- FALSE
        if (substr(sequence, i, i) == "^") {
            direction <- c(-1, 0)
        } else if (substr(sequence, i, i) == "v") {
            direction <- c(1, 0)
        } else if (substr(sequence, i, i) == "<") {
            direction <- c(0, -.5)
            check_position <- position + 2 * direction
            horizontal <- TRUE
        } else if (substr(sequence, i, i) == ">") {
            direction <- c(0, .5)
            check_position <- position + direction
            horizontal <- TRUE
        }
        next_position <- position + direction

        if(horizontal) {
            while(TRUE) {
                if(boxes[row == check_position[1] & col == check_position[2], .N] > 0) {
                    boxes[row == check_position[1] & col == check_position[2], active := TRUE]
                    check_position <- check_position + 2 * direction
                } else if(borders[row == check_position[1] & abs(col + .25 - check_position[2]) < .3, .N] > 0) {
                    boxes[, active := FALSE]
                    break
                } else {
                    position <- next_position
                    break
                }
            }
        } else {
            pushing = list(next_position)
            wall_hit = FALSE
            while(length(pushing) > 0) {
                check_position <- pushing[[1]]
                pushing <- pushing[-1]

                if(borders[row == check_position[1] & abs(col + .25 - check_position[2]) < .3, .N] > 0) {
                    boxes[, active := FALSE]
                    wall_hit <- TRUE
                    break
                }
                if(boxes[row == check_position[1] & col == check_position[2] - .5, .N] > 0) {
                    boxes[row == check_position[1] & col == check_position[2] - .5, active := TRUE]
                    pushing[[length(pushing) + 1]] <- check_position + c(0, -.5) + direction
                    pushing[[length(pushing) + 1]] <- check_position + direction
                }
                if(boxes[row == check_position[1] & col == check_position[2], .N] > 0) {
                    boxes[row == check_position[1] & col == check_position[2], active := TRUE]
                    pushing[[length(pushing) + 1]] <- check_position + direction
                    pushing[[length(pushing) + 1]] <- check_position + c(0, .5) + direction
                }
            }
            if(!wall_hit) {
                position <- next_position
            }
        }

        boxes[active == TRUE, c("row", "col", "active") := list(row + direction[1], col + direction[2], FALSE)]
        setTxtProgressBar(pb, i)
    }
    close(pb)
    return(boxes)
}

solve2 <- function(data) {
    boxes <- data.table(which(data[[1]] == "O", arr.ind = TRUE))
    borders <- data.table(which(data[[1]] == "#", arr.ind = TRUE))
    robot <- which(data[[1]] == "@", arr.ind = TRUE)

    moved_boxes <- state_machine_double(boxes, borders, robot, data[[2]])
    moved_boxes[, GPS := (row - 1) * 2 * 100 + col - 1]

    return(moved_boxes[, sum(GPS)])
}
