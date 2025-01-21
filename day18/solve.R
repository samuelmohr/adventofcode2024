read_input <- function(filename) {
    data <- data.table::fread(filename, header = FALSE)
    colnames(data) <- c("row", "col")
    data[]
}

shortest_path <- function(data, fallen) {
    size <- 71

    start <- c(0, 0)
    end <- c(size - 1, size - 1)
    corrupted <- data[1:fallen, ]

    queue <- list()
    queue <- append(queue, list(list(start, start)))
    queue <- append(queue, list(NULL))
    time <- 0

    visited <- data.table::data.table(row = integer(), col = integer(), from_row = integer(), from_col = integer())

    while(length(queue) > 0) {
        current <- queue[[1]]
        queue <- queue[-1]

        if(is.null(current)) {
            if(length(queue) == 0) {
                break
            }
            queue <- append(queue, list(NULL))
            time <- time + 1
            next
        }

        if(identical(current[[1]], end)) {
            node <- current[[2]]
            path <- data.table::data.table(row = node[1], col = node[2])
            while(!identical(node, start)) {
                prev <- visited[row == node[1] & col == node[2], .(from_row, from_col)]
                node <- c(prev$from_row, prev$from_col)
                path <- rbind(path, data.table::data.table(row = node[1], col = node[2]))
            }
            return(list(path = path, time = time))
        }

        if(corrupted[corrupted$row == current[[1]][1] & corrupted$col == current[[1]][2], .N] > 0) {
            next
        }

        if(visited[row == current[[1]][1] & col == current[[1]][2], .N] > 0) {
            next
        }

        visited <- rbind(visited, data.table::data.table(row = current[[1]][1], col = current[[1]][2], from_row = current[[2]][1], from_col = current[[2]][2]))

        if(current[[1]][1] > 0) {
            queue <- append(queue, list(list(c(current[[1]][1] - 1, current[[1]][2]), current[[1]])))
        }
        if(current[[1]][1] < size - 1) {
            queue <- append(queue, list(list(c(current[[1]][1] + 1, current[[1]][2]), current[[1]])))
        }
        if(current[[1]][2] > 0) {
            queue <- append(queue, list(list(c(current[[1]][1], current[[1]][2] - 1), current[[1]])))
        }
        if(current[[1]][2] < size - 1) {
            queue <- append(queue, list(list(c(current[[1]][1], current[[1]][2] + 1), current[[1]])))
        }
    }
    return(list(path = NULL, time = -1))
}

solve1 <- function(data) {
    return(shortest_path(data, 1024)[["time"]])
}

solve2 <- function(data) {
    last <- 1024
    while(TRUE) {
        result <- shortest_path(data, last)
        if(result[["time"]] == -1) {
            print(paste0("Failed at byte at ", data[last, row], ",", data[last, col]))
            return(last)
        }
        while(TRUE) {
            last <- last + 1
            coords <- data[last, ]
            if(result[["path"]][row == coords$row & col == coords$col, .N] > 0) {
                break
            }
        }
    }
}

