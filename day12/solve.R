library(data.table)

read_input <- function(filename) {
    lines <- data.table::fread(filename, header = FALSE, sep = "\n", colClasses = "character")
    data <- setDT(lines)[, tstrsplit(V1, split = "(?=.)", perl = TRUE, type.convert = TRUE)]
    data[]
}

process <- function(coordinates, borders, garden, type, xpos, ypos) {
    if (xpos < 1 || xpos > nrow(garden) || ypos < 1 || ypos > ncol(garden)) {
        return(-1)
    }

    if (garden[xpos, ypos] != type) {
        return(-1)
    }

    if (!coordinates[x == xpos & y == ypos, active]) {
        return(0)
    }

    coordinates[x == xpos & y == ypos, active := FALSE]

    size <- 1
    for (dir in list(c(-1, 0), c(1, 0), c(0, -1), c(0, 1))) {
        correction <- process(coordinates, borders, garden, type, xpos + dir[1], ypos + dir[2])
        if (correction == -1) {
            borders$coordinates <- rbind(borders$coordinates, data.table(x = xpos + 0.4 * dir[1], y = ypos + 0.4 * dir[2]))
            # 0.4 to indicate the inside and outside of the fence
        } else {
            size <- size + correction
        }
    }
    return(size)
}

fence_pricing <- function(data, scoring_function) {
    garden <- data |> as.matrix()
    plots <- data.table(x = rep(1:nrow(garden), ncol(garden)), y = rep(1:ncol(garden), each = nrow(garden)))
    plots[, `:=`(active = TRUE, score = 0)]
    
    while (plots[active == TRUE, .N] > 0) {
        entrance <- plots[active == TRUE, .(x, y)][1]
        borders <- new.env()
        assign("coordinates", data.table(x = numeric(), y = numeric()), envir = borders)
        size <- process(plots, borders, garden, garden[entrance[,x], entrance[,y]], entrance[,x], entrance[,y])
        plots[x == entrance[,x] & y == entrance[,y], score := scoring_function(size, borders$coordinates)]
    }

    plots[, score] |> sum()
}

solve1 <- function(data) {
    score <- function(size, border) {
        return(size * (nrow(border)))
    }
    return(fence_pricing(data, score))
}

solve2 <- function(data) {
    score <- function(size, border) {
        setorder(border, y)
        border[x %% 1 != 0, diff := c(1, diff(y)), by = x]
        setorder(border, x)
        border[y %% 1 != 0, diff := c(1, diff(x)), by = y]
        segments <- border[x %% 1 != 0, .N, by = x][, .N] + border[x %% 1 != 0 & diff != 1, .N] + border[y %% 1 != 0, .N, by = y][, .N] + border[y %% 1 != 0 & diff != 1, .N]        
        return(size * segments)
    }
    return(fence_pricing(data, score))
}
