library(data.table)

read_input <- function(filename) {
    lines <- data.table::fread(filename, header = FALSE, sep = "\n")
    data <- setDT(lines)[, tstrsplit(V1, split = "(?=.)", perl = TRUE)]
    data[]
}

test_xmas <- function(data, xpos, ypos, xstep, ystep) {
    tryCatch({
        return(data[[xpos, ypos]] == "X" && data[[xpos + xstep, ypos + ystep]] == "M" && data[[xpos + 2 * xstep, ypos + 2 * ystep]] == "A" && data[[xpos + 3 * xstep, ypos + 3 * ystep]] == "S")
    }, error = function(e) {
        return(FALSE)
    })
}

test_mas <- function(data, xpos, ypos, xstep, ystep) {
    tryCatch({
        return(data[[xpos, ypos]] == "A" && data[[xpos + xstep, ypos + ystep]] == "S" && data[[xpos - xstep, ypos - ystep]] == "M")
    }, error = function(e) {
        return(FALSE)
    })
}

solve1 <- function(data) {
    starts <- which(data == "X", arr.ind = TRUE) |> as.data.table() 
    # for each starting position, check if the next 3 steps are M, A, S
    check <- function(start) {
        # check west
        west <- test_xmas(data, start[1], start[2], 0, -1)
        # check north west
        nw <- test_xmas(data, start[1], start[2], -1, -1)
        # check north
        north <- test_xmas(data, start[1], start[2], -1, 0)
        # check north east
        ne <- test_xmas(data, start[1], start[2], -1, 1)
        # check east
        east <- test_xmas(data, start[1], start[2], 0, 1)
        # check south east
        se <- test_xmas(data, start[1], start[2], 1, 1)
        # check south
        south <- test_xmas(data, start[1], start[2], 1, 0)
        # check south west
        sw <- test_xmas(data, start[1], start[2], 1, -1)
        # return sum of all directions
        return(west + nw + north + ne + east + se + south + sw)
    }
    apply(starts, 1, check) |> sum()
}

solve2 <- function(data) {
    starts <- which(data == "A", arr.ind = TRUE) |> as.data.table()
    # for each starting position, check if there is a MAS cross pattern
    check <- function(start) {
        # check backslash
        backslash <- test_mas(data, start[1], start[2], -1, -1) || test_mas(data, start[1], start[2], 1, 1)
        # check forward slash
        forwardslash <- test_mas(data, start[1], start[2], -1, 1) || test_mas(data, start[1], start[2], 1, -1)
        # return true if both are true
        return(backslash && forwardslash)
    }
    apply(starts, 1, check) |> sum()
}
