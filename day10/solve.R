library(data.table)

read_input <- function(filename) {
    lines <- data.table::fread(filename, header = FALSE, sep = "\n", colClasses = "character")
    data <- setDT(lines)[, tstrsplit(V1, split = "(?=.)", perl = TRUE, type.convert = TRUE)]
    data[]
}

peak_finder <- function(data, xpos, ypos) {
    if (data[xpos, ypos] == 9) {
        return(list(c(xpos, ypos)))
    }
    peaks <- list()
    if (xpos > 1 && data[xpos - 1, ypos] == data[xpos, ypos] + 1) {
        peaks <- c(peaks, peak_finder(data, xpos - 1, ypos))
    }
    if (xpos < nrow(data) && data[xpos + 1, ypos] == data[xpos, ypos] + 1) {
        peaks <- c(peaks, peak_finder(data, xpos + 1, ypos))
    }
    if (ypos > 1 && data[xpos, ypos - 1] == data[xpos, ypos] + 1) {
        peaks <- c(peaks, peak_finder(data, xpos, ypos - 1))
    }
    if (ypos < ncol(data) && data[xpos, ypos + 1] == data[xpos, ypos] + 1) {
        peaks <- c(peaks, peak_finder(data, xpos, ypos + 1))
    }
    return(peaks)
}

solve1 <- function(data) {
    trail_heads <- which(data == "0", arr.ind = TRUE) |> as.data.table() |> setnames(c("x", "y"))
    peaks <- trail_heads[, .(peaks = peak_finder(as.matrix(data), x, y)), by = 1:nrow(trail_heads)]
    peaks[, .(peaks = unique(peaks)), by = nrow] |> nrow()
}

solve2 <- function(data) {
    trail_heads <- which(data == "0", arr.ind = TRUE) |> as.data.table() |> setnames(c("x", "y"))
    peaks <- trail_heads[, .(peaks = peak_finder(as.matrix(data), x, y)), by = 1:nrow(trail_heads)]
    peaks |> nrow()
}
