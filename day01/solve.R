read_input <- function(filename) {
    data.table::fread(filename, header = FALSE)[]
}

solve1 <- function(data) {
    data <- data[, lapply(.SD, sort)] 
    data <- data[, diff := abs(V1 - V2)]
    data[, sum(diff)][]
}

solve2 <- function(data) {
    counts <- data[, .N, by = V2]
    counts <- counts[ data, on = .(V2 = V1), .(V1, freq = N), nomatch = 0]
    counts[, score := V1 * freq]
    counts[, sum(score)]
}
