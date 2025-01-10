library(data.table)

if (!requireNamespace("gmp", quietly = TRUE)) {
    install.packages("gmp")
}

read_input <- function(filename) {
    lines <- data.table::fread(filename, header = FALSE, sep = "\n")
    data <- setDT(lines)[, tstrsplit(V1, split = "(?=.)", perl = TRUE)]
    data[]
}

solve1 <- function(data) {
    counts <- table(unlist(data))
    antinodes <- data.table(x = numeric(), y = numeric())

    for (element in names(counts)) {
        if (element == ".") {
            next
        }
        positions <- which(data == element, arr.ind = TRUE)
        for (i in 1:(nrow(positions) - 1)) {
            for (j in (i + 1):nrow(positions)) {
                antinode1 <- c(2 * positions[i, 1] - positions[j, 1], 2 * positions[i, 2] - positions[j, 2])
                antinode2 <- c(2 * positions[j, 1] - positions[i, 1], 2 * positions[j, 2] - positions[i, 2])
                antinodes <- rbind(antinodes, data.table(x = c(antinode1[1], antinode2[1]), y = c(antinode1[2], antinode2[2])))
            }
        }
    }

    nrow(unique(antinodes[antinodes$x >= 1 & antinodes$x <= nrow(data) & antinodes$y >= 1 & antinodes$y <= ncol(data)]))
}

solve2 <- function(data) {
    counts <- table(unlist(data))
    antinodes <- data.table(x = numeric(), y = numeric())

    for (element in names(counts)) {
        if (element == ".") {
            next
        }
        positions <- which(data == element, arr.ind = TRUE)
        for (i in 1:(nrow(positions) - 1)) {
            for (j in (i + 1):nrow(positions)) {
                diff <- positions[j, ] - positions[i, ]
                gcd <- abs(gmp::gcd(diff[1], diff[2]))
                diff <- diff / gcd

                antinode1 <- positions[i, ]
                while (antinode1[1] >= 1 & antinode1[1] <= nrow(data) & antinode1[2] >= 1 & antinode1[2] <= ncol(data)) {
                    antinodes <- rbind(antinodes, data.table(x = antinode1[1], y = antinode1[2]))
                    antinode1 <- antinode1 + diff
                }
                antinode2 <- positions[i, ] - diff
                while (antinode2[1] >= 1 & antinode2[1] <= nrow(data) & antinode2[2] >= 1 & antinode2[2] <= ncol(data)) {
                    antinodes <- rbind(antinodes, data.table(x = antinode2[1], y = antinode2[2]))
                    antinode2 <- antinode2 - diff
                }
            }
        }
    }

    nrow(unique(antinodes[antinodes$x >= 1 & antinodes$x <= nrow(data) & antinodes$y >= 1 & antinodes$y <= ncol(data)]))
}
