library(data.table)
library(bit64)

read_input <- function(filename) {
    data.table::fread(filename, header = FALSE, colClasses = "numeric") |> transpose() |> setnames(c("stone"))
}

transform <- function(number) {
    if (number == 0) {
        return(c(1))
    } else if (nchar(number) %% 2 == 0) {
        return(c(as.integer(substr(number, 1, nchar(number) / 2)), as.integer(substr(number, (nchar(number) / 2) + 1, nchar(number)))))
    } else {
        return(c(number * 2024))
    }
}

memory <- data.table(number = integer(), iterations = integer(), result = integer64())
setkey(memory, number, iterations)

blink <- function(Xnumber, Xiterations) {
    if (Xiterations == 1) {
        return(as.integer64(length(transform(Xnumber))))
    }
    if (memory[number == Xnumber & iterations == Xiterations, .N] > 0) {
        result <- as.integer64(c())
        for (i in 2:Xiterations) {
            result <- c(result, as.integer64(memory[number == Xnumber & iterations == (Xiterations - i + 2), result]))
        }
        return(result)
    }
    stones <- transform(Xnumber)
    result <- blink(stones[1], Xiterations - 1)
    if (length(stones) > 1) {
        result <- result + blink(stones[2], Xiterations - 1)
    }
    if (Xiterations > 2) {
        t <- blink(stones[1], 1)
        if (length(stones) > 1) {
            t <- t + blink(stones[2], 1)
        }
        result <- c(result, t)
    }
    for (i in 1:length(result)) {
        if (memory[number == Xnumber & iterations == (Xiterations - i + 1), .N] == 0) {
            memory <<- rbind(memory, data.table(number = Xnumber, iterations = (Xiterations - i + 1), result = as.integer64(result[i])))
        } else {
            break
        }
    }
    return(result)
}

solve1 <- function(data) {
    data[, .(result = blink(stone, 25)[1]), by = 1:nrow(data)][, sum(result)]
}

solve2 <- function(data) {
    data[, .(result = blink(stone, 75)[1]), by = 1:nrow(data)][, sum(result)]
}
