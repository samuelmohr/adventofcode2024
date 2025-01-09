if (!requireNamespace("bit64", quietly = TRUE)) {
    install.packages("bit64")
}
library(bit64)

read_input <- function(filename) {
    data <- data.table::fread(filename, header = FALSE, fill = TRUE)
    data.table::setnames(data, "V1", "result")
    data[ , result := as.integer64(substr(result, 1, nchar(result) - 1))][]
}

check_calc <- function(numbers, result, extended = FALSE) {
    if (length(numbers) == 1) {
        return(numbers == result)
    }
    element <- tail(numbers, 1)
    remaining <- head(numbers, -1)
    if (is.na(element)) {
        return(check_calc(remaining, result, extended))
    }

    if (extended) {
        n_digits <- floor(log10(element)) + 1
        if (result %% (10 ^ n_digits) == element) {
            if (check_calc(remaining, floor(result / (10 ^ n_digits)), extended)) {
                return(TRUE)
            }
        }
    }

    if (result %% element == 0) {
        if (sum(remaining) * element == result) {
            return(TRUE)
        }
        if (check_calc(remaining, result / element, extended)) {
            return(TRUE)
        }
    }

    if (sum(remaining) + element == result) {
        return(TRUE)
    }
    if (check_calc(remaining, result - element, extended)) {
        return(TRUE)
    }
    return(FALSE)
}

solve1 <- function(data) {
    data[, valid := FALSE]
    for (i in 1:nrow(data)) {
        if (check_calc(as.integer(data[i, 2:(length(data) - 1)]), data$result[i])) {
            data[i, valid := TRUE]
        }
    }
    return(sum(data$result[data$valid]))
}

solve2 <- function(data) {
    for (i in 1:nrow(data)) {
        if (data$valid[i] || check_calc(as.integer(data[i, 2:(length(data) - 1)]), data$result[i], TRUE)) {
            data[i, valid := TRUE]
        }
    }
    return(sum(data$result[data$valid]))
}

