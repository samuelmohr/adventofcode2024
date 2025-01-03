read_input <- function(filename) {
    program <- paste(readLines(filename), collapse = "")
    data.table(V1 = program)
}

library(data.table)

solve1 <- function(data) {
    split_data <- setDT(data)[, tstrsplit(V1, split = "(?=mul\\(\\d{1,3},\\d{1,3}\\))", perl = TRUE)]
    split_data <- split_data[, .SD, .SDcols = seq(3, ncol(split_data), by = 2)]

    eval <- function(string) {
        factor1 <- as.numeric(gsub("ul\\((\\d{1,3}),(\\d{1,3})\\).*", "\\1", string))
        factor2 <- as.numeric(gsub("ul\\((\\d{1,3}),(\\d{1,3})\\).*", "\\2", string))
        factor1 * factor2
    }
    
    split_data[, lapply(.SD, eval)] |> sum(na.rm = TRUE)
}

solve2 <- function(data) {
    split_data <- setDT(data)[, tstrsplit(V1, split = "(?=mul\\(\\d{1,3},\\d{1,3}\\))|(?=don't\\(\\))|(?=do\\(\\))", perl = TRUE)]
    split_data <- split_data[, .SD, .SDcols = seq(3, ncol(split_data), by = 2)]

    eval <- function(string) {
        if (grepl("^on't\\(\\)", string)) {
            return("disabled")
        }
        if (grepl("^o\\(\\)", string)) {
            return("enabled")
        }
        factor1 <- as.numeric(gsub("ul\\((\\d{1,3}),(\\d{1,3})\\).*", "\\1", string))
        factor2 <- as.numeric(gsub("ul\\((\\d{1,3}),(\\d{1,3})\\).*", "\\2", string))
        factor1 * factor2
    }

    split_data <- split_data[, lapply(.SD, function(col) sapply(col, eval))] |> transpose()

    disabling <- function(column) {
        disabled <- FALSE
        for (i in seq_along(column)) {
            if (is.na(column[i])) {
                next
            }
            if (column[i] == "disabled") {
                disabled <- TRUE
                column[i] <- NA
            } else if (column[i] == "enabled") {
                disabled <- FALSE
                column[i] <- NA
            } else if (disabled) {
                column[i] <- 0
            }
        }
        as.numeric(column)
    }
    split_data <- split_data[, lapply(.SD, disabling)]
    split_data |> sum(na.rm = TRUE)
}
