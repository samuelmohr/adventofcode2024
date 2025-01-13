library(data.table)
library(dplyr)
library(stringr)
library(bit64)

read_input <- function(filename) {
    lines <-  strsplit(readr::read_file(filename), "\n\n", fixed = TRUE)[[1]]
    data <- data.table::data.table(V1 = lines)[, tstrsplit(V1, split = "\n", perl = TRUE)]

    data <- data |> mutate(
        ButtonA_X = as.integer64(str_extract(V1, "(?<=X\\+)\\d*")),
        ButtonA_Y = as.integer64(str_extract(V1, "(?<=Y\\+)\\d*")),
        ButtonB_X = as.integer64(str_extract(V2, "(?<=X\\+)\\d*")),
        ButtonB_Y = as.integer64(str_extract(V2, "(?<=Y\\+)\\d*")),
        Prize_X = as.integer64(str_extract(V3, "(?<=X=)\\d*")),
        Prize_Y = as.integer64(str_extract(V3, "(?<=Y=)\\d*"))
    )
}

solve1 <- function(data) {
    data[, b := (Prize_X * ButtonA_Y - Prize_Y * ButtonA_X) / (ButtonB_X * ButtonA_Y - ButtonB_Y * ButtonA_X)]
    data[, a := (Prize_X - ButtonB_X * b) / ButtonA_X]
    data[a %% 1 == 0 & b %% 1 == 0, .(a, b)][, sum(3 * a + b)]
}

solve2 <- function(data) {
    data[, divisable := ((10000000000000 + Prize_X) * ButtonA_Y - (10000000000000 + Prize_Y) * ButtonA_X) %% (ButtonB_X * ButtonA_Y - ButtonB_Y * ButtonA_X) == 0]
    data[, b := ((10000000000000 + Prize_X) * ButtonA_Y - (10000000000000 + Prize_Y) * ButtonA_X) %/% (ButtonB_X * ButtonA_Y - ButtonB_Y * ButtonA_X)]
    data[, divisable := divisable & (10000000000000 + Prize_X - ButtonB_X * b) %% ButtonA_X == 0]
    data[, a := (10000000000000 + Prize_X - ButtonB_X * b) %/% ButtonA_X]
    data[divisable == TRUE, .(a, b)][, sum(3 * a + b)]
}

