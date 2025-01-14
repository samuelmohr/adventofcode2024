library(dplyr)

read_input <- function(filename) {
    data <- data.table::fread(filename, header = FALSE, sep = " ")
    data <- data |> mutate(
        Position_X = as.numeric(str_extract(V1, "(?<=p=)-?\\d*")),
        Position_Y = as.numeric(str_extract(V1, "(?<=,)-?\\d*")),
        Velocity_X = as.numeric(str_extract(V2, "(?<=v=)-?\\d*")),
        Velocity_Y = as.numeric(str_extract(V2, "(?<=,)-?\\d*"))
    )
}

solve1 <- function(data) {
    wide <- 101
    tall <- 103
    time <- 100

    data[, x := (Position_X + Velocity_X * time) %% wide]
    data[, y := (Position_Y + Velocity_Y * time) %% tall]

    data[x < (wide - 1) / 2 & y < (tall - 1) / 2, .N] *
      data[x > (wide - 1) / 2 & y < (tall - 1) / 2, .N] *
      data[x < (wide - 1) / 2 & y > (tall - 1) / 2, .N] *
      data[x > (wide - 1) / 2 & y > (tall - 1) / 2, .N]
}

solve2 <- function(data) {
    wide <- 101
    tall <- 103
    time_range <- 0:10000

    for (time in time_range) {
        data[, x := (Position_X + Velocity_X * time) %% wide]
        data[, y := (Position_Y + Velocity_Y * time) %% tall]
        consecutives <- data[, .(consecutive = sum(diff(sort(x)) == 1)), by = y][, sum(consecutive)]

        if (consecutives > 200) {
            image <- matrix(" ", nrow = tall, ncol = wide)
            for (i in 1:nrow(data)) {
                image[data[i, y + 1], data[i, x + 1]] <- "#"
            }
            image_str <- apply(image, 1, paste, collapse = "")
            print(image_str)
            return(time)
        }
    }

}

