if (!requireNamespace("bit64", quietly = TRUE)) {
    install.packages("bit64")
}
library(bit64)

read_input <- function(filename) {
    lines <- data.table::fread(filename, header = FALSE, sep = "\n")
    data.table::setDT(lines)[, data.table::tstrsplit(V1, split = "(?=.)", perl = TRUE, type.convert = TRUE)] |> data.table::transpose()
}

State <- setRefClass(
    "State",
    fields = list(
        reader_pos = "numeric",
        file_number = "numeric",
        file_size = "numeric",
        occupied = "logical",
        space_size = "numeric",
        last_file_number = "numeric",
        last_file_size = "numeric",
        checksum = "integer64"
    )
)

solve1 <- function(data) {
    state <- State$new(
        reader_pos = 0,
        file_number = 0,
        file_size = 0,
        occupied = TRUE,
        space_size = 0,
        last_file_number = (nrow(data) - 1) / 2,
        last_file_size = 0,
        checksum = as.integer64(0)
    )
    cat("Number of files:", (nrow(data) + 1) / 2, "\n")
    cat("Length of disk:", sum(data), "blocks\n")

    while (state$file_number != state$last_file_number || (state$file_size + state$last_file_size) != data[state$file_number * 2 + 1]) {
        if (state$occupied) {
            if (state$file_size < data[state$file_number * 2 + 1]) {
                state <- State$new(
                    reader_pos = state$reader_pos + 1,
                    file_number = state$file_number,
                    file_size = state$file_size + 1,
                    occupied = TRUE,
                    space_size = 0,
                    last_file_number = state$last_file_number,
                    last_file_size = state$last_file_size,
                    checksum = state$checksum + state$file_number * state$reader_pos
                )
            } else {
                state$occupied <- FALSE
                cat("File", state$file_number, "completed\n")
            }
        } else {
            if (state$space_size < data[state$file_number * 2 + 2]) {
                if (state$last_file_size < data[state$last_file_number * 2 + 1]) {
                    state <- State$new(
                        reader_pos = state$reader_pos + 1,
                        file_number = state$file_number,
                        file_size = state$file_size,
                        occupied = FALSE,
                        space_size = state$space_size + 1,
                        last_file_number = state$last_file_number,
                        last_file_size = state$last_file_size + 1,
                        checksum = state$checksum + state$last_file_number * state$reader_pos
                    )
                } else {
                    state$last_file_number <- state$last_file_number - 1
                    state$last_file_size <- 0
                }
            } else {
                state$file_number <- state$file_number + 1
                state$file_size <- 0
                state$occupied <- TRUE
            }
        }
        
    }

    state$checksum
}

solve2 <- function(data) {
    data.table::setnames(data, "block_size")
    disk <- data.table::copy(data)
    disk[, block_begin := cumsum(block_size) - block_size]
    checksum <- as.integer64(0)

    for (i in ((nrow(data) - 1)/2):0) {
        file_size <- data$block_size[i * 2 + 1]
        spaces <- which(disk$block_size >= as.numeric(file_size))
        spaces <- spaces[spaces %% 2 == 0]
        if (length(spaces) > 0 && spaces[1] < i * 2 + 1) {
            checksum <- checksum + (file_size * (file_size - 1) / 2 + file_size * disk$block_begin[spaces[1]]) * i
            disk[spaces[1]] <- disk[spaces[1]] + c(-file_size, file_size)
            disk$block_size[spaces[1] - 1] <- disk$block_size[spaces[1] - 1] + file_size
        } else {
            checksum <- checksum + (file_size * (file_size - 1) / 2 + file_size * disk$block_begin[i * 2 + 1]) * i
        }
    }

    checksum
}
