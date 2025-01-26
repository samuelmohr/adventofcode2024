library(data.table)
library(parallel)

read_input <- function(filename) {
    lines <- readLines(filename)
    patterns <- setDT(data.table::fread(text = paste(lines[1], "\n"), header = FALSE, sep = ",")) |> transpose() 
    setorderv(patterns)
    designs <- setDT(data.table::fread(text = paste(lines[3:length(lines)], collapse = "\n"), header = FALSE, sep = "\n"))
    
    return(list(patterns, designs))
}

is_valid <- function(patterns, design) {
    if(nchar(design) == 0) {
        return(TRUE)
    }
    for(pattern in patterns[[1]]) {
        if(startsWith(design, pattern)) {
            if(is_valid(patterns, substr(design, nchar(pattern) + 1, nchar(design)))) {
                return(TRUE)
            }
        }
    }
    return(FALSE)
}

count_valid <- function(patterns, design, max_length) {
    if(nchar(design) == 0) {
        return(1)
    }

    mid_index <- (nchar(design) + 1) %/% 2
    count <- 0
    for(l in seq_len(max_length)) {
        if(l > nchar(design)) {
            break
        }
        for(offset in max(0, l - nchar(design) + mid_index - 1):min(l - 1, mid_index - 1)) {
            possible_patterns <- patterns[length == l & V1 == substr(design, mid_index - offset, mid_index - offset + l - 1)]
            if(nrow(possible_patterns) > 0) {
                count_left <- count_valid(patterns, substr(design, 1, mid_index - offset - 1), max_length)
                if(count_left > 0) {
                    count_right <- count_valid(patterns, substr(design, mid_index - offset + l, nchar(design)), max_length)
                    count <- count + count_left * count_right * nrow(possible_patterns)
                }
            }
        }
    }
    return(count)
}


solve1 <- function(data) {
    patterns <- data[[1]]
    designs <- data[[2]]
    designs[, valid := sapply(designs$V1, function(design) is_valid(patterns, design))]
    return(sum(designs$valid))
}

solve2 <- function(data) {
    patterns <- data[[1]]
    patterns[, length := nchar(V1)]
    max_length <- max(patterns$length)
    designs <- data[[2]]
    designs[, possibilities := mcmapply(function(design, valid) if (valid) count_valid(patterns, design, max_length) else 0, V1, valid, mc.cores = detectCores())]
    return(sum(designs$possibilities))
}
