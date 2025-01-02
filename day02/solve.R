read_input <- function(filename) {
    data.table::fread(filename, header = FALSE, fill = TRUE) |> data.table::transpose()
}

goodness <- function(row) {
    min_max <- range(diff(row), na.rm = TRUE)
    (min_max[1] >= -3 && min_max[2] <= -1) || (min_max[1] >= 1 && min_max[2] <= 3)
}

solve1 <- function(data) {
    data[ , lapply(.SD, goodness)] |> sum()
}

solve2 <- function(data) {
    goodness_with_error <- function(row) {
        if (goodness(row))
            return(TRUE)
        differences <- diff(row)
        bad <- which(abs(differences) > 3 | differences == 0)
        if (length(bad) > 0) {
            return(goodness(row[-bad[1]]) || goodness(row[-(bad[1] + 1)]) )
        } else {
            negatives <- which(differences < 0)
            positives <- which(differences > 0)
            if (length(negatives) == 1) {
                return(goodness(row[-negatives[1]]) || goodness(row[-(negatives[1] + 1)]) )
            } else if (length(positives) == 1) {
                return(goodness(row[-positives[1]]) || goodness(row[-(positives[1] + 1)]) )
            } else {
                return(FALSE)
            }
        }
    }

    data[ , lapply(.SD, goodness_with_error)] |> sum()
    
}
