read_input <- function(filename) {
    lines <- readLines(filename)
    split <- which(lines == "")[1]
    
    orders <- data.table::fread(text = paste(lines[1:(split - 1)], collapse = "\n"), header = FALSE, sep = "|")
    pages <- data.table::fread(text = paste(lines[(split + 1):length(lines)], collapse = "\n"), header = FALSE, sep = ",", fill = TRUE)
    # return the pair of data.tables
    return(list(orders, pages))
}

compare <- function(a, b, orders) {
    if(is.na(a) || is.na(b)) {
        return(0)
    }
    if(a == b) {
        return(0)
    }
    if(orders[V1 == a & V2 == b, .N] > 0) {
        return(1)
    }
    if(orders[V1 == b & V2 == a, .N] > 0) {
        return(-1)
    }
    return(0)
}

issorted <- function(list, orders) {
    first_na <- which(is.na(list))[1]
    if(is.na(first_na)) {
        first_na <- length(list) + 1
    }
    for(i in 1:(first_na - 2)) {
        for(j in (i + 1):(first_na - 1)) {
            if(compare(list[i], list[j], orders) == -1) {
                return(FALSE)
            }
        }
    }
    return(TRUE)
}

sort_page <- function(page, orders) {
    # implentation of insertion sort
    extract <- function(page, start, end) {
        return(if(end >= start) {page[start:end]} else { c() })
    }

    for(i in 2:(length(page))) {
        j <- 1
        while(j < i && compare(page[j], page[i], orders) >= 0) {
            j <- j + 1
        }
        if(j < i) {
            page <- c(extract(page, 1, (j - 1)), page[i], extract(page, j, (i - 1)), extract(page, (i + 1), length(page)))
        }
    }
    return(page)
}

solve1 <- function(data) {
    orders <- data[[1]]
    pages <- data[[2]]

    pages[, valid := apply(pages, 1, function(row) {
        issorted(row, orders)
    })]

    pages[, middle := apply(pages, 1, function(row) {
        row[!is.na(row)][ceiling(length(row[!is.na(row)])/2)]
    })]

    return(sum(pages[valid == TRUE, middle]))
}

solve2 <- function(data) {
    orders <- data[[1]]
    pages <- data[[2]]

    pages <- pages[valid == FALSE]
    pages <- pages[, c("valid", "middle") := NULL]

    pages <- data.table::transpose(pages)
    pages <- pages[, lapply(.SD, sort_page, orders)]
    pages <- data.table::transpose(pages)

    pages[, middle := apply(pages, 1, function(row) {
        row[!is.na(row)][ceiling(length(row[!is.na(row)])/2)]
    })]

    return(sum(pages[, middle]))
}
