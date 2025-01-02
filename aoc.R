typeline <- function(prompt="Enter text: ") {
  if (interactive() ) {
    txt <- readline(prompt)
  } else {
    cat(prompt);
    txt <- readLines("stdin",n=1);
  }
  return(txt)
}

day <- as.integer(typeline(prompt = "Enter the day: "))

source(sprintf("./day%02d/solve.R", day))

filename_option <- as.integer(typeline(prompt = "Choose the input file (1 for inputText.txt, 2 for input.txt): "))

filename <- switch(filename_option,
                   "1" = "inputTest.txt",
                   "2" = "input.txt",
                   stop("Invalid option"))

cat("You chose:", filename, "\n")

data <- read_input(sprintf("./day%02d/%s", day, filename))

cat("Part 1: ", solve1(data), "\n")

cat("Part 2: ", solve2(data), "\n")

