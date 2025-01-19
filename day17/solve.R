library(bit64)

read_input <- function(filename) {
    lines <- readLines(filename)
    regA <- as.integer(strsplit(lines[1], " ")[[1]][3])
    regB <- as.integer(strsplit(lines[2], " ")[[1]][3])
    regC <- as.integer(strsplit(lines[3], " ")[[1]][3])
    program <- as.integer(strsplit(strsplit(lines[5], " ")[[1]][2], ",")[[1]])
    return(list(regA, regB, regC, program))
}

xor64 <- function(a, b) {
    value <- 0
    for (i in 0:63) {
        value <- value + 2^i * ((a %/% 2^i) %% 2 + (b %/% 2^i) %% 2) %% 2
    }
    return(value)
}

state_machine <- function(registers, program) {
    regA <- registers[1]
    regB <- registers[2]
    regC <- registers[3]
    pc <- 1

    combo <- function(value) {
        if (value == 4) {
            return(regA)
        } else if (value == 5) {
            return(regB)
        } else if (value == 6) {
            return(regC)
        } else if (value == 7) {
            stop("Invalid register")
        } else {
            return(value)
        }
    }
    output <- ""

    while(pc <= length(program)) {
        instr <- program[pc]
        
        if (instr == 0) {
            regA <- regA %/% 2^combo(program[pc + 1])
        } else if (instr == 1) {
            regB <- xor64(regB, program[pc + 1])
        } else if (instr == 2) {
            regB <- combo(program[pc + 1]) %% 8
        } else if (instr == 3) {
            pc <- if (regA == 0) pc else program[pc + 1] - 1
        } else if (instr == 4) {
            regB <- xor64(regB, regC)
        } else if (instr == 5) {
            output <- paste(output, combo(program[pc + 1]) %% 8, sep = ",")
        } else if (instr == 6) {
            regB <- regA %/% 2^combo(program[pc + 1])
        } else if (instr == 7) {
            regC <- regA %/% 2^combo(program[pc + 1])
        } else {
            message("Unknown instruction: ", instr)
        }        
        pc <- pc + 2
    }
    return(substr(output, 2, nchar(output)))
}


solve1 <- function(data) {
    registers <- c(data[[1]], data[[2]], data[[3]])
    output <- state_machine(registers, data[[4]])
    print(paste("Output: ", output))
    return(output)
}

recursive <- function(length, programm, start) {
    if (length == 0) {
        return(start)
    }
    for (i in 0:7) {
        registers <- c(8 * start + i, 0, 0)
        output <- state_machine(registers, programm)
        if (output == paste(programm[length:length(programm)], collapse = ",")) {
            tmp <- recursive(length - 1, programm, 8 * start + i)
            if (!is.na(tmp)) {
                return(tmp)
            }
        }
    }
    return(NA)
}

solve2 <- function(data) {
    return(recursive(length(data[[4]]), data[[4]], 0))
}
