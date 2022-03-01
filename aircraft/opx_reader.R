# Read the NMAP .opx or .o10_output file in .txt format
# Write the tables into individual .csv files

library(stringr)
library(data.table)

text <- readLines("~/R/r-tools/data/O10_output_2-28-22.txt")

# Get the starting lines for each power setting in the data file
op_lines <- function(opx) { 
    lines <- c()
    for (i in seq_along(text)) {
        ifelse(
               grepl("FLIGHT AIRCRAFT ID:", text[i]) == TRUE,
               lines <- c(lines, i),
               next
        )
    }
    return(lines)
}

# Get the information about the power setting
op_info <- function(opx) {
    lines <- op_lines(opx)
    info_list <- list()
    for (i in seq_along(lines)) {
        data <- fread(text = str_squish(opx[lines[i] : (lines[i] + 6)]),
              sep = ":",
              col.names = c("param", "value"))
        power <- word(
                      str_squish((opx[lines[i] + 10])), 
                      3, 4)
        info_list[[i]] <- dcast(
                            melt(
                                 rbind(data, 
                                       list(param = "POWER SETTING",
                                            value = power)),
                                 id.vars = "param"),
                            variable ~ param)[, 2:7]
    }
    return(rbindlist(info_list))
}

# Get the noise data
op_data <- function(opx) {
    lines <- op_lines(opx)
    data_list <- list()
    for (i in seq_along(lines)) {
        data <- fread(
                text =
                str_squish(opx[(lines[i] + 12):(lines[i] + 35)]))
        data_list[[i]] <- data
    }
    return(data_list)
}

op_info_test <- function(opx) {
    lines <- op_lines(opx)
    info_list <- list()
    for (i in seq_along(lines)) {
        #info_list[[i]] <- str_squish(opx[lines[i] : (lines[i] + 5)])
        item <- info_list[[i]] <- str_squish(opx[lines[i] : (lines[i] + 5)])
        param_value <- str_split(item, ": ")
    }
    #print(data.table(info_list))
}

op_info_test(text)
