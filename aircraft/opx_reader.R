# Read the NMAP .opx or .o10_output file in .txt format
# Write the tables into individual .csv files

library(stringr)
library(data.table)

text <- readLines("~/R/r-tools/data/O10_output_2-28-22.txt")

op_indeces <- function(opx) {
    indeces <- c()

    for (i in seq_along(text)) {
        ifelse(
               grepl("FLIGHT AIRCRAFT ID:", text[i]) == TRUE,
               indeces <- c(indeces, i),
               next
        )
    }

    return(indeces)
}

op_head <- function(opx) {
    indeces <- op_indeces(opx)
    heads <- list()
    for (i in seq_along(indeces)) {
        data <- fread(text = str_squish(opx[indeces[i] : (indeces[i] + 6)]),
              sep = ":",
              col.names = c("param", "value"))
        power <- word(str_squish((opx[indeces[i] + 10])), 3, 4)
        heads[[i]] <- dcast(melt(rbind(data, list(param = "POWER SETTING", value = power)), id.vars = "param"), variable ~ param)[, 2:7]
    }
    return(rbindlist(heads))
}

a <- op_head(text)
a
