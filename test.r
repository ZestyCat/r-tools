library(data.table)
library(readr)

data <- fread("data/LS_11-4-21_FF.csv", select = c(2, 4))

data[[1]] <- round(data[[1]], digits = 0)

for (i in seq_along(data[[1]])) {
    if (nchar(data[[1]][i] == 5)) {
        data[[1]][i] <- as.character(strptime(paste("0", data[[1]][i], sep = ""), "%H%M%S"))
    } else {
        data[[1]][i] <- as.character(strptime(data[[1]][i], "%H%M%S"))
    }
}


