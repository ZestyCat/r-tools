library(dplyr)
library(data.table)

data <- fread("data/LS_11-4-21_FF.csv")[1:3, c(2, 4)]

timestr <- c()

for (t in seq_along(data[[1]])) {
	if (nchar(data[[1]][t]) == 11) {
		timestr <- c(timestr, paste("0", data[[1]][t], sep = ""))
	}
}

timestamp <- strptime(timestr, "%H%M%S")

timestamp
