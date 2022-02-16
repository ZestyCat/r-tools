data <- read.table("./data/F-18.csv",
                   header = FALSE,
                   sep = ",",
                   skip = 3,
                   strip.white = TRUE,
                   col.names = c(read.table("./data/F-18.csv",
                               header = FALSE,
                               sep = ",",
                               skip = 1,
                               nrows = 1))
                   )[, c("Distance", "SEL")]
mean(data[, c("SEL")])
