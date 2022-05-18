library(tidyverse)
library(data.table)

read_eeps <- function(file) {
    lines <- fread(file, fill = TRUE)
    lines
}

read_eeps("../data/Comparison/2005_5-18-22.csv")
