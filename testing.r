library(readr)

data <- read_csv("./data/ad_viz_plotval_data.csv")

conc <- data[[5]]
mean(conc)
