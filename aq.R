library(readr)

data <- read_csv("./data/ad_viz_plotval_data.csv")[
	c("Date", "Daily Mean PM2.5 Concentration", "UNITS", "Site Name")]

mean(data[[2]])
