library(data.table)

data <- fread("data/ad_viz_plotval_data.csv", select = c("Date", "Daily Mean PM2.5 Concentration", "Site ID"))

sites <- unique(data[["Site ID"]])
conc <- data[["Daily Mean PM2.5 Concentration"]]

data[`Site ID` == sites[1] & `Daily Mean PM2.5 Concentration` > 10]
