library(data.table)
library(ggplot2)

data <- fread("data/alabama_pm.csv",
              select = c("Date",
                         "Daily Mean PM2.5 Concentration",
                         "UNITS",
                         "Site ID"))

data <- data[, .(Date, conc = `Daily Mean PM2.5 Concentration`, site = `Site ID`)]

days_over <- data.table(table(data[conc > 35, Date]))[order(N, -V1), .(Date = V1, count = N)]
bad_sites <- data.table(table(data[conc > 35, site]))[order(-N), .(site = V1, days_over = N)]

ggplot(days_over, aes(Date, count)) +
    geom_col()

ggplot(bad_sites, aes(site, days_over)) +
    geom_col()
