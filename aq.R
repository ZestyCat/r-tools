library(data.table)

data <- fread("data/alabama_pm.csv",
              select = c("Date",
                         "Daily Mean PM2.5 Concentration",
                         "UNITS",
                         "Site ID"))

data[["Attainment"]] <- data[["Daily Mean PM2.5 Concentration"]] < 35

data[Attainment == FALSE]

table(data[["Attainment"]])
