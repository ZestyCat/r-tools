library(data.table)
library(ggplot2)
library(dplyr)

plot <- TRUE

data <- fread("data/alabama_pm.csv",
              select = c("Date",
                         "Daily Mean PM2.5 Concentration",
                         "UNITS",
                         "Site ID",
                         "Site Name"))

data <- data[, .(Date = as.Date(Date, format = "%m/%d/%y"),
                 conc = `Daily Mean PM2.5 Concentration`,
                 site = as.character(`Site ID`),
                 name = `Site Name`,
                 units = UNITS)]

yearly_mean <- data[, .(mean_conc_ugm3 = mean(conc),
                        attainment = mean(conc) < 13),
                        by = .(site, name)]

daily_limit_exceeded <- data[conc > 35L, .(count = sum(table(site))), Date]

ggplot(data) +
   geom_point(aes(Date, conc, color = site)) +
   geom_smooth(aes(Date, conc)) +
   labs(title = "Measured concentration by date",
        x = "Date",
        y = "Concentration (ug/m^3)"
   ) +
   scale_color_discrete(name = "Site") +
   theme(
        plot.title = element_text(size = 30, face = "bold")
   ) +
   theme_bw()

data
