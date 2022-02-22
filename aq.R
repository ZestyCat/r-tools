library(data.table)
library(ggplot2)
library(ggthemes)
library(dplyr)

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

table_mean <- data[, .(mean_conc_ugm3 = mean(conc),
                        attainment = mean(conc) < 13),
                        .(site, name = ifelse(name == "", site, name))]

dplyr_mean <- group_by(data, site, name) %>%
                summarize(mean_conc_ugm3 = mean(conc)) %>%
                mutate(attainment = mean_conc_ugm3 < 13,
                       name = ifelse(name == "", site, name))

table_mean

dplyr_mean

#p <- ggplot(data) +
#       geom_point(aes(Date, conc, color = site), size = 1.25) +
#       geom_hline(yintercept = 35, linetype = "dashed") +
#       geom_text(
#                 aes(x = as.Date("2020/01/01"),
#                     y = 35, label = "35ug/m^3 daily NAAQS",
#                     hjust = 0.3,
#                     vjust = -0.5),
#                 data = data.frame()) +
#       labs(title = "Measured concentration by date",
#            x = "Date",
#            y = "Concentration (ug/m^3)"
#       ) +
#       scale_color_discrete(name = "Site") +
#       theme(
#            plot.title = element_text(size = 30, face = "bold")) +
#       theme_solarized(light = FALSE)
#
#ggsave("/mnt/c/Users/gregory.bizup/Pictures/ggplot.png", plot = p, width = 15, height = 7, dpi = 1000)
