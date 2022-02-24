library(ggplot2)
library(ggthemes)
library(dplyr)
library(data.table)

open_pmdata <- function(file) {
    data <- fread(file, select = c(1, 5, 6, 3, 8, 12))
    data <- setNames(data, c("date", "conc", "units", "site", "name", "param"))
    data <- data[, date := as.Date(date, format = "%m/%d/%y")]
    data <- data[, site := as.character(site)]
    return(data)
}

site_yearly_mean <- function(aq_data) {
    mean <- group_by(aq_data, site, name) %>%
            summarize(mean_conc_ugm3 = mean(conc)) %>%
            mutate(attainment = mean_conc_ugm3 < 13,
                name = ifelse(name == "", site, name))
    return(mean)
}

p <- ggplot(data) +
       geom_point(aes(Date, conc, color = site), size = 1.25) +
       geom_hline(yintercept = 35, linetype = "dashed") +
       geom_text(
                 aes(x = as.Date("2020/01/01"),
                     y = 35, label = "35ug/m^3 daily NAAQS",
                     hjust = 0.3,
                     vjust = -0.5),
                 data = data.frame()) +
       labs(title = "Measured concentration by date",
            x = "Date",
            y = "Concentration (ug/m^3)"
       ) +
       scale_color_discrete(name = "Site") +
       theme(
            plot.title = element_text(size = 30, face = "bold")) +
       theme_solarized(light = FALSE)

#ggsave("/mnt/c/Users/gregory.bizup/Pictures/ggplot.png", plot = p, width = 15, height = 7, dpi = 1000)
