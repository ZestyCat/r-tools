# Functions for EPA air quality data

library(ggplot2)
library(ggthemes)
library(dplyr)
library(data.table)

open_data <- function(file) {
    data <- fread(file, select = c(1, 5, 6, 3, 8, 12))
    data <- setNames(data, c("date", "conc", "units", "site", "name", "param"))
    data <- data[, date := as.Date(date, format = "%m/%d/%y")]
    data <- data[, site := as.character(site)]
    data
}

site_yearly_mean <- function(aq_data) {
    mean <- group_by(aq_data, site, name) %>%
            summarize(mean_conc = mean(conc)) %>%
            mutate(attainment = ifelse(mean_conc < 13, "Attainment", "Nonattainment"),
                name = ifelse(name == "", site, name)) %>%
            arrange(mean_conc)
    mean
}

bar <- function(table, 
                savedir = "./", 
                savename = "bar", 
                filetype = ".png") {
    ggplot(table) +
        geom_col(aes(x = reorder(name, mean_conc), y = mean_conc, fill = attainment)) +
        geom_hline(yintercept = 13, linetype = "dashed") +
        coord_flip() +
        scale_color_discrete(name = "Attainment") +
        labs(
            title = "Annual mean PM2.5 conentration by site",
            y = "Concentration (ug/m^3)",
            x = ""
        ) +
        theme(
              axis.text.x = element_text(angle = 90),
              axis.text.y = element_text(angle = 90)
        ) +
        theme_solarized()
    ggsave(paste(savedir, 
                 savename, 
                 filetype, 
                 sep = ""), 
           width = 15, 
           height = 7, 
           dpi = 1000)
    print("Plot saved")
}

point <- function(table, 
                savedir = "./", 
                savename = "point", 
                filetype = ".png") {
    ggplot(table) +
        geom_point(aes(x = date, y = conc, color = site), size = 1.25) +
        geom_hline(yintercept = 35, linetype = "dashed") +
        labs(
            title = "Daily mean PM2.5 conentration by date",
            y = "Concentration (ug/m^3)",
            x = ""
        ) +
        theme_bw()
    ggsave(paste(savedir, 
                 savename, 
                 filetype, 
                 sep = ""), 
           width = 15, 
           height = 7, 
           dpi = 1000)
    print("Plot saved")
}
