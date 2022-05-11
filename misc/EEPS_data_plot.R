library(tidyverse)

df <- read_csv("../data/EEPS_comparison.csv")

mean_pct_diff <- df %>% summarise(mean(`% Difference`))

g <- ggplot(df) +
    geom_point(aes(x = `Channel Size [nm]`, y = `dN/log(dP) 2014 EEPS`, color = 'blue')) + 
    geom_point(aes(x = `Channel Size [nm]`, y = `dN/log(dP) 2005 EEPS`, color = 'red')) +
    geom_line(aes(x = `Channel Size [nm]`, y = `dN/log(dP) 2014 EEPS`, color = 'blue')) + 
    geom_line(aes(x = `Channel Size [nm]`, y = `dN/log(dP) 2005 EEPS`, color = 'red')) +
    scale_color_discrete(name = "EEPS Year", labels = c(2014, 2005)) +
    coord_cartesian(ylim = c(0, 6500000), clip = "off") +
    annotate("text", x = 400, y = 7000000, hjust = 0, label = paste(round(mean_pct_diff, 2), "% Difference")) +
    labs(title = "2005 and 2014 EEPS Comparison\nAtomized Tap Water PSD") +
    ylab("dN / log(dP)") +
    theme_bw()

ggsave("/mnt/c/Users/gregory.bizup/Documents/AESO/Instruments/EEPS/Comparison_ggplot.png", g)
