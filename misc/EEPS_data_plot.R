library(tidyverse)

df <- read_csv("../data/Comparison/2014_2005_comparison.csv")

df <- df %>% group_by(`EEPS Model`) %>%
             summarise_all(list(mean)) %>%
             pivot_longer(!`EEPS Model`, names_to = "Diameter (nm)", values_to = "dN / log(dP)")

d_05 <- df %>% filter(`EEPS Model` == "2005 EEPS") %>% pull(`dN / log(dP)`)
d_14 <- df %>% filter(`EEPS Model` == "2014 EEPS") %>% pull(`dN / log(dP)`)
mean_pct_diff = abs(mean((d_14 - d_05) / d_05) * 100)
mean_pct_diff

g <- ggplot(df) +
    geom_point(aes(x = as.numeric(`Diameter (nm)`), y = `dN / log(dP)`, color = `EEPS Model`)) + 
    geom_line(aes(x = as.numeric(`Diameter (nm)`), y = `dN / log(dP)`, color = `EEPS Model`)) + 
    coord_cartesian(ylim = c(0, 6500000), clip = "off") +
    #annotate("text", x = 399, y = 7000000, hjust = 0, label = paste(round(mean_pct_diff, 2), "% Difference")) +
    labs(title = "2005 and 2014 EEPS Comparison\nAtomized Tap Water PSD") +
    ylab("dN / log(dP)") +
    xlab("Diameter (nm)") +
    theme_bw()

g
ggsave("/mnt/c/Users/gregory.bizup/Documents/AESO/Instruments/EEPS/5-18-22_comparison.png", g)
