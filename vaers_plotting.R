library(data.table)
library(ggplot2)

vaers_hairs <- fread("data/vaers_hairs.csv")

ggplot(vaers_hairs) +
    geom_bar(aes(VAX_NAME))
