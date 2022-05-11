library(data.table)
library(stringr)

df <- fread("~/python/python-tools/noiseplot_generator/data/noisefile.csv")

pwr <- df[, power]

u <- unique(pwr)


