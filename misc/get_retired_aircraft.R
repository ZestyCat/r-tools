# Adds a column saying whether the aircraft is retired or not

library(data.table)

# 1 column aircraft, 1 column retired status
ac_retired <- fread("/mnt/c/Users/gregory.bizup/Documents/AESO/Noise/84\ noise\ report/power_setting_review.csv",
                    select = c("Aircraft name", "Retired?"),
                    col.names = c("name", "retired"))

# get lsit of all retired
ac_retired <- ac_retired[retired == "Y"]

#data to add column to
data <- fread("~/python/python-tools/noiseplot_generator/data/noisefile.csv")

# Grep for retired aircrft, add colum
df <- data[, retired := grepl(paste(ac_retired$name, collapse = "|"), ac)]

#write
fwrite(df, "~/python/python-tools/noiseplot_generator/data/noisefile_2.csv")
