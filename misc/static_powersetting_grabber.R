library(data.table)
library(tidyverse)

# Writes static power settings from static01.dat into a df/csv

dat <- readLines("~/python/python-tools/noiseplot_generator/Static01.dat")

# Line indeces with AC and Power settings
ac_locs <- unlist(lapply(grep("BAND", dat), function(x) c(x - 2, x - 1)))

lines   <- dat[ac_locs]

l <- list("Aircraft" = c(), "Engine" = c(), "Power" = c(), "Units" = c())

for (i in seq_along(lines)) {
    if (i %% 2 == 1) { # odd lines have ac data
        l[["Aircraft"]] <- c(l[["Aircraft"]], str_squish(substr(lines[i], 1, 20)))
        l[["Engine"]]   <- c(l[["Engine"]],   str_squish(substr(lines[i], 21, 40)))
    } else { # even lines have power setting data
        l[["Power"]]    <- c(l[["Power"]], str_squish(substr(lines[i], 20, 28)))
        l[["Units"]]    <- c(l[["Units"]], str_squish(substr(lines[i], 30, 39)))
    }
}

df <- data.frame(l)
df[sample(nrow(df), 10), ]
#lines[!grepl("MILITARY", lines)] # They all contain "MILITARY" check

#substr(lines, 11, 16)

#l
fwrite(data.frame(l), file = "./static_power_setting_list.csv")
