library(data.table)

# Writes static power settings from static01.dat into a df/csv

dat <- readLines("~/python/python-tools/noiseplot_generator/Static01.dat")

# Line indeces with AC and Power settings
ac_locs <- unlist(lapply(grep("BAND", dat), function(x) c(x - 2, x - 1)))

lines   <- dat[ac_locs]

l <- list("Aircraft" = c(), "Power"= c())

for (i in seq_along(lines)) {
    if (i %% 2 == 1) { # odd lines have ac data
        l[["Aircraft"]] <- c(l[["Aircraft"]], str_squish(substr(lines[i], 1, 20)))
    } else { # even lines have power setting data
        l[["Power"]] <- c(l[["Power"]], str_squish(substr(lines[i], 20, 28)))
    }
}

fwrite(data.frame(l), file = "~/python/python-tools/noiseplot_generator/data/static_power_setting_list.csv")
