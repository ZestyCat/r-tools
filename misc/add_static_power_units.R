library(data.table)
library(tidyverse)

sp <- fread("./static_power_setting_list.csv")
ac <- fread("~/python/python-tools/noiseplot_generator/data/acdata.csv")

empty <- ac[engine == "" | units == ""][, aircraft]
add   <- unique(sp[Aircraft %in% empty][, .(Aircraft, Code, Engine, Units, flight = FALSE, static = TRUE)])

#rbindlist(list(ac[engine != "" & units != ""], add))
ac <- setkey(ac, aircraft)[add, engine := i.Engine][add, units := i.Units][add, code := i.Code][, !c(1)]

fwrite(ac, file = "~/python/python-tools/noiseplot_generator/data/acdata.csv")

# Need to get code/engine/units for static only aircraft
