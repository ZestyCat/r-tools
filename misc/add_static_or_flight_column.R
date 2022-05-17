library(tidyverse)
library(data.table)

# Adds columns specifying which aircraft are in the static and flight databases

acdata <- read_csv("~/python/python-tools/noiseplot_generator/data/acdata.csv")
static <- read_csv("~/python/python-tools/noiseplot_generator/data/static_power_setting_list.csv")

static <- static %>% distinct(Code, Aircraft) # Get list of all static aircraft

acdata <- acdata %>% 
    mutate(flight = TRUE) %>% # All entries thus far are in flight01
    mutate(static = is.element(aircraft, static_ac)) %>%
    add_row(aircraft = static[["Aircraft"]][unlist(lapply(static[["Aircraft"]], function(e) !is.element(e, acdata[["aircraft"]])))],
        flight = FALSE, static = TRUE # Find all static ac that are not elements of flight data
    )

    tail(acdata)
#fwrite(acdata, file = "~/python/python-tools/noiseplot_generator/data/acdata.csv")
