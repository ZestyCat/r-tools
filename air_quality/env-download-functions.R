library(tidyverse)
library(data.table)

# Funtions for fetching environmental data from various sources

# Download EPA annual concentration by monitor based on parameter and state
get_epa <- function(year, param = NULL, state = NULL, county = NULL) {
    temp <- tempfile()
    url  <- paste0("https://aqs.epa.gov/aqsweb/airdata/annual_conc_by_monitor_",
                 year,
                 ".zip")
    download.file(url, temp) # Save data at url into tempfile
    data <- read_csv(temp)

    if (!is.null(param)) {
        data <- filter(data, grepl(param, `Parameter Name`, ignore.case = TRUE))
    }
    if (!is.null(state)) {
        data <- filter(data, grepl(state, `State Name`, ignore.case = TRUE))
    }
    if (!is.null(county)) {
        data <- filter(data, grepl(county, `County Name`, ignore.case = TRUE))
    }

    unlink(temp)
    return(data)
}

# Get first day of each month within specified daterange vector
# collect data for every combination of callsign and d_1
collect_1min <- function(daterange, callsign, ...) {
    d_1 <- unique(round.POSIXt(seq(as.Date(daterange[1]),
                                   as.Date(daterange[2]),
                                   by = 1), units = "months"))
    data <- mapply(function(day, cs) { 
                get_1min(day, cs, range = daterange, ...)},
                as.vector(expand.grid(d_1, callsign)[[1]]),
                as.vector(expand.grid(d_1, callsign)[[2]]),
                SIMPLIFY = FALSE)
    return(rbindlist(data))
}

# Get 6406 and 6405 data url for day/callsign
# trim returned data to the specified date range
# save if true, return
get_1min <- function(day, callsign, range, save = FALSE, file = NULL) {
    urls <- c(get_url(callsign, 6405, day), get_url(callsign, 6406, day))
    data <- trim_by_date(left_join(read_6405(urls[1]), read_6406(urls[2]),
                            by = c("station", "time")), range)
    if (save == TRUE) write_asos(data, file)
    return(data)
}

# Make a vector for every day in date range
# Turn that vector into a | separated regex
# Filter data based on the regex
# Return
trim_by_date <- function(x, daterange) {
    dates    <- seq(as.Date(daterange[1]), as.Date(daterange[2]), by = 1)
    dates_rgx <- paste(paste0(format(dates, format = "%Y%m%d")), collapse = "|")
    fdata    <- filter(x, grepl(dates_rgx, time))
    return(fdata)
}

write_asos <- function(x, filename = NULL) {
    if (is.null(filename))  fwrite(x, "./asos_download.csv", append = TRUE)
    if (!is.null(filename)) fwrite(x, filename, append = TRUE)
}

read_6405 <- function(con) { # Reads ASOS wind data (6405) as fixed width file
    data <- tryCatch({
                        read_fwf(con, fwf_positions(
                                        c(1, 11, 72, 78, 82, 88),
                                        c(9, 30, 74, 79, 84, 89),
                                        c("station", "time",
                                          "2min avg wind dir (deg)",
                                          "2min avg wind speed (kts)",
                                          "5sec avg wind dir (deg)",
                                          "5sec avg wind speed (kts)")))
                      },
                      error = function(cond) {
                            message(paste("404 not found:", con))
                            return(NULL)
                      })
    return(data)
}

read_6406 <- function(con) { # Reads ASOS temp data (6406) as fixed width file
    data <- tryCatch({
                        read_fwf(con, fwf_positions(
                                        c(1, 11, 33, 46, 71, 79, 87, 96, 101),
                                        c(9, 30, 34, 49, 76, 84, 92, 97, 102),
                                        c("station", "time",
                                          "Precipitation",
                                          "Amount (1/100 inch)",
                                          "Pressure 1 (inches Hg)",
                                          "Pressure 2 (inches Hg)",
                                          "Pressure 3 (inches Hg)",
                                          "Dry bulb temp",
                                          "Dew point temp")))
                      },
                      error = function(cond) {
                            message(paste("404 not found:", con))
                            return(NULL)
                      })
    return(data)
}

get_url <- function(cs, ds, date) { # Callsign, dataset, year, month
    y <- format(as.Date(date), format = "%Y")
    m <- format(as.Date(date), format = "%m")
    return(paste0("https://www.ncei.noaa.gov/pub/data/",
             ifelse(ds == 6401, "asos-fivemin",
             ifelse(ds == 6405 | ds == 6406, "asos-onemin",
             stop(print_info(ds = "asos", error = TRUE)))),
             "/", ds, "-", y, "/", ds, "0", cs, y, m, ".dat")
    )
}

read_station_list <- function(wban = NULL, cs = NULL,
                              state = NULL, county = NULL) {
    con <- "https://www.ncei.noaa.gov/pub/data/ASOS_Station_Photos/asos-stations.txt"
    temp <- tempfile()
    download.file(con, temp) # Save data at url into tempfile
    data <- read_fwf(temp, skip = 2) %>%
            rename_at(vars(1:14), function(x) c("NCDCID", "WBAN",
                                                "COOPID", "CALL",
                                                "NAME", "ALT_NAME",
                                                "COUNTRY", "STATE",
                                                "COUNTY", "LAT", "LON",
                                                "ELEV", "UTC", "TRNTYPE")) %>%
            slice(-c(1, 2)) %>%
            mutate(CALL = paste0("K", CALL))

    if (!is.null(wban)) {
        data <- data %>% filter(WBAN == wban)
    }
    if (!is.null(cs)) {
        data <- data %>% filter(CALL == cs)
    }
    if (!is.null(state)) {
        data <- data %>% filter(STATE == state)
    }
    if (!is.null(county)) {
        data <- data %>% filter(COUNTY == county)
    }
    
    unlink(temp)
    return(data)
}

return_callsign_vector <- function(...) {
    data <- read_station_list(...)
    return(select(data, CALL)[[1]])
}
