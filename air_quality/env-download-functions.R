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

# Download weather data from NCDC for specified parameters
# Fetches from ftp://ftp.ncdc.noaa.gov/pub/data/
# dates: date range to be extracted c(YYYY-MM-DD, YYYY-MM-DD)
# cs: ICAO Callsign e.g. "KNZY", or vector of cs e.g. c("KNZY", "KHSV")
# ds: Dataset (e.g. 6405, 6406, 6401)
# Makes a sequence of dates, makes url list, reads each url, binds data
# Filters where the date is not equal to the last day
get_asos <- function(dates, cs, ...) {
    dates <- unique(round.POSIXt(seq(as.Date(dates[1]),
                                     as.Date(dates[2]),
                                     by = 1),
                    "months"))
    d <- lapply(dates, function(x) {
                    trim_and_write(read_6405_6406(x, cs))
                 })


    return(d)
}

read_6405_6406 <- function(date, cs, save = FALSE, ...) {
    url_6405 <- get_url(cs, 6405, as.Date(date))
    url_6406 <- get_url(cs, 6406, as.Date(date))
    d_6405   <- read_6405(url_6405)
    d_6406   <- read_6406(url_6406)
    data     <- left_join(d_6405, d_6406, by = c("station", "time"))
    tdata    <- trim_by_date(data, dates = ...)
    
    if (save == TRUE) write_asos(tdata, filename = ...)

    return(tdata)
}

# Make a vector for every day in date range
# Turn that vector into a | separated regex
# Filter data based on the regex
# Write, return
trim_by_date <- function(x, dates) {
    dates    <- seq(as.Date(dates[1]), as.Date(dates[2]), by = 1)
    dates_rx <- paste0(paste0(format(dates, format = "%Y%m%d")), collapse = "|")
    fdata    <- x %>% filter(grepl(dates_rx, time))
    return(fdata)
}

write_asos <- function(x, filename = NULL) {
    ifelse(is.null(filename),
        fwrite(x, "./asos_download.csv", append = TRUE),
        fwrite(x, filename, append = TRUE))
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
    y <- format(date, format = "%Y")
    m <- format(date, format = "%m")
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
