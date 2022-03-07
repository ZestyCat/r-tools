library(tidyverse)
library(data.table)

# Funtions for fetching environmental data from various sources

# Download EPA annual concentration by monitor based on parameter and state
get_epa <- function(year, param = NULL, state = NULL) {
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

    unlink(temp)
    return(data)
}

# Download weather data from NCDC for specified parameters
# Fetches from ftp://ftp.ncdc.noaa.gov/pub/data/
# dates: date range to be extracted c(YYYY-MM-DD, YYYY-MM-DD)
# cs: ICAO Callsign (e.g. "KNZY")
# ds: Dataset (e.g. 6405, 6406, 6401)
# Makes a sequence of dates, makes url list, reads each url, binds data
# Filters where the date is not equal to the last day
get_asos <- function(dates, cs, download = FALSE) {
    dates <- seq(as.Date(dates[1]), as.Date(dates[2]), by = 1)


    search_regex <- paste0( # Make a "|" separated regex of every date in range
                       paste0(substr(cs, 2, 4), # Format (e.g. NZY20200615)
                              format(dates, format = "%Y%m%d")),
                       collapse = "|") # Separate vector with "|"

    d6405 <- rbindlist(
                lapply(
                    unique(get_url(cs, 6405, dates)),
                trim_read),
              fill = TRUE) %>%
            filter(grepl(search_regex, X2)) %>%
            mutate(X2 = as.POSIXct(substr(X2, 4, 17), format = "%Y%m%d%H%M"))

    d6406 <- rbindlist(
                lapply(
                    unique(get_url(cs, 6406, dates)),
                trim_read),
             fill = TRUE) %>%
            filter(grepl(search_regex, X2)) %>%
            mutate(X2 = as.POSIXct(substr(X2, 4, 17), format = "%Y%m%d%H%M"))

    if (download == TRUE) {
        fwrite(d6405, paste0("./", "6405", cs, dates[1], dates[2], ".csv"))
        fwrite(d6406, paste0("./", "6406", cs, dates[1], dates[2], ".csv"))
    }

    return(list(d6405 = d6405, d6406 = d6406))

}

trim_read <- function(con) {
       lines <- str_squish(readLines(con)) # Trim excess whitespace
       data  <- read_table(lines, col_names = FALSE)
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
