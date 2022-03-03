library(tidyverse)

# Funtions for fetching environmental data from various sources

# Download EPA annual concentration by monitor based on parameter and state
download_epa <- function(year, param = NULL, state = NULL) {
    temp <- tempfile()
    url  <- paste("https://aqs.epa.gov/aqsweb/airdata/annual_conc_by_monitor_",
                 year, 
                 ".zip", 
                 sep = "")
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


# Download weather data from NCDC based on data parameters
# Fetches from ftp://ftp.ncdc.noaa.gov/pub/data/
download_ncdc <- function(date          = "2022-01-01",
                          icao_callsign = "KNZY",
                          dataset       = 6405,
                          timeframe     = "day") {

    year  <- format(as.Date(date), format = "%Y")
    month <- format(as.Date(date), format = "%m")
    day   <- format(as.Date(date), format = "%d")
    
    get_url <- function(cs = icao_callsign,
                        ds = dataset, 
                        y = year, 
                        m = month) { 
        return(paste(
                "ftp://ftp.ncdc.noaa.gov/pub/data/",
                 ifelse(ds == 6401,          # If 6401,
                        "asos-fivemin",      # return asos-fivemin
                 ifelse(ds == 6405 |         # else if 6405
                        ds == 6406,          # or 6406,
                        "asos-onemin",       # return asos-onemin
                           stop(print_info(ds = "asos",
                                           error   = TRUE)))),
                 "/", ds, "-", y, "/", ds, "0", cs, y, m, ".dat",
                 sep = "")
        )
    }

        if (timeframe == "day") {
               con <- get_url()
               lines <- str_squish(readLines(con)) # Trim excess whitespace
               data  <- read_table(lines, col_names = FALSE)
               return(filter_day(data, icao_callsign, year, month, day))
        } else if (timeframe == "month") {
               con <- get_url()
               lines <- str_squish(readLines(con)) # Trim excess whitespace
               data  <- read_table(lines, col_names = FALSE)
               return(data)
        } else if (timeframe == "year") {
               return(print("Do yearly stuff"))
        } else {
            stop(print("Choose a valid timeframe (day/month/year)"))
        }
}

print_info <- function(dataset = "asos", error = FALSE) {
    msg <- ""
    if (dataset == "asos") {
        msg <- paste(
                "Valid ASOS datasets:\n",
                "1. 6401 (5 minute)\n",
                "2. 6405 (1 minute wind)\n",
                "3. 6406 (1 minute temperature)")
    }
    if (error == TRUE) msg <- paste("\nError: Invalid dataset value.\n", msg)
    return(msg)
}

filter_day <- function(data, icao_callsign, year, month, day) {
    return(
        filter(data, # Filters second column for string like "NZY20200527"
           grepl(paste(
              substr(icao_callsign, 2, 4), year, month, day, sep = ""), d[[2]])
        )
    )
}


