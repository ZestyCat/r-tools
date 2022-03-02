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
                          datasets      = 6401) {
    data <- list()
    for (i in seq_along(datasets)) {
        con <- paste(
                    "ftp://ftp.ncdc.noaa.gov/pub/data/",
                     ifelse(datasets[i] == 6401,     # If 6401,
                            "asos-fivemin",          # return asos-fivemin
                     ifelse(datasets[i] == 6405 |    # else if 6405 or 6406,
                            datasets[i] == 6406,     # or 6406
                            "asos-onemin",           # return asos-onemin
                               stop(print_info(dataset = "asos",
                                               error   = TRUE)))),
                     "/",
                     datasets[i], "-",
                     format(as.Date(date), format = "%Y"), "/",
                     datasets[i], "0",
                     icao_callsign,
                     format(as.Date(date), format = "%Y"),
                     format(as.Date(date), format = "%m"),
                     ".dat",
                     sep = "")

        lines <- str_squish(readLines(con))

        if (length(datasets) == 1) { # If only one dataset, return tibble
            data  <- read_table(lines, col_names = FALSE)
            return(data)
        } else {                     # If multiple datasets, return as list
            data[[i]] <- read_table(lines, col_names = FALSE)
        }
    }
    return(data)
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

d <- download_ncdc()
