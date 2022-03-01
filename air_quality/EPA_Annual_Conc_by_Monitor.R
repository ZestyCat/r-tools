library(tidyverse)

download <- function(year, param = NULL, state = NULL) {
    temp <- tempfile()
    url <- paste("https://aqs.epa.gov/aqsweb/airdata/annual_conc_by_monitor_",
                 year, 
                 ".zip", 
                 sep = "")
    download.file(url, temp)
    con <- unz(temp, paste("annual_conc_by_monitor_",
                                 year,
                                 ".csv",
                                 sep = ""))
    data <- read_csv(con)

    if (!is.null(param)) {
        data <- filter(data, grepl(param, `Parameter Name`, ignore.case = TRUE))
    }
    if (!is.null(state)) {
        data <- filter(data, grepl(state, `State Name`, ignore.case = TRUE))
    }

    unlink(temp)
    return(data)
}
