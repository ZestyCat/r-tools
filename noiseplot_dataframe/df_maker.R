library(data.table)
library(tidyverse)

# Parses static01.dat into vactors. See NMAP Static01.dat file format reference
read_static01 <- function(file = "./Static01.dat") {
        Static01 <- read_lines(file)

        table_locs <- grep("MILITARY|CIVILIAN", Static01)

        power_code <- unlist(lapply(
                                str_split(Static01[table_locs], " "),
                                function(x) x[2]))

        interpolation <- unlist(lapply(power_code, function(x) {
                ifelse(grepl("F", x), "FIXED", "VARIABLE")
        }))

        aircraft_code <- substr(power_code, 2, 7) 
        aircraft <- str_squish(substr(Static01[table_locs + 1], 1, 20))
        engine <- str_squish(substr(Static01[table_locs + 1], 21, 40))

        description <- str_squish(substr(Static01[table_locs + 2], 1, 20))

        power_1 <- str_squish(substr(Static01[table_locs + 2], 21, 29))
        power_2 <- str_squish(substr(Static01[table_locs + 2], 41, 49))
        power_3 <- str_squish(substr(Static01[table_locs + 2], 61, 69))

        unit_1 <- str_squish(substr(Static01[table_locs + 2], 31, 40))
        unit_2 <- str_squish(substr(Static01[table_locs + 2], 51, 60))
        unit_3 <- str_squish(substr(Static01[table_locs + 2], 71, 80))

        tibble(aircraft, aircraft_code, engine, power_code, description,
               power_1, unit_1, power_2, unit_2, power_3, unit_3,
               interpolation, operation_type = "STATIC")
}

read_flight01 <- function(file = "./Flight01.dat") {
        Flight01 <- read_lines(file)

        table_locs <- grep("MILITARY|CIVILIAN", Flight01)
        
        power_code <- unlist(lapply(
                                str_split(Flight01[table_locs], " "),
                                function(x) x[2]))

        interpolation <- unlist(lapply(power_code, function(x) {
                vec <- str_split(x, "")
                code <- tail(vec[[1]], n = 1)
                ifelse(code == "F", "FIXED",
                ifelse(code == "P", "PARALLEL",
                ifelse(code == "V", "VARIABLE", NULL)))
        }))

        aircraft_code <- substr(power_code, 2, 7) 
        aircraft <- str_squish(substr(Flight01[table_locs + 1], 1, 20))
        engine <- str_squish(substr(Flight01[table_locs + 1], 21, 40))

        description <- str_squish(substr(Flight01[table_locs + 2], 1, 20))

        power_1 <- str_squish(substr(Flight01[table_locs + 2], 21, 29))
        power_2 <- str_squish(substr(Flight01[table_locs + 2], 60, 68))
        power_3 <- str_squish(substr(Flight01[table_locs + 2], 99, 107))

        unit_1 <- str_squish(substr(Flight01[table_locs + 2], 31, 40))
        unit_2 <- str_squish(substr(Flight01[table_locs + 2], 70, 79))
        unit_3 <- str_squish(substr(Flight01[table_locs + 2], 109, 118))

        tibble(aircraft, aircraft_code, engine, power_code, description,
               power_1, unit_1, power_2, unit_2, power_3, unit_3,
               interpolation, operation_type = "FLIGHT")
}

static_df <- read_static01()
flight_df <- read_flight01()

static_df

df <- rbindlist(list(static_df, flight_df))
fwrite(df, file = "./operation_data.csv")
