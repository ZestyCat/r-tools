library(data.table)
library(dplyr)

#fread data (data.table)
data <- fread("data/2021VAERSDATA.csv",
            select = c(
                "VAERS_ID",
                "AGE_YRS",
                "SEX",
                "SYMPTOM_TEXT",
                "DIED",
                "ER_VISIT",
                "HOSPITAL"
            )
        )

vax <- fread("data/2021VAERSVAX.csv",
            select = c(
                "VAERS_ID",
                "VAX_TYPE",
                "VAX_LOT",
                "VAX_DOSE_SERIES",
                "VAX_NAME"
            )
       )

symptoms <- fread("data/2021VAERSSYMPTOMS.csv",
                select = c(
                    "VAERS_ID",
                    "SYMPTOM1",
                    "SYMPTOM2",
                    "SYMPTOM3",
                    "SYMPTOM4",
                    "SYMPTOM5"
                )
            )

#join and filter (dplyr)
v <- data %>%
    left_join(vax, by = "VAERS_ID") %>%
    left_join(symptoms, by = "VAERS_ID") %>%
    filter(VAX_TYPE == "COVID19" & grepl("bald|alopecia|hair loss|hair|beard", SYMPTOM_TEXT, ignore.case = TRUE) == TRUE) %>%
    distinct(VAERS_ID, .keep_all = TRUE)
