library(data.table)
library(dplyr)

# Take all the airraft noise data files and make them into one

reframe <- function(file) {
    data <- fread(file)
    info <- list(ac = data[1, 1], eng = data[1, 2], power = data[1, 3]) 
    df <- if (ncol(data) == 9 & nrow(data) == 25) {
            data[-c(1:3), ] %>%
            mutate(ac = info$ac[[1]],
                   eng = info$eng[[1]],
                   power = info$power[[1]]) %>%
            relocate(c(ac, eng, power)) %>%
            select(1:5, 9) %>%
            rename(`Dist ft.` = 4,
                   `SEL A-G (dB)` = 5,
                   `ALM A-G (dB)` = 6) 
           } else { NULL }
    return(df)
}

concatenate <- function(dir = getwd(), file = "./cat-noisefile.csv") {
    files = dir(dir)
    df <- rbindlist(lapply(files, reframe), fill = TRUE)
    fwrite(df, file = file)
}

concatenate()
