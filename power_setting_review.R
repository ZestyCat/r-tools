library(dplyr)
library(readr)

data <- read_csv("data/power_setting_review.csv")
    
ret_list <- data %>%
    filter(nchar(`Aircraft name`) > 2) %>%
    group_by(`Aircraft name`) %>%
    summarize(retired = unique(`Retired?`))

count_ret <- ret_list %>%
    group_by(retired) %>%
    count(retired)

ret_list
count_ret
