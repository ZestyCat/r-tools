library(tidyverse)

df <- read_csv("./data/ALL_AESO_LTO.csv")

df <- df %>% group_by(`Equipment Type`) %>% 
             filter(`Altitude AFE (ft)` < 3000) %>%
             summarise(
              	 "LTO Fuel Burn (lb)" = sum(`Fuel Burn (lb)`),
#              	 "LTO CO (lb)" = sum(`CO (lb)`),
              	 "LTO NOx (lb)" = sum(`NOx (lb)`),
              	 "LTO THC (lb)" = sum(`THC (lb)`)
#              	 "LTO PM 10 (lb)" = sum(`PM 10 (lb)`),
#              	 "LTO PM 2.5 (lb)" = sum(`PM 2.5 (lb)`),
#              	 "LTO CO2 (lb)" = sum(`CO2 (lb)`)
             )

#write_csv(df, "/mnt/c/Users/AESO 1/Documents/AEDT/AEDT_LTO_SUMS.csv")

df
