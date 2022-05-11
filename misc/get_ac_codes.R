library(data.table)
library(tidyverse)

#lines <- readLines("./Old-to-New Aircraft Code Table.txt")

#table <- data.table(lines[unlist(lapply(lines, function(l) !grepl("#", l[1])))])

#fwrite(table, file = "ac_codes.txt")

codes <- read_fwf("ac_codes.txt") %>% 
         select(!c(X1, X2, X6, X8, X9, X7, X10)) %>% 
         rename(code = X3, aircraft = X4, engine = X5) %>%
         slice(2:nrow(codes))

codes
