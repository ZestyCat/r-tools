library(tidyverse)

df <- read_csv("/mnt/c/Users/AESO 1/Documents/AEDT/AESO_vs_AEDT_LTO.csv")



aedt <- df %>% filter(Method == "AEDT") 
icao <- df %>% filter(Method == "ICAO")

# Change from AEDT to ICAO
chg <- (aedt[3:9] - icao[3:9]) / icao[3:9]

chg <- chg %>%
	mutate("Equipment Type" = pull(aedt, `Equipment Type`)) %>%
	relocate(`Equipment Type`, .before = 1) %>%
	pivot_longer(cols = c(3:8), names_to = "Species", values_to = "change")

names <- c("CO", "CO2", "NOx", "PM 10", "PM 2.5", "THC")

g <- ggplot(chg) +
	geom_col(
		aes(fill = `Species`, x = `Equipment Type`, y = `change`),
		position = position_dodge(0.4),
		width = 0.2, ) +
	labs(title = "LTO Emission Relative Changes from ICAO to AEDT",
	     fill = "Species") +
	xlab("") +
	ylab("Relative change") +
	theme_classic() +
	theme(axis.text.x = element_text(angle = 45, vjust = 0.95, hjust=1),
	      legend.key.size = unit(0.7, "lines"),
	      legend.title = element_text(size = 10),
	      legend.text = element_text(size = 8)
	) +
	geom_hline(yintercept = 0, linetype = 2) +
	scale_fill_grey(labels = names)

ggsave("/mnt/c/Users/AESO 1/Pictures/AEDT_Comparison.png", g)

chg
