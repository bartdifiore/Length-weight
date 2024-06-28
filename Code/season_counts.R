library(tidyverse)
lw_data <- readRDS('Desktop/GMRI/data/bottomTrawlSurvey_indLengthWeight.rds')
## file to determine how to sort each species!
filtered_data <- lw_data %>%
  filter(!is.na(LENGTH) & !is.na(INDWT)) %>%
  filter(YEAR < 2000)

grouped_data <- filtered_data %>%
  group_by(COMNAME) %>%
  summarise(count = n())
filtered_groups <- grouped_data %>%
  filter(count >= 5 & !is.na(COMNAME))

unique_species <- filtered_groups$COMNAME

season_counts <- filtered_data %>%
  filter(COMNAME %in% unique_species) %>% 
  filter(!is.na(SEASON)) %>%             
  group_by(COMNAME, SEASON) %>%
  summarise(count = n()) %>%
  ungroup()

print(season_counts, n=262)
