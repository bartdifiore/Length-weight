library(tidyverse)

lw_data <- readRDS('Data/bottomTrawlSurvey_indLengthWeight.rds')
lw_coeffs <- readRDS('Data/lw_coeffs.rds')

unique_comname <- unique(lw_data$COMNAME)
unique_comname <- na.omit(unique_comname)

print(unique_comname)

# Want to restrict DF rows (COMNAMES) to only species that have at least 5 observations in one decade
  # loop through species, collect decade counts, pull if < 5?
  # should have 5 observations in every decade

#### trying
data <- lw_data %>%
  filter(!is.na(LENGTH) & !is.na(INDWT) & !is.na(COMNAME)) %>%
  mutate(decade = case_when(YEAR >= 2020 ~ "2020s",
                            YEAR >= 2010 & YEAR < 2020 ~ "2010s",
                            YEAR >= 2000 & YEAR < 2010 ~ "2000s", 
                            YEAR < 2000 ~ "1990s"))

grouped_data <- data %>%
  group_by(COMNAME, decade) %>%
  summarise(count = n())

filtered_groups <- grouped_data %>%
  filter(count >= 5) %>%
  ungroup()
View(filtered_groups)

unique_species <- unique(filtered_groups$COMNAME)
species <- c()

for (spec in unique_species) {
  spec_decs <- filtered_groups %>%
    filter(COMNAME == spec)
  if (nrow(spec_decs) == 4) {
    species <- c(species, spec)
  }
}

season_counts <- data %>%
  filter(COMNAME %in% species) %>% 
  filter(!is.na(SEASON)) %>%             
  group_by(COMNAME, decade, SEASON) %>%
  summarise(count = n()) %>%
  filter(count >= 5) %>%
  ungroup()

View(season_counts)
####


coeffs.by.decade.df <- data.frame(row.names = species)
decades = c('1990s', '2000s', '2010s', '2020s')
seasons = c('SPRING', 'SUMMER', 'FALL', 'WINTER')

for (d in decades) {
  for (s in seasons) {
    coeffs.by.decade.df[[paste0("B.", s, ".", d)]] <- NA
    coeffs.by.decade.df[[paste0("A.", s, ".", d)]] <- NA
  }
}

print(coeffs.by.decade.df)

for (spec in species) {
  spec_data <- lw_data %>%
    filter(COMNAME == spec) %>%
    filter(INDWT != 0) %>%
    mutate(decade = case_when(
      YEAR >= 2020 ~ "2020s",
      YEAR >= 2010 & YEAR < 2020 ~ "2010s",
      YEAR >= 2000 & YEAR < 2010 ~ "2000s", 
      YEAR < 2000 ~ "1990s"))
  
  for (dec in decades) {
    for (szn in seasons) {
      season_data <- spec_data %>%
        filter(SEASON == szn & decade == dec)
      if (nrow(season_data) >= 5) {
        spec.szn.lm <- lm(log(INDWT) ~ log(LENGTH), data = season_data)
        coeffs.by.decade.df[spec, paste0("B.", szn, ".", dec)] <- coef(spec.szn.lm)[2]
        coeffs.by.decade.df[spec, paste0("A.", szn, ".", dec)] <- coef(spec.szn.lm)[1]
      }
    }
  }
}

View (coeffs.by.decade.df)
