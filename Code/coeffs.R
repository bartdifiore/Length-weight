library(tidyverse)

lw_data <- readRDS('Desktop/GMRI/data/bottomTrawlSurvey_indLengthWeight.rds')
lw_coeffs <- readRDS('Desktop/GMRI/data/lw_coeffs.rds')

unique_comname <- unique(lw_data$COMNAME)
unique_comname <- na.omit(unique_comname)

# Want to restrict DF rows (COMNAMES) to only species that have at least 5 observations in one decade
  # loop through species, collect decade counts, pull if < 5?

coeffs.by.decade.df <- data.frame(row.names = unique_comname)
decades = c('1990s', '2000s', '2010s', '2020s')
seasons = c('SPRING', 'SUMMER', 'FALL', 'WINTER')

for (d in decades) {
  for (s in seasons) {
    coeffs.by.decade.df[[paste0("B.", s, ".", d)]] <- NA
    coeffs.by.decade.df[[paste0("A.", s, ".", d)]] <- NA
  }
}

for (species in unique_comname) {
  spec_data <- lw_data %>%
    filter(COMNAME == species) %>%
    filter(INDWT != 0) %>%
    mutate(decade = case_when(
      YEAR >= 2020 ~ "2020s",
      YEAR >= 2010 & YEAR < 2020 ~ "2010s",
      YEAR >= 2000 & YEAR < 2010 ~ "2000s", 
      YEAR < 2000 ~ "1990s"
    ))
  
  for (dec in decades) {
    for (szn in seasons) {
      season_data <- spec_data %>%
        filter(SEASON == szn & decade == dec)
      
      if (nrow(season_data) >= 5) {
        spec.szn.lm <- lm(log(INDWT) ~ log(LENGTH), data = season_data)
        coeffs.by.decade.df[species, paste0("B.", szn, ".", dec)] <- coef(spec.szn.lm)[2]
        coeffs.by.decade.df[species, paste0("A.", szn, ".", dec)] <- coef(spec.szn.lm)[1]
      }
    }
  }
}

