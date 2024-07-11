library(tidyverse)

lw_data <- readRDS('Data/bottomTrawlSurvey_indLengthWeight.rds')
lw_coeffs <- readRDS('Data/lw_coeffs.rds')

unique_comname <- unique(lw_data$COMNAME)
unique_comname <- na.omit(unique_comname)

data <- lw_data %>%
  filter(!is.na(LENGTH) & !is.na(INDWT) & !is.na(COMNAME)) %>%
  mutate(decade = case_when(YEAR >= 2014 ~ "2014 - 2023",
                            YEAR >= 2003 & YEAR < 2014 ~ "2003 - 2013",
                            YEAR <= 2002 ~ "1992 - 2002"))

grouped_data <- data %>%
  group_by(COMNAME, decade) %>%
  summarise(count = n())

filtered_groups <- grouped_data %>%
  filter(count >= 5) %>%
  ungroup()

unique_species <- unique(filtered_groups$COMNAME)
species <- c()

for (spec in unique_species) {
  spec_decs <- filtered_groups %>%
    filter(COMNAME == spec)
  if (nrow(spec_decs) == 3) {
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

####

coeffs.by.decade.df <- data.frame(row.names = species)
decades = c('1992-2002', '2000s', '2010s', '2020s')
seasons = c('SPRING', 'SUMMER', 'FALL', 'WINTER')

# filtering out large outliers
data <- data %>%
  filter(! (COMNAME == "AMERICAN PLAICE" & INDWT > 4) ) %>%
  filter(! (COMNAME == "ATLANTIC HERRING" & INDWT > 0.5)) %>%
  filter(! (COMNAME == "ATLANTIC SHARPNOSE SHARK" & LENGTH > 106)) %>%
  ## bluntnose stingray - says they can be up to 100 cm. a few observations are close/over
  filter(! (COMNAME == "BLUNTNOSE STINGRAY" & INDWT > 50)) %>%
  filter(! (COMNAME == "BLUNTNOSE STINGRAY" & INDWT > 10 & LENGTH < 10)) %>%
  # just some low observations... not sure if these are errors
  filter(! (COMNAME == "BUTTERFISH" & INDWT <= 0.01 & LENGTH > 12.5)) %>%
  filter(! (COMNAME == "LITTLE SKATE" & INDWT > 4)) %>%
  filter(! (COMNAME == "LONGFIN SQUID" & INDWT > 1)) %>%
  filter(! (COMNAME == "LONGHORN SCULPIN" & INDWT > 750)) %>%
  filter(! (COMNAME == "NORTHERN KINGFISH" & INDWT > 1)) %>%
  filter(! (COMNAME == "NORTHERN SHORTFIN SQUID" & INDWT > 1)) %>%
  filter(! (COMNAME == "WINTER FLOUNDER" & LENGTH < 20 & INDWT > 3))

for (d in decades) {
  for (s in seasons) {
    coeffs.by.decade.df[[paste0("B.", s, ".", d)]] <- NA
    coeffs.by.decade.df[[paste0("A.", s, ".", d)]] <- NA
  }
}


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
