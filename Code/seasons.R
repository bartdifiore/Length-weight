library(tidyverse)
lw_data <- readRDS('Desktop/GMRI/data/bottomTrawlSurvey_indLengthWeight.rds')

### 1990s

filtered_data <- lw_data %>%
  filter(!is.na(LENGTH) & !is.na(INDWT)) %>%
  filter(YEAR < 2000)

grouped_data <- filtered_data %>%
  group_by(COMNAME) %>%
  summarise(count = n())
filtered_groups <- grouped_data %>%
  filter(count >= 5 & !is.na(COMNAME))

unique_species <- filtered_groups$COMNAME
nineties <- c('1990s Combined', '1990s Spring', '1990s Summer', '1990s Winter', '1990s Fall')

b.by.season <- matrix(nrow = length(unique_species), ncol = length(nineties), byrow = TRUE)
rownames(b.by.season) <- unique_species
colnames(b.by.season) <- nineties

a.by.season <- matrix(nrow = length(unique_species), ncol = length(nineties), byrow = TRUE)
rownames(a.by.season) <- unique_species
colnames(a.by.season) <- nineties

for (species in unique_species) {

  data <- lw_data %>%
    filter(COMNAME == species) %>%
    filter(INDWT != 0) %>%
    mutate(season = case_when(
      YEAR < 2000 ~ '1990s Combined',
      SEASON == 'SPRING' ~ "1990s Spring",
      SEASON == 'SUMMER' ~ "1990s Summer", 
      SEASON == 'WINTER' ~ "1990s Winter",
      SEASON == 'FALL' ~ "1990s Fall"))
  
  combined_lm <- lm(log(INDWT) ~ log(LENGTH), data)
  a.by.season[species, '1990s Combined'] <- coef(combined_lm)[1]
  b.by.season[species, '1990s Combined'] <- coef(combined_lm)[2]
  
  for (seas in nineties) {
    seas_data <- data %>%
      filter(season == seas)
    
    if (nrow(seas_data) > 0) {
      seas_lm <- lm(log(INDWT) ~ log(LENGTH), seas_data)
      a.by.season[species, seas] <- coef(seas_lm)[1]
      b.by.season[species, seas] <- coef(seas_lm)[2]
    }
  }
}

b_by_90season_df <- as.data.frame(b.by.season, stringsAsFactors = FALSE)
View(b_by_90season_df)
write.csv(b_by_90season_df, file = "~/Desktop/1990sData.csv", row.names = TRUE)

### 2000s

filtered_data <- lw_data %>%
  filter(!is.na(LENGTH) & !is.na(INDWT)) %>%
  filter(YEAR >= 2000 & YEAR < 2010)

grouped_data <- filtered_data %>%
  group_by(COMNAME) %>%
  summarise(count = n())
filtered_groups <- grouped_data %>%
  filter(count >= 5 & !is.na(COMNAME))

unique_species <- filtered_groups$COMNAME
thousands <- c('2000s Combined', '2000s Spring', '2000s Summer', '2000s Winter', '2000s Fall')

b.by.season <- matrix(nrow = length(unique_species), ncol = length(thousands), byrow = TRUE)
rownames(b.by.season) <- unique_species
colnames(b.by.season) <- thousands

a.by.season <- matrix(nrow = length(unique_species), ncol = length(thousands), byrow = TRUE)
rownames(a.by.season) <- unique_species
colnames(a.by.season) <- thousands

for (species in unique_species) {
  
  data <- lw_data %>%
    filter(COMNAME == species) %>%
    filter(INDWT != 0) %>%
    mutate(season = case_when(
      YEAR >= 2000 & YEAR < 2010 ~ '2000s Combined',
      SEASON == 'SPRING' ~ "2000s Spring",
      SEASON == 'SUMMER' ~ "2000s Summer", 
      SEASON == 'WINTER' ~ "2000s Winter",
      SEASON == 'FALL' ~ "2000s Fall"))
  
  combined_lm <- lm(log(INDWT) ~ log(LENGTH), data)
  a.by.season[species, '2000s Combined'] <- coef(combined_lm)[1]
  b.by.season[species, '2000s Combined'] <- coef(combined_lm)[2]
  
  for (seas in thousands) {
    seas_data <- data %>%
      filter(season == seas)
    
    if (nrow(seas_data) > 0) {
      seas_lm <- lm(log(INDWT) ~ log(LENGTH), seas_data)
      a.by.season[species, seas] <- coef(seas_lm)[1]
      b.by.season[species, seas] <- coef(seas_lm)[2]
    }
  }
}

b_by_2000season_df <- as.data.frame(b.by.season, stringsAsFactors = FALSE)
write.csv(b_by_2000season_df, file = "~/Desktop/2000sData.csv", row.names = TRUE)

## 2010s

filtered_data <- lw_data %>%
  filter(!is.na(LENGTH) & !is.na(INDWT)) %>%
  filter(YEAR >= 2010 & YEAR < 2020)

grouped_data <- filtered_data %>%
  group_by(COMNAME) %>%
  summarise(count = n())
filtered_groups <- grouped_data %>%
  filter(count >= 5 & !is.na(COMNAME))

unique_species <- filtered_groups$COMNAME
tens <- c('2010s Combined', '2010s Spring', '2010s Summer', '2010s Winter', '2010s Fall')

b.by.season <- matrix(nrow = length(unique_species), ncol = length(tens), byrow = TRUE)
rownames(b.by.season) <- unique_species
colnames(b.by.season) <- tens

a.by.season <- matrix(nrow = length(unique_species), ncol = length(tens), byrow = TRUE)
rownames(a.by.season) <- unique_species
colnames(a.by.season) <- tens

for (species in unique_species) {
  
  data <- lw_data %>%
    filter(COMNAME == species) %>%
    filter(INDWT != 0) %>%
    mutate(season = case_when(
      YEAR >= 2010 & YEAR < 2020 ~ '2010s Combined',
      SEASON == 'SPRING' ~ "2010s Spring",
      SEASON == 'SUMMER' ~ "2010s Summer", 
      SEASON == 'WINTER' ~ "2010s Winter",
      SEASON == 'FALL' ~ "2010s Fall"))
  
  combined_lm <- lm(log(INDWT) ~ log(LENGTH), data)
  a.by.season[species, '2010s Combined'] <- coef(combined_lm)[1]
  b.by.season[species, '2010s Combined'] <- coef(combined_lm)[2]
  
  for (seas in tens) {
    seas_data <- data %>%
      filter(season == seas)
    
    if (nrow(seas_data) > 0) {
      seas_lm <- lm(log(INDWT) ~ log(LENGTH), seas_data)
      a.by.season[species, seas] <- coef(seas_lm)[1]
      b.by.season[species, seas] <- coef(seas_lm)[2]
    }
  }
}

b_by_2010season_df <- as.data.frame(b.by.season, stringsAsFactors = FALSE)
write.csv(b_by_2010season_df, file = "~/Desktop/2010sData.csv", row.names = TRUE)

## 2020s

filtered_data <- lw_data %>%
  filter(!is.na(LENGTH) & !is.na(INDWT)) %>%
  filter(YEAR >= 2020)

grouped_data <- filtered_data %>%
  group_by(COMNAME) %>%
  summarise(count = n())
filtered_groups <- grouped_data %>%
  filter(count >= 5 & !is.na(COMNAME))

unique_species <- filtered_groups$COMNAME
twenties <- c('2020s Combined', '2020s Spring', '2020s Summer', '2020s Winter', '2020s Fall')

b.by.season <- matrix(nrow = length(unique_species), ncol = length(twenties), byrow = TRUE)
rownames(b.by.season) <- unique_species
colnames(b.by.season) <- twenties

a.by.season <- matrix(nrow = length(unique_species), ncol = length(twenties), byrow = TRUE)
rownames(a.by.season) <- unique_species
colnames(a.by.season) <- twenties

for (species in unique_species) {
  
  data <- lw_data %>%
    filter(COMNAME == species) %>%
    filter(INDWT != 0) %>%
    mutate(season = case_when(
      YEAR >= 2020 ~ '2020s Combined',
      SEASON == 'SPRING' ~ "2020s Spring",
      SEASON == 'SUMMER' ~ "2020s Summer", 
      SEASON == 'WINTER' ~ "2020s Winter",
      SEASON == 'FALL' ~ "2020s Fall"))
  
  combined_lm <- lm(log(INDWT) ~ log(LENGTH), data)
  a.by.season[species, '2020s Combined'] <- coef(combined_lm)[1]
  b.by.season[species, '2020s Combined'] <- coef(combined_lm)[2]
  
  for (seas in twenties) {
    seas_data <- data %>%
      filter(season == seas)
    
    if (nrow(seas_data) > 0) {
      seas_lm <- lm(log(INDWT) ~ log(LENGTH), seas_data)
      a.by.season[species, seas] <- coef(seas_lm)[1]
      b.by.season[species, seas] <- coef(seas_lm)[2]
    }
  }
}

b_by_2020season_df <- as.data.frame(b.by.season, stringsAsFactors = FALSE)
write.csv(b_by_2020season_df, file = "~/Desktop/2020sData.csv", row.names = TRUE)
