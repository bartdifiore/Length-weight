library(tidyverse)
lw_data <- readRDS('GMRI/data/bottomTrawlSurvey_indLengthWeight.rds')
lw_coeffs <- readRDS('GMRI/data/lw_coeffs.rds')

species_labels <- unique(lw_data$COMNAME)
decades <- c('1990s', '2000s', '2010s', '2020s')

b.by.decade <- matrix(nrow = length(species_labels), ncol = length(decades), byrow = TRUE)
rownames(b.by.decade) <- species_labels
colnames(b.by.decade) <- decades

a.by.decade <- matrix(nrow = length(species_labels), ncol = length(decades), byrow = TRUE)
rownames(a.by.decade) <- species_labels
colnames(a.by.decade) <- decades

for (species in species_labels) {
  
  data <- lw_data %>%
    filter(COMNAME == species) %>%
    filter(INDWT != 0) %>%
    mutate(decade = case_when(
      YEAR >= 2020 ~ "2020s",
      YEAR >= 2010 & YEAR < 2020 ~ "2010s",
      YEAR >= 2000 & YEAR < 2010 ~ "2000s", 
      YEAR < 2000 ~ "1990s"))
  
  for (dec in decades) {
    dec_data <- data %>%
      filter(decade == dec)
    
    if (nrow(dec_data) > 0) {
      dec_lm <- lm(log(INDWT) ~ log(LENGTH), dec_data)
      a.by.decade[species, dec] <- coef(dec_lm)[1]
      b.by.decade[species, dec] <- coef(dec_lm)[2]
    }
  }
}

b_by_decade_df <- as.data.frame(b.by.decade, stringsAsFactors = FALSE)
print(b_by_decade_df)

