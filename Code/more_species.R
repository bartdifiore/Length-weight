library(tidyverse)
lw_data <- readRDS('data/bottomTrawlSurvey_indLengthWeight.rds')
lw_coeffs <- readRDS('data/lw_coeffs.rds')

decades <- c('1990s', '2000s', '2010s', '2020s')
species_labels <- c('Cod', 'American Plaice', 'Atlantic Herring', 'Spiny Dogfish', 'Winter Skate', 'Pollock', 'White Hake', 'Witch Flounder', 'Atlantic Mackerel')
b.by.decade <- matrix(nrow = length(species_labels), ncol = length(decades), byrow = TRUE)
rownames(b.by.decade) <- species_labels
colnames(b.by.decade) <- decades

a.by.decade <- matrix(nrow = length(species_labels), ncol = length(decades), byrow = TRUE)

rownames(a.by.decade) <- species_labels
colnames(a.by.decade) <- decades

species_list <- c('ATLANTIC COD', 'AMERICAN PLAICE', 'ATLANTIC HERRING', 'SPINY DOGFISH', 'WINTER SKATE', 'POLLOCK', 'WHITE HAKE', 'WITCH FLOUNDER', 'ATLANTIC MACKEREL')

for (i in seq_along(species_list)) {
  species <- species_list[i]
  species_name <- species_labels[i]
  
  data <- lw_data %>%
    filter(COMNAME == species) %>%
    filter(INDWT != 0) %>%
    mutate(decade = case_when(
      YEAR >= 2020 ~ "2020s",
      YEAR >= 2010 & YEAR < 2020 ~ "2010s",
      YEAR >= 2000 & YEAR < 2010 ~ "2000s", 
      YEAR < 2000 ~ "1990s"
    ))
  
  for (dec in decades) {
    dec_data <- data %>%
      filter(decade == dec)
    dec_lm <- lm(log(INDWT) ~ log(LENGTH), dec_data)
    a.by.decade[species_name, dec] <- summary(dec_lm)$coefficients[1, 1]
    b.by.decade[species_name, dec] <- summary(dec_lm)$coefficients[2, 1]
  }
}

b_by_decade_df <- as.data.frame(b.by.decade, stringsAsFactors = FALSE)
b_by_decade_df$Species <- rownames(b.by.decade)
print(b_by_decade_df)

b_long <- pivot_longer(b_by_decade_df, 
                       cols = decades, 
                       names_to = "Decade", 
                       values_to = "Slope")
print(b_long)

ggplot(b_long, aes(x = Decade, y = Slope, group = Species, color = Species)) +
  geom_line() +
  geom_point() +
  labs(title = "Slope of Relationship Between log(Length) and log(Weight) By Decade", 
       x = "Decade", 
       y = "Slope (b)") +
  theme_minimal()

##### EXCLUDING 2000s

decades <- c('1990s', '2020s')
species_labels <- c('Cod', 'American Plaice', 'Atlantic Herring', 'Spiny Dogfish', 'Winter Skate', 'Pollock', 'White Hake', 'Witch Flounder', 'Atlantic Mackerel')

b.by.decade <- matrix(nrow = length(species_labels), ncol = length(decades), byrow = TRUE)
rownames(b.by.decade) <- species_labels
colnames(b.by.decade) <- decades

a.by.decade <- matrix(nrow = length(species_labels), ncol = length(decades), byrow = TRUE)
rownames(a.by.decade) <- species_labels
colnames(a.by.decade) <- decades

# Define species list and names
species_list <- c('ATLANTIC COD', 'AMERICAN PLAICE', 'ATLANTIC HERRING', 'SPINY DOGFISH', 'WINTER SKATE', 'POLLOCK', 'WHITE HAKE', 'WITCH FLOUNDER', 'ATLANTIC MACKEREL')

# Loop through each species to calculate the 'a' and 'b' coefficients by decade
for (i in seq_along(species_list)) {
  species <- species_list[i]
  species_name <- species_labels[i]
  
  data <- lw_data %>%
    filter(COMNAME == species) %>%
    filter(INDWT != 0) %>%
    mutate(decade = case_when(
      YEAR >= 2020 ~ "2020s",
      # YEAR >= 2010 & YEAR < 2020 ~ "2010s",
      YEAR < 2000 ~ "1990s"  # Exclude '2000s'
    ))
  
  for (dec in decades) {
    dec_data <- data %>%
      filter(decade == dec)
    
    if (nrow(dec_data) > 0) {
      dec_lm <- lm(log(INDWT) ~ log(LENGTH), dec_data)
      a.by.decade[species_name, dec] <- summary(dec_lm)$coefficients[1, 1]
      b.by.decade[species_name, dec] <- summary(dec_lm)$coefficients[2, 1]
    } else {
      a.by.decade[species_name, dec] <- NA
      b.by.decade[species_name, dec] <- NA
    }
  }
}

# Convert 'b.by.decade' matrix to data frame and reshape for plotting
b_by_decade_df <- as.data.frame(b.by.decade, stringsAsFactors = FALSE)
b_by_decade_df$Species <- rownames(b.by.decade)
rownames(b.by.decade) <- NULL
b_by_decade_df <- b_by_decade_df %>%
  select(Species, everything())

b_long <- pivot_longer(b_by_decade_df, 
                       cols = decades, 
                       names_to = "Decade", 
                       values_to = "Slope")

# Plot the results
ggplot(b_long, aes(x = Decade, y = Slope, group = Species, color = Species)) +
  geom_line() +
  geom_point() +
  labs(title = "Slope of Relationship Between log(Length) and log(Weight) By Decade", 
       x = "Decade", 
       y = "Slope (b)") +
  theme_minimal()


