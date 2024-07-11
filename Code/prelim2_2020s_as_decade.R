# -----------------------------------------------------------------------------
# Preliminary Analysis 2: this code adjusts prelim1.R by adding the 2020s as a
# decade, limiting Atlantic Herring's INDWT to only valid values in the model,
# and calculating parameters for more species.
# -----------------------------------------------------------------------------

library(tidyverse) # loading in packages & data
lw_data <- readRDS('Data/bottomTrawlSurvey_indLengthWeight.rds')
lw_coeffs <- readRDS('Data/lw_coeffs.rds')

# Table to hold b parameter
prelim2.b <- matrix(nrow=4, ncol=4, byrow=TRUE)
rownames(prelim2.b) <- c('Cod', 'American Plaice', 'Atlantic Herring', 'Spiny Dogfish')
colnames(prelim2.b) <- c('1990s', '2000s', '2010s', '2020s')

# Table to hold log(a) parameter
prelim2.a <- matrix(nrow=4, ncol=4, byrow=TRUE)
rownames(prelim2.a) <- c('Cod', 'American Plaice', 'Atlantic Herring', 'Spiny Dogfish')
colnames(prelim2.a) <- c('1990s', '2000s', '2010s', '2020s')

# -----------------------------------------------------------------------------
# Atlantic Cod - Periodic Fish
# -----------------------------------------------------------------------------
cod <- lw_data %>%
  filter(COMNAME == 'ATLANTIC COD') %>%
  filter(INDWT != 0) %>%
  mutate(decade = case_when(YEAR >= 2020 ~ "2020s",
                            YEAR >= 2010 & YEAR < 2020 ~ "2010s",
                            YEAR >= 2000 & YEAR < 2010 ~ "2000s", 
                            YEAR < 2000 ~ "1990s"))

### Original Cod Data Plot
cod_orig <- cod %>%
  ggplot(aes(x = LENGTH, y = INDWT))+
  geom_point() +
  labs(title = "Atlantic Cod: Length vs Weight",
       x = "Length",
       y = "Individual Weight")+
  theme_bw()
print(cod_orig)

### Transformed Cod Data Plot
cod_log <- cod %>%
  ggplot(aes(x = LENGTH, y = INDWT)) +
  geom_point() +
  labs(title = "Atlantic Cod: log(Length) vs log(Weight)",
       x = "log(Length)",
       y = "log(Individual Weight)")+
  theme_bw()+
  scale_x_log10()+
  scale_y_log10()
print(cod_log)

### Original Cod Data Plot, colored by decade
cod_dec.by.color.orig <- cod %>%
  ggplot(aes(x = LENGTH, y = INDWT)) +
  geom_point(aes(color = decade))+
  labs(title = "Atlantic Cod: Length vs Weight",
       x = "Length",
       y = "Individual Weight")+
  theme_bw()
print(cod_dec.by.color.orig)

### Transformed Cod Data Plot, colored by decade
cod_dec.by.color.log <- cod %>%
  ggplot(aes(x = LENGTH, y = INDWT)) +
  geom_point(aes(color = decade))+
  labs(title = "Atlantic Cod: log(Length) vs log(Weight)",
       x = "log(Length)",
       y = "log(Individual Weight)")+
  theme_bw()+
  scale_x_log10()+
  scale_y_log10()
print(cod_dec.by.color.log)

### Original Cod Data Plot, split by decade
cod_dec.split.orig <- cod %>%
  ggplot(aes(x = LENGTH, y = INDWT)) +
  geom_point()+
  facet_wrap(~decade)+
  labs(title = "Atlantic Cod: Length vs Weight",
       x = "Length",
       y = "Individual Weight")+
  theme_bw()
print(cod_dec.split.orig)

### Transformed Cod Data Plot, split by decade
cod_dec.split.log <- cod %>%
  ggplot(aes(x = LENGTH, y = INDWT)) +
  geom_point()+
  facet_wrap(~decade)+
  labs(title = "Atlantic Cod: log(Length) vs log(Weight)",
       x = "log(Length)",
       y = "log(Individual Weight)")+
  theme_bw()+
  scale_x_log10()+
  scale_y_log10()
print(cod_dec.split.log)

### Cod Model
cod_model <- lm(log(INDWT) ~ log(LENGTH)*decade, cod)
summary(cod_model)
prelim2.a["Cod", "1990s"] <- cod_model$coefficients[1]
prelim2.b["Cod", "1990s"] <- cod_model$coefficients[2]
prelim2.a["Cod", "2000s"] <- cod_model$coefficients[1] + cod_model$coefficients[3]
prelim2.b["Cod", "2000s"] <- cod_model$coefficients[2] + cod_model$coefficients[6]
prelim2.a["Cod", "2010s"] <- cod_model$coefficients[1] + cod_model$coefficients[4]
prelim2.b["Cod", "2010s"] <- cod_model$coefficients[2] + cod_model$coefficients[7]
prelim2.a["Cod", "2020s"] <- cod_model$coefficients[1] + cod_model$coefficients[5]
prelim2.b["Cod", "2020s"] <- cod_model$coefficients[2] + cod_model$coefficients[8]

###### FROM BART - FIGURE OUT
###### FROM BART - FIGURE OUT
# library(emmeans)
# conts <- emmeans(cod_model, ~decade, by = LENGTH)
# pairs(conts)
# summary(cod$LENGTH)
# emmeans(mod1b, ~bait_type, condition = "response")
###### FROM BART - FIGURE OUT
###### FROM BART - FIGURE OUT

### Cod Predictions Plot
new_cod = expand.grid(LENGTH = seq(min(cod$LENGTH), max(cod$LENGTH), length.out = 100), decade = c("1990s", "2000s", "2010s", "2020s")) 
new_cod$predicted <- exp(predict(cod_model, newdata = new_cod, type = "response")) 
new_cod$lower <- exp(predict(cod_model, newdata = new_cod, type = "response", interval = 
                               "confidence")[,3])
new_cod$upper <- exp(predict(cod_model, newdata = new_cod, type = "response", interval = 
                               "confidence")[,2])
ggplot(new_cod, aes(x = LENGTH, y = predicted))+
  geom_line(aes(color = decade))+
  geom_ribbon(aes(group = decade, ymax = upper, ymin = lower), alpha = 0.1)+ 
  scale_x_log10()+
  scale_y_log10()+ 
  theme_classic()+
  labs(title = "Cod Predicted Weights by Length",
       x = "Length",
       y = "Individual Weight")

# -----------------------------------------------------------------------------
# American Plaice - Periodic Fish
# -----------------------------------------------------------------------------
plaice <- lw_data %>%
  filter(COMNAME == 'AMERICAN PLAICE') %>%
  filter(INDWT != 0) %>%
  mutate(decade = case_when(YEAR >= 2020 ~ "2020s",
                            YEAR >= 2010 & YEAR < 2020 ~ "2010s",
                            YEAR >= 2000 & YEAR < 2010 ~ "2000s", 
                            YEAR < 2000 ~ "1990s"))

### Original Plaice Data Plot
plaice_orig <- plaice %>%
  ggplot(aes(x = LENGTH, y = INDWT))+
  geom_point() +
  labs(title = "American Plaice: Length vs Weight",
       x = "Length",
       y = "Individual Weight")+
  theme_bw()
print(plaice_orig)

### Transformed Plaice Data Plot
plaice_log <- plaice %>%
  ggplot(aes(x = LENGTH, y = INDWT)) +
  geom_point() +
  labs(title = "American Plaice: log(Length) vs log(Weight)",
       x = "log(Length)",
       y = "log(Individual Weight)")+
  theme_bw()+
  scale_x_log10()+
  scale_y_log10()
print(plaice_log)

### Original Data Plot, colored by decade
plaice_dec.by.color.orig <- plaice %>%
  ggplot(aes(x = LENGTH, y = INDWT)) +
  geom_point(aes(color = decade))+
  labs(title = "American Plaice: Length vs Weight",
       x = "Length",
       y = "Individual Weight")+
  theme_bw()
print(plaice_dec.by.color.orig)

### Transformed Data Plot, colored by decade
plaice_dec.by.color.log <- plaice %>%
  ggplot(aes(x = LENGTH, y = INDWT)) +
  geom_point(aes(color = decade))+
  labs(title = "American Plaice: log(Length) vs log(Weight)",
       x = "log(Length)",
       y = "log(Individual Weight)")+
  theme_bw()+
  scale_x_log10()+
  scale_y_log10()
print(plaice_dec.by.color.log)

### Original Data Plot, split by decade
plaice_dec.split.orig <- plaice %>%
  ggplot(aes(x = LENGTH, y = INDWT)) +
  geom_point()+
  facet_wrap(~decade)+
  labs(title = "American Plaice: Length vs Weight",
       x = "Length",
       y = "Individual Weight")+
  theme_bw()
print(plaice_dec.split.orig)

### Transformed Data Plot, split by decade
plaice_dec.split.log <- plaice %>%
  ggplot(aes(x = LENGTH, y = INDWT)) +
  geom_point()+
  facet_wrap(~decade)+
  labs(title = "American Plaice: log(Length) vs log(Weight)",
       x = "log(Length)",
       y = "log(Individual Weight)")+
  theme_bw()+
  scale_x_log10()+
  scale_y_log10()
print(plaice_dec.split.log)

### Plaice Model
plaice_model <- lm(log(INDWT) ~ log(LENGTH)*decade, plaice)
summary(plaice_model)
prelim2.a["American Plaice", "1990s"] <- plaice_model$coefficients[1]
prelim2.b["American Plaice", "1990s"] <- plaice_model$coefficients[2]
prelim2.a["American Plaice", "2000s"] <- plaice_model$coefficients[1] + plaice_model$coefficients[3]
prelim2.b["American Plaice", "2000s"] <- plaice_model$coefficients[2] + plaice_model$coefficients[6]
prelim2.a["American Plaice", "2010s"] <- plaice_model$coefficients[1] + plaice_model$coefficients[4]
prelim2.b["American Plaice", "2010s"] <- plaice_model$coefficients[2] + plaice_model$coefficients[7]
prelim2.a["American Plaice", "2020s"] <- plaice_model$coefficients[1] + plaice_model$coefficients[5]
prelim2.b["American Plaice", "2020s"] <- plaice_model$coefficients[2] + plaice_model$coefficients[8]

### Plaice Predictions Plot
new_plaice = expand.grid(LENGTH = seq(min(plaice$LENGTH), max(plaice$LENGTH), length.out = 100), decade = c("1990s", "2000s", "2010s", "2020s")) 
new_plaice$predicted <- exp(predict(plaice_model, newdata = new_plaice, type = "response")) 
new_plaice$lower <- exp(predict(plaice_model, newdata = new_plaice, type = "response", interval = 
                                  "confidence")[,3])
new_plaice$upper <- exp(predict(plaice_model, newdata = new_plaice, type = "response", interval = 
                                  "confidence")[,2])
ggplot(new_plaice, aes(x = LENGTH, y = predicted))+
  geom_line(aes(color = decade))+
  geom_ribbon(aes(group = decade, ymax = upper, ymin = lower), alpha = 0.1)+ 
  scale_x_log10()+
  scale_y_log10()+ 
  theme_classic()+
  labs(title = "American Plaice Predicted Weights by Length",
       x = "Length",
       y = "Individual Weight")

# -----------------------------------------------------------------------------
# Atlantic Herring - Opportunistic Fish
# -----------------------------------------------------------------------------
herring <- lw_data %>%
  filter(COMNAME == 'ATLANTIC HERRING') %>%
  filter(INDWT != 0 & INDWT < 0.75) %>%
  mutate(decade = case_when(YEAR >= 2020 ~ "2020s",
                            YEAR >= 2010 & YEAR < 2020 ~ "2010s",
                            YEAR >= 2000 & YEAR < 2010 ~ "2000s", 
                            YEAR < 2000 ~ "1990s"))

### Original Herring Data Plot
herring_orig <- herring %>%
  ggplot(aes(x = LENGTH, y = INDWT))+
  geom_point() +
  labs(title = "Atlantic Herring: Length vs Weight",
       x = "Length",
       y = "Individual Weight")+
  theme_bw()
print(herring_orig)

### Transformed Herring Data Plot
herring_log <- herring %>%
  ggplot(aes(x = LENGTH, y = INDWT)) +
  geom_point() +
  labs(title = "Atlantic Herring: log(Length) vs log(Weight)",
       x = "log(Length)",
       y = "log(Individual Weight)")+
  theme_bw()+
  scale_x_log10()+
  scale_y_log10()
print(herring_log)

### Original Herring Data Plot, colored by decade
herring_dec.by.color.orig <- herring %>%
  ggplot(aes(x = LENGTH, y = INDWT)) +
  geom_point(aes(color = decade))+
  labs(title = "Atlantic Herring: Length vs Weight",
       x = "Length",
       y = "Individual Weight")+
  theme_bw()
print(herring_dec.by.color.orig)

### Transformed Herring Data Plot, colored by decade
herring_dec.by.color.log <- herring %>%
  ggplot(aes(x = LENGTH, y = INDWT)) +
  geom_point(aes(color = decade))+
  labs(title = "Atlantic Herring: log(Length) vs log(Weight)",
       x = "log(Length)",
       y = "log(Individual Weight)")+
  theme_bw()+
  scale_x_log10()+
  scale_y_log10()
print(herring_dec.by.color.log)

### Original Herring Data Plot, split by decade
herring_dec.split.orig <- herring %>%
  ggplot(aes(x = LENGTH, y = INDWT)) +
  geom_point()+
  facet_wrap(~decade)+
  labs(title = "Atlantic Herring: Length vs Weight",
       x = "Length",
       y = "Individual Weight")+
  theme_bw()
print(herring_dec.split.orig)

### Transformed Herring Data Plot, split by decade
herring_dec.split.log <- herring %>%
  ggplot(aes(x = LENGTH, y = INDWT)) +
  geom_point()+
  facet_wrap(~decade)+
  labs(title = "Atlantic Herring: log(Length) vs log(Weight)",
       x = "log(Length)",
       y = "log(Individual Weight)")+
  theme_bw()+
  scale_x_log10()+
  scale_y_log10()
print(herring_dec.split.log)

### Herring Model
herring_model <- lm(log(INDWT) ~ log(LENGTH)*decade, herring)
summary(herring_model)
prelim2.a["Atlantic Herring", "1990s"] <- herring_model$coefficients[1]
prelim2.b["Atlantic Herring", "1990s"] <- herring_model$coefficients[2]
prelim2.a["Atlantic Herring", "2000s"] <- herring_model$coefficients[1] + herring_model$coefficients[3]
prelim2.b["Atlantic Herring", "2000s"] <- herring_model$coefficients[2] + herring_model$coefficients[6]
prelim2.a["Atlantic Herring", "2010s"] <- herring_model$coefficients[1] + herring_model$coefficients[4]
prelim2.b["Atlantic Herring", "2010s"] <- herring_model$coefficients[2] + herring_model$coefficients[7]
prelim2.a["Atlantic Herring", "2020s"] <- herring_model$coefficients[1] + herring_model$coefficients[5]
prelim2.b["Atlantic Herring", "2020s"] <- herring_model$coefficients[2] + herring_model$coefficients[8]

### Herring Predictions Plot
new_herring = expand.grid(LENGTH = seq(min(herring$LENGTH), max(herring$LENGTH), length.out = 100), decade = c("1990s", "2000s", "2010s", "2020s")) 
new_herring$predicted <- exp(predict(herring_model, newdata = new_herring, type = "response")) 
new_herring$lower <- exp(predict(herring_model, newdata = new_herring, type = "response", interval = 
                                   "confidence")[,3])
new_herring$upper <- exp(predict(herring_model, newdata = new_herring, type = "response", interval = 
                                   "confidence")[,2])
ggplot(new_herring, aes(x = LENGTH, y = predicted))+
  geom_line(aes(color = decade))+
  geom_ribbon(aes(group = decade, ymax = upper, ymin = lower), alpha = 0.1)+ 
  scale_x_log10()+
  scale_y_log10()+ 
  theme_classic()+
  labs(title = "Atlantic Herring Predicted Weights by Length",
       x = "Length",
       y = "Individual Weight")

# -----------------------------------------------------------------------------
# Spiny Dogfish - Equilibrium Fish
# -----------------------------------------------------------------------------
dogfish <- lw_data %>%
  filter(COMNAME == 'SPINY DOGFISH') %>%
  filter(INDWT != 0) %>%
  mutate(decade = case_when(YEAR >= 2020 ~ "2020s",
                            YEAR >= 2010 & YEAR < 2020 ~ "2010s",
                            YEAR >= 2000 & YEAR < 2010 ~ "2000s", 
                            YEAR < 2000 ~ "1990s"))

### Original Dogfish Data Plot
dogfish_orig <- dogfish %>%
  ggplot(aes(x = LENGTH, y = INDWT))+
  geom_point() +
  labs(title = "Spiny Dogfish: Length vs Weight",
       x = "Length",
       y = "Individual Weight")+
  theme_bw()
print(dogfish_orig)

### Transformed Dogfish Data Plot
dogfish_log <- dogfish %>%
  ggplot(aes(x = LENGTH, y = INDWT)) +
  geom_point() +
  labs(title = "Spiny Dogfish: log(Length) vs log(Weight)",
       x = "log(Length)",
       y = "log(Individual Weight)")+
  theme_bw()+
  scale_x_log10()+
  scale_y_log10()
print(dogfish_log)

### Original Dogfish Data Plot, colored by decade
dogfish_dec.by.color.orig <- dogfish %>%
  ggplot(aes(x = LENGTH, y = INDWT)) +
  geom_point(aes(color = decade))+
  labs(title = "Spiny Dogfish Length vs Weight",
       x = "Length",
       y = "Individual Weight")+
  theme_bw()
print(dogfish_dec.by.color.orig)

### Transformed Dogfish Data Plot, colored by decade
dogfish_dec.by.color.log <- dogfish %>%
  ggplot(aes(x = LENGTH, y = INDWT)) +
  geom_point(aes(color = decade))+
  labs(title = "Spiny Dogfish: log(Length) vs log(Weight)",
       x = "log(Length)",
       y = "log(Individual Weight)")+
  theme_bw()+
  scale_x_log10()+
  scale_y_log10()
print(dogfish_dec.by.color.log)

### Original Dogfish Data Plot, split by decade
dogfish_dec.split.orig <- dogfish %>%
  ggplot(aes(x = LENGTH, y = INDWT)) +
  geom_point()+
  facet_wrap(~decade)+
  labs(title = "Spiny Dogfish: Length vs Weight",
       x = "Length",
       y = "Individual Weight")+
  theme_bw()
print(dogfish_dec.split.orig)

### Transformed Dogfish Data Plot, split by decade
dogfish_dec.split.log <- dogfish %>%
  ggplot(aes(x = LENGTH, y = INDWT)) +
  geom_point()+
  facet_wrap(~decade)+
  labs(title = "Spiny Dogfish: log(Length) vs log(Weight)",
       x = "log(Length)",
       y = "log(Individual Weight)")+
  theme_bw()+
  scale_x_log10()+
  scale_y_log10()
print(dogfish_dec.split.log)

### Spiny Dogfish Model
dogfish_model <- lm(log(INDWT) ~ log(LENGTH)*decade, dogfish)
summary(dogfish_model)
prelim2.a["Spiny Dogfish", "1990s"] <- dogfish_model$coefficients[1]
prelim2.b["Spiny Dogfish", "1990s"] <- dogfish_model$coefficients[2]
prelim2.a["Spiny Dogfish", "2000s"] <- dogfish_model$coefficients[1] + dogfish_model$coefficients[3]
prelim2.b["Spiny Dogfish", "2000s"] <- dogfish_model$coefficients[2] + dogfish_model$coefficients[6]
prelim2.a["Spiny Dogfish", "2010s"] <- dogfish_model$coefficients[1] + dogfish_model$coefficients[4]
prelim2.b["Spiny Dogfish", "2010s"] <- dogfish_model$coefficients[2] + dogfish_model$coefficients[7]
prelim2.a["Spiny Dogfish", "2020s"] <- dogfish_model$coefficients[1] + dogfish_model$coefficients[5]
prelim2.b["Spiny Dogfish", "2020s"] <- dogfish_model$coefficients[2] + dogfish_model$coefficients[8]

# -----------------------------------------------------------------------------
# Slope Plot
# -----------------------------------------------------------------------------

prelim2.b.to.df <- data.frame(
  Species = c("Cod", "American Plaice", "Atlantic Herring", "Spiny Dogfish"),
  `1990s` = c(3.075384, 3.281383, 3.048610, 3.124006),
  `2000s` = c(3.104295, 3.341870, 3.126295, 3.179436),
  `2010s` = c(3.083302, 3.304652, 3.035329, 3.118535),
  `2020s` = c(3.072278, 3.310441, 3.087207, 3.011393),
  stringsAsFactors = FALSE
)

prelim2.b.long <- pivot_longer(prelim2.b.to.df, 
                       cols = starts_with("X"), 
                       names_to = "Decade", 
                       values_to = "Slope")

ggplot(prelim2.b.long, aes(x = Decade, y = Slope, group = Species, color = Species)) +
  geom_line() +
  geom_point() +
  labs(title = "Slope of Relationship Between log(Length) and log(Weight) By Decade", x = "Decade", y = "Slope (b)") +
  theme_minimal()

# -----------------------------------------------------------------------------
# More Species Analyzed - Fish from Kathy's study who were larger at younger 
# ages and smaller at older ages
# -----------------------------------------------------------------------------
decades <- c('1990s', '2000s', '2010s', '2020s')
species_labels <- c('Cod', 'American Plaice', 'Atlantic Herring', 'Spiny Dogfish', 'Winter Skate', 'Pollock', 'White Hake', 'Witch Flounder', 'Atlantic Mackerel')
species_list <- c('ATLANTIC COD', 'AMERICAN PLAICE', 'ATLANTIC HERRING', 'SPINY DOGFISH', 'WINTER SKATE', 'POLLOCK', 'WHITE HAKE', 'WITCH FLOUNDER', 'ATLANTIC MACKEREL')

more.b.by.decade <- matrix(nrow = length(species_labels), ncol = length(decades), byrow = TRUE)
rownames(more.b.by.decade) <- species_labels
colnames(more.b.by.decade) <- decades

more.a.by.decade <- matrix(nrow = length(species_labels), ncol = length(decades), byrow = TRUE)
rownames(more.a.by.decade) <- species_labels
colnames(more.a.by.decade) <- decades

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
    all.spec.lm <- lm(log(INDWT) ~ log(LENGTH)*decade, data)
    more.a.by.decade[species_name, "1990s"] <- all.spec.lm$coefficients[1]
    more.b.by.decade[species_name, "1990s"] <- all.spec.lm$coefficients[2]
    more.a.by.decade[species_name, "2000s"] <- all.spec.lm$coefficients[1] + all.spec.lm$coefficients[3]
    more.b.by.decade[species_name, "2000s"] <- all.spec.lm$coefficients[2] + all.spec.lm$coefficients[6]
    more.a.by.decade[species_name, "2010s"] <- all.spec.lm$coefficients[1] + all.spec.lm$coefficients[4]
    more.b.by.decade[species_name, "2010s"] <- all.spec.lm$coefficients[2] + all.spec.lm$coefficients[7]
    more.a.by.decade[species_name, "2020s"] <- all.spec.lm$coefficients[1] + all.spec.lm$coefficients[5]
    more.b.by.decade[species_name, "2020s"] <- all.spec.lm$coefficients[2] + all.spec.lm$coefficients[8]
}

more.b.by.decade.df <- as.data.frame(more.b.by.decade, stringsAsFactors = FALSE)
more.b.by.decade.df$Species <- rownames(more.b.by.decade)

more.b.long <- pivot_longer(more.b.by.decade.df, 
                       cols = decades, 
                       names_to = "Decade", 
                       values_to = "Slope")

ggplot(more.b.long, aes(x = Decade, y = Slope, group = Species, color = Species)) +
  geom_line() +
  geom_point() +
  labs(title = "Slope of Relationship Between log(Length) and log(Weight) By Decade", 
       x = "Decade", 
       y = "Slope (b)") +
  theme_minimal()

# -----------------------------------------------------------------------------
# Fitting the length-weight relationship for all species, by decade: unfiltered
# (i.e. there may be some errors - this will be fixed in a future document)
# -----------------------------------------------------------------------------
all.species.labels <- unique(lw_data$COMNAME)

all.b.by.decade <- matrix(nrow = length(all.species.labels), ncol = length(decades), byrow = TRUE)
rownames(all.b.by.decade) <- all.species.labels
colnames(all.b.by.decade) <- decades

all.a.by.decade <- matrix(nrow = length(all.species.labels), ncol = length(decades), byrow = TRUE)
rownames(all.a.by.decade) <- all.species.labels
colnames(all.a.by.decade) <- decades

for (species in all.species.labels) {
  
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
      all.species.lm <- lm(log(INDWT) ~ log(LENGTH), dec_data)
      all.a.by.decade[species, dec] <- coef(all.species.lm)[1]
      all.b.by.decade[species, dec] <- coef(all.species.lm)[2]
    }
  }
}

all.b.by.decade.df <- as.data.frame(all.b.by.decade, stringsAsFactors = FALSE)