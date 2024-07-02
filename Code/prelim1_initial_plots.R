## -------------------------------------------------------------------------------
# Preliminary Analysis 1: this code constructs plots for Cod, American Plaice,
# Atlantic Herring, and Spiny Dogfish species in the 1990s, 2000s, and 2010s + 2020s.
## --------------------------------------------------------------------------------
library(tidyverse)
lw_data <- readRDS('Data/bottomTrawlSurvey_indLengthWeight.rds')
lw_coeffs <- readRDS('Data/lw_coeffs.rds')

# Create table to hold b parameter
prelim1.b <- matrix(nrow=4, ncol=3, byrow=TRUE)
rownames(prelim1.b) <- c('Cod', 'American Plaice', 'Atlantic Herring', 'Spiny Dogfish')
colnames(prelim1.b) <- c('1990s', '2000s', '2010s')

# Create table to hold log(a) parameter
prelim1.a <- matrix(nrow=4, ncol=3, byrow=TRUE)
rownames(prelim1.a) <- c('Cod', 'American Plaice', 'Atlantic Herring', 'Spiny Dogfish')
colnames(prelim1.a) <- c('1990s', '2000s', '2010s')

#-------------------------------------------------------------------------------
# Atlantic Cod - Periodic Fish
#-------------------------------------------------------------------------------
cod <- lw_data %>%
  filter(COMNAME == 'ATLANTIC COD') %>%
  filter(INDWT != 0) %>%
  mutate(decade = case_when(YEAR >= 2010 ~ "2010s",
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

### Original Cod Data - colored by decade
cod_dec.by.color.orig <- cod %>%
  ggplot(aes(x = LENGTH, y = INDWT)) +
  geom_point(aes(color = decade))+
  labs(title = "Atlantic Cod: Length vs Weight",
       x = "Length",
       y = "Individual Weight")+
  theme_bw()
print(cod_dec.by.color.orig)

### Transformed Cod Data - colored by decade
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

### Original Cod Data - split by decade
cod_dec.split.orig <- cod %>%
  ggplot(aes(x = LENGTH, y = INDWT)) +
  geom_point()+
  facet_wrap(~decade)+
  labs(title = "Atlantic Cod: Length vs Weight",
       x = "Length",
       y = "Individual Weight")+
  theme_bw()
print(cod_dec.split.orig)

### Transformed Cod Data - split by decade
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

### Cod Multiple Linear Regression Model
cod_model <- lm(log(INDWT) ~ log(LENGTH)*decade, cod)
summary(cod_model)
cod_model$coefficients
prelim1.a["Cod", "1990s"] <- cod_model$coefficients[1]
prelim1.a["Cod", "2000s"] <- cod_model$coefficients[1] + cod_model$coefficients[3]
prelim1.a["Cod", "2010s"] <- cod_model$coefficients[1] + cod_model$coefficients[4]
prelim1.b["Cod", "1990s"] <- cod_model$coefficients[2]
prelim1.b["Cod", "2000s"] <- cod_model$coefficients[2] + cod_model$coefficients[5]
prelim1.b["Cod", "2010s"] <- cod_model$coefficients[2] + cod_model$coefficients[6]

# Predictions Plot
new_cod = expand.grid(LENGTH = seq(min(cod$LENGTH), max(cod$LENGTH), length.out = 100), decade = c("1990s", "2000s", "2010s")) 
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
  mutate(decade = case_when(YEAR >= 2010 ~ "2010s",
                            YEAR >= 2000 & YEAR < 2010 ~ "2000s", 
                            YEAR < 2000 ~ "1990s"))

### Original Plaice Data Plot
plaice_orig <- plaice %>%
  ggplot(aes(x = LENGTH, y = INDWT))+
  geom_point() +
  labs(title = "Spiny Dogfish: Length vs Weight",
       x = "Length",
       y = "Individual Weight")+
  theme_bw()
print(plaice_orig)

### Transformed Plaice Data Plot
plaice_log <- plaice %>%
  ggplot(aes(x = LENGTH, y = INDWT)) +
  geom_point() +
  labs(title = "Spiny Dogfish: log(Length) vs log(Weight)",
       x = "log(Length)",
       y = "log(Individual Weight)")+
  theme_bw()+
  scale_x_log10()+
  scale_y_log10()
print(plaice_log)

### Original Plaice Data Plot, colored by decade
plaice_dec.by.color.orig <- plaice %>%
  ggplot(aes(x = LENGTH, y = INDWT)) +
  geom_point(aes(color = decade))+
  labs(title = "Spiny Dogfish: Length vs Weight",
       x = "Length",
       y = "Individual Weight")+
  theme_bw()
print(plaice_dec.by.color.orig)

### Transformed Plaice Data plot, colored by decade
plaice_dec.by.color.log <- plaice %>%
  ggplot(aes(x = LENGTH, y = INDWT)) +
  geom_point(aes(color = decade))+
  labs(title = "Spiny Dogfish: log(Length) vs log(Weight)",
       x = "log(Length)",
       y = "log(Individual Weight)")+
  theme_bw()+
  scale_x_log10()+
  scale_y_log10()
print(plaice_dec.by.color.log)

### Original Plaice Data Plot, split by decade
plaice_dec.split.orig <- plaice %>%
  ggplot(aes(x = LENGTH, y = INDWT)) +
  geom_point()+
  facet_wrap(~decade)+
  labs(title = "Spiny Dogfish: Length vs Weight",
       x = "Length",
       y = "Individual Weight")+
  theme_bw()
print(plaice_dec.split.orig)

### Transformed Plaice Data Plot, split by decade
plaice_dec.split.log <- plaice %>%
  ggplot(aes(x = LENGTH, y = INDWT)) +
  geom_point()+
  facet_wrap(~decade)+
  labs(title = "Spiny Dogfish: log(Length) vs log(Weight)",
       x = "log(Length)",
       y = "log(Individual Weight)")+
  theme_bw()+
  scale_x_log10()+
  scale_y_log10()
print(plaice_dec.split.log)

### Plaice Model
plaice_model <- lm(log(INDWT) ~ log(LENGTH)*decade, plaice)
prelim1.a["American Plaice", "1990s"] <- plaice_model$coefficients[1]
prelim1.a["American Plaice", "2000s"] <- plaice_model$coefficients[1] + plaice_model$coefficients[3]
prelim1.a["American Plaice", "2010s"] <- plaice_model$coefficients[1] + plaice_model$coefficients[4]
prelim1.b["American Plaice", "1990s"] <- plaice_model$coefficients[2]
prelim1.b["American Plaice", "2000s"] <- plaice_model$coefficients[2] + plaice_model$coefficients[5]
prelim1.b["American Plaice", "2010s"] <- plaice_model$coefficients[2] + plaice_model$coefficients[6]

### Plaice Predictions Plot
new_plaice = expand.grid(LENGTH = seq(min(plaice$LENGTH), max(plaice$LENGTH), length.out = 100), decade = c("1990s", "2000s", "2010s")) 
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
  labs(title = "Spiny Dogfish Predicted Weights by Length",
       x = "Length",
       y = "Individual Weight")

# -----------------------------------------------------------------------------
# Atlantic Herring - Opportunistic Fish
# -----------------------------------------------------------------------------
herring <- lw_data %>%
  filter(COMNAME == 'ATLANTIC HERRING') %>%
  filter(INDWT != 0) %>%
  mutate(decade = case_when(YEAR >= 2010 ~ "2010s",
                            YEAR >= 2000 & YEAR < 2010 ~ "2000s", 
                            YEAR < 2000 ~ "1990s"))

### Original Herring Data Plot
herring_orig <- herring %>%
  ggplot(aes(x = LENGTH, y = INDWT))+
  geom_point() +
  labs(title = "Atlantic Herring: Length vs Weight",
       x = "Length",
       y = "Individual Weight")+
  theme_bw()+
  ylim(0, 0.75) # had to add - some HEAVY outliers
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
  theme_bw()+
  ylim(0, 0.75)
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
  theme_bw()+
  ylim(0, 0.75)
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
prelim1.a["Atlantic Herring", "1990s"] <- herring_model$coefficients[1]
prelim1.a["Atlantic Herring", "2000s"] <- herring_model$coefficients[1] + herring_model$coefficients[3]
prelim1.a["Atlantic Herring", "2010s"] <- herring_model$coefficients[1] + herring_model$coefficients[4]
prelim1.b["Atlantic Herring", "1990s"] <- herring_model$coefficients[2]
prelim1.b["Atlantic Herring", "2000s"] <- herring_model$coefficients[2] + herring_model$coefficients[5]
prelim1.b["Atlantic Herring", "2010s"] <- herring_model$coefficients[2] + herring_model$coefficients[6]


### Atlantic Herring Predictions Plot
new_herring = expand.grid(LENGTH = seq(min(herring$LENGTH), max(herring$LENGTH), length.out = 100), decade = c("1990s", "2000s", "2010s")) 
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
# Spiny dogfish - Equilibrium Fish
# -----------------------------------------------------------------------------
dogfish <- lw_data %>%
  filter(COMNAME == 'SPINY DOGFISH') %>%
  filter(INDWT != 0) %>%
  mutate(decade = case_when(YEAR >= 2010 ~ "2010s",
                            YEAR >= 2000 & YEAR < 2010 ~ "2000s", 
                            YEAR < 2000 ~ "1990s"))

### Original Spiny Dogfish Data Plot
dogfish_orig <- dogfish %>%
  ggplot(aes(x = LENGTH, y = INDWT))+
  geom_point() +
  labs(title = "Spiny Dogfish: Length vs Weight",
       x = "Length",
       y = "Individual Weight")+
  theme_bw()
print(dogfish_orig)

### Transformed Spiny Dogfish Data Plot
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

### Original Spiny Dogfish Data, colored by decade
dogfish_dec.by.color.orig <- dogfish %>%
  ggplot(aes(x = LENGTH, y = INDWT)) +
  geom_point(aes(color = decade))+
  labs(title = "Spiny Dogfish Length vs Weight",
       x = "Length",
       y = "Individual Weight")+
  theme_bw()
print(dogfish_dec.by.color.orig)

### Transformed Spiny Dogfish Data, colored by decade
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

### Original Spiny Dogfish Data Plot, split by decade
dogfish_dec.split.orig <- dogfish %>%
  ggplot(aes(x = LENGTH, y = INDWT)) +
  geom_point()+
  facet_wrap(~decade)+
  labs(title = "Spiny Dogfish: Length vs Weight",
       x = "Length",
       y = "Individual Weight")+
  theme_bw()
print(dogfish_dec.split.orig)

### Transformed Spiny Dogfish Data Plot, split by decade
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
prelim1.a["Spiny Dogfish", "1990s"] <- dogfish_model$coefficients[1]
prelim1.a["Spiny Dogfish", "2000s"] <- dogfish_model$coefficients[1] + dogfish_model$coefficients[3]
prelim1.a["Spiny Dogfish", "2010s"] <- dogfish_model$coefficients[1] + dogfish_model$coefficients[4]
prelim1.b["Spiny Dogfish", "1990s"] <- dogfish_model$coefficients[2]
prelim1.b["Spiny Dogfish", "2000s"] <- dogfish_model$coefficients[2] + dogfish_model$coefficients[5]
prelim1.b["Spiny Dogfish", "2010s"] <- dogfish_model$coefficients[2] + dogfish_model$coefficients[6]

### Spiny Dogfish Predictions Plot
new_dogfish = expand.grid(LENGTH = seq(min(dogfish$LENGTH), max(dogfish$LENGTH), length.out = 100), decade = c("1990s", "2000s", "2010s")) 
new_dogfish$predicted <- exp(predict(dogfish_model, newdata = new_dogfish, type = "response")) 
new_dogfish$lower <- exp(predict(dogfish_model, newdata = new_dogfish, type = "response", interval = 
                                   "confidence")[,3])
new_dogfish$upper <- exp(predict(dogfish_model, newdata = new_dogfish, type = "response", interval = 
                                   "confidence")[,2])
ggplot(new_dogfish, aes(x = LENGTH, y = predicted))+
  geom_line(aes(color = decade))+
  geom_ribbon(aes(group = decade, ymax = upper, ymin = lower), alpha = 0.1)+ 
  scale_x_log10()+
  scale_y_log10()+ 
  theme_classic()+
  labs(title = "Spiny Dogfish Predicted Weights by Length",
       x = "Length",
       y = "Individual Weight")

# -----------------------------------------------------------------------------
# Slope Plot
# -----------------------------------------------------------------------------
prelim1.b.to.df <- data.frame(
  Species = c("Cod", "American Plaice", "Atlantic Herring", "Spiny Dogfish"),
  `1990s` = c(3.075384, 3.281383, 3.048610, 3.124006),
  `2000s` = c(3.104295, 3.341870, 3.126295, 3.179436),
  `2010s` = c(3.080731, 3.305686, 3.040111, 3.100683),
  stringsAsFactors = FALSE)

prelim1.b.long <- pivot_longer(prelim1.b.to.df, 
                        cols = starts_with("X"), 
                        names_to = "Decade", 
                        values_to = "Slope")

ggplot(b.long, aes(x = Decade, y = Slope, group = Species, color = Species)) +
  geom_line() +
  geom_point() +
  labs(title = "Slope of Relationship Between log(Length) and log(Weight) By Decade", x = "Decade", y = "Slope (b)") +
  theme_minimal()
