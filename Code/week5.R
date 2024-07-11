### ---------------------------------------------------------------------------
### Decades broken up in 10 year increments, EPU prediction plots
### ---------------------------------------------------------------------------
library(tidyverse) # loading in packages & data
lw_data <- readRDS('Data/length_weight_epu.rds')
lw_coeffs <- readRDS('Data/lw_coeffs.rds')

# Table to hold b parameter
b.by.10yrs <- matrix(nrow=4, ncol=3, byrow=TRUE)
rownames(b.by.10yrs) <- c('Cod', 'American Plaice', 'Atlantic Herring', 'Spiny Dogfish')
colnames(b.by.10yrs) <- c('1992 - 2002', '2003 - 2013', '2014 - 2023')

# Table to hold log(a) parameter
a.by.10yrs <- matrix(nrow=4, ncol=3, byrow=TRUE)
rownames(a.by.10yrs) <- c('Cod', 'American Plaice', 'Atlantic Herring', 'Spiny Dogfish')
colnames(a.by.10yrs) <- c('1992 - 2002', '2003 - 2013', '2014 - 2023')

# -----------------------------------------------------------------------------
# Atlantic Cod - Periodic Fish
# -----------------------------------------------------------------------------
cod <- lw_data %>%
  filter(COMNAME == 'ATLANTIC COD') %>%
  filter(INDWT != 0) %>%
  mutate(decade = case_when(YEAR >= 2014 ~ "2014 - 2023",
                            YEAR >= 2003 & YEAR < 2014 ~ "2003 - 2013",
                            YEAR <= 2002 ~ "1992 - 2002")) %>%
  filter(!is.na(EPU))

### Cod Data Plot
cod_plot <- cod %>%
  ggplot(aes(x = LENGTH, y = INDWT))+
  geom_point(aes(color=decade)) +
  facet_wrap(~EPU)+
  labs(title = "Atlantic Cod: Length vs Weight",
       x = "Length",
       y = "Individual Weight")+
  theme_bw()
print(cod_plot)

cod_model <- lm(log(INDWT) ~ log(LENGTH)*decade + log(LENGTH)*EPU, data=cod)
summary(cod_model)
a.by.10yrs["Cod", "1992 - 2002"] <- cod_model$coefficients[1] + cod_model$coefficients[5]
b.by.10yrs["Cod", "1992 - 2002"] <- cod_model$coefficients[2] + cod_model$coefficients[10]
a.by.10yrs["Cod", "2003 - 2013"] <- cod_model$coefficients[1] + cod_model$coefficients[5] + cod_model$coefficients[3]
b.by.10yrs["Cod", "2003 - 2013"] <- cod_model$coefficients[2] + cod_model$coefficients[10] + cod_model$coefficients[8]
a.by.10yrs["Cod", "2014 - 2023"] <- cod_model$coefficients[1] + cod_model$coefficients[5] + cod_model$coefficients[4]
b.by.10yrs["Cod", "2014 - 2023"] <- cod_model$coefficients[2] + cod_model$coefficients[10] + cod_model$coefficients[9]

# Cod Predictions Plot - by EPU
cod$decade <- factor(cod$decade)
cod$EPU <- factor(cod$EPU)
new_cod <- expand.grid(LENGTH = seq(min(cod$LENGTH), max(cod$LENGTH), length.out = 100), 
  decade = levels(cod$decade),
  EPU = levels(cod$EPU))

new_cod$decade <- factor(new_cod$decade, levels = levels(cod$decade))
new_cod$EPU <- factor(new_cod$EPU, levels = levels(cod$EPU))

predictions <- predict(cod_model, newdata = new_cod, interval = "confidence")
predictions

new_cod$predicted <- exp(predictions[, "fit"])
new_cod$lower <- exp(predictions[, "lwr"])
new_cod$upper <- exp(predictions[, "upr"])

ggplot(new_cod, aes(x = LENGTH, y = predicted, color = decade)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = decade), alpha = 0.1) +
  scale_x_log10() +
  scale_y_log10() +
  theme_classic() +
  labs(
    title = "Cod Predicted Weights by Length",
    x = "Length",
    y = "Individual Weight"
  ) +
  facet_wrap(~EPU)

cod.conts.epu <- emmeans(cod_model, ~EPU, by = "LENGTH", data=cod)
pairs(cod.conts.epu)

cod.conts.dec <- emmeans(cod_model, ~decade, by = "LENGTH", data=cod)
pairs(cod.conts.dec)

# -----------------------------------------------------------------------------
# American Plaice - Periodic Fish (Pt 2)
# -----------------------------------------------------------------------------
plaice <- lw_data %>%
  filter(COMNAME == 'AMERICAN PLAICE') %>%
  filter(INDWT < 4) %>%
  filter(INDWT != 0) %>%
  mutate(decade = case_when(YEAR >= 2014 ~ "2014 - 2023",
                            YEAR >= 2003 & YEAR < 2014 ~ "2003 - 2013",
                            YEAR <= 2002 ~ "1992 - 2002")) %>%
  filter(!is.na(EPU))

### American Plaice Data Plot
plaice_plot <- plaice %>%
  ggplot(aes(x = LENGTH, y = INDWT))+
  geom_point(aes(color=decade)) +
  facet_wrap(~EPU)+
  labs(title = "Atlantic Plaice: Length vs Weight",
       x = "Length",
       y = "Individual Weight")+
  theme_bw()
print(plaice_plot)

plaice_model <- lm(log(INDWT) ~ log(LENGTH)*decade + log(LENGTH)*EPU, data=plaice)
summary(plaice_model)
a.by.10yrs["American Plaice", "1992 - 2002"] <- plaice_model$coefficients[1] + plaice_model$coefficients[5]
b.by.10yrs["American Plaice", "1992 - 2002"] <- plaice_model$coefficients[2] + plaice_model$coefficients[10]
a.by.10yrs["American Plaice", "2003 - 2013"] <- plaice_model$coefficients[1] + plaice_model$coefficients[5] + plaice_model$coefficients[3]
b.by.10yrs["American Plaice", "2003 - 2013"] <- plaice_model$coefficients[2] + plaice_model$coefficients[10] + plaice_model$coefficients[8]
a.by.10yrs["American Plaice", "2014 - 2023"] <- plaice_model$coefficients[1] + plaice_model$coefficients[5] + plaice_model$coefficients[4]
b.by.10yrs["American Plaice", "2014 - 2023"] <- plaice_model$coefficients[2] + plaice_model$coefficients[10] + plaice_model$coefficients[9]

# American Plaice Predictions Plot - by EPU
plaice$decade <- factor(plaice$decade)
plaice$EPU <- factor(plaice$EPU)
new_plaice <- expand.grid(LENGTH = seq(min(plaice$LENGTH), max(plaice$LENGTH), length.out = 100), 
                       decade = levels(plaice$decade),
                       EPU = levels(plaice$EPU))

new_plaice$decade <- factor(new_plaice$decade, levels = levels(plaice$decade))
new_plaice$EPU <- factor(new_plaice$EPU, levels = levels(plaice$EPU))

predictions <- predict(plaice_model, newdata = new_plaice, interval = "confidence")

new_plaice$predicted <- exp(predictions[, "fit"])
new_plaice$lower <- exp(predictions[, "lwr"])
new_plaice$upper <- exp(predictions[, "upr"])

ggplot(new_plaice, aes(x = LENGTH, y = predicted, color = decade)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = decade), alpha = 0.1) +
  scale_x_log10() +
  scale_y_log10() +
  theme_light() +
  labs(title = "American Plaice Predicted Weights by Length",
    x = "Length",
    y = "Individual Weight") +
  facet_wrap(~EPU)

plaice.conts.epu <- emmeans(plaice_model, ~EPU, by = "LENGTH", data=plaice)
pairs(plaice.conts.epu)

plaice.conts.dec <- emmeans(plaice_model, ~decade, by = "LENGTH", data=plaice)
pairs(plaice.conts.dec)

# -----------------------------------------------------------------------------
# Atlantic Herring - Opportunistic Fish 
# -----------------------------------------------------------------------------
herring <- lw_data %>%
  filter(COMNAME == 'ATLANTIC HERRING') %>%
  filter(INDWT < 4) %>%
  filter(INDWT != 0) %>%
  mutate(decade = case_when(YEAR >= 2014 ~ "2014 - 2023",
                            YEAR >= 2003 & YEAR < 2014 ~ "2003 - 2013",
                            YEAR <= 2002 ~ "1992 - 2002")) %>%
  filter(!is.na(EPU))

### American herring Data Plot
herring_plot <- herring %>%
  ggplot(aes(x = LENGTH, y = INDWT))+
  geom_point(aes(color=decade)) +
  facet_wrap(~EPU)+
  labs(title = "Atlantic Herring: Length vs Weight",
       x = "Length",
       y = "Individual Weight")+
  theme_bw()
print(herring_plot)

herring_model <- lm(log(INDWT) ~ log(LENGTH)*decade + log(LENGTH)*EPU, data=herring)
summary(herring_model)
a.by.10yrs["Atlantic Herring", "1992 - 2002"] <- herring_model$coefficients[1] + herring_model$coefficients[5]
b.by.10yrs["Atlantic Herring", "1992 - 2002"] <- herring_model$coefficients[2] + herring_model$coefficients[10]
a.by.10yrs["Atlantic Herring", "2003 - 2013"] <- herring_model$coefficients[1] + herring_model$coefficients[5] + herring_model$coefficients[3]
b.by.10yrs["Atlantic Herring", "2003 - 2013"] <- herring_model$coefficients[2] + herring_model$coefficients[10] + herring_model$coefficients[8]
a.by.10yrs["Atlantic Herring", "2014 - 2023"] <- herring_model$coefficients[1] + herring_model$coefficients[5] + herring_model$coefficients[4]
b.by.10yrs["Atlantic Herring", "2014 - 2023"] <- herring_model$coefficients[2] + herring_model$coefficients[10] + herring_model$coefficients[9]

# Atlantic Herring Predictions Plot - by EPU
herring$decade <- factor(herring$decade)
herring$EPU <- factor(herring$EPU)
new_herring <- expand.grid(LENGTH = seq(min(herring$LENGTH), max(herring$LENGTH), length.out = 100), 
                          decade = levels(herring$decade),
                          EPU = levels(herring$EPU))

new_herring$decade <- factor(new_herring$decade, levels = levels(herring$decade))
new_herring$EPU <- factor(new_herring$EPU, levels = levels(herring$EPU))

predictions <- predict(herring_model, newdata = new_herring, interval = "confidence")
new_herring$predicted <- exp(predictions[, "fit"])
new_herring$lower <- exp(predictions[, "lwr"])
new_herring$upper <- exp(predictions[, "upr"])

ggplot(new_herring, aes(x = LENGTH, y = predicted, color = decade)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = decade), alpha = 0.1) +
  scale_x_log10() +
  scale_y_log10() +
  theme_light() +
  labs(title = "Atlantic Herring Predicted Weights by Length",
       x = "Length",
       y = "Individual Weight") +
  facet_wrap(~EPU)

herring.conts.epu <- emmeans(herring_model, ~EPU, by = "LENGTH", data=herring)
pairs(herring.conts.epu)

herring.conts.dec <- emmeans(herring_model, ~decade, by = "LENGTH", data=herring)
pairs(herring.conts.dec)

# -----------------------------------------------------------------------------
# Spiny Dogfish - Equilibrium
# -----------------------------------------------------------------------------
dogfish <- lw_data %>%
  filter(COMNAME == 'SPINY DOGFISH') %>%
  filter(INDWT < 4) %>%
  filter(INDWT != 0) %>%
  mutate(decade = case_when(YEAR >= 2014 ~ "2014 - 2023",
                            YEAR >= 2003 & YEAR < 2014 ~ "2003 - 2013",
                            YEAR <= 2002 ~ "1992 - 2002")) %>%
  filter(!is.na(EPU))

### Spiny dogfish Data Plot
dogfish_plot <- dogfish %>%
  ggplot(aes(x = LENGTH, y = INDWT))+
  geom_point(aes(color=decade)) +
  facet_wrap(~EPU)+
  labs(title = "Spiny Dogfish: Length vs Weight",
       x = "Length",
       y = "Individual Weight")+
  theme_bw()
print(dogfish_plot)

dogfish_model <- lm(log(INDWT) ~ log(LENGTH)*decade + log(LENGTH)*EPU, data=dogfish)
summary(dogfish_model)
a.by.10yrs["Spiny Dogfish", "1992 - 2002"] <- dogfish_model$coefficients[1] + dogfish_model$coefficients[5]
b.by.10yrs["Spiny Dogfish", "1992 - 2002"] <- dogfish_model$coefficients[2] + dogfish_model$coefficients[10]
a.by.10yrs["Spiny Dogfish", "2003 - 2013"] <- dogfish_model$coefficients[1] + dogfish_model$coefficients[5] + dogfish_model$coefficients[3]
b.by.10yrs["Spiny Dogfish", "2003 - 2013"] <- dogfish_model$coefficients[2] + dogfish_model$coefficients[10] + dogfish_model$coefficients[8]
a.by.10yrs["Spiny Dogfish", "2014 - 2023"] <- dogfish_model$coefficients[1] + dogfish_model$coefficients[5] + dogfish_model$coefficients[4]
b.by.10yrs["Spiny Dogfish", "2014 - 2023"] <- dogfish_model$coefficients[2] + dogfish_model$coefficients[10] + dogfish_model$coefficients[9]

# Spiny Dogfish Predictions Plot - by EPU
dogfish$decade <- factor(dogfish$decade)
dogfish$EPU <- factor(dogfish$EPU)
new_dogfish <- expand.grid(LENGTH = seq(min(dogfish$LENGTH), max(dogfish$LENGTH), length.out = 100), 
                          decade = levels(dogfish$decade),
                          EPU = levels(dogfish$EPU))

new_dogfish$decade <- factor(new_dogfish$decade, levels = levels(dogfish$decade))
new_dogfish$EPU <- factor(new_dogfish$EPU, levels = levels(dogfish$EPU))

predictions <- predict(dogfish_model, newdata = new_dogfish, interval = "confidence")

new_dogfish$predicted <- exp(predictions[, "fit"])
new_dogfish$lower <- exp(predictions[, "lwr"])
new_dogfish$upper <- exp(predictions[, "upr"])

ggplot(new_dogfish, aes(x = LENGTH, y = predicted, color = decade)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = decade), alpha = 0.1) +
  scale_x_log10() +
  scale_y_log10() +
  theme_classic() +
  labs(title = "Spiny Dogfish Predicted Weights by Length",
       x = "Length",
       y = "Individual Weight") +
  facet_wrap(~EPU)

dogfish.conts.epu <- emmeans(dogfish_model, ~EPU, by = "LENGTH", data=dogfish)
pairs(dogfish.conts.epu)

dogfish.conts.dec <- emmeans(dogfish_model, ~decade, by = "LENGTH", data=dogfish)
pairs(dogfish.conts.dec)

### ---------------------------------------------------------------------------
### Plot of Slope Changes
### ---------------------------------------------------------------------------

b.by.10yrs.df <- as.data.frame(b.by.10yrs, stringsAsFactors = FALSE)
b.by.10yrs.df$Species <- rownames(b.by.10yrs)

b.by.10yrs.df.long <- pivot_longer(b.by.10yrs.df, 
                            cols = c("1992 - 2002", "2003 - 2013", "2014 - 2023"), 
                            names_to = "Decade", 
                            values_to = "Slope")

ggplot(b.by.10yrs.df.long, aes(x = Decade, y = Slope, group = Species, color = Species)) +
  geom_line() +
  geom_point() +
  labs(title = "Slope of Relationship Between log(Length) and log(Weight) in the Gulf of Maine", 
       x = "Decade", 
       y = "Slope (b)") +
  theme_minimal()

### ---------------------------------------------------------------------------
### Coefficients 
### ---------------------------------------------------------------------------
