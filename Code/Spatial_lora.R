# -----------------------------------------------------------------------------
# Spatial Analysis
# -----------------------------------------------------------------------------
library(tidyverse)
lw_spatial <- readRDS('Data/length_weight_epu.rds')
lw_spatial <- lw_spatial %>%
  filter(INDWT != 0 & INDWT < 750) %>%
  filter(!is.na(EPU)) %>%
  mutate(decade = case_when(YEAR < 2000 ~ "1990s",
                            YEAR >= 2000 & YEAR < 2010 ~ "2000s",
                            YEAR >= 2010 & YEAR < 2020 ~ "2010s",
                            YEAR >= 2020 ~ "2020s"))
# create tables to hold parameter values
b.by.decade <- matrix(nrow=4, ncol=4, byrow=TRUE)
rownames(b.by.decade) <- 
  c('Cod', 'American Plaice', 'Atlantic Herring', 'Spiny Dogfish')
colnames(b.by.decade) <- 
  c('1990s', '2000s', '2010s', '2020s')

a.by.decade <- matrix(nrow=4, ncol=4, byrow=TRUE)
rownames(a.by.decade) <- 
  c('Cod', 'American Plaice', 'Atlantic Herring', 'Spiny Dogfish')
colnames(a.by.decade) <- 
  c('1990s', '2000s', '2010s', '2020s')

# -----------------------------------------------------------------------------
# All Species, All EPUs
# -----------------------------------------------------------------------------

ggplot(data = lw_spatial, aes(x = LENGTH, y=INDWT)) +
  geom_point() +
  facet_grid(rows=vars(decade), cols=vars(EPU)) +
  theme_bw()

model <- lm(log(INDWT) ~ log(LENGTH)*decade + log(LENGTH)*EPU, data = lw_spatial)
summary(model)

# -----------------------------------------------------------------------------
# All Species, Gulf of Maine only
# -----------------------------------------------------------------------------

gom <- lw_spatial %>%
  filter(EPU == "GOM") %>%
  filter(INDWT != 0 & INDWT < 60)

ggplot(data = gom, aes(x=LENGTH, y=INDWT)) +
  geom_point() +
  facet_wrap(~decade) +
  theme_bw()

gom_model <- lm(log(INDWT) ~ log(LENGTH)*decade, data=gom)
summary(gom_model)

# -----------------------------------------------------------------------------
# Atlantic Cod
# -----------------------------------------------------------------------------
cod <- lw_spatial %>%
  filter(COMNAME == "ATLANTIC COD")

ggplot(data=cod, aes(x=LENGTH, y=INDWT)) +
  theme_bw() +
  facet_wrap(~EPU) +
  geom_point(aes(color=decade))

cod_model <- lm(log(INDWT) ~ log(LENGTH)*EPU + log(LENGTH)*decade, data = cod)
summary(cod_model)

cod.conts.epu <- emmeans(cod_model, ~EPU, by = "LENGTH", data=cod)
pairs(cod.conts.epu)

cod.conts.dec <- emmeans(cod_model, ~decade, by = "LENGTH", data=cod)
pairs(cod.conts.dec)

cod_gom <- cod %>%
  filter(EPU == "GOM")

ggplot(data = cod_gom, aes(x = LENGTH, y = INDWT))+
  geom_point(aes(color=decade)) +
  labs(title = "Atlantic Cod in the Gulf of Maine: Length vs Weight",
       x = "Length",
       y = "Individual Weight") +
  theme_bw()

ggplot(data = cod_gom, aes(x = LENGTH, y = INDWT))+
  geom_point() +
  facet_wrap(~decade) +
  labs(title = "Atlantic Cod in the Gulf of Maine: Length vs Weight",
       x = "Length",
       y = "Individual Weight") +
  theme_bw()

cod_gom_model <- lm(log(INDWT) ~ log(LENGTH)*decade, data = cod_gom)
summary(cod_gom_model)

a.by.decade["Cod", "1990s"] <- cod_gom_model$coefficients[1]
b.by.decade["Cod", "1990s"] <- cod_gom_model$coefficients[2]
a.by.decade["Cod", "2000s"] <- cod_gom_model$coefficients[1] + cod_gom_model$coefficients[3]
b.by.decade["Cod", "2000s"] <- cod_gom_model$coefficients[2] + cod_gom_model$coefficients[6]
a.by.decade["Cod", "2010s"] <- cod_gom_model$coefficients[1] + cod_gom_model$coefficients[4]
b.by.decade["Cod", "2010s"] <- cod_gom_model$coefficients[2] + cod_gom_model$coefficients[7]
a.by.decade["Cod", "2020s"] <- cod_gom_model$coefficients[1] + cod_gom_model$coefficients[5]
b.by.decade["Cod", "2020s"] <- cod_gom_model$coefficients[2] + cod_gom_model$coefficients[8]

cod.conts.gom <- emmeans(cod_gom_model, ~decade, by = "LENGTH", data=cod_gom)
pairs(cod.conts.gom)

# -----------------------------------------------------------------------------
# American Plaice
# -----------------------------------------------------------------------------
plaice <- lw_spatial %>%
  filter(COMNAME == "AMERICAN PLAICE") %>%
  filter(INDWT < 4)

ggplot(data=plaice, aes(x=LENGTH, y=INDWT)) +
  theme_bw() +
  facet_wrap(~EPU) +
  geom_point(aes(color=decade))

plaice_model <- lm(log(INDWT) ~ log(LENGTH)*EPU + log(LENGTH)*decade, data = plaice)
summary(plaice_model)

plaice.conts.epu <- emmeans(plaice_model, ~EPU, by = "LENGTH", data=plaice)
pairs(plaice.conts.epu)

plaice.conts.dec <- emmeans(plaice_model, ~decade, by = "LENGTH", data=plaice)
pairs(plaice.conts.dec)

plaice_gom <- plaice %>%
  filter(EPU == "GOM") %>%

ggplot(data = plaice_gom, aes(x = LENGTH, y = INDWT))+
  geom_point(aes(color=decade)) +
  labs(title = "American Plaice in the Gulf of Maine: Length vs Weight",
       x = "Length",
       y = "Individual Weight") +
  theme_bw()

ggplot(data = plaice_gom, aes(x = LENGTH, y = INDWT))+
  geom_point() +
  facet_wrap(~decade) +
  labs(title = "American Plaice in the Gulf of Maine: Length vs Weight",
       x = "Length",
       y = "Individual Weight") +
  theme_bw()

plaice_gom_model <- lm(log(INDWT) ~ log(LENGTH)*decade, data = plaice_gom)
summary(plaice_gom_model)

a.by.decade["American Plaice", "1990s"] <- plaice_gom_model$coefficients[1]
b.by.decade["American Plaice", "1990s"] <- plaice_gom_model$coefficients[2]
a.by.decade["American Plaice", "2000s"] <- plaice_gom_model$coefficients[1] + plaice_gom_model$coefficients[3]
b.by.decade["American Plaice", "2000s"] <- plaice_gom_model$coefficients[2] + plaice_gom_model$coefficients[6]
a.by.decade["American Plaice", "2010s"] <- plaice_gom_model$coefficients[1] + plaice_gom_model$coefficients[4]
b.by.decade["American Plaice", "2010s"] <- plaice_gom_model$coefficients[2] + plaice_gom_model$coefficients[7]
a.by.decade["American Plaice", "2020s"] <- plaice_gom_model$coefficients[1] + plaice_gom_model$coefficients[5]
b.by.decade["American Plaice", "2020s"] <- plaice_gom_model$coefficients[2] + plaice_gom_model$coefficients[8]

plaice.conts.gom <- emmeans(plaice_gom_model, ~decade, by = "LENGTH", data=plaice_gom)
pairs(plaice.conts.gom)

# -----------------------------------------------------------------------------
# Atlantic Herring
# -----------------------------------------------------------------------------
herring <- lw_spatial %>%
  filter(COMNAME == "ATLANTIC HERRING") %>%
  filter(INDWT < 0.75)

ggplot(data=herring, aes(x=LENGTH, y=INDWT)) +
  theme_bw() +
  facet_wrap(~EPU) +
  geom_point(aes(color=decade))

herring_model <- lm(log(INDWT) ~ log(LENGTH)*EPU + log(LENGTH)*decade, data = herring)
summary(herring_model)

herring.conts.epu <- emmeans(herring_model, ~EPU, by = "LENGTH", data=herring)
pairs(herring.conts.epu)

herring.conts.dec <- emmeans(herring_model, ~decade, by = "LENGTH", data=herring)
pairs(herring.conts.dec)

herring_gom <- herring %>%
  filter(EPU == "GOM")

ggplot(data = herring_gom, aes(x = LENGTH, y = INDWT))+
  geom_point(aes(color=decade)) +
  labs(title = "Atlantic Herring in the Gulf of Maine: Length vs Weight",
       x = "Length",
       y = "Individual Weight") +
  theme_bw()

ggplot(data = herring_gom, aes(x = LENGTH, y = INDWT))+
  geom_point() +
  facet_wrap(~decade) +
  labs(title = "Atlantic Herring in the Gulf of Maine: Length vs Weight",
       x = "Length",
       y = "Individual Weight") +
  theme_bw()

herring_gom_model <- lm(log(INDWT) ~ log(LENGTH)*decade, data = herring_gom)
summary(herring_gom_model)

a.by.decade["Atlantic Herring", "1990s"] <- herring_gom_model$coefficients[1]
b.by.decade["Atlantic Herring", "1990s"] <- herring_gom_model$coefficients[2]
a.by.decade["Atlantic Herring", "2000s"] <- herring_gom_model$coefficients[1] + herring_gom_model$coefficients[3]
b.by.decade["Atlantic Herring", "2000s"] <- herring_gom_model$coefficients[2] + herring_gom_model$coefficients[6]
a.by.decade["Atlantic Herring", "2010s"] <- herring_gom_model$coefficients[1] + herring_gom_model$coefficients[4]
b.by.decade["Atlantic Herring", "2010s"] <- herring_gom_model$coefficients[2] + herring_gom_model$coefficients[7]
a.by.decade["Atlantic Herring", "2020s"] <- herring_gom_model$coefficients[1] + herring_gom_model$coefficients[5]
b.by.decade["Atlantic Herring", "2020s"] <- herring_gom_model$coefficients[2] + herring_gom_model$coefficients[8]

herring.conts.gom <- emmeans(herring_gom_model, ~decade, by = "LENGTH", data=herring_gom)
pairs(herring.conts.gom)

# -----------------------------------------------------------------------------
# Spiny Dogfish
# -----------------------------------------------------------------------------
dogfish <- lw_spatial %>%
  filter(COMNAME == "SPINY DOGFISH")

ggplot(data=dogfish, aes(x=LENGTH, y=INDWT)) +
  theme_bw() +
  facet_wrap(~EPU) +
  geom_point(aes(color=decade))

dogfish_model <- lm(log(INDWT) ~ log(LENGTH)*EPU + log(LENGTH)*decade, data = dogfish)
summary(dogfish_model)

dogfish.conts.epu <- emmeans(dogfish_model, ~EPU, by = "LENGTH", data=dogfish)
pairs(dogfish.conts.epu)

dogfish.conts.dec <- emmeans(dogfish_model, ~decade, by = "LENGTH", data=dogfish)
pairs(dogfish.conts.dec)

dogfish_gom <- dogfish %>%
  filter(EPU == "GOM") %>%
  filter(INDWT < 4)

ggplot(data = dogfish_gom, aes(x = LENGTH, y = INDWT))+
  geom_point(aes(color=decade)) +
  labs(title = "Spiny dogfish in the Gulf of Maine: Length vs Weight",
       x = "Length",
       y = "Individual Weight") +
  theme_bw()

ggplot(data = dogfish_gom, aes(x = LENGTH, y = INDWT))+
  geom_point() +
  facet_wrap(~decade) +
  labs(title = "Spiny dogfish in the Gulf of Maine: Length vs Weight",
       x = "Length",
       y = "Individual Weight") +
  theme_bw()

dogfish_gom_model <- lm(log(INDWT) ~ log(LENGTH)*decade, data = dogfish_gom)
summary(dogfish_gom_model)

a.by.decade["Spiny Dogfish", "1990s"] <- dogfish_gom_model$coefficients[1]
b.by.decade["Spiny Dogfish", "1990s"] <- dogfish_gom_model$coefficients[2]
a.by.decade["Spiny Dogfish", "2000s"] <- dogfish_gom_model$coefficients[1] + dogfish_gom_model$coefficients[3]
b.by.decade["Spiny Dogfish", "2000s"] <- dogfish_gom_model$coefficients[2] + dogfish_gom_model$coefficients[6]
a.by.decade["Spiny Dogfish", "2010s"] <- dogfish_gom_model$coefficients[1] + dogfish_gom_model$coefficients[4]
b.by.decade["Spiny Dogfish", "2010s"] <- dogfish_gom_model$coefficients[2] + dogfish_gom_model$coefficients[7]
a.by.decade["Spiny Dogfish", "2020s"] <- dogfish_gom_model$coefficients[1] + dogfish_gom_model$coefficients[5]
b.by.decade["Spiny Dogfish", "2020s"] <- dogfish_gom_model$coefficients[2] + dogfish_gom_model$coefficients[8]

dogfish.conts.gom <- emmeans(dogfish_gom_model, ~decade, by = "LENGTH", data=dogfish_gom)
pairs(dogfish.conts.gom)

# -----------------------------------------------------------------------------
# GoM parameters, visualized
# -----------------------------------------------------------------------------
print(b.by.decade)

b.to.df <- data.frame(
  Species = c("Cod", "American Plaice", "Atlantic Herring", "Spiny Dogfish"),
  `1990s` = c(3.119827, 3.321450, 3.101192, 3.086833),
  `2000s` = c(3.133303, 3.358385, 3.312612, 3.319610),
  `2010s` = c(3.083302, 3.174635, 3.086706, 3.008332),
  `2020s` = c(3.072278, 3.146342, 3.109407, 2.989787),
  stringsAsFactors = FALSE
)

b.long <- pivot_longer(b.to.df, 
                      cols = starts_with("X"), 
                      names_to = "Decade", 
                      values_to = "Slope")

ggplot(b.long, aes(x = Decade, y = Slope, group = Species, color = Species)) +
  geom_line() +
  geom_point() +
  labs(title = "Slope of Relationship Between log(Length) and log(Weight) By Decade in the Gulf of Maine", 
       x = "Decade", y = "Slope (b)") +
  theme_minimal()



