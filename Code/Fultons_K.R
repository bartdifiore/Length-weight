# -----------------------------------------------------------------------------
# Fulton's K Analysis
# -----------------------------------------------------------------------------
lw_data <- readRDS('Data/length_weight_epu.rds') #loading in data
library(tidyverse)

k_data <- lw_data %>%
  filter(COMNAME %in% c("ATLANTIC COD", "AMERICAN PLAICE", "ATLANTIC HERRING",
                        "SPINY DOGFISH")) %>%
  filter(!(INDWT < 0.001)) %>%
  filter(!(is.na(EPU) == TRUE)) %>%
  filter(!(COMNAME == "ATLANTIC HERRING" & INDWT > 0.6)) %>%
  filter(!(COMNAME == "AMERICAN PLAICE" & INDWT > 4)) %>%
  mutate(weight = INDWT * 1000) %>%
  mutate(K = (100 * (weight / (LENGTH^3)))) %>%
  dplyr::select(YEAR,COMNAME,EPU,LENGTH,INDWT,K)

cod_k <- k_data %>%
  filter(COMNAME == "ATLANTIC COD")
cod_km <- lm(K ~ YEAR*EPU, cod_k)
summary(cod_km)
ggplot(cod_k, aes(x=YEAR, y=K)) +
  geom_point()

plaice_k <- k_data %>%
  filter(COMNAME == "AMERICAN PLAICE")
plaice_km <- lm(K ~ YEAR*EPU, plaice_k)
summary(plaice_km)

herring_k <- k_data %>%
  filter(COMNAME == "ATLANTIC HERRING")
herring_km <- lm(K ~ YEAR*EPU, herring_k)
summary(herring_km)

dog_k <- k_data %>%
  filter(COMNAME == "SPINY DOGFISH")
dog_km <- lm(K ~ YEAR*EPU, dog_k)
summary(dog_km)

### On Tow Scale - WIP
tow_data <- lw_data %>%
  filter(COMNAME == "ATLANTIC COD") %>%
  filter(!(INDWT < 0.001)) %>%
  filter(!(is.na(EPU) == TRUE)) %>%
  mutate(weight = INDWT * 1000) %>%
  mutate(K = (100 * (weight / (LENGTH^3)))) %>%
  select(TOW,YEAR,COMNAME,EPU,LENGTH,INDWT,K) 
tow_data$YEAR_TOW <- paste(tow_data$YEAR, tow_data$TOW, sep = "-")
# need to make continuous - change tow from 1-20 per year to 1-20, 21-41, ...