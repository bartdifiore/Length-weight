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




#-----------------------------------------------
## Mixed effects model practice
#------------------------------------------------

library(lme4)
library(lmerTest)

df <- k_data %>% 
  janitor::clean_names()

df %>% group_by(comname) %>%
  summarize(mean_k = mean(k))


lm1 <- lm(k ~ year * comname, data = df)
summary(lm1)

# ggplot(df, aes(x = year, y = k))+
#   geom_point()+
#   geom_smooth(method = "lm")

lmer1 <- lmer(k ~ year + (1|comname), data = df)
summary(lmer1)
ranef(lmer1)

library(ggeffects)

newdata <- expand.grid(year = unique(df$year), comname = unique(df$comname))
newdata$predicted_k <- predict(lmer1, newdata = newdata, re.form = ~(1|comname))
newdata$predict_k_pop <- predict(lmer1, newdata = newdata, re.form = NA)

p1 <- ggplot(newdata, aes(x = year, y = predicted_k))+
  geom_line(aes(color = comname))+
  geom_line(aes(x =year, y = predict_k_pop), col = "black", linewidth = 2)+
  coord_cartesian(ylim = c(0, 1))+
  labs(title = "Mixed effects")+
  theme_classic()


# p1 <- plot(ggpredict(lmer1, terms = ~(1|comname)), ci = F)+
#   coord_cartesian(ylim = c(0, 1))+
#   labs(title = "Mixed effects")
# p1

p2 <- plot(ggpredict(lm1, terms = ~year+comname, ci.lvl = 0.95))+
  coord_cartesian(ylim = c(0, 1))+
  labs(title = "linear regression")
p2

plot(ggpredict(lmer1, terms = ~year + comname), ci = F)

cowplot::plot_grid(p2, p1, nrow = 1)
































