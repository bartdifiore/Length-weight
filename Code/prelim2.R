library(tidyverse)
lw_data <- readRDS('GMRI/data/bottomTrawlSurvey_indLengthWeight.rds')
lw_coeffs <- readRDS('GMRI/data/lw_coeffs.rds')

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

# ************************* COD ********************************************
cod <- lw_data %>%
  filter(COMNAME == 'ATLANTIC COD') %>%
  filter(INDWT != 0) %>%
  mutate(decade = case_when(YEAR >= 2020 ~ "2020s",
                            YEAR >= 2010 & YEAR < 2020 ~ "2010s",
                            YEAR >= 2000 & YEAR < 2010 ~ "2000s", 
                            YEAR < 2000 ~ "1990s"))

### ORIGINAL DATA PLOT
cod_orig <- cod %>%
  ggplot(aes(x = LENGTH, y = INDWT))+
  geom_point() +
  labs(title = "Atlantic Cod: Length vs Weight",
       x = "Length",
       y = "Individual Weight")+
  theme_bw()
print(cod_orig)

### TRANSFORMED DATA PLOT
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

### ORIGINAL DATA - COLORED BY DECADE
cod_dec.by.color.orig <- cod %>%
  ggplot(aes(x = LENGTH, y = INDWT)) +
  geom_point(aes(color = decade))+
  labs(title = "Atlantic Cod: Length vs Weight",
       x = "Length",
       y = "Individual Weight")+
  theme_bw()
print(cod_dec.by.color.orig)

### TRANSFORMED DATA - COLORED BY DECADE
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

### ORIGINAL DATA - SPLIT BY DECADE
cod_dec.split.orig <- cod %>%
  ggplot(aes(x = LENGTH, y = INDWT)) +
  geom_point()+
  facet_wrap(~decade)+
  labs(title = "Atlantic Cod: Length vs Weight",
       x = "Length",
       y = "Individual Weight")+
  theme_bw()
print(cod_dec.split.orig)

### TRANSFORMED DATA - SPLIT BY DECADE
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

# okay. actual statistics stuff now - saving parameters
cod_90s <- cod %>%
  filter(decade == '1990s')
cod_90s_lm <- lm(log(INDWT) ~ log(LENGTH), cod_90s)
a.by.decade['Cod', '1990s'] <- summary(cod_90s_lm)$coefficients[1,1]
b.by.decade['Cod', '1990s'] <- summary(cod_90s_lm)$coefficients[2,1]

cod_00s <- cod %>%
  filter(decade == '2000s')
cod_00s_lm <- lm(log(INDWT) ~ log(LENGTH), cod_00s)
a.by.decade['Cod', '2000s'] <- summary(cod_00s_lm)$coefficients[1,1]
b.by.decade['Cod', '2000s'] <- summary(cod_00s_lm)$coefficients[2,1]

cod_10s <- cod %>%
  filter(decade == '2010s')
cod_10s_lm <- lm(log(INDWT) ~ log(LENGTH), cod_10s)
a.by.decade['Cod', '2010s'] <- summary(cod_10s_lm)$coefficients[1,1]
b.by.decade['Cod', '2010s'] <- summary(cod_10s_lm)$coefficients[2,1]

cod_20s <- cod %>%
  filter(decade == '2020s')
cod_20s_lm <- lm(log(INDWT) ~ log(LENGTH), cod_20s)
a.by.decade['Cod', '2020s'] <- summary(cod_20s_lm)$coefficients[1,1]
b.by.decade['Cod', '2020s'] <- summary(cod_20s_lm)$coefficients[2,1]

# actual model!
cod_model <- lm(log(INDWT) ~ log(LENGTH)*decade, cod)
summary(cod_model)

# generating predictions for the model
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

## *********************** AMERICAN PLAICE *********************************
plaice <- lw_data %>%
  filter(COMNAME == 'AMERICAN PLAICE') %>%
  filter(INDWT != 0) %>%
  mutate(decade = case_when(YEAR >= 2020 ~ "2020s",
                            YEAR >= 2010 & YEAR < 2020 ~ "2010s",
                            YEAR >= 2000 & YEAR < 2010 ~ "2000s", 
                            YEAR < 2000 ~ "1990s"))

### ORIGINAL DATA PLOT
plaice_orig <- plaice %>%
  ggplot(aes(x = LENGTH, y = INDWT))+
  geom_point() +
  labs(title = "American Plaice: Length vs Weight",
       x = "Length",
       y = "Individual Weight")+
  theme_bw()
print(plaice_orig)

### TRANSFORMED DATA PLOT
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

### ORIGINAL DATA - COLORED BY DECADE
plaice_dec.by.color.orig <- plaice %>%
  ggplot(aes(x = LENGTH, y = INDWT)) +
  geom_point(aes(color = decade))+
  labs(title = "American Plaice: Length vs Weight",
       x = "Length",
       y = "Individual Weight")+
  theme_bw()
print(plaice_dec.by.color.orig)

### TRANSFORMED DATA - COLORED BY DECADE
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

### ORIGINAL DATA - SPLIT BY DECADE
plaice_dec.split.orig <- plaice %>%
  ggplot(aes(x = LENGTH, y = INDWT)) +
  geom_point()+
  facet_wrap(~decade)+
  labs(title = "American Plaice: Length vs Weight",
       x = "Length",
       y = "Individual Weight")+
  theme_bw()
print(plaice_dec.split.orig)

### TRANSFORMED DATA - SPLIT BY DECADE
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

# actual statistics stuff part two! - saving parameters
plaice_90s <- plaice %>%
  filter(decade == '1990s')
plaice_90s_lm <- lm(log(INDWT) ~ log(LENGTH), plaice_90s)
a.by.decade['American Plaice', '1990s'] <- summary(plaice_90s_lm)$coefficients[1,1]
b.by.decade['American Plaice', '1990s'] <- summary(plaice_90s_lm)$coefficients[2,1]

plaice_00s <- plaice %>%
  filter(decade == '2000s')
plaice_00s_lm <- lm(log(INDWT) ~ log(LENGTH), plaice_00s)
a.by.decade['American Plaice', '2000s'] <- summary(plaice_00s_lm)$coefficients[1,1]
b.by.decade['American Plaice', '2000s'] <- summary(plaice_00s_lm)$coefficients[2,1]

plaice_10s <- plaice %>%
  filter(decade == '2010s')
plaice_10s_lm <- lm(log(INDWT) ~ log(LENGTH), plaice_10s)
a.by.decade['American Plaice', '2010s'] <- summary(plaice_10s_lm)$coefficients[1,1]
b.by.decade['American Plaice', '2010s'] <- summary(plaice_10s_lm)$coefficients[2,1]

plaice_20s <- plaice %>%
  filter(decade == '2020s')
plaice_20s_lm <- lm(log(INDWT) ~ log(LENGTH), plaice_20s)
a.by.decade['American Plaice', '2020s'] <- summary(plaice_20s_lm)$coefficients[1,1]
b.by.decade['American Plaice', '2020s'] <- summary(plaice_20s_lm)$coefficients[2,1]

# actual model!
plaice_model <- lm(log(INDWT) ~ log(LENGTH)*decade, plaice)
summary(plaice_model)

# generating predictions for the model
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

## *********************** HERRING *****************************************
herring <- lw_data %>%
  filter(COMNAME == 'ATLANTIC HERRING') %>%
  filter(INDWT != 0 & INDWT < 0.75) %>%
  mutate(decade = case_when(YEAR >= 2020 ~ "2020s",
                            YEAR >= 2010 & YEAR < 2020 ~ "2010s",
                            YEAR >= 2000 & YEAR < 2010 ~ "2000s", 
                            YEAR < 2000 ~ "1990s"))

### ORIGINAL DATA
herring_orig <- herring %>%
  ggplot(aes(x = LENGTH, y = INDWT))+
  geom_point() +
  labs(title = "Atlantic Herring: Length vs Weight",
       x = "Length",
       y = "Individual Weight")+
  theme_bw()
print(herring_orig)

### TRANSFORMED DATA
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

### ORIGINAL DATA - COLORED BY DECADE
herring_dec.by.color.orig <- herring %>%
  ggplot(aes(x = LENGTH, y = INDWT)) +
  geom_point(aes(color = decade))+
  labs(title = "Atlantic Herring: Length vs Weight",
       x = "Length",
       y = "Individual Weight")+
  theme_bw()
print(herring_dec.by.color.orig)

### TRANSFORMED DATA - COLORED BY DECADE
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

### ORIGINAL DATA - SPLIT BY DECADE
herring_dec.split.orig <- herring %>%
  ggplot(aes(x = LENGTH, y = INDWT)) +
  geom_point()+
  facet_wrap(~decade)+
  labs(title = "Atlantic Herring: Length vs Weight",
       x = "Length",
       y = "Individual Weight")+
  theme_bw()
print(herring_dec.split.orig)

### TRANSFORMED DATA - SPLIT BY DECADE
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

# actual statistics stuff again! - saving parameters
herring_90s <- herring %>%
  filter(decade == '1990s')
herring_90s_lm <- lm(log(INDWT) ~ log(LENGTH), herring_90s)
a.by.decade['Atlantic Herring', '1990s'] <- summary(herring_90s_lm)$coefficients[1,1]
b.by.decade['Atlantic Herring', '1990s'] <- summary(herring_90s_lm)$coefficients[2,1]

herring_00s <- herring %>%
  filter(decade == '2000s')
herring_00s_lm <- lm(log(INDWT) ~ log(LENGTH), herring_00s)
a.by.decade['Atlantic Herring', '2000s'] <- summary(herring_00s_lm)$coefficients[1,1]
b.by.decade['Atlantic Herring', '2000s'] <- summary(herring_00s_lm)$coefficients[2,1]

herring_10s <- herring %>%
  filter(decade == '2010s')
herring_10s_lm <- lm(log(INDWT) ~ log(LENGTH), herring_10s)
a.by.decade['Atlantic Herring', '2010s'] <- summary(herring_10s_lm)$coefficients[1,1]
b.by.decade['Atlantic Herring', '2010s'] <- summary(herring_10s_lm)$coefficients[2,1]

herring_20s <- herring %>%
  filter(decade == '2020s')
herring_20s_lm <- lm(log(INDWT) ~ log(LENGTH), herring_20s)
a.by.decade['Atlantic Herring', '2020s'] <- summary(herring_20s_lm)$coefficients[1,1]
b.by.decade['Atlantic Herring', '2020s'] <- summary(herring_20s_lm)$coefficients[2,1]

# actual model!
herring_model <- lm(log(INDWT) ~ log(LENGTH)*decade, herring)
summary(herring_model)

# generating predictions for the model
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

## *********************** SPINY DOGFISH ***********************************
dogfish <- lw_data %>%
  filter(COMNAME == 'SPINY DOGFISH') %>%
  filter(INDWT != 0) %>%
  mutate(decade = case_when(YEAR >= 2020 ~ "2020s",
                            YEAR >= 2010 & YEAR < 2020 ~ "2010s",
                            YEAR >= 2000 & YEAR < 2010 ~ "2000s", 
                            YEAR < 2000 ~ "1990s"))

### ORIGINAL DATA PLOT
dogfish_orig <- dogfish %>%
  ggplot(aes(x = LENGTH, y = INDWT))+
  geom_point() +
  labs(title = "Spiny Dogfish: Length vs Weight",
       x = "Length",
       y = "Individual Weight")+
  theme_bw()
print(dogfish_orig)

### TRANSFORMED DATA PLOT
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

### ORIGINAL DATA - COLORED BY DECADE
dogfish_dec.by.color.orig <- dogfish %>%
  ggplot(aes(x = LENGTH, y = INDWT)) +
  geom_point(aes(color = decade))+
  labs(title = "Spiny Dogfish Length vs Weight",
       x = "Length",
       y = "Individual Weight")+
  theme_bw()
print(dogfish_dec.by.color.orig)

### TRANSFORMED DATA - COLORED BY DECADE
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

### ORIGINAL DATA - SPLIT BY DECADE
dogfish_dec.split.orig <- dogfish %>%
  ggplot(aes(x = LENGTH, y = INDWT)) +
  geom_point()+
  facet_wrap(~decade)+
  labs(title = "Spiny Dogfish: Length vs Weight",
       x = "Length",
       y = "Individual Weight")+
  theme_bw()
print(dogfish_dec.split.orig)

### TRANSFORMED DATA - SPLIT BY DECADE
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

# actual statistics stuff once more! - saving parameters
dogfish_90s <- dogfish %>%
  filter(decade == '1990s')
dogfish_90s_lm <- lm(log(INDWT) ~ log(LENGTH), dogfish_90s)
a.by.decade['Spiny Dogfish', '1990s'] <- summary(dogfish_90s_lm)$coefficients[1,1]
b.by.decade['Spiny Dogfish', '1990s'] <- summary(dogfish_90s_lm)$coefficients[2,1]

dogfish_00s <- dogfish %>%
  filter(decade == '2000s')
dogfish_00s_lm <- lm(log(INDWT) ~ log(LENGTH), dogfish_00s)
a.by.decade['Spiny Dogfish', '2000s'] <- summary(dogfish_00s_lm)$coefficients[1,1]
b.by.decade['Spiny Dogfish', '2000s'] <- summary(dogfish_00s_lm)$coefficients[2,1]

dogfish_10s <- dogfish %>%
  filter(decade == '2010s')
dogfish_10s_lm <- lm(log(INDWT) ~ log(LENGTH), dogfish_10s)
a.by.decade['Spiny Dogfish', '2010s'] <- summary(dogfish_10s_lm)$coefficients[1,1]
b.by.decade['Spiny Dogfish', '2010s'] <- summary(dogfish_10s_lm)$coefficients[2,1]

dogfish_20s <- dogfish %>%
  filter(decade == '2020s')
dogfish_20s_lm <- lm(log(INDWT) ~ log(LENGTH), dogfish_20s)
a.by.decade['Spiny Dogfish', '2020s'] <- summary(dogfish_20s_lm)$coefficients[1,1]
b.by.decade['Spiny Dogfish', '2020s'] <- summary(dogfish_20s_lm)$coefficients[2,1]

# actual model!
dogfish_model <- lm(log(INDWT) ~ log(LENGTH)*decade, dogfish)
summary(dogfish_model)

# generating predictions for the model
new_dogfish = expand.grid(LENGTH = seq(min(dogfish$LENGTH), max(dogfish$LENGTH), length.out = 100), decade = c("1990s", "2000s", "2010s", "2020s")) 
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

### ****************** TABLES **************************
print(b.by.decade)

b.to.df <- data.frame(
  Species = c("Cod", "American Plaice", "Atlantic Herring", "Spiny Dogfish"),
  `1990s` = c(3.075384, 3.281383, 3.048610, 3.124006),
  `2000s` = c(3.104295, 3.341870, 3.126295, 3.179436),
  `2010s` = c(3.083302, 3.304652, 3.035329, 3.118535),
  `2020s` = c(3.072278, 3.310441, 3.087207, 3.011393),
  stringsAsFactors = FALSE
)

b.long <- pivot_longer(b.to.df, 
                       cols = starts_with("X"), 
                       names_to = "Decade", 
                       values_to = "Slope")

ggplot(b.long, aes(x = Decade, y = Slope, group = Species, color = Species)) +
  geom_line() +
  geom_point() +
  labs(title = "Slope of Relationship Between log(Length) and log(Weight) By Decade", x = "Decade", y = "Slope (b)") +
  theme_minimal()

