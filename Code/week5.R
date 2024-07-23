### ---------------------------------------------------------------------------
### Time_Periods broken up in 10 year increments, EPU prediction plots
### ---------------------------------------------------------------------------
library(tidyverse) # loading in packages & data
library(emmeans)

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
# Atlantic Cod - Time_Periodic Fish
# -----------------------------------------------------------------------------
cod <- lw_data %>%
  filter(COMNAME == 'ATLANTIC COD') %>%
  filter(INDWT != 0) %>%
  mutate(Time_Period = case_when(YEAR >= 2014 ~ "2014 - 2023",
                            YEAR >= 2003 & YEAR < 2014 ~ "2003 - 2013",
                            YEAR <= 2002 ~ "1992 - 2002")) %>%
  filter(!is.na(EPU))

### Cod Data Plot
cod_plot <- cod %>%
  ggplot(aes(x = LENGTH, y = INDWT))+
  geom_point(aes(color=Time_Period)) +
  facet_wrap(~EPU)+
  labs(title = "Atlantic Cod: Length vs. Weight",
       x = "Length",
       y = "Individual Weight")+
  theme_bw()+
  theme(legend.box.background = element_rect(color="black"),
        legend.title = element_text(size = 15), 
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5)) +
  scale_color_manual(name = "Time Period",  values = c("orangered2", "yellowgreen", "steelblue2")) 
print(cod_plot)

### Cod Data Plot, not faceted
cod_plot_all <- cod %>%
  ggplot(aes(x = LENGTH, y = INDWT))+
  geom_point(aes(color=Time_Period)) +
  labs(title = "Atlantic Cod: Length vs. Weight",
       x = "Length",
       y = "Individual Weight")+
  theme_bw()+
  theme(legend.box.background = element_rect(color="black"),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5)) +
  scale_color_manual(name = "Time Period",  values = c("orangered2", "yellowgreen", "steelblue2")) 
print(cod_plot_all)

### Just Cod in GOM Plot
cod_gom <- cod %>%
  filter(EPU == "GOM") %>%
  ggplot(aes(x = LENGTH, y = INDWT))+
  geom_point(aes(color=Time_Period)) +
  labs(title = "Atlantic Cod in the Gulf of Maine: Length vs. Weight",
       x = "Length",
       y = "Individual Weight")+
  theme_bw()+
  theme(legend.box.background = element_rect(color="black"),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5)) +
  scale_color_manual(name = "Time Period",  values = c("orangered2", "yellowgreen", "steelblue2"))
print(cod_gom)

### Cod GOM Log Plot
cod_gom_log <- cod %>%
  filter(EPU == "GOM") %>%
  ggplot(aes(x = LENGTH, y = INDWT))+
  geom_point(aes(color=Time_Period)) +
  labs(title = "Atlantic Cod: Length vs Weight in the Gulf of Maine",
       x = "log(Length)",
       y = "log(Individual Weight)")+
  theme_bw()+
  scale_x_log10()+
  scale_y_log10()+
  theme(legend.box.background = element_rect(color="black"),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5)) +
  scale_color_manual(name = "Time Period",  values = c("orangered2", "yellowgreen", "steelblue2"))
print(cod_gom_log)

### Cod Model
cod_model <- lm(log(INDWT) ~ log(LENGTH)*Time_Period + log(LENGTH)*EPU, data=cod)
summary(cod_model)
a.by.10yrs["Cod", "1992 - 2002"] <- cod_model$coefficients[1] + cod_model$coefficients[5]
b.by.10yrs["Cod", "1992 - 2002"] <- cod_model$coefficients[2] + cod_model$coefficients[10]
a.by.10yrs["Cod", "2003 - 2013"] <- cod_model$coefficients[1] + cod_model$coefficients[5] + cod_model$coefficients[3]
b.by.10yrs["Cod", "2003 - 2013"] <- cod_model$coefficients[2] + cod_model$coefficients[10] + cod_model$coefficients[8]
a.by.10yrs["Cod", "2014 - 2023"] <- cod_model$coefficients[1] + cod_model$coefficients[5] + cod_model$coefficients[4]
b.by.10yrs["Cod", "2014 - 2023"] <- cod_model$coefficients[2] + cod_model$coefficients[10] + cod_model$coefficients[9]

### Cod Model 2 - Includes Triple Interaction Term
cod_model2 <- lm(log(INDWT) ~ log(LENGTH)*Time_Period*EPU, data=cod)
summary(cod_model2)
# comparing slopes for each EPU by Time_Period
em_cod <- emtrends(cod_model2, ~EPU + Time_Period, "log(LENGTH)", data = cod)
pairs(em_cod, simple = "Time_Period")

# EPU DF
cod.epu.table <- matrix(nrow=4, ncol=3, byrow=TRUE)
rownames(cod.epu.table) <- c('GB', 'GOM', 'MAB', 'SS')
colnames(cod.epu.table) <- c('1992 - 2002', '2003 - 2013', '2014 - 2023')
# GB
cod.epu.table[1, 1] <- cod_model2$coefficients[2]
cod.epu.table[1, 2] <- cod_model2$coefficients[2] + cod_model2$coefficients[8]
cod.epu.table[1, 3] <- cod_model2$coefficients[2] + cod_model2$coefficients[9]
# GOM
cod.epu.table[2, 1]<-cod_model2$coefficients[2]+cod_model2$coefficients[10]
cod.epu.table[2, 2]<-cod_model2$coefficients[2]+cod_model2$coefficients[10]+cod_model2$coefficients[8]+cod_model2$coefficients[19]
cod.epu.table[2, 3]<-cod_model2$coefficients[2]+cod_model2$coefficients[10]+cod_model2$coefficients[9]+cod_model2$coefficients[20]
# MAB
cod.epu.table[3, 1]<-cod_model2$coefficients[2]+cod_model2$coefficients[11]
cod.epu.table[3, 2]<-cod_model2$coefficients[2]+cod_model2$coefficients[11]+cod_model2$coefficients[8]+cod_model2$coefficients[21]
cod.epu.table[3, 3]<-cod_model2$coefficients[2]+cod_model2$coefficients[11]+cod_model2$coefficients[9]+cod_model2$coefficients[22]
# SS
cod.epu.table[4, 1]<-cod_model2$coefficients[2]+cod_model2$coefficients[12]
cod.epu.table[4, 2]<-cod_model2$coefficients[2]+cod_model2$coefficients[12]+cod_model2$coefficients[8]+cod_model2$coefficients[23]
cod.epu.table[4, 3]<-cod_model2$coefficients[2]+cod_model2$coefficients[12]+cod_model2$coefficients[9]+cod_model2$coefficients[24]

# graphs by EPU
cod.epu.df <- as.data.frame(cod.epu.table, stringsAsFactors = FALSE)
cod.epu.df$EPU <- rownames(cod.epu.table)
cod.epu.df.long <- pivot_longer(cod.epu.df, 
                            cols = c("1992 - 2002", "2003 - 2013", "2014 - 2023"), 
                            names_to = "Time_Period", 
                            values_to = "Slope")
ggplot(cod.epu.df.long, aes(x = Time_Period, y = Slope, group = EPU, color = EPU)) +
  geom_line() +
  geom_point() +
  labs(title = "Slope of Relationship between log(Length) and log(Length) in Atlantic Cod", 
       x = "Time_Period", 
       y = "Slope (b)") +
  theme_minimal() +
  theme(legend.box.background = element_rect(color="black"),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 15)) +
  scale_color_manual(name = "EPU",  values = c("orangered2", "yellowgreen", "steelblue2", "purple"))

### Cod Predictions Plot With Plotted Points
cod_vec_size <- seq(min(cod$LENGTH), max(cod$LENGTH), length.out = 100)
cod_predictions <- as.data.frame(ggeffects::ggpredict(cod_model2, terms = c("LENGTH [cod_vec_size]", "Time_Period",  "EPU")))
ggplot(cod, aes(x = LENGTH, y = INDWT))+
  geom_point(aes(color = Time_Period), size = 0.25, alpha = 0.5)+
  geom_ribbon(data = cod_predictions, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high, group = group), alpha = 0.1)+
  geom_line(data = cod_predictions, aes(x = x, y = predicted, color = group), linewidth = 1)+
  facet_wrap(~facet)+
  labs(title = "Atlantic Cod Length vs. Weight by EPU with Predicted Points Line", 
       x = "Length (cm)",
       y = "Individual Weight (kg)") +
  theme_bw()+
  theme(legend.box.background = element_rect(color="black"),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 15)) +
  scale_color_manual(name = "Time Period",  values = c("orangered2", "yellowgreen", "steelblue2"))

# -----------------------------------------------------------------------------
# American Plaice - Time_Periodic Fish (Pt 2)
# -----------------------------------------------------------------------------
plaice <- lw_data %>%
  filter(COMNAME == 'AMERICAN PLAICE') %>%
  filter(INDWT < 4) %>%
  filter(INDWT != 0) %>%
  filter(EPU != "MAB") %>%
  mutate(Time_Period = case_when(YEAR >= 2014 ~ "2014 - 2023",
                            YEAR >= 2003 & YEAR < 2014 ~ "2003 - 2013",
                            YEAR <= 2002 ~ "1992 - 2002")) %>%
  filter(!is.na(EPU))

### American Plaice Data Plot
plaice_plot <- plaice %>%
  ggplot(aes(x = LENGTH, y = INDWT))+
  geom_point(aes(color=Time_Period)) +
  facet_wrap(~EPU)+
  labs(title = "American Plaice: Length vs. Weight",
       x = "Length",
       y = "Individual Weight")+
  theme_bw()+
  theme(legend.box.background = element_rect(color="black"),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5)) +
  scale_color_manual(name = "Time Period",  values = c("orangered2", "yellowgreen", "steelblue2"))
print(plaice_plot)

### American Plaice Data Plot, not faceted
plaice_plot_all <- plaice %>%
  ggplot(aes(x = LENGTH, y = INDWT))+
  geom_point(aes(color=Time_Period)) +
  labs(title = "American Plaice: Length vs Weight",
       x = "Length",
       y = "Individual Weight")+
  theme_bw()+
  theme(legend.box.background = element_rect(color="black"),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5)) +
  scale_color_manual(name = "Time Period",  values = c("orangered2", "yellowgreen", "steelblue2"))
print(plaice_plot_all)

plaice_model <- lm(log(INDWT) ~ log(LENGTH)*Time_Period + log(LENGTH)*EPU, data=plaice)
summary(plaice_model)
a.by.10yrs["American Plaice", "1992 - 2002"] <- plaice_model$coefficients[1] + plaice_model$coefficients[5]
b.by.10yrs["American Plaice", "1992 - 2002"] <- plaice_model$coefficients[2] + plaice_model$coefficients[9]
a.by.10yrs["American Plaice", "2003 - 2013"] <- plaice_model$coefficients[1] + plaice_model$coefficients[5] + plaice_model$coefficients[3]
b.by.10yrs["American Plaice", "2003 - 2013"] <- plaice_model$coefficients[2] + plaice_model$coefficients[9] + plaice_model$coefficients[7]
a.by.10yrs["American Plaice", "2014 - 2023"] <- plaice_model$coefficients[1] + plaice_model$coefficients[5] + plaice_model$coefficients[4]
b.by.10yrs["American Plaice", "2014 - 2023"] <- plaice_model$coefficients[2] + plaice_model$coefficients[9] + plaice_model$coefficients[8]

# Plaice Model 2 - Includes Triple Interaction Term
plaice_model2 <- lm(log(INDWT) ~ log(LENGTH)*Time_Period*EPU, data=plaice)
summary(plaice_model2)
# comparing slopes for each EPU by Time_Period
em_plaice <- emtrends(plaice_model2, ~EPU + Time_Period, "log(LENGTH)", data = plaice)
pairs(em_plaice, simple = "Time_Period")

# EPU DF
plaice.epu.table <- matrix(nrow=3, ncol=3, byrow=TRUE)
rownames(plaice.epu.table) <- c('GB', 'GOM', 'SS')
colnames(plaice.epu.table) <- c('1992 - 2002', '2003 - 2013', '2014 - 2023')
# GB
plaice.epu.table[1, 1] <- plaice_model2$coefficients[2]
plaice.epu.table[1, 2] <- plaice_model2$coefficients[2] + plaice_model2$coefficients[7]
plaice.epu.table[1, 3] <- plaice_model2$coefficients[2] + plaice_model2$coefficients[8]
# GOM
plaice.epu.table[2, 1]<-plaice_model2$coefficients[2]+plaice_model2$coefficients[9]
plaice.epu.table[2, 2]<-plaice_model2$coefficients[2]+plaice_model2$coefficients[9]+plaice_model2$coefficients[7]+plaice_model2$coefficients[15]
plaice.epu.table[2, 3]<-plaice_model2$coefficients[2]+plaice_model2$coefficients[9]+plaice_model2$coefficients[8]+plaice_model2$coefficients[16]
# SS
plaice.epu.table[3, 1]<-plaice_model2$coefficients[2]+plaice_model2$coefficients[10]
plaice.epu.table[3, 2]<-plaice_model2$coefficients[2]+plaice_model2$coefficients[10]+plaice_model2$coefficients[7]+plaice_model2$coefficients[17]
plaice.epu.table[3, 3]<-plaice_model2$coefficients[2]+plaice_model2$coefficients[10]+plaice_model2$coefficients[8]+plaice_model2$coefficients[18]

# graphs by EPU
plaice.epu.df <- as.data.frame(plaice.epu.table, stringsAsFactors = FALSE)
plaice.epu.df$EPU <- rownames(plaice.epu.table)
plaice.epu.df.long <- pivot_longer(plaice.epu.df, 
                            cols = c("1992 - 2002", "2003 - 2013", "2014 - 2023"), 
                            names_to = "Time_Period", 
                            values_to = "Slope")
ggplot(plaice.epu.df.long, aes(x = Time_Period, y = Slope, group = EPU, color = EPU)) +
  geom_line() +
  geom_point() +
  labs(title = "Slope of Relationship Between log(Length) and log(Weight) in American Plaice", 
       x = "Time_Period", 
       y = "Slope (b)") +
  theme_minimal()+
  theme(legend.box.background = element_rect(color="black"),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 15)) +
  scale_color_manual(name = "EPU",  values = c("orangered2", "yellowgreen", "steelblue3"))

### Plaice Predictions Plot With Plotted Points
plaice_vec_size <- seq(min(plaice$LENGTH), max(plaice$LENGTH), length.out = 100)
plaice_predictions <- as.data.frame(ggeffects::ggpredict(plaice_model2, terms = c("LENGTH [plaice_vec_size]", "Time_Period",  "EPU")))
ggplot(plaice, aes(x = LENGTH, y = INDWT))+
  geom_point(aes(color = Time_Period), size = 0.5, alpha = 0.5)+
  geom_ribbon(data = plaice_predictions, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high, group = group), alpha = 0.1)+
  geom_line(data = plaice_predictions, aes(x = x, y = predicted, color = group), linewidth = 1)+
  facet_wrap(~facet)+
  labs(title = "American Plaice Length vs. Weight by EPU with Predicted Points Line", 
       x = "Length (cm)",
       y = "Individual Weight (kg)") +
  theme_bw()+
  theme(legend.box.background = element_rect(color="black"),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 15)) +
  scale_color_manual(name = "Time Period",  values = c("orangered2", "yellowgreen", "steelblue3"))

# -----------------------------------------------------------------------------
# Atlantic Herring - Opportunistic Fish 
# -----------------------------------------------------------------------------
herring <- lw_data %>%
  filter(COMNAME == 'ATLANTIC HERRING') %>%
  filter(INDWT < 0.6) %>%
  filter(INDWT != 0) %>%
  mutate(Time_Period = case_when(YEAR >= 2014 ~ "2014 - 2023",
                            YEAR >= 2003 & YEAR < 2014 ~ "2003 - 2013",
                            YEAR <= 2002 ~ "1992 - 2002")) %>%
  filter(!is.na(EPU))

### Atlantic Herring Data Plot
herring_plot <- herring %>%
  ggplot(aes(x = LENGTH, y = INDWT))+
  geom_point(aes(color=Time_Period)) +
  facet_wrap(~EPU)+
  labs(title = "Atlantic Herring: Length vs Weight",
       x = "Length",
       y = "Individual Weight")+
  theme_bw()+
  theme(legend.box.background = element_rect(color="black"),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 15, face="bold", hjust=0.5)) +
  scale_color_manual(name = "Time Period",  values = c("orangered2", "yellowgreen", "steelblue3"))
print(herring_plot)

### Atlantic Herring Data Plot, not faceted
herring_plot_all <- herring %>%
  ggplot(aes(x = LENGTH, y = INDWT))+
  geom_point(aes(color=Time_Period)) +
  labs(title = "Atlantic Herring: Length vs Weight",
       x = "Length",
       y = "Individual Weight")+
  theme_bw()+
  theme_bw()+
  theme(legend.box.background = element_rect(color="black"),
        legend.title = element_text(size = 15), 
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5)) +
  scale_color_manual(name = "Time Period",  values = c("orangered2", "yellowgreen", "steelblue2")) 
print(herring_plot_all)

herring_model <- lm(log(INDWT) ~ log(LENGTH)*Time_Period + log(LENGTH)*EPU, data=herring)
summary(herring_model)
a.by.10yrs["Atlantic Herring", "1992 - 2002"] <- herring_model$coefficients[1] + herring_model$coefficients[5]
b.by.10yrs["Atlantic Herring", "1992 - 2002"] <- herring_model$coefficients[2] + herring_model$coefficients[10]
a.by.10yrs["Atlantic Herring", "2003 - 2013"] <- herring_model$coefficients[1] + herring_model$coefficients[5] + herring_model$coefficients[3]
b.by.10yrs["Atlantic Herring", "2003 - 2013"] <- herring_model$coefficients[2] + herring_model$coefficients[10] + herring_model$coefficients[8]
a.by.10yrs["Atlantic Herring", "2014 - 2023"] <- herring_model$coefficients[1] + herring_model$coefficients[5] + herring_model$coefficients[4]
b.by.10yrs["Atlantic Herring", "2014 - 2023"] <- herring_model$coefficients[2] + herring_model$coefficients[10] + herring_model$coefficients[9]

# Herring Model 2 - Includes Triple Interaction Term
herring_model2 <- lm(log(INDWT) ~ log(LENGTH)*Time_Period*EPU, data=herring)
summary(herring_model2)
# Comparing slopes for each EPU by Time_Period
em_herring <- emtrends(herring_model2, ~EPU + Time_Period, "log(LENGTH)", data = herring)
pairs(em_herring, simple = "Time_Period")

# EPU DF
herring.epu.table <- matrix(nrow=4, ncol=3, byrow=TRUE)
rownames(herring.epu.table) <- c('GB', 'GOM', 'MAB', 'SS')
colnames(herring.epu.table) <- c('1992 - 2002', '2003 - 2013', '2014 - 2023')
# GB
herring.epu.table[1, 1] <- herring_model2$coefficients[2]
herring.epu.table[1, 2] <- herring_model2$coefficients[2] + herring_model2$coefficients[8]
herring.epu.table[1, 3] <- herring_model2$coefficients[2] + herring_model2$coefficients[9]
# GOM
herring.epu.table[2, 1]<-herring_model2$coefficients[2]+herring_model2$coefficients[10]
herring.epu.table[2, 2]<-herring_model2$coefficients[2]+herring_model2$coefficients[10]+herring_model2$coefficients[8]+herring_model2$coefficients[19]
herring.epu.table[2, 3]<-herring_model2$coefficients[2]+herring_model2$coefficients[10]+herring_model2$coefficients[9]+herring_model2$coefficients[20]
# MAB
herring.epu.table[3, 1]<-herring_model2$coefficients[2]+herring_model2$coefficients[11]
herring.epu.table[3, 2]<-herring_model2$coefficients[2]+herring_model2$coefficients[11]+herring_model2$coefficients[8]+herring_model2$coefficients[21]
herring.epu.table[3, 3]<-herring_model2$coefficients[2]+herring_model2$coefficients[11]+herring_model2$coefficients[9]+herring_model2$coefficients[22]
# SS
herring.epu.table[4, 1]<-herring_model2$coefficients[2]+herring_model2$coefficients[12]
herring.epu.table[4, 2]<-herring_model2$coefficients[2]+herring_model2$coefficients[12]+herring_model2$coefficients[8]+herring_model2$coefficients[23]
herring.epu.table[4, 3]<-herring_model2$coefficients[2]+herring_model2$coefficients[12]+herring_model2$coefficients[9]+herring_model2$coefficients[24]

# graphs by EPU
herring.epu.df <- as.data.frame(herring.epu.table, stringsAsFactors = FALSE)
herring.epu.df$EPU <- rownames(herring.epu.table)
herring.epu.df.long <- pivot_longer(herring.epu.df, 
                            cols = c("1992 - 2002", "2003 - 2013", "2014 - 2023"), 
                            names_to = "Time_Period", 
                            values_to = "Slope")
ggplot(herring.epu.df.long, aes(x = Time_Period, y = Slope, group = EPU, color = EPU)) +
  geom_line() +
  geom_point() +
  labs(title = "Slope of Relationship Between log(Length) and log(Weight) in Atlantic Herring", 
       x = "Time_Period", 
       y = "Slope (b)") +
  theme_minimal()+
  theme_bw()+
  theme(legend.box.background = element_rect(color="black"),
        legend.title = element_text(size = 15), 
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 15)) +
  scale_color_manual(name = "EPU",  values = c("orangered2", "yellowgreen", "steelblue2","purple")) 

# Herring Predictions Plot With Plotted Points
herring_vec_size <- seq(min(herring$LENGTH), max(herring$LENGTH), length.out = 100)
herring_predictions <- as.data.frame(ggeffects::ggpredict(herring_model2, terms = c("LENGTH [herring_vec_size]", "Time_Period",  "EPU")))
ggplot(herring, aes(x = LENGTH, y = INDWT))+
  geom_point(aes(color = Time_Period), size = 0.25, alpha = 0.5)+
  geom_ribbon(data = herring_predictions, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high, group = group), alpha = 0.1)+
  geom_line(data = herring_predictions, aes(x = x, y = predicted, color = group), linewidth = 1)+
  facet_wrap(~facet)+
  labs(title = "Atlantic Herring Length vs. Weight by EPU with Predicted Points Line", 
       x = "Length (cm)",
       y = "Individual Weight (kg)") +
  theme_bw()+
  theme_bw()+
  theme(legend.box.background = element_rect(color="black"),
        legend.title = element_text(size = 15), 
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 15)) +
  scale_color_manual(name = "Time Period",  values = c("orangered2", "yellowgreen", "steelblue2")) 

# -----------------------------------------------------------------------------
# Spiny Dogfish - Equilibrium
# -----------------------------------------------------------------------------
dogfish <- lw_data %>%
  filter(COMNAME == 'SPINY DOGFISH') %>%
  filter(INDWT != 0) %>%
  mutate(Time_Period = case_when(YEAR >= 2014 ~ "2014 - 2023",
                            YEAR >= 2003 & YEAR < 2014 ~ "2003 - 2013",
                            YEAR <= 2002 ~ "1992 - 2002")) %>%
  filter(!is.na(EPU))

### Spiny dogfish Data Plot
dogfish_plot <- dogfish %>%
  ggplot(aes(x = LENGTH, y = INDWT))+
  geom_point(aes(color=Time_Period)) +
  facet_wrap(~EPU)+
  labs(title = "Spiny Dogfish: Length vs Weight",
       x = "Length",
       y = "Individual Weight")+
  theme_bw()+
  theme(legend.box.background = element_rect(color="black"),
        legend.title = element_text(size = 15), 
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5)) +
  scale_color_manual(name = "Time Period",  values = c("orangered2", "yellowgreen", "steelblue2")) 
print(dogfish_plot)

### Spiny dogfish Data Plot, not faceted
dogfish_plot_all <- dogfish %>%
  ggplot(aes(x = LENGTH, y = INDWT))+
  geom_point(aes(color=Time_Period)) +
  labs(title = "Spiny Dogfish: Length vs Weight",
       x = "Length",
       y = "Individual Weight")+
  theme_bw()+
  theme(legend.box.background = element_rect(color="black"),
        legend.title = element_text(size = 15), 
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5)) +
  scale_color_manual(name = "Time Period",  values = c("orangered2", "yellowgreen", "steelblue2")) 
print(dogfish_plot_all)

dogfish_model <- lm(log(INDWT) ~ log(LENGTH)*Time_Period + log(LENGTH)*EPU, data=dogfish)
summary(dogfish_model)
a.by.10yrs["Spiny Dogfish", "1992 - 2002"] <- dogfish_model$coefficients[1] + dogfish_model$coefficients[5]
b.by.10yrs["Spiny Dogfish", "1992 - 2002"] <- dogfish_model$coefficients[2] + dogfish_model$coefficients[10]
a.by.10yrs["Spiny Dogfish", "2003 - 2013"] <- dogfish_model$coefficients[1] + dogfish_model$coefficients[5] + dogfish_model$coefficients[3]
b.by.10yrs["Spiny Dogfish", "2003 - 2013"] <- dogfish_model$coefficients[2] + dogfish_model$coefficients[10] + dogfish_model$coefficients[8]
a.by.10yrs["Spiny Dogfish", "2014 - 2023"] <- dogfish_model$coefficients[1] + dogfish_model$coefficients[5] + dogfish_model$coefficients[4]
b.by.10yrs["Spiny Dogfish", "2014 - 2023"] <- dogfish_model$coefficients[2] + dogfish_model$coefficients[10] + dogfish_model$coefficients[9]

# Dogfish Model 2 - Includes Triple Interaction Term
dogfish_model2 <- lm(log(INDWT) ~ log(LENGTH)*Time_Period*EPU, data=dogfish)
summary(dogfish_model2)
# Comparing slopes for each EPU by Time_Period
em_dog <- emtrends(dogfish_model2, ~EPU*Time_Period, "log(LENGTH)", data = dogfish)
pairs(em_dog, simple = "Time_Period")

# EPU DF
dogfish.epu.table <- matrix(nrow=4, ncol=3, byrow=TRUE)
rownames(dogfish.epu.table) <- c('GB', 'GOM', 'MAB', 'SS')
colnames(dogfish.epu.table) <- c('1992 - 2002', '2003 - 2013', '2014 - 2023')
# GB
dogfish.epu.table[1, 1] <- dogfish_model2$coefficients[2]
dogfish.epu.table[1, 2] <- dogfish_model2$coefficients[2] + dogfish_model2$coefficients[8]
dogfish.epu.table[1, 3] <- dogfish_model2$coefficients[2] + dogfish_model2$coefficients[9]
# GOM
dogfish.epu.table[2, 1]<-dogfish_model2$coefficients[2]+dogfish_model2$coefficients[10]
dogfish.epu.table[2, 2]<-dogfish_model2$coefficients[2]+dogfish_model2$coefficients[10]+dogfish_model2$coefficients[8]+dogfish_model2$coefficients[19]
dogfish.epu.table[2, 3]<-dogfish_model2$coefficients[2]+dogfish_model2$coefficients[10]+dogfish_model2$coefficients[9]+dogfish_model2$coefficients[20]
# MAB
dogfish.epu.table[3, 1]<-dogfish_model2$coefficients[2]+dogfish_model2$coefficients[11]
dogfish.epu.table[3, 2]<-dogfish_model2$coefficients[2]+dogfish_model2$coefficients[11]+dogfish_model2$coefficients[8]+dogfish_model2$coefficients[21]
dogfish.epu.table[3, 3]<-dogfish_model2$coefficients[2]+dogfish_model2$coefficients[11]+dogfish_model2$coefficients[9]+dogfish_model2$coefficients[22]
# SS
dogfish.epu.table[4, 1]<-dogfish_model2$coefficients[2]+dogfish_model2$coefficients[12]
dogfish.epu.table[4, 2]<-dogfish_model2$coefficients[2]+dogfish_model2$coefficients[12]+dogfish_model2$coefficients[8]+dogfish_model2$coefficients[23]
dogfish.epu.table[4, 3]<-dogfish_model2$coefficients[2]+dogfish_model2$coefficients[12]+dogfish_model2$coefficients[9]+dogfish_model2$coefficients[24]

dogfish.epu.table
# graphs by EPU
dogfish.epu.df <- as.data.frame(dogfish.epu.table, stringsAsFactors = FALSE)
dogfish.epu.df$EPU <- rownames(dogfish.epu.table)
dogfish.epu.df.long <- pivot_longer(dogfish.epu.df, 
                            cols = c("1992 - 2002", "2003 - 2013", "2014 - 2023"), 
                            names_to = "Time_Period", 
                            values_to = "Slope")
ggplot(dogfish.epu.df.long, aes(x = Time_Period, y = Slope, group = EPU, color = EPU)) +
  geom_line() +
  geom_point() +
  labs(title = "Slope of Relationship Between log(Length) and log(Weight) in Spiny Dogfish", 
       x = "Time_Period", 
       y = "Slope (b)") +
  theme_bw()+
  theme(legend.box.background = element_rect(color="black"),
        legend.title = element_text(size = 15), 
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 15)) +
  scale_color_manual(name = "EPU",  values = c("orangered2", "yellowgreen", "steelblue2", "purple")) 

# Spiny Dogfish Predictions Plot With Plotted Points
dogfish_vec_size <- seq(min(dogfish$LENGTH), max(dogfish$LENGTH), length.out = 100)
dogfish_predictions <- as.data.frame(ggeffects::ggpredict(dogfish_model2, terms = c("LENGTH [dogfish_vec_size]", "Time_Period",  "EPU")))
ggplot(dogfish, aes(x = LENGTH, y = INDWT))+
  geom_point(aes(color = Time_Period), size = 0.1, alpha = 0.1)+
  geom_ribbon(data = dogfish_predictions, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high, group = group), alpha = 0.1)+
  geom_line(data = dogfish_predictions, aes(x = x, y = predicted, color = group), linewidth = 1)+
  facet_wrap(~facet)+
  labs(title = "Spiny Dogfish Length vs. Weight by EPU with Predicted Points Line", 
       x = "Length (cm)",
       y = "Individual Weight (kg)") +
  theme_bw()+
  theme(legend.box.background = element_rect(color="black"),
        legend.title = element_text(size = 15), 
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 15)) +
  scale_color_manual(name = "Time Period",  values = c("orangered2", "yellowgreen", "steelblue2")) 
### ---------------------------------------------------------------------------
### Plot of Slope Changes
### ---------------------------------------------------------------------------
b.by.10yrs.df <- as.data.frame(b.by.10yrs, stringsAsFactors = FALSE)
b.by.10yrs.df$Species <- rownames(b.by.10yrs)

b.by.10yrs.df.long <- pivot_longer(b.by.10yrs.df, 
                            cols = c("1992 - 2002", "2003 - 2013", "2014 - 2023"), 
                            names_to = "Time_Period", 
                            values_to = "Slope")

ggplot(b.by.10yrs.df.long, aes(x = Time_Period, y = Slope, group = Species, color = Species)) +
  geom_line() +
  geom_point() +
  labs(title = "Slope of Relationship Between log(Length) and log(Weight) in the Gulf of Maine", 
       x = "Time_Period", 
       y = "Slope (b)") +
  theme_bw()+
  theme(legend.box.background = element_rect(color="black"),
        legend.title = element_text(size = 15), 
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 15)) +
  scale_color_manual(name = "Species",  values = c("orangered2", "yellowgreen", "steelblue2", "purple")) 

### ---------------------------------------------------------------------------
### Coefficients 
### ---------------------------------------------------------------------------
unique_comname <- unique(lw_data$COMNAME)
unique_comname <- na.omit(unique_comname)

data <- lw_data %>%
  filter(!is.na(LENGTH) & !is.na(INDWT) & !is.na(COMNAME)) %>%
  mutate(Time_Period = case_when(YEAR >= 2014 ~ "2014 - 2023",
                            YEAR >= 2003 & YEAR < 2014 ~ "2003 - 2013",
                            YEAR <= 2002 ~ "1992 - 2002"))

grouped_data <- data %>%
  group_by(COMNAME, Time_Period) %>%
  summarise(count = n())

filtered_groups <- grouped_data %>%
  filter(count >= 5) %>%
  ungroup()

unique_species <- unique(filtered_groups$COMNAME)
species <- c()

for (spec in unique_species) {
  spec_decs <- filtered_groups %>%
    filter(COMNAME == spec)
  if (nrow(spec_decs) == 3) {
    species <- c(species, spec)
  }
}

season_counts <- data %>%
  filter(COMNAME %in% species) %>% 
  filter(!is.na(SEASON)) %>%             
  group_by(COMNAME, Time_Period, SEASON) %>%
  summarise(count = n()) %>%
  filter(count >= 5) %>%
  ungroup()

coeffs.by.Time_Period.df <- data.frame(row.names = species)
Time_Periods = c('1992 - 2002', '2003 - 2013', '2014 - 2023')
seasons = c('SPRING', 'FALL')

# filtering out large outliers
data <- data %>%
  filter(! (COMNAME == "AMERICAN PLAICE" & INDWT > 4) ) %>%
  filter(! (COMNAME == "ATLANTIC HERRING" & INDWT > 0.5)) %>%
  filter(! (COMNAME == "ATLANTIC SHARPNOSE SHARK" & LENGTH > 106)) %>%
  ## blunt nose stingray - says they can be up to 100 cm. a few observations are close/over
  filter(! (COMNAME == "BLUNTNOSE STINGRAY" & INDWT > 50)) %>%
  filter(! (COMNAME == "BLUNTNOSE STINGRAY" & INDWT > 10 & LENGTH < 10)) %>%
  # just some low observations... not sure if these are errors
  filter(! (COMNAME == "BUTTERFISH" & INDWT <= 0.01 & LENGTH > 12.5)) %>%
  filter(! (COMNAME == "LITTLE SKATE" & INDWT > 4)) %>%
  filter(! (COMNAME == "LONGFIN SQUID" & INDWT > 1)) %>%
  filter(! (COMNAME == "LONGHORN SCULPIN" & INDWT > 750)) %>%
  filter(! (COMNAME == "NORTHERN KINGFISH" & INDWT > 1)) %>%
  filter(! (COMNAME == "NORTHERN SHORTFIN SQUID" & INDWT > 1)) %>%
  filter(! (COMNAME == "WINTER FLOUNDER" & LENGTH < 20 & INDWT > 3))

for (d in Time_Periods) {
  for (s in seasons) {
    coeffs.by.Time_Period.df[[paste0("B.", s, ".", d)]] <- NA
    coeffs.by.Time_Period.df[[paste0("A.", s, ".", d)]] <- NA
  }
}

for (spec in species) {
  spec_data <- lw_data %>%
    filter(COMNAME == spec) %>%
    filter(INDWT != 0) %>%
    mutate(Time_Period = case_when(YEAR >= 2014 ~ "2014 - 2023",
                              YEAR >= 2003 & YEAR < 2014 ~ "2003 - 2013",
                              YEAR <= 2002 ~ "1992 - 2002"))
  
  for (dec in Time_Periods) {
    for (szn in seasons) {
      season_data <- spec_data %>%
        filter(SEASON == szn & Time_Period == dec)
      if (nrow(season_data) >= 5) {
        spec.szn.lm <- lm(log(INDWT) ~ log(LENGTH), data = season_data)
        coeffs.by.Time_Period.df[spec, paste0("B.", szn, ".", dec)] <- coef(spec.szn.lm)[2]
        coeffs.by.Time_Period.df[spec, paste0("A.", szn, ".", dec)] <- coef(spec.szn.lm)[1]
      }
    }
  }
}
print(coeffs.by.Time_Period.df)

