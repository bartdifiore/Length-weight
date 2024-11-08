### ---------------------------------------------------------------------------
### Time_Periods broken up in 10 year increments, EPU prediction plots
### ---------------------------------------------------------------------------
library(tidyverse) # loading in packages & data
library(emmeans)

lw_data <- readRDS('Data/length_weight_epu.rds')
lw_coeffs <- readRDS('Data/lw_coeffs.rds')

# Tables to hold b parameter
gom.b <- matrix(nrow=4, ncol=3, byrow=TRUE)
rownames(gom.b) <- c('Atlantic Cod', 'American Plaice', 'Atlantic Herring', 'Spiny Dogfish')
colnames(gom.b) <- c('1992 - 2002', '2003 - 2013', '2014 - 2023')

gb.b <- matrix(nrow=4, ncol=3, byrow=TRUE)
rownames(gb.b) <- c('Atlantic Cod', 'American Plaice', 'Atlantic Herring', 'Spiny Dogfish')
colnames(gb.b) <- c('1992 - 2002', '2003 - 2013', '2014 - 2023')

mab.b <- matrix(nrow=3, ncol=3, byrow=TRUE)
rownames(mab.b) <- c('Atlantic Cod', 'Atlantic Herring', 'Spiny Dogfish')
colnames(mab.b) <- c('1992 - 2002', '2003 - 2013', '2014 - 2023')

ss.b <- matrix(nrow=4, ncol=3, byrow=TRUE)
rownames(ss.b) <- c('Atlantic Cod', 'American Plaice', 'Atlantic Herring', 'Spiny Dogfish')
colnames(ss.b) <- c('1992 - 2002', '2003 - 2013', '2014 - 2023')

# Table to hold log(a) parameter
a.by.10yrs <- matrix(nrow=4, ncol=3, byrow=TRUE)
rownames(a.by.10yrs) <- c('Atlantic Cod', 'American Plaice', 'Atlantic Herring', 'Spiny Dogfish')
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

### Cod - Ignoring Interaction
codlm <- lm(log(INDWT) ~ log(LENGTH)*Time_Period, cod)
summary(codlm)
b.tibble <- matrix(nrow=4, ncol=3, byrow=TRUE)
rownames(b.tibble) <- c('Atlantic Cod', 'American Plaice', 'Atlantic Herring', 'Spiny Dogfish')
colnames(b.tibble) <- c('1992 - 2002', '2003 - 2013', '2014 - 2023')
b.tibble["Atlantic Cod", "1992 - 2002"] <- codlm$coefficients[2]
b.tibble["Atlantic Cod", "2003 - 2013"] <- codlm$coefficients[2]+codlm$coefficients[5]
b.tibble["Atlantic Cod", "2014 - 2023"] <- codlm$coefficients[2]+codlm$coefficients[6]

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
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        text=element_text(size=15)) +
  scale_color_manual(name = "Time Period",  values = c("orangered2", "yellowgreen", "steelblue2")) 
print(cod_plot_all)
ggsave(filename="CodPlotAll.png", plot=cod_plot_all, width=8, height=6)

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
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        text=element_text(size=15)) +
  scale_color_manual(name = "Time Period",  values = c("orangered2", "yellowgreen", "steelblue2"))
print(cod_gom)
ggsave(filename="CodGOM.png",plot=cod_gom,width=8,height=6)

### Cod GOM Log Plot
cod_gom_log <- cod %>%
  filter(EPU == "GOM") %>%
  ggplot(aes(x = LENGTH, y = INDWT))+
  geom_point(aes(color=Time_Period)) +
  labs(title = "Atlantic Cod in the Gulf of Maine: log(Length) vs log(Weight)",
       x = "log(Length)",
       y = "log(Individual Weight)")+
  theme_bw()+
  scale_x_log10()+
  scale_y_log10()+
  theme(legend.box.background = element_rect(color="black"),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        text=element_text(size=15))+
  scale_color_manual(name = "Time Period",  values = c("orangered2", "yellowgreen", "steelblue2"))
print(cod_gom_log)
ggsave(filename="CodGOMLog.png",plot=cod_gom_log,width=8,height=6)

### Cod Model
cod_model <- lm(log(INDWT) ~ log(LENGTH)*Time_Period + log(LENGTH)*EPU, data=cod)
summary(cod_model)
a.by.10yrs["Atlantic Cod", "1992 - 2002"] <- cod_model$coefficients[1] + cod_model$coefficients[5]
gom.b["Atlantic Cod", "1992 - 2002"] <- cod_model$coefficients[2] + cod_model$coefficients[10]
a.by.10yrs["Atlantic Cod", "2003 - 2013"] <- cod_model$coefficients[1] + cod_model$coefficients[5] + cod_model$coefficients[3]
gom.b["Atlantic Cod", "2003 - 2013"] <- cod_model$coefficients[2] + cod_model$coefficients[10] + cod_model$coefficients[8]
a.by.10yrs["Atlantic Cod", "2014 - 2023"] <- cod_model$coefficients[1] + cod_model$coefficients[5] + cod_model$coefficients[4]
gom.b["Atlantic Cod", "2014 - 2023"] <- cod_model$coefficients[2] + cod_model$coefficients[10] + cod_model$coefficients[9]

### Cod Model 2 - Includes Triple Interaction Term
cod_model2 <- lm(log(INDWT) ~ log(LENGTH)*Time_Period*EPU, data=cod)
summary(cod_model2)
# comparing slopes for each EPU by Time_Period
em_cod <- emtrends(cod_model2, ~EPU + Time_Period, "log(LENGTH)", data = cod)
pairs(em_cod, simple = "Time_Period")
pairs(em_cod, simple = "EPU")
cod

# EPU DF
cod.epu.table <- matrix(nrow=4, ncol=3, byrow=TRUE)
rownames(cod.epu.table) <- c('GB', 'GOM', 'MAB', 'SS')
colnames(cod.epu.table) <- c('1992 - 2002', '2003 - 2013', '2014 - 2023')
# GB
cod.epu.table[1, 1] <- cod_model2$coefficients[2]
gb.b["Atlantic Cod", "1992 - 2002"] <- cod.epu.table[1, 1]
cod.epu.table[1, 2] <- cod_model2$coefficients[2] + cod_model2$coefficients[8]
gb.b["Atlantic Cod", "2003 - 2013"] <- cod.epu.table[1, 2]
cod.epu.table[1, 3] <- cod_model2$coefficients[2] + cod_model2$coefficients[9]
gb.b["Atlantic Cod", "2014 - 2023"] <- cod.epu.table[1, 3]
# GOM
cod.epu.table[2, 1]<-cod_model2$coefficients[2]+cod_model2$coefficients[10]
gom.b["Atlantic Cod", "1992 - 2002"] <- cod.epu.table[2, 1]
cod.epu.table[2, 2]<-cod_model2$coefficients[2]+cod_model2$coefficients[10]+cod_model2$coefficients[8]+cod_model2$coefficients[19]
gom.b["Atlantic Cod", "2003 - 2013"] <- cod.epu.table[2, 2]
cod.epu.table[2, 3]<-cod_model2$coefficients[2]+cod_model2$coefficients[10]+cod_model2$coefficients[9]+cod_model2$coefficients[20]
gom.b["Atlantic Cod", "2014 - 2023"] <- cod.epu.table[2, 3]
# MAB
cod.epu.table[3, 1]<-cod_model2$coefficients[2]+cod_model2$coefficients[11]
mab.b["Atlantic Cod", "1992 - 2002"] <- cod.epu.table[3, 1]
cod.epu.table[3, 2]<-cod_model2$coefficients[2]+cod_model2$coefficients[11]+cod_model2$coefficients[8]+cod_model2$coefficients[21]
mab.b["Atlantic Cod", "2003 - 2013"] <- cod.epu.table[3, 2]
cod.epu.table[3, 3]<-cod_model2$coefficients[2]+cod_model2$coefficients[11]+cod_model2$coefficients[9]+cod_model2$coefficients[22]
mab.b["Atlantic Cod", "2014 - 2023"] <- cod.epu.table[3, 3]
# SS
cod.epu.table[4, 1]<-cod_model2$coefficients[2]+cod_model2$coefficients[12]
ss.b["Atlantic Cod", "1992 - 2002"] <- cod.epu.table[4, 1]
cod.epu.table[4, 2]<-cod_model2$coefficients[2]+cod_model2$coefficients[12]+cod_model2$coefficients[8]+cod_model2$coefficients[23]
ss.b["Atlantic Cod", "2003 - 2013"] <- cod.epu.table[4, 2]
cod.epu.table[4, 3]<-cod_model2$coefficients[2]+cod_model2$coefficients[12]+cod_model2$coefficients[9]+cod_model2$coefficients[24]
ss.b["Atlantic Cod", "2014 - 2023"] <- cod.epu.table[4, 3]

# graphs by EPU
cod.epu.df <- as.data.frame(cod.epu.table, stringsAsFactors = FALSE)
cod.epu.df$EPU <- rownames(cod.epu.table)
cod.epu.df.long <- pivot_longer(cod.epu.df, 
                            cols = c("1992 - 2002", "2003 - 2013", "2014 - 2023"), 
                            names_to = "Time_Period", 
                            values_to = "Slope")
ggplot(cod.epu.df.long, aes(x = Time_Period, y = Slope, group = EPU, color = EPU)) +
  geom_line(size = 1.5, show.legend = FALSE) +
  geom_point(size = 3, show.legend = FALSE) +
  labs(title = "Atlantic Cod: β Coefficient by EPU", 
       x = "Time Period", 
       y = "β Coefficient") +
  theme_bw() +
  theme(legend.position = "bottom",            
        legend.box.background = element_rect(color = "black"),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 16)) +
  scale_color_manual(name = "EPU", values = c("orangered2", "yellowgreen", "steelblue2", "purple"))->cod_epu_plot
ggsave(filename="CodEPUPlot.png", plot=cod_epu_plot, width=7, height=6)

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

### Plaice - Ignoring Interaction
plaicelm <- lm(log(INDWT) ~ log(LENGTH)*Time_Period, plaice)
summary(plaicelm)
b.tibble["American Plaice", "1992 - 2002"] <- plaicelm$coefficients[2]
b.tibble["American Plaice", "2003 - 2013"] <- plaicelm$coefficients[2]+plaicelm$coefficients[5]
b.tibble["American Plaice", "2014 - 2023"] <- plaicelm$coefficients[2]+plaicelm$coefficients[6]


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
# ggsave(filename="PlaicePlotUnfiltered.png", plot=plaice_plot_all, width=8, height=6)

plaice_model <- lm(log(INDWT) ~ log(LENGTH)*Time_Period + log(LENGTH)*EPU, data=plaice)
summary(plaice_model)
a.by.10yrs["American Plaice", "1992 - 2002"] <- plaice_model$coefficients[1] + plaice_model$coefficients[5]
gom.b["American Plaice", "1992 - 2002"] <- plaice_model$coefficients[2] + plaice_model$coefficients[9]
a.by.10yrs["American Plaice", "2003 - 2013"] <- plaice_model$coefficients[1] + plaice_model$coefficients[5] + plaice_model$coefficients[3]
gom.b["American Plaice", "2003 - 2013"] <- plaice_model$coefficients[2] + plaice_model$coefficients[9] + plaice_model$coefficients[7]
a.by.10yrs["American Plaice", "2014 - 2023"] <- plaice_model$coefficients[1] + plaice_model$coefficients[5] + plaice_model$coefficients[4]
gom.b["American Plaice", "2014 - 2023"] <- plaice_model$coefficients[2] + plaice_model$coefficients[9] + plaice_model$coefficients[8]

# Plaice Model 2 - Includes Triple Interaction Term
plaice_model2 <- lm(log(INDWT) ~ log(LENGTH)*Time_Period*EPU, data=plaice)
summary(plaice_model2)
# comparing slopes for each EPU by Time_Period
em_plaice <- emtrends(plaice_model2, ~EPU + Time_Period, "log(LENGTH)", data = plaice)
pairs(em_plaice, simple = "Time_Period")
plaice
# EPU DF
plaice.epu.table <- matrix(nrow=3, ncol=3, byrow=TRUE)
rownames(plaice.epu.table) <- c('GB', 'GOM', 'SS')
colnames(plaice.epu.table) <- c('1992 - 2002', '2003 - 2013', '2014 - 2023')
# GB
plaice.epu.table[1, 1] <- plaice_model2$coefficients[2]
gb.b["American Plaice", "1992 - 2002"] <- plaice.epu.table[1, 1]
plaice.epu.table[1, 2] <- plaice_model2$coefficients[2] + plaice_model2$coefficients[7]
gb.b["American Plaice", "2003 - 2013"] <- plaice.epu.table[1, 2]
plaice.epu.table[1, 3] <- plaice_model2$coefficients[2] + plaice_model2$coefficients[8]
gb.b["American Plaice", "2014 - 2023"] <- plaice.epu.table[1, 3]
# GOM
plaice.epu.table[2, 1]<-plaice_model2$coefficients[2]+plaice_model2$coefficients[9]
gom.b["American Plaice", "1992 - 2002"] <- plaice.epu.table[2, 1]
plaice.epu.table[2, 2]<-plaice_model2$coefficients[2]+plaice_model2$coefficients[9]+plaice_model2$coefficients[7]+plaice_model2$coefficients[15]
gom.b["American Plaice", "2003 - 2013"] <- plaice.epu.table[2, 2]
plaice.epu.table[2, 3]<-plaice_model2$coefficients[2]+plaice_model2$coefficients[9]+plaice_model2$coefficients[8]+plaice_model2$coefficients[16]
gom.b["American Plaice", "2014 - 2023"] <- plaice.epu.table[2, 3]
# SS
plaice.epu.table[3, 1]<-plaice_model2$coefficients[2]+plaice_model2$coefficients[10]
ss.b["American Plaice", "1992 - 2002"] <- plaice.epu.table[3, 1]
plaice.epu.table[3, 2]<-plaice_model2$coefficients[2]+plaice_model2$coefficients[10]+plaice_model2$coefficients[7]+plaice_model2$coefficients[17]
ss.b["American Plaice", "2003 - 2013"] <- plaice.epu.table[3, 2]
plaice.epu.table[3, 3]<-plaice_model2$coefficients[2]+plaice_model2$coefficients[10]+plaice_model2$coefficients[8]+plaice_model2$coefficients[18]
ss.b["American Plaice", "2014 - 2023"] <- plaice.epu.table[3, 3]

# graphs by EPU
plaice.epu.df <- as.data.frame(plaice.epu.table, stringsAsFactors = FALSE)
plaice.epu.df$EPU <- rownames(plaice.epu.table)
plaice.epu.df.long <- pivot_longer(plaice.epu.df, 
                            cols = c("1992 - 2002", "2003 - 2013", "2014 - 2023"), 
                            names_to = "Time_Period", 
                            values_to = "Slope")
ggplot(plaice.epu.df.long, aes(x = Time_Period, y = Slope, group = EPU, color = EPU)) +
  geom_line(size = 1.5, show.legend = FALSE) +
  geom_point(size = 3, show.legend = FALSE) +
  labs(title = "American Plaice: β Coefficient by EPU", 
       x = "Time Period", 
       y = "β Coefficient") +
  theme_bw() +
  theme(legend.position = "bottom",            
        legend.box.background = element_rect(color = "black"),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 16)) +
  scale_color_manual(name = "EPU", values = c("orangered2", "yellowgreen", "purple"))->plaice_epu_plot
ggsave(filename="PlaiceEPUPlot.png", plot=plaice_epu_plot, width=7, height=6)

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
  filter(INDWT < 2) %>%
  filter(INDWT != 0) %>%
  mutate(Time_Period = case_when(YEAR >= 2014 ~ "2014 - 2023",
                            YEAR >= 2003 & YEAR < 2014 ~ "2003 - 2013",
                            YEAR <= 2002 ~ "1992 - 2002")) %>%
  filter(!is.na(EPU))

### Herring - Ignoring Interaction
herringlm <- lm(log(INDWT) ~ log(LENGTH)*Time_Period, herring)
summary(herringlm)
b.tibble["Atlantic Herring", "1992 - 2002"] <- herringlm$coefficients[2]
b.tibble["Atlantic Herring", "2003 - 2013"] <- herringlm$coefficients[2]+herringlm$coefficients[5]
b.tibble["Atlantic Herring", "2014 - 2023"] <- herringlm$coefficients[2]+herringlm$coefficients[6]


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
gom.b["Atlantic Herring", "1992 - 2002"] <- herring_model$coefficients[2] + herring_model$coefficients[10]
a.by.10yrs["Atlantic Herring", "2003 - 2013"] <- herring_model$coefficients[1] + herring_model$coefficients[5] + herring_model$coefficients[3]
gom.b["Atlantic Herring", "2003 - 2013"] <- herring_model$coefficients[2] + herring_model$coefficients[10] + herring_model$coefficients[8]
a.by.10yrs["Atlantic Herring", "2014 - 2023"] <- herring_model$coefficients[1] + herring_model$coefficients[5] + herring_model$coefficients[4]
gom.b["Atlantic Herring", "2014 - 2023"] <- herring_model$coefficients[2] + herring_model$coefficients[10] + herring_model$coefficients[9]

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
gb.b["Atlantic Herring", "1992 - 2002"] <- herring.epu.table[1, 1]
herring.epu.table[1, 2] <- herring_model2$coefficients[2] + herring_model2$coefficients[8]
gb.b["Atlantic Herring", "2003 - 2013"] <- herring.epu.table[1, 2]
herring.epu.table[1, 3] <- herring_model2$coefficients[2] + herring_model2$coefficients[9]
gb.b["Atlantic Herring", "2014 - 2023"] <- herring.epu.table[1, 3]
# GOM
herring.epu.table[2, 1]<-herring_model2$coefficients[2]+herring_model2$coefficients[10]
gom.b["Atlantic Herring", "1992 - 2002"] <- herring.epu.table[2, 1]
herring.epu.table[2, 2]<-herring_model2$coefficients[2]+herring_model2$coefficients[10]+herring_model2$coefficients[8]+herring_model2$coefficients[19]
gom.b["Atlantic Herring", "2003 - 2013"] <- herring.epu.table[2, 2]
herring.epu.table[2, 3]<-herring_model2$coefficients[2]+herring_model2$coefficients[10]+herring_model2$coefficients[9]+herring_model2$coefficients[20]
gom.b["Atlantic Herring", "2014 - 2023"] <- herring.epu.table[2, 3]
# MAB
herring.epu.table[3, 1]<-herring_model2$coefficients[2]+herring_model2$coefficients[11]
mab.b["Atlantic Herring", "1992 - 2002"] <- herring.epu.table[3, 1]
herring.epu.table[3, 2]<-herring_model2$coefficients[2]+herring_model2$coefficients[11]+herring_model2$coefficients[8]+herring_model2$coefficients[21]
mab.b["Atlantic Herring", "2003 - 2013"] <- herring.epu.table[3, 2]
herring.epu.table[3, 3]<-herring_model2$coefficients[2]+herring_model2$coefficients[11]+herring_model2$coefficients[9]+herring_model2$coefficients[22]
mab.b["Atlantic Herring", "2014 - 2023"] <- herring.epu.table[3, 3]
# SS
herring.epu.table[4, 1]<-herring_model2$coefficients[2]+herring_model2$coefficients[12]
ss.b["Atlantic Herring", "1992 - 2002"] <- herring.epu.table[4, 1]
herring.epu.table[4, 2]<-herring_model2$coefficients[2]+herring_model2$coefficients[12]+herring_model2$coefficients[8]+herring_model2$coefficients[23]
ss.b["Atlantic Herring", "2003 - 2013"] <- herring.epu.table[4, 2]
herring.epu.table[4, 3]<-herring_model2$coefficients[2]+herring_model2$coefficients[12]+herring_model2$coefficients[9]+herring_model2$coefficients[24]
ss.b["Atlantic Herring", "2014 - 2023"] <- herring.epu.table[4, 3]

# graphs by EPU
herring.epu.df <- as.data.frame(herring.epu.table, stringsAsFactors = FALSE)
herring.epu.df$EPU <- rownames(herring.epu.table)
herring.epu.df.long <- pivot_longer(herring.epu.df, 
                            cols = c("1992 - 2002", "2003 - 2013", "2014 - 2023"), 
                            names_to = "Time_Period", 
                            values_to = "Slope")
ggplot(herring.epu.df.long, aes(x = Time_Period, y = Slope, group = EPU, color = EPU)) +
  geom_line(size = 1.5, show.legend = FALSE) +
  geom_point(size = 3, show.legend = FALSE) +
  labs(title = "Atlantic Herring: β Coefficient by EPU", 
       x = "Time Period", 
       y = "β Coefficient") +
  theme_bw() +
  theme(legend.position = "bottom",            
        legend.box.background = element_rect(color = "black"),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 16)) +
  scale_color_manual(name = "EPU", values = c("orangered2", "yellowgreen", "steelblue2", "purple"))->herring_epu_plot
ggsave(filename="HerringEPUPlot.png", plot=herring_epu_plot, width=7, height=6)

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

### Dogfish - Ignoring Interaction
dogfishlm <- lm(log(INDWT) ~ log(LENGTH)*Time_Period, dogfish)
summary(dogfishlm)
# weight for a 100cm dogfish, early
exp(-13.039562)*(100^3.144316)
# weight for a 100cm dogfish, late
exp(-13.039562 + 0.248203)*(100^(3.144316-0.065107))
b.tibble["Spiny Dogfish", "1992 - 2002"] <- dogfishlm$coefficients[2]
b.tibble["Spiny Dogfish", "2003 - 2013"] <- dogfishlm$coefficients[2]+dogfishlm$coefficients[5]
b.tibble["Spiny Dogfish", "2014 - 2023"] <- dogfishlm$coefficients[2]+dogfishlm$coefficients[6]

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
gom.b["Spiny Dogfish", "1992 - 2002"] <- dogfish_model$coefficients[2] + dogfish_model$coefficients[10]
a.by.10yrs["Spiny Dogfish", "2003 - 2013"] <- dogfish_model$coefficients[1] + dogfish_model$coefficients[5] + dogfish_model$coefficients[3]
gom.b["Spiny Dogfish", "2003 - 2013"] <- dogfish_model$coefficients[2] + dogfish_model$coefficients[10] + dogfish_model$coefficients[8]
a.by.10yrs["Spiny Dogfish", "2014 - 2023"] <- dogfish_model$coefficients[1] + dogfish_model$coefficients[5] + dogfish_model$coefficients[4]
gom.b["Spiny Dogfish", "2014 - 2023"] <- dogfish_model$coefficients[2] + dogfish_model$coefficients[10] + dogfish_model$coefficients[9]

# Dogfish Model 2 - Includes Triple Interaction Term
dogfish_model2 <- lm(log(INDWT) ~ log(LENGTH)*Time_Period*EPU, data=dogfish)
summary(dogfish_model2)
# Comparing slopes for each EPU by Time_Period
em_dog <- emtrends(dogfish_model2, ~EPU*Time_Period, "log(LENGTH)", data = dogfish)
pairs(em_dog, simple = "Time_Period")
dogfish

# EPU DF
dogfish.epu.table <- matrix(nrow=4, ncol=3, byrow=TRUE)
rownames(dogfish.epu.table) <- c('GB', 'GOM', 'MAB', 'SS')
colnames(dogfish.epu.table) <- c('1992 - 2002', '2003 - 2013', '2014 - 2023')
# GB
dogfish.epu.table[1, 1] <- dogfish_model2$coefficients[2]
gb.b["Spiny Dogfish", "1992 - 2002"] <- dogfish.epu.table[1, 1]
dogfish.epu.table[1, 2] <- dogfish_model2$coefficients[2] + dogfish_model2$coefficients[8]
gb.b["Spiny Dogfish", "2003 - 2013"] <- dogfish.epu.table[1, 2]
dogfish.epu.table[1, 3] <- dogfish_model2$coefficients[2] + dogfish_model2$coefficients[9]
gb.b["Spiny Dogfish", "2014 - 2023"] <- dogfish.epu.table[1, 3]
# GOM
dogfish.epu.table[2, 1]<-dogfish_model2$coefficients[2]+dogfish_model2$coefficients[10]
gom.b["Spiny Dogfish", "1992 - 2002"] <- dogfish.epu.table[2, 1]
dogfish.epu.table[2, 2]<-dogfish_model2$coefficients[2]+dogfish_model2$coefficients[10]+dogfish_model2$coefficients[8]+dogfish_model2$coefficients[19]
gom.b["Spiny Dogfish", "2003 - 2013"] <- dogfish.epu.table[2, 2]
dogfish.epu.table[2, 3]<-dogfish_model2$coefficients[2]+dogfish_model2$coefficients[10]+dogfish_model2$coefficients[9]+dogfish_model2$coefficients[20]
gom.b["Spiny Dogfish", "2014 - 2023"] <- dogfish.epu.table[2, 3]
# MAB
dogfish.epu.table[3, 1]<-dogfish_model2$coefficients[2]+dogfish_model2$coefficients[11]
mab.b["Spiny Dogfish", "1992 - 2002"] <- dogfish.epu.table[3, 1]
dogfish.epu.table[3, 2]<-dogfish_model2$coefficients[2]+dogfish_model2$coefficients[11]+dogfish_model2$coefficients[8]+dogfish_model2$coefficients[21]
mab.b["Spiny Dogfish", "2003 - 2013"] <- dogfish.epu.table[3, 2]
dogfish.epu.table[3, 3]<-dogfish_model2$coefficients[2]+dogfish_model2$coefficients[11]+dogfish_model2$coefficients[9]+dogfish_model2$coefficients[22]
mab.b["Spiny Dogfish", "2014 - 2023"] <- dogfish.epu.table[3, 3]
# SS
dogfish.epu.table[4, 1]<-dogfish_model2$coefficients[2]+dogfish_model2$coefficients[12]
ss.b["Spiny Dogfish", "1992 - 2002"] <- dogfish.epu.table[4, 1]
dogfish.epu.table[4, 2]<-dogfish_model2$coefficients[2]+dogfish_model2$coefficients[12]+dogfish_model2$coefficients[8]+dogfish_model2$coefficients[23]
ss.b["Spiny Dogfish", "2003 - 2013"] <- dogfish.epu.table[4, 2]
dogfish.epu.table[4, 3]<-dogfish_model2$coefficients[2]+dogfish_model2$coefficients[12]+dogfish_model2$coefficients[9]+dogfish_model2$coefficients[24]
ss.b["Spiny Dogfish", "2014 - 2023"] <- dogfish.epu.table[4, 3]

# graphs by EPU
dogfish.epu.df <- as.data.frame(dogfish.epu.table, stringsAsFactors = FALSE)
dogfish.epu.df$EPU <- rownames(dogfish.epu.table)
dogfish.epu.df.long <- pivot_longer(dogfish.epu.df, 
                            cols = c("1992 - 2002", "2003 - 2013", "2014 - 2023"), 
                            names_to = "Time_Period", 
                            values_to = "Slope")
ggplot(dogfish.epu.df.long, aes(x = Time_Period, y = Slope, group = EPU, color = EPU)) +
  geom_line(size = 1.5, show.legend = FALSE) +
  geom_point(size = 3, show.legend = FALSE) +
  labs(title = "Spiny Dogfish: β Coefficient by EPU", 
       x = "Time Period", 
       y = "β Coefficient") +
  theme_bw() +
  theme(legend.position = "bottom",            
        legend.box.background = element_rect(color = "black"),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 16)) +
  scale_color_manual(name = "EPU", values = c("orangered2", "yellowgreen", "steelblue2", "purple"))->dogfish_epu_plot
ggsave(filename="DogfishEPUPlot.png", plot=dogfish_epu_plot, width=7, height=6)

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
# IGNORING INTERACTION
b.df <- as.data.frame(b.tibble, stringsAsFactors = FALSE)
b.df$Species <- rownames(b.tibble)
b.df.long <- pivot_longer(b.df, 
                          cols = c("1992 - 2002", "2003 - 2013", "2014 - 2023"), 
                          names_to = "Time_Period", 
                          values_to = "Slope")
ggplot(b.df.long, aes(x = Time_Period, y = Slope, group = Species, color = Species)) +
  geom_line() +
  geom_point() +
  labs(title = "β Coefficient by Species", 
       x = "Time Period", 
       y = "Slope (β)") +
  theme_bw()+
  theme(legend.box.background = element_rect(color="black"),
        legend.title = element_text(size = 15), 
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 15, face="bold"),
        text=element_text(size=15)) +
  scale_color_manual(name = "Species",  values = c("orangered2", "yellowgreen", "steelblue2", "purple"))->species_slopes
ggsave(filename="BBySpecies.png", plot=species_slopes, width=8, height=6)

# GB
gb.b.df <- as.data.frame(gb.b, stringsAsFactors = FALSE)
gb.b.df$Species <- rownames(gb.b)
gb.b.df.long <- pivot_longer(gb.b.df, 
                              cols = c("1992 - 2002", "2003 - 2013", "2014 - 2023"), 
                              names_to = "Time_Period", 
                              values_to = "Slope")

ggplot(gb.b.df.long, aes(x = Time_Period, y = Slope, group = Species, color = Species)) +
  geom_line() +
  geom_point() +
  labs(title = "Georges Bank: β Coefficient by Species", 
       x = "Time Period", 
       y = "Slope (β)") +
  theme_bw()+
  theme(legend.box.background = element_rect(color="black"),
        legend.title = element_text(size = 15), 
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 15, face="bold"),
        text=element_text(size=15)) +
  scale_color_manual(name = "Species",  values = c("orangered2", "yellowgreen", "steelblue2", "purple"))->BGB
ggsave(filename="BGB.png", plot=BGB, width=8, height=6)

# gom
gom.b.df <- as.data.frame(gom.b, stringsAsFactors = FALSE)
gom.b.df$Species <- rownames(gom.b)

gom.b.df.long <- pivot_longer(gom.b.df, 
                            cols = c("1992 - 2002", "2003 - 2013", "2014 - 2023"), 
                            names_to = "Time_Period", 
                            values_to = "Slope")

ggplot(gom.b.df.long, aes(x = Time_Period, y = Slope, group = Species, color = Species)) +
  geom_line() +
  geom_point() +
  labs(title = "Gulf of Maine: β Coefficient by Species", 
       x = "Time Period", 
       y = "Slope (β)") +
  theme_bw()+
  theme(legend.box.background = element_rect(color="black"),
        legend.title = element_text(size = 15), 
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 15, face="bold"),
        text=element_text(size=15)) +
  scale_color_manual(name = "Species",  values = c("orangered2", "yellowgreen", "steelblue2", "purple"))->BGOM
ggsave(filename="BGOM.png", plot=BGOM, width=8, height=6)

# MAB
mab.b.df <- as.data.frame(mab.b, stringsAsFactors = FALSE)
mab.b.df$Species <- rownames(mab.b)
mab.b.df.long <- pivot_longer(mab.b.df, 
                              cols = c("1992 - 2002", "2003 - 2013", "2014 - 2023"), 
                              names_to = "Time_Period", 
                              values_to = "Slope")

ggplot(mab.b.df.long, aes(x = Time_Period, y = Slope, group = Species, color = Species)) +
  geom_line() +
  geom_point() +
  labs(title = "Mid-Atlantic Bight: β Coefficient by Species", 
       x = "Time Period", 
       y = "Slope (β)") +
  theme_bw()+
  theme(legend.box.background = element_rect(color="black"),
        legend.title = element_text(size = 15), 
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 15, face="bold"),
        text=element_text(size=15)) +
  scale_color_manual(name = "Species",  values = c("yellowgreen","steelblue2", "purple"))->BMAB
ggsave(filename="BMAB.png", plot=BMAB, width=8, height=6)

# SS
ss.b.df <- as.data.frame(ss.b, stringsAsFactors = FALSE)
ss.b.df$Species <- rownames(ss.b)
ss.b.df.long <- pivot_longer(ss.b.df, 
                              cols = c("1992 - 2002", "2003 - 2013", "2014 - 2023"), 
                              names_to = "Time_Period", 
                              values_to = "Slope")

ggplot(ss.b.df.long, aes(x = Time_Period, y = Slope, group = Species, color = Species)) +
  geom_line() +
  geom_point() +
  labs(title = "Scotian Shelf: β Coefficient by Species", 
       x = "Time Period", 
       y = "Slope (β)") +
  theme_bw()+
  theme(legend.box.background = element_rect(color="black"),
        legend.title = element_text(size = 15), 
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 15, face="bold"),
        text=element_text(size=15)) +
  scale_color_manual(name = "Species",  values = c("orangered2", "yellowgreen", "steelblue2", "purple"))->BSS
ggsave(filename="BSS.png", plot=BSS, width=8, height=6)

### ---------------------------------------------------------------------------
### Coefficients 
### ---------------------------------------------------------------------------
unique_comname <- unique(lw_data$COMNAME)
unique_comname <- na.omit(unique_comname)

data <- lw_data %>%
  mutate(Time_Period = case_when(YEAR < 2000 ~ "Wigley",
                            YEAR >= 2016 ~ "Current")) %>%
  filter(!is.na(Time_Period)) %>%
  filter(!is.na(LENGTH) & !is.na(INDWT) & !is.na(COMNAME) & INDWT > 0)

grouped_data <- data %>%
  group_by(COMNAME, Time_Period) %>%
  summarise(count = n())

filtered_groups <- grouped_data %>%
  filter(count >= 5) %>%
  ungroup()

unique_species <- unique(filtered_groups$COMNAME)
unique_species
species <- c()
unique_species = unique_species[unique_species != "LOGGERHEAD SEATURTLE"]
unique_species
for (spec in unique_species) {
  spec_decs <- filtered_groups %>%
    filter(COMNAME == spec)
  if (nrow(spec_decs) == 2) {
    species <- c(species, spec)
  }
}
species
b.by.Time_Period.df <- data.frame(row.names = species)
lna.by.Time_Period.df <- data.frame(row.names = species)
Time_Periods = c('Wigley', 'Recent')

# filtering out large outliers/loggerhead seaturtle
data <- data %>%
  filter(! (COMNAME == "AMERICAN PLAICE" & INDWT > 4) ) %>%
  filter(! (COMNAME == "ATLANTIC HERRING" & INDWT > 2)) %>%
  filter(! (COMNAME == "BLUNTNOSE STINGRAY" & INDWT > 100)) %>%
  filter(! (COMNAME == "BLUNTNOSE STINGRAY" & INDWT > 10 & LENGTH < 15)) %>%
  filter(! (COMNAME == "LITTLE SKATE" & INDWT > 4)) %>%
  filter(! (COMNAME == "LONGFIN SQUID" & INDWT > 1)) %>%
  filter(! (COMNAME == "NORTHERN KINGFISH" & INDWT > 1)) %>%
  filter(! (COMNAME == "NORTHERN SHORTFIN SQUID" & INDWT > 1))

for (d in Time_Periods) {
    b.by.Time_Period.df["B.Wigley"] <- NA
    b.by.Time_Period.df["B.Wigley.s"] <- NA
    b.by.Time_Period.df["B.Recent"] <- NA
    b.by.Time_Period.df["B.Recent.s"] <- NA
    
    lna.by.Time_Period.df["lnA.Wigley"] <- NA
    lna.by.Time_Period.df["lnA.Wigley.s"] <- NA
    lna.by.Time_Period.df["lnA.Recent"] <- NA
    lna.by.Time_Period.df["lnA.Recent.s"] <- NA
}

# for loop taking care of beta p-values, increases, and decreases
for (spec in species) {
  species_dat <- data %>%
    filter(COMNAME == spec)
  cur_lm <- lm(log(species_dat$INDWT) ~ log(species_dat$LENGTH)*species_dat$Time_Period)
  p_value <- summary(cur_lm)$coefficients["log(species_dat$LENGTH):species_dat$Time_PeriodWigley", 4]
  b.by.Time_Period.df[spec, "B.Wigley"] <- cur_lm$coefficients[2]
  b.by.Time_Period.df[spec, "B.Recent"] <- cur_lm$coefficients[2]+cur_lm$coefficients[4]
  b.by.Time_Period.df[spec, "B.Wigley.s"] <- summary(cur_lm)$coefficients[2,2]
  b.by.Time_Period.df[spec, "B.Recent.s"] <- summary(cur_lm)$coefficients[4,2]
  b.by.Time_Period.df[spec, "B.P"] <- p_value
  if (p_value < 0.5) {
    change <- b.by.Time_Period.df[spec, "B.Recent"] - b.by.Time_Period.df[spec, "B.Wigley"]
    if (change > 0) {
      b.by.Time_Period.df[spec, "B.Result"] <- "Increase"
    } else {
      b.by.Time_Period.df[spec, "B.Result"] <- "Decrease"
    }
  }
  else {
    b.by.Time_Period.df[spec, "B.Result"] <- "No Significant Change"
  }
}

# Counts of Beta Changes
ggplot(b.by.Time_Period.df, aes(x = B.Result)) +
  geom_bar(fill = "steelblue") + 
  labs(title = "Changes in β Between 1992-1999 and 2016-2023",
       x = "Result",
       y = "Number of Species") +
  theme_minimal() +
  theme(plot.title = element_text(size = 18, face = "bold"),  
        axis.title = element_text(size = 16),                
        axis.text = element_text(size = 16))-> beta_changes
ggsave(filename="Beta_changes.png", plot=beta_changes, width=8, height=6)
beta_changes

beta_counts <- b.by.Time_Period.df %>%
  group_by(B.Result) %>%
  summarize(count = n())
beta_counts

# looking at species names who showed an increase
increase_species <- b.by.Time_Period.df %>%
  filter(B.Result == "Increase") %>%
  rownames_to_column("Species") %>%  # If species names are row names
  select(Species)

# looking at species names who showed an decrease
decrease_species <- b.by.Time_Period.df %>%
  filter(B.Result == "Decrease") %>%
  rownames_to_column("Species") %>%  # If species names are row names
  select(Species)
decrease_species

# dealing with alpha: p-values, increases, and decreases
for (spec in species) {
  cur_lm <- lm(log(species_dat$INDWT) ~ log(species_dat$LENGTH)*species_dat$Time_Period)
  lna.by.Time_Period.df[spec, "lnA.Wigley"] <- cur_lm$coefficients[1]
  lna.by.Time_Period.df[spec, "lnA.Recent"] <- cur_lm$coefficients[1]+cur_lm$coefficients[3]
  lna.by.Time_Period.df[spec, "lnA.Wigley.s"] <- summary(cur_lm)$coefficients[1,2]
  lna.by.Time_Period.df[spec, "lnA.Recent.s"] <- summary(cur_lm)$coefficients[3,2]
  lna.by.Time_Period.df[spec, "lnA.P"] <- p_value
  species_dat <- data %>%
    filter(COMNAME == spec)
  p_value <- summary(cur_lm)$coefficients["species_dat$Time_PeriodWigley", 4]
  lna.by.Time_Period.df[spec, "lnA.P"] <- p_value
  if (p_value < 0.5) {
    change <- lna.by.Time_Period.df[spec, "lnA.Recent"] - lna.by.Time_Period.df[spec, "lnA.Wigley"]
    if (change > 0) {
      lna.by.Time_Period.df[spec, "lnA.Result"] <- "Increase"
    } else {
      lna.by.Time_Period.df[spec, "lnA.Result"] <- "Decrease"
    }
  }
  else {
    lna.by.Time_Period.df[spec, "lnA.Result"] <- "No Significant Change"
  }
}

# Counts of Alpha Changes
ggplot(lna.by.Time_Period.df, aes(x = lnA.Result)) +
  geom_bar(fill = "steelblue") + 
  labs(title = "Changes in ln(α) Between 1992-1999 and 2016-2023",
       x = "Result",
       y = "Number of Species") +
  theme_minimal() +
  theme(text = element_text(size=12, face="bold")) -> alpha_changes
ggsave(filename="Alpha_changes.png", plot=alpha_changes, width=6, height=8)

alpha_counts <- lna.by.Time_Period.df %>%
  group_by(lnA.Result) %>%
  summarize(count = n())
alpha_counts

# rounding estimates & s
b.by.Time_Period.df$B.Wigley <- round(b.by.Time_Period.df$B.Wigley, 3)
b.by.Time_Period.df$B.Recent <- round(b.by.Time_Period.df$B.Recent, 3)
b.by.Time_Period.df$B.Wigley.s <- round(b.by.Time_Period.df$B.Wigley.s, 5)
b.by.Time_Period.df$B.Recent.s <- round(b.by.Time_Period.df$B.Recent.s, 5)
lna.by.Time_Period.df$lnA.Wigley <- round(lna.by.Time_Period.df$lnA.Wigley, 3)
lna.by.Time_Period.df$lnA.Recent <- round(lna.by.Time_Period.df$lnA.Recent, 3)
lna.by.Time_Period.df$lnA.Wigley.s <- round(lna.by.Time_Period.df$lnA.Wigley.s, 5)
lna.by.Time_Period.df$lnA.Recent.s <- round(lna.by.Time_Period.df$lnA.Recent.s, 5)

# rounding P values
b.by.Time_Period.df <- b.by.Time_Period.df %>%
  mutate(B.P = ifelse(B.P < 0.001, "<0.001", sprintf("%.3f", B.P)))
lna.by.Time_Period.df <- lna.by.Time_Period.df %>%
  mutate(lnA.P = ifelse(lnA.P < 0.001, "<0.001", sprintf("%.3f", lnA.P)))
library(stringr)
rownames(b.by.Time_Period.df) <- str_to_title(tolower(rownames(b.by.Time_Period.df)))
rownames(lna.by.Time_Period.df) <- str_to_title(tolower(rownames(lna.by.Time_Period.df)))
rownames(change.df) <- str_to_title(tolower(rownames(change.df)))

write.csv(b.by.Time_Period.df,"~/Downloads/BetaFile.csv", row.names = TRUE)
write.csv(lna.by.Time_Period.df,"~/Downloads/AlphaFile.csv", row.names = TRUE)
