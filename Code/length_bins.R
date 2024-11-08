# -----------------------------------------------------------------------------
# 4 Species - Lengths Through Time
# -----------------------------------------------------------------------------

length_data <- readRDS("Data/nefsc_weight_at_length.rds") # loading in data
library(tidyverse)

# -----------------------------------------------------------------------------
# Atlantic Cod
# -----------------------------------------------------------------------------
cod <- length_data %>%
  filter(scientific_name == "gadus morhua")

cod <- cod %>% 
  group_by(longitude, latitude, trawl_id, season, year, scientific_name, survey) %>% 
  mutate(number_at_length = round(number_at_length)) %>% 
  uncount(number_at_length) %>%
  mutate(decade = case_when(year >= 1970 & year < 1980 ~ "1970s",
                            year >= 1980 & year < 1990 ~ "1980s",
                            year >= 1990 & year < 2000 ~ "1990s",
                            year >= 2000 & year < 2010 ~ "2000s",
                            year >= 2010 ~ "2010s & 2020s")) %>%
  filter(!is.na(decade))

cod_10s <- cod %>%
  filter(decade == "2010s & 2020s")

cod_70s <- cod %>%
  filter(decade == "1970s")

codp90 <- quantile(cod$length_cm, 0.9)
codp90late <- quantile(cod_10s$length_cm, 0.9)
codp90early <- quantile(cod_70s$length_cm, 0.9)

ggplot() +
  geom_histogram(data=cod, aes(x = length_cm), binwidth = 10, fill = "grey", color = "black", alpha = 0.7) +
  geom_vline(aes(xintercept = codp90), color = "black", linetype = "dashed", size = 1) +
  annotate("text", x = codp90, y = Inf, label = paste('90th percentile, all data:', round(codp90, 2), "cm"), 
           hjust = -0.12, vjust = 11, color = "black") +
  labs(title = "Histogram of Cod Lengths (cm), 1970-2023", x = "Length (cm)", y = "Frequency") +
  theme_minimal()+
  theme(text=element_text(size=12))-> codhist
ggsave(filename="CodHist.png",plot=codhist,width=5,height=3)

ggplot() +
  geom_histogram(data=cod, aes(x = length_cm), binwidth = 10, fill = "grey", color = "black", alpha = 0.7) +
  geom_histogram(data=cod_10s, aes(x = length_cm), binwidth=10, fill= "blue", color="black",alpha=0.7) +
  geom_vline(aes(xintercept = codp90), color = "black", linetype = "dashed", size = 1) +
  annotate("text", x = codp90, y = Inf, label = paste('90th percentile, all data:', round(codp90, 2), "cm"), 
           hjust = -0.12, vjust = 11, color = "black") +
  geom_vline(aes(xintercept = codp90late), color = "blue", linetype = "dashed", size = 1) +
  annotate("text", x = codp90late, y = Inf, label = paste('90th percentile for 2010-2023:', round(codp90late, 2), "cm"), 
           hjust = -0.27, vjust = 14, color = "blue") +
  labs(title = "Histogram of Cod Lengths (cm), 1970-2023", x = "Length (cm)", y = "Frequency") +
  theme_minimal()+
  theme(text=element_text(size=12))-> p
ggsave(filename="CodHist2.png",plot=p,width=5,height=3)

# total cod
cod_tot <- cod %>%
  group_by(decade) %>%
  summarize(total_count = n()) %>%
  ungroup()

# 90th percentile cod
cod_90th <- cod %>%
  filter(length_cm > quantile(cod$length_cm, probs = c(0.9))) %>%
  group_by(decade) %>%
  summarize(ninetieth_count = n()) %>%
  ungroup()

cod_90th$percent <- cod_90th$ninetieth_count / sum(cod_90th$ninetieth_count)

# plot cod in 90th percentile by decade
ggplot(cod_90th, aes(x = decade, y = ninetieth_count)) +
  geom_bar(stat = "identity", fill="grey", colour="black") +
  labs(title = "Number of Atlantic Cod in the 90th Percentile of Length Found in Each Decade",
       x = "Decade",
       y = "Count") +
  theme_bw()+
  theme(text=element_text(size=15),
        plot.title=element_text(size=15)) -> cod_counts
ggsave(filename="Cod90Counts.png", plot=cod_counts, width=8, height=6)

# merge data
cod_combined <- left_join(cod_tot, cod_90th, by = "decade")

# calculate in terms of %
cod_combined <- cod_combined %>%
  mutate(total_percentage = (total_count / sum(total_count)) * 100,
         ninetieth_percentage = (ninetieth_count / sum(ninetieth_count)) * 100)

cod_plot <- cod_combined %>%
  dplyr::select(decade, total_percentage, ninetieth_percentage) %>%
  pivot_longer(cols = c(total_percentage, ninetieth_percentage), 
               names_to = "type", 
               values_to = "percentage") %>%
  mutate(type = recode(type,
                       total_percentage = "% Total Cod",
                       ninetieth_percentage = "% Cod in 90th Percentile"))

ggplot(cod_plot, aes(x = decade, y = percentage, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Percentage of Cod Caught in Trawl Survey per Decade",
       x = "Decade",
       y = "Percentage",
       fill = "Type") +
  theme_bw()+
  theme(legend.box.background = element_rect(color="black"),
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 15)) +
  scale_color_manual(name = "Type",  values = c("firebrick4", "steelblue2"),
                     aesthetics = "fill") 

### Cod List - Standardized
cod_list <- c()
for (i in 1:5) {
  cod_list <- c(cod_list, (cod_90th[i, 2] / cod_tot[i, 2])[1, 1] * 100)
}
cod_df <- as.data.frame(cod_list, stringsAsFactors = TRUE)
cod_df <- data.frame(cod_list, Decade = c("1970s", "1980s", "1990s", "2000s", "2010s and 2020s"))
cod_long <- pivot_longer(cod_df, cols = "cod_list", names_to = "Metric", values_to = "value")
ggplot(cod_long, aes(x = Decade, y = value, group = 1)) +
  geom_line(size=1.5) +
  geom_point(size=3) +
  labs(title = "Proportion of 90th Percentile Atlantic Cod", 
       x = "Decade", y = "% of Large Individuals Caught") +
  theme_bw(base_size=18)+  
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 15),
        text = element_text(size = 15)) -> cod_90_props
ggsave(filename="Cod90Line.png", plot=cod_90_props, width=7, height=6)
cod_90_props
cod_long
ggsave(filename="Cod90Line.png", plot=cod_90_props, width=8, height=6)

# experimenting... by year instead of decade
cod_year_counts <- cod %>%
  group_by(year) %>%
  summarize(total_count = n()) %>%
  ungroup()

cod_year_counts_90 <- cod %>%
  filter(length_cm >= quantile(cod$length_cm, 0.9)) %>%
  group_by(year) %>%
  summarize(ninetieth_count = n()) %>%
  ungroup()

cod_com <- left_join(cod_year_counts, cod_year_counts_90, by="year")
cod_com$as_percent <- cod_com$ninetieth_count / cod_com$total_count
cod_com <- as.data.frame(cod_com)
ggplot(cod_com, aes(x=year, y=as_percent)) +
  geom_line() +
  geom_point() +
  labs(title = "Atlantic Cod: Number in 90th Percentile / Total Number Caught", 
       x = "Year", y = "Value") +
  theme_bw()

# -----------------------------------------------------------------------------
# American Plaice
# -----------------------------------------------------------------------------
plaice <- length_data %>%
  filter(scientific_name == "hippoglossoides platessoides")

plaice <- plaice %>% 
  group_by(longitude, latitude, trawl_id, season, year, scientific_name, survey) %>% 
  mutate(number_at_length = round(number_at_length)) %>% 
  uncount(number_at_length) %>%
  mutate(decade = case_when(year >= 1970 & year < 1980 ~ "1970s",
                            year >= 1980 & year < 1990 ~ "1980s",
                            year >= 1990 & year < 2000 ~ "1990s",
                            year >= 2000 & year < 2010 ~ "2000s",
                            year >= 2010 ~ "2010s & 2020s")) %>%
  filter(!is.na(decade))

plaice_10s <- plaice %>%
  filter(decade == "2010s & 2020s")

plaice_70s <- plaice %>%
  filter(decade == "1970s")

plaicep90 <- quantile(plaice$length_cm, 0.9)
plaicep90late <- quantile(plaice_10s$length_cm, 0.9)
plaicep90early <- quantile(plaice_70s$length_cm, 0.9)


ggplot() +
  geom_histogram(data=plaice, aes(x = length_cm), binwidth = 5, fill = "grey", color = "black", alpha = 0.7) +
  geom_vline(aes(xintercept = plaicep90), color = "black", linetype = "dashed", size = 1) +
  annotate("text", x = plaicep90, y = Inf, label = paste('90th percentile, all data:', round(plaicep90, 2)), 
           hjust = -0.12, vjust = 11, color = "black") +
  labs(title = "Histogram of American Plaice Lengths (cm), 1970-2023", x = "Length (cm)", y = "Frequency") +
  theme_minimal()

ggplot() +
  geom_histogram(data=plaice, aes(x = length_cm), binwidth = 5, fill = "grey", color = "black", alpha = 0.7) +
  geom_histogram(data=plaice_70s, aes(x = length_cm), binwidth=5, fill="red", color="black", alpha=0.5) +
  geom_histogram(data=plaice_10s, aes(x = length_cm), binwidth=5, fill= "blue", color="black",alpha=0.7) +
  geom_vline(aes(xintercept = plaicep90), color = "black", linetype = "dashed", size = 1) +
  annotate("text", x = plaicep90, y = Inf, label = paste('90th percentile, all data:', round(plaicep90, 2)), 
           hjust = -0.16, vjust = 11, color = "black") +
  geom_vline(aes(xintercept = plaicep90late), color = "blue", linetype = "dashed", size = 1) +
  annotate("text", x = plaicep90late, y = Inf, label = paste('90th percentile for 2010-2023:', round(plaicep90late, 2)), 
           hjust = -0.22, vjust = 14, color = "blue") +
  geom_vline(aes(xintercept = plaicep90early), color = "red", linetype = "dashed", size = 1) +
  annotate("text", x = plaicep90early, y = Inf, label = paste('90th percentile for 1970s:', round(plaicep90early, 2)), 
           hjust = -0.05, vjust = 17, color = "red") +
  labs(title = "Histogram of American Plaice Lengths (cm), 1970-2023", x = "Length (cm)", y = "Frequency") +
  theme_minimal()

# total plaice
plaice_tot <- plaice %>%
  group_by(decade) %>%
  summarize(total_count = n()) %>%
  ungroup()

# 90th percentile plaice
plaice_90th <- plaice %>%
  filter(length_cm > quantile(plaice$length_cm, probs = c(0.9))) %>%
  group_by(decade) %>%
  summarize(ninetieth_count = n()) %>%
  ungroup()

# plot plaice in 90th percentile by decade
ggplot(plaice_90th, aes(x = decade, y = ninetieth_count)) +
  geom_bar(stat = "identity", fill="grey", colour="black") +
  labs(title = "Number of American Plaice in the 90th Percentile of Length Found in Each Decade",
       x = "Decade",
       y = "Count") +
  theme_bw()

# merge data
plaice_combined <- left_join(plaice_tot, plaice_90th, by = "decade")

# calculate in terms of %
plaice_combined <- plaice_combined %>%
  mutate(total_percentage = (total_count / sum(total_count)) * 100,
         ninetieth_percentage = (ninetieth_count / sum(ninetieth_count)) * 100)

plaice_plot <- plaice_combined %>%
  dplyr::select(decade, total_percentage, ninetieth_percentage) %>%
  pivot_longer(cols = c(total_percentage, ninetieth_percentage), 
               names_to = "type", 
               values_to = "percentage") %>%
  mutate(type = recode(type,
                       total_percentage = "% Total Plaice",
                       ninetieth_percentage = "% Plaice in 90th Percentile"))

ggplot(plaice_plot, aes(x = decade, y = percentage, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Percentage of American Plaice Caught in Trawl Survey per Decade",
       x = "Decade",
       y = "Percentage",
       fill = "Type") +
  theme_bw()+
  theme(legend.box.background = element_rect(color="black"),
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 15)) +
  scale_color_manual(name = "Type",  values = c("firebrick4", "steelblue2"),
                     aesthetics = "fill") 

### Plaice List - Standardized
plaice_list <- c()
for (i in 1:5) {
  plaice_list <- c(plaice_list, (plaice_90th[i, 2] / plaice_tot[i, 2])[1, 1] * 100)
}
plaice_df <- as.data.frame(plaice_list, stringsAsFactors = TRUE)
plaice_df <- data.frame(plaice_list, Decade = c("1970s", "1980s", "1990s", "2000s", "2010s and 2020s"))
plaice_long <- pivot_longer(plaice_df, cols = "plaice_list", names_to = "Metric", values_to = "value")
ggplot(plaice_long, aes(x = Decade, y = value, group = 1)) +
  geom_line(size=1.5) +
  geom_point(size=3) +
  labs(title = "Proportion of 90th Percentile American Plaice", 
       x = "Decade", y = "% of Large Individuals Caught") +
  theme_bw(base_size=18)+  
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 15),
        text = element_text(size = 15)) -> plaice_90_props
ggsave(filename="Plaice90Line.png", plot=plaice_90_props, width=7, height=6)
plaice_90_props

# experimenting... by year instead of decade
plaice_year_counts <- plaice %>%
  group_by(year) %>%
  summarize(total_count = n()) %>%
  ungroup()

plaice_year_counts_90 <- plaice %>%
  filter(length_cm >= quantile(plaice$length_cm, 0.9)) %>%
  group_by(year) %>%
  summarize(ninetieth_count = n()) %>%
  ungroup()

plaice_com <- left_join(plaice_year_counts, plaice_year_counts_90, by="year")
plaice_com$as_percent <- plaice_com$ninetieth_count / plaice_com$total_count
plaice_com <- as.data.frame(plaice_com)
ggplot(plaice_com, aes(x=year, y=as_percent)) +
  geom_line() +
  geom_point() +
  labs(title = "American Plaice: Number in 90th Percentile / Total Number Caught", 
       x = "Year", y = "Value") +
  theme_bw()

# -----------------------------------------------------------------------------
# Atlantic Herring
# -----------------------------------------------------------------------------
herring <- length_data %>%
  filter(scientific_name == "clupea harengus")

herring <- herring %>% 
  group_by(longitude, latitude, trawl_id, season, year, scientific_name, survey) %>% 
  mutate(number_at_length = round(number_at_length)) %>% 
  uncount(number_at_length) %>%
  mutate(decade = case_when(year >= 1970 & year < 1980 ~ "1970s",
                            year >= 1980 & year < 1990 ~ "1980s",
                            year >= 1990 & year < 2000 ~ "1990s",
                            year >= 2000 & year < 2010 ~ "2000s",
                            year >= 2010 ~ "2010s & 2020s")) %>%
  filter(!is.na(decade))

herring_10s <- herring %>%
  filter(decade == "2010s & 2020s")

herring_70s <- herring %>%
  filter(decade == "1970s")

herringp90 <- quantile(herring$length_cm, 0.9)
herringp90late <- quantile(herring_10s$length_cm, 0.9)
herringp90early <- quantile(herring_70s$length_cm, 0.9)

ggplot() +
  geom_histogram(data=herring, aes(x = length_cm), binwidth = 2, fill = "grey", color = "black", alpha = 0.7) +
  geom_vline(aes(xintercept = herringp90), color = "black", linetype = "dashed", size = 1) +
  annotate("text", x = herringp90, y = Inf, label = paste('90th percentile, all data:', round(herringp90, 2)), 
           hjust = -0.12, vjust = 11, color = "black") +
  labs(title = "Histogram of Atlantic Herring Lengths (cm), 1970-2023", x = "Length (cm)", y = "Frequency") +
  theme_minimal()

ggplot() +
  geom_histogram(data=herring, aes(x = length_cm), binwidth = 2, fill = "grey", color = "black", alpha = 0.7) +
  geom_histogram(data=herring_70s, aes(x = length_cm), binwidth=2, fill="red", color="black", alpha=0.5) +
  geom_histogram(data=herring_10s, aes(x = length_cm), binwidth=2, fill= "blue", color="black",alpha=0.7) +
  geom_vline(aes(xintercept = herringp90), color = "black", linetype = "dashed", size = 1) +
  annotate("text", x = herringp90, y = Inf, label = paste('90th percentile, all data:', round(herringp90, 2)), 
           hjust = -0.25, vjust = 11, color = "black") +
  geom_vline(aes(xintercept = herringp90late), color = "blue", linetype = "dashed", size = 1) +
  annotate("text", x = herringp90late, y = Inf, label = paste('90th percentile for 2010-2023:', round(herringp90late, 2)), 
           hjust = -0.27, vjust = 14, color = "blue") +
  geom_vline(aes(xintercept = herringp90early), color = "red", linetype = "dashed", size = 1) +
  annotate("text", x = herringp90early, y = Inf, label = paste('90th percentile for 1970s:', round(herringp90early, 2)), 
           hjust = -0.08, vjust = 17, color = "red") +
  labs(title = "Histogram of Atlantic Herring Lengths (cm), 1970-2023", x = "Length (cm)", y = "Frequency") +
  theme_minimal()

# total herring
herring_tot <- herring %>%
  group_by(decade) %>%
  summarize(total_count = n()) %>%
  ungroup()

# 90th percentile herring
herring_90th <- herring %>%
  filter(length_cm > quantile(herring$length_cm, probs = c(0.9))) %>%
  group_by(decade) %>%
  summarize(ninetieth_count = n()) %>%
  ungroup()

# plot herring in 90th percentile by decade
ggplot(herring_90th, aes(x = decade, y = ninetieth_count)) +
  geom_bar(stat = "identity", fill="grey", colour="black") +
  labs(title = "Number of Atlantic Herring in the 90th Percentile of Length Found in Each Decade",
       x = "Decade",
       y = "Count") +
  theme_bw()

# merge data
herring_combined <- left_join(herring_tot, herring_90th, by = "decade")

# calculate in terms of %
herring_combined <- herring_combined %>%
  mutate(total_percentage = (total_count / sum(total_count)) * 100,
         ninetieth_percentage = (ninetieth_count / sum(ninetieth_count)) * 100)

herring_plot <- herring_combined %>%
  dplyr::select(decade, total_percentage, ninetieth_percentage) %>%
  pivot_longer(cols = c(total_percentage, ninetieth_percentage), 
               names_to = "type", 
               values_to = "percentage") %>%
  mutate(type = recode(type,
                       total_percentage = "% Total Herring",
                       ninetieth_percentage = "% Herring in 90th Percentile"))

ggplot(herring_plot, aes(x = decade, y = percentage, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Percentage of Atlantic Herring Caught in Trawl Survey per Decade",
       x = "Decade",
       y = "Percentage",
       fill = "Type") +
  theme_bw()+
  theme(legend.box.background = element_rect(color="black"),
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 15)) +
  scale_color_manual(name = "Type",  values = c("firebrick4", "steelblue2"),
                     aesthetics = "fill") 

### Atlantic Herring List - Standardized
herring_list <- c()
for (i in 1:5) {
  herring_list <- c(herring_list, (herring_90th[i, 2] / herring_tot[i, 2])[1, 1] * 100)
}
herring_df <- as.data.frame(herring_list, stringsAsFactors = TRUE)
herring_df <- data.frame(herring_list, Decade = c("1970s", "1980s", "1990s", "2000s", "2010s and 2020s"))
herring_long <- pivot_longer(herring_df, cols = "herring_list", names_to = "Metric", values_to = "value")
ggplot(herring_long, aes(x = Decade, y = value, group = 1)) +
  geom_line(size=1.5) +
  geom_point(size=3) +
  labs(title = "Proportion of 90th Percentile Atlantic Herring", 
       x = "Decade", y = "% of Large Individuals Caught") +
  theme_bw(base_size=18)+  
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 15),
        text = element_text(size = 15)) -> herring_90_props
herring_90_props
ggsave(filename="Herring90Line.png", plot=herring_90_props, width=7, height=6)

# experimenting... by year instead of decade
herring_year_counts <- herring %>%
  group_by(year) %>%
  summarize(total_count = n()) %>%
  ungroup()

herring_year_counts_90 <- herring %>%
  filter(length_cm >= quantile(herring$length_cm, 0.9)) %>%
  group_by(year) %>%
  summarize(ninetieth_count = n()) %>%
  ungroup()

herring_com <- left_join(herring_year_counts, herring_year_counts_90, by="year")
herring_com$as_percent <- herring_com$ninetieth_count / herring_com$total_count
herring_com <- as.data.frame(herring_com)
ggplot(herring_com, aes(x=year, y=as_percent)) +
  geom_line() +
  geom_point() +
  labs(title = "Atlantic Herring: Number in 90th Percentile / Total Number Caught", 
       x = "Year", y = "Value") +
  theme_bw()

# -----------------------------------------------------------------------------
# Spiny Dogfish
# -----------------------------------------------------------------------------
dogfish <- length_data %>%
  filter(scientific_name == "squalus acanthias")

dogfish <- dogfish %>% 
  group_by(longitude, latitude, trawl_id, season, year, scientific_name, survey) %>% 
  mutate(number_at_length = round(number_at_length)) %>% 
  uncount(number_at_length) %>%
  mutate(decade = case_when(year >= 1970 & year < 1980 ~ "1970s",
                            year >= 1980 & year < 1990 ~ "1980s",
                            year >= 1990 & year < 2000 ~ "1990s",
                            year >= 2000 & year < 2010 ~ "2000s",
                            year >= 2010 ~ "2010s & 2020s")) %>%
  filter(!is.na(decade))

dogfish_10s <- dogfish %>%
  filter(decade == "2010s & 2020s")

dogfish_70s <- dogfish %>%
  filter(decade == "1970s")

dogfishp90 <- quantile(dogfish$length_cm, 0.9)
dogfishp90late <- quantile(dogfish_10s$length_cm, 0.9)
dogfishp90early <- quantile(dogfish_70s$length_cm, 0.9)

ggplot() +
  geom_histogram(data=dogfish, aes(x = length_cm), binwidth = 10, fill = "grey", color = "black", alpha = 0.7) +
  geom_vline(aes(xintercept = dogfishp90), color = "black", linetype = "dashed", size = 1) +
  annotate("text", x = dogfishp90, y = Inf, label = paste('90th percentile, all data:', round(dogfishp90, 2)), 
           hjust = -0.12, vjust = 11, color = "black") +
  labs(title = "Histogram of Spiny Dogfish Lengths (cm), 1970-2023", x = "Length (cm)", y = "Frequency") +
  theme_minimal()

ggplot() +
  geom_histogram(data=dogfish, aes(x = length_cm), binwidth = 10, fill = "grey", color = "black", alpha = 0.7) +
  geom_histogram(data=dogfish_70s, aes(x = length_cm), binwidth=10, fill="red", color="black", alpha=0.5) +
  geom_histogram(data=dogfish_10s, aes(x = length_cm), binwidth=10, fill= "blue", color="black",alpha=0.7) +
  geom_vline(aes(xintercept = dogfishp90), color = "black", linetype = "dashed", size = 1) +
  annotate("text", x = dogfishp90, y = Inf, label = paste('90th percentile, all data:', round(dogfishp90, 2)), 
           hjust = -0.28, vjust = 11, color = "black") +
  geom_vline(aes(xintercept = dogfishp90late), color = "blue", linetype = "dashed", size = 1) +
  annotate("text", x = dogfishp90late, y = Inf, label = paste('90th percentile for 2010-23:', round(dogfishp90late, 2)), 
           hjust = -0.32, vjust = 14, color = "blue") +
  geom_vline(aes(xintercept = dogfishp90early), color = "red", linetype = "dashed", size = 1) +
  annotate("text", x = dogfishp90early, y = Inf, label = paste('90th percentile for 1970s:', round(dogfishp90early, 2)), 
           hjust = -0.04, vjust = 17, color = "red") +
  labs(title = "Histogram of Spiny Dogfish Lengths (cm), 1970-2023", x = "Length (cm)", y = "Frequency") +
  theme_minimal()

# total dogfish
dogfish_tot <- dogfish %>%
  group_by(decade) %>%
  summarize(total_count = n()) %>%
  ungroup()

# 90th percentile dogfish
dogfish_90th <- dogfish %>%
  filter(length_cm > quantile(dogfish$length_cm, probs = c(0.9))) %>%
  group_by(decade) %>%
  summarize(ninetieth_count = n()) %>%
  ungroup()

# plot dogfish in 90th percentile by decade
ggplot(dogfish_90th, aes(x = decade, y = ninetieth_count)) +
  geom_bar(stat = "identity", fill="grey", colour="black") +
  labs(title = "Number of Spiny Dogfish in the 90th Percentile of Length Found in Each Decade",
       x = "Decade",
       y = "Count") +
  theme_bw()

# merge data
dogfish_combined <- left_join(dogfish_tot, dogfish_90th, by = "decade")

# calculate in terms of %
dogfish_combined <- dogfish_combined %>%
  mutate(total_percentage = (total_count / sum(total_count)) * 100,
         ninetieth_percentage = (ninetieth_count / sum(ninetieth_count)) * 100)

dogfish_plot <- dogfish_combined %>%
  dplyr::select(decade, total_percentage, ninetieth_percentage) %>%
  pivot_longer(cols = c(total_percentage, ninetieth_percentage), 
               names_to = "type", 
               values_to = "percentage") %>%
  mutate(type = recode(type,
                       total_percentage = "% Total Dogfish",
                       ninetieth_percentage = "% Dogfish in 90th Percentile"))

ggplot(dogfish_plot, aes(x = decade, y = percentage, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Percentage of Spiny Dogfish Caught in Trawl Survey per Decade",
       x = "Decade",
       y = "Percentage",
       fill = "Type") +
  theme_bw()+
  theme(legend.box.background = element_rect(color="black"),
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 15)) +
  scale_color_manual(name = "Type",  values = c("firebrick4", "steelblue2"),
                     aesthetics = "fill") 

### Spiny Dogfish List - Standardized
dogfish_list <- c()
for (i in 1:5) {
  dogfish_list <- c(dogfish_list, (dogfish_90th[i, 2] / dogfish_tot[i, 2])[1, 1] * 100)
}
dogfish_df <- as.data.frame(dogfish_list, stringsAsFactors = TRUE)
dogfish_df <- data.frame(dogfish_list, Decade = c("1970s", "1980s", "1990s", "2000s", "2010s and 2020s"))
dogfish_long <- pivot_longer(dogfish_df, cols = "dogfish_list", names_to = "Metric", values_to = "value")
dogfish_long
ggplot(dogfish_long, aes(x = Decade, y = value, group = 1)) +
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  labs(title = "Proportion of 90th Percentile Spiny Dogfish", 
       x = "Decade", y = "% of Large Individuals Caught") +
  theme_bw(base_size=18)+  
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 15),
        text = element_text(size = 15)) -> dogfish_90_props
dogfish_90_props
ggsave(filename="Dogfish90Line.png", plot=dogfish_90_props, width=7, height=6)

# experimenting... by year instead of decade
dogfish_year_counts <- dogfish %>%
  group_by(year) %>%
  summarize(total_count = n()) %>%
  ungroup()

dogfish_year_counts_90 <- dogfish %>%
  filter(length_cm >= quantile(dogfish$length_cm, 0.9)) %>%
  group_by(year) %>%
  summarize(ninetieth_count = n()) %>%
  ungroup()

dogfish_com <- left_join(dogfish_year_counts, dogfish_year_counts_90, by="year")
dogfish_com$as_percent <- dogfish_com$ninetieth_count / dogfish_com$total_count
dogfish_com <- as.data.frame(dogfish_com)
ggplot(dogfish_com, aes(x=year, y=as_percent)) +
  geom_line() +
  geom_point() +
  labs(title = "Spiny Dogfish: Number in 90th Percentile / Total Number Caught", 
       x = "Year", y = "Value") +
  theme_bw()
