# -----------------------------------------------------------------------------
# 4 Species - Lengths Through Time
# -----------------------------------------------------------------------------
length_data <- readRDS("Data/nefsc_weight_at_length.rds")
library(tidyverse)
# -----------------------------------------------------------------------------
# Atlantic Cod
# -----------------------------------------------------------------------------
cod <- length_data %>%
  filter(scientific_name == "gadus morhua")

cod %>% 
  group_by(longitude, latitude, trawl_id, season, year, scientific_name, survey) %>% 
  mutate(number_at_length = round(number_at_length)) %>% 
  uncount(number_at_length)

# cod hist, all years
hist(cod$length_cm, main="Histogram of Cod Lengths (cm), 1970-2023", xlab="Length (cm)")
percentile_90 <- quantile(cod$length_cm, 0.9)
abline(v = percentile_90, col = 'red', lwd = 2, lty = 2)
text(percentile_90, par("usr")[4]*0.9, labels = paste('90th percentile:', round(percentile_90, 2)), pos = 4, col = 'red')

# cod hist, 2010s and 2020s
cod_10s <- cod %>%
  mutate(decade = case_when(year < 1980 ~ "1970s",
                            year >= 1980 & year < 1990 ~ "1980s",
                            year >= 1990 & year < 2000 ~ "1990s",
                            year >= 2000 & year < 2010 ~ "2000s",
                            year >= 2010 ~ "2010s & 2020s")) %>%
  filter(decade == "2010s & 2020s")

hist(cod_10s$length_cm, main="Histogram of Cod Lengths (cm), 2010-2023", xlab="Length (cm)")
percentile_90 <- quantile(cod_10s$length_cm, 0.9)
abline(v = percentile_90, col = 'red', lwd = 2, lty = 2)
text(percentile_90, par("usr")[4]*0.9, labels = paste('90th percentile:', round(percentile_90, 2)), pos = 4, col = 'red')

# total cod
cod_tot <- cod %>%
  mutate(decade = case_when(year < 1980 ~ "1970s",
                            year >= 1980 & year < 1990 ~ "1980s",
                            year >= 1990 & year < 2000 ~ "1990s",
                            year >= 2000 & year < 2010 ~ "2000s",
                            year >= 2010 ~ "2010s & 2020s")) %>%
  group_by(decade) %>%
  summarize(total_count = n()) %>%
  ungroup()

# 90th percentile cod
cod_90th <- cod %>%
  filter(length_cm > quantile(cod$length_cm, probs = c(0.9))) %>%
  mutate(decade = case_when(
    year < 1980 ~ "1970s",
    year >= 1980 & year < 1990 ~ "1980s",
    year >= 1990 & year < 2000 ~ "1990s",
    year >= 2000 & year < 2010 ~ "2000s",
    year >= 2010 ~ "2010s & 2020s"
  )) %>%
  group_by(decade) %>%
  summarize(ninetieth_count = n()) %>%
  ungroup()

ggplot(cod_90th, aes(x = decade, y = ninetieth_count)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Atlantic Cod in the 90th Percentile Found in Each Decade",
       x = "Decade",
       y = "Count") +
  theme_bw() +
  theme(plot.title = element_text(size = 16))

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
                       total_percentage = "Total Percentage",
                       ninetieth_percentage = "90th Percentile Percentage"))

ggplot(cod_plot, aes(x = decade, y = percentage, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Percentage of Cod in the 90th Percentile and Total Found in Each Decade",
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
  cod_list <- c(cod_list, (cod_90th[i, 2] / cod_tot[i, 2])[1, 1])
}
cod_df <- as.data.frame(cod_list, stringsAsFactors = TRUE)
cod_df <- data.frame(cod_list, Decade = c("1970s", "1980s", "1990s", "2000s", "2010s and 2020s"))
cod_long <- pivot_longer(cod_df, cols = "cod_list", names_to = "Metric", values_to = "value")
ggplot(cod_long, aes(x = Decade, y = value, group = 1)) +
  geom_line() +
  geom_point() +
  labs(title = "Atlantic Cod: Number in 90th Percentile of Whole Dataset / Total Caught in each Decade", 
       x = "Decade", y = "Value") +
  theme_bw()

# -----------------------------------------------------------------------------
# American Plaice
# -----------------------------------------------------------------------------
plaice <- length_data %>%
  filter(scientific_name == "hippoglossoides platessoides")

plaice %>% 
  group_by(longitude, latitude, trawl_id, season, year, scientific_name, survey) %>% 
  mutate(number_at_length = round(number_at_length)) %>% 
  uncount(number_at_length)

plaice_90th <- plaice %>%
  filter(length_cm > quantile(plaice$length_cm, probs=c(0.9))) %>%
  mutate(decade = case_when(year < 1980 ~ "1970s",
                            year >= 1980 & year < 1990 ~ "1980s",
                            year >= 1990 & year < 2000 ~ "1990s",
                            year >= 2000 & year < 2010 ~ "2000s",
                            year >= 2010 ~ "2010s & 2020s")) %>%
  group_by(decade) %>%
  summarize(ninetieth_count = n()) %>%
  ungroup()

ggplot(plaice_90th, aes(x = decade, y = ninetieth_count)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of American Plaice in the 90th Percentile Found in Each Decade",
       x = "Decade",
       y = "Count") +
  theme_bw()

# total plaice
plaice_tot <- plaice %>%
  mutate(decade = case_when(year < 1980 ~ "1970s",
                            year >= 1980 & year < 1990 ~ "1980s",
                            year >= 1990 & year < 2000 ~ "1990s",
                            year >= 2000 & year < 2010 ~ "2000s",
                            year >= 2010 ~ "2010s & 2020s")) %>%
  group_by(decade) %>%
  summarize(total_count = n()) %>%
  ungroup()

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
                       total_percentage = "Total Percentage",
                       ninetieth_percentage = "90th Percentile Percentage"))

ggplot(plaice_plot, aes(x = decade, y = percentage, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Percentage of American Plaice in the 90th Percentile and Total Found in Each Decade",
       x = "Decade",
       y = "Percentage",
       fill = "Type") +
  theme_bw()+
  theme(legend.box.background = element_rect(color="black"),
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 14)) +
  scale_color_manual(name = "Type",  values = c("firebrick4", "steelblue2"),
                     aesthetics = "fill") 

# American Plaice List - Standardized
plaice_list <- c()
for (i in 1:5) {
  plaice_list <- c(plaice_list, (plaice_90th[i, 2] / plaice_tot[i, 2])[1, 1])
}
plaice_df <- as.data.frame(plaice_list, stringsAsFactors = TRUE)
plaice_df <- data.frame(plaice_list, Decade = c("1970s", "1980s", "1990s", "2000s", "2010s and 2020s"))
plaice_long <- pivot_longer(plaice_df, cols = "plaice_list", names_to = "Metric", values_to = "value")
ggplot(plaice_long, aes(x = Decade, y = value, group = 1)) +
  geom_line() +
  geom_point() +
  labs(title = "American Plaice: Number in 90th Percentile of Whole Dataset / Total Caught in each Decade", 
       x = "Decade", y = "Value") +
  theme_bw()

# -----------------------------------------------------------------------------
# Atlantic Herring
# -----------------------------------------------------------------------------
herring <- length_data %>%
  filter(scientific_name == "clupea harengus")

herring %>% 
  group_by(longitude, latitude, trawl_id, season, year, scientific_name, survey) %>% 
  mutate(number_at_length = round(number_at_length)) %>% 
  uncount(number_at_length)

herring_90th <- herring %>%
  filter(length_cm > quantile(herring$length_cm, probs=c(0.9))) %>%
  mutate(decade = case_when(year < 1980 ~ "1970s",
                            year >= 1980 & year < 1990 ~ "1980s",
                            year >= 1990 & year < 2000 ~ "1990s",
                            year >= 2000 & year < 2010 ~ "2000s",
                            year >= 2010 ~ "2010s & 2020s")) %>%
  group_by(decade) %>%
  summarize(ninetieth_count = n()) %>%
  ungroup()

ggplot(herring_90th, aes(x = decade, y = ninetieth_count)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Atlantic Herring in the 90th Percentile Found in Each Decade",
       x = "Decade",
       y = "Count") +
  theme_bw()

# total herring
herring_tot <- herring %>%
  mutate(decade = case_when(year < 1980 ~ "1970s",
                            year >= 1980 & year < 1990 ~ "1980s",
                            year >= 1990 & year < 2000 ~ "1990s",
                            year >= 2000 & year < 2010 ~ "2000s",
                            year >= 2010 ~ "2010s & 2020s")) %>%
  group_by(decade) %>%
  summarize(total_count = n()) %>%
  ungroup()

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
                       total_percentage = "Total Percentage",
                       ninetieth_percentage = "90th Percentile Percentage"))

ggplot(herring_plot, aes(x = decade, y = percentage, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Percentage of Atlantic Herring in the 90th Percentile and Total Found in Each Decade",
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

# Atlantic Herring List - Standardized
herring_list <- c()
for (i in 1:5) {
  herring_list <- c(herring_list, (herring_90th[i, 2] / herring_tot[i, 2])[1, 1])
}
herring_df <- as.data.frame(herring_list, stringsAsFactors = TRUE)
herring_df <- data.frame(herring_list, Decade = c("1970s", "1980s", "1990s", "2000s", "2010s and 2020s"))
herring_long <- pivot_longer(herring_df, cols = "herring_list", names_to = "Metric", values_to = "value")
ggplot(herring_long, aes(x = Decade, y = value, group = 1)) +
  geom_line() +
  geom_point() +
  labs(title = "Atlantic Herring: Number in 90th Percentile of Whole Dataset / Total Caught in each Decade", 
       x = "Decade", y = "Value") +
  theme_bw()

# -----------------------------------------------------------------------------
# Spiny Dogfish
# -----------------------------------------------------------------------------
dogfish <- length_data %>%
  filter(scientific_name == "squalus acanthias")

dogfish %>% 
  group_by(longitude, latitude, trawl_id, season, year, scientific_name, survey) %>% 
  mutate(number_at_length = round(number_at_length)) %>% 
  uncount(number_at_length)

dogfish_90th <- dogfish %>%
  filter(length_cm > quantile(dogfish$length_cm, probs=c(0.9))) %>%
  mutate(decade = case_when(year < 1980 ~ "1970s",
                            year >= 1980 & year < 1990 ~ "1980s",
                            year >= 1990 & year < 2000 ~ "1990s",
                            year >= 2000 & year < 2010 ~ "2000s",
                            year >= 2010 ~ "2010s & 2020s")) %>%
  group_by(decade) %>%
  summarize(ninetieth_count = n()) %>%
  ungroup()

ggplot(dogfish_90th, aes(x = decade, y = ninetieth_count)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Spiny Dogfish in the 90th Percentile Found in Each Decade",
       x = "Decade",
       y = "Count") +
  theme_bw()

# total dogfish
dogfish_tot <- dogfish %>%
  mutate(decade = case_when(year < 1980 ~ "1970s",
                            year >= 1980 & year < 1990 ~ "1980s",
                            year >= 1990 & year < 2000 ~ "1990s",
                            year >= 2000 & year < 2010 ~ "2000s",
                            year >= 2010 ~ "2010s & 2020s")) %>%
  group_by(decade) %>%
  summarize(total_count = n()) %>%
  ungroup()

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
                       total_percentage = "Total Percentage",
                       ninetieth_percentage = "90th Percentile Percentage"))

ggplot(dogfish_plot, aes(x = decade, y = percentage, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Percentage of Spiny Dogfish in the 90th Percentile and Total Found in Each Decade",
       x = "Decade",
       y = "Percentage",
       fill = "Type") +
  theme_bw()+
  theme(legend.box.background = element_rect(color="black"),
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 14)) +
  scale_color_manual(name = "Type",  values = c("firebrick4", "steelblue2"),
                     aesthetics = "fill") 

# Spiny Dogfish List - Standardized
dogfish_list <- c()
for (i in 1:5) {
  dogfish_list <- c(dogfish_list, (dogfish_90th[i, 2] / dogfish_tot[i, 2])[1, 1])
}
dogfish_df <- as.data.frame(dogfish_list, stringsAsFactors = TRUE)
dogfish_df <- data.frame(dogfish_list, Decade = c("1970s", "1980s", "1990s", "2000s", "2010s and 2020s"))
dogfish_long <- pivot_longer(dogfish_df, cols = "dogfish_list", names_to = "Metric", values_to = "value")
ggplot(dogfish_long, aes(x = Decade, y = value, group = 1)) +
  geom_line() +
  geom_point() +
  labs(title = "Spiny Dogfish: Number in 90th Percentile of Whole Dataset / Total Caught in each Decade", 
       x = "Decade", y = "Value") +
  theme_bw()
