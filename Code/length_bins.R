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

# total cod
cod_tot <- cod %>%
  mutate(decade = case_when(
    year < 1980 ~ "1970s",
    year >= 1980 & year < 1990 ~ "1980s",
    year >= 1990 & year < 2000 ~ "1990s",
    year >= 2000 & year < 2010 ~ "2000s",
    year >= 2010 ~ "2010s & 2020s"
  )) %>%
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
  labs(title = "Number of Atlantic Cod in the 90th Percentile Found in Each Period",
       x = "Decade",
       y = "Count") +
  theme_bw()

# merge data
cod_combined <- left_join(cod_tot, cod_90th, by = "decade")

# calculate in terms of %
cod_combined <- cod_combined %>%
  mutate(total_percentage = (total_count / sum(total_count)) * 100,
         ninetieth_percentage = (ninetieth_count / sum(ninetieth_count)) * 100)

cod_plot <- cod_combined %>%
  select(decade, total_percentage, ninetieth_percentage) %>%
  pivot_longer(cols = c(total_percentage, ninetieth_percentage), 
               names_to = "type", 
               values_to = "percentage") %>%
  mutate(type = recode(type,
                       total_percentage = "Total Percentage",
                       ninetieth_percentage = "90th Percentile Percentage"))

ggplot(cod_plot, aes(x = decade, y = percentage, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Percentage of Cod in the 90th Percentile and Total Found in Each Period",
       x = "Decade",
       y = "Percentage",
       fill = "Type") +
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
  labs(title = "Number of American Plaice in the 90th Percentile Found in Each Period",
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
  select(decade, total_percentage, ninetieth_percentage) %>%
  pivot_longer(cols = c(total_percentage, ninetieth_percentage), 
               names_to = "type", 
               values_to = "percentage") %>%
  mutate(type = recode(type,
                       total_percentage = "Total Percentage",
                       ninetieth_percentage = "90th Percentile Percentage"))

ggplot(plaice_plot, aes(x = decade, y = percentage, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Percentage of American Plaice in the 90th Percentile and Total Found in Each Period",
       x = "Decade",
       y = "Percentage",
       fill = "Type") +
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
  labs(title = "Number of Atlantic Herring in the 90th Percentile Found in Each Period",
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
  select(decade, total_percentage, ninetieth_percentage) %>%
  pivot_longer(cols = c(total_percentage, ninetieth_percentage), 
               names_to = "type", 
               values_to = "percentage") %>%
  mutate(type = recode(type,
                       total_percentage = "Total Percentage",
                       ninetieth_percentage = "90th Percentile Percentage"))

ggplot(herring_plot, aes(x = decade, y = percentage, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Percentage of Atlantic Herring in the 90th Percentile and Total Found in Each Period",
       x = "Decade",
       y = "Percentage",
       fill = "Type") +
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
  labs(title = "Number of Spiny Dogfish in the 90th Percentile Found in Each Period",
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
  select(decade, total_percentage, ninetieth_percentage) %>%
  pivot_longer(cols = c(total_percentage, ninetieth_percentage), 
               names_to = "type", 
               values_to = "percentage") %>%
  mutate(type = recode(type,
                       total_percentage = "Total Percentage",
                       ninetieth_percentage = "90th Percentile Percentage"))

ggplot(dogfish_plot, aes(x = decade, y = percentage, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Percentage of Spiny Dogfish in the 90th Percentile and Total Found in Each Period",
       x = "Decade",
       y = "Percentage",
       fill = "Type") +
  theme_bw()
