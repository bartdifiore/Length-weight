# -----------------------------------------------------------------------------
# Sensitivity Analysis Tornado Plot - Total Population
# -----------------------------------------------------------------------------
library(tidyverse)

data2 <- data.frame(pop_min = c(4251.587, 2.735476e-49, 1.068914e-71, 217103.6, 43.54149),
                    pop_max = c(1625514, 1221029, 1221029, 1625514, 1625514),
                    parameter = c("condition", "condsurv", "cond&condsurv", 
                                  "fecundity", "recruitment"))
data2$difference <- data2$pop_max - data2$pop_min

# sort difference, greatest to least
data2 <- data2[order(data2$difference, decreasing = TRUE),]

# difference plot
ggplot(data2, aes(x = reorder(parameter, difference), y = difference)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Difference Plot: Maximum and Minimum Population", x = "Parameter", 
       y = "Difference between Maximum and Minimum Population")

# middle value of plot - 0 doesn't show anything
middle <- 500000

# order parameters in descending order, highest to lowest difference
order.parameters <- data2 %>% 
  arrange(difference) %>%
  mutate(parameter = factor(parameter, levels = parameter)) %>%
  dplyr::select(parameter) %>% 
  unlist() %>% 
  levels()

# Put in long format for plotting
df2_long <- data2 %>% 
  gather(key = 'Type', value = 'pop', pop_min:pop_max) %>%
  dplyr::select(parameter, Type, pop, difference) %>%
  mutate(parameter = factor(parameter, levels = order.parameters),
         ymin = pmin(pop, middle),
         ymax = pmax(pop, middle),
         xmin = as.numeric(parameter) - 0.95 / 2,
         xmax = as.numeric(parameter) + 0.95 / 2)

# tornado plot
ggplot()+
  geom_rect(data = df2_long, 
            aes(ymax = ymax, ymin = ymin, xmax = xmax, xmin = xmin, fill = Type)) +
  theme_bw() +
  theme(axis.title.y = element_blank(), 
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = 10),
        axis.text.y = element_text(size = 10)) + 
  geom_hline(yintercept = middle, linetype = "dashed", color = "red") +
  scale_x_continuous(breaks = 1:length(order.parameters), 
                     labels = order.parameters) +
  coord_flip() +
  labs(title = "Population Model Sensitivity Analysis Tornado Plot: Total Population", 
       y = "Total Population") +
  scale_fill_manual(values = c("pop_min" = "firebrick4", "pop_max" = "steelblue4"),
                    labels = c("Maximum Population", "Minimum Population"))
 