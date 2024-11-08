# -----------------------------------------------------------------------------
# Sensitivity Analysis Tornado Plot - SSA
# -----------------------------------------------------------------------------
library(tidyverse)

data2 <- data.frame(ssa_min = c(1482.944, 7.06E-51, 9.89E-73, 35101.45, 18.47595),
                    ssa_max = c(123738.5, 92610.59, 92610.59, 123738.5, 123738.5234),
                    parameter = c("Condition-Dependent Fecundity", "Condition-Dependent Survival", "Condition Two-way", 
                    "Fecundity", "Recruitment"))
data2$difference <- data2$ssa_max - data2$ssa_min

# sort difference, greatest to least
data2 <- data2[order(data2$difference, decreasing = TRUE),]

# difference plot
ggplot(data2, aes(x = reorder(parameter, difference), y = difference)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Difference Plot - SSA", x = "Parameter", 
       y = "Difference between Maximum and Minimum SSA")

# middle value of plot - 0 doesn't show anything
middle <- 50000

# order parameters in descending order, highest to lowest difference
order.parameters <- data2 %>% 
  arrange(difference) %>%
  mutate(parameter = factor(parameter, levels = parameter)) %>%
  dplyr::select(parameter) %>% 
  unlist() %>% 
  levels()
data2
# Put in long format for plotting
df2_long <- data2 %>% 
  gather(key = 'Type', value = 'ssa', ssa_min:ssa_max) %>%
  dplyr::select(parameter, Type, ssa, difference) %>%
  mutate(parameter = factor(parameter, levels = order.parameters),
         ymin = pmin(ssa, middle),
         ymax = pmax(ssa, middle),
         xmin = as.numeric(parameter) - 0.95 / 2,
         xmax = as.numeric(parameter) + 0.95 / 2)

# tornado plot
ggplot()+
  geom_rect(data = df2_long, 
          aes(ymax = ymax, ymin = ymin, xmax = xmax, xmin = xmin, fill = Type)) +
  theme_bw() +
  theme(axis.title.y = element_blank(), 
        axis.title.x = element_text(size = 16),         
        legend.title = element_blank(),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = 16),
        axis.text.y = element_text(size = 14),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
        legend.text = element_text(size = 16)) + 
  geom_hline(yintercept = middle, linetype = "dashed", color = "red") +
  scale_x_continuous(breaks = 1:length(order.parameters), 
                     labels = order.parameters) +
  coord_flip() +
  labs(title = "Population Model Sensitivity Analysis: Spawning Stock Abundance", 
       y = "Spawning Stock Abundance") +
  scale_fill_manual(values = c("ssa_min" = "firebrick4", "ssa_max" = "steelblue4"),
                    labels = c("Maximum SSA", "Minimum SSA")) -> SSASensPlot
SSASensPlot
ggsave(filename="SSASensPlot.png", plot=SSASensPlot, width=10, height=6)

