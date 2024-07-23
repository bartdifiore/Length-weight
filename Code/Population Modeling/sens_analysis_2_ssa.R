# -----------------------------------------------------------------------------
# Sensitivity Analysis Tornado Plot - SSA (From 100% to 90%)
# -----------------------------------------------------------------------------
library(tidyverse)

data2 <- data.frame(ssa_0.9 = c(41762.03, 59502.03, 37273.02, 54180.15, 39971.56),
                    ssa_1 = c(123738.5, 92610.59, 92610.59, 123738.5, 123738.5234),
                    parameter = c("condition", "condsurv", "cond&condsurv", 
                                  "fecundity", "recruitment"))
data2$difference <- data2$ssa_1 - data2$ssa_0.9

# sort difference, greatest to least
data2 <- data2[order(data2$difference, decreasing = TRUE),]
data2

# difference plot
ggplot(data2, aes(x = reorder(parameter, difference), y = difference)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Difference Plot: 100% and 90% Sensitivity", x = "Parameter", 
       y = "Difference of SSA between 100% and 90% Sensitivity")

# middle value of plot - 0 doesn't show anything
middle <- 75000

# order parameters in descending order, highest to lowest difference
order.parameters <- data2 %>% 
  arrange(difference) %>%
  mutate(parameter = factor(parameter, levels = parameter)) %>%
  dplyr::select(parameter) %>% 
  unlist() %>% 
  levels()

# Put in long format for plotting
df2_long <- data2 %>% 
  gather(key = 'Type', value = 'ssa', ssa_1:ssa_0.9) %>%
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
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = 10),
        axis.text.y = element_text(size = 10)) + 
  geom_hline(yintercept = middle, linetype = "dashed", color = "red") +
  scale_x_continuous(breaks = 1:length(order.parameters), 
                     labels = order.parameters) +
  coord_flip() +
  labs(title = "Sensitivity Analysis from 100% to 90% Tornado Plot: SSA", 
       y = "Spawning Stock Abundance") +
  scale_fill_manual(values = c("ssa_1" = "firebrick4", "ssa_0.9" = "steelblue4"),
                    labels = c("90% Sensitivity", "100% Sensitivity"))

