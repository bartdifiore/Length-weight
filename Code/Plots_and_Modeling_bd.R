lw <- readRDS('Data/bottomTrawlSurvey_indLengthWeight.rds')
# You have to add the file path to where you saved this file before (e.g. "Loras_computer/Desktop/bottomTrawlSurvey_indLengthWeight.rds)

#This is just an example of how you could filter
lw %>%
  rename_all(tolower) %>% # this just makes stuff lowercase cause I'm lazy
  filter(sciname == "GADUS MORHUA")%>% # filter by scientific name
  filter(season %in% c("SPRING", "FALL")) %>% # The only consistent data is collected in spring and fall
  filter(year > 2000 & year < 2010) %>% # this is how you could filter by different time periods
  ggplot(aes(x = length, y = indwt))+ # this makes a plot
  geom_point(aes(color = season))+ # adds color based on season
  scale_x_log10()+
  scale_y_log10()


sub <- lw %>% # Here I've pulled out cod, and assigned a new variable "decade" to different time periods
  rename_all(tolower) %>%
  filter(sciname == "GADUS MORHUA", season %in% c("SPRING", "FALL"))%>%
  mutate(decade = case_when(year >= 2010 ~ "late",
                            year >= 2000 & year < 2010 ~ "mid", 
                            year < 2000 ~ "early")) %>%
  filter(indwt != 0)


lm1 <- lm(log(indwt) ~ log(length)*decade, sub) # This is the actual statistical model
summary(lm1)

# This is just a fast way to get a plot... I wouldn't recommend using this unless you are really comfortable generating your own predictions from a model.
plot(ggeffects::ggpredict(lm1, terms = ~ length*decade))+
  scale_x_log10()+
  scale_y_log10()

# Here is an example of how you could generate your own predictions for the model

new = expand.grid(length = seq(min(sub$length), max(sub$length), length.out = 100), decade = c("early", "late", "mid")) # Generate some data to use in the prediction. 

new$predicted <- exp(predict(lm1, newdata = new, type = "response")) # You have to exponentiate the predictions in order to estimate on the correct scale

new$lower <- exp(predict(lm1, newdata = new, type = "response", interval = 
                           "confidence")[,3]) # this gets you the upper confidence interval

new$upper <- exp(predict(lm1, newdata = new, type = "response", interval = 
                           "confidence")[,2]) # this gets you the lower confidence interval

ggplot(new, aes(x = length, y = predicted))+
  geom_line(aes(color = decade))+
  geom_ribbon(aes(group = decade, ymax = upper, ymin = lower), alpha = 0.1)+ #The confidence intervals are so tight you can't even see them, which isn't uncommon for linear fits and it also because we aren't dealing with the log-normal variance correctly
  scale_x_log10()+
  scale_y_log10()+ # You can see there there isn't a huge difference in these lines. 
  theme_classic()
