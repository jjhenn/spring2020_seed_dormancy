# *~*~* Laura's Wish List *~*~*
# - count days with temp <0 to see at what date seeds met the 120 day cold strat requirement. Does that date relate to any patterns we see in total germaination or speed of germination?
# - Graph growing degree day accumulation to see how it differs between sites. Does that help account for the higher variation in germination from IL seeds? 

library(tidyverse)
library(lubridate)

temps <- read.csv("working/temps.csv") 

str(temps)

# This is the graph that Jon made in import_temp_data.R but I wanted it here so I could look at it when thinking more about the temperature data
temps %>% filter(treatment != "air") %>% filter(month(Time) < 3 | month(Time) > 8) %>% 
  ggplot(aes(x = Time, y = TempC, group = ID, color = treatment)) +
  geom_line() +
  facet_wrap(~site, ncol = 1) +
  theme(text = element_text(size = 14)) +
  ylim(c(-10, 15))

## Attempting to make some sort of daily summary, but not sure how to split Time in the third line below
temps_daily <- temps %>% 
  select (-c(Serial.number, date_deploy, notes)) %>% #getting rid of extra columns
  mutate() %/% #split Time at the space and make two columns, "date" and "time" 
  group_by(site, plot, date) %>%
  summarize(min_temp = min(TempC), max_temp = max(TempC))
             



