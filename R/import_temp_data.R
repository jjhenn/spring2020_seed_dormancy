library(tidyverse)
library(lubridate)

treats <- read.csv("data/snowTreatment_ibuttons_seedDormancy.csv", sep = ";")

temps1 <- list.files(path="./data/iButton_data/", 
                                pattern="*.csv", 
                                full.names = T) %>% 
  map_dfr(~read.csv(., header = T, colClasses = "character")) %>% 
  mutate(Time = ymd_hms(Time)) %>% 
  mutate(TempC = as.numeric(TempC)) %>% 
  mutate(ID = as.integer(ID)) %>% 
  filter(ID != 122)

temps2 <- list.files(path="./data/iButton_data/", 
                     pattern="*.csv", 
                     full.names = T) %>% 
  map_dfr(~read.csv(., header = T, colClasses = "character")) %>% 
  mutate(Time = mdy_hms(Time)) %>% 
  mutate(TempC = as.numeric(TempC)) %>% 
  mutate(ID = as.integer(ID)) %>% 
  filter(ID == 122) %>% 
  mutate(year1 = ifelse(month(Time) == 12, year(Time) <- 2019, year(Time) <- 2020)) 

year(temps2$Time) <- temps2$year1

temps2 <- temps2 %>% 
  select(-year1)


temps <- bind_rows(temps1, temps2) %>% 
  left_join(treats) %>% 
  mutate(site = factor(site, levels = c("Green Bay", "Madison", "Illinois")))

write.csv(temps, file = "working/temps.csv", row.names = F)


temps %>% filter(treatment != "air") %>% filter(month(Time) < 4 | month(Time) > 10) %>% 
  ggplot(aes(x = Time, y = TempC, group = ID, color = treatment)) +
  geom_line() +
  facet_wrap(~site, ncol = 1) +
  theme(text = element_text(size = 14)) +
  ylim(c(-10, 15))
