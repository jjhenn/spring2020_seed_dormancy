# to do: 
#Compare germaintion percentages in snow removal and control plots
# Germination percentages through time at each site

library(tidyverse)

germ<-read.csv("working/compiled_data.csv") #import germ data
treats <- read.csv("data/snowTreatment_ibuttons_seedDormancy.csv", sep = ";") #import trt data

#merge treatments onto germination data

treats <- treats %>% #making a common column for sie for merging with other datasets
  rename(site_long = site) %>%
  mutate(site = case_when(
    startsWith(site_long, "M") ~ "MA", 
    startsWith(site_long, "I") ~ "IL", 
    startsWith(site_long, "G") ~ "GB"))

treats$siterep <-paste(treats$site, "_", treats$plot) #creating a merging column
germ$siterep <-paste(germ$site, "_", germ$rep) #creating a merging column

germ <- left_join(germ, treats, by = "siterep") #adding trt to germination data

germ <- germ %>% 
  select (-c(start_date, NOTES, site_long, date_deploy, ID, notes, site.y, plot)) %>% #getting rid of extra columns
  rename(site = site.x)

germ_to <- germ %>% #making summary dataset with just Tradescantia
  filter(species == "TO") %>%
  group_by(site, round, rep)
  #summarize(tot_germ =sum(n_germ)) #couldn't get this code to work, so exported data, and summarized germaination data per run with a pivot table in excel

write.csv(germ_to, file = "working/TO_germ_daily.csv", row.names = F)
germ_to_sum <- read.csv("data/TO_germ_sum.csv") #reading in summary data from excel

germ_to_sum$siterep <-paste(germ_to_sum$site, "_", germ_to_sum$rep) #creating a merging column
germ_to_sum <- left_join(germ_to_sum, treats, by = "siterep") #adding trt to germination data

germ_to_sum <- germ_to_sum %>% 
  select (-c(site_long, date_deploy, ID, notes, site.y, plot)) %>% #getting rid of extra columns
  rename(site = site.x)

germ_to_sum_mean <- germ_to_sum %>%
  group_by(site, round, treatment) %>%
  summarize (sum_germ = mean(sum_germ))


## BASIC GRAPH
# panel of three graphs, one for each site, line graph, x = round, y = sum_germ, one line for trt
tots <- ggplot(data = germ_to_sum, aes(x = round, y = sum_germ, group = treatment, color = treatment)) +
  #geom_line() +
  geom_point () +
  geom_point (data=germ_to_sum_mean,size = 4) + #adding means
  theme_classic() +
  facet_wrap(vars(site), nrow =3)
tots






