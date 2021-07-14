#Seed dormancy, winter 2019/2020 field experiment in Madison (Biocore Prairie), Charleston IL and Green Bay WI (UWGB)
# Note: Week 1-4 were grown in the growth chamber on campus, Week 5-end were grown in Laura's livingroom due to the pandemic. Unavoidable differences in methods might influence results

# to do: 
#Compare germaintion percentages in snow removal and control plots
# Germination percentages through time at each site

library(tidyverse)
library(TDPanalysis)
library(ggplot2)
library(gridExtra)

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
  rename(site = site.x) %>%
  mutate(perc_germ = (sum_germ/50)*100) # making percent germiantion variable for graphing

germ_to_sum_mean <- germ_to_sum %>%
  group_by(site, round, treatment) %>% #grouping to then calculate mean values
  summarize (sum_germ = mean(sum_germ), perc_germ = mean(perc_germ)) 

## ~*~*~*~*~*~*~*~*~*~*~*~*~
## ~*~*~* BASIC GRAPHS ~*~*~
## ~*~*~*~*~*~*~*~*~*~*~*~*~

## Total Germination
# panel of three graphs, one for each site, line graph, x = round, y = sum_germ, one line for trt
neworder <- c("GB", "MA", "IL") #ordering sites from north to south

tots <- ggplot(data = germ_to_sum,  
               aes(x = round, y = sum_germ, group = treatment, color = treatment)) +
  #geom_line() +
  geom_point () +
  geom_point (data=germ_to_sum_mean,size = 4) + #adding means
  theme_classic() +
  #ylab ("Germaination count")
  xlab("Collection Round") +
  facet_wrap(vars(site = factor(site, levels = neworder)), nrow =3)
tots

pdf("working/Germ_Totals.pdf", width = 6, height = 10)
grid.arrange(tots)
#grid.arrange(spring_summer, flwr_month, ncol = 2, widths = c(1, 2.5))
dev.off()


# total germination percentages
tots_perc <- ggplot(data = germ_to_sum,  
               aes(x = round, y = perc_germ, group = treatment, color = treatment)) +
  #geom_line() +
  geom_point () +
  geom_point (data=germ_to_sum_mean,size = 4) + #adding means
  theme_classic() +
  #ylab ("Germaination count")
  xlab("Collection Round") +
  facet_wrap(vars(site = factor(site, levels = neworder)), nrow =3)
tots_perc

pdf("working/Germ_Totals_Percentages.pdf", width = 6, height = 10)
grid.arrange(tots_perc)
dev.off()



## First germination

germ_to$date2 <- gsub("-", "/", germ_to$date, fixed = TRUE)
germ_to$doy <- date.to.DOY(germ_to$date2, format = "yyyy/mm/dd") #made a doy column

germ_timing <- germ_to %>%
  drop_na() %>% #dropping missing data 
  group_by(site, round, rep) %>%
  mutate (min_doy =min(doy)) %>% #first day of planting/observations
  filter (n_germ >0) %>% #filtering out zero germ days so I can find first day of germiation
  mutate (min_germ_doy = min(doy)) %>%
  mutate(days_to_germ = min_germ_doy - min_doy) %>%
  select (-c(date, date2, doy, siterep, n_germ))

germ_timing <- germ_timing[!duplicated(germ_timing), ] #removing duplicate rows


germ_timing_mean <- germ_timing %>%
  group_by(site, round, treatment) %>% #grouping to then calculate mean values
  summarize (mean_time = mean(days_to_germ))
germ_timing_mean$days_to_germ <- germ_timing_mean$mean_time
  
gtime <- ggplot(data = germ_timing,  
               aes(x = round, y = days_to_germ, group = treatment, color = treatment)) +
  #geom_line() +
  geom_point () +
  geom_point (data=germ_timing_mean, size = 4) + #adding means but code not working
  #geom_jitter() +
  theme_classic() +
  #ylab ("Days til first germination") +
  xlab("Collection Round") +
  facet_wrap(vars(site = factor(site, levels = neworder)), nrow =3)
gtime

pdf("working/First_germ.pdf", width = 6, height = 10)
grid.arrange(gtime)
dev.off()

