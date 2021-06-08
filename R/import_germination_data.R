library(readxl)
library(tidyverse)
#install.packages ("tidyverse")


#Installing germination data. It is in an excel sheet with 8 different tabs, one for each round of surveys, so we are importing them separately.

#LML tried to modify code so it would work on my computer, but no such luck
data1 <- read_excel("data/germination_data.xlsx", sheet = 2, col_types = "text") %>%
#data1 <- read_excel("G:/My Drive/Grad School/Projects/active/spring2020_seed_dormancy/data/germination_data.xlsx", sheet = 2, col_types = "text") %>% 
  pivot_longer(cols = c(6:(ncol(.)-1)), names_to = "date", values_to = "n_germ") %>% 
  mutate(date = as.Date(as.numeric(date), origin = "1899-12-30"),
         start_date = as.Date(as.numeric(start_date), origin = "1899-12-30")) %>% 
  filter(!is.na(date))

data2 <- read_excel("data/germination_data.xlsx", sheet = 3, col_types = "text") %>% 
  pivot_longer(cols = c(6:(ncol(.)-1)), names_to = "date", values_to = "n_germ") %>% 
  mutate(date = as.Date(as.numeric(date), origin = "1899-12-30"),
         start_date = as.Date(as.numeric(start_date), origin = "1899-12-30")) %>% 
  filter(!is.na(date)) %>% 
  rename("NOTES" = "entry_checked_date")

data3 <- read_excel("data/germination_data.xlsx", sheet = 4, col_types = "text") %>% 
  pivot_longer(cols = c(6:(ncol(.)-1)), names_to = "date", values_to = "n_germ") %>% 
  mutate(date = as.Date(as.numeric(date), origin = "1899-12-30"),
         start_date = as.Date(as.numeric(start_date), origin = "1899-12-30")) %>% 
  filter(!is.na(date))

data4 <- read_excel("data/germination_data.xlsx", sheet = 5, col_types = "text") %>% 
  pivot_longer(cols = c(6:(ncol(.)-1)), names_to = "date", values_to = "n_germ") %>% 
  mutate(date = as.Date(as.numeric(date), origin = "1899-12-30"),
         start_date = as.Date(as.numeric(start_date), origin = "1899-12-30")) %>% 
  filter(!is.na(date))

data5 <- read_excel("data/germination_data.xlsx", sheet = 6, col_types = "text") %>% 
  pivot_longer(cols = c(6:(ncol(.)-1)), names_to = "date", values_to = "n_germ") %>% 
  mutate(date = as.Date(as.numeric(date), origin = "1899-12-30"),
         start_date = as.Date(as.numeric(start_date), origin = "1899-12-30")) %>% 
  filter(!is.na(date))

data6 <- read_excel("data/germination_data.xlsx", sheet = 7, col_types = "text") %>% 
  pivot_longer(cols = c(6:(ncol(.)-1)), names_to = "date", values_to = "n_germ") %>% 
  mutate(date = as.Date(as.numeric(date), origin = "1899-12-30"),
         start_date = as.Date(as.numeric(start_date), origin = "1899-12-30")) %>% 
  filter(!is.na(date))

data7 <- read_excel("data/germination_data.xlsx", sheet = 8, col_types = "text") %>% 
  pivot_longer(cols = c(6:(ncol(.)-1)), names_to = "date", values_to = "n_germ") %>% 
  mutate(date = as.Date(as.numeric(date), origin = "1899-12-30"),
         start_date = as.Date(as.numeric(start_date), origin = "1899-12-30")) %>% 
  filter(!is.na(date))

data8 <- read_excel("data/germination_data.xlsx", sheet = 9, col_types = "text") %>% 
  pivot_longer(cols = c(6:(ncol(.)-1)), names_to = "date", values_to = "n_germ") %>% 
  mutate(date = as.Date(as.numeric(date), origin = "1899-12-30"),
         start_date = as.Date(as.numeric(start_date), origin = "1899-12-30")) %>% 
  filter(!is.na(date))

comp_data <- bind_rows(data1, data2, data3, data4, data5, data6, data7, data8)

write.csv(comp_data, file = "working/compiled_data.csv", row.names = F)
