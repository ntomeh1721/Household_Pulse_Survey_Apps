
#### Note: First run the app to load the vis_functions functions
library(tidyverse)
library(readxl)
library(WriteXLS)

a <- fi_data("All Households") %>% 
  mutate(
    week = case_when(
      week < 13 ~ week, 
      week %in% 18:34 ~ (week/2) + 4, 
      week %in% 38:48 ~ (week/2) + 3, 
      week > 50 ~ (week/2) + 2
    )) %>% 
  mutate(num_hungry =  often_not_enough + sometimes_not_enough,
         total = enough_not_wanted + enough_wanted + often_not_enough + sometimes_not_enough,
         perc_hungry = 100*num_hungry/total,
         perc_insecure = 100*num_insecure/total) %>% 
  select(week, state, race, num_hungry, perc_hungry, num_insecure, perc_insecure, total) %>% 
  filter(state == "USA") %>% 
  ungroup() %>% 
  select(-wave, - state) %>% 
  arrange(week, race) 

b <- fi_data("Respondents With Children") %>% 
  mutate(
    week = case_when(
      week < 13 ~ week, 
      week %in% 18:34 ~ (week/2) + 4, 
      week %in% 38:48 ~ (week/2) + 3, 
      week > 50 ~ (week/2) + 2
    )) %>% 
  mutate(num_hungry =  often_not_enough + sometimes_not_enough,
         total = enough_not_wanted + enough_wanted + often_not_enough + sometimes_not_enough,
         perc_hungry = 100*num_hungry/total,
         perc_insecure = 100*num_insecure/total) %>% 
  select(week, state, race, num_hungry, perc_hungry, num_insecure, perc_insecure, total) %>% 
  filter(state == "USA") %>% 
  ungroup() %>% 
  select(-wave, - state) %>% 
  arrange(week, race) 

c <- fi_data("All Households") %>% 
  mutate(
    week = case_when(
      week < 13 ~ week, 
      week %in% 18:34 ~ (week/2) + 4, 
      week %in% 38:48 ~ (week/2) + 3, 
      week > 50 ~ (week/2) + 2
    )) %>% 
  mutate(num_hungry =  often_not_enough + sometimes_not_enough,
         total = enough_not_wanted + enough_wanted + often_not_enough + sometimes_not_enough,
         perc_hungry = 100*num_hungry/total,
         perc_insecure = 100*num_insecure/total) %>% 
  select(week, state, race, num_hungry, perc_hungry, num_insecure, perc_insecure, total) %>% 
  filter(state == "Michigan") %>% 
  ungroup() %>% 
  select(-wave, - state) %>% 
  arrange(week, race) 


d <- fi_data("Respondents With Children") %>% 
  mutate(
    week = case_when(
      week < 13 ~ week, 
      week %in% 18:34 ~ (week/2) + 4, 
      week %in% 38:48 ~ (week/2) + 3, 
      week > 50 ~ (week/2) + 2
    )) %>% 
  mutate(num_hungry =  often_not_enough + sometimes_not_enough,
         total = enough_not_wanted + enough_wanted + often_not_enough + sometimes_not_enough,
         perc_hungry = 100*num_hungry/total,
         perc_insecure = 100*num_insecure/total) %>% 
  select(week, state, race, num_hungry, perc_hungry, num_insecure, perc_insecure, total) %>% 
  filter(state == "Michigan") %>% 
  ungroup() %>% 
  select(-wave, - state) %>% 
  arrange(week, race) 


WriteXLS(list(a, b, c, d), ExcelFileName = "Michigan Data.xls", 
         SheetNames = c("USA All Households", "USA With Children", "MI All Households", "MI With Children"))
         