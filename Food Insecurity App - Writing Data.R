# Set Up 

# To Update:
## Update Latest HPS Week 
new_week <- 36 
## Updated Latest HPS Week Start Date 
new_week_start <- "8/18/21"

# Set Up ------------------------------------------------------------------

# Loading Libraries 
library(tidyverse) 
library(readxl)
library(janitor)

# P-EBT release Guide 
guide <- read_xlsx("data/guides/State Names w PEBT.xlsx") %>% clean_names()

# Food Insecurity Conversion Guide 
new_multipliers <- read_csv("data/guides/all_fi_multipliers.csv")

# Establishing Data to Work With
states <- c("USA", guide$states)

# Importing the old data
old_data <- read_csv("Food Insecurity App/data_prepped/Aggregate.csv")
second_wave <- read_csv("data/food_security_data.csv")


# Prepping Aggregate

all_states <- {} 

# Helper Function ---------------------------------------------------------


week_cleaning <- function(week_df){
  
  names(week_df) <- c("characteristic", "total", 
                      "enough_wanted", "enough_not_wanted", 
                      "sometimes_not_enough", "often_not_enough", 
                      "did_not_report")
  
  week_df <- week_df %>% 
    filter(!is.na(characteristic))
  
  week_df <- week_df %>% 
    mutate(index = 1:nrow(week_df))
  
  stats <- week_df %>% 
    filter(is.na(total)) %>% 
    select(characteristic, index) %>% 
    mutate(count = lead(index) - (index + 1), 
           count = ifelse(is.na(count), max(week_df$index) - index, count))  %>% 
    filter(!is.na(count))
  
  stats_vector <- rep(stats$characteristic, stats$count)
  
  week_df %>% 
    na.omit() %>% 
    mutate(category =  c("Total", stats_vector),
           total = total %>% as.numeric(),
           enough_wanted = enough_wanted %>% as.numeric(), 
           enough_not_wanted = enough_not_wanted %>% as.numeric(), 
           sometimes_not_enough = sometimes_not_enough %>% as.numeric(),
           often_not_enough = often_not_enough %>% as.numeric(), 
           did_not_report = ifelse(did_not_report == "-", 0, did_not_report %>% as.numeric())) %>% 
    select(category, names(week_df), -index)
  
  
}


# Running the Cleaning  ---------------------------------------------------


for(i in states){
  
  ## Getting State Abbreviation 
  sheet_name <- if(i == "USA") {
    "US" 
  } else { 
    guide %>% 
      filter(states == i) %>% 
      pluck("state_abbr") 
  } 
  
  ## P-EBT Release Weeks 
  p_ebt_week <- if(i == "USA") {
    1 
  } else { 
    guide %>% 
      filter(states == i) %>% 
      pluck("first_week")
  } 
  
  if(i == "USA"){
    
    
    a <- new_multipliers %>% 
      filter(region %in% c("Overall", "White", "Black", "Hispanic", "Asian")) %>% 
      arrange(region) %>% 
      cbind(race = c("Asian alone, not Hispanic",
                     "Black alone, not Hispanic", 
                     "Hispanic or Latino (may be of any race)",
                     "Two or more races + Other races, not Hispanic",
                     "White alone, not Hispanic")) 
    
    conversion_filtered <- rbind(a, a %>% 
                                   filter(region == "Overall") %>% 
                                   mutate(race = "Total")) %>% 
      select(-region)
    
    
  } else {
    
    ## Conversion numbers 
    conversion_filtered <- new_multipliers %>% 
      filter(region == ifelse(i == "USA", "USA", tolower(i)))
    
    # Overall Conversions 
    food1 <- conversion_filtered %>% 
      pluck("food1")
    
    food2 <- conversion_filtered %>% 
      pluck("food2")
    
    food3 <- conversion_filtered %>% 
      pluck("food3")
    
    food4 <- conversion_filtered %>% 
      pluck("food4")
    
    # With Kids Conversions
    food1_kid <- conversion_filtered %>% 
      pluck("food1_kid")
    
    food2_kid <- conversion_filtered %>% 
      pluck("food2_kid")
    
    food3_kid <- conversion_filtered %>% 
      pluck("food3_kid")
    
    food4_kid <- conversion_filtered %>% 
      pluck("food4_kid")
    
    
  }
  
  
  ## Prepping to get both no kids and all households 
  folders <- c("All Households", "With Children")
  
  for(f in folders){
    
    if(f == "With Children"){
      url_listed <- paste0(
        "https://www2.census.gov/programs-surveys/demo/tables/hhp/2021",
        "/wk", new_week, 
        "/food",
        case_when(
          new_week %in% 1:21 ~ "3b", 
          new_week %in% 22:33 ~ "3",
          new_week >= 34 ~ "2"
        ), 
        "_week",
        new_week, 
        ".xlsx")
      
    } else {
      
      url_listed <- paste0(
        "https://www2.census.gov/programs-surveys/demo/tables/hhp/2021",
        "/wk",
        new_week, 
        "/food",
        case_when(
          new_week %in% 1:21 ~ "2b", 
          new_week %in% 22:33 ~ "2",
          new_week >= 32 ~ "1"), 
        "_week",
        new_week, 
        ".xlsx")
      
    }
    
    download.file(url_listed, "data/table.xlsx")

    week <- read_excel("data/table.xlsx",
                       sheet = sheet_name,
                       range = "A5:G167")
    
    file.remove("data/table.xlsx")
    
    full_state <- week_cleaning(week) %>% 
      mutate(week_start = as.Date(new_week_start,
                                  format = "%m/%d/%y"),
             week = ifelse(
               old_data$week %>% unique() %>% length() == new_week,
               old_data$week %>% max(),
               old_data$week %>% max() + 2 # NOTE: Week Change
             )) 

    
    ## Editing Aggregate for Food Insecurity
    
    if(i == "USA"){
      
      full_state <- full_state %>% 
        left_join(conversion_filtered %>% rename("characteristic" = "race"))
      
    }
    
    if(f == "All Households"){
      
      all_households <- full_state %>% 
        mutate(enough_wanted = ifelse(is.na(enough_wanted), 0, enough_wanted),
               enough_not_wanted = ifelse(is.na(enough_not_wanted), 0, enough_not_wanted),
               sometimes_not_enough = ifelse(is.na(sometimes_not_enough), 0, sometimes_not_enough),
               often_not_enough = ifelse(is.na(often_not_enough), 0, often_not_enough),
               sum = enough_wanted + enough_not_wanted + sometimes_not_enough + often_not_enough, 
               converted_perc_insecure = 100*(food1*(enough_wanted/sum) + food2*(enough_not_wanted/sum) + 
                                                food3*(sometimes_not_enough/sum) + food4*(often_not_enough/sum)), 
               converted_num_insecure = sum*converted_perc_insecure/100, 
               post_pebt = ifelse(week >= p_ebt_week, 1, 0), 
               state = i) %>% 
        select(category, characteristic, total, sum, 
               converted_perc_insecure, converted_num_insecure,
               enough_wanted, enough_not_wanted, sometimes_not_enough, 
               often_not_enough, week_start, week, post_pebt, state)
      
      
      ## "ah" == "All Households"
      
      names(all_households) <- c("category", "characteristic", "ah_total", "ah_sum", 
                                 "ah_perc_insecure", "ah_num_insecure", "ah_enough_wanted", 
                                 "ah_enough_not_wanted", "ah_sometimes_not_enough", 
                                 "ah_often_not_enough", "week_start", "week", "post_pebt", "state")
      
      
    } else { 
      
      with_children <- full_state %>% 
        mutate(
          # Getting Rid of NAs 
          enough_wanted = ifelse(is.na(enough_wanted), 0, enough_wanted),
          enough_not_wanted = ifelse(is.na(enough_not_wanted), 0, enough_not_wanted),
          sometimes_not_enough = ifelse(is.na(sometimes_not_enough), 0, sometimes_not_enough),
          often_not_enough = ifelse(is.na(often_not_enough), 0, often_not_enough),
          
          # finding the total 
          sum = enough_wanted + enough_not_wanted + sometimes_not_enough + often_not_enough,
          #Conversion 
          converted_perc_insecure = 100*(food1_kid*(enough_wanted/sum) + 
                                           food2_kid*(enough_not_wanted/sum) + 
                                           food3_kid*(sometimes_not_enough/sum) + 
                                           food4_kid*(often_not_enough/sum)), 
          converted_num_insecure = sum*converted_perc_insecure/100, 
          post_pebt = ifelse(week >= p_ebt_week, 1, 0), 
          state = i) %>% 
        select(category, characteristic, total, sum, 
               converted_perc_insecure, converted_num_insecure,
               enough_wanted, enough_not_wanted, sometimes_not_enough, 
               often_not_enough, week_start, week, post_pebt, state)
      
      
      ## "wc" == "With Children" 
      
      names(with_children) <- c("category", "characteristic", "wc_total", "wc_sum", 
                                "wc_perc_insecure", "wc_num_insecure", "wc_enough_wanted", 
                                "wc_enough_not_wanted", "wc_sometimes_not_enough", 
                                "wc_often_not_enough", "week_start", "week", "post_pebt", "state")
      
      
    }
    
  }
  
  # Joining All Households and With Children
  analyzable <- merge(all_households, with_children, by = c("category", "characteristic", "week", "post_pebt", "state", "week_start")) %>% 
    ## "nc" = "no children"
    mutate(nc_total = ah_total - wc_total, 
           nc_sum = ah_sum - wc_sum, 
           nc_enough_wanted = ah_enough_wanted - wc_enough_wanted, 
           nc_enough_not_wanted = ah_enough_not_wanted - wc_enough_not_wanted, 
           nc_sometimes_not_enough = ah_sometimes_not_enough - wc_sometimes_not_enough, 
           nc_often_not_enough = ah_often_not_enough - wc_often_not_enough, 
           nc_perc_insecure = nc_sometimes_not_enough + nc_often_not_enough, ## laziness, ngl
           nc_num_insecure = nc_sum * nc_perc_insecure/100) 
  
  
  # Getting the Aggregate Table 
  all_states <- rbind(all_states, analyzable)
  
}


# Finalizing Data  ---------------------------------------------------

## Getting into a longer Data Frame 
## Note: does not inlude the specific question answers

# Totals Columns (Includes Did Not Respond)
total_df <- all_states %>% 
  select(ah_total, wc_total, nc_total,
         category, characteristic, week, post_pebt, state, week_start) %>% 
  pivot_longer(cols = 1:3, values_to = "total", names_to = "household_type") %>% 
  mutate(household_type = ifelse(substring(household_type, 1, 2) == "wc", 
                                 "with_children", 
                                 ifelse(substring(household_type, 1, 2) == "nc", 
                                        "no_children", 
                                        "all_households")
  ))

# Sums Columns (Does not include did not respond)
sum_df <- all_states %>% 
  select(ah_sum, wc_sum, nc_sum,
         category, characteristic, week, post_pebt, state, week_start) %>% 
  pivot_longer(cols = 1:3, values_to = "sum", names_to = "household_type") %>% 
  mutate(household_type = ifelse(substring(household_type, 1, 2) == "wc", 
                                 "with_children", 
                                 ifelse(substring(household_type, 1, 2) == "nc", 
                                        "no_children", 
                                        "all_households")
  ))

# Percent Insecure Column - converted insecurity 
perc_insecure_df <- all_states %>% 
  select(ah_perc_insecure, wc_perc_insecure, nc_perc_insecure,
         category, characteristic, week, post_pebt, state, week_start) %>% 
  pivot_longer(cols = 1:3, values_to = "perc_insecure", names_to = "household_type") %>% 
  mutate(household_type = ifelse(substring(household_type, 1, 2) == "wc", 
                                 "with_children", 
                                 ifelse(substring(household_type, 1, 2) == "nc", 
                                        "no_children", 
                                        "all_households")
  ))

# Number of insecure column 
num_insecure_df <- all_states %>% 
  select(ah_num_insecure, wc_num_insecure, nc_num_insecure,
         category, characteristic, week, post_pebt, state, week_start) %>% 
  pivot_longer(cols = 1:3, values_to = "num_insecure", names_to = "household_type") %>% 
  mutate(household_type = ifelse(substring(household_type, 1, 2) == "wc", 
                                 "with_children", 
                                 ifelse(substring(household_type, 1, 2) == "nc", 
                                        "no_children", 
                                        "all_households")
  ))

## Joining the Data Sets 

final <- merge(total_df, sum_df, by = c("category", "characteristic", "week", 
                                        "post_pebt", "state", "week_start", "household_type")) %>% 
  merge(perc_insecure_df, by = c("category", "characteristic", "week", 
                                 "post_pebt", "state", "week_start", "household_type")) %>% 
  merge(num_insecure_df, by = c("category", "characteristic", "week", 
                                "post_pebt", "state", "week_start", "household_type"))



# Editing Second Wave -----------------------------------------------------

second_wave <- second_wave %>% 
  mutate(category = "Hispanic origin and Race",
         week_start = case_when(
           week == 13 ~ as.Date("08/19/20", 
                                format = "%m/%d/%y"),
           week == 14 ~ as.Date("09/02/20", 
                                format = "%m/%d/%y"),
           week == 15 ~ as.Date("09/16/20", 
                                format = "%m/%d/%y"),
           week == 16 ~ as.Date("09/30/20", 
                                format = "%m/%d/%y"),
           week == 17 ~ as.Date("10/14/20", 
                                format = "%m/%d/%y"),
         ), 
         week = case_when(
           week_start == "2020-08-19" ~ 16, 
           week_start == "2020-09-02" ~ 18,
           week_start == "2020-09-16" ~ 20,
           week_start == "2020-09-30" ~ 22,
           week_start == "2020-10-14" ~ 24
         ), 
         characteristic = case_when(
           race == "Hispanic or Latino, all Races" ~ "Hispanic or Latino (may be of any race)", 
           race == "Two or more races or Other" ~ "Two or more races + Other races, not Hispanic", 
           TRUE ~ race), 
         post_pebt = 1
  ) %>% 
  select(-race)

second_wave <- second_wave %>% 
  group_by(category, characteristic, week, post_pebt, state, week_start, food_sufficiency) %>% 
  summarize(people = sum(people)) %>% 
  mutate(household_type = "all_households") %>% 
  rbind(
    second_wave %>% 
      filter(kids == "yes") %>% 
      mutate(household_type = "with_children") %>% 
      select(-kids)
  ) %>% 
  filter(!is.na(food_sufficiency))

wave <- second_wave %>% 
  pivot_wider(names_from = food_sufficiency, values_from = people) %>% 
  mutate(region = tolower(state), 
         `enough not wanted` = ifelse(is.na(`enough not wanted`), 0, `enough not wanted`), 
         `enough wanted` = ifelse(is.na(`enough wanted`), 0, `enough wanted`), 
         `often not enough` = ifelse(is.na(`often not enough`), 0, `often not enough`),
         `sometimes not enough` = ifelse(is.na(`sometimes not enough`), 0, `sometimes not enough`), 
         total = `enough not wanted` + `enough wanted` + `often not enough` + `sometimes not enough`,
         sum = total) %>% 
  rename(
    "enough_not_wanted" = "enough not wanted", 
    "enough_wanted" = "enough wanted",
    "often_not_enough" = "often not enough", 
    "sometimes_not_enough" = "sometimes not enough"
  ) %>% 
  left_join(new_multipliers)%>% 
  mutate(
    num_insecure = ifelse(
      household_type == "with_children", 
      food1_kid*enough_wanted + 
        food2_kid*enough_not_wanted + 
        food3_kid*sometimes_not_enough + 
        food4_kid*often_not_enough, 
      food1*enough_wanted + 
        food2*enough_not_wanted + 
        food3*sometimes_not_enough + 
        food4*often_not_enough), 
    perc_insecure = 100*num_insecure/total)

totals <- wave %>% 
  group_by(week, post_pebt, state, week_start, 
           household_type) %>% 
  summarize(category = "Total", 
            characteristic = "Total", 
            total = sum(total), 
            sum = sum(sum), 
            num_insecure = sum(num_insecure), 
            perc_insecure = 100*num_insecure/total)

wave_usa <- wave %>% 
  rbind(totals) %>% 
  group_by(category, characteristic, week, week_start, post_pebt, 
           household_type) %>% 
  summarize(state = "USA",
            total = sum(total),
            sum = sum(sum), 
            num_insecure = sum(num_insecure), 
            perc_insecure = 100*num_insecure/total)

final_waves <- wave %>% 
  select(-food1, -food2, -food3, -food4,
         -food1_kid, -food2_kid, -food3_kid, -food4_kid, 
         -enough_wanted, -often_not_enough, -sometimes_not_enough, 
         -enough_not_wanted, -region) %>% 
  rbind(final) %>% 
  rbind(totals) %>% 
  rbind(wave_usa) %>% 
  mutate(wave = case_when(
    week %in% 1:12 ~ "first", 
    week %in% 13:24 ~ "second",
    week > 25 ~ "third"
  ))

final_waves <- final_waves %>% 
  rbind(old_data) %>% 
  unique()

# Writing the Files  ------------------------------------------------------

write.csv(final_waves, "Food Insecurity App/data_prepped/Aggregate_new.csv", row.names = F)



