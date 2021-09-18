
# Visualization Functions for the Full Household Pulse App 
# This file contains the functions that are used in the app to create the graphics. 
# Almost all errors or issues that arise from this document will me in the 
# scale_x_continuous() function. Typically a mismatch between the breaks and labels 
# in the graph. 
# 
# To update this file with new weeks of data, add dates to the week_labels vector 
# below increasing by one week at a time up to the last date of the most recent week of HPS. 
# Unlike the Writing Data script, there's no need to run this script on its own. If you 
# try, you will get an error. You will know that more needs to be changed in this script 
# if you get an error when trying to run the app.R script. 

# Week Label
week_labels <- c("", "5/5", "5/12", "5/19", "5/26", 
                 "6/2", "6/9", "6/16", "6/23", 
                 "6/30", "7/7", "7/14", "7/21", 
                 "7/28", "8/4", "8/11", "8/18", 
                 "8/25", "8/31", "9/7", "9/14",
                 "9/21", "9/28", "10/5", "10/12", 
                 "10/19", "10/26", "11/2", "11/9", 
                 "11/16", "11/23", "11/30", "12/7", 
                 "12/14", "12/21", "12/28", "1/04",
                 "1/11", "1/18", "1/25", "2/1", 
                 "2/8", "2/15", "2/22", "3/1", 
                 "3/8", "3/15", "3/22", "3/29", 
                 "4/5", "4/12", "4/19", "4/26", 
                 "5/3", "5/10", "5/17", "5/24",
                 "5/31", "6/7", "6/14", "6/21", 
                 "6/28", "7/5", "7/12", "7/19", 
                 "7/26", "8/2", "8/9", "8/16")


# Set Up ------------------------------------------------------------------


# Loading Libraries 
library(tidyverse)
library(ggrepel)
library(plotly)

# Loading Data
data <- read_csv("data/food_insecurity.csv")
employment_data <- read.csv("data/employment.csv") 
housing_data <- read.csv("data/housing.csv") %>% 
  mutate(housing_threat = case_when(
    is.na(eviction) & !is.na(foreclosure) ~ foreclosure, 
    !is.na(eviction) & is.na(foreclosure) ~ eviction
  ))
children_data <- read.csv("data/children.csv") 
finances_data <- read.csv("data/finances.csv") 
mental_health_data <- read.csv("data/mental_health.csv") 

# Getting Food Insecurity Multipliers 
fi_multipliers <- read_csv("data/guides/all_fi_multipliers.csv") %>%
  mutate(race = case_when(
    region == "White" ~ "White alone, not Hispanic",
    region == "Black" ~  "Black alone, not Hispanic",
    region == "Hispanic" ~ "Hispanic or Latino, all Races",
    region == "Asian" ~ "Asian alone, not Hispanic",
    region == "Overall" ~ "Two or more races or Other",
    TRUE ~ "All"
  ),
  region = ifelse(region %in% c("Overall", "White", "Black", "Asian", "Hispanic"),
                 "USA", region)) 

fi_mult_med <- fi_multipliers %>% 
  filter(region != "USA") %>% 
  select(-race) 

fi_multipliers <- rbind(fi_mult_med %>% mutate(race = "White alone, not Hispanic"),
      fi_mult_med %>% mutate(race = "Black alone, not Hispanic")) %>% 
  rbind(fi_mult_med %>% mutate(race = "Hispanic or Latino, all Races")) %>% 
  rbind(fi_mult_med %>% mutate(race = "Asian alone, not Hispanic")) %>% 
  rbind(fi_mult_med %>% mutate(race = "Two or more races or Other")) %>% 
  rbind(fi_multipliers)


# fi_multipliers <- read_csv("data/guides/state-fi-multipliers.csv")

# Getting rid of scientific notation 
options(scipen = 999)


`%notin%` <- negate(`%in%`)

states <- data %>% 
  select(state) %>% 
  filter(state != "USA") %>% 
  unique() %>% 
  arrange(state) %>% 
  pluck("state")

races <- data %>% 
  select(race) %>% 
  unique() %>% 
  pluck("race")



# Food Insecurity Headline Functions -----------------------------------------------

fi_data <- function(input_kids){
  # Changing Data Based on Whether There are Kids 
  if(input_kids == "Respondents With Children"){
    
    if_kids <- data %>% 
      filter(kids == "yes") 
    
    # Widening the Data 
    widened <- if_kids %>% 
      mutate(food_sufficiency = case_when(
        food_sufficiency == "enough not wanted" ~ "enough_not_wanted", 
        food_sufficiency == "enough wanted" ~ "enough_wanted", 
        food_sufficiency == "often not enough" ~ "often_not_enough", 
        food_sufficiency == "sometimes not enough" ~ "sometimes_not_enough"
      )) %>% 
      group_by(week, state, race, wave, food_sufficiency) %>% 
      summarize(people = sum(people))  
    
    widened <- widened %>% 
      group_by(week, state, wave, food_sufficiency) %>% 
      summarize(people = sum(people)) %>% 
      mutate(race = "All") %>% 
      rbind(widened) %>% 
      pivot_wider(names_from = food_sufficiency, values_from = people)
    
    widened %>% 
      mutate(enough_not_wanted = ifelse(is.na(enough_not_wanted), 0, enough_not_wanted), 
             enough_wanted = ifelse(is.na(enough_wanted), 0, enough_wanted), 
             often_not_enough = ifelse(is.na(often_not_enough), 0, often_not_enough), 
             sometimes_not_enough = ifelse(is.na(sometimes_not_enough), 0, sometimes_not_enough), 
             total = enough_not_wanted + enough_wanted + often_not_enough + sometimes_not_enough, 
             region = ifelse(state == "USA", "USA", state %>% tolower())) %>%
      left_join(fi_multipliers) %>% 
      mutate(num_insecure = food1_kid*enough_wanted + 
               food2_kid*enough_not_wanted + 
               food3_kid*sometimes_not_enough + 
               food4_kid*often_not_enough, 
             rate = 100*num_insecure/total) 
    
  } else { 
    
    if_kids <- data  
    
    # Widening the Data 
    widened <- if_kids %>% 
      mutate(food_sufficiency = case_when(
        food_sufficiency == "enough not wanted" ~ "enough_not_wanted", 
        food_sufficiency == "enough wanted" ~ "enough_wanted", 
        food_sufficiency == "often not enough" ~ "often_not_enough", 
        food_sufficiency == "sometimes not enough" ~ "sometimes_not_enough"
      )) %>% 
      group_by(week, wave, state, race, food_sufficiency) %>% 
      summarize(people = sum(people))  
    
    widened <- widened %>% 
      group_by(week, wave, state, food_sufficiency) %>% 
      summarize(people = sum(people)) %>% 
      mutate(race = "All") %>% 
      rbind(widened) %>% 
      pivot_wider(names_from = food_sufficiency, values_from = people)
    
    widened %>% 
      mutate(enough_not_wanted = ifelse(is.na(enough_not_wanted), 0, enough_not_wanted), 
             enough_wanted = ifelse(is.na(enough_wanted), 0, enough_wanted), 
             often_not_enough = ifelse(is.na(often_not_enough), 0, often_not_enough), 
             sometimes_not_enough = ifelse(is.na(sometimes_not_enough), 0, sometimes_not_enough), 
             total = enough_not_wanted + enough_wanted + often_not_enough + sometimes_not_enough, 
             region = ifelse(state == "USA", "USA", state %>% tolower()))%>% 
      left_join(fi_multipliers) %>% 
      mutate(num_insecure = food1*enough_wanted + 
               food2*enough_not_wanted + 
               food3*sometimes_not_enough + 
               food4*often_not_enough, 
             rate = 100*num_insecure/total) 
  }
  

}

### Data Table 
show_most_recent <- function(input_state, input_kids){ 
  ## Replaces "show_averages" 
  
  table_data <- fi_data(input_kids) 
  
  max_week <- table_data$week %>% max()
  
  table_data %>% 
    filter(week %in% (max_week - 2):max_week) %>% 
    select(state, race, rate) %>% 
    group_by(state, race) %>% 
    summarize(rate = mean(rate)) %>% 
    mutate(race = case_when(
      race == "Asian alone, not Hispanic" ~ "Asian", 
      race == "Black alone, not Hispanic" ~ "Black",
      race == "Hispanic or Latino, all Races" ~ "Hispanic or Latino",
      race == "Two or more races or Other" ~ "Other",
      race == "White alone, not Hispanic" ~ "White", 
      TRUE ~ "All"
    )) %>% 
    filter(state %in%  c("USA", input_state)) %>% 
    mutate(rate = round(rate, 1) %>% format(nsmall = 1)) %>% 
    rename(
      `Current Month % Insecure` = rate, 
      State = state, 
      `Race/ Ethnicity` = race) 
  
}

### US Overview 
fi_summary_graph <- function(input_state, input_kids){
  
  # The Data 
  
  df <- fi_data(input_kids) %>% 
    select(week, wave, state, race, rate) %>% 
    filter(race == "All") 
    
  us_data <- df %>% 
    filter(state == "USA") 
  
  selected_data <- df %>% 
    filter(state == input_state)
  
  not_selected_data <- df %>% 
    filter(state != input_state & 
             state != "USA")
  
  text_dt <- df %>% 
    filter(state %in% c("USA", input_state), 
           week == max(df$week)) 
  
  # The Plot
  ggplot() + 
    geom_point(data = selected_data, 
               aes(x = week, y = rate), 
               color = "#7FCECD", 
               size = 0.9, 
               alpha = 1, 
               show.legend = F
              ) +
    geom_line(data = selected_data, 
              aes(x = week, y = rate, group = wave), 
              color = "#7FCECD", 
              size = 0.9, 
              alpha = 1, 
              show.legend = F 
              ) + 
    geom_line(data = not_selected_data, 
              aes(x = week, y = rate, 
                  group = interaction(state, wave)), 
              size = 0.2, 
              alpha = 0.75, 
              show.legend = F, 
              color = "#BBB8B8") + 
    geom_point(data = us_data, 
               aes(x = week, y = rate), 
               alpha = 1, 
               size = ifelse("USA" == input_state, 0.9, 0.7), 
               color = ifelse("USA" == input_state, "#342F2E", "#716C6B"),
               show.legend = F) + 
    geom_line(data = us_data, 
              aes(x = week, y = rate, group = wave), 
              alpha = 1, 
              size = ifelse("USA" == input_state, 0.9, 0.7), 
              color = ifelse("USA" == input_state, "#342F2E", "#716C6B"),
              show.legend = F) + 
    geom_label_repel(data = text_dt, 
                     aes(x = week, y = rate, 
                         label = state),
                     nudge_x = 0.5, 
                     label.size = 0, 
                     alpha = 0.8,
                     size = 4,
                     direction = "y", 
                     segment.alpha = 0,
                     family = "Akkurat Pro") +
    scale_x_continuous(breaks = seq(0, max(df$week) + 1, 1)[c(FALSE, TRUE, FALSE, FALSE)],
                       labels = c(week_labels, "")[c(FALSE, TRUE, FALSE, FALSE)],
                       limits = c(1, max(df$week) + 1)) +
    scale_y_continuous(breaks = seq(0, 100, 5),
                       labels = paste0(seq(0, 100, 5), "%"),
                       limits = c(0, max(df$rate))) + 
    theme_classic() + 
    labs( 
      x = NULL, 
      y = NULL
    )  + 
    theme(
      axis.text = element_text(family = "Akkurat Pro",
                               size = 10)
    )
  
}



# Weekly Food Insecurity By Race Functions ----------------------------------------

fi_race <- function(input_kids, input_state){
  
  df <- fi_data(input_kids) %>% 
    filter(state == input_state) 
  
  ggplot() + 
    # Races 
    geom_point(data = df %>% 
                 filter(race != "All"), 
               aes(x = week, y = rate, group = race), 
               color = "#7FCECD", 
               show.legend = F) + 
    geom_line(data = df %>% 
                filter(race != "All"), 
              aes(x = week, y = rate, linetype = race,
                  group = interaction(race, wave)), 
              color = "#7FCECD", 
              show.legend = F, 
              size = 1.1) + 
    # Overall 
    geom_point(data = df %>% 
                 filter(race == "All"), 
               aes(x = week, y = rate), 
               color = "#BBB8B8", 
               show.legend = F) + 
    geom_line(data = df %>% 
                filter(race == "All"), 
              aes(x = week, y = rate, 
                  group = wave), 
              color = "#BBB8B8", 
              show.legend = F, 
              size = 1.1) + 
    # Graph Specifications
    scale_x_continuous(breaks = seq(0, max(df$week) + 1, 1)[c(FALSE, TRUE, FALSE, FALSE)],
                       labels = c(week_labels, "")[c(FALSE, TRUE, FALSE, FALSE)],
                       limits = c(1, max(df$week) + 0.99)) +
    scale_y_continuous(breaks = seq(0, 100, 10)[c(TRUE, FALSE)],
                       labels = paste0(seq(0, 100, 10)[c(T, F)], "%"),
                       limits = c(0, 100)) +
    theme_classic() +
    labs(
      x = NULL,
      y = NULL
    ) + 
    theme(
      axis.text = element_text(family = "Akkurat Pro",
                               size = 11), 
      strip.text = element_text(family = "Akkurat Pro",
                                size = 12),
      legend.text = element_text(family = "Akkurat Pro", 
                                 size = 10)
    )
  
}


race_line <- function(input_state, input_kids, input_races){
  
  df <- fi_data(input_kids) %>% 
    filter(state == input_state) %>% 
    mutate(race = ifelse(race == "All", "Overall", race))
  
  text_df <- df %>% 
    filter(week == max(df$week)) %>% 
    mutate(text_label = case_when(
      race == "Asian alone, not Hispanic" ~ "Asian", 
      race == "Black alone, not Hispanic" ~ "Black",
      race == "Hispanic or Latino, all Races" ~ "Hispanic or Latino",
      race == "Two or more races or Other" ~ "Other",
      race == "White alone, not Hispanic" ~ "White", 
      race == "Overall" ~ "Overall"
    )) %>% 
    filter(race %in% input_races)
  

  ggplot() + 
    # Races in Selected, Not Overall 
    geom_point(data = df %>% 
                 filter(race != "Overall" & race %in% input_races), 
               aes(x = week, y = rate, group = race), 
               color = "#7FCECD", 
               show.legend = F) + 
    geom_line(data = df %>% 
                filter(race != "Overall" & race %in% input_races), 
              aes(x = week, y = rate, linetype = race, group = interaction(race, wave)), 
              color = "#7FCECD", 
              show.legend = F, 
              size = 1.1) + 
    # Overall 
    geom_point(data = df %>% 
                 filter(race == "Overall"), 
               aes(x = week, y = rate), 
               color = "#BBB8B8", 
               show.legend = F) + 
    geom_line(data = df %>% 
                filter(race == "Overall"), 
              aes(x = week, y = rate, 
                  group = wave), 
              color = "#BBB8B8", 
              show.legend = F, 
              size = 1.1) + 
    # Not Selected Races 
    geom_line(data = df %>% 
                filter(race %notin% input_races), 
              aes(x = week, y = rate, 
                  group = interaction(race, wave)), 
              color = "#D8D6D6", 
              show.legend = F, 
              size = 0.8) + 
    # Specifications
    geom_label_repel(data = text_df,
                     aes(x = week, y = rate,
                         label = text_label),
                     size = 4,
                     alpha = 0.8,
                     label.size = 0,
                     nudge_x = 0.2, 
                     segment.alpha = 0,
                     family = "Akkurat Pro") +
    # Graph Specifications
    scale_x_continuous(breaks = seq(0, max(df$week) + 1, 1)[c(FALSE, TRUE, FALSE, FALSE)],
                       labels = c(week_labels, "")[c(FALSE, TRUE, FALSE, FALSE)],
                       limits = c(1, max(df$week) + 0.99)) +
    scale_y_continuous(breaks = seq(0, 100, 5),
                       labels = paste0(seq(0, 100, 5), "%"), 
                       limits = c(0, max(df$rate))) +
    theme_classic() +
    labs(
      x = "\nWeek Ending",
      y = "Percent of Respondents\n"
    ) + 
    theme(
      axis.text = element_text(family = "Akkurat Pro",
                               size = 12), 
      axis.title = element_text(family = "Akkurat Pro",
                                size = 12), 
      legend.text = element_text(family = "Akkurat Pro", 
                                 size = 10)
    )
  
  
}

input_state <- "USA"
input_kids <- "All"
input_races <- "White alone, not Hispanic"
fi_race_bar <- function(input_state, input_kids, input_races){
 
   df <- fi_data(input_kids) %>% 
    filter(state == input_state) %>% 
    mutate(race = ifelse(race %in% input_races | race == "All", 
                         race, 
                         "Other"))
  
   graphable_data <- left_join(df %>% 
                         filter(race != "All"), 
                       df %>% 
                         filter(race == "All")%>% 
                         rename(state_total = total) %>% 
                         select(week, state_total)) %>% 
    mutate(perc_insecure = 100*num_insecure/state_total,
           race = case_when(
             race == "Asian alone, not Hispanic" ~ "Asian",
             race == "Black alone, not Hispanic" ~ "Black",
             race == "Hispanic or Latino, all Races" ~ "Hispanic or Latino",
             race == "Two or more races or Other" ~ "Two or more races or Other",
             race == "White alone, not Hispanic" ~ "White",
             TRUE ~ "All Other"
           ),
           race = factor(race, levels = c("All Other", "Two or more races or Other",
                                          "Asian", "Hispanic or Latino",
                                          "Black", "White")))
   
#######    RETURN
   first <- graphable_data %>%
     select(week, state, race, enough_not_wanted, enough_wanted,
            often_not_enough, sometimes_not_enough, num_insecure) %>% 
     group_by(week, state, race) %>% 
     summarize(enough_not_wanted = sum(enough_not_wanted), 
               enough_wanted = sum(enough_wanted), 
               often_not_enough = sum(often_not_enough), 
               sometimes_not_enough = sum(sometimes_not_enough), 
               num_insecure = sum(num_insecure)) %>% 
     mutate(group_total = enough_not_wanted + enough_wanted + often_not_enough + sometimes_not_enough) 
   
   second <- first %>% 
     group_by(state, week) %>% 
     summarize(total_population = sum(group_total),
               total_insecure = sum(num_insecure)) %>% 
     left_join(first) %>%
     mutate(perc_of_insecure = 100*num_insecure/total_insecure)
   
  graphable_data <- second %>% 
     group_by(state, race) %>% 
     summarize(perc_of_pop = mean(100*group_total/total_population)) %>% 
     left_join(second) %>% 
     select(state, week, race, perc_of_insecure, perc_of_pop) %>% 
     left_join(graphable_data)
  
  
  races <- ifelse(
    "White alone, not Hispanic" %in% input_races & 
      "Asian alone, not Hispanic" %in% input_races &
      "Hispanic or Latino, all Races" %in% input_races &
      "Black alone, not Hispanic" %in% input_races &
      "Two or more races or Other" %in% input_races, 
    "all", 
    "not_all" 
  )

  
  plot <- ggplot(data = graphable_data, 
                 aes(x = week, y = perc_insecure, fill = race, color = race)) + 
    geom_bar(aes(width = ifelse(week > 13, 1.8, 0.9), 
                 text = paste0(race, " Respondents:\n", 
                               perc_of_insecure %>% round(1) %>% format(nsmall = 1), 
                               "% of Food Insecure\n", 
                                perc_of_pop %>% round(1) %>% format(nsmall = 1), 
                                "% of Population"
                               )), 
             stat = "identity", position = "stack", size = 0) + 
    scale_fill_manual(values = case_when(
      races == "all" ~ c("#007FA4", "#836EAA", "#B6ACD1", "#E4E0EE","#401F68"),
      TRUE ~ c("#D8D6D6", "#836EAA", "#B6ACD1", "#E4E0EE","#401F68")
    )) +
    scale_color_manual(values = case_when(
      races == "all" ~ c("#007FA4", "#836EAA", "#B6ACD1", "#342F2E","#401F68"),
      TRUE ~ c("#D8D6D6", "#836EAA", "#B6ACD1", "#342F2E","#401F68")
    )) +
    theme_classic() + 
    scale_x_continuous(breaks = seq(0, max(df$week) + 1, 1)[c(TRUE, FALSE, FALSE, FALSE)],
                       labels = c(week_labels, "")[c(TRUE, FALSE, FALSE, FALSE)],
                       limits = c(min(df$week) - 0.5, max(df$week) + 1)) +
    scale_y_continuous(breaks = seq(0, 100, 5),
                       labels = paste0(seq(0, 100, 5), "%")) +
    theme_classic() + 
    labs( 
      x = "Week Ending", 
      y = "Percent of Respondents", 
      fill = NULL
    )  + 
    theme(
      axis.text = element_text(family = "Akkurat Pro",
                               size = 12),
      axis.title = element_text(family = "Akkurat Pro",
                                size = 12), 
      legend.position = "right", 
      legend.text = element_text(family = "Akkurat Pro",
                                 size = 12)
    ) + 
    guides(
      color = F
    )
  
  ggplotly(plot, tooltip = "text") %>% 
    config(modeBarButtonsToRemove = c(
      "pan2d", "lasso2d", "zoom2d", 
      "toggleSpikelines", "hoverCompareCartesian", 
      "hoverClosestCartesian", 
      "zoomIn2d", "zoomOut2d", 
      "autoScale2d" # "resetScale2d" 
    ))  %>% 
    layout(showlegend = FALSE)
  
}

# Stacker Function --------------------------------------------------------

### Works for: food_suf_confidence, housing_paid, housing_confidence, anxious,  worry,  down,
###  interest

stacker_data <- function(input_state, input_kids, desired_variable, specified_race, responses, home_status){
  
  
  if(desired_variable %notin% c("housing_confidence", "housing_paid", 
                                "housing_threat")){
    df <- data
    
  } else {
    
    if(length(home_status) == 1 & home_status != "Rent or Own Their Home with a Mortgage or Loan"){
      
      if("Rent their Home" %in% home_status |
         "Renters" %in% home_status){
        
        df <- housing_data %>% 
          filter(housing == "rented") 
        
      }
      
      if("Own their Home with a Mortgage/Loan" %in% home_status | 
         "Homeowners" %in% home_status){
        
        df <- housing_data %>% 
          filter(housing == "owned, mortgage or loan") 
        
      }
      
    } else {
      
      df <- housing_data
      
    }
    
  }
  
  
  # Filtering If Kids
  if(input_kids == "Respondents With Children"){
    df <- df %>% filter(kids == "yes") 
  } else {
    df <- df 
  }
  
  # Data Manipulation 
  first <-  df %>% 
    filter(state == input_state) %>%
    rename(see_this = desired_variable) %>% 
    group_by(week, race, see_this) %>% 
    summarize(people = sum(people)) %>% 
    na.omit(see_this) 
  
  first <- first %>% 
    group_by(week, see_this) %>% 
    summarize(people = sum(people)) %>% 
    mutate(race = "Overall") %>% 
    rbind(first) %>% 
    pivot_wider(names_from = see_this, values_from = people) 
  
  
  if(desired_variable == "food_suf_confidence"){
    
    graphable <- first %>% 
      mutate(
        moderately = ifelse(is.na(moderately), 0, moderately), 
        `not at all` = ifelse(is.na(`not at all`), 0, `not at all`), 
        somewhat = ifelse(is.na(somewhat), 0, somewhat), 
        very = ifelse(is.na(very), 0, very), 
        perc_moderately = moderately/(moderately + `not at all` + somewhat + very),
        perc_not = `not at all`/(moderately + `not at all` + somewhat + very), 
        perc_somewhat =  somewhat/(moderately + `not at all` + somewhat + very), 
        perc_very =  very/(moderately + `not at all` + somewhat + very)
      ) %>% 
      select(week, race, perc_moderately, perc_not, perc_somewhat, perc_very) %>% 
      pivot_longer(cols = 3:6, names_to = "stat", values_to = "perc") %>% 
      mutate(perc = 100 * perc, 
             stat = case_when(
               stat == "perc_moderately" ~ "Moderate", 
               stat == "perc_not" ~ "No Confidence", 
               stat == "perc_somewhat" ~ "Somewhat", 
               stat == "perc_very" ~ "Very Confident"
             ), 
             stat = factor(stat, levels = c("Very Confident",  "Moderate", "Somewhat", "No Confidence"))) %>% 
      filter(race %in% specified_race, 
             stat %in% responses, 
             week < 36)
    
  }
  
  if(desired_variable == "housing_paid"){
    
    graphable <- first %>% 
      mutate(
        yes = ifelse(is.na(yes), 0, yes), 
        no = ifelse(is.na(no), 0, no), 
        deferred = ifelse(is.na(deferred), 0, deferred), 
        perc_yes = 100*yes/(yes + no + deferred), 
        perc_no = 100*no/(yes + no + deferred),
        perc_deferred = 100*deferred/(yes + no + deferred)) %>% 
      select(-yes, -no, -deferred) %>% 
      pivot_longer(cols = 3:5, names_to = "stat", values_to = "perc") %>% 
      mutate(
        stat = case_when(
          stat == "perc_yes" ~ "Paid",
          stat == "perc_no" ~ "Not Paid", 
          stat == "perc_deferred" ~ "Deferred"
        ), 
        stat = case_when(
          week > 12 & stat == "Paid" ~ "Caught Up", 
          week > 12 & stat == "Not Paid" ~ "Not Caught Up", 
          TRUE ~ stat
        ), 
        stat = factor(stat, levels = c("Paid", "Caught Up", 
                                       "Deferred", "Not Paid", 
                                       "Not Caught Up"))) %>% 
      filter(race %in% specified_race,
             stat %in% responses)
  }
  
  if(desired_variable == "housing_confidence") {
    
    graphable <- first %>% 
      mutate(
        high  = ifelse(is.na(`high confidence`), 0, `high confidence`), 
        no  = ifelse(is.na(`no confidence`), 0, `no confidence`),
        slight  = ifelse(is.na(`slight confidence`), 0, `slight confidence`),
        moderate  = ifelse(is.na(`moderate confidence`), 0, `moderate confidence`),
        deferred  = ifelse(is.na(`will be deferred`), 0, `will be deferred`),
        perc_high = 100 * high / (high + no + slight + moderate + deferred), 
        perc_no = 100 * no / (high + no + slight + moderate + deferred),
        perc_slight = 100 * slight / (high + no + slight + moderate + deferred),
        perc_mod = 100 * moderate / (high + no + slight + moderate + deferred),
        perc_deferred = 100 * deferred / (high + no + slight + moderate + deferred)) %>%
      select(week, race, perc_high, perc_no, perc_deferred, perc_slight, perc_mod) %>% 
      pivot_longer(cols = 3:7, names_to = "stat", values_to = "perc") %>% 
      mutate(
        stat = case_when(
          stat == "perc_high" ~ "High",
          stat == "perc_no" ~ "No Confidence", 
          stat == "perc_deferred" ~ "Will Be Deferred", 
          stat == "perc_slight" ~ "Slight", 
          stat == "perc_mod" ~ "Moderate"
        ), 
        stat = factor(stat, levels = c("High", "Moderate", "Slight", 
                                       "Will Be Deferred", "No Confidence"))) %>% 
      filter(race %in% specified_race,
             stat %in% responses)
    
  }
  
  if(desired_variable == "housing_threat"){
    
    graphable <- first %>% 
      mutate(
        extrememly = ifelse(is.na(extrememly), 0, extrememly), 
        not_likely = ifelse(is.na(not_likely), 0, not_likely), 
        somewhat = ifelse(is.na(somewhat), 0, somewhat), 
        very = ifelse(is.na(very), 0, very), 
        perc_extremely = 100*extrememly/(extrememly + not_likely + somewhat + very), 
        perc_not_likely = 100*not_likely/(extrememly + not_likely + somewhat + very),
        perc_very = 100*very/(extrememly + not_likely + somewhat + very),
        perc_somewhat = 100*somewhat/(extrememly + not_likely + somewhat + very)) %>%
      select(-extrememly, -not_likely, -somewhat, -very) %>% 
      pivot_longer(cols = 3:6, names_to = "stat", values_to = "perc") %>%
      mutate(
        stat = case_when(
          stat == "perc_extremely" ~ "Extremely Likely",
          stat == "perc_not_likely" ~ "Not Likely", 
          stat == "perc_very" ~ "Very Likely", 
          stat == "perc_somewhat" ~ "Somewhat Likely"
        ), 
        stat = factor(stat, levels = c("Not Likely", "Somewhat Likely", 
                                       "Very Likely", "Extremely Likely"))) %>% 
      filter(race %in% specified_race,
             stat %in% responses)
  }
  
  if(desired_variable %in% c("anxious", "worry", "down", "interest")) {
    
    graphable <- first %>% 
      mutate(
        several_days = ifelse(is.na(several_days), 0, several_days), 
        not = ifelse(is.na(not), 0, not), 
        almost_every_day = ifelse(is.na(almost_every_day), 0, almost_every_day), 
        most_days = ifelse(is.na(most_days), 0, most_days), 
        perc_several = 100 * several_days/(several_days + not + almost_every_day + most_days), 
        perc_not = 100 * not/(several_days + not + almost_every_day + most_days), 
        perc_almost = 100 *almost_every_day/(several_days + not + almost_every_day + most_days), 
        perc_most = 100 *most_days/(several_days + not + almost_every_day + most_days)) %>% 
      select(week, race, perc_several, perc_not, perc_almost, perc_most) %>% 
      pivot_longer(cols = 3:6, names_to = "stat", values_to = "perc") %>% 
      mutate(
        stat = case_when(
          stat == "perc_several" ~ "Several Days",
          stat == "perc_not" ~ "Not at All", 
          stat == "perc_almost" ~ "Nearly Every Day", 
          stat == "perc_most" ~ "More than Half the Days"
        ), 
        stat = factor(stat, levels = c("Not at All", "Several Days", "More than Half the Days", "Nearly Every Day"))) %>% 
      filter(race %in% specified_race, 
             stat %in% responses)
    # stat != "Not at All")  
    
  }
  
  if(desired_variable == "food_sufficiency"){
    
    graphable <- first %>% 
      mutate(
        Often = ifelse(is.na(`often not enough`), 0, `often not enough`), 
        Sometimes = ifelse(is.na(`sometimes not enough`), 0, `sometimes not enough`),
        Enough = ifelse(is.na(`enough wanted`), 0, `enough wanted`),
        Not_Wanted = ifelse(is.na(`enough not wanted`), 0, `enough not wanted`),
        
        perc_often = 100*Often/(Often + Sometimes + Enough + Not_Wanted), 
        perc_sometimes = 100*Sometimes/(Sometimes + Often + Enough + Not_Wanted), 
        perc_enough = 100 - (perc_often + perc_sometimes)) %>% 
      # perc_enough = 100*Enough/(Often + Sometimes + Enough + Not_Wanted),
      # perc_not_wanted = 100*Not_Wanted/(Often + Sometimes + Enough + Not_Wanted)) %>% 
      
      select(week, race, perc_often, perc_sometimes, perc_enough #, perc_not_wanted
      ) %>%
      
      pivot_longer(cols = 3:5, names_to = "stat", values_to = "perc") %>% 
      mutate(
        stat = case_when(
          stat == "perc_sometimes" ~ "Sometimes", 
          stat == "perc_often" ~ "Often", 
          stat == "perc_enough"~ "Never" # , 
          #  stat == "perc_not_wanted" ~ "Not Wanted"
        ), 
        stat = factor(stat, levels = c("Never", "Sometimes","Often"))) %>% 
      filter(race %in% specified_race, 
             stat %in% responses)
    
  }
  
  graphable <- graphable %>% 
    mutate(race = factor(race, levels = c("Overall", "Asian alone, not Hispanic", 
                                          "Black alone, not Hispanic", 
                                          "Hispanic or Latino, all Races", 
                                          "White alone, not Hispanic", 
                                          "Two or more races or Other")),
           stat_num = as.numeric(stat)) %>% 
    arrange(race, week, desc(stat)) 
  
  
  all_vect <- {} 
  
  for(r in graphable$race %>% unique()){
    
    vect <- {}
    
    for(i in 1:max(graphable$week)){
      dat <- graphable %>% 
        filter(week == i, 
               race == r) %>% 
        arrange(desc(stat_num))
      
      height <- cumsum(dat$perc)
      vect <- c(vect, height)
      
    }
    
    all_vect <- c(all_vect, vect)
  }
  
  
  cbind(graphable %>% arrange(race, week, desc(stat)), height = all_vect) %>% 
    mutate(min_height = height - perc)
}

input_state <- "USA"
input_kids <- "All"
desired_variable <- "food_suf_confidence"
specified_race <- "White alone, not Hispanic"
responses <- "Somewhat"

input_state2 <- "USA"
input_kids2 <- "All"
desired_variable2 <- "food_suf_confidence"
specified_race2 <- "White alone, not Hispanic"
responses2 <- "Somewhat"


stacker_data("USA", "All", "housing_confidence", "White alone, not Hispanic", "Slight", "Rent their Home")

stacker <- function(input_state2, input_kids2, desired_variable2, specified_race2, responses2, home_status2){
  
  if(desired_variable2 == "food_suf_confidence") { 
    week_labels2 <- week_labels[1:32]
  } else {
    week_labels2 <- week_labels
    }

  
  graphable <- stacker_data(input_state2, input_kids2, desired_variable2, specified_race2, responses2, home_status2)
  
  plot <- ggplot(graphable, aes(x = week, y = perc, fill = stat)) + 
    geom_bar(aes(width = ifelse(week > 12, 1.8, 0.9),
                 text = paste0(
                   stat, ": ", 
                   perc %>% round(1) %>% format(nsmall = 1),
                   "%"
                 )), 
             stat = "identity", position = "stack") + 
    labs(x = "Week Ending", 
         y = "Percent of Respondents", 
         fill = NULL, 
         title = NULL) + 
    scale_fill_manual(values = c(
      "#007FA4", 
      "#58B947", 
      "#EF553F", 
      "#FFC520", 
      "#7FCECD"
    )) + 
    scale_alpha_identity() + 
    theme_classic() + 
    scale_x_continuous(breaks = seq(0, max(graphable$week), 1)[c(FALSE, FALSE, TRUE, FALSE)],
                       labels = c(week_labels2, " ")[c(FALSE, FALSE, TRUE, FALSE)],
                       limits = c(min(graphable$week) - 1, max(graphable$week) + 1)) + 
    scale_y_continuous(breaks = seq(0, 100, 10),
                       labels = paste0(seq(0, 100, 10), "%")) +
    theme_classic() + 
    labs( 
      x = NULL, 
      y = NULL
    )  + 
    theme(
      axis.text = element_text(family = "Akkurat Pro",
                               size = 8),
      legend.position = "top", 
      legend.text = element_text(family = "Akkurat Pro",
                                 size = 8), 
      strip.text = element_text(family = "Akkurat Pro", size = 10) #, 
    #  panel.spacing = unit(2, "lines")
    ) + 
    facet_wrap("race", 
               ncol = 2, 
               scales = "free_x") +
    guides(
      color = F
    )
  
  ggplotly(plot, tooltip = "text") %>% 
    config(modeBarButtonsToRemove = c(
      "pan2d", "lasso2d", "zoom2d", 
      "toggleSpikelines", "hoverCompareCartesian", 
      "hoverClosestCartesian", 
      "zoomIn2d", "zoomOut2d", 
      "autoScale2d" # "resetScale2d" 
    )) %>% 
    layout(
      legend = list(
        orientation = "h",
         x = 0.3, 
         y = 1.1 
      )
    )
  
}



# Header Graph  -----------------------------------------------------------

stacker_head <- function(input_state, input_kids, desired_variable){
 
  week_labels2 <- week_labels 
  
  if(desired_variable %in% c("housing_paid","housing_confidence", "housing_threat")){
    df <- housing_data
  } else {

      df <- data
  }


  # Filtering If Kids
  if(input_kids == "Respondents With Children"){
    df <- df %>% filter(kids == "yes") 
  } else {
    df <- df 
  }
  
  # Data Manipulation 
  first <- df %>% 
    filter(state == input_state) %>% 
    rename(see_this = desired_variable) %>% 
    group_by(week, race, see_this) %>% 
    summarize(people = sum(people)) %>% 
    na.omit(see_this) 
  
  first <- first %>% 
    group_by(week, see_this) %>% 
    summarize(people = sum(people)) %>% 
    mutate(race = "Overall") %>% 
    rbind(first) %>% 
    pivot_wider(names_from = see_this, values_from = people) 
  
  
  if(desired_variable == "food_suf_confidence"){
   
    week_labels2 <- week_labels[1:35]
    
    graphable <- first %>% 
      mutate(
        moderately = ifelse(is.na(moderately), 0, moderately), 
        `not at all` = ifelse(is.na(`not at all`), 0, `not at all`), 
        somewhat = ifelse(is.na(somewhat), 0, somewhat), 
        very = ifelse(is.na(very), 0, very), 
        perc_moderately = moderately/(moderately + `not at all` + somewhat + very),
        perc_not = `not at all`/(moderately + `not at all` + somewhat + very), 
        perc_somewhat =  somewhat/(moderately + `not at all` + somewhat + very), 
        perc_very =  very/(moderately + `not at all` + somewhat + very)
      ) %>% 
      select(week, race, perc_moderately, perc_not, perc_somewhat, perc_very) %>% 
      pivot_longer(cols = 3:6, names_to = "stat", values_to = "perc") %>% 
      mutate(perc = 100 * perc, 
             stat = case_when(
               stat == "perc_moderately" ~ "Moderate", 
               stat == "perc_not" ~ "No Confidence", 
               stat == "perc_somewhat" ~ "Somewhat", 
               stat == "perc_very" ~ "Very Confident"
             ), 
             stat = factor(stat, levels = c("Very Confident",  "Moderate", "Somewhat", "No Confidence"))) %>% 
      filter(race == "Overall", 
             stat != "Very Confident",
             week %in% 1:36)
    
  }
  
  if(desired_variable == "housing_paid"){
    
    graphable <- first %>% 
      mutate(
        yes = ifelse(is.na(yes), 0, yes), 
        no = ifelse(is.na(no), 0, no), 
        deferred = ifelse(is.na(deferred), 0, deferred), 
        perc_yes = 100*yes/(yes + no + deferred), 
        perc_no = 100*no/(yes + no + deferred), 
        perc_deferred = 100*deferred/(yes + no + deferred)) %>% 
      select(week, race, perc_yes, perc_no, perc_deferred) %>% 
      pivot_longer(cols = 3:5, names_to = "stat", values_to = "perc") %>% 
      mutate(
        stat = case_when(
          stat == "perc_yes" ~ "Paid",
          stat == "perc_no" ~ "Not Paid", 
          stat == "perc_deferred" ~ "Deferred"
        ), 
        stat = factor(stat, levels = c("Paid", "Deferred", "Not Paid"))) %>% 
      filter(race == "Overall", 
             stat != "Paid")  
  }
  
  if(desired_variable == "housing_confidence") {
    
    graphable <- first %>% 
      mutate(
        high  = ifelse(is.na(`high confidence`), 0, `high confidence`), 
        no  = ifelse(is.na(`no confidence`), 0, `no confidence`),
        slight  = ifelse(is.na(`slight confidence`), 0, `slight confidence`),
        moderate  = ifelse(is.na(`moderate confidence`), 0, `moderate confidence`),
        deferred  = ifelse(is.na(`will be deferred`), 0, `will be deferred`),
        perc_high = 100 * high / (high + no + slight + moderate + deferred), 
        perc_no = 100 * no / (high + no + slight + moderate + deferred),
        perc_slight = 100 * slight / (high + no + slight + moderate + deferred),
        perc_mod = 100 * moderate / (high + no + slight + moderate + deferred),
        perc_deferred = 100 * deferred / (high + no + slight + moderate + deferred)) %>% 
      select(week, race, perc_high, perc_no, perc_deferred, perc_slight, perc_mod) %>% 
      pivot_longer(cols = 3:7, names_to = "stat", values_to = "perc") %>% 
      mutate(
        stat = case_when(
          stat == "perc_high" ~ "High",
          stat == "perc_no" ~ "No Confidence", 
          stat == "perc_deferred" ~ "Will Be Deferred", 
          stat == "perc_slight" ~ "Slight", 
          stat == "perc_mod" ~ "Moderate"
        ), 
        stat = factor(stat, levels = c("High", "Moderate", "Slight", 
                                       "No Confidence", 
                                       "Will Be Deferred"))) %>% 
      filter(race == "Overall", 
             stat != "High")
    
  }
  
  if(desired_variable == "housing_threat"){
    
    graphable <- first %>% 
      mutate(
        extrememly = ifelse(is.na(extrememly), 0, extrememly), 
        not_likely = ifelse(is.na(not_likely), 0, not_likely), 
        somewhat = ifelse(is.na(somewhat), 0, somewhat), 
        very = ifelse(is.na(very), 0, very), 
        perc_extremely = 100*extrememly/(extrememly + not_likely + somewhat + very), 
        perc_not_likely = 100*not_likely/(extrememly + not_likely + somewhat + very),
        perc_very = 100*very/(extrememly + not_likely + somewhat + very),
        perc_somewhat = 100*somewhat/(extrememly + not_likely + somewhat + very)) %>%
      select(-extrememly, -not_likely, -somewhat, -very) %>% 
      pivot_longer(cols = 3:6, names_to = "stat", values_to = "perc") %>%
      mutate(
        stat = case_when(
          stat == "perc_extremely" ~ "Extremely Likely",
          stat == "perc_not_likely" ~ "Not Likely", 
          stat == "perc_very" ~ "Very Likely", 
          stat == "perc_somewhat" ~ "Somewhat Likely"
        ), 
        stat = factor(stat, levels = c("Not Likely", "Somewhat Likely", 
                                       "Very Likely", "Extremely Likely"))) %>% 
      filter(race %in% "Overall",
             stat %in% c("Somewhat Likely", 
                         "Very Likely", "Extremely Likely"))
  }
  
  if(desired_variable == "food_sufficiency"){
    
    graphable <- first %>% 
      mutate(
        Often = ifelse(is.na(`often not enough`), 0, `often not enough`), 
        Sometimes = ifelse(is.na(`sometimes not enough`), 0, `sometimes not enough`),
        Enough = ifelse(is.na(`enough wanted`), 0, `enough wanted`),
        Not_Wanted = ifelse(is.na(`enough not wanted`), 0, `enough not wanted`),
        
        perc_often = 100*Often/(Often + Sometimes + Enough + Not_Wanted), 
        perc_sometimes = 100*Sometimes/(Sometimes + Often + Enough + Not_Wanted), 
        perc_enough = 100*(Enough+Not_Wanted)/(Often + Sometimes + Enough + Not_Wanted), 
        perc_enough = ifelse(perc_enough == 0 | is.na(perc_enough), 
                             100 - (perc_often + perc_sometimes), 
                             perc_enough)
          # 100*(Enough+Not_Wanted)/(Often + Sometimes + Enough + Not_Wanted),
       # perc_not_wanted = 100*Not_Wanted/(Often + Sometimes + Enough + Not_Wanted)
       ) %>% 
      
      select(week, race, perc_often, 
             perc_sometimes, perc_enough #, perc_not_wanted
             ) %>%
      
      pivot_longer(cols = 3:5, names_to = "stat", values_to = "perc") %>% 
      mutate(
        stat = case_when(
          stat == "perc_sometimes" ~ "Sometimes", 
          stat == "perc_often" ~ "Often", 
          stat == "perc_enough"~ "Never" 
  #        stat == "perc_not_wanted" ~ "Not Wanted"
        ), 
        stat = factor(stat, levels = c("Never", "Sometimes", "Often"))) %>% 
      filter(race == "Overall", 
             stat != "Never")  
    
  }
  
  graphable <- graphable %>% 
    mutate(race = factor(race, levels = c("Overall", "Asian alone, not Hispanic", 
                                          "Black alone, not Hispanic", 
                                          "Hispanic or Latino, all Races", 
                                          "White alone, not Hispanic", 
                                          "Two or more races or Other")))
  
  plot <- ggplot(graphable, aes(x = week, y = perc)) + 
    geom_bar(aes(fill = stat, width = ifelse(week > 12, 1.8, 0.9),
                 text = paste0(stat, ": ", 
                               perc %>% 
                                 round(1) %>% 
                                 format(nsmall = 1), 
                               "%")), 
             stat = "identity", position = "stack", 
             show.legend = ifelse(
               desired_variable %in% c("anxious", "worry", "down", "interest"), 
               T, 
               F))  +  
    labs(x = "Week Ending", 
         y = "Percent of Respondents", 
         fill = NULL, 
         title = NULL) + 
    scale_fill_manual(values = c(
      "#007FA4", 
      "#58B947", 
      "#FFC520", 
      "#EF553F", 
      "#7FCECD"
    )) +
    theme_classic() + 
    scale_x_continuous(breaks = seq(0, 
                                    max(graphable$week) + 1, 
                                    1)[c(TRUE, FALSE, FALSE, FALSE)],
                       labels = c(week_labels2, "")[c(TRUE, FALSE, FALSE, FALSE)]) +
    scale_y_continuous(breaks = seq(0, 100, 10)[c(TRUE, FALSE)],
                       labels = paste0(seq(0, 100, 10)[c(TRUE, FALSE)], "%"), 
                       limits = c(0, 100)) +
    theme_classic() + 
    labs( 
      x = NULL, 
      y = NULL
    )  + 
    theme(
      axis.text = element_text(family = "Akkurat Pro",
                               size = 10),
      legend.position = "top", 
      legend.text = element_text(family = "Akkurat Pro",
                                 size = 12)
    ) 
  
  ### GETTING US VLINE FOR STRESS VARIABLES 

  if(desired_variable == "food_sufficiency"){
    print(plot) 
  } else {
    ggplotly(plot, tooltip = "text") %>% 
      config(modeBarButtonsToRemove = c(
        "pan2d", "lasso2d", "zoom2d", 
        "toggleSpikelines", "hoverCompareCartesian", 
        "hoverClosestCartesian", 
        "zoomIn2d", "zoomOut2d", 
        "autoScale2d" # "resetScale2d" 
      ))%>% 
      layout(showlegend = FALSE,
             height = 300)
    
  }

}


# Employment Functions ----------------------------------------------------------

### Works for: lost_work, housing, comp_available, wifi_avail

employment_donut_data <- function(input_state, input_kids){
  
  # If there are kids, redefine base data 
  if(input_kids == "Respondents With Children"){
    donut_df <- employment_data %>% 
      filter(kids == "yes") 
  } else {
    donut_df <- employment_data 
  }
  
  # Renaming Desired Variables 
  
  donut_df <- donut_df %>%       
    mutate(
      lost_work = case_when(
        lost_work == "yes" ~ "Lost Employment Income", 
        lost_work == "no" ~ "Did Not Lose Employment Income"
      ),
      lost_work = factor(lost_work, 
                         levels = c("Lost Employment Income",
                                    "Did Not Lose Employment Income")))
  
  # Core Function
  donut_df <- donut_df %>% 
    filter(state == input_state) %>% 
    group_by(week, race, lost_work) %>% 
    summarize(people = sum(people)) %>% 
    na.omit(lost_work)
  
  donut_totals <- donut_df %>% 
    group_by(week, lost_work) %>% 
    summarize(people = sum(people)) %>% 
    mutate(race = "Overall")
  
  rbind(donut_df, donut_totals) %>% 
    group_by(race, lost_work) %>%
    mutate(people = mean(people)) %>% 
    select(-week) %>% 
    unique()
  
} 

employment_donut_overall <- function(state_input, kids_input){
  
  # Finalizing the Data 
  donut <- employment_donut_data(state_input, kids_input) %>% 
    filter(race == "Overall") 
  
  donut <- donut %>% 
    mutate(share = people / sum(donut$people))
  
  donut <- donut %>% 
    cbind(max = cumsum(donut$share)) %>% 
    mutate(min = max - share, 
           labpos = min + ((max - min)/2),
           label = format(round(100*share, 1), nsmall = 1) %>% paste0("%"))
  
  # Plotting the Graph 
  plot <- donut %>% 
    ggplot(aes(ymin = min, ymax = max, 
               xmin = 3, xmax = 4,
               fill = lost_work)) + 
    geom_rect() + 
    geom_label(x = 3.5, aes(y = labpos, label = label), 
               label.size = NA, 
               fill = "white", alpha = 0.7) + 
    coord_polar(theta = "y") + 
    theme_void() +
    labs(
      fill = NULL, 
      title = NULL
    ) + 
    scale_fill_manual(values = c(
      "#7FCECD", 
      "#58B947", 
      "#FFC520", 
      "#EF553F", 
      "#007FA4"
    )) + 
    theme(
      legend.position = "bottom", 
      legend.direction = "vertical", 
      text = element_text(size = 14)
    ) + 
    xlim(c(2, 4))
  
  print(plot) 
  
}

employment_bar <- function(state_input, kids_input){
  
  df <- employment_donut_data(state_input, kids_input) 
  
  graphable <- df %>% 
    pivot_wider(names_from = "lost_work", values_from = "people") %>% 
    mutate(
      `Lost Employment Income Share` = `Lost Employment Income`/(`Lost Employment Income`+`Did Not Lose Employment Income`),
      `Did Not Lose Employment Income Share` = `Did Not Lose Employment Income`/(`Lost Employment Income`+`Did Not Lose Employment Income`)
    ) %>% 
    select(-`Lost Employment Income`, -`Did Not Lose Employment Income`) %>% 
    rename("Lost Employment Income" = "Lost Employment Income Share",
           "Did Not Lose Employment Income" = "Did Not Lose Employment Income Share") %>% 
    pivot_longer(cols = 2:3, names_to = "lost_work", values_to = "share") %>% 
    mutate(race2 = factor(race, levels = c("White alone, not Hispanic",
                                           "Hispanic or Latino, all Races",
                                           "Black alone, not Hispanic", 
                                           "Asian alone, not Hispanic", 
                                           "Two or more races or Other",
                                           "Overall")),
           share = 100 * share) %>% 
    filter(lost_work == "Lost Employment Income")
  
  plot <- ggplot(graphable, aes(x = race2, y = share, fill = lost_work)) + 
    geom_bar(aes(text = paste0(share %>% 
                                 round(1) %>% 
                                 format(nsmall = 1), 
                               "%")), 
             stat = "identity", position = "stack") + 
    theme_classic() + 
    coord_flip() + 
    labs(
      y = "Percentage", 
      x = NULL, 
      fill = NULL
    ) + 
    scale_fill_manual(values = c("#5091CD", 
                                 "#FFC520"
    )) + 
    scale_y_continuous(breaks = seq(0, 100, 25),
                       labels = paste0(seq(0, 100, 25), "%")) +
    theme(
      axis.text = element_text(family = "Akkurat Pro",
                               size = 10),
      legend.position = "top", 
      legend.text = element_text(family = "Akkurat Pro",
                                 size = 12)
    )
  
  ggplotly(plot, tooltip = "text") %>% 
    config(modeBarButtonsToRemove = c(
      "pan2d", "lasso2d", "zoom2d", 
      "toggleSpikelines", "hoverCompareCartesian", 
      "hoverClosestCartesian", 
      "zoomIn2d", "zoomOut2d", 
      "autoScale2d" # "resetScale2d" 
    ))%>% 
    layout(showlegend = FALSE)
  
  
}

### Line Graphs 
overall_employment_perc <- function(input_state, input_kids){ 
  # If there are kids, redefine base data 
  if(input_kids == "Respondents With Children"){
    df1 <- employment_data %>% 
      filter(kids == "yes")
  } else {
    df1 <- employment_data 
  }
  
  
  # Selected State 
  df <- df1 %>% 
    filter(state == input_state) %>% 
    group_by(week, wave, state, employed) %>% 
    summarize(people = sum(people)) %>% 
    na.omit(employed) %>% 
    pivot_wider(names_from = "employed", values_from = "people") %>% 
    mutate(rate = 100*yes/(yes+no))
  
  
  # USA Data 
  df_usa <- df1 %>% 
    filter(state == "USA") %>% 
    group_by(week, wave, state, employed) %>% 
    summarize(people = sum(people)) %>% 
    na.omit(employed) %>% 
    pivot_wider(names_from = "employed", values_from = "people") %>% 
    mutate(rate = 100*yes/(yes+no))
  
  # Not Selected States 
  df2 <- df1 %>% 
    filter(state != input_state & state != "USA") %>% 
    group_by(week, wave, state, employed) %>% 
    summarize(people = sum(people)) %>% 
    na.omit(employed)%>% 
    pivot_wider(names_from = "employed", values_from = "people") %>% 
    mutate(rate = 100*yes/(yes+no))
  
  # Text Data 
  text_dt <- rbind(df, df_usa) %>% 
    select(week, state, rate) %>% 
    filter(state %in% c("USA", input_state), 
           week == max(df$week)) %>% 
    unique()

  ylim_max <- max(max(df$rate), max(df2$rate))
  
  ggplot() + 
    # States Not Selected
    geom_line(data = df2, 
              aes(x = week, y = rate, 
                  group = interaction(state, wave)), 
              color = "#BBB8B8", 
              show.legend = F, 
              size = 0.2, 
              alpha = 0.75) + 
    # Selected State
    geom_point(data = df, 
               aes(x = week, y = rate), 
               color ="#7FCECD" , 
               show.legend = F) + 
    geom_line(data = df, 
              aes(x = week, y = rate, group = wave), 
              color = "#7FCECD", 
              show.legend = F, 
              size = 1.1) + 
    # USA
    geom_point(data = df_usa, 
               aes(x = week, y = rate), 
               color ="#342F2E" , 
               show.legend = F) + 
    geom_line(data = df_usa, 
              aes(x = week, y = rate,
                  group = wave), 
              color = "#342F2E", 
              show.legend = F, 
              size = 1.1) + 
    geom_label_repel(data = text_dt, 
                     aes(x = week, y = rate, 
                         label = state),
                     nudge_x = 0.5, 
                     label.size = 0, 
                     alpha = 0.8,
                     size = 4,
                     direction = "y", 
                     segment.alpha = 0,
                     family = "Akkurat Pro") +
    # Graph Specifications
    scale_x_continuous(breaks = seq(0, max(df$week) + 1, 1)[c(FALSE, TRUE, FALSE, FALSE)],
                       labels = c(week_labels, "")[c(FALSE, TRUE, FALSE, FALSE)],
                       limits = c(1, max(df$week) + 1)) +
    scale_y_continuous(breaks = seq(0, 100, 10),
                       labels = paste0(seq(0, 100, 10), "%"),
                       limits = c(0, ylim_max)) +
    theme_classic() +
    labs(
      x = "\nWeek Ending",
      y = "Percent of Respondents\n"
    ) + 
    theme(
      axis.text = element_text(family = "Akkurat Pro",
                               size = 12), 
      axis.title = element_text(family = "Akkurat Pro",
                                size = 12), 
      legend.text = element_text(family = "Akkurat Pro", 
                                 size = 10)
    ) 
  
}

overall_employment_perc_expect <- function(input_state, input_kids){ 
  # If there are kids, redefine base data 
  if(input_kids == "Respondents With Children"){
    df1 <- employment_data %>% 
      filter(kids == "yes") %>% 
      filter(week <= 62)
  } else {
    df1 <- employment_data %>% 
      filter(week <= 62)
  }
  
  new_labs <- week_labels[1:64]
  
  # Selected State 
  df <- df1 %>% 
    filter(state == input_state) %>% 
    group_by(week, wave, state, expect_loss) %>% 
    summarize(people = sum(people)) %>% 
    na.omit(expect_loss) %>% 
    pivot_wider(names_from = "expect_loss", values_from = "people") %>% 
    mutate(rate = 100*yes/(yes+no))
  
  
  # USA Data 
  df_usa <- df1 %>% 
    filter(state == "USA") %>% 
    group_by(week, wave, state, expect_loss) %>% 
    summarize(people = sum(people)) %>% 
    na.omit(expect_loss) %>% 
    pivot_wider(names_from = "expect_loss", values_from = "people") %>% 
    mutate(rate = 100*yes/(yes+no))
  
  # Not Selected States 
  df2 <- df1 %>% 
    filter(state != input_state & state != "USA") %>% 
    group_by(week, wave, state, expect_loss) %>% 
    summarize(people = sum(people)) %>% 
    na.omit(expect_loss)%>% 
    pivot_wider(names_from = "expect_loss", values_from = "people") %>% 
    mutate(rate = 100*yes/(yes+no))
  
  # Text Data 
  text_dt <- rbind(df, df_usa) %>% 
    select(week, state, rate) %>% 
    filter(state %in% c("USA", input_state), 
           week == max(df$week)) %>% 
    unique()
  
  ylim_max <- max(max(df$rate), max(df2$rate))
  
  ggplot() + 
    # States Not Selected
    geom_line(data = df2, 
              aes(x = week, y = rate, 
                  group = interaction(state, wave)), 
              color = "#BBB8B8", 
              show.legend = F, 
              size = 0.2, 
              alpha = 0.75) + 
    # Selected State
    geom_point(data = df, 
               aes(x = week, y = rate), 
               color ="#7FCECD" , 
               show.legend = F) + 
    geom_line(data = df, 
              aes(x = week, y = rate, 
                  group = wave), 
              color = "#7FCECD", 
              show.legend = F, 
              size = 1.1) + 
    # USA
    geom_point(data = df_usa, 
               aes(x = week, y = rate), 
               color ="#342F2E" , 
               show.legend = F) + 
    geom_line(data = df_usa, 
              aes(x = week, y = rate,
                  group = wave), 
              color = "#342F2E", 
              show.legend = F, 
              size = 1.1) + 
    geom_label_repel(data = text_dt, 
                     aes(x = week, y = rate, 
                         label = state),
                     nudge_x = 0.5, 
                     label.size = 0, 
                     alpha = 0.8,
                     size = 4,
                     direction = "y", 
                     segment.alpha = 0,
                     family = "Akkurat Pro") +
    # Graph Specifications
    scale_x_continuous(breaks = seq(0, max(df$week) + 1, 1)[c(FALSE, TRUE, FALSE, FALSE)],
                       labels = c(new_labs, "")[c(FALSE, TRUE, FALSE, FALSE)],
                       limits = c(1, max(df$week) + 1)) +
    scale_y_continuous(breaks = seq(0, 100, 10),
                       labels = paste0(seq(0, 100, 10), "%"),
                       limits = c(0, ylim_max)) +
    theme_classic() +
    labs(
      x = "\nWeek Ending",
      y = "Percent of Respondents\n"
    ) + 
    theme(
      axis.text = element_text(family = "Akkurat Pro",
                               size = 12), 
      axis.title = element_text(family = "Akkurat Pro",
                                size = 12), 
      legend.text = element_text(family = "Akkurat Pro", 
                                 size = 10)
    ) 
  
}

employment_by_race_expect <- function(input_state, input_kids, input_races){ 
  
  # If there are kids, redefine base data 
  if(input_kids == "Respondents With Children"){
    
    df1 <- employment_data %>% 
      filter(kids == "yes") 
    
  } else {
    df1 <- employment_data 
  }
  
  df1 <- df1 %>% 
    filter(weeek <= 62)  ### EXAMPLE OF ERROR: filtering weeks 
  
  new_labs <- week_labels[1:64] #### EXAMPLE OF ERROR: limits labels 
  
  # Getting an overall race line
  df_overall <- df1 %>% 
    filter(state == input_state) %>% 
    group_by(week, wave, state, expect_loss) %>% 
    summarize(people = sum(people)) %>% 
    na.omit(expect_loss) %>% 
    pivot_wider(names_from = "expect_loss", values_from = "people") %>% 
    mutate(rate = 100*yes/(yes+no), 
           race = "Overall")
  
  # Getting Data By Race 
  df_race <- df1 %>% 
    filter(state == input_state) %>% 
    group_by(week, wave, state, race, expect_loss) %>% 
    summarize(people = sum(people)) %>% 
    na.omit(expect_loss) %>% 
    pivot_wider(names_from = "expect_loss", values_from = "people") %>% 
    mutate(rate = 100*yes/(yes+no))
  
  # Combining Data 
  df <- rbind(df_overall, df_race) 
  
  df_selected <- df %>% 
    filter(race %in% input_races)
  
  df_other <- df %>% 
    filter(race %notin% input_races)
  
  # Text Data 
  text_dt <- df_selected %>% 
    select(week, rate, race) %>% 
    filter(week == max(df$week)) %>% 
    mutate(race = case_when(
      race == "Asian alone, not Hispanic" ~ "Asian", 
      race == "Black alone, not Hispanic" ~ "Black",
      race == "Hispanic or Latino, all Races" ~ "Hispanic or Latino",
      race == "Two or more races or Other" ~ "Other",
      race == "White alone, not Hispanic" ~ "White", 
      race == "Overall" ~ "Overall"
    )) %>% 
    unique()
  
  ylim_max <- max(df$rate)
  
  ggplot() + 
    # States Not Selected
    geom_line(data = df_other, 
              aes(x = week, y = rate, 
                  group = interaction(race, wave)), 
              color = "#BBB8B8", 
              show.legend = F, 
              size = 0.2, 
              alpha = 0.75) + 
    # Selected State
    geom_point(data = df_selected, 
               aes(x = week, y = rate), 
               color ="#7FCECD" , 
               show.legend = F) + 
    geom_line(data = df_selected, 
              aes(x = week, y = rate, linetype = race, 
                  group = interaction(race, wave)), 
              color = "#7FCECD", 
              show.legend = F, 
              size = 1.1) + 
    # label
    geom_label_repel(data = text_dt, 
                     aes(x = week, y = rate, 
                         label = race),
                     nudge_x = 0.5, 
                     label.size = 0, 
                     alpha = 0.8,
                     size = 4,
                     direction = "y", 
                     segment.alpha = 0,
                     family = "Akkurat Pro") +
    # Graph Specifications
    scale_x_continuous(breaks = seq(0, max(df$week) + 1, 1)[c(FALSE, TRUE, FALSE, FALSE)],
                       labels = c(new_labs, "")[c(FALSE, TRUE, FALSE, FALSE)],
                       limits = c(1, max(df$week) + 1)) +
    scale_y_continuous(breaks = seq(0, 100, 10),
                       labels = paste0(seq(0, 100, 10), "%"),
                       limits = c(0, ylim_max)) +
    theme_classic() +
    labs(
      x = "\nWeek Ending",
      y = "Percent of Respondents\n"
    ) + 
    theme(
      axis.text = element_text(family = "Akkurat Pro",
                               size = 12), 
      axis.title = element_text(family = "Akkurat Pro",
                                size = 12), 
      legend.text = element_text(family = "Akkurat Pro", 
                                 size = 10)
    ) 
  
}

employment_by_race <- function(input_state, input_kids, input_races){ 
 
   # If there are kids, redefine base data 
  if(input_kids == "Respondents With Children"){
    df1 <- employment_data %>% 
      filter(kids == "yes")
  } else {
    df1 <- employment_data 
  }
  
  
  # Getting an overall race line
  df_overall <- df1 %>% 
    filter(state == input_state) %>% 
    group_by(week, wave, state, employed) %>% 
    summarize(people = sum(people)) %>% 
    na.omit(employed) %>% 
    pivot_wider(names_from = "employed", values_from = "people") %>% 
    mutate(rate = 100*yes/(yes+no), 
           race = "Overall")
  
  # Getting Data By Race 
  df_race <- df1 %>% 
    filter(state == input_state) %>% 
    group_by(week, wave, state, race, employed) %>% 
    summarize(people = sum(people)) %>% 
    na.omit(employed) %>% 
    pivot_wider(names_from = "employed", values_from = "people") %>% 
    mutate(rate = 100*yes/(yes+no))
  
  # Combining Data 
  df <- rbind(df_overall, df_race) 
  
  df_selected <- df %>% 
    filter(race %in% input_races)
  
  df_other <- df %>% 
    filter(race %notin% input_races)
  
  # Text Data 
  text_dt <- df_selected %>% 
    select(week, rate, race) %>% 
    filter(week == max(df$week)) %>% 
    mutate(race = case_when(
      race == "Asian alone, not Hispanic" ~ "Asian", 
      race == "Black alone, not Hispanic" ~ "Black",
      race == "Hispanic or Latino, all Races" ~ "Hispanic or Latino",
      race == "Two or more races or Other" ~ "Other",
      race == "White alone, not Hispanic" ~ "White", 
      race == "Overall" ~ "Overall"
    )) %>% 
    unique()
  
  ylim_max <- max(df$rate)
  
  ggplot() + 
    # States Not Selected
    geom_line(data = df_other, 
              aes(x = week, y = rate, 
                  group = interaction(race, wave)), 
              color = "#BBB8B8", 
              show.legend = F, 
              size = 0.2, 
              alpha = 0.75) + 
    # Selected State
    geom_point(data = df_selected, 
               aes(x = week, y = rate), 
               color ="#7FCECD" , 
               show.legend = F) + 
    geom_line(data = df_selected, 
              aes(x = week, y = rate, linetype = race,
                  group = interaction(race, wave)), 
              color = "#7FCECD", 
              show.legend = F, 
              size = 1.1) + 
    # label
    geom_label_repel(data = text_dt, 
                     aes(x = week, y = rate, 
                         label = race),
                     nudge_x = 0.5, 
                     label.size = 0, 
                     alpha = 0.8,
                     size = 4,
                     direction = "y", 
                     segment.alpha = 0,
                     family = "Akkurat Pro") +
    # Graph Specifications
    scale_x_continuous(breaks = seq(0, max(df$week) + 1, 1)[c(FALSE, TRUE, FALSE, FALSE)],
                       labels = c(week_labels, "")[c(FALSE, TRUE, FALSE, FALSE)],
                       limits = c(1, max(df$week) + 1)) +
    scale_y_continuous(breaks = seq(0, 100, 10),
                       labels = paste0(seq(0, 100, 10), "%"),
                       limits = c(0, ylim_max)) +
    theme_classic() +
    labs(
      x = "\nWeek Ending",
      y = "Percent of Respondents\n"
    ) + 
    theme(
      axis.text = element_text(family = "Akkurat Pro",
                               size = 12), 
      axis.title = element_text(family = "Akkurat Pro",
                                size = 12), 
      legend.text = element_text(family = "Akkurat Pro", 
                                 size = 10)
    ) 
  
}

overall_employment <- function(input_state, input_kids, var){
  
  # If there are kids, redefine base data 
  if(input_kids == "Respondents With Children"){
    df1 <- employment_data %>% 
      filter(kids == "yes")
  } else {
    df1 <- employment_data 
  }
  
  # Core Function
  df <- df1 %>% 
    rename(show_this = ifelse(var == "expect_loss", "expect_loss", "employed")) %>% 
    filter(state == input_state) %>% 
    group_by(week, race, show_this) %>% 
    summarize(people = sum(people)) %>% 
    na.omit(show_this)
  
  df <- df %>% 
    group_by(week, show_this) %>% 
    summarize(people = sum(people)) %>% 
    filter(show_this == "yes") %>% 
    rename(rate = people)
  
  df2 <- df1 %>% 
    rename(show_this = ifelse(var == "expect_loss", "expect_loss", "employed")) %>% 
    filter(state != input_state & state != "USA") %>% 
    group_by(week, state, race, show_this) %>% 
    summarize(people = sum(people)) %>% 
    na.omit(show_this)
  
  df2 <- df2 %>% 
    group_by(week, state, show_this) %>% 
    summarize(people = sum(people)) %>% 
    filter(show_this == "yes") %>% 
    rename(rate = people)
  
  check <- F
  
  if(max(df$rate) < 1000000){
    
    these <- df2 %>% 
      group_by(state) %>% 
      summarize(mx = max(rate)) %>% 
      filter(mx < 1000000) %>% 
      pluck("state")
    
    df2 <- df2 %>% 
      filter(state %in% these)
    
    check <- T 
    
  }
  
  ggplot() + 
    # Races Not Selected
    geom_line(data = df2, 
              aes(x = week, y = rate/1000000, group = state), 
              color = "#BBB8B8", 
              show.legend = F, 
              size = 0.7, 
              alpha = 0.4) + 
    # Races in Selected, Not Overall 
    geom_point(data = df, 
               aes(x = week, y = rate/1000000), 
               color ="#7FCECD" , 
               show.legend = F) + 
    geom_line(data = df, 
              aes(x = week, y = rate/1000000), 
              color = "#7FCECD", 
              show.legend = F, 
              size = 1.1) + 
    # Graph Specifications
    scale_x_continuous(breaks = seq(0, max(df$week) + 1, 1)[c(FALSE, TRUE, FALSE, FALSE)],
                       labels = c(week_labels, "")[c(FALSE, TRUE, FALSE, FALSE)],
                       limits = c(1, max(df$week) + 1)) +
    theme_classic() +
    labs(
      x = "\nWeek Ending",
      y = "Number of Respondents (millions)\n"
    ) + 
    theme(
      axis.text = element_text(family = "Akkurat Pro",
                               size = 12), 
      axis.title = element_text(family = "Akkurat Pro",
                                size = 12), 
      legend.text = element_text(family = "Akkurat Pro", 
                                 size = 10)
    ) + 
   ylim(c(0, ifelse(
     input_state %in% c("Texas", "California", 
                        "New York", "Florida"), 
    16, 
     ifelse(input_state == "USA", max(df$rate)/1000000, 
            ifelse(check == T, 1, 
                   ifelse(var == "expect_loss", 4, 6))))))
  
  
}

## Employment By Race 
employment_area_data <- function(state_input, kids_input, races_input, var_chosen){
  # If there are kids, redefine base data 
  if(kids_input == "Respondents With Children"){
    df1 <- employment_data %>% 
      filter(kids == "yes") 
  } else {
    df1 <- employment_data 
  }
  
  df1 <- df1 %>% 
    rename(this_one = var_chosen)
  
  ## Data Prep 
  df2 <- df1 %>% 
    filter(state == state_input) %>% 
    mutate(race = ifelse(race %in% races_input, 
                         race,
                         "Other"))
  
  levs <- df2 %>%
    filter(this_one == "yes") %>% 
    group_by(race) %>% 
    summarize(val = mean(people)) %>% 
    filter(race != "Other") %>% 
    arrange(val) %>% 
    pluck("race")
  
  
  df3 <- df2 %>% 
    mutate(race = factor(race, levels = c("Other",
                                          levs))) %>% 
    group_by(week, race, this_one) %>% 
    summarize(people = sum(people)) 

  graphable_data <- df3 %>% 
    filter(this_one == "yes") %>% 
    select(-this_one)
  
  ## Getting the data for proportional bar 
  full <- df3 %>% 
    na.omit(this_one) %>% 
    group_by(week, race) %>% 
    summarize(people = sum(people)) %>% 
    group_by(race) %>% 
    summarize(pop = mean(people))
  
  total <- df3 %>% 
    filter(week == max(df3$week), 
           this_one == "yes") %>% 
    pluck("people") %>% 
    sum()
  
  prop_bar <- full %>% 
    mutate(pop_prop = pop/sum(full$pop), 
           height = pop_prop*total, 
           week = max(df3$week) + 1) %>% 
    select(week, race, people = height, pop_prop)
  
  
  testing <- rbind(graphable_data, prop_bar) %>% 
    mutate(people = people/1000000)
  
  heights <- {} 
  for(i in 1:length(unique(testing$week))){
    
    new_dat <- testing %>% 
      filter(week == i) %>% 
      arrange(desc(race))
    
    vals <- cumsum(new_dat$people)
    
    heights <- c(heights, vals)
    
  }
  
  cbind(testing %>% arrange(week, desc(race)), heights = heights) %>% 
    mutate(min_height = heights - people)
  
}

employment_area <- function(input_state, input_kids, input_races, chosen_var){
  
  data <- employment_area_data(input_state, input_kids, input_races, chosen_var) 
  
  graphable_data <- data %>% 
    filter(is.na(pop_prop)) %>% 
    select(-pop_prop)
  
  prop_bar <- data %>% 
    filter(!is.na(pop_prop))
  
  ### plotting 
  
  plot <- ggplot(data = graphable_data, 
                 mapping = aes(x = week, y = people, fill = race, color = race)) + 
    geom_area() +
    geom_bar(data = prop_bar, 
             position = "stack", stat = "identity", alpha = 0.6) + 
    scale_fill_manual(values = case_when(
      "Other" %in% graphable_data$race %>% unique() ~ c("#D8D6D6", "#836EAA", "#B6ACD1", "#E4E0EE","#401F68"),
      TRUE ~ c("#007FA4", "#836EAA", "#B6ACD1", "#E4E0EE","#401F68")
    )) +
    scale_color_manual(values = case_when(
      "Other" %in% graphable_data$race %>% unique() ~ c("#D8D6D6", "#836EAA", "#B6ACD1", "#342F2E","#401F68"),
      TRUE ~ c("#007FA4", "#836EAA", "#B6ACD1", "#342F2E","#401F68")
    )) +
    theme_classic() + 
    scale_x_continuous(breaks = seq(0, max(graphable_data$week) + 1, 1)[c(FALSE, TRUE, FALSE, FALSE)],
                       labels = c(week_labels, "")[c(FALSE, TRUE, FALSE, FALSE)],
                       limits = c(1, max(graphable_data$week) + 1)) +
    # scale_y_continuous(breaks = seq(0, 100, 5),
    #                    labels = paste0(seq(0, 100, 5), "%")) +
    theme_classic() + 
    labs( 
      x = "Week Ending", 
      y = "People (million)", 
      fill = NULL
    )  + 
    theme(
      axis.text = element_text(family = "Akkurat Pro",
                               size = 12),
      axis.title = element_text(family = "Akkurat Pro",
                                size = 12), 
      legend.position = "right", 
      legend.text = element_text(family = "Akkurat Pro",
                                 size = 12), 
      text = element_text(family = "Akkurat Pro", size = 12)
    ) + 
    guides(color = F)
  
  print(plot)

}

# Housing -----------------------------------------------------------------

housing_donut_data <- function(input_state, input_kids){
  
  # If there are kids, redefine base data 
  if(input_kids == "Respondents With Children"){
    donut_df <- housing_data %>% 
      filter(kids == "yes") 
  } else {
    donut_df <- housing_data 
  }
  
  # Renaming Desired Variables 
  
  donut_df <- donut_df %>%       
    mutate(
      housing = case_when(
        housing == "occupied w/o rent" ~ "Occupied Without Payment of Rent", 
        housing == "owned" ~ "Fully Owned", 
        housing == "owned, mortgage or loan" ~ "Owned With Mortgage or Loan", 
        housing == "rented" ~ "Rented"
      ), 
      housing = factor(housing, levels = c("Occupied Without Payment of Rent",
                                           "Fully Owned",
                                           "Owned With Mortgage or Loan",
                                           "Rented")))
  
  # Core Function
  donut_df <- donut_df %>% 
    filter(state == input_state) %>% 
    group_by(week, race, housing) %>% 
    summarize(people = sum(people)) %>% 
    na.omit(housing)
  
  donut_totals <- donut_df %>% 
    group_by(week, housing) %>% 
    summarize(people = sum(people)) %>% 
    mutate(race = "Overall")
  
  rbind(donut_df, donut_totals) %>% 
    group_by(race, housing) %>%
    mutate(people = mean(people)) %>% 
    select(-week) %>% 
    unique()
  
} 

housing_donut_overall <- function(state_input, kids_input){
  
  # Finalizing the Data 
  donut <- housing_donut_data(state_input, kids_input) %>% 
    filter(race == "Overall") 
  
  donut <- donut %>% 
    mutate(share = people / sum(donut$people))
  
  donut <- donut %>% 
    cbind(max = cumsum(donut$share)) %>% 
    mutate(min = max - share, 
           labpos = min + ((max - min)/2),
           label = format(round(100*share, 1), nsmall = 1) %>% paste0("%"))
  
  # Plotting the Graph 
  plot <- donut %>% 
    ggplot(aes(ymin = min, ymax = max, 
               xmin = 3, xmax = 4,
               fill = housing)) + 
    geom_rect() + 
    geom_label(x = 3.5, aes(y = labpos, label = label), 
               label.size = NA, 
               fill = "white", alpha = 0.7) + 
    coord_polar(theta = "y") + 
    theme_void() +
    labs(
      fill = NULL, 
      title = NULL
    ) + 
    scale_fill_manual(values =c("#401F68",
                                "#B6ACD1", 
                                "#5091CD", 
                                "#7FCECD",
                                "#EF553F"
    )) + 
    theme(
      legend.position = "bottom", 
      legend.direction = "vertical", 
      text = element_text(family = "Akkurat Pro", size = 14)
    ) + 
    xlim(c(2, 4))
  
  print(plot) 
  
}

housing_header_data <- function(input_state, input_kids){
  
  if(input_kids == "Respondents With Children"){ 
    df <- housing_data %>% 
      filter(kids == "yes") 
  } else {
    df <- housing_data
  }
  # The Data 
  
  all_data <- df %>% 
    select(week, state, race, housing_paid, people) %>% 
    na.omit(housing_paid) %>% 
    group_by(week, state, housing_paid) %>% 
    summarize(people = sum(people)) %>% 
    pivot_wider(names_from = housing_paid, values_from = people) %>% 
    mutate(no = ifelse(is.na(no), 0, no),
           yes = ifelse(is.na(yes), 0, yes), 
           deferred = ifelse(is.na(deferred), 0, deferred),
           rate = (no + deferred)*100/(yes+no+deferred))
  
  
  all_data %>% 
    filter(state == input_state | 
             state == "USA") %>% 
    select(- deferred, - no, - yes)
  
}

housing_header <- function(input_state, input_kids){
  
  if(input_kids == "Respondents With Children"){ 
    df <- housing_data %>% 
      filter(kids == "yes") 
  } else {
      df <- housing_data
    }
  # The Data 
  
  all_data <- df %>% 
    select(week, wave, state, race, housing_paid, people) %>% 
    na.omit(housing_paid) %>% 
    group_by(week, wave, state, housing_paid) %>% 
    summarize(people = sum(people)) %>% 
    pivot_wider(names_from = housing_paid, values_from = people) %>% 
    mutate(no = ifelse(is.na(no), 0, no),
           yes = ifelse(is.na(yes), 0, yes), 
           deferred = ifelse(is.na(deferred), 0, deferred),
           rate = (no + deferred)*100/(yes+no+deferred))
  
  
  selected_data <- all_data %>% 
    filter(state == input_state) 
  
  us_data <- all_data %>% 
    filter(state == "USA")
  
  not_selected_data <- all_data %>% 
    filter(state != input_state & state != "USA")
  
  text_dt <- rbind(
    selected_data,
    us_data
  ) %>% 
    filter(week == us_data$week %>% max())
  
  # The Plot
  ggplot() + 
    geom_point(data = selected_data, 
               aes(x = week, y = rate), 
               color = "#7FCECD", 
               size = 0.9, 
               alpha = 1, 
               show.legend = F
    ) + 
    geom_line(data = selected_data, 
              aes(x = week, y = rate, 
                  group = wave), 
              color = "#7FCECD", 
              size = 0.9, 
              alpha = 1, 
              show.legend = F 
    ) + 
    geom_line(data = not_selected_data, 
              aes(x = week, y = rate, group = interaction(state, wave)), 
              size = 0.2, 
              alpha = 0.75, 
              show.legend = F, 
              color = "#BBB8B8") + 
    geom_point(data = us_data, 
               aes(x = week, y = rate), 
               alpha = 1, 
               size = ifelse("USA" == input_state, 0.9, 0.5), 
               color = ifelse("USA" == input_state, "#342F2E", "#716C6B"),
               show.legend = F) +
    geom_line(data = us_data, 
              aes(x = week, y = rate, group = wave), 
              alpha = 1, 
              size = ifelse("USA" == input_state, 0.9, 0.5), 
              color = ifelse("USA" == input_state, "#342F2E", "#716C6B"),
              show.legend = F) + 
    geom_label_repel(data = text_dt %>% unique(), 
                     aes(x = week, y = rate, 
                         label = state),
                     nudge_x = 0.5, 
                     label.size = 0, 
                     alpha = 0.8,
                     size = 4,
                     direction = "y", 
                     segment.alpha = 0,
                     family = "Akkurat Pro") + 
    scale_x_continuous(breaks = seq(0, max(df$week) + 1, 1)[c(FALSE, TRUE, FALSE, FALSE)],
                       labels = c(week_labels, "")[c(FALSE, TRUE, FALSE, FALSE)],
                       limits = c(1, max(df$week) + 1)) + 
    scale_y_continuous(breaks = seq(0, 100, 5),
                       labels = paste0(seq(0, 100, 5), "%"),
                       limits = c(0, max(all_data$rate))) + 
    theme_classic() + 
    labs( 
      x = NULL, 
      y = NULL
    )  + 
    theme(
      axis.text = element_text(family = "Akkurat Pro",
                               size = 10)
    )
  
  
}




housing_donut_race <- function(state_input, kids_input, view_race){
  
  # Finalizing the Data 
  donut <- housing_donut_data(state_input, kids_input) %>% 
    filter(race == view_race) 
  
  donut <- donut %>% 
    mutate(share = people / sum(donut$people))
  
  donut <- donut %>% 
    cbind(max = cumsum(donut$share)) %>% 
    mutate(min = max - share, 
           labpos = min + ((max - min)/2),
           label = format(round(100*share, 1), nsmall = 1) %>% paste0("%"))
  
  # Plotting the Graph 
  plot <- donut %>% 
    ggplot(aes(ymin = min, ymax = max, 
               xmin = 3, xmax = 4,
               fill = housing)) + 
    geom_rect() + 
    geom_label(x = 3.5, aes(y = labpos, label = label), 
               label.size = NA, 
               fill = "white", alpha = 0.7) + 
    coord_polar(theta = "y") + 
    theme_void() +
    labs(
      fill = NULL, 
      title = NULL
    ) + 
    scale_fill_manual(values =c("#401F68",
                                "#B6ACD1", 
                                "#5091CD", 
                                "#7FCECD",
                                "#EF553F"
    )) + 
    theme(
      legend.position = "bottom", 
      legend.direction = "vertical", 
      text = element_text(family = "Akkurat Pro", size = 14)
    ) + 
    xlim(c(2, 4))
  
  print(plot) 
  
}



# Children ----------------------------------------------------------------

children_donut_data <- function(input_state, input_kids, var){
  
  # If there are kids, redefine base data 
    donut_df <- children_data %>% 
      filter(kids == "yes") 
    
  # Renaming Desired Variables 
  donut_df <- donut_df %>%       
    na.omit(comp_available) %>%
    na.omit(wifi_available) %>% 
    mutate(
      comp_available = case_when(
        comp_available == "always" ~ "Always",
        comp_available == "usually" ~ "Usually",
        comp_available == "sometimes" ~ "Sometimes",
        comp_available == "rarely" ~ "Rarely",
        comp_available == "never" ~ "Never"
      ), 
      comp_available = factor(comp_available, 
                              levels = c("Always",
                                         "Usually",
                                         "Sometimes",
                                         "Rarely",
                                         "Never")), 
      wifi_avail = case_when(
        wifi_avail == "always" ~ "Always",
        wifi_avail == "usually" ~ "Usually",
        wifi_avail == "sometimes" ~ "Sometimes",
        wifi_avail == "rarely" ~ "Rarely",
        wifi_avail == "never" ~ "Never"
      ), 
      wifi_avail = factor(wifi_avail, 
                          levels = c("Always",
                                     "Usually",
                                     "Sometimes",
                                     "Rarely",
                                     "Never"))) %>% 
    rename(show_this = var)
  
  # Core Function
  donut_df <- donut_df %>% 
    filter(state == input_state) %>% 
    group_by(week, race, show_this) %>% 
    summarize(people = sum(people)) %>% 
    na.omit(show_this)
  
  donut_totals <- donut_df %>% 
    group_by(week, show_this) %>% 
    summarize(people = sum(people)) %>% 
    mutate(race = "Overall")
  
  rbind(donut_df, donut_totals) %>% 
    group_by(race, show_this) %>%
    mutate(people = mean(people)) %>% 
    select(-week) %>% 
    unique()
  
} 

children_donut_overall <- function(state_input, kids_input, var){
  
  # Finalizing the Data 
  donut <- children_donut_data(state_input, kids_input, var) %>% 
    filter(race == "Overall") 
  
  donut <- donut %>% 
    mutate(share = people / sum(donut$people))
  
  donut <- donut %>% 
    cbind(max = cumsum(donut$share)) %>% 
    mutate(min = max - share, 
           labpos = min + ((max - min)/2),
           label = format(round(100*share, 1), nsmall = 1) %>% paste0("%"))
  
  # Plotting the Graph 
  plot <- donut %>% 
    ggplot(aes(ymin = min, ymax = max, 
               xmin = 3, xmax = 4,
               fill = show_this)) + 
    geom_rect(show.legend = T) + 
    geom_label(x = 3.6, aes(y = labpos, label = label), 
               label.size = NA, 
               fill = "white", alpha = 0.7) + 
    coord_polar(theta = "y") + 
    theme_void() +
    labs(
      fill = NULL, 
      title = NULL
    ) + 
    scale_fill_manual(values =c("#401F68",
                                "#B6ACD1", 
                                "#5091CD", 
                                "#7FCECD",
                                "#EF553F"
    )) + 
    theme(
      legend.position = "right", 
      legend.direction = "vertical"
    ) + 
    xlim(c(2, 4))
  
  print(plot)
  
}
  

availability_data <- function(input_state, resource, freaqs){
  if(resource == "comp"){
    df <- children_data %>% 
      rename(this = comp_available) 
  } else {
    
    df <- children_data %>% 
      rename(this = wifi_avail) 
  }
  
  us_standard <- df %>% 
    na.omit(this) %>% 
    filter(state == "USA") %>% 
    group_by(this) %>% 
    summarize(people = sum(people)) %>% 
    mutate(race = "US Overall", 
           state = "USA")
  
  
  df1 <- df %>% 
    na.omit(this) %>% 
    filter(state == input_state) %>%
    select(state, race, this, people) %>% 
    rbind(us_standard) %>% 
    mutate(race = case_when(
      race == "Asian alone, not Hispanic" ~ "Asian",
      race == "Black alone, not Hispanic" ~ "Black",
      race == "Hispanic or Latino, all Races" ~ "Hispanic or Latino",
      race == "Two or more races or Other" ~ "2+ or Other",
      race == "White alone, not Hispanic" ~ "White",
      race == "US Overall" ~ "US Overall"
    ), 
    race = factor(race, levels = c("US Overall", "2+ or Other", "Asian",
                                   "Black", "Hispanic or Latino","White"))) %>% 
    group_by(race, this) %>% 
    summarize(people = sum(people)) %>% 
    pivot_wider(names_from = this, values_from = people) %>% 
    mutate(
      # perc_always = 100*always/(always + never + rarely + sometimes + usually), 
      perc_never = 100*never/(always + never + rarely + sometimes + usually), 
      perc_rarely = 100*rarely/(always + never + rarely + sometimes + usually), 
      perc_sometimes = 100*sometimes/(always + never + rarely + sometimes + usually), 
      perc_usually = 100*usually/(always + never + rarely + sometimes + usually), 
      perc_always = 100 - (perc_never + perc_rarely + perc_sometimes + perc_usually)
    ) %>% 
    pivot_longer(cols = 7:11, names_to = "freq", values_to = "rate") %>%
    mutate(freq = case_when(
      freq == "perc_always" ~ "Always", 
      freq == "perc_never" ~ "Never", 
      freq == "perc_rarely" ~ "Rarely", 
      freq == "perc_sometimes" ~ "Sometimes", 
      freq == "perc_usually" ~ "Usually" 
    ), 
    freq = factor(freq, levels = c("Always", "Usually", "Sometimes", "Rarely", "Never"))) %>% 
    select(race, freq, rate) %>% 
    filter(freq %in% freaqs) %>% 
    mutate(race_num = as.numeric(race))
  
  
  vect <- {} 
  for(i in 1:max(df1$race_num)){
    mid <- df1 %>% 
      filter(race_num == i) %>% 
      arrange(desc(freq)) 
    
    height <- cumsum(mid$rate) 
    
    vect <- c(vect, height)

  }
  
  cbind(df1, height = vect) %>% 
    mutate(min_height = height - rate)
  
}

availability <- function(input_state2, resource2, freaqs2){
  
  to_graph <- availability_data(input_state2, resource2, freaqs2)
  
  plot <- ggplot(to_graph, aes(x = race_num, y = rate, fill = freq, color = freq)) + 
    geom_bar(aes(text = paste0(
      freq, 
      ": ", 
      rate %>% round(1) %>% format(nsmall = 1), 
      "%"
    )), 
             stat = "identity", position = "stack") + 
    scale_fill_manual(values = c("#007FA4", "#836EAA", "#B6ACD1", "#E4E0EE","#401F68")) +
    scale_color_manual(values = c("#007FA4", "#836EAA", "#B6ACD1", "#342F2E","#401F68")) +
    theme_classic() + 
    scale_y_continuous(breaks = seq(0, 100, 10),
                       labels = paste0(seq(0, 100, 10), "%"), 
                       limits = c(0, 100)) + 
    scale_x_continuous(breaks = seq(1, 6, 1), 
                       labels = c("US Overall", "2+ or Other", "Asian",
                                  "Black", "Hispanic or Latino","White")) + 
    labs( 
      x = NULL, 
      y = "Percent of Respondents", 
      fill = NULL
    )  + 
    theme(
      axis.text = element_text(family = "Akkurat Pro",
                               size = 12),
      axis.title = element_text(family = "Akkurat Pro",
                                size = 12), 
      legend.position = "right", 
      legend.text = element_text(family = "Akkurat Pro",
                                 size = 12)
) + 
    guides(
      color = F
    )
  
  ggplotly(plot, tooltip = "text") %>% 
    config(modeBarButtonsToRemove = c(
      "pan2d", "lasso2d", "zoom2d", 
      "toggleSpikelines", "hoverCompareCartesian", 
      "hoverClosestCartesian", 
      "zoomIn2d", "zoomOut2d", 
      "autoScale2d" # "resetScale2d" 
    ))  %>% 
    layout()
  
}
  
# Finances ----------------------------------------------------------------
  

finances_header_data <- function(state_input, kids_input, race_input, 
                                 spending_source, difficulties){
  #  Filtering If Kids
  if(kids_input == "Respondents With Children"){
    df <- finances_data %>% 
      filter(kids == "yes", 
             week <= 62) 
  } else {
    df <- finances_data %>% 
      filter(week <= 62)
  }
  
  
  
  df <- df %>% 
    filter(expenses_difficulty %in% difficulties)
  
  one <- df %>%
    filter(state == state_input) %>%
    group_by(week, wave, race, credit_card_loans, regular_income,
             savings_assets, borrowing_relatives, 
             ui, snap_spending_needs, stimulus, deferred_payments) %>%
    summarize(people = sum(people))
  
  two <- one %>%
    group_by(week, wave, credit_card_loans, regular_income,
             savings_assets, borrowing_relatives,
             ui, snap_spending_needs, stimulus, deferred_payments) %>%
    summarize(people = sum(people)) %>%
    mutate(race = "Overall")
  
  three <- rbind(one, two)
  
  graphable <- three %>%
    pivot_longer(cols = 4:11,
                 names_to = "source", values_to = "used") %>%
    mutate(source = case_when(
      source == "credit_card_loans" ~ "Credit Card Loans",
      source == "regular_income" ~ "Regular Income",
      source == "savings_assets" ~ "Savings or Assests",
      source == "borrowing_relatives" ~ "Borrowing From Friends or Relatives",
      source == "ui" ~ "Unemployment Insurance",
      source == "stimulus" ~ "Government Stimulus Payment",
      source == "deferred_payments" ~ "Deferred Payments",
      source == "snap_spending_needs" ~ "SNAP Benefits"
    ),
    source = factor(source, levels = c(
      "Borrowing From Friends or Relatives",
      "Savings or Assests",
      "Credit Card Loans",
      "Deferred Payments",
      "Unemployment Insurance",
      "SNAP Benefits", 
      "Government Stimulus Payment",
      "Regular Income"
    ))) %>%
    group_by(week, wave, race, source, used) %>%
    summarize(people = sum(people)) %>%
    filter(used == "yes") %>%
    left_join(
      three %>%
        group_by(week, wave, race) %>%
        summarize(total = sum(people))
    ) %>%
    mutate(perc_used = 100*people/total) %>%
    filter(race == race_input, 
           source %in% spending_source
           )
  
  
}

finances_header_func <- function(state_input, kids_input, race_input, 
                                 spending_source, difficulties){
  
  graphable <- finances_header_data(state_input, kids_input, race_input, 
                                    spending_source, difficulties)
  
  new_labs <- week_labels[1:64]
  
  # Plotting
  plot <- ggplot(aes(x = week, y = perc_used, color = source),
         data = graphable) +
    geom_point(aes(text = perc_used %>% round(1) %>% format(nsmall = 1)),
               size = 2, alpha = 0.8) + 
    geom_line(size = 1, alpha = 0.8) +
    labs(x = "\nWeek Ending",
         y = NULL,
         color = NULL,
         title = "Percent of Respondents Who Reported Using Each Source") +
    scale_x_continuous(breaks = seq(17, max(graphable$week) + 1, 1)[c(FALSE, TRUE, FALSE, FALSE)],
                       labels = c(new_labs[18:length(new_labs)], "")[c(FALSE, TRUE, FALSE, FALSE)],
                       limits = c(17, max(graphable$week) + 1)) +
    scale_y_continuous(breaks = seq(0, 100, 10),
                       labels = paste0(seq(0, 100, 10), "%")) + 
    scale_color_manual(
      values = c("#401F68", "#7FCECD", "#EF553F",
                 "#EDE93B", "#008656", "#B6ACD1",
                 "#5091CD", "#D85820", "#FFC520",
                 "#58B947", "#0D2D6C", "#CA7C1B")) +
    theme_classic() + 
    theme(
      text = element_text(family = "Akkurat Pro", size = 12)
    ) + 
    expand_limits(y = 0)
  
  ggplotly(plot, tooltip = "text") %>% 
    config(modeBarButtonsToRemove = c(
      "pan2d", "lasso2d", "zoom2d", 
      "toggleSpikelines", "hoverCompareCartesian", 
      "hoverClosestCartesian", 
      "zoomIn2d", "zoomOut2d", 
      "autoScale2d" # "resetScale2d" 
    ))  %>% 
    layout()
  
  
  
}


### Finances Stacker 

finances_stacker <- function(input_state, input_kids, specified_race, responses){
  
  df <- finances_data %>% 
    filter(week > 12)
  
  # Filtering If Kids
  if(input_kids == "Respondents With Children"){
    df <- df %>% 
      filter(kids == "yes") %>% 
      select(-kids, -wave)
  } else {
    df <- df %>% 
      select(-kids, -wave)
  }
  
  # Data Manipulation 
  first <- df %>% 
    filter(state == input_state) %>% 
    group_by(week, race, expenses_difficulty) %>% 
    summarize(people = sum(people)) %>% 
    na.omit(expenses_difficulty)
    
  first <- first %>% 
    group_by(week, expenses_difficulty) %>% 
    summarize(people = sum(people)) %>% 
    mutate(race = "Overall") %>% 
    rbind(first) %>% 
    pivot_wider(names_from = expenses_difficulty, values_from = people) 
  
  graphable <- first %>% 
    mutate(
      a_little = ifelse(is.na(a_little), 0, a_little),
      not = ifelse(is.na(not), 0, not),
      somewhat = ifelse(is.na(somewhat), 0, somewhat),
      very = ifelse(is.na(very), 0, very), 
      perc_alittle = a_little/(a_little + not + somewhat + very), 
      perc_not = not/(a_little + not + somewhat + very), 
      perc_somewhat = somewhat/(a_little + not + somewhat + very), 
      perc_very = very/(a_little + not + somewhat + very)
    ) %>%
    select(week, race, perc_alittle, perc_not, perc_somewhat, perc_very) %>% 
    pivot_longer(cols = 3:6, names_to = "stat", values_to = "perc") %>% 
    mutate(perc = 100 * perc, 
           stat = case_when(
             stat == "perc_alittle" ~ "a_little",
             stat == "perc_not" ~ "not", 
             stat == "perc_somewhat" ~ "somewhat", 
             stat == "perc_very" ~ "very"
           )) %>% 
    filter(race %in% specified_race, 
           stat %in% responses) %>% 
    mutate(stat = case_when(
      stat == "a_little" ~ "A Little Difficult",
      stat == "not" ~ "Not Difficult", 
      stat == "somewhat" ~  "Somewhat Difficult", 
      stat == "very" ~ "Very Difficult"
           ))
  
  
  graphable <- graphable %>% 
    mutate(race = factor(race, levels = c("Overall", "Asian alone, not Hispanic", 
                                          "Black alone, not Hispanic", 
                                          "Hispanic or Latino, all Races", 
                                          "White alone, not Hispanic", 
                                          "Two or more races or Other")),
           stat_num = as.numeric(stat)) %>% 
    arrange(race, week, desc(stat))
  
  
  all_vect <- {} 
  
  for(r in graphable$race %>% unique()){
    
    vect <- {}
    
    for(i in 1:max(graphable$week)){
      dat <- graphable %>% 
        filter(week == i, 
               race == r) %>% 
        arrange(desc(stat_num))
      
      height <- cumsum(dat$perc)
      vect <- c(vect, height)
      
    }
    
    all_vect <- c(all_vect, vect)
  }
  
  
  graphable <- cbind(graphable %>% arrange(race, week, desc(stat)), height = all_vect) %>% 
    mutate(min_height = height - perc)
  

  plot <- ggplot(graphable, aes(x = week, y = perc, fill = stat)) + 
    geom_bar(aes(text = paste0(stat, " ", 
                               perc %>% round(1) %>% format(nsmall = 1), 
                               "%")), 
    width = 1.8, stat = "identity", position = "stack") + 
    scale_fill_manual(values = c(
      "#007FA4", 
      "#58B947", 
      "#EF553F", 
      "#FFC520", 
      "#7FCECD"
    )) + 
    theme_classic() + 
    scale_x_continuous(breaks = seq(17, max(graphable$week) + 1, 1)[c(FALSE, TRUE, FALSE, FALSE)],
                       labels = c(week_labels[18:length(week_labels)], "")[c(FALSE, TRUE, FALSE, FALSE)]) +
    scale_y_continuous(breaks = seq(0, 100, 10),
                       labels = paste0(seq(0, 100, 10), "%"), 
                       limits = c(0, 101)) +
    labs(x = "Week Ending", 
         y = "Percent of Respondents", 
         fill = NULL, 
         title = NULL) + 
    theme(
      axis.text = element_text(family = "Akkurat Pro",
                               size = 10),
      legend.position = "right", 
      legend.text = element_text(family = "Akkurat Pro",
                                 size = 12), 
      strip.text = element_text(family = "Akkurat Pro", size = 14), 
      panel.spacing = unit(2, "lines")
    ) + 
    guides(
      color = F
    )
  
  ggplotly(plot, tooltip = "text") %>% 
    config(modeBarButtonsToRemove = c(
      "pan2d", "lasso2d", "zoom2d", 
      "toggleSpikelines", "hoverCompareCartesian", 
      "hoverClosestCartesian", 
      "zoomIn2d", "zoomOut2d", 
      "autoScale2d" # "resetScale2d" 
    ))
  
}




# Mental Health  ----------------------------------------------------------

mental_health_data_func <- function(input_state, input_kids, desired_variable2, input_race, input_levels2){
  
  # Filtering If Kids
  if(input_kids == "Respondents With Children"){
    df <- mental_health_data %>% filter(kids == "yes") 
  } else {
    df <- mental_health_data 
  }
  
  # Data Manipulation 
  first <- df %>% 
    filter(state == input_state) %>% 
    rename(see_this = desired_variable2) %>% 
    group_by(week, wave, race, see_this) %>% 
    summarize(people = sum(people)) %>% 
    na.omit(see_this) 
  
  first <- first %>% 
    group_by(week, wave, see_this) %>% 
    summarize(people = sum(people)) %>% 
    mutate(race = "Overall") %>% 
    rbind(first) %>% 
    pivot_wider(names_from = see_this, values_from = people) 
  
  graphable <- first %>% 
    mutate(
      several_days = ifelse(is.na(several_days), 0, several_days), 
      not = ifelse(is.na(not), 0, not), 
      almost_every_day = ifelse(is.na(almost_every_day), 0, almost_every_day), 
      most_days = ifelse(is.na(most_days), 0, most_days), 
      perc_several = 100 * several_days/(several_days + not + almost_every_day + most_days), 
      perc_not = 100 * not/(several_days + not + almost_every_day + most_days), 
      perc_almost = 100 *almost_every_day/(several_days + not + almost_every_day + most_days), 
      perc_most = 100 *most_days/(several_days + not + almost_every_day + most_days), 
      perc_not = ifelse(is.na(perc_not) | perc_not == 0, 
                        100 - (perc_several+ perc_almost + perc_most), 
                        perc_not)) %>% 
    select(week, wave, race, perc_several, perc_not, perc_almost, perc_most) %>% 
    pivot_longer(cols = 4:7, names_to = "stat", values_to = "perc") %>% 
    mutate(
      stat = case_when(
        stat == "perc_several" ~ "Several Days",
        stat == "perc_not" ~ "Never", 
        stat == "perc_almost" ~ "Nearly Every Day", 
        stat == "perc_most" ~ "More than Half the Days"
      ), 
      stat = factor(stat, levels = c("Never", "Several Days", "More than Half the Days", "Nearly Every Day"))) %>% 
    filter(race == input_race, 
           stat %in% input_levels2)  
  
  graphable %>% 
    mutate(race = factor(race, levels = c("Overall", "Asian alone, not Hispanic", 
                                          "Black alone, not Hispanic", 
                                          "Hispanic or Latino, all Races", 
                                          "White alone, not Hispanic", 
                                          "Two or more races or Other")))
}

mental_health <- function(input_state2, input_kids2, desired_variable, input_race2, input_levels){
  
  # Filtering If Kids
  if(input_kids2 == "Respondents With Children"){
    df <- mental_health_data %>% filter(kids == "yes") 
  } else {
    df <- mental_health_data 
  }
  
  graphable <- mental_health_data_func(input_state2, input_kids2, desired_variable, input_race2, input_levels)
  
  plot <- ggplot(graphable, aes(x = week, y = perc)) + 
    geom_bar(aes(fill = stat, width = ifelse(week > 12, 1.8, 0.9),
                 text = paste0(stat, ": ", perc %>% round(1) %>% format(nsmall = 1), "%")), 
             stat = "identity", position = "stack", 
             show.legend = ifelse(
               desired_variable %in% c("anxious", "worry", "down", "interest"), 
               T, 
               F))  +  
    labs(x = "Week Ending", 
         y = "Percent of Respondents", 
         fill = NULL, 
         title = NULL) + 
    scale_fill_manual(values = c("#5091CD", 
                                 "#007FA4",
                                 "#0D2D6C", 
                                 "#CA7C1B",
                                 "#FFC520"
    )) +
    theme_classic() + 
    scale_x_continuous(breaks = seq(0, max(graphable$week) + 1, 1)[c(FALSE, TRUE, FALSE, FALSE)],
                       labels = c(week_labels, "")[c(FALSE, TRUE, FALSE, FALSE)],
                       limits = c(0, max(graphable$week) + 4)) +
    scale_y_continuous(breaks = seq(0, 100, 10)[c(TRUE, FALSE)],
                       labels = paste0(seq(0, 100, 10)[c(TRUE, FALSE)], "%"), 
                       limits = c(0, 101)) +
    theme_classic() + 
    labs( 
      x = NULL, 
      y = NULL
    )  + 
    theme(
      axis.text = element_text(family = "Akkurat Pro",
                               size = 8),
      legend.position = "top", 
      legend.text = element_text(family = "Akkurat Pro",
                                 size = 9)
    ) 

  
  ### GETTING US VLINE FOR STRESS VARIABLES 
    us <- df %>% 
      filter(state == "USA") %>% 
      rename(see_this = desired_variable) %>% 
      group_by(week, wave, race, see_this) %>% 
      summarize(people = sum(people)) %>% 
      na.omit(see_this) 
    
    us <- us %>% 
      group_by(week, wave, see_this) %>% 
      summarize(people = sum(people)) %>% 
      mutate(race = "Overall") %>% 
      rbind(us) %>% 
      pivot_wider(names_from = see_this, values_from = people) 
    
    datable <- us %>% 
      mutate(
        several_days = ifelse(is.na(several_days), 0, several_days), 
        not = ifelse(is.na(not), 0, not), 
        almost_every_day = ifelse(is.na(almost_every_day), 0, almost_every_day), 
        most_days = ifelse(is.na(most_days), 0, most_days), 
        perc_several = 100 * several_days/(several_days + not + almost_every_day + most_days), 
        perc_not = 100 * not/(several_days + not + almost_every_day + most_days), 
        perc_almost = 100 *almost_every_day/(several_days + not + almost_every_day + most_days), 
        perc_most = 100 *most_days/(several_days + not + almost_every_day + most_days), 
        perc_not = ifelse(is.na(perc_not) | perc_not == 0, 
                          100 - (perc_several+ perc_almost + perc_most), 
                          perc_not)) %>% 
      select(week, wave, race, perc_several, perc_not, perc_almost, perc_most) %>% 
      pivot_longer(cols = 4:7, names_to = "stat", values_to = "perc") %>% 
      mutate(
        stat = case_when(
          stat == "perc_several" ~ "Several Days",
          stat == "perc_not" ~ "Never", 
          stat == "perc_almost" ~ "Nearly Every Day", 
          stat == "perc_most" ~ "More than Half the Days"
        ), 
        stat = factor(stat, levels = c("Never", "Several Days", "More than Half the Days", "Nearly Every Day"))) %>% 
      filter(race == "Overall", 
             stat %in% input_levels)  
    
    val <- datable %>% 
      group_by(week, wave) %>% 
      summarize(rate = sum(perc)) %>% 
      pluck("rate") %>% 
      mean()
    
    
    ### STATE VARIABLES 
    state <- df %>% 
      filter(state == input_state2) %>% 
      rename(see_this = desired_variable) %>% 
      group_by(week, wave, race, see_this) %>% 
      summarize(people = sum(people)) %>% 
      na.omit(see_this) 
    
    state <- state %>% 
      group_by(week, wave, see_this) %>%
      summarize(people = sum(people)) %>% 
      mutate(race = "Overall") %>% 
      rbind(state) %>% 
      pivot_wider(names_from = see_this, values_from = people) 
    
    datable_state <- state %>% 
      mutate(
        several_days = ifelse(is.na(several_days), 0, several_days), 
        not = ifelse(is.na(not), 0, not), 
        almost_every_day = ifelse(is.na(almost_every_day), 0, almost_every_day), 
        most_days = ifelse(is.na(most_days), 0, most_days), 
        perc_several = 100 * several_days/(several_days + not + almost_every_day + most_days), 
        perc_not = 100 * not/(several_days + not + almost_every_day + most_days), 
        perc_almost = 100 *almost_every_day/(several_days + not + almost_every_day + most_days), 
        perc_most = 100 *most_days/(several_days + not + almost_every_day + most_days), 
        perc_not = ifelse(is.na(perc_not) | perc_not == 0, 
                          100 - (perc_several+ perc_almost + perc_most), 
                          perc_not)) %>% 
      select(week, wave, race, perc_several, perc_not, perc_almost, perc_most) %>% 
      pivot_longer(cols = 4:7, names_to = "stat", values_to = "perc") %>% 
      mutate(
        stat = case_when(
          stat == "perc_several" ~ "Several Days",
          stat == "perc_not" ~ "Never", 
          stat == "perc_almost" ~ "Nearly Every Day", 
          stat == "perc_most" ~ "More than Half the Days"
        ), 
        stat = factor(stat, levels = c("Never", "Several Days", "More than Half the Days", "Nearly Every Day"))) %>% 
      filter(race == "Overall", 
             stat %in% input_levels)  
    
    val_state <- datable_state %>% 
      group_by(week, wave) %>% 
      summarize(rate = sum(perc)) %>% 
      pluck("rate") %>% 
      mean()
    
    label_df <- data.frame(
      xs = c(max(datable$week) + 2, max(datable_state$week) + 2), 
      vals = c(val, val_state), 
      labs = c("US\nAverage", 
               paste0(ifelse(input_state2 == "USA",
                             "US", 
                             input_state2), 
                      "\nAverage"))) %>% 
      unique()
    
    ## Adding to the Plot 
    plot <- plot + 
      geom_hline(data = label_df, aes(yintercept = vals), alpha = 0.5, size = 0.25) + 
      geom_text(data = label_df, aes(x = xs + 0.5, 
                                     y = ifelse(vals == max(vals), 
                                                vals + 5, 
                                                vals - 5), 
                                     label = labs), 
                      alpha = 0.7, fill = "white", size = 3, 
                      direction = "y")
  
  
    ggplotly(plot, tooltip = "text") %>% 
      config(modeBarButtonsToRemove = c(
        "pan2d", "lasso2d", "zoom2d", 
        "toggleSpikelines", "hoverCompareCartesian", 
        "hoverClosestCartesian", 
        "zoomIn2d", "zoomOut2d", 
        "autoScale2d" # "resetScale2d" 
      ))  %>% 
      layout(
        legend = list(
          orientation = "h"
        ), 
        autosize = T, 
        margin = list(l = 0),
        height = 400
      )
}



