
# To Update: Add week end dates to the week_labels vector up to the last date of the most recent . 


# Set Up  -----------------------------------------------------------------

# libraries
library(tidyverse)
library(ggrepel)
library(plotly)

# data 
all_states <- read_csv("data_prepped/Aggregate.csv") %>% 
  mutate(wave = ifelse(wave == "third", "second", wave),
         week = ifelse(week > 12, week + 2, week))


# Updating Weeks 

num_weeks <- all_states$week %>% max() 
week_labels <- c("5/5", "5/12", "5/19", "5/26", "6/2", 
                 "6/9", "6/16", "6/23", "6/30", "7/07", 
                 "7/14",  "7/21", "7/28", "8/4", "8/11", 
                 "8/18", "8/25", "8/31", "9/7", "9/14", 
                 "9/21", "9/28", "10/5", "10/12", "10/19",
                 "10/26", "11/2", "11/9", "11/16", "11/23", 
                 "11/30", "12/7", "12/14", "12/21", "12/28",
                 "1/4", "1/11", "1/18", "1/25", "2/1", 
                 "2/8", "2/15", "2/22", "3/1", "3/8", 
                 "3/15", "3/22", "3/29", "4/5", "4/12", 
                 "4/19", "4/26", "5/3", "5/10", "5/17",
                 "5/24","5/31","6/7", "6/14","6/21",  
                 "6/28", "7/5", "7/12", "7/19", "7/26",
                 "8/2", "8/9", "8/16", "8/23", "8/30"
                 )

# Not In function 
`%notin%` <- negate(`%in%`)




# Warning Function  --------------------------------------------------------

warning <- function(regions){
  
  regions_vector <- head(regions, 3)
  
  df <- all_states %>% 
    filter(state %in% regions & state %notin% regions_vector) %>% 
    select(state)
  
  # Getting a unique vector of the states
  states_vector <- df$state %>% unique()
  
  
  if(length(states_vector) > 0) {
    
    text_output <- ""
    
    states_vector_not_showing <- for(i in 1:length(states_vector)){
      text_output <- paste0(text_output,
                            ifelse(i == length(states_vector) & i != 1, " and ", ""),
                            states_vector[i], 
                            ifelse(i == length(states_vector), ".", 
                                   ifelse(i == length(states_vector) - 1 & i == 1, " ",
                                          ", ")))
    }
    
    
    final_warning <- paste0("Warning: Can compare a maximum of 3 states. ",
                            length(states_vector) + 3, 
                            " states chosen. Not showing ",
                            text_output)
    
    
  } else {
    
    final_warning <- NULL
  }
  
  final_warning
  
}    




# Filtering Data Based on Household Type ----------------------------------

choose_data <- function(household) {
  
  # Filtering Based on Household Types
  data_one <- all_states %>% 
    filter(category %in% c("Total", "Hispanic origin and Race"),
           household_type == ifelse(household == "All", "all_households", "with_children")) %>% 
    select(characteristic, week, state, 
           total, sum, perc_insecure, 
           num_insecure, wave)
  
  # Getting the totals 
  data_two <- data_one %>% 
    filter(characteristic == "Total") %>% 
    rename(sum_total = sum, 
           num_insecure_total = num_insecure) %>% 
    select(-characteristic, -perc_insecure, -total)
  
  # Adding the totals as part of household type data 
  ## This is the data with the original percentage of the population by week, state, and characteristic
  data_three <- left_join(data_one, data_two) %>% 
    mutate(perc_of_pop = 100*sum /sum_total) %>% 
    select(week, state, characteristic, perc_of_pop, 
           sum_total, num_insecure_total, sum, perc_insecure, 
           num_insecure, wave) 
  
  # Data Ready to be graphed, suppressed by percentage of the population 
  data_four <- data_three %>% 
    mutate(characteristic = ifelse(perc_of_pop < 3, 
                                   "Two or more races + Other races, not Hispanic", 
                                   characteristic)) %>% 
    group_by(characteristic, week, state) %>% 
    summarize(sum = sum(sum), 
              num_insecure = sum(num_insecure)) %>% 
    mutate(perc_insecure = 100 * num_insecure / sum) %>% 
    merge(data_two, by = c("week", "state"), all = T) %>% 
    mutate(perc_of_pop = 100*sum/sum_total)
  
  data_four
  
}


# Averages Data Table -----------------------------------------------------

show_averages <- function(regions, races, household_type) {
  
  choose_data(household_type) %>% 
    filter(state %in% c("USA", head(regions, 3)),
           characteristic %in% c("Total", races), 
           week == num_weeks) %>% 
    mutate(characteristic = case_when(
      characteristic == "Asian alone, not Hispanic" ~ "Asian", 
      characteristic == "Black alone, not Hispanic" ~ "Black",
      characteristic == "Hispanic or Latino (may be of any race)" ~ "Hispanic or Latino",
      characteristic == "Two or more races + Other races, not Hispanic" ~ "2+ or Other",
      characteristic == "White alone, not Hispanic" ~ "White", 
      characteristic == "Total" ~ "All"
    )) %>% 
    mutate("Current % Insecure" = perc_insecure %>% round(1) %>% format(nsmall = 1)) %>% 
    rename(State = state, 
           `Race/ Ethnicity` = characteristic) %>% 
    select(State, `Race/ Ethnicity`, `Current % Insecure`)
  
}

# Summary Plot  -----------------------------------------------------------


summary_graph <- function(region_vector, house_types){
  
  # Filtering Based on Household Type Specification 
  all_states_data <- choose_data(house_types)
  
  # Selecting only first three of regions 
  regions <- head(region_vector, 3)
  
  # Getting the data for the US Line 
  us_dt <- all_states_data %>% 
    filter(characteristic == "Total",
           state == "USA") 
  
  # Getting the data for the selected states lines 
  dt1 <- all_states_data %>% 
    filter(characteristic == "Total",
           state %in% regions)
  
  # Getting the data for the non selected states 
  dt2 <- all_states_data %>% 
    filter(characteristic == "Total",
           state %notin% regions) 
  
  # Getting the labels for the lines on the graph 
  text_dt <- dt1 %>% 
    unique() %>% 
    filter(week == num_weeks)
  
  # Getting Plot A 
  ggplot() + 
    # Selected States 
    geom_point(data = dt1, 
               aes(x = week, y = perc_insecure, color = state), 
               size = 0.9,
               alpha = 1, 
               show.legend = F) + 
    geom_line(data = dt1, 
              aes(x = week, y = perc_insecure, color = state,
                  group = interaction(state, wave)), 
              alpha = 1, 
              size = 0.9,
              show.legend = F) + 
    # Setting the colors for the selected states 
    scale_color_manual(values = c("#4E2A84", "#7FCECD", "#EF553F")) + 
    # Not Selected States 
    geom_line(data = dt2, 
              aes(x = week, y = perc_insecure, 
                  group = interaction(state, wave)), 
              size = 0.2,
              alpha = 0.75, 
              show.legend = F, 
              color = "#BBB8B8") + 
    # US Plot
    geom_point(data = us_dt,
               aes(x = week, y = perc_insecure),
               alpha = ifelse("USA" %in% regions, 1, 0.7),
               size = ifelse("USA" %in% regions, 0.9, 0),
               color = ifelse("USA" %in% regions, "#342F2E", "#716C6B"),
               show.legend = F) +
    geom_line(data = us_dt, 
              aes(x = week, y = perc_insecure, 
                  group = interaction(state, wave)), 
              alpha = ifelse("USA" %in% regions, 1, 0.7),
              color = ifelse("USA" %in% regions, "#342F2E", "#716C6B"), 
              size = ifelse("USA" %in% regions, 0.9, 0.7), 
              show.legend = F) + 
    # Plotting Text 
    geom_label_repel(data = text_dt, 
                     aes(x = week, y = perc_insecure, 
                         label = state),
                     nudge_x = 0.5, 
                     label.size = 0, 
                     alpha = 0.8,
                     size = 4,
                     direction = "y", 
                     segment.alpha = 0,
                     family = "Akkurat Pro") + 
    # Graph Specifications 
    scale_x_continuous(breaks = seq(1, num_weeks, 1)[c(FALSE, TRUE, FALSE, FALSE)],
                       labels = week_labels[c(FALSE, TRUE, FALSE, FALSE)],
                       limits = c(1, num_weeks + 1)) +
    scale_y_continuous(breaks = seq(0, 100, 5),
                       labels = paste0(seq(0, 100, 5), "%"),
                       limits = c(0, max(all_states_data %>% 
                                           filter(characteristic == "Total") %>%
                                           select(perc_insecure)))) + 
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

# Splitting by Race Line Graph  ------------------------------------------------



race_line <- function(regions, races, house_types){
  
  # Selecting only first three of regions 
  regions <- head(regions, 3)
  
  # Filtering Based on Household Type Specification 
  all_states_data <- choose_data(house_types) %>% 
    filter(state %in% regions)
  
  # US Data, Selected 
  us_dt_selected <- all_states_data %>% 
    filter(state == "USA",
           characteristic %in% races) 
  
  
  # State Data, Selected  
  dt1 <- all_states_data %>% 
    filter(characteristic %in% races)
  
  # State Data, Not Selected 
  dt2 <- all_states_data %>% 
    filter(characteristic %notin% races, 
           characteristic != "Total")
  
  
  # Text Data 
  text_dt <- dt1 %>% 
    filter(week == num_weeks) %>% 
    mutate(text_label = case_when(
      characteristic == "Asian alone, not Hispanic" ~ "Asian", 
      characteristic == "Black alone, not Hispanic" ~ "Black",
      characteristic == "Hispanic or Latino (may be of any race)" ~ "Hispanic\nor Latino",
      characteristic == "Two or more races + Other races, not Hispanic" ~ "2+ or Other",
      characteristic == "White alone, not Hispanic" ~ "White"
    )) 
  
  
  
  ## Building the graph
  
  ggplot() +
    # Selected Races
    geom_point(data = dt1,
               aes(x = week, y = perc_insecure,
                   color = state), show.legend = F) +
    geom_line(data = dt1,
              aes(x = week, y = perc_insecure,
                  color = state, linetype = characteristic,
                  group = interaction(characteristic, state, wave)),
              show.legend = F, size = 1.1 ) +
    # Selected Races Manual Color Set
    scale_color_manual(values = c("#4E2A84", "#7FCECD", "#EF553F")) +
    # US
    geom_point(data = us_dt_selected,
               aes(x = week, y = perc_insecure),
               color = "#342F2E", show.legend = F) +
    geom_line(data = us_dt_selected,
              aes(x = week, y = perc_insecure, linetype = characteristic, 
                  group = interaction(characteristic, state, wave)),
              color = "#342F2E", show.legend = F, size = 1.1) +
    # Not Selected Races
    geom_line(data = dt2,
              aes(x = week, y = perc_insecure, 
                  group = interaction(characteristic, state, wave)),
              size = 0.2,
              alpha = 0.75,
              show.legend = F,
              color = "#BBB8B8") +
    # Text
    geom_label_repel(data = text_dt,
                     aes(x = week, y = perc_insecure,
                         label = text_label),
                     size = 4,
                     alpha = 0.8, 
                     label.size = 0, 
                     segment.alpha = 0,
                     family = "Akkurat Pro") +
    # Graph Specifications
    scale_x_continuous(breaks = seq(1, num_weeks, 1)[c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE)],
                       labels = week_labels[c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE)],
                       limits = c(1, num_weeks + 0.99)) +
    scale_y_continuous(breaks = seq(0, 100, 5),
                       labels = paste0(seq(0, 100, 5), "%"), 
                       limits = c(0, max(all_states_data %>% 
                                           filter(characteristic != "Total") %>%
                                           select(perc_insecure)))) +
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
    ) + 
    facet_wrap(~state) 
  
}


# Stacking Race, Bar Graph  -----------------------------------------------

race_bar_data <- function(regions, races, house_types){
  
  ### DATA EDITING 
  
  regions <- head(regions, 3)
  
  # Selecting only the useful data 
  choose_data(house_types) %>% 
    # Only states that were chosen
    filter(state %in% regions,
           characteristic != "Total") %>% 
    # Only Races that  were chosen 
    mutate(characteristic = ifelse(characteristic %in% races | characteristic == "Total",
                                   characteristic, 
                                   "Other"), 
           characteristic = case_when(
             characteristic == "Asian alone, not Hispanic" ~ "Asian", 
             characteristic == "Black alone, not Hispanic" ~ "Black",
             characteristic == "Hispanic or Latino (may be of any race)" ~ "Hispanic or Latino",
             characteristic == "Two or more races + Other races, not Hispanic" ~ "2+ or Other",
             characteristic == "White alone, not Hispanic" ~ "White", 
             characteristic == "Other" ~ "All Other"
           ) %>% as.factor(), 
           characteristic = fct_relevel(characteristic, "Other")) %>% 
    group_by(characteristic, week, state, sum_total, num_insecure_total) %>% 
    summarize(sum = sum(sum), 
              num_insecure = sum(num_insecure)) %>% 
    mutate(
      # Percent of the total respondents who are food insecure and from the given group 
      ## This will be what's stacked and mapped into graph
      perc_insecure_and_race = 100 * num_insecure/sum_total, 
      # Percent of the total number of food insecure respondents who are of this group 
      perc_of_insecure = 100*num_insecure/num_insecure_total, 
      # Percent of the population of each group 
      perc_of_pop = 100*sum / sum_total) 
  
}


race_bar <- function(regions, races, house_types) {
  
  ## Data Editing 
  first_data <- race_bar_data(regions, races, house_types)


  ### GRAPHING 
  
  bar <- ggplot(data = first_data, aes(x = week, y = perc_insecure_and_race)) + 
    geom_bar(aes(width = ifelse(week > 13, 1.8, 0.9), 
                 text = paste0(characteristic, " Respondents:\n", 
                        perc_of_insecure %>% round(1) %>% format(nsmall = 1), 
                        "% of Food Insecure\n", 
                        perc_of_pop %>% round(1) %>% format(nsmall = 1), 
                        "% of Population"),
                 fill = characteristic), 
             stat = "identity", position = "stack")  +
    scale_x_continuous(breaks = seq(1, num_weeks, 1)[c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE)],
                       labels = week_labels[c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE)]) +
    scale_y_continuous(breaks = seq(0, 100, 5),
                       labels = paste0(seq(0, 100, 5), "%")) +
    scale_fill_manual(values = case_when(
      length(races) == 5 ~ c("#007FA4", "#836EAA", "#B6ACD1", "#E4E0EE","#401F68"),
      TRUE ~ c("#D8D6D6", "#836EAA", "#B6ACD1", "#E4E0EE","#401F68")
    )) +
    scale_color_manual(values = c(NA, "#342F2E"),
                       guide = F) + 
    theme_classic() +
    labs(
      x = NULL,
      y = NULL,
      fill = NULL
    ) +
    theme(
      axis.text = element_text(family = "Akkurat Pro",
                               size = 11), 
      strip.text = element_text(family = "Akkurat Pro",
                                size = 12),
      legend.text = element_text(family = "Akkurat Pro", 
                                 size = 10)
    ) + 
    facet_wrap(~state)
  
  ggplotly(bar, tooltip = "text") %>% 
    config(modeBarButtonsToRemove = c(
      "pan2d", "lasso2d", "zoom2d", 
      "toggleSpikelines", "hoverCompareCartesian", 
      "hoverClosestCartesian", 
       "zoomIn2d", "zoomOut2d", 
      "autoScale2d" # "resetScale2d" 
    )) %>% 
    layout(showlegend = FALSE)
  
}

# Points on the Line, Summary ---------------------------------------------

points_on_the_line <- function(household, regions){
  
  
  data_one <- choose_data(household) %>% 
    filter(characteristic == "Total", 
           state %notin% head(regions, 3) & state != "USA") %>% 
    select(week, state, perc_insecure) %>% 
    arrange(state) %>% 
    mutate(next_week_perc_insecure = ifelse(week == max(week), NA, lead(perc_insecure)), 
           slope = (next_week_perc_insecure - perc_insecure))
  
  for(i in 1:3) {
    
    data_one <- rbind(data_one, data_one)
    
  }
  
  
  data_one %>% 
    arrange(state, week) %>% 
    filter(week <= max(data_one$week)) %>% 
    mutate(num = rep_len(0:7, length.out = nrow(data_one)),
           num = ifelse(num == 0, 0, num*1/8), 
           perc_insecure = perc_insecure + slope*num,
           week = week + num) %>% 
    arrange(state, week, num) %>% 
    select(week, state, perc_insecure)
  
}








