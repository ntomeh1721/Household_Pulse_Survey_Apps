# Loading Libraries 
library(tidyverse)
library(ggrepel)
library(plotly)


# Data 
housing_data <- read.csv("Household_Pulse/data/housing.csv") %>% 
  mutate(housing_threat = case_when(
    is.na(eviction) & !is.na(foreclosure) ~ foreclosure, 
    !is.na(eviction) & is.na(foreclosure) ~ eviction
  ))

# Getting rid of scientific notation 
options(scipen = 999)

# Week Label
week_labels <- c("","5/5", "5/12", "5/19", "5/26", 
                 "6/2", "6/9", "6/16", "6/23", 
                 "6/30", "7/7", "7/14", "7/21", 
                 "7/28", "8/4", "8/11", "8/18", 
                 "8/25", "8/31", "9/7", "9/14",
                 "9/21", "9/28", "10/5", "10/12", 
                 "10/19", "10/26", "11/2", "11/9", 
                 "11/16", "11/23")

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

## Setting Inputs 
# input_state <- "USA"
# input_state2 <- "USA"
# input_kids <- "All"
# input_kids2 <- "All"
# specified_race <- "Overall"
# responses <- c("Somewhat Likely", "Very Likely", "Extremely Likely")
# responses2 <- c("Somewhat Likely", "Very Likely", "Extremely Likely")
# home_status <- c("Renters", "Homeowners")
# home_status2 <- c("Renters", "Homeowners")
# desired_variable <- "housing_threat"
# desired_variable2 <- "housing_threat"




### Stacker Data 
stacker_data <- function(input_state, input_kids, desired_variable, specified_race, responses, home_status){
  
  
  if(desired_variable %notin% c("housing_confidence", "housing_paid", 
                               "housing_threat")){
    df <- data
    
  } else {
    
    if(length(home_status) == 1){
      
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
             stat %in% responses)
    
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
    # stat != "High")
    
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

### Stacker 
stacker <- function(input_state2, input_kids2, desired_variable2, specified_race2, responses2, home_status2){
  
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
    scale_x_continuous(breaks = seq(0, max(graphable$week) + 1, 1)[c(FALSE, FALSE, TRUE, FALSE)],
                       labels = c(week_labels, "")[c(FALSE, FALSE, TRUE, FALSE)],
                       limits = c(min(graphable$week) - 0.5, max(graphable$week) + 1)) + 
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


### Stacker Head 

stacker_head <- function(input_state, input_kids, desired_variable){
  
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
             stat != "Very Confident")
    
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
    scale_x_continuous(breaks = seq(0, max(graphable$week) + 1, 1)[c(TRUE, FALSE, FALSE, FALSE)],
                       labels = c(week_labels, "")[c(TRUE, FALSE, FALSE, FALSE)]) +
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



# Testers
# stacker_data("USA", "All", "housing_threat", "Overall", c("Somewhat Likely", "Very Likely", "Extremely Likely"), c("Renters", "Homeowners"))
stacker("USA", "All", "housing_threat", "Overall", c("Somewhat Likely", "Very Likely", "Extremely Likely"),
        c("Renters", "Homeowners"))
# stacker_head("USA", "All", "housing_threat")
