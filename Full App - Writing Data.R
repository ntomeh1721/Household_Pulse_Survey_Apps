# 
# This file downloads and reads the HPS micro-data and preps it to be 
# used in the Full App which includes data on food 
# insecurity, expectations, employment, finances, housing, mental 
# health, among others. 
# 
# To update this file with new weeks of data:
## 
## 1. Set `num_weeks` below to the number of weeks of data (as 
##    according to HPS). 
num_weeks <- 36
## 2. Cntrl + A then Cntrl + Return to run all the code. This will take 10+ minutes. 
## 3. Check the console at the end to see if there was an error. 
## 
## Note: If HPS took another hiatus between survey weeks, then 
### Cntrl + F and search for "NOTE". At that location, adjust the 
### "week" conversion to account for the hiatus. The point 
### of the "week" variable conversion is to have appropriate spacing along the 
### x axis in the final graphs. The new week number is the counted number 
### of calendar weeks since the first survey release.  
### 

# Set Up ------------------------------------------------------------------

# Libraries 
library(tidyverse)
library(janitor) 
library(readxl)

# Removing Scientific Notation 
options(scipen = 999)

# State Naming Doc 
guide <- read_xlsx("data/guides/State Names w PEBT.xlsx") %>% clean_names()

# Loading Data --------------------------------------------------------

data <- {} 

for(i in 1:num_weeks){
  
  #### Downloading the Data #### 
  
  url_name <- paste0(
    "https://www2.census.gov/programs-surveys/demo/datasets/hhp/",
    ifelse(i < 22, "2020", "2021"), 
    "/wk",
    i,
    "/HPS_Week",
    ifelse(i < 10, paste0("0", i), i), 
    "_PUF_CSV.zip"
  )
    
  file_loc <- "data/downloaded.zip"
  
  file_name <- paste0("pulse", ifelse(i < 22, "2020", "2021"),
                      "_puf_", ifelse(i < 10, paste0("0", i), i),
                      ".csv")
  
  download.file(url_name, file_loc)
  
  week <- read_csv(unz(file_loc, file_name)) %>% 
    clean_names()
  
  file.remove("data/downloaded.zip")
  
  
  #### 
  
  
  #### Prepping Data #### 
  
  if(i %in% 1:6){
    
    week <- week %>% 
      select(rhispanic, rrace, thhld_numkid, 
             wrkloss, expctloss, anywork,  
             curfoodsuf, foodconf,
             tenure, mortlmth, mortconf,
             compavail, intrntavail, est_st,
             anxious, worry, down, interest, 
             pweight, week) %>% 
      mutate(
        wave = "first", 
        spndsrc1 = 0, spndsrc2 = 0, spndsrc3 = 0, 
        spndsrc4 = 0, spndsrc5 = 0, spndsrc6 = 0, 
        spndsrc7 = 0, 
        ui_apply = 0, ui_recv = 0, expns_dif = 0,
        spndsrc8 = 0, snap_yn = 0, 
        snapmnth1 = 0, snapmnth2 = 0, snapmnth3 = 0, 
        snapmnth4 = 0, snapmnth5 = 0, snapmnth6 = 0, 
        snapmnth7 = 0, snapmnth8 = 0, snapmnth9 = 0, 
        snapmnth10 = 0, snapmnth11 = 0, snapmnth12 = 0, 
        mh_notget = 0, rentcur = 0, mortcur = 0, 
        evict = 0,forclose = 0
      )
    
  }
  
  if(i %in% 7:12){
    
    week <- week %>% 
      select(rhispanic, rrace, thhld_numkid,
             wrkloss, expctloss, anywork,  
             curfoodsuf, foodconf,
             tenure, mortlmth, mortconf,
             compavail, intrntavail, est_st,
             pweight, week, spndsrc1, 
             anxious, worry, down, interest, 
             spndsrc2, spndsrc3, spndsrc4, 
             spndsrc5, spndsrc6, spndsrc7
      ) %>% 
      mutate(
        wave = "first", 
        ui_apply = 0, ui_recv = 0, expns_dif = 0,
        spndsrc8 = 0, snap_yn = 0, 
        snapmnth1 = 0, snapmnth2 = 0, snapmnth3 = 0, 
        snapmnth4 = 0, snapmnth5 = 0, snapmnth6 = 0, 
        snapmnth7 = 0, snapmnth8 = 0, snapmnth9 = 0, 
        snapmnth10 = 0, snapmnth11 = 0, snapmnth12 = 0, 
        mh_notget = 0, rentcur = 0, mortcur = 0, evict = 0, 
        forclose = 0 
      )
    
  }
  
  if(i %in% 13:21) {
    
    week <- week %>% 
      select(
        # Same as above 
        rhispanic, rrace, thhld_numkid, 
        wrkloss, expctloss, anywork, 
        curfoodsuf, foodconf, 
        tenure, mortconf, # mortlmth missing 
        compavail, intrntavail, 
        anxious, worry, down, interest,
        spndsrc1, spndsrc2, spndsrc3, spndsrc4, 
        spndsrc5, spndsrc6, spndsrc7, 
        est_st, pweight, week, 
        
        # New 
        ui_apply, ui_recv, expns_dif, 
        spndsrc8, snap_yn, 
        snapmnth1, snapmnth2, snapmnth3,
        snapmnth4, snapmnth5, snapmnth6,
        snapmnth7, snapmnth8, snapmnth9,
        snapmnth10, snapmnth11, snapmnth12,
        mh_notget, rentcur, mortcur, 
        evict, forclose
      ) %>% 
      mutate(mortlmth = 0,
             wave = "second")
    
  }
  
  if(i %in% 21:27) {
    
    week <- week %>% 
      select(
        # Same as above 
        rhispanic, rrace, thhld_numkid, 
        wrkloss, expctloss, anywork, 
        curfoodsuf, 
        tenure, mortconf, # mortlmth missing 
        compavail, intrntavail, 
        anxious, worry, down, interest,
        spndsrc1, spndsrc2, spndsrc3, spndsrc4, 
        spndsrc5, spndsrc6, spndsrc7, 
        est_st, pweight, week, 
        
        # New 
        ui_apply, ui_recv, expns_dif, 
        spndsrc8, snap_yn, 
        mh_notget, rentcur, mortcur, 
        evict, forclose
      ) %>% 
      mutate(mortlmth = 0,
             wave = "second",
             foodconf = 0, 
             snapmnth1 = 0, snapmnth2 = 0, 
             snapmnth3 = 0, 
             snapmnth4= 0, snapmnth5= 0, snapmnth6= 0,
             snapmnth7= 0, snapmnth8= 0, snapmnth9 = 0,
             snapmnth10 = 0,  snapmnth11 = 0, snapmnth12 = 0)
    
  }
  
  if(i %in% 27:33){
    
    week <- week %>% 
      select(
        rhispanic, rrace, thhld_numkid, 
        expctloss, anywork, 
        curfoodsuf, 
        tenure, mortconf,
        compavail, intrntavail, 
        anxious, worry, down, interest,
        spndsrc1, spndsrc2, spndsrc3, spndsrc4, 
        spndsrc5, spndsrc6, spndsrc7, 
        est_st, pweight, week, expns_dif, 
        spndsrc8, snap_yn, 
        mh_notget, rentcur, mortcur, 
        evict, forclose
      ) %>% 
      mutate(
        
        wrkloss = 0, 
        ui_apply = 0, ui_recv = 0, 
        mortlmth = 0,
        wave = "second",
        foodconf = 0, 
        snapmnth1 = 0, snapmnth2 = 0, snapmnth3 = 0,
        snapmnth4 = 0, snapmnth5= 0, snapmnth6= 0,
        snapmnth7 = 0, snapmnth8= 0, snapmnth9 = 0,
        snapmnth10 = 0,  snapmnth11 = 0, snapmnth12 = 0
        
      )
    
  }
  
  if(i > 33){
    
    week <- week %>% 
      select(
        
        rhispanic, rrace, thhld_numkid, 
        anywork, curfoodsuf, tenure, mortconf,
        anxious, worry, down, interest,
        est_st, pweight, week, expns_dif, 
        snap_yn, mh_notget, rentcur, mortcur, 
        evict, forclose, 
        
      ) %>% 
      mutate(
        
        wrkloss = 0, expctloss = 0, 
        ui_apply = 0, ui_recv = 0, 
        mortlmth = 0, compavail = 0, intrntavail = 0, 
        wave = "second",
        foodconf = 0, 
        snapmnth1 = 0, snapmnth2 = 0, snapmnth3 = 0,
        snapmnth4 = 0, snapmnth5 = 0, snapmnth6 = 0,
        snapmnth7 = 0, snapmnth8 = 0, snapmnth9 = 0,
        snapmnth10 = 0,  snapmnth11 = 0, snapmnth12 = 0,
        spndsrc1 = 0, spndsrc2 = 0, spndsrc3 = 0, spndsrc4 = 0, 
        spndsrc5 = 0, spndsrc6 = 0, spndsrc7 = 0, spndsrc8 = 0
        
      )
    
  }
  
  #### 
  
  
  #### Combining the data for each week into one dataset
  data <- rbind(week, data)
  
}

# Editing  -----------------------------------------------------

## State Names
st_names <- guide %>%
  select(states) 

st_nums <- data %>% 
  arrange(est_st) %>% 
  select(est_st) %>% 
  unique() 

state_ids <- cbind(st_names, st_nums)

data <- left_join(data, state_ids, by = "est_st") %>% 
  select(-est_st) %>% 
  rename(state = states)

## Other Variables 

data <- data %>% 
  mutate( 
    # Week 
    week = case_when(
      week < 13 ~ week, 
      week %in% 13:21 ~ 2*(week - 4), 
      week %in% 22:27 ~ 2*(week - 3),
      week %in% 28:33 ~ 2*(week - 2),
      week > 33 ~ 2*(week - 1)  
      # NOTE 
      # e.g. If there's a week between the end of week 35 and start of 36
      # change the line above to  
      ## week %in% 33:35 ~ 2*(week - 1), 
      # and add the line below 
      ## week >= 36 ~ 2*(week - 1) + 1
    ), 
    # Race
    race = case_when(
      rhispanic == 2 ~ "Hispanic or Latino, all Races",
      rhispanic == 1 & rrace == 1 ~ "White alone, not Hispanic", 
      rhispanic == 1 & rrace == 2 ~ "Black alone, not Hispanic",
      rhispanic == 1 & rrace == 3 ~ "Asian alone, not Hispanic", 
      TRUE ~ "Two or more races or Other"
    ), 
    # Kids 
    kids = ifelse(
      thhld_numkid > 0,
      "yes",
      "no"
    ),
    # Has someone in the household lost work within the past 7 days 
    lost_work = case_when(
      wrkloss == 1 ~ "yes", 
      wrkloss == 2 ~ "no"
    ), 
    # Expected Household Job Loss
    expect_loss = case_when(
      expctloss == 1 ~ "yes", 
      expctloss == 2 ~ "no"
    ), 
    # Have you worked for pay or profit within the past 7 days 
    employed = case_when(
      anywork == 1 ~ "yes", 
      anywork == 2 ~ "no"
    ), 
    # Food Sufficiency in the Past 7 Days 
    food_sufficiency = case_when(
      curfoodsuf == 1 ~ "enough wanted", 
      curfoodsuf == 2~ "enough not wanted", 
      curfoodsuf == 3 ~ "sometimes not enough", 
      curfoodsuf == 4 ~ "often not enough"
    ), 
    # Confidence in food sufficiency in the next four weeks 
    food_suf_confidence = case_when(
      foodconf == 1 ~ "not at all", 
      foodconf == 2 ~ "somewhat", 
      foodconf == 3 ~ "moderately", 
      foodconf == 4 ~ "very"
    ), 
    # How is Housing Paid for 
    housing = case_when(
      tenure == 1 ~ "owned", 
      tenure == 2 ~ "owned, mortgage or loan", 
      tenure == 3 ~ "rented", 
      tenure == 4 ~ "occupied w/o rent"
    ),
    # Has your housing been paid in the past month (wave 1) / 
    #   Are you caught up on rent/mortage? (wave 2) 
    housing_paid = case_when(
      mortlmth == 1 | rentcur == 1 | mortcur == 1 ~ "yes", 
      mortlmth == 2 | rentcur == 2 | mortcur == 2 ~ "no", 
      mortlmth == 3 ~ "deferred"
    ), 
    # Confidence in ability to pay mortgage or rent next month
    housing_confidence = case_when(
      mortconf == 1 ~ "no confidence", 
      mortconf == 2 ~ "slight confidence",
      mortconf == 3 ~ "moderate confidence", 
      mortconf == 4 ~ "high confidence", 
      mortconf == 5 ~ "will be deferred"
    ),
    # Likelihood of Eviction or foreclosure in the next two months 
    eviction = case_when(
      evict == 1 ~ "extrememly", 
      evict == 2 ~ "very", 
      evict == 3 ~ "somewhat", 
      evict == 4 ~ "not_likely"
    ),
    foreclosure = case_when(
      forclose == 1 ~ "extrememly", 
      forclose == 2 ~ "very", 
      forclose == 3 ~ "somewhat", 
      forclose == 4 ~ "not_likely"
    ), 
    # Computer available for child's enrollment 
    comp_available = case_when(
      compavail == 1 ~ "always", 
      compavail == 2 ~ "usually", 
      compavail == 3 ~ "sometimes", 
      compavail == 4 ~ "rarely", 
      compavail == 5 ~ "never"
    ), 
    # Internet availability for education 
    wifi_avail = case_when(
      intrntavail == 1 ~ "always", 
      intrntavail == 2 ~ "usually", 
      intrntavail == 3 ~ "sometimes", 
      intrntavail == 4 ~ "rarely", 
      intrntavail == 5 ~ "never"
    ),
    # Money Used to Meet Needs 
    expenses_difficulty = case_when(
      expns_dif == 1 ~ "not", 
      expns_dif == 2 ~ "a_little",
      expns_dif == 3 ~ "somewhat",
      expns_dif == 4 ~ "very"
    ), 
    ### normal income 
    regular_income = ifelse(spndsrc1 == 1, "yes", "no"), 
    ### income from credit card loans 
    credit_card_loans = ifelse(spndsrc2 == 1, "yes", "no"), 
    ### income from savings or selling assets 
    savings_assets = ifelse(spndsrc3 == 1, "yes", "no"),
    ### income from borrow from friends or relatives 
    borrowing_relatives = ifelse(spndsrc4 == 1, "yes", "no"), 
    ### government payments: stimulus or ui 
    ui = ifelse(spndsrc5 == 1, "yes", "no"), 
    stimulus = ifelse(spndsrc6 == 1, "yes", "no"),
    ### deferred paymnets
    deferred_payments = ifelse(spndsrc7 == 1, "yes", "no"), 
    snap_spending_needs = ifelse(spndsrc8 == 1, "yes", "no"),
    # Frequency of anxiety over the past seven days 
    anxious = case_when(
      anxious == 1 ~ "not", 
      anxious == 2 ~ "several_days", 
      anxious == 3 ~ "most_days", 
      anxious == 4 ~ "almost_every_day"
    ), 
    # frequency of worry over the past seven days
    worry = case_when(
      worry == 1 ~ "not", 
      worry == 2 ~ "several_days", 
      worry == 3 ~ "most_days", 
      worry == 4 ~ "almost_every_day"
    ), 
    # frequency of feeling depressed over previous seven days 
    down = case_when(
      down == 1 ~ "not", 
      down == 2 ~ "several_days", 
      down == 3 ~ "most_days", 
      down == 4 ~ "almost_every_day"
    ), 
    interest = case_when(
      interest == 1 ~ "not", 
      interest == 2 ~ "several_days", 
      interest == 3 ~ "most_days", 
      interest == 4 ~ "almost_every_day"
    ), 
    # Unemployment Insurance Applied / Received 
    ui_applied = ifelse(ui_apply == 1, "yes", "no"), 
    ui_received = ifelse(ui_recv == 1, "yes", "no"), 
    # SNAP Rates, by Month 
    snap_yn = ifelse(snap_yn == 1, "yes", "no"),
    snap_jan = ifelse(snapmnth1 == 1, "yes", "no"), 
    snap_feb = ifelse(snapmnth2 == 1, "yes", "no"), 
    snap_mar = ifelse(snapmnth3 == 1, "yes", "no"),
    snap_apr = ifelse(snapmnth4 == 1, "yes", "no"),
    snap_may = ifelse(snapmnth5 == 1, "yes", "no"),
    snap_jun = ifelse(snapmnth6 == 1, "yes", "no"),
    snap_jul = ifelse(snapmnth7 == 1, "yes", "no"),
    snap_aug = ifelse(snapmnth8 == 1, "yes", "no"),
    snap_sep = ifelse(snapmnth9 == 1, "yes", "no"),
    snap_oct = ifelse(snapmnth10 == 1, "yes", "no"),
    snap_nov = ifelse(snapmnth11 == 1, "yes", "no"),
    snap_dec = ifelse(snapmnth12 == 1, "yes", "no"), 
    # Mental Health Services Sought Out 
    ### Needed but not gotten 
    mh_needed = ifelse(mh_notget == 1, "yes", "no"),
  ) %>% 
  select(week, state, pweight, race, kids, wave, 
         lost_work, expect_loss, employed, 
         food_sufficiency, food_suf_confidence,
         housing, housing_paid, housing_confidence, 
         eviction, foreclosure, comp_available, 
         wifi_avail, expenses_difficulty, 
         regular_income, credit_card_loans, 
         savings_assets, borrowing_relatives, 
         ui, stimulus, deferred_payments, 
         snap_spending_needs, anxious,
         worry, down, interest, ui_applied,
         ui_received, snap_yn, snap_jan, 
         snap_feb, snap_mar, snap_apr, 
         snap_may, snap_jun, snap_jul, 
         snap_aug, snap_sep, snap_oct, 
         snap_nov, snap_dec, mh_needed)


### Remove Duplicates 

all <- data %>% 
  group_by(week, state, race, kids, wave, 
           lost_work, expect_loss, employed, 
           food_sufficiency, food_suf_confidence,
           housing, housing_paid, housing_confidence, 
           eviction, foreclosure, comp_available, 
           wifi_avail, expenses_difficulty, 
           regular_income, credit_card_loans, 
           savings_assets, borrowing_relatives, 
           ui, stimulus, deferred_payments, 
           snap_spending_needs, anxious,
           worry, down, interest, ui_applied,
           ui_received, snap_yn, snap_jan, 
           snap_feb, snap_mar, snap_apr, 
           snap_may, snap_jun, snap_jul, 
           snap_aug, snap_sep, snap_oct, 
           snap_nov, snap_dec, mh_needed) %>% 
  summarize(people = sum(pweight))

### Add USA Overall 
usa <- all %>%
  group_by(week, race, kids, wave, 
           lost_work, expect_loss, employed, 
           food_sufficiency, food_suf_confidence,
           housing, housing_paid, housing_confidence, 
           eviction, foreclosure, comp_available, 
           wifi_avail, expenses_difficulty, 
           regular_income, credit_card_loans, 
           savings_assets, borrowing_relatives, 
           ui, stimulus, deferred_payments, 
           snap_spending_needs, anxious,
           worry, down, interest, ui_applied,
           ui_received, snap_yn, snap_jan, 
           snap_feb, snap_mar, snap_apr, 
           snap_may, snap_jun, snap_jul, 
           snap_aug, snap_sep, snap_oct, 
           snap_nov, snap_dec, mh_needed) %>% 
  summarize(people = sum(people)) %>% 
  mutate(state = "USA")


### Finalize 
data <- rbind(all, usa)


### Food Insecurity 
food_insecurity <- data %>% 
  group_by(week, state, race, kids, wave, 
           food_sufficiency, 
           food_suf_confidence,snap_yn, snap_jan, 
           snap_feb, snap_mar, snap_apr, 
           snap_may, snap_jun, snap_jul, 
           snap_aug, snap_sep, snap_oct, 
           snap_nov, snap_dec) %>% 
  summarize(people = sum(people))

### Employment 
employment <- data %>% 
  group_by(week, state, race, kids, wave, 
           lost_work, 
           expect_loss, employed, ui_applied,
           ui_received) %>% 
  summarize(people = sum(people))

### Housing
housing <- data %>% 
  group_by(week, state, race, kids, wave, 
           housing, housing_paid, 
           housing_confidence, eviction, 
           foreclosure) %>% 
  summarize(people = sum(people))


### Children
children <- data %>% 
  group_by(week, state, wave, race, kids, comp_available, wifi_avail) %>% 
  summarize(people = sum(people))

### Finances 
finances <- data %>% 
  group_by(week, state, race, wave, 
           kids, expenses_difficulty, 
           regular_income, credit_card_loans, savings_assets, 
           borrowing_relatives, ui, stimulus, deferred_payments, 
           snap_spending_needs) %>% 
  summarize(people = sum(people))

### Mental Health 
mental_health <- data %>% 
  group_by(week, wave, state, race, kids, anxious, worry, down, interest, mh_needed) %>% 
  summarize(people = sum(people))

### Write CSV

write.csv(data, "Full App/data/Aggregate.csv", row.names = F)

write.csv(food_insecurity, "Full App/data/food_insecurity.csv", row.names = F)
write.csv(employment, "Full App/data/employment.csv", row.names = F)
write.csv(housing, "Full App/data/housing.csv", row.names = F)
write.csv(children, "Full App/data/children.csv", row.names = F)
write.csv(finances, "Full App/data/finances.csv", row.names = F)
write.csv(mental_health, "Full App/data/mental_health.csv", row.names = F)






## Checking Code 
week_count <- data$week %>% unique() %>% length()

if(week_count != num_weeks){
  
  print(paste0("ERROR: Variable change beginning week", week_count + 1, 
               ". Check ReadMe for instructions.")) 
  
} else {
  
  "No error, continue on to updating the visualization functions in the Full App folder." 
  
}


