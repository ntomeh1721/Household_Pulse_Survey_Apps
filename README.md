# Household_Pulse_Survey_Apps

Interactive web applications illustrating food insecurity in the wake of COVID-19 created with R for Northwestern's Institute for Policy Research. 

The two apps included are the Food Insecurity App and the Full App. The Food Insecurity App includes only food insecurity/sufficiency data and the Full App includes the graphs on employment, housing, expectations, etc. To update the apps, follow the instructions below. Note that the instructions are the same in Steps 2 and 3 and only different in Step 1. Also note that because the Full App uses the HPS public use files, while the Food Insecurity App uses the HPS data tables, the Full App will be a week behind the Food Insecurity App.

I've included what to do in the case of the most common errors, but if there's something more involved, the code is pretty dense so just email me. 

### Step 1: Downloading New Data

##### Full App 

1. Open "Full App - Writing Data.R". 
2. Update `num_weeks` to the latest week number, as according to HPS Public Use Files. For example, as of Sept. 17th, 2021, this number is 35. 
3. Run all. Note that this will take 10+ minutes for the Full App.s
4. Check the console. If it says "ERROR: Variable change beginning week X. Check ReadMe for instructions." then continue on below. Otherwise move on to Step 2: Updating Visualization Functions. 
5. Note: If HPS took another hiatus, then Cntrl + F and search for "NOTE". At that location, adjust the "week" conversion to account for the gap. The point of the "week" variable conversion is to have appropriate spacing along the x axis in the final graphs. The new week number is the counted number of calendar weeks since the first survey release. 


Error Instructions 

1. Note the week number in the error message. This will be referred to as X in the following instructions. 
2. Restart R. 
3. Run the code in the Set Up Section. 
4. Beginning in the next section, "Loading Data", you'll see the line of code `data <- {}` then `for(i in 1:num_weeks){`. Change the `1` to X. 
4. Run the full Loading Data section. You'll get the error message in the console, `Error: Can't subset columns that don't exist. x Column [variable] doesn't exist.` This means that CPS removed a question from the survey. 
5. Change the line `if(i > 33){` to `if(i %in% 33:Y){` where Y = X - 1. For example, if the error message said "ERROR: Variable change beginning week 40. Check ReadMe for instructions", then `if(i > 33){` becomes `if(i %in% 33:39){`. 
6. Copy and paste the conditional statement beginning `if(i %in% 33:Y){`. An easy way to do this is to collapse it by clicking the triangle net to the line number, then selecting that line and copying and pasting into the section below. 
7. Change the condition to `if(i > Y){`. 
8. Find `[variable]` as named in the error message in the `select()` function. 
9. Remove `[varibale]` from the `select()` function and move into the `mutate()` function. 
10. Set `[variable] = 0`. 
11. Run the "Loading Data" section again to see if the error persists. There may be more than one variable that needs to be moved to the `mutate()` section. 
12. Scroll back up to the beginning of the "Loading Data" and reset X in `for(i in X:num_weeks){` back to 1. 
13. Restart R and rerun the whole script. 

##### Food Insecurity App 

1. Open "Food Insecurity App - Writing Data.R". Update `new_week` with the latest week number as according to HPS Data Tables. Update `new_week_start` with the first date of the latest week. 
2. Run All. 
3. Note: If HPS took another hiatus, then Cntrl + F and search for "NOTE: Week Change". At that location, change `old_data$week %>% max() + 2` to `old_data$week %>% max() + X` in which X is the number of calendar weeks between the end of the last HPS week and the start of the new HPS week. 

### Step 2: Updating Visualization Functions

1. Open "vis_functions.R" in the app folder. 
2. Update the `week_labels` vector with the additional week dates. The dates will increase by one week at a time and should go up until the last day of the most recent week of survey data. 
3. Don't run this script - you will get an error.


ERROR INSTRUCTIONS 
If in Step 1: Downloading New Data, you had to move a variable into the `mutate()` function and set it equal to zero, then, in `vis_functions.R`: 
1. Note that this document is a series of functions. There are data setting functions, whose names end in `_data`, and visualization functions (all the rest). Search in the script for the visualization function that contains the variable that resulted in the error in the Writing Data script. 
3. Within the function, there will be a `ggplot()` function. The first argument in the `ggplot()` function is the dataset. Find where data is assigned to that dataset name (i.e. you will see `ggplot(dataset, aes(...))` find `dataset <- other...` and the line `dataset <- dataset %>% filter(week <= Y)` where Y is as defined in Step 1: Downloading New Data. For an example of this, search in the document "### EXAMPLE OF ERROR: filtering weeks". 
4. Create a new vector `new_labs <- week_labels[1:Y]` where Y is as defined in Step 1: Downloading New Data. For an example of this, search in the document "### EXAMPLE OF ERROR: limits labels". 
5. Scroll down to the function `scale_x_continuous()` and within that replace `week_labels` with `new_labs`. 
6. If the above doesn't work, contact me (`ntomeh52@gmail.com`). 



### Step 3: Running the App 

1. Open the "app.R" script in the app folder. 
2. Click "Run App" in the top right corner. It will take a while to load, but then the app should pop up in a separate window. 
3. Try open all the graphs in the app to make sure they're all working. The most common error will appear in place of the graphs and will be "Breaks and labels do not match".  
3a. If you don't see this error anywhere, click "Republish" in the top right corner. You'll need to be logged into ShinyApps to do this. Patricia at IPR is the person to set you up with this if you're not already.
3b. If you get the error, return to Step 1: Downloading New Data. If that runs without error, contact me (`ntomeh52@gmail.com`).



