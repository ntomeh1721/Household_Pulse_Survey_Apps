
# Loading Libraries 
library(shiny)
library(shinyWidgets)
library(DT)
library(htmltools)
library(tidyverse) 
library(ggrepel)
library(plotly)

# Sourcing Functions 
source("vis_functions.R")

# Loading Data 
all_states <- read_csv("data_prepped/Aggregate.csv")

# Defining Choices 
states <- all_states %>% 
  arrange(state) %>% 
  filter(state != "USA") %>% 
  pluck("state") %>% 
  unique()

races <- all_states %>% 
  filter(category == "Hispanic origin and Race") %>% 
  pluck("characteristic") %>% 
  unique() 



# Loading Data

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Analytics Tracker 
  tags$script(HTML("analytics_code.html")),
  
  # Application title
  span(titlePanel("Weekly Food Insecurity Rates during COVID-19"), style = "font-family:Campton"),
  br(), 
  column(12, 
         actionButton("instructions", "How to Use the App"),
         style = "font-family:Campton-Light"),
  br(), 
  column(12, 
         span(uiOutput("the_app"), style = "font-family:AkkuratPro-Light")), 
  br(), 
  column(12, 
         actionButton("data_notes", "Notes on the Data"),
         style = "font-family:Campton-Light"),
  br(), 
  column(12, 
         span(uiOutput("data_notes"), style = "font-family:AkkuratPro-Light"), 
         br()
  ), 
  
  # Inputs Panel 
  
  fluidRow(
    
    br(), 
    column(4, 
           pickerInput(
             "states", 
             "Select States:",
             choices = c("USA", states),
             selected = "USA", 
             multiple = T,
             options = list(
               `max-options-group` = 3,
               `max-options-text` = "Maximum of 3 States")),
           style = "font-family:AkkuratPro-Light"), 
    column(4, 
           pickerInput(
             "races", 
             "Select Race/Ethnicity of Respondent:", 
             choices = races, 
             multiple = T,
             options = list(
               `actions-box` = T,
               `select-all-text` = "Select All", 
               `deselect-all-text` = "Select None"
             )
           ),
           style = "font-family:AkkuratPro-Light"),
    column(4,  
           radioButtons(
             "children", 
             "Select Respondents: ",
             choices = c("All", "With Children"), 
             selected = "All",
             inline = T
           )),
    style = "font-family:AkkuratPro-Light"),
  
  fluidRow(
    column(12, 
           span(textOutput("max_warning"), style = "color:red; font-family:Campton-light"),
           br())), 
  
  fluidRow(
    column(4, 
           DT::dataTableOutput("averages"), 
           style = "height: 420px; overflow-y: scroll; font-family:AkkuratPro-light",
           align = "center"), 
    column(8, 
           span(h4("Weekly Food Insecurity"), style = "font-family:Campton"), 
           plotOutput("summary",
                      hover = hoverOpts(id = "plot1_hover",
                                        delay = 100, 
                                        delayType = "debounce",
                                        clip = T, 
                                        nullOutside = T)),
           br())), 
  
  fluidRow(column(11, 
                  span(h4("Weekly Food Insecurity for Selected Races/Ethnicities by State*"), style = "font-family:Campton"), 
                  plotOutput("race_line", 
                             hover = hoverOpts(id = "plot2_hover",
                                               delay = 100, 
                                               delayType = "debounce",
                                               clip = T, 
                                               nullOutside = T)), 
                  br()),
           column(1, "", 
                  br())),
  fluidRow(column(12, 
                  span(h4("Share of Weekly Overall Food Insecurity by Race/Ethnicity for Selected States"), 
                       style = "font-family:Campton"),
                  plotlyOutput("race_bar"),
                             # hover = hoverOpts(id = "bar_hover", 
                             #                   delay = 100, 
                             #                   delayType = "debounce", 
                             #                   clip = T, 
                             #                   nullOutside = T)), 
                  br(),
                  span(h5("* Data on racial/ethnic groups that represent <3% of the surveyed population in a given state and week are not separately reported, and instead appear in the “Two or more races or Other” category."), 
                       style = "font-family:Campton-light"),
                  br()))
)




# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  # How to Use the App Message 
  
  observeEvent(input$instructions, {
    
    if(input$instructions/2 == round(input$instructions/2)){
      
      output$the_app <- NULL 
      
    } else {
      
      output$the_app <- renderUI(
        HTML(
          
          "<br> This app visualizes food insecurity rates in the wake of COVID-19, across states and 
          racial/ethnic groups, using data from the U.S. Census Bureau’s Household Pulse Survey.
          <br><br> Users can choose to display up to 3 states at a time by checking the desired states’ 
          names using the drop-down menu at the top left. Data can be reported for respondents overall, 
          or for respondents with children, by selecting the option using the buttons at the top right.
          Using the middle menu, users can select to display a range of racial/ethnic groups.
          <br><br> The table on the left displays average food insecurity rates across all weeks 
          for the selected states and groups. The graph on the right displays weekly aggregate
          food insecurity rates for the selected states. Users can use the cursor to hover over
          individual points to see the rate in a particular week, and over grey lines to view which state t
          hey represent. Note: the dates along the x axis are the last day in each week of surveys. 
          <br><br> Directly below these results, line graph(s) depicting food insecurity rates for selected 
          racial/ethnicity groups are displayed. Note that data on racial/ethnic groups that represent 
          fewer than 3% of the surveyed population in a given state and week are not separately reported, 
          and instead appear in the “Two or more races or Other” category.
          <br><br> At the bottom of the page, bar graphs are displayed representing the racial/ethnic breakdown of 
          the food insecure population in the state/week. By using the cursor to hover over the bars, users can 
          see the percentage of the state’s overall population represented by the racial/ethnic group, and the 
          percentage of the state’s food insecure population represented by the racial/ethnic group. Percentage 
          of the population for each race/ethnicity is a weighted estimate of the overall population based on the 
          sample population in a given week.
          <br><br>"
          
        )
      )
      
      
    }
    
    
  })
  
  
  ## Data Notes Button 
  observeEvent(input$data_notes, {
    
    if(input$data_notes / 2 == round(input$data_notes/2)) {
      
      output$data_notes <- NULL 
      
    } else { 
      
      output$data_notes <- renderUI(
        HTML(
          "<br>Since April 23rd, 2020, the Census Bureau has conducted a weekly Household Pulse survey, 
          which collects data on a range of topics including employment, health, housing, and food sufficiency. 
          Household Pulse Food Sufficiency and Food Security Data Tables 2b and 3b, “Food Sufficiency for
          Households, in the Last 7 Days, by Select Characteristics” and “Food Sufficiency for Households 
          with Children, in the Last 7 Days, by Select Characteristics” provide data by state on race and 
          food sufficiency, which is plotted below. Sample sizes vary by state and racial/ethnic group. 
          Only point estimates are presented here without statistical confidence intervals; differences 
          across time, groups, and states may not be statistically significant.
          <br><br> Usually a household’s food security status is measured using a 10-item scale 
          (18 items for households with children) that assesses a variety of aspects of a household’s 
          access to food, ranging from how often the household members worried about having enough money
          for food to how often a child has gone a day without eating. This series of questions is then
          used to determine the household’s food security status.
          <br><br> The Census Household Pulse Survey asks only a single question to all households about their 
          access to food. Respondents are asked a question about “food sufficiency” and are asked to choose
          which statement best represents the food eaten in their households over the last 7 days. Potential 
          responses are:
          <ul><li> “Enough of the kinds of food we wanted to eat”
          <li> “Enough, but not always the kinds of food we wanted to eat”
          <li> “Sometimes not enough to eat”
          <li> “Often not enough to eat” </ul>
          <br> We use responses to the “food sufficiency” question to predict rates of food insecurity, 
          using the usual relationship between an individual’s answer to the food sufficiency question and 
          their food insecurity status in a dataset that includes both measures, the December Food Security 
          Supplement to the Current Population Survey. Note that the data are weighted at the respondent 
          level and represent respondents' reports about their household's food security status. 
          The relationship is calculated separately for eachstate using 2015-18 pooled data. 
          See Schanzenbach and Pitts, June 1, 2020, for more details.
          <br><br>      
          <br><br>  
          UPDATED July 6th, 2021: added the most recent data. Note the gap in the chart is due to the absence of 
          survey data during that time. 
          <br><br>"
          
        )
      )
      
    }
    
  })
  
  ## Warning Message
  output$max_warning <- renderText({
    
    warning(input$states)
    
  })
  
  
  ## Averages table 
  
  output$averages <- DT::renderDataTable({
    
    show_averages(input$states, input$races, input$children)
    
    
  },
  options = list(
    dom = "t",
    paging = F
  ), 
  rownames = F, 
  class = "row-border",
  )
  
  
  ## Plot1: All States Plot 
  output$summary <- renderPlot({
    
    summary_graph(input$states, input$children)
    
  })
  
  ## Hover Function - Summary 
  observeEvent(input$plot1_hover, {
    
    ## Over Selected Data  
    hover_data <- choose_data(input$children) %>% 
      filter(state %in% head(input$states, 3) | state == "USA",
             characteristic == "Total") %>% 
      nearPoints(input$plot1_hover, 
                 xvar = "week", 
                 yvar = "perc_insecure") %>% 
      mutate(label = paste0(round(perc_insecure, 1), "%"))
    
    
    hover_data2 <- points_on_the_line(input$children, input$states) %>% 
      nearPoints(input$plot1_hover, 
                 xvar = "week", 
                 yvar = "perc_insecure")
    
    output$summary <- renderPlot({
      
      if(nrow(hover_data) == 1){
        
        summary_graph(input$states, input$children) + 
          geom_label(data = hover_data, 
                     aes(x = week, y = perc_insecure, label = label),
                     alpha = 0.7, 
                     nudge_y = 2,
                     family = "Akkurat Pro")
        
      } else {
        
        if(nrow(hover_data2) == 1){
          
          summary_graph(input$states, input$children) + 
            geom_text(data = choose_data(input$children) %>% 
                        filter(characteristic == "Total", 
                               state == hover_data2 %>% pluck("state"),
                               week == max(choose_data(input$children)$week)),
                      aes(x = 8, y = 0, label = hover_data2 %>% pluck("state")), 
                      nudge_x = 0.5,
                      family = "Akkurat Pro", 
                      size = 3.5) +
            geom_line(data = choose_data(input$children) %>% 
                        filter(characteristic == "Total", 
                               state == hover_data2 %>% pluck("state")),
                      aes(x = week, y = perc_insecure), 
                      color = "#B6ACD1")
          
          
        } else { 
          
          ## Not Over Anything 
          summary_graph(input$states, input$children)
          
        }
        
      }
      
    })
    
    
  })
  
  
  ## Plot2: Race by State Plot 
  output$race_line <- renderPlot({
    
    race_line(input$states, input$races, input$children)
    
  })
  
  ## Hover Function - Race Line Plot  
  observeEvent(input$plot2_hover, {
    
    ## Over Selected Data  
    hover_data <- choose_data(input$children) %>% 
      filter(state %in% head(input$states, 3),
             characteristic %in% input$races) %>% 
      nearPoints(input$plot2_hover, 
                 xvar = "week", 
                 yvar = "perc_insecure") %>% 
      mutate(label = paste0(round(perc_insecure, 1), "%"))
    
    
    
    output$race_line <- renderPlot({
      
      if(nrow(hover_data) == 1){
        
        race_line(input$states, input$races, input$children) + 
          geom_label(data = hover_data, 
                     aes(x = week, y = perc_insecure, label = label),
                     nudge_y = - 3.5,
                     alpha = 0.7, 
                     family = "Akkurat Pro")
        
      } else {
        
        
        ## Not Over Anything 
        race_line(input$states, input$races, input$children)
        
        
        
      }
      
    })
    
    
  })
  
  ## Plot3: Share of State by Race Plot 
  output$race_bar <- renderPlotly({
    
    race_bar(input$states, input$races, input$children)
    
  })

  
  
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
