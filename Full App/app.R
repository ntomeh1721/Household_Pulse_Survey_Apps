
library(shiny)
library(tidyverse)
library(shinyWidgets)
library(DT)
library(htmltools)
library(plotly)

source("vis_functions.R")
source("food_insecurity_source.R")
source("employment_source.R")
source("housing_source.R")
source("mental_health_source.R")
source("children_source.R")
source("expectations_source.R")
source("finances_source.R")


## UI ----------------------------------------------------------------------


# Define UI for application that draws a histogram
ui <- fluidPage(
    



# Heading -----------------------------------------------------------------
    
## USING SHINYJS 
shinyjs::useShinyjs(), 

## Editing Icons 


    tags$style(".selectize-input.focus {
    border-color: #B6ACD1 ; 
    outline-color:#E4E0EE;
    -webkit-box-shadow: inset 0 1px 1px rgba(0,0,0,.075), 0 0 8px rgba(164, 149, 195, 0.6);
    box-shadow: inset 0 1px 1px rgba(0,0,0,.075), 0 0 8px rgba(204, 196, 223, 0.6)}"),

    tags$style(type ="text/css", 
    ".selectize-input{
    font-size:24px; 
    border-color:white;
    font-family:Campton
               }
    .selectize-dropdown{
    font-size:18px; 
    font-family:Campton-Light
    }"),

    tags$style(".fa-chart-line {color:#836EAA}"),
    tags$style(".fa-pen {color:#836EAA}"),

    br(), 
    
    # Application title
    

    fluidRow(
     column(8, 
            span(titlePanel("Household Pulse Survey"), 
                 style = "font-family:Campton")), 
     column(4, 
            actionButton("instructions", "Using the App", 
                        icon = icon("chart-line"), 
                        style = "border-color:white"),
           actionButton("data_notes", "Notes on the Data", 
                        icon = icon("pen"), 
                        style = "border-color:white"),
           style = "font-family:Campton-Light",
           align = "right")),
    br(), 
    br(), 
    shinydashboard::box(span(uiOutput("the_app"), style = "font-family:AkkuratPro-Light"), 
                        width = 12), 
    column(12, 
           span(uiOutput("data_notes"), style = "font-family:AkkuratPro-Light"), 
           br()
           ), 
    br(), 
    fluidRow(
        column(3, 
               selectizeInput(
                   "topic",
                   label = NULL,
                   choices = c(
                      "Food Insecurity",
                      "Employment",
                      "Housing",
                      "Expectations",
                      "Finances",
                      "Mental Health",
                      "Kid's Education"
                       ),
                   multiple = F)), 
        column(1, 
               p("in", 
                 style = "font-family:Campton-Light;font-size:24px"), 
               align = "center"),
        column(3,
               selectizeInput(
                   "state",
                   label = NULL,
                   choices = c("USA", states),
                   multiple = F)), 
        column(1, 
               p("for", 
                 style = "font-family:Campton-Light;font-size:24px"),
               align = "center"),
        column(4, 
               selectizeInput(
                   "children",
                   label = NULL,
                   choices = c("All Respondents", "Respondents With Children"),
                   selected = "All Respondents"))
        
    ), 
hr(), 

conditionalPanel(
    condition = "input.topic == `Food Insecurity`",
    food_insecurity_ui
),

conditionalPanel(
    condition = "input.topic == `Employment`",
    employment_ui
),

conditionalPanel(
    condition = "input.topic == `Housing`",
    housing_ui
),

conditionalPanel(
    condition = "input.topic == `Kid's Education`",
    children_ui
),

conditionalPanel(
    condition = "input.topic == `Mental Health`",
    mental_health_ui
),

conditionalPanel(
    condition = "input.topic == `Finances`",
    finances_ui
),

conditionalPanel(
    condition = "input.topic == `Expectations`",
    expectations_ui
)


)



## Server ------------------------------------------------------------------

server <- function(input, output) {
    
    # Pages Servers 
    fi_server(input, output)
    employment_server(input, output)
    housing_server(input, output)
    children_server(input, output)
    mental_health_server(input, output)
    finances_server(input, output)
    expectations_server(input, output)

    # Data Notes and How to Use the App
    # How to Use the App Message
    observeEvent(input$instructions, {
        
        if(input$instructions/2 == round(input$instructions/2)){
            
            output$the_app <- NULL 
            
        } else {
            
            output$the_app <- renderUI(
                HTML(
                    
                    "<br>Using the Census Bureau’s Household Pulse Survey data, this app 
                    visualizes results on food insecurity, employment, housing, household finance, 
                    mental health, and children’s access to educational resources
                   from April 14th through November 9th.
                    <br><br>
                    Users can select a topic, a state, and household type (with children under 18 or without). 
                    The categories are: 
                    <br><br> 
                    <ul>
                    <li> Food Insecurity: Responses on difficultly affording food are presented and
                    converted into security metrics.
                    <li> Employment: Rates of work and job loss as a share of all respondents.
                    <li> Housing: Distribution of home ownership and payment status.
                    <li> Expectations: Respondents’ confidence in their job, food security, and housing payments, 
                    and likelihood of experiencing foreclosure on or eviction from their home.
                    <li> Mental Health: Reported levels of anxiety, depression, worry, and interest. 
                    <li> Finances: Reported level of difficulty meeting spending needs and how 
                    respondents are metting the needs. 
                    <li> Kid’s Education: Frequency of kids’ access to a computer or Wi-Fi.
                    </ul> <br> 
                    On each page, respondents are able to engage with the headline graphs to 
                    reveal more information. Hovering over points on the graph will reveal 
                    their values. Clicking on headline graphs – with the exception of those 
                    in the “Mental Health” or \"Finances\" categories – will reveal the data expanded by 
                    race of respondent and specific responses. On the Mental Health 
                    page, race of respondent can be selected at the top and compared to the 
                    average across the US and selected state. 
                    <br><br> 
                    Updated July 15th, 2021" 
                    
                    
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
"<br> Since April 23rd, 2020, the Census Bureau has conducted a weekly Household Pulse survey
which collects data on a range of topics including employment, health, housing, availability of 
resources for children, and food sufficiency, with the exception of a six week hiatus in the early summer. 
This app uses the Public Use Data File (PUF) to build its visualizations. 
The PUF were released weekly before the hiatus and bi-weekly after. They detail every combination of responses 
received in the week prior as well as their person-level weighting. Sample sizes vary by state and 
racial/ethnic group. Only point estimates are presented here without statistical confidence 
intervals; differences across time, groups, and states may not be statistically significant.

<br><br> 

<i>Note on Food Insecurity: </i> 
<br><br> 

Usually a household’s food security status is measured using a 10-item scale (18 items for 
households with children) that assesses a variety of aspects of a household’s access to food, 
ranging from how often the household members worried about having enough money for food to 
how often a child has gone a day without eating. This series of questions is then used to 
determine the household’s food security status.
<br><br> 


The Census Household Pulse Survey asks only a single question to all households about their
 access to food. Respondents are asked a question about “food sufficiency” and are asked to 
choose which statement best represents the food eaten in their households over the last 7 days. 
Potential responses are:
<br><br> 
<ul><li> “Enough of the kinds of food we wanted to eat”
<li> “Enough, but not always the kinds of food we wanted to eat”
<li> “Sometimes not enough to eat”
<li> “Often not enough to eat” </ul> 
<br>

We use responses to the “food sufficiency” question to predict rates of food insecurity, using the 
usual relationship between an individual’s answer to the food sufficiency question and their food 
insecurity status in a dataset that includes both measures, the December Food Security 
Supplement to the Current Population Survey. Note that the data are weighted at the respondent 
level and represent respondents' reports about their household's food security status. The 
relationship is calculated separately for eachstate using 2015-18 pooled data. See 
Schanzenbach and Pitts, June 1, 2020, for more details.
<br><br> 



<i> Note on Employment: </i> 
<br><br> 

Employment is calculated as the percentage of all respondents who reported doing any work in the 
past seven days for pay or profit. This measure does not distinguish between whether 
respondents are in or out of the labor force. 
<br><br> 


<i> Note on Question Changes: </i> 
<br><br> 
 
<ul><li> On Housing: Beginning in August, questions about being caught up on rent/mortgage changed slightly, 
and others were added asking if respondents' anticipate being evicted or having their house foreclosed upon 
in the next four weeks. 
<li> On Food Confidence:  Beginning in January 2021, respondents were no longer asked about their 
confidence in their ability to afford food in the next four weeks. 

<br><br> 
"
                    
                )
            )
            
        }
        
    })
    
    
    
    
}



# Run the application 
shinyApp(ui = ui, server = server) 
