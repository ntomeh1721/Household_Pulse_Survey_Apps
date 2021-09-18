
# UI ----------------------------------------------------------------------
expectations_ui <- fluidRow(
  
  # Selected Titles 
  tags$style(type ="text/css", 
             ".type_class .selectize-input{
    font-size:14px; 
    border-color:white;
    font-family:Campton
               }
    .type_class  .selectize-dropdown{
    font-size:14px; 
    font-family:Campton-Light
    }"),

  fluidRow(
    column(6, 
           align = "center", 
           h4("Employment Income Uncertainty:", 
              style = "font-family:Campton"), 
           h6("Respondents Expecting to Lose Employment Income in the Next 4 Weeks", 
              style = "font-family:Campton-Light"), 
           plotOutput("job_expectation", 
                      hover = "expect_job_hover"), 
           style = "height:400px"),
    column(6, 
           align = "center", 
           h4("Housing Status Uncertainty:", 
              style = "font-family:Campton"), 
           h6("Respondents Reporting \"Extremely\", \"Very\", or \"Somewhat\" Likelihoods
           of Eviction or Foreclosure in the Next Two Months", 
              style = "font-family:Campton-Light"), 
           plotlyOutput("housing_threat_expectation"), 
           style = "height:400px") 
  ), 
  br(), 
  br(), 
  fluidRow(
    column(6, 
           align = "left",
           h4("Food Uncertainty:", 
              style = "font-family:Campton"), 
           h6("Respondents With \"No\", \"Some\", or \"Moderate\" Confidence
              in Ability to Pay for Food In the Next 4 Weeks", 
              style = "font-family:Campton-Light"), 
           br(), 
           plotlyOutput("food_confidence_header"), 
           align = "left", 
           style = "height:400px"), 
    column(6, 
           align = "left",
           h4("Housing Payment Uncertainty:", 
              style = "font-family:Campton"),
           h6("Respondents Deferring Payments or With \"No\",  
          \"Slight\", or \"Moderate\" Confidence in Ability to Pay 
              Next Month's Rent/Mortgage", 
              style = "font-family:Campton-Light"),
           br(),
           plotlyOutput("house_confidence"), 
           style = "height:400px")
  ), 
  fluidRow(
    column(12, 
           align = "center", 
           h4("Click on a Graph for More Info", 
              style = "font-family:Campton"), 
           icon("angle-down"))
  ), 
  hr(), 
  
  ### STATE ID 
  tags$style("#exp_state_id {
  font-family:Campton-Light;
  font-size:18px;
  font-color:#BBB8B8
             } "), 
  fluidRow(column(12, 
                  align = "right", 
                  textOutput("exp_state_id"))), 
  fluidRow(
    column(12, 
           h4(id = "employment_expectation", 
              "Employment Income Uncertainty:", 
              style = "font-family:Campton"),
           h5(id = "employment_expectation2", 
              "Respondents Expecting to Lose Employment Income in the Next 4 Weeks, by Race", 
              style = "font-family:Campton-Light"),
           
           h4(id = "ef_expectation", 
              "Housing Status Uncertainty:", 
              style = "font-family:Campton"),
           h5(id = "ef_expectation2", 
              "Likelihood of Losing Home to Eviction/Foreclosure in the Next Two Months, by Race/Ethnicity", 
              style = "font-family:Campton-Light"),
           
           h4(id = "food_expectation", 
             "Food Uncertainty:", 
              style = "font-family:Campton"), 
           h5(id = "food_expectation2", 
              "Confidence in Ability to Pay for Food In the Next 4 Weeks, by Race/Ethnicity", 
              style = "font-family:Campton-Light"),
           
           
           h4(id = "housing_expectation", 
             "Housing Payment Uncertainty:", 
              style = "font-family:Campton"), 
           h5(id = "housing_expectation2", 
              "Confidence in Ability to Pay Next Month's Rent/Mortgage, by Race/Ethnicity", 
              style = "font-family:Campton-Light"))
    
  ), 
  
  br(), 
  fluidRow(column(12,
                    checkboxGroupInput(
                      "home_type",
                      label = NULL,
                      choices = c("Renters", 
                                  "Homeowners"), 
                      selected = c("Renters", "Homeowners"),
                      inline = T), 
                  style = "font-family:Campton;font-size:15px", 
                  align = "center")),
  fluidRow(column(12, 
                  checkboxGroupInput("races_expected", 
                                     NULL, 
                                     c("Overall" = "Overall", 
                                       "Asian" = "Asian alone, not Hispanic", 
                                       "Black" = "Black alone, not Hispanic", 
                                       "Hispanic or Latino" = "Hispanic or Latino, all Races", 
                                       "White" = "White alone, not Hispanic", 
                                       "2+ or Other" = "Two or more races or Other"), 
                                     selected = c("Overall", "Asian alone, not Hispanic", "Black alone, not Hispanic", 
                                                  "Hispanic or Latino, all Races", 
                                                  "White alone, not Hispanic", "Two or more races or Other"),
                                     inline = T
                  ), 
                  style = "font-family:Campton;font-size:15px", 
                  align = "center")), 
  fluidRow(column(12, 
                  checkboxGroupInput("expect_responses_food", 
                                     NULL, 
                                     c("Very Confident", "Moderate", "Somewhat", "No Confidence"), 
                                     selected = c("Moderate", "Somewhat", "No Confidence"),
                                     inline = T
                  ), 
                  style = "font-family:Campton;font-size:15px", 
                  align = "center"),
           column(12, 
                  checkboxGroupInput("threat_responses_housing", 
                                     NULL, 
                                     c("Not Likely", "Somewhat Likely", "Very Likely", "Extremely Likely"), 
                                     selected = c("Somewhat Likely", "Very Likely", "Extremely Likely"),
                                     inline = T
                  ), 
                  style = "font-family:Campton;font-size:15px", 
                  align = "center")), 
  fluidRow(column(12, 
                  checkboxGroupInput("expect_responses_housing", 
                                     NULL, 
                                     c("High", "Moderate", "Slight", 
                                       "Will Be Deferred", "No Confidence"), 
                                     selected = c("Moderate", "Slight", "Will Be Deferred", "No Confidence"),
                                     inline = T
                  ), 
                  style = "font-family:Campton;font-size:15px", 
                  align = "center")), 
  br(), 
  fluidRow(
    column(12, 
           plotOutput("losing_work", 
                      hover = "area_hover_exp"), 
           plotlyOutput("food_confidence", height = "900px"), 
                     # hover = "food_confidence_hover"), 
           plotlyOutput("rent_confidence", height = "900px"), 
                      #hover = "rent_confidence_hover"), 
           plotlyOutput("race_housing_threat", height = "900px")
           )
  )
  
)

# Server ------------------------------------------------------------------


expectations_server <- function(input, output){
  
  output$food_confidence_header <- renderPlotly({
    stacker_head(input$state, input$children, "food_suf_confidence") 
  })
  
  output$house_confidence <- renderPlotly({
    stacker_head(input$state, input$children, "housing_confidence")
  })
  
  output$housing_threat_expectation <- renderPlotly({
    stacker_head(input$state, input$children, "housing_threat")
  })
  
  output$job_expectation <- renderPlot({
    overall_employment_perc_expect(input$state, input$children)
  }, height = 300)
  
  output$losing_work <- renderPlot({
    employment_by_race_expect(input$state, input$children, input$races_expected)
  })
  
  output$food_confidence <- renderPlotly({
    stacker(input$state, input$children, "food_suf_confidence", input$races_expected, input$expect_responses_food)
  })
#  height = reactive(ceiling(length(input$races_expected)/2)*300))
  
  output$rent_confidence <- renderPlotly({
    stacker(input$state, input$children, "housing_confidence", 
            input$races_expected, input$expect_responses_housing, 
            input$home_type)
  })
 # height = reactive(ceiling(length(input$races_expected)/2)*300))
  
  output$race_housing_threat <- renderPlotly({
    stacker(input$state, input$children, "housing_threat", 
            input$races_expected,
            input$threat_responses_housing, 
            input$home_type)
  })
 # height = reactive(ceiling(length(input$races_expected)/2)*300))
  
  output$exp_state_id <- renderText({
    input$state
  })
  
  ## Click Plots 
  shinyjs::hide("employment_expectation")
  shinyjs::hide("food_expectation")
  shinyjs::hide("housing_expectation")
  shinyjs::hide("employment_expectation2")
  shinyjs::hide("food_expectation2")
  shinyjs::hide("housing_expectation2")
  shinyjs::hide("ef_expectation2")
  shinyjs::hide("ef_expectation")
  
  shinyjs::hide("races_expected")
  shinyjs::hide("expect_responses_food")
  shinyjs::hide("expect_responses_housing")
  shinyjs::hide("home_type")
  shinyjs::hide("threat_responses_housing")
  
  shinyjs::hide("losing_work")
  shinyjs::hide("food_confidence")
  shinyjs::hide("rent_confidence")
  shinyjs::hide("race_housing_threat")
  
  
  shinyjs::hide("exp_state_id")
  
  shinyjs::onclick("food_confidence_header",
                   function(){
                     shinyjs::hide("employment_expectation")
                     shinyjs::show("food_expectation")
                     shinyjs::hide("housing_expectation")
                     shinyjs::hide("employment_expectation2")
                     shinyjs::show("food_expectation2")
                     shinyjs::hide("housing_expectation2")
                     shinyjs::hide("losing_work")
                     shinyjs::show("races_expected")
                     shinyjs::show("expect_responses_food")
                     shinyjs::hide("expect_responses_housing")
                     shinyjs::show("food_confidence")
                     shinyjs::hide("rent_confidence")
                     shinyjs::show("exp_state_id")
                     shinyjs::hide("ef_expectation2")
                     shinyjs::hide("ef_expectation")
                     shinyjs::hide("home_type")
                     shinyjs::hide("race_housing_threat")
                     shinyjs::hide("threat_responses_housing")
                  
                   })

  shinyjs::onclick("house_confidence",
                   function(){
                     shinyjs::hide("employment_expectation")
                     shinyjs::hide("food_expectation")
                     shinyjs::show("housing_expectation")
                     shinyjs::hide("employment_expectation2")
                     shinyjs::hide("food_expectation2")
                     shinyjs::show("housing_expectation2")
                     shinyjs::hide("losing_work")
                     shinyjs::show("races_expected")
                     shinyjs::hide("expect_responses_food")
                     shinyjs::show("expect_responses_housing")
                     shinyjs::hide("food_confidence")
                     shinyjs::show("rent_confidence")
                     shinyjs::show("exp_state_id")
                     shinyjs::hide("ef_expectation2")
                     shinyjs::hide("ef_expectation")
                     shinyjs::show("home_type")
                     shinyjs::hide("race_housing_threat")
                     shinyjs::hide("threat_responses_housing")
                   })

  shinyjs::onclick("job_expectation",
                   function(){
                     shinyjs::show("employment_expectation")
                     shinyjs::hide("food_expectation")
                     shinyjs::hide("housing_expectation")
                     shinyjs::show("employment_expectation2")
                     shinyjs::hide("food_expectation2")
                     shinyjs::hide("housing_expectation2")
                     shinyjs::show("losing_work")
                     shinyjs::show("races_expected")
                     shinyjs::hide("expect_responses_food")
                     shinyjs::hide("expect_responses_housing")
                     shinyjs::hide("food_confidence")
                     shinyjs::hide("rent_confidence")
                     shinyjs::show("exp_state_id")
                     shinyjs::hide("ef_expectation2")
                     shinyjs::hide("ef_expectation")
                     shinyjs::hide("home_type")
                     shinyjs::hide("race_housing_threat")
                     shinyjs::hide("threat_responses_housing")
                   })
  
  shinyjs::onclick("housing_threat_expectation",
                   function(){
                     shinyjs::hide("employment_expectation")
                     shinyjs::hide("food_expectation")
                     shinyjs::hide("housing_expectation")
                     shinyjs::hide("employment_expectation2")
                     shinyjs::hide("food_expectation2")
                     shinyjs::hide("housing_expectation2")
                     shinyjs::hide("losing_work")
                     shinyjs::show("races_expected")
                     shinyjs::hide("expect_responses_food")
                     shinyjs::hide("expect_responses_housing")
                     shinyjs::hide("food_confidence")
                     shinyjs::hide("rent_confidence")
                     shinyjs::show("exp_state_id")
                     shinyjs::show("ef_expectation2")
                     shinyjs::show("ef_expectation")
                     shinyjs::show("home_type")
                     shinyjs::show("race_housing_threat")
                     shinyjs::show("threat_responses_housing")
                   })
  
# Hover: Employment Header ------------------------------------------------
  
  expect_employment_reactive <- reactive({
    # If there are kids, redefine base data
    if(input$children == "Respondents With Children"){
      df1 <- employment_data %>%
        filter(kids == "yes")
    } else {
      df1 <- employment_data
    }
    
    # Selected State Data 
    df <- df1 %>% 
      filter(state == input$state | state == "USA") %>% 
      group_by(week, state, expect_loss) %>% 
      summarize(people = sum(people)) %>% 
      na.omit(expect_loss) %>% 
      pivot_wider(names_from = "expect_loss", values_from = "people") %>% 
      mutate(rate = 100*yes/(yes+no))
    
    
    # With Test  
    df %>% 
      mutate(label = paste0(
        round(rate, 1) %>% format(nsmall = 1), 
        "%"
      ))
    
  })
  
  observeEvent(input$expect_job_hover, {
    
    df <- expect_employment_reactive()
    
    df <- df %>%
      nearPoints(input$expect_job_hover,
                 xvar = "week",
                 yvar = "rate")
    
    output$job_expectation <- renderPlot({
      
      if(nrow(df) == 1){
        overall_employment_perc_expect(input$state, input$children) +
          geom_label_repel(data = df,
                           aes(x = week, y = rate, label = label),
                           alpha = 0.7,
                           direction = "y",
                           family = "Akkurat Pro")
      } else {
        overall_employment_perc_expect(input$state, input$children)
      }
      
    }, height = 300)
    
    
  })
  

# Hover: Food Sufficiency by Race Plots -----------------------------------
# 
#   food_confidence_hover_data <- reactive({
#     stacker_data(input$state, input$children, "food_suf_confidence", input$races_expected, input$expect_responses_food)
#   })
#   
#   observeEvent(input$food_confidence_hover, {
#     
#     hd <- food_confidence_hover_data()
#     
#     hd_bar <- hd %>% 
#       filter(
#         week == round(input$food_confidence_hover$x), 
#         height >= input$food_confidence_hover$y, 
#         min_height <= input$food_confidence_hover$y, 
#         race == input$food_confidence_hover$panelvar1
#       ) %>% 
#       mutate(label = perc %>% round(1) %>% format(nsmall = 1))
#       
#     output$food_confidence <- renderPlot({
#       
#       if(nrow(hd_bar) == 1){
#         stacker(input$state, input$children, "food_suf_confidence", input$races_expected, input$expect_responses_food) + 
#           geom_label(data = hd_bar, 
#                     mapping = aes(x = week, 
#                                   y = height, 
#                                   label = label),
#                     size = 4.2, 
#                     fill = "white", 
#                     nudge_y = - 0.2 * hd_bar$perc, 
#                     family = "Akkurat Pro")
#       } else {
#         stacker(input$state, input$children, "food_suf_confidence", input$races_expected, input$expect_responses_food)
#       }
#       
#     }, 
#     height = reactive(ceiling(length(input$races_expected)/2)*300))
#   })
#   

# Hover: Rent Confidence by Race Plots --------------------------------------------------

  # rent_confidence_hover_data <- reactive({
  #   stacker_data(input$state, input$children, "housing_confidence", input$races_expected, input$expect_responses_housing)
  # })
  # 
  # observeEvent(input$rent_confidence_hover, {
  #   
  #   hd <- rent_confidence_hover_data()
  #   
  #   hd_bar <- hd %>% 
  #     filter(
  #       week == round(input$rent_confidence_hover$x), 
  #       height >= input$rent_confidence_hover$y, 
  #       min_height <= input$rent_confidence_hover$y, 
  #       race == input$rent_confidence_hover$panelvar1
  #     ) %>% 
  #     mutate(label = perc %>% round(1) %>% format(nsmall = 1))
  #   
  #   output$rent_confidence <- renderPlot({
  #     
  #     if(nrow(hd_bar) == 1){
  #       stacker(input$state, input$children, "housing_confidence", input$races_expected, input$expect_responses_housing) + 
  #         geom_label(data = hd_bar, 
  #                    mapping = aes(x = week, 
  #                                  y = height, 
  #                                  label = label),
  #                    size = 4.2, 
  #                    fill = "white", 
  #                    nudge_y = - 0.2 * hd_bar$perc, 
  #                    family = "Akkurat Pro")
  #     } else {
  #       stacker(input$state, input$children, "housing_confidence", input$races_expected, input$expect_responses_housing)
  #     }
  #     
  #   }, 
  #   height = reactive(ceiling(length(input$races_expected)/2)*300))
  # })
  # 


# Hover: Expect Job Loss --------------------------------------------------

  job_expect_loss_reactive <- reactive({
    # If there are kids, redefine base data
    if(input$children == "Respondents With Children"){
      df1 <- employment_data %>%
        filter(kids == "yes")
    } else {
      df1 <- employment_data
    }
    
    # Getting an overall race line
    df_overall <- df1 %>%
      filter(state == input$state) %>%
      group_by(week, state, expect_loss) %>%
      summarize(people = sum(people)) %>%
      na.omit(expect_loss) %>%
      pivot_wider(names_from = "expect_loss", values_from = "people") %>%
      mutate(rate = 100*yes/(yes+no),
             race = "Overall")
    
    # Getting Data By Race
    df_race <- df1 %>%
      filter(state ==  input$state) %>%
      group_by(week, state, race, expect_loss) %>%
      summarize(people = sum(people)) %>%
      na.omit(expect_loss) %>%
      pivot_wider(names_from = "expect_loss", values_from = "people") %>%
      mutate(rate = 100*yes/(yes+no))
    
    # Combining Data
    rbind(df_overall, df_race) %>%
      filter(race %in% input$races_expected) %>%
      mutate(label = paste0(
        rate %>% round(1) %>% format(nsmall = 1),
        "%"
      ))
    
  })
  
  observeEvent(input$area_hover_exp, {
    
    aid <- job_expect_loss_reactive()
    
    hover_dat <- nearPoints(aid,
                            input$area_hover_exp,
                            xvar = "week",
                            yvar = "rate")
    
    ## Redefining the Plot
    output$losing_work <- renderPlot({
      
      if(nrow(hover_dat) == 1){
        employment_by_race_expect(input$state, input$children, input$races_expected) + 
          geom_label_repel(data = hover_dat,
                           aes(x = week, y = rate, label = label),
                           alpha = 0.7,
                           direction = "y",
                           family = "Akkurat Pro")
        
      } else {
        employment_by_race_expect(input$state, input$children, input$races_expected)
      }
      
    })
    
  })
} 

  
  
  