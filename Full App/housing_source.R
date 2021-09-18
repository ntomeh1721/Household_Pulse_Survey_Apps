

# UI ----------------------------------------------------------------------
housing_ui <- fluidPage(
  
  fluidRow(column(5, 
                  span(h5("Home Ownership"), style = "font-family:Campton"),
                  plotOutput("housing"), 
                  align = "center"),
           column(7, 
                  span(h5("Share of Respondents Behind on Rent/Mortgage Payments"), 
                       style = "font-family:Campton"),
                  plotOutput("housing_paid", 
                             hover = "housing_paid_hover"),
                  align = "center")),
  
  br(), 
  br(), 
  # Hover Features
  fluidRow(
    column(5, align = "center", 
           p(id = "showmore_left_home", "Click on Graph for More Info"), 
           icon("angle-down")), 
    column(7, align = "center", 
           p(id = "showmore_right_home", "Click on Graph for More Info"), 
           icon("angle-down")), 
    style = "font-family:AkkuratPro-Bold;font-size:13px;font-color:#716C6B"
    
  ), 
  
  br(), 
  hr(), 
  tags$style("#h_state_id {
  font-family:Campton;
  font-size:18px;
  font-color:#716C6B
             } "), 
  fluidRow(column(12, 
                  align = "right", 
                  textOutput("h_state_id"))), 
  
  # Selected Titles 
  tags$style(type ="text/css", 
             ".rent_class .selectize-input{
    font-size:14px; 
    border-color:white;
    font-family:Campton
               }
    .rent_class  .selectize-dropdown{
    font-size:14px; 
    font-family:Campton-Light
    }"),
  
  fluidRow(
    column(12, 
           h4(id = "ownership_title_housing", 
              "Home Ownership by Race/Ethnicity", 
              style = "font-family:Campton"),
           h4(id = "housing_paid_title_housing", 
              "Rates of Rent/Mortgage Payment Delay", 
              style = "font-family:Campton")), 
    column(8, 
           h5(id = "housing_paid_title_housing2", 
              "Payment Status of Last Month's Rent/Mortgage, Among Those who ", 
              style = "font-family:Campton-Light;padding:12px", 
              align = "left")), 
    column(4,
           tags$div(
             id = "rent_div", 
             class = "rent_class",
             selectizeInput(
               "rent_mort",
               label = NULL,
               choices = c("Rent or Own Their Home with a Mortgage or Loan", 
                           "Rent their Home", 
                           "Own their Home with a Mortgage/Loan"), 
               selected = c("Rent or Own Their Home with a Mortgage or Loan"),
               multiple = F),
             align = "center"
           )
           )),
  fluidRow(column(12, 
                  p(id = "housing_clause", 
                    "NOTE: Beginning in August, the question \"Did you pay last month's rent or mortgage on time?\", 
                  in which potential responses were \"Paid\", \"Not Paid\", and \"Deferred\",
                  changed to \"Are you caught up on [rent/mortgage]?\" in which the potential responses were a simple
                    \"Yes\" or \"No\".", 
                    style = "font-family:Campton-Light;font-size:12px"), 
                  align = "left")), 
  br(), 
  br(), 
  fluidRow(column(12, 
                  checkboxGroupInput("races_home", 
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
                  checkboxGroupInput("paid_responses", 
                                     NULL, 
                                     c("Paid", "Caught Up", 
                                       "Deferred", "Not Paid",
                                       "Not Caught Up"), 
                                     selected = c("Deferred", "Not Paid", "Not Caught Up"),
                                     inline = T
                  ), 
                  style = "font-family:Campton;font-size:15px", 
                  align = "center")), 
  br(), 
  fluidRow(column(12, 
                  plotlyOutput("housing_payment_plots", height = "900px"), 
                            # hover = "home_race_hover"), 
                  align = "center")), 
  
  fluidRow(column(4, 
                  h5(id = "us_overall_title_housing", 
                     "US Respondents", 
                     style = "font-family:Campton"), 
                  plotOutput("housing_donut_us_overall")), 
           column(4, 
                  h5(id = "twoplus_title_housing", 
                     "Respondents Who Are Two or More Races or Other", 
                     style = "font-family:Campton"),
                  plotOutput("housing_donut_twoplus")), 
           column(4, 
                  h5(id = "asian_title_housing", 
                     "Asian Respondents", 
                     style = "font-family:Campton"),
                  plotOutput("housing_donut_asian"))), 
  br(), 
  br(), 
  fluidRow(
    column(4, 
           h5(id = "black_title_housing", 
              "Black Respondents", 
              style = "font-family:Campton"),
           plotOutput("housing_donut_black")),
    column(4, 
           h5(id = "latino_title_housing", 
              "Hispanic or Latino Respondents", 
              style = "font-family:Campton"),
           plotOutput("housing_donut_latino")),
    column(4, 
           h5(id = "white_title_housing", 
              "White Respondents", 
              style = "font-family:Campton"),
           plotOutput("housing_donut_white"))
  )
  
)


  

# Server ------------------------------------------------------------------

housing_server <- function(input, output){
  
  output$housing <- renderPlot({
    housing_donut_overall(input$state, input$children)
  })
  
  output$housing_paid <- renderPlot({
    housing_header(input$state, input$children)
  })
  
  output$housing_payment_plots <- renderPlotly({
    stacker(input$state, input$children, "housing_paid", input$races_home, input$paid_responses, input$rent_mort)
  })#, height =  reactive(ceiling(length(input$races_home)/2)*300))
  
  output$housing_donut_us_overall <- renderPlot({
    housing_donut_overall("USA", input$children)
  })
  
  output$housing_donut_twoplus <- renderPlot({
    housing_donut_race(input$state, input$children, "Two or more races or Other")
  })
  
  output$housing_donut_asian <- renderPlot({
    housing_donut_race(input$state, input$children, "Asian alone, not Hispanic")
  })
  
  output$housing_donut_black <- renderPlot({
    housing_donut_race(input$state, input$children, "Black alone, not Hispanic")
  })
  
  output$housing_donut_white <- renderPlot({
    housing_donut_race(input$state, input$children, "White alone, not Hispanic")
  })
  
  output$housing_donut_latino <- renderPlot({
    housing_donut_race(input$state, input$children, "Hispanic or Latino, all Races")
  })
  
  output$h_state_id <- renderText({
    input$state
  })
  
  ### Show More Drop Down
  shinyjs::hide("showmore_left_home")
  shinyjs::hide("showmore_right_home")
  
  shinyjs::onevent("hover", "housing", 
                   function(){
                     shinyjs::show("showmore_left_home") 
                     shinyjs::hide("showmore_right_home")
                   }
  )
  shinyjs::onevent("hover", "housing_paid", 
                   function(){
                     shinyjs::hide("showmore_left_home") 
                     shinyjs::show("showmore_right_home")
                   } 
  )
  
  
  ### Titles + Control
  shinyjs::hide("ownership_title_housing")
  shinyjs::hide("housing_paid_title_housing")
  shinyjs::hide("housing_paid_title_housing2")
  
  shinyjs::hide("races_home")
  shinyjs::hide("paid_responses")
  
  shinyjs::hide("payment_type")
  shinyjs::hide("rent_mort")
  
  shinyjs::hide("housing_clause")
  
  shinyjs::hide("us_overall_title_housing")
  shinyjs::hide("housing_donut_us_overall")
  shinyjs::hide("twoplus_title_housing")
  shinyjs::hide("housing_donut_twoplus")
  shinyjs::hide("asian_title_housing")
  shinyjs::hide("housing_donut_asian")
  shinyjs::hide("black_title_housing")
  shinyjs::hide("housing_donut_black")
  shinyjs::hide("white_title_housing")
  shinyjs::hide("housing_donut_white")
  shinyjs::hide("latino_title_housing")
  shinyjs::hide("housing_donut_latino")
  
  shinyjs::hide("h_state_id")
  
  
  ### Graphs 
  shinyjs::hide("housing_payment_plots")
  
  shinyjs::onclick("housing", 
                   function(){
                     shinyjs::show("ownership_title_housing") 
                     shinyjs::hide("housing_paid_title_housing")
                     shinyjs::hide("housing_paid_title_housing2")
                     shinyjs::hide("races_home")
                     shinyjs::hide("payment_type")
                     shinyjs::hide("rent_mort")
                     shinyjs::hide("paid_responses")
                     shinyjs::hide("housing_clause")
                     shinyjs::hide("housing_payment_plots")
                     shinyjs::show("us_overall_title_housing")
                     shinyjs::show("housing_donut_us_overall")
                     shinyjs::show("twoplus_title_housing")
                     shinyjs::show("housing_donut_twoplus")
                     shinyjs::show("asian_title_housing")
                     shinyjs::show("housing_donut_asian")
                     shinyjs::show("black_title_housing")
                     shinyjs::show("housing_donut_black")
                     shinyjs::show("white_title_housing")
                     shinyjs::show("housing_donut_white")
                     shinyjs::show("latino_title_housing")
                     shinyjs::show("housing_donut_latino")
                     shinyjs::show("h_state_id")
                   })
  
  shinyjs::onclick("housing_paid", 
                   function(){
                     shinyjs::hide("ownership_title_housing") 
                     shinyjs::show("housing_paid_title_housing")
                     shinyjs::show("housing_paid_title_housing2")
                     shinyjs::show("races_home")
                     shinyjs::show("payment_type")
                     shinyjs::show("housing_clause")
                     shinyjs::show("rent_mort")
                     shinyjs::show("paid_responses")
                     shinyjs::show("housing_payment_plots")
                     shinyjs::hide("us_overall_title_housing")
                     shinyjs::hide("housing_donut_us_overall")
                     shinyjs::hide("twoplus_title_housing")
                     shinyjs::hide("housing_donut_twoplus")
                     shinyjs::hide("asian_title_housing")
                     shinyjs::hide("housing_donut_asian")
                     shinyjs::hide("black_title_housing")
                     shinyjs::hide("housing_donut_black")
                     shinyjs::hide("white_title_housing")
                     shinyjs::hide("housing_donut_white")
                     shinyjs::hide("latino_title_housing")
                     shinyjs::hide("housing_donut_latino")
                     shinyjs::show("h_state_id")
                   })
  
  # Payment by Race Plots 
  # home_race_data <- reactive({
  #   stacker_data(input$state, input$children, "housing_paid", input$races_home, input$paid_responses, input$rent_mort)
  # })
  # 
  # observeEvent(input$home_race_hover, {
  #   
  #   home_race_hover_data <- home_race_data()
  #   
  #   hrhd <- home_race_hover_data %>% 
  #     filter(
  #       week == round(input$home_race_hover$x), 
  #       height >= input$home_race_hover$y, 
  #       min_height <= input$home_race_hover$y, 
  #       race == input$home_race_hover$panelvar1
  #     ) %>% 
  #     mutate(label = perc %>% round(1) %>% format(nsmall = 1))
  #   
  #   output$housing_payment_plots <- renderPlot({
  #     
  #     if(nrow(hrhd) == 1){
  #       
  #       stacker(input$state, input$children, "housing_paid", input$races_home, input$paid_responses, input$rent_mort) + 
  #         geom_label(data = hrhd, 
  #                    aes(x = week, 
  #                        y = height, 
  #                        label = label),
  #                    size = 4.2, 
  #                    fill = "white", 
  #                    nudge_y = - 0.2 * hrhd$perc, 
  #                    family = "Akkurat Pro")
  #       
  #     } else {
  #       stacker(input$state, input$children, "housing_paid", input$races_home, input$paid_responses, input$rent_mort)
  #     }
  #     
  #     
  #     
  #   }, height =  reactive(ceiling(length(input$races_home)/2)*300))
  #   
  #   
  # })
  # 
  ## Header Graph 
  
  housing_header_dat <- reactive({
    housing_header_data(input$state, input$children) 
  })
  
  observeEvent(input$housing_paid_hover, { 
   
     hover_home_dat <- housing_header_dat() 
     
     hover_home <- nearPoints(hover_home_dat, 
                              input$housing_paid_hover, 
                              xvar = "week", 
                              yvar = "rate") %>% 
       mutate(label = round(rate, 1) %>% format(nsmall = 1) %>% paste0("%"))
     
    output$housing_paid <- renderPlot({
      
      if(nrow(hover_home) == 1){ 
        
        housing_header(input$state, input$children) + 
          geom_label(data =hover_home, 
                     aes(x = week, y = rate, label = label), 
                     alpha = 0.7, 
                     nudge_y = 0.1*hover_home$rate, 
                     family = "Akkurat Pro")
        
      } else {
        housing_header(input$state, input$children)
        }
      
    })
    
    })
  
}
