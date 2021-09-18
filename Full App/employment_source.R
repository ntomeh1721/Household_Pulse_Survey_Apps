
# UI  ---------------------------------------------------------------------

employment_ui <- fluidPage(
  
  
  fluidRow(
    column(5, 
           span(h4("Household Employment Income Loss Since March 13"), style = "font-family:Campton"),
           plotOutput("lost_work_donut"),
            align = "center"), 
    column(7, 
           span(h4("Work for Pay or Profit in the Past 7 Days"), style = "font-family:Campton"), 
           span(h6("As a Percentage of All Respondents"), style = "font-family:Campton"), 
           plotOutput("employed", 
                      hover = "employment_hover"), 
           align = "center")), 
    br(), 
    br(), 
    # Hover Features
    fluidRow(
      column(5, align = "center", 
             p(id = "showmore_left_emp", "Click on Graph for More Info"), 
             icon("angle-down")), 
      column(7, align = "center", 
             p(id = "showmore_right_emp", "Click on Graph for More Info"), 
             icon("angle-down")), 
      style = "font-family:AkkuratPro-Bold;font-size:13px;font-color:#716C6B"
      
    ), 
    
    br(), 
    hr(), 
  tags$style("#e_state_id {
  font-family:Campton-Light;
  font-size:18px;
  font-color:#BBB8B8
             } "), 
  fluidRow(column(12, 
                  align = "right", 
                  textOutput("e_state_id"))), 
    
    # Selected Titles 
    fluidRow(
      column(12, 
             h4(id = "lost_work_title", 
                "Household Employment Income Loss Since March 13:", 
                style = "font-family:Campton"),
             h5(id = "lost_work_title2", 
                "Respondents Reporting Loss of Household Employment Income Since March 13th, 2020, by Race/Ethnicity", 
                style = "font-family:Campton-Light"),
             
             h4(id = "employed_title", 
                "Work for Pay or Profit in the Past 7 Days:", 
                style = "font-family:Campton"), 
             h5(id = "employed_title2", 
                "Percent of All Respondents Reporting Having Worked For Pay/Profit in the Last 7 Days, by Race/Ethnicity", 
                style = "font-family:Campton-Light"), 
             
             br())), 
  fluidRow(column(12, 
                  checkboxGroupInput("races_emp2", 
                                     NULL, 
                                     c("Overall", ## REMOVE
                                       "Asian" = "Asian alone, not Hispanic", 
                                       "Black" = "Black alone, not Hispanic", 
                                       "Hispanic or Latino" = "Hispanic or Latino, all Races", 
                                       "White" = "White alone, not Hispanic", 
                                       "2+ or Other" = "Two or more races or Other"), 
                                     selected = c("Overall", ## REMOVE 
                                                  "Asian alone, not Hispanic", "Black alone, not Hispanic", 
                                                  "Hispanic or Latino, all Races", 
                                                  "White alone, not Hispanic", "Two or more races or Other"),
                                     inline = T
                  ), 
                  style = "font-family:Campton;font-size:15px", 
                  align = "center")), 
  
  br(), 
  fluidRow(column(12, 
                  plotOutput("employment_area_plot", 
                             hover = "area_hover_emp"), 
                  align = "center")), 
  fluidRow(column(12, 
                  plotlyOutput("lost_work_bar"), 
                  align = "center"))
)

# Server ------------------------------------------------------------------

employment_server <- function(input, output){
  
  output$lost_work_donut <- renderPlot({
    employment_donut_overall(input$state, input$children)
  })
  
  output$employed <- renderPlot({
   # overall_employment(input$state, input$children, "employed")
    overall_employment_perc(input$state, input$children)
  })

  output$employment_area_plot <- renderPlot({
    # employment_area(input$state, input$children, input$races_emp2, "employed")
    employment_by_race(input$state, input$children, input$races_emp2)
  })
  
  output$lost_work_bar <- renderPlotly({
    employment_bar(input$state, input$children)
  })
  
  output$e_state_id <- renderText({
    input$state
  })
  
  ### Show More Drop Down
  shinyjs::hide("showmore_left_emp")
  shinyjs::hide("showmore_right_emp")
  
  shinyjs::onevent("hover", "lost_work_donut", 
                   function(){
                     shinyjs::show("showmore_left_emp") 
                     shinyjs::hide("showmore_right_emp")
                   }
  )
  shinyjs::onevent("hover", "employed", 
                   function(){
                     shinyjs::hide("showmore_left_emp") 
                     shinyjs::show("showmore_right_emp")
                   } 
  )
  
  
  ### Titles + Control
  shinyjs::hide("lost_work_title")
  shinyjs::hide("employed_title")
  shinyjs::hide("lost_work_title2")
  shinyjs::hide("employed_title2")
  
  shinyjs::hide("e_state_id")
  
  shinyjs::hide("races_emp2")
  

  ### Graphs 
  shinyjs::hide("employment_area_plot")
  shinyjs::hide("lost_work_bar")
  
 shinyjs::onclick("lost_work_donut", 
                   function(){
                     shinyjs::show("lost_work_title") 
                     shinyjs::hide("employed_title")
                     shinyjs::show("lost_work_title2") 
                     shinyjs::hide("employed_title2")
                     shinyjs::hide("races_emp2")
                     shinyjs::hide("employment_area_plot")
                     shinyjs::show("e_state_id")
                     shinyjs::show("lost_work_bar")
                   })
  
  shinyjs::onclick("employed", 
                   function(){
                     shinyjs::hide("lost_work_title") 
                     shinyjs::show("employed_title")
                     shinyjs::hide("lost_work_title2") 
                     shinyjs::show("employed_title2")
                     shinyjs::show("races_emp2")
                     shinyjs::show("employment_area_plot")
                     shinyjs::show("e_state_id")
                     shinyjs::hide("lost_work_bar")
                   })
  
  employment_hover_data <- reactive({
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
      group_by(week, state, employed) %>% 
      summarize(people = sum(people)) %>% 
      na.omit(employed) %>% 
      pivot_wider(names_from = "employed", values_from = "people") %>% 
      mutate(rate = 100*yes/(yes+no))

    
    # With Test  
     df %>% 
      mutate(label = paste0(
        round(rate, 1) %>% format(nsmall = 1), 
        "%"
      ))

  })
  
  observeEvent(input$employment_hover, {
    
    df <- employment_hover_data()
    
    df <- df %>%
      nearPoints(input$employment_hover,
                 xvar = "week",
                 yvar = "rate")
    
    output$employed <- renderPlot({
      
      if(nrow(df) == 1){
        overall_employment_perc(input$state, input$children) +
          geom_label_repel(data = df,
                           aes(x = week, y = rate, label = label),
                           alpha = 0.7,
                           direction = "y",
                           family = "Akkurat Pro")
      } else {
        overall_employment_perc(input$state, input$children)
      }
      
    })
    
    
  })
  
  
  area_interactive_data <- reactive({
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
      group_by(week, state, employed) %>%
      summarize(people = sum(people)) %>%
      na.omit(employed) %>%
      pivot_wider(names_from = "employed", values_from = "people") %>%
      mutate(rate = 100*yes/(yes+no),
             race = "Overall")

    # Getting Data By Race
    df_race <- df1 %>%
      filter(state ==  input$state) %>%
      group_by(week, state, race, employed) %>%
      summarize(people = sum(people)) %>%
      na.omit(employed) %>%
      pivot_wider(names_from = "employed", values_from = "people") %>%
      mutate(rate = 100*yes/(yes+no))

    # Combining Data
    rbind(df_overall, df_race) %>%
      filter(race %in% input$races_emp2) %>%
      mutate(label = paste0(
        rate %>% round(1) %>% format(nsmall = 1),
        "%"
      ))

  })

  observeEvent(input$area_hover_emp, {

    aid <- area_interactive_data()

    hover_dat <- nearPoints(aid,
                            input$area_hover_emp,
                           xvar = "week",
                           yvar = "rate")

    ## Redefining the Plot
    output$employment_area_plot <- renderPlot({

      if(nrow(hover_dat) == 1){
        employment_by_race(input$state, input$children, input$races_emp2) +
          geom_label_repel(data = hover_dat,
                           aes(x = week, y = rate, label = label),
                           alpha = 0.7,
                           direction = "y",
                           family = "Akkurat Pro")

      } else {
        employment_by_race(input$state, input$children, input$races_emp2)
      }

    })
  })




 
}

