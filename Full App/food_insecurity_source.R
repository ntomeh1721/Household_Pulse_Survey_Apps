


# UI ----------------------------------------------------------------------

food_insecurity_ui <- fluidPage(
  
  fluidRow(
    column(4, 
           dataTableOutput("fi_table"), 
           style = "height: 420px; overflow-y: scroll; font-family:AkkuratPro-light",
           align = "center"), 
    column(8, 
           span(h4("Weekly Food Insecurity"), style = "font-family:Campton"), 
           plotOutput("fi_summary",
                      hover = hoverOpts(id = "plot1_hover",
                                        delay = 100, 
                                        delayType = "debounce",
                                        clip = T, 
                                        nullOutside = T)),
           br())), 
  
  br(), 
  
  # Three Graphs 
  fluidRow(
    column(6, 
           span(h5("Sometimes or Often Not Had Enough to Eat, Past 7 Days"), 
                style = "font-family:Campton; font-size:16px"),
           plotOutput("fi_overall"), 
           style = "height:350px"
    ), 
  column(6, 
         span(h5("Food Insecurity, by Race/Ethnicity"), 
              style = "font-family:Campton; font-size:16px"),
         plotOutput("fi_by_race"), 
         style = "height:350px")
  ), 
  br(), 
  
  # Follow Up Prompts 
  
  fluidRow(
    column(6, align = "center", 
           p(id = "showmore_left", "Click on Graph for More Info"), 
           icon("angle-down")), 
    column(6, align = "center", 
           p(id = "showmore_right", "Click on Graph for More Info"), 
           icon("angle-down")), 
    style = "font-family:AkkuratPro-Bold;font-size:13px;font-color:#716C6B"
    
  ), 
  br(), 
  hr(), 
  
  ### STATE ID 
  tags$style("#fi_state_id {
  font-family:Campton-Light;
  font-size:18px;
  font-color:#BBB8B8
             } "), 
  fluidRow(column(12, 
                  align = "right", 
                 textOutput("fi_state_id"))), 
  
  ### Follow Up Graphs 
  
  
  ####### Titles 
  fluidRow(
    column(12, 
           h4(id = "fi_sufficiency_title", 
              "Frequency of Not Having Enough to Eat:", 
              style = "font-family:Campton"),
           h5(id = "fi_sufficiency_title2", 
              "Frequency of Household Not Having Enough to Eat in the Past 7 Days, by Race/Ethnicity", 
              style = "font-family:Campton-Light; font-color:#716C6B"), 
           h4(id = "fi_weekly_title", 
              "Food Insecurity:", 
              style = "font-family:Campton"), 
           h5(id = "fi_weekly_title2", 
              "Weekly Rates per Race/Ethnicity and by Race/Ethnicity", 
              style = "font-family:Campton-Light; font-color:#716C6B"), 
           br())), 
  
  br(),
  
  fluidRow(column(12, 
                  checkboxGroupInput("races_fi", 
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
  br(), 
  fluidRow(column(12, 
                  checkboxGroupInput("responses_fi", 
                                     NULL, 
                                     c("Never", "Sometimes", "Often"), 
                                     selected = c("Sometimes", "Often"),
                                     inline = T
                  ), 
                  style = "font-family:Campton;font-size:15px", 
                  align = "center")), 
  
  br(), 
  
  ######## The Graphs themselves 
  
  fluidRow(
    column(12, 
    plotlyOutput("fi_sufficiency_graphs",
                 width = "auto", 
                 height = "900px"
               ))), 
  fluidRow(
    h6(id = "weekly_race_title", 
       "Weekly Rates of Food Insecurity for Selected Racial/Ethnic Groups", 
       style = "font-family:Campton; font-size:16px"),
    
     plotOutput("fi_weekly_by_race", 
                hover = "race_weekly_hover"), 
    br(),
    h6(id = "fi_weekly_bar_title", 
       "Share of Weekly Overall Food Insecurity by Race/Ethnicity for Selected State", 
       style = "font-family:Campton; font-size:16px"),
    plotlyOutput("fi_weekly_bar", width = "auto", height = "auto"),
    align = "left"
  ))




# Server ------------------------------------------------------------------

fi_server <- function(input, output) {
  
  
  output$fi_table <- renderDataTable({
    show_most_recent(input$state, input$children)
  },
  options = list(
    dom = "t",
    paging = F
  ), 
  rownames = F, 
  class = "row-border",
  )
  
  
  ### Plot1: All States Plot 
  output$fi_summary <- renderPlot({
    fi_summary_graph(input$state, input$children)
  })
  
  output$fi_state_id <- renderText({
    input$state
  })
    
    
  observeEvent(input$plot1_hover, {

    hover_data <- fi_data(input$children) %>%
      filter(state == "USA" | state == input$state,
             race == "All") %>%
      select(week, state, rate) %>%
      nearPoints(input$plot1_hover,
                 xvar = "week",
                 yvar = "rate") %>%
      mutate(label = paste0(format(round(rate, 1), nsmall = 1), "%"))

    if(nrow(hover_data) == 1){
      output$fi_summary <- renderPlot({
        fi_summary_graph(input$state, input$children) +
          geom_label(data = hover_data,
                     aes(x = week, y = rate, label = label),
                     alpha = 0.7, nudge_y = 2,
                     family = "Akkurat Pro")
      })
    }


  })

  
  ### Plot2: By Race Plot 
  output$fi_by_race <- renderPlot({
   fi_race(input$children, input$state)
  }, height = 300)
  
  # Plot 3: Food Sufficiency sufficiency 
  output$fi_overall <- renderPlot({
    stacker_head(input$state, input$children, "food_sufficiency") 
  }, height = 300)
  
  ## Sub Plots 
  
  output$fi_sufficiency_graphs <- renderPlotly({
    stacker(input$state, input$children, "food_sufficiency", input$races_fi, input$responses_fi)
  }) #, height = reactive(ceiling(length(input$races_fi)/2)*300))
  
  output$fi_weekly_by_race <- renderPlot({
    race_line(input$state, input$children, input$races_fi)
  })
  
  output$fi_weekly_bar <- renderPlotly({
    fi_race_bar(input$state, input$children, input$races_fi)
  })
  
  
  
  # Hide and Show Controls 
  
  ### Show More Drop Down
  shinyjs::hide("showmore_left")
  shinyjs::hide("showmore_mid") 
  shinyjs::hide("showmore_right")
  
  ### Titles + Control
  shinyjs::hide("fi_weekly_title")
  shinyjs::hide("fi_sufficiency_title")
  shinyjs::hide("fi_sufficiency_title2")
  shinyjs::hide("fi_weekly_title2")
  shinyjs::hide("weekly_race_title")
  shinyjs::hide("fi_weekly_bar_title")
  
  shinyjs::hide("races_fi")
  shinyjs::hide("responses_fi")
  
  shinyjs::hide("fi_state_id")
  
  ### Graphs
  shinyjs::hide("fi_sufficiency_graphs")
  shinyjs::hide("fi_weekly_by_race")
  shinyjs::hide("fi_weekly_bar") 
  
  ## Interactions 
  
  shinyjs::onevent("hover", "fi_overall", 
                   function(){
                     shinyjs::show("showmore_left") 
                     shinyjs::hide("showmore_mid")
                     shinyjs::hide("showmore_right")
                   }
  )
  
  shinyjs::onevent("hover", "fi_by_race", 
                   function(){
                     shinyjs::show("showmore_right")
                     shinyjs::hide("showmore_left")
                     shinyjs::hide("showmore_mid")
                   }
  )
  
  
  shinyjs::onclick("fi_overall", 
                   function() {
                     shinyjs::show("fi_sufficiency_title") 
                     shinyjs::show("fi_sufficiency_title2") 
                     shinyjs::show("races_fi") 
                     shinyjs::show("responses_fi")
                     shinyjs::show("fi_sufficiency_graphs")
                     shinyjs::hide("fi_weekly_title")
                     shinyjs::hide("fi_weekly_title2")
                     shinyjs::hide("fi_weekly_by_race")
                     shinyjs::hide("fi_weekly_bar")
                     shinyjs::hide("weekly_race_title")
                     shinyjs::hide("fi_weekly_bar_title")
                     shinyjs::show("fi_state_id")
                   })
  
  
  shinyjs::onclick("fi_by_race", 
                   function(){
                     shinyjs::hide("fi_sufficiency_title")
                     shinyjs::show("races_fi")
                     shinyjs::hide("responses_fi")
                     shinyjs::show("fi_weekly_title") 
                     shinyjs::hide("fi_sufficiency_graphs")
                     shinyjs::show("fi_weekly_by_race")
                     shinyjs::show("fi_weekly_bar") 
                     shinyjs::show("weekly_race_title")
                     shinyjs::show("fi_weekly_bar_title")
                     shinyjs::show("fi_state_id")
                     shinyjs::show("fi_weekly_title2")
                     shinyjs::hide("fi_sufficiency_title2")
                   }
  )
 
# Observe Values 

  ## By Race Weekly
  
  race_weekly_hover_data <- reactive({
    fi_data(input$children) %>%
      mutate(race = ifelse(race == "All", "Overall", race)) %>%
      filter(state == input$state,
             race %in% input$races_fi) %>%
      select(week, rate)
  })

  observeEvent(input$race_weekly_hover, {
    race_weekly_dat <- race_weekly_hover_data()
    hover_data <-  race_weekly_dat %>%
      nearPoints(input$race_weekly_hover,
                 xvar = "week",
                 yvar = "rate") %>%
      mutate(label = paste0(format(round(rate, 1), nsmall = 1), "%"))

    if(nrow(hover_data) == 1){
      output$fi_weekly_by_race <- renderPlot({
        race_line(input$state, input$children, input$races_fi) +
          geom_label(data = hover_data,
                     aes(x = week, y = rate, label = label),
                     alpha = 0.7, nudge_y = 2,
                     family = "Akkurat Pro")
      })
    }


  })
  

  
}

