
# UI ----------------------------------------------------------------------

mental_health_ui <- fluidPage(
  fluidRow(
    column(3, 
           p("who identify as", 
             style = "font-family:Campton-Light;font-size:24px"), 
           align = "left"), 
    column(5, 
           selectizeInput(
                    "races_mh",
                    label = NULL,
                    choices = c("Any Race/Ethnicity" = "Overall", 
                                "Asian" = "Asian alone, not Hispanic", 
                                  "Black" = "Black alone, not Hispanic", 
                                  "Hispanic or Latino" = "Hispanic or Latino, all Races", 
                                  "White" = "White alone, not Hispanic", 
                                  "2+ or Other Races" = "Two or more races or Other"), 
                    selected = c("Any Race/Ethnicity"),
                    multiple = F),
           align = "left")),
  br(), 
  fluidRow(column(6, 
                  span(h5("Frequency of Feeling Anxious or on Edge"),
                       style = "font-family:Campton"),
                  plotlyOutput("anxious")), 
                         #    hover = "anxious_hover")), 
           column(6, 
                  span(h5("Frequency of Feeling Down, Depressed, or Hopeless"), style = "font-family:Campton"),
                  plotlyOutput("down"))), 
                         #    hover = "down_hover"))), 
  fluidRow(column(6, span(h5("Frequency of Feeling Unable to Stop Worrying"), 
                          style = "font-family:Campton"),
                  plotlyOutput("worry")),
                           #  hover = "worry_hover")), 
           column(6, 
                  span(h5("Frequency of Having Little Interest or Pleasure in Doing Things"), style = "font-family:Campton"),
                  plotlyOutput("interest")))
                            # hover = "interest_hover")))
)


# Server ------------------------------------------------------------------

mental_health_server <- function(input, output){
  
  output$anxious <- renderPlotly({
    mental_health(input$state, input$children, "anxious", 
                  input$races_mh, c("Several Days", "More than Half the Days", "Nearly Every Day"))
  })
  #}, height = 350)
  
  output$worry <- renderPlotly({
    mental_health(input$state, input$children, "worry", 
                  input$races_mh, c("Several Days", "More than Half the Days", "Nearly Every Day"))
  })
 # }, height = 350)
  
  output$down <- renderPlotly({
    mental_health(input$state, input$children, "down", 
                  input$races_mh, c("Several Days", "More than Half the Days", "Nearly Every Day"))
  })
 # }, height = 350)
  
  output$interest <- renderPlotly({
    mental_health(input$state, input$children, "interest", 
                  input$races_mh, c("Several Days", "More than Half the Days", "Nearly Every Day"))
  })
  # }, height = 350)

  
  
}
