
# Ui ----------------------------------------------------------------------
finances_ui <- fluidPage(
  fluidRow(
    column(4, 
           p("who self-identify as", 
             style = "font-family:Campton-Light;font-size:20px;padding:10px"), 
           align = "left"), 
    column(8, 
           selectizeInput(
             "races_finances",
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
  fluidRow(
    column(8, 
           p("and reported that paying for usual household expenses in the past 7 days was:", 
             style = "font-family:Campton-Light;font-size:20px;padding:20px"), 
           align = "left", 
           ), 
    column(4, 
                  checkboxGroupInput("difficulty_level", 
                                     NULL, 
                                     c("Not Difficult" = "not",
                                       "A Little Difficult" = "a_little", 
                                       "Somewhat Difficult" = "somewhat",
                                       "Very Difficult" = "very"), 
                                     selected = c("not", "a_little", 
                                                  "somewhat", "very")
                  ), 
                  style = "font-family:Campton;font-size:15px", 
                  align = "left")),
  br(), 
  fluidRow(plotlyOutput("difficulties_graph")), 
  br(), 
  br(), 
  fluidRow(column(12, 
                  p("to meet their spending needs they used some combination of:", 
                    style = "font-family:Campton-Light;font-size:20px;padding:20px"),
                  align = "left"), 
           br(), 
           column(12, 
                  checkboxGroupInput("source_of_spending", 
                                     NULL, 
                                     c("Money From Friends or Relatives" = "Borrowing From Friends or Relatives",
                                       "Savings or Assests",
                                       "Credit Card Loans",
                                       "Deferred Payments",
                                       "Unemployment Insurance",
                                       "SNAP Benefits", 
                                       "Government Stimulus Payment",
                                       "Regular Income"
                                     ), 
                                     selected = c(
                                       "Borrowing From Friends or Relatives",
                                       "Savings or Assests",
                                       "Credit Card Loans",
                                       "Deferred Payments",
                                       "Unemployment Insurance",
                                       "SNAP Benefits", 
                                       "Government Stimulus Payment",
                                       "Regular Income"
                                     ), 
                                     inline = T
                  ), 
                  style = "font-family:Campton;font-size:15px", 
                  align = "center")), 
  br(), 
  br(), 
  fluidRow(
    plotlyOutput("sources_graph")
  )
)
# Server ------------------------------------------------------------------

finances_server <- function(input, output){

  output$sources_graph <- renderPlotly({
    finances_header_func(input$state, input$children, 
                         input$races_finances, 
                         input$source_of_spending, 
                         input$difficulty_level)
  })
  
  output$difficulties_graph <- renderPlotly({
    finances_stacker(input$state, input$children, input$races_finances, input$difficulty_level)
  })
  
}