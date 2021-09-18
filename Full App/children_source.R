
# UI  ---------------------------------------------------------------------
children_ui <- fluidPage(
  
  fluidRow(
    column(12, 
           span(h4("Frequency of Resource Availability for Children's Education"), 
                style = "font-family:Campton"),
          )
  ), 
  br(), 
  fluidRow(
    column(6, 
           span(h5("Computer"), 
                style = "font-family:Campton"),
           plotOutput("comp_available"),
           align = "center"),
    column(6, 
           span(h5("Wifi"), 
           style = "font-family:Campton"), 
           plotOutput("wifi_available"),
           align = "center")), 
  br(), 
  br(),
  
  fluidRow(
    column(6, align = "center", 
           p(id = "showmore_left_kids", "Click on Graph for More Info"), 
           icon("angle-down")), 
    column(6, align = "center", 
           p(id = "showmore_right_kids", "Click on Graph for More Info"), 
           icon("angle-down")), 
    style = "font-family:AkkuratPro-Bold;font-size:13px;font-color:#716C6B"
    
  ), 
  br(),
  hr(), 
  tags$style("#ke_state_id {
  font-family:Campton;
  font-size:18px;
  font-color:##BBB8B8
             } "), 
  fluidRow(column(12, 
                  align = "right", 
                  textOutput("ke_state_id"))), 
  fluidRow(
    column(12, 
           h4(id = "computer_title", 
              "Computer:", 
              style = "font-family:Campton"),
           h5(id = "computer_title2", 
              "Frequency of Availability for Children's Education, by Race", 
              style = "font-family:Campton-Light"),
           
           h4(id = "wifi_title", 
              "Wifi:", 
              style = "font-family:Campton"), 
           h5(id = "wifi_title2", 
              "Frequency of Availability for Children's Education, by Race", 
              style = "font-family:Campton-Light"), 
           
           br())), 
  
  br(), 
  fluidRow(column(12, 
                  checkboxGroupInput("frequencies", 
                                     NULL, 
                                     c("Always", 
                                       "Usually", 
                                       "Sometimes", 
                                       "Rarely",
                                       "Never"), 
                                     selected = c("Sometimes", 
                                                  "Rarely",
                                                  "Never"),
                                     inline = T
                  ), 
                  style = "font-family:Campton;font-size:15px", 
                  align = "center")), 
  
  br(), 
  br(),
  fluidRow(
    column(12, 
           plotlyOutput("computer_availability_race"),
           plotlyOutput("wifi_availability_race")
  )
  
)

) 

# Server ------------------------------------------------------------------

children_server <- function(input, output){
  
  output$comp_available <- renderPlot({
    children_donut_overall(input$state, input$children, "comp_available")
  })
  
  output$wifi_available <- renderPlot({
    children_donut_overall(input$state, input$children, "wifi_avail")
  })
  
  output$computer_availability_race <- renderPlotly({
    availability(input$state, "comp", input$frequencies)
  })
  
  output$wifi_availability_race <- renderPlotly({
    availability(input$state, "wifi", input$frequencies)
  })
  
  output$ke_state_id <- renderText({
    input$state
  })
  
  ### Show More Drop Down
  shinyjs::hide("showmore_left_kids")
  shinyjs::hide("showmore_right_kids")
  
  shinyjs::onevent("hover", "comp_available", 
                   function(){
                     shinyjs::show("showmore_left_kids") 
                     shinyjs::hide("showmore_right_kids")
                     
                   }
  )
  shinyjs::onevent("hover", "wifi_available", 
                   function(){
                     shinyjs::hide("showmore_left_kids") 
                     shinyjs::show("showmore_right_kids")
                   } 
  )
  
  
  ### Titles + Control
  shinyjs::hide("computer_title")
  shinyjs::hide("wifi_title")
  shinyjs::hide("computer_title2")
  shinyjs::hide("wifi_title2")
  shinyjs::hide("frequencies")
  
  shinyjs::hide("ke_state_id")
  
  ### Graphs 
  shinyjs::hide("computer_availability_race")
  shinyjs::hide("wifi_availability_race")
  
  shinyjs::onclick("comp_available", 
                   function(){
                     shinyjs::show("computer_title") 
                     shinyjs::hide("wifi_title")
                     shinyjs::show("computer_availability_race")
                     shinyjs::hide("wifi_availability_race")
                     shinyjs::show("frequencies")
                     shinyjs::show("ke_state_id")
                     shinyjs::show("computer_title2")
                     shinyjs::hide("wifi_title2")
                   })
  
  shinyjs::onclick("wifi_available", 
                   function(){
                     shinyjs::hide("computer_title") 
                     shinyjs::show("wifi_title")
                     shinyjs::hide("computer_availability_race")
                     shinyjs::show("wifi_availability_race")
                     shinyjs::show("frequencies")
                     shinyjs::show("ke_state_id")
                     shinyjs::hide("computer_title2")
                     shinyjs::show("wifi_title2")
                     })
  
  
}




