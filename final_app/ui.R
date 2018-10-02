#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application
shinyUI(fluidPage(
  
  #select theme
  theme = shinytheme("yeti"),
  
  
  # Application title
  titlePanel("A History of Major League Baseball's Active Franchises"),
  
  p("You can use this application to examine various performance metrics of Major League Baseball teams. Just select a ", 
    strong("Team,"),
    strong("Year"),
    " and the ", 
    strong("Statistic") ," of interest."),
  
  p('Click the "Play Animation" button in the ', strong('Year'), ' section to see how performance changes over multiple years.'),
  
  p("Use the buttons in the top-right corner of the plot for a closer look."),
  
  
  
  # select layout 
  verticalLayout(
    
    # create panel with a select box for team
    wellPanel(
      
      #select box for team
      selectInput("select_team", 
                  label = h3("Team"), 
                  choices = team_choices, 
                  selected = team_choices[1]),
      
      #tell server to render more some ui (for slider box)
      uiOutput("team"),
      
      #select box for statistic
      selectInput("select_stat", 
                  label = h3("Statistic"),
                  choices = select_stat_choices,
                  selected = "Runs"),
      
      #will tell server to render plotly object
      plotlyOutput("detailed_plot")
      
      #for debugging purposes
      ,textOutput("test_print")
    )
  )
))