#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic 
shinyServer(function(input, output) {
  
  
  #first year in mlb for selected team
  start_year <- reactive({
    as.numeric(teamIDs[which(teamIDs$Team_ID == input$select_team), "First_Year"][[1,1]])
  })
  
  
  #server will render some ui corresponding to "team" (slider box)
  output$team <- renderUI({
    
    sliderInput("slider", 
                label = h3("Year"), 
                min = start_year(), 
                max = 2017, 
                value = start_year(),
                sep = "",
                round = TRUE,
                step = 1,
                animate = animationOptions(interval = 900, 
                                           playButton = "Play Animation"))
  })
    
    
    
    #get a team's data over all years as a tibble
    team_all_years <- reactive({
      combine_years(input$select_team)
    })
    
    #get limits for plot
    limit_data <- reactive({
      
      stat <- input$select_stat
      #get min and max for stat over all years
      var <- team_all_years()[,c("year", stat)] %>%
        mutate_all(function(col){ 
          col[is.na(col)] <- "0"
          col <- as.numeric(col)
          col}) %>%
        summarise(minimum = min(as.numeric(eval(parse(text = input$select_stat))), na.rm = TRUE),
                  maximum = max(as.numeric(eval(parse(text = input$select_stat))), na.rm = TRUE)) 
      
      
      #lower and upper bound for plot y-axis limits
      c(var[[1,"minimum"]], 1.1*var[[1,"maximum"]])
      
    })
    
    
    
    #for plotly
    team_year_data <- reactive({
      get_csv_by_name(paste(input$select_team,
                            input$slider,
                            ".csv",
                            sep="")) %>%
        add_columns(year = input$slider)
    })
    
    
    
    #render the plotly object
    output$detailed_plot <- renderPlotly({
      
      x <- 1:nrow(team_year_data())
      y <- team_year_data()[[input$select_stat]]
      hover <- paste(team_year_data()[["date"]], "\n",
                     team_year_data()[["who_and_where"]], "\n",
                     "Score: ", team_year_data()[["runs"]], "-", team_year_data()[["runs_allowed"]],
                     ", ", team_year_data()[["winner"]], "\n",
                     sep = '')
      
      team_year_data() %>%
        plot_ly(x = ~x,
                y = ~y, 
                hovertext = hover,
                hoverinfo = "text",
                type = 'scatter', 
                mode = 'lines+markers') %>%
        layout(xaxis = list(title = "Game Number in Season"),
               yaxis = list(title = names(select_stat_choices[select_stat_choices==input$select_stat]),
                            range = limit_data()))
      
    })
    
    
  })
  
  
  
  
  
  
  
  
   
  # output$distPlot <- renderPlot({
  #   
  #   # generate bins based on input$bins from ui.R
  #   x    <- faithful[, 2] 
  #   bins <- seq(min(x), max(x), length.out = input$bins + 1)
  #   
  #   # draw the histogram with the specified number of bins
  #   hist(x, breaks = bins, col = 'darkgray', border = 'white')
  #   
  # })
  
