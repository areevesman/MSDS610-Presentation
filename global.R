library(shiny)
library(shinythemes)
library(RCurl)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(plotly)

### Functions to interact with github reposity and manipulate data ###

data_folder <- "https://raw.githubusercontent.com/areevesman/mlb_app_edits/master/data/"


#read in a csv file by name from data_url
#ex: get_table_by_name("STL2006.csv")
get_csv_by_name <- function(name, data_url=data_folder){
  myCSV <- getURL(paste(data_url, name, sep=''))
  myDF <- read.csv(text=myCSV, stringsAsFactors = F) %>% 
    as_tibble()
  return(myDF)
}


#add columns to the original team data from github
#year argument allows a coulmn to be added for year
add_columns <- function(original_team_data, year){
  
  team_data <- original_team_data
  
  team_data$year <- rep(year, times = nrow(original_team_data))
  team_data$games_ahead <- str_replace_all(team_data$games_behind, 'Tied', '0')
  team_data$games_ahead <- gsub(pattern = 'up', replacement = '-', x = team_data$games_ahead)
  team_data$games_ahead <- gsub(pattern = ' ', replacement = '', x = team_data$games_ahead)
  team_data$games_ahead <- -1*as.numeric(team_data$games_ahead)
  team_data$run_diff <- team_data$runs - team_data$runs_allowed
  team_data$r_so_far <- cumsum(team_data$runs)
  team_data$ra_so_far = cumsum(team_data$runs_allowed)
  team_data$rd_so_far = cumsum(team_data$run_diff)
  team_data$wins_so_far = cumsum(!grepl(x = team_data$win_or_loss, pattern='L'))
  team_data$losses_so_far = cumsum(!grepl(x = team_data$win_or_loss, pattern='W'))
  team_data$win_loss_differential = team_data$wins_so_far - team_data$losses_so_far
  team_data$record_so_far =  team_data$wins_so_far / (team_data$wins_so_far + team_data$losses_so_far)
  team_data$who_and_where = ifelse(team_data$home_or_away == "@", 
                                   paste(team_data$team, "@", team_data$opponent),
                                   paste(team_data$opponent, "@", team_data$team))
  team_data$winner = ifelse(team_data$runs > team_data$runs_allowed, 
                            team_data$team,
                            team_data$opponent)
  
  return(team_data)
}


#get one team's csv's over all years in one csv
combine_years <- function(team, start_year){
  return(get_csv_by_name(paste(team, "_all_years.csv", sep="")))
}



# get teamID data and fix minor data inconsistencies
teamIDs <- get_csv_by_name("teamIDs.csv")
teamIDs[1,] <- c("LAA", "LAA", "Los Angeles Angels of Anaheim", 2005)
teamIDs <- teamIDs[c(2:14,1,15:nrow(teamIDs)),]



### Define Global Variables ###

#all mlb teams (as "STL")
team_choices <- teamIDs$Team_ID
#number of years team has been around
team_num_years <- 2018 - as.integer(teamIDs$First_Year)
#name the vectors with full team names
names(team_choices) <- teamIDs$Full_Team_Name
names(team_num_years) <- teamIDs$Full_Team_Name

#choices of statistics to select
select_stat_choices <- list("Record" = "record_so_far",
                            "Win-Loss Differential" = "win_loss_differential",
                            "Games Ahead/Behind in Division" = "games_ahead",
                            "Cumulative Wins" = "wins_so_far",
                            "Runs" = "runs", 
                            "Runs Allowed" = "runs_allowed",
                            "Run Differential" = "run_diff",
                            "Cumulative Runs" = "r_so_far", 
                            "Cumulative Runs Allowed" = "ra_so_far",
                            "Cumulative Run Differential" = "rd_so_far")