library(tidyverse)
library(googlesheets4)
library(lubridate)

get_data <- function(){
    pandas_data <- read.csv("data/pandas_data.csv")
    period_data <- read.csv("data/periods.csv")
    coach_data <- read.csv("data/coach.csv")
    summary <- read.csv("data/summary.csv")
    return(list(pandas_data, period_data, coach_data, summary))
}

clean_data <- function(data, period_data, coach_data, summary){
    # Change the names
    names(data) <- c("player", "played", "starting_5", "cheered", "time", 
                     "points", "freethrow_attempts", "freethrow_points", 
                     "fouls", "comment", "game")
    names(summary) <- c('date', 'opponent', 'opponent_score', 'pandas_score', 'went_for_beer')
    names(period_data) <- c(paste0("points", 1:4), "team_fouls", "game")
    names(coach_data) <- c("time_outs", "coach_fouls", "coach_comments", "game")
    
    # Clean data andd join
    period_data <- period_data %>% 
        mutate("team"=rep(c("opponent", "pandas"), nrow(period_data)/2)) %>% 
        pivot_wider(names_from = team, values_from = c(points1, points2, points3, points4, team_fouls))
    
    summary <- summary %>% mutate(date = ymd(date),
                                  went_for_beer = went_for_beer=="yes",
                                  game=row_number()) %>% 
        left_join(., period_data, by=join_by(game)) %>% 
        left_join(., coach_data, by=join_by(game))
    
    data <- data %>% 
        mutate(time = sub("^(\\d{1,2}:\\d{1,2}).*$", "\\1", time)) %>% 
        mutate(time = ms(time)) %>% 
        mutate(across(c('played', 'starting_5', 'cheered'), ~.=="yes")) %>% 
        left_join(summary, by=join_by(game)) %>% 
        filter(player != "Friend")
    
    return(data)
}






