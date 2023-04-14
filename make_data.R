library(tidyverse)
library(googlesheets4)
library(lubridate)

get_data <- function(){
    data <- read.csv("data/pandas_data.csv")
    summary <- read.csv("data/summary.csv")
    return(list(data, summary))
}

clean_data <- function(data, summary){
    names(data) <- c("player", "played", "starting_5", "cheered", "time", 
                     "points", "freethrow_attempts", "freethrow_points", 
                     "fouls", "comment", "game")
    names(summary) <- c('date', 'opponent', 'opponent_score', 'pandas_score', 'went_for_beer')
    summary <- summary %>% mutate(date = ymd(date),
                                  went_for_beer = went_for_beer=="yes",
                                  game=row_number())
    
    data <- data %>% 
        mutate(time = sub("^(\\d{1,2}:\\d{1,2}).*$", "\\1", time)) %>% 
        mutate(time = ms(time)) %>% 
        mutate(across(c('played', 'starting_5', 'cheered'), ~.=="yes")) %>% 
        left_join(summary, by=join_by(game)) %>% 
        filter(player != "Friend")
    
    return(data)
}






