get_freethrow_stats <- function(player_name="team"){
    prop <- data
    if(player_name != "team"){
        prop <- prop %>% 
            filter(player == player_name, played)
    }
    
    if(sum(prop$freethrow_attempts)==0){
        return(NA)
    } else{
        return(sum(prop$freethrow_points)/sum(prop$freethrow_attempts))
    }
}

get_number_of_games <- function(player_name){
    num_games <- data %>% 
        filter(player == player_name, played) %>% 
        nrow()
    return(num_games)
}

more_fouls_than_points <- function(player_name="team"){
    if(player_name=="team"){
        num_games <- data %>% 
            group_by(date, opponent) %>% 
            summarise(total_points=sum(points), total_fouls=sum(fouls), .groups = "keep") %>% 
            filter(total_points<total_fouls) %>% 
            nrow()
    } else{
        num_games <- data %>% 
            filter(player == player_name, played, points<fouls) %>% 
            nrow()
    }
    return(num_games)
}

get_point_stats <- function(player_name="team"){
    if(player_name!="team"){
        return_dat <- data %>% 
            filter(player == player_name, played) %>% 
            summarise("most points"=max(points), 
                      "average points" = mean(points),
                      "total points" = sum(points))
    } else{
        return_dat <- data %>% 
            select(date, opponent, pandas_score, opponent_score) %>% 
            unique() %>% 
            gather(key=key, value=value, pandas_score, opponent_score) %>% 
            group_by(key) %>% 
            summarise("most points"=max(value), 
                      "average points" = mean(value),
                      "total points" = sum(value))
    }
    return(return_dat)
}

get_foul_stats <- function(player_name="team"){
    if(player_name!="team"){
        return_dat <- data %>% 
            filter(player == player_name, played) %>% 
            summarise("most fouls"=max(fouls), 
                      "average fouls" = mean(fouls),
                      "total fouls" = sum(fouls))
    } else{
        return_dat <- data %>% 
            select(date, opponent, fouls) %>% 
            group_by(date, opponent) %>% 
            summarise(fouls=sum(fouls), .groups = "keep") %>% 
            ungroup() %>% 
            summarise("most fouls"=max(fouls), 
                      "average fouls" = mean(fouls),
                      "total fouls" = sum(fouls))
    }
    return(return_dat)
}

starting_five <- function(player_name){
    num_games <- data %>% 
        filter(player == player_name, played, starting_5) %>% 
        nrow()
    return(num_games)
}

get_time_stats <- function(player_name){
    return(data %>% 
               filter(player == player_name, played) %>% 
               summarise("most minutes played"= round((time %>% seconds() %>% max())/60,1), 
                         "average minutes played" = round((time %>% seconds() %>% mean())/60,1)))
}


