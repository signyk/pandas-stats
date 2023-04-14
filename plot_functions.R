library(tidyverse)
theme_set(theme_light())

plot_player_points <- function(player_name){
    plot_data <- data %>% 
        filter(player==player_name, played)
    
    p <- plot_data %>% 
        ggplot(aes(x=as.character(game), y=points)) + 
        geom_col(color="black", fill="dodgerblue", alpha=0.5) +
        geom_text(aes(x=as.character(game), y=points, label=opponent), size=3, angle=90, nudge_y = 1) +
        ylim(0, max(plot_data$points)+2) +
        labs(x="Game", y="Points") +
        theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())
    
    return(p)
}

plot_game_points <- function(game_date, color){
    plot_data <- data %>% 
        filter(date==game_date, played)
    
    plot_data %>% ggplot(aes(x=player, y=points)) +
        geom_col(alpha=0.7, color="black", fill=color) +
        labs(title=plot_data$opponent[1], x="", y="Points")
}



