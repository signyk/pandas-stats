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

plot_periods_boxplot <- function(){
    plot_data <- data %>% select(points1_opponent:points4_pandas, game) %>% 
        unique() %>% 
        pivot_longer(names_to="key", values_to="points", points1_opponent:points4_pandas) %>% 
        mutate(team=substr(key, 9, 20)) %>% mutate(period=substr(key, 7, 7)) %>% select(-key)
    
    p <- plot_data %>% ggplot(aes(x=period, y=points, fill=team)) + 
        geom_boxplot(alpha=0.7) + scale_fill_manual(values=c("indianred3", "dodgerblue")) +
        labs(x="Period", y="Points", fill="")
    
    return(p)
}



