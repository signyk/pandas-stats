library(tidyverse)
library(grid)
library(gridExtra)

source("make_data.R")
source("plot_functions.R")
source("help_functions.R")

data_list <- get_data()
data_raw <- data_list[[1]]
period_data <- data_list[[2]]
coach_data <- data_list[[3]]
summary <- data_list[[4]]

data <- clean_data(data_raw, summary)

# Total stats
## Plots
dates <- data$date %>% unique()
colors <- rainbow(length(dates))

game_plots <- mapply(plot_game_points, dates, colors, SIMPLIFY = F)
grid.arrange(grobs=game_plots, ncol=2)
plot1 <- arrangeGrob(grobs=game_plots, ncol=2)
ggsave(file="plots/plot1.png", plot1, width = 12, height = 12)

plot2 <- plot_periods_boxplot()
ggsave(file="plots/plot2.png", plot2, width = 8, height = 5)

## Stats
cat(paste0("Number of games with more fouls than points: ", more_fouls_than_points()))
cat(paste0("Freethrow rate: ", 100*(get_freethrow_stats() %>% round(.,4)), "%"))
get_point_stats()
get_foul_stats()
get_periods_stats()



# Per player
players <- data %>% pull(player) %>% unique()
player_name <- "Alexandra"

plot_player_points(player_name)
freethrow_rate <- get_freethrow_stats(player_name)
cat(paste0("Freethrow rate: ", 100*(get_freethrow_stats(player_name) %>% round(.,4)), "%"))
cat(paste0("Number of games played: ", get_number_of_games(player_name)))
cat(paste0("In starting five: ", starting_five(player_name)))
cat(paste0("Number of games with more fouls than points: ", more_fouls_than_points(player_name)))
get_point_stats(player_name)
get_foul_stats(player_name)
get_time_stats(player_name)




