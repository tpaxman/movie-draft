library(tidyverse)

input_filename <- 'final.csv'

maintidy <-
  read_csv(input_filename) %>%
  gather(key = 'param', value = 'value', year, votes, imdb_rating)

plotter <- function(paramname, breaklist){
  
  thistidy <- maintidy %>% filter(param==paramname)
  
  maxes <- thistidy %>% group_by(team) %>% filter((value == max(value)))
  mins <- thistidy %>% group_by(team) %>% filter((value == min(value)))

  ggplot(data = thistidy, aes(x=reorder(team, -value), y=value)) +
    geom_boxplot(aes(fill = team)) +
    scale_y_continuous(breaks = breaklist) +
    geom_text(data = maxes, aes(x = team, y = value, label = title), hjust = 1, vjust = -1) +
    geom_text(data = mins, aes(x = team, y = value, label = title), hjust = 0, vjust = -1) +
    geom_point(data = maxes, aes(x = team, y = value)) +
    geom_point(data = mins, aes(x = team, y = value)) +
    coord_flip()
}


plotter('year', seq(1950, 2020, 5))

plotter('imdb_rating', seq(0, 10, 0.5))

plotter('votes')
