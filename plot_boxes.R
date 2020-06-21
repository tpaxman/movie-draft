library(tidyverse)

input_filename <- 'final.csv'

maintidy <-
  read_csv(input_filename) %>%
  gather(key = 'param', value = 'value', year, votes, imdb_rating)

plotter <- function(paramname, breaklist){
  
  thistidy <- maintidy %>% filter(param==paramname)
  
  outliers <- thistidy %>%
    group_by(team) %>% 
    filter((value == max(value)) | (value == min(value)))

  ggplot(data = thistidy, aes(x=reorder(team, -value), y=value)) +
    geom_boxplot() +
    scale_y_continuous(breaks = breaklist) +
    geom_text(data = outliers, aes(x = team, y = value, label = title), hjust = 0, vjust = -1) +
    geom_point(data = outliers, aes(x = team, y = value)) +
    coord_flip()
}


plotter('year', seq(1950, 2020, 5))

plotter('imdb_rating', seq(0, 10, 0.25))

plotter('votes')
