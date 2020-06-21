library(tidyverse)

input_filename <- 'final.csv'

maintidy <-
  read_csv(input_filename) %>%
  gather(key = 'param', value = 'value', year, votes, imdb_rating)

plotter <- function(paramname, breaklist){
  
  thistidy <- maintidy %>% filter(param==paramname)
  
  outliers <- thistidy %>%
    group_by(team) %>% 
    filter(value = max(value))
  
  tidysum <- thistidy %>% 
    group_by(team) %>% 
    summarise(minval = min(value), meanval = mean(value), maxval = max(value)) %>% 
    arrange(meanval)
  
  ggplot(data=thistidy, aes(x=reorder(team, -value), y=value, label = title)) +
    #geom_linerange(data=tidysum, aes(x=team, ymin=minval, ymax=maxval)) +
    geom_boxplot() +
    scale_y_continuous(breaks = breaklist) +
    geom_text() +
    #geom_point(data=thistidy, aes(x=team, y=value)) +
    coord_flip()
}

result <- df %>% 
  group_by(A, B) %>%
  filter(value == max(value)) %>%
  arrange(A,B,C)


plotter('year', seq(1950, 2020, 5))

plotter('imdb_rating', seq(0, 10, 0.25))

plotter('votes')
