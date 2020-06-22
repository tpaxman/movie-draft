library(tidyverse)

input_filename <- 'final.csv'

maintidy <-
  read_csv(input_filename) %>%
  gather(key = 'param', value = 'value', year, votes, imdb_rating) %>% 
  mutate(title = str_replace(title, 'Star Wars: Episode V - The Empire Strikes Back', 'The Empire Strikes Back'),
         title = str_replace(title, 'The Lord of the Rings: The Return of the King', 'The Return of the King'),
         title = str_replace(title, 'Indiana Jones and the ', ''))

plotter <- function(paramname, breaklist, limitlist){
  
  valuename <- ifelse(paramname=='year', 'Release Year', 'IMDB Rating')
  
  thistidy <- maintidy %>%
    filter(param==paramname) %>%
    group_by(team)
  
  maxes <- thistidy %>% filter(value == max(value)) %>% mutate(aggtype = 'max')
  mins <- thistidy %>% filter(value==min(value)) %>% mutate(aggtype = 'min')
  outliers <- bind_rows(maxes, mins) %>% 
    group_by(team, value, aggtype) %>% 
    mutate(title_list = paste0(title, collapse = ',\n')) %>% 
    select(-title, -id) %>% 
    distinct()

  ggplot(data = thistidy, aes(x=reorder(team, value), y=value)) +
    geom_boxplot(aes(fill = team), size = 1) +
    scale_y_continuous(breaks = breaklist, limits = limitlist) +
    labs(x = NULL, y = valuename) +
    geom_text(data = outliers, aes(x = team, y = value, label = title_list, hjust = aggtype), vjust = 0.5, show.legend = FALSE) +
    scale_discrete_manual(aesthetics = 'hjust', values = c(-0.1,1.1)) +
    geom_point(data = outliers, aes(x = team, y = value), size = 2) +
    coord_flip() +
    guides(fill = FALSE) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank(),
          axis.text.y = element_text(size = 13),
          axis.text.x = element_text(size = 9),
          axis.title.x = element_text(size = 13)) +
    ggsave(paste0('plot_', paramname, '.png'), height = 7, width = 13)

}


plotter('year', seq(1950, 2020, 10), c(1950, 2040))

plotter('imdb_rating', seq(0, 10, 0.5), c(3.5, 11))


paramname = 'imdb_rating'

  