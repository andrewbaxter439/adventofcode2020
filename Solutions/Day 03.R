library(tidyverse)

slope <- read_lines("data/day03.txt")

encounter_trees <- function(right = 3, down  = 1) {
  slope[seq(1, length(slope), down)] %>%
  map2_chr(seq(0, (length(.)-1)*right, right) %% 31, ~ str_sub(.x, .y+1, .y+1)) %>% 
  str_count("#") %>% sum(0)
}

encounter_trees(3, 1)

tibble(right = c(1,3,5,7,1), down = c(1,1,1,1,2)) %>% 
  rowwise() %>% 
  mutate(ntrees = encounter_trees(right, down)) %>% 
  ungroup() %>% 
  summarise(mult = prod(ntrees))
