library(tidyverse)

answers <- read_file("data/day06.txt")

# Problem 1
answers %>% 
  str_split("\r\n\r\n") %>% 
  unlist() %>% 
  str_split("\r\n") %>% 
  map(~ str_split(.x, "") %>% 
        unlist() %>% 
        unique() %>% 
        length()) %>% 
  reduce(sum)

# Problem 2
answers %>% 
  str_split("\r\n\r\n") %>% 
  unlist() %>% 
  str_split("\r\n") %>% 
  map(~ str_split(.x, "")  %>%
        map(~ tibble(.x)) %>% 
        reduce(inner_join, by = ".x") %>% 
        nrow()
      ) %>% 
  reduce(sum)
  
  
