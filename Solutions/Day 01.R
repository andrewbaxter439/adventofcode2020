library(tidyverse)

input <- read_tsv("data/day01.txt", col_names = "value")

input %>% 
  filter((2020 - value) %in% input$value) %>% 
  summarise(prod(value))

input %>% 
  rowwise() %>% 
  group_map(~ tibble(a = .x$value, b = input$value)) %>% 
  reduce(bind_rows) %>% 
  filter((2020-a-b) %in% input$value) %>% 
  summarise(prod(unique(a)))
