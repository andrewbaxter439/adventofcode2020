library(tidyverse)

baggage <- read_lines("data/day07.txt")

bag_rules <- tibble(rule = baggage,
                    container = str_extract(rule, "^.*?(?= bags)"),
                    contents = str_extract(rule, "(?<=contain ).*$")) %>% 
  rowwise() %>% 
  group_map(~ mutate(.x, contents_exp = str_split(contents, ", ")) %>% 
              unnest(contents_exp)) %>% 
  reduce(bind_rows) %>% 
  mutate(quantity = as.numeric(str_extract(contents_exp,
                                           "^\\d+")),
         content_col = str_extract(contents_exp,
                                   "(?<=^\\d{1,2} ).*(?= bag)")) %>% 
  select(container, quantity, content_col, rule)

# Problem 1

what_bags_contain <- function(base_col = "shiny gold") {
  
  get_next_rows <- function(contents) {
    
    iteration <- map(contents, ~ 
                       bag_rules %>% 
                       filter(content_col == .x) %>% 
                       select(container)) %>% 
      reduce(bind_rows)
    
    if (nrow(iteration) == 0) {
      return(unique(out_tb))
    } else {
      out_tb <<- bind_rows(out_tb, iteration)
      get_next_rows(iteration$container)
    }
  }
  
  base_col <- "shiny gold"
  
  out_tb <- bag_rules %>% 
    filter(content_col == base_col) %>% 
    select(container)
  
  get_next_rows(out_tb$container)
  
  unique(out_tb)
  
}

what_bags_contain("shiny gold")

# Problem 2

how_many_bags_are_in <- function(base_col = "shiny gold") {
  
  
  get_next_contents <- function(container_tb) {
    
    iteration <- map2(container_tb$content_col, container_tb$quantity, 
                      ~ bag_rules %>% 
                        filter(container == .x) %>% 
                        mutate(quantity = quantity * .y) %>% 
                        select(quantity, content_col)
    ) %>% 
      reduce(bind_rows) %>% 
      filter(!is.na(content_col))
    
    if (nrow(iteration) == 0) {
      return(n_bags)
    } else {
      n_bags <<- n_bags + sum(iteration$quantity)
      get_next_contents(iteration)
    }
    
  }
  
  
  cont_tb <- bag_rules %>% 
    filter(container == base_col) %>% 
    select(quantity, content_col) 
  
  n_bags <- sum(cont_tb$quantity)
  
  get_next_contents(cont_tb)
  
  n_bags
  
}

how_many_bags_are_in("shiny gold")
