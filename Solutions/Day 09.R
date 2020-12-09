library(tidyverse)

encrypted <- read_lines("data/day09.txt") %>% as.numeric()


find_invalids <- function() {
  
  invalid <- tibble()
  
  map(26:length(encrypted), function(pos) {
    
    digit <- encrypted[pos]
    
    preamble <- tibble(num = encrypted[(pos - 26):(pos - 1)])
    
    valid_opts <- preamble %>% 
      transmute(num1 = num) %>% 
      mutate(id = 1:nrow(preamble)) %>% 
      group_by(id) %>% 
      group_map( ~ 
                   # print(.y)
                   tibble(num1 = .x$num1, num2 = preamble$num[-.y$id])
      ) %>% 
      reduce(bind_rows) %>% 
      mutate(sum = num1 + num2)
    
    if (!(digit %in% valid_opts$sum)) {
      invalid <<- tibble(digit, pos) %>% 
        bind_rows(invalid, .)
    }
    
  })
  
  invalid
  
}

invalids <- find_invalids()

find_answer <- function(val = invalids$digit, 
                        invalid_pos = invalids$pos, 
                        searches = encrypted[1:(invalid_pos-1)]) {
  
  first_row <- 1
  last_row <- first_row
  
  while (TRUE) {
    
    total_under <- TRUE
    
    while (total_under) {
      
      total <- sum(searches[first_row:last_row])
      
      last_row <- last_row + 1
      
      if (total > val) {
        
        total_under <- FALSE
        first_row <- first_row + 1
        last_row <- first_row
        
      } else if (total == val) {
        
        print(searches[first_row:last_row])
        
        return(min(searches[first_row:last_row]) + max(searches[first_row:last_row]))
        
      }
    }
    
    
  }
  
}

find_answer()
