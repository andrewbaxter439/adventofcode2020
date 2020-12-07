library(tidyverse)

tickets <- read_lines("data/day05.txt")

find_seat <- function(ticket = tickets[1]) {
  
  find_row_part <- function(part, instr) {
    rows <- length(part)
    if (rows == 1) {
      return(part)
    } else if (instr[i] == "F") {
      part <- part[1:(rows/2)]
      i <<- i+1
      find_row_part(part, instr)
    } else {
      part <- part[(rows/2+1):rows]
      i <<- i+1
      find_row_part(part, instr)
    }
  }
  
  find_col_part <- function(part, instr) {
    seat <- length(part)
    if (seat == 1) {
      return(part)
    } else if (instr[i] == "L") {
      part <- part[1:(seat/2)]
      i <<- i+1
      find_col_part(part, instr)
    } else {
      part <- part[(seat/2+1):seat]
      i <<- i+1
      find_col_part(part, instr)
    }
  }
  
  i <- 1
  instr <- str_split(ticket, "") %>% unlist()
  
  row_num <- find_row_part(1:128, instr) - 1
  col_num <- find_col_part(1:8, instr) - 1
  
  tibble(row_num, col_num)
  
}

find_seat(tickets[5])

all_tickets <- map_dfr(tickets,  ~ find_seat(.x)) %>%
  mutate(seat_num = row_num*8 + col_num) %>% 
  arrange(desc(seat_num))

all_tickets %>% 
  anti_join(tibble(seat_num = 0:806), .) %>% View()
