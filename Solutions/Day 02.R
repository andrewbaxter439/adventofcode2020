library(tidyverse)

passwords <- tibble(input = read_lines("data/day02.txt"))

# Part 1
passwords %>% 
  mutate(low = str_extract(input, "^\\d+"),
         high = str_extract(input, "(?<=-)\\d+"),
         letter = str_extract(input, "\\w(?=:)"),
         password = str_extract(input, "\\w*$"),
         n_letter = str_count(password, letter),
         across(low:high, as.numeric),
         valid = n_letter >= low & n_letter <= high) %>% 
  summarise(sum(valid))
  
# Part 2
passwords %>% 
  rowwise() %>%
  mutate(n1 = as.numeric(str_extract(input, "^\\d+")),
         n2 = as.numeric(str_extract(input, "(?<=-)\\d+")),
         letter = str_extract(input, "\\w(?=:)"),
         password = str_split(str_extract(input, "\\w*$"), ""),
         pos1 = password[n1]==letter,
         pos2 = password[n2]==letter,
         n_pred = pos1+pos2) %>% 
  count(n_pred == 1)