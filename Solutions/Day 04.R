library(tidyverse)

batch <- read_file("data/day04.txt")

# Counting passports with all relevant fields
all_pps <- batch %>% str_split("\r\n\r\n") %>%
  unlist() %>% 
  map2(1:260, function(pas, id) {
    str_extract_all(pas, "\\w{3}:[#\\w]+") %>% 
      unlist() %>% 
      str_split(":", simplify = TRUE) %>% 
      as.data.frame() %>% 
      pivot_wider(names_from = "V1", values_from = "V2") %>% 
      mutate(ppid = id) %>% 
      select(ppid, !ppid)
  }) %>% 
  reduce(bind_rows)

all_pps %>% 
  select(-ppid, -cid) %>% 
  summarise(nas = sum(!is.na(c_across()))) %>% 
  summarise(sum(nas==7))

all_pps %>%
  rowwise() %>% 
  mutate(
         nas = sum(is.na(c_across(eyr:pid))),
         across(ends_with("yr"), as.numeric),
         valid = case_when(
           !(byr %in% 1920:2002) ~ FALSE,
           !(iyr %in% 2010:2020) ~ FALSE,
           !(eyr %in% 2020:2030) ~ FALSE,
           !((as.numeric(str_extract(hgt, "^\\d{3}(?=cm$)")) %in% 150:193) |
               (as.numeric(str_extract(hgt, "^\\d{2}(?=in$)")) %in% 59:76)) ~ FALSE,
           !(str_detect(hcl, "#[0-9a-f]{6}")) ~ FALSE,
           !(ecl %in% c("amb", "blu","brn","gry", "grn","hzl", "oth")) ~ FALSE,
           !(str_detect(pid, "^\\d{9}$")) ~ FALSE,
           nas > 0 ~ FALSE,
           TRUE ~ TRUE
         )
         ) %>% 
  ungroup() %>% 
  summarise(valid = sum(valid))

all_pps %>%
  rowwise() %>% 
  mutate(nas = sum(is.na(c_across(eyr:pid))),
         across(ends_with("yr"), as.numeric)) %>% 
  filter(byr %in% 1920:2002, 
         iyr %in% 2010:2020, 
         eyr %in% 2020:2030, 
         (as.numeric(str_extract(hgt, "^\\d{3}(?=cm$)")) %in% 150:193) |
           (as.numeric(str_extract(hgt, "^\\d{2}(?=in$)")) %in% 59:76),
         str_detect(hcl, "#[0-9a-f]{6}"),
         ecl %in% c("amb", "blu","brn","gry", "grn","hzl", "oth"),
         str_detect(pid, "^\\d{9}$"))
