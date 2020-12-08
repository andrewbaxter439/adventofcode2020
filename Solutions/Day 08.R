library(tidyverse)

instructions <- read_lines("data/day08.txt")

instr_parsed <- instructions %>% 
  str_split(" ") %>%
  map_dfr(~ tibble(instr = .x[1], val = as.numeric(.x[2])))

# change nops to jmps just to have one less thing to deal with
instr_tidied <- instr_parsed %>% 
  mutate(
    val = ifelse(instr == "nop", 1, val),
    instr = ifelse(instr == "nop", "jmp", instr)
  )

find_final_val <- function(instruction_df = instr_parsed) {
  
  pos <- 1
  acc <- 0
  steps <- tibble()
  
  instruction_df <- bind_rows(instruction_df, 
                              tibble(instr = "terminate", val = 0))
  
  for (i in 1:nrow(instruction_df)) {
    
    instr <- instruction_df$instr[pos]
    val <- instruction_df$val[pos]
    
    if (instr == "terminate") {
      
      message("Reached the end!\nFinal value:\n")
      cat(acc)
      # cat("\nSteps taken:\n")
      # print(steps)
      return(steps)
      
    } else if (instr == "done") {
      
      message("Caught in loop, terminating\nFinal value:\n")
      cat(acc)
      # cat("\nSteps taken:\n")
      # print(steps)
      return(steps)
      
    } else if (instr == "acc") {
      
      steps <- bind_rows(steps, tibble(instr, pos))
      
      acc <- acc + val
      instruction_df$instr[pos] <- "done"
      pos <- pos + 1
      
    } else if (instr == "nop") {
      
      steps <- bind_rows(steps, tibble(instr, pos))
      
      instruction_df$instr[pos] <- "done"
      pos <- pos + 1
      
    } else {
      
      steps <- bind_rows(steps, tibble(instr, pos))
      
      instruction_df$instr[pos] <- "done"
      pos <- pos + val
      
    }
    
  }
  
  # return(steps)
}



looped <- find_final_val()

find_effective_change <- function(instructions_df = instr_parsed) {
  
  quiet_val <- quietly(find_final_val)
  
  for (step in looped$pos[looped$instr != "acc"]) {
    
    if (instructions_df$instr[step]=="jmp") {
      
      new_instr <- instructions_df
      new_instr$instr[step] <- "nop"
      loop_out <- quiet_val(new_instr)
      
      if (loop_out$messages != "Caught in loop, terminating\nFinal value:\n\n") {
        
        message("We got it!\n")
        print(loop_out$output)
        return(loop_out$result)
      }
      
    } else {
      
      new_instr <- instructions_df
      new_instr$instr[step] <- "jmp"
      loop_out <- quiet_val(new_instr)
      
      if (loop_out$messages != "Caught in loop, terminating\nFinal value:\n\n") {
        
        message("We got it!\n")
        print(loop_out$output)
        return(loop_out$result)
      }
    }
    
  }
  
}


find_effective_change()
