library(tidyverse)
library(here)
library(glue)

wdir <- here::here("day6")

# Q1 ####

# input_6.1 <- c(3,4,3,1,2)
input_6.1 <- as.numeric(read_delim(glue("{wdir}/input_6.1.txt"), delim = ",", col_names = FALSE))

fish_count <- function(vec, max_days = 18) {
  for(i in 1:max_days) {
    n_begin <- length(vec)
    zeros <- which(vec == 0)
    if(length(zeros)!=0) {
      zero_pos <- which(vec==0)
      vec[zero_pos] <- 6
      for(j in 1:length(zeros)) {
        vec[length(vec)+1] <- 8
      }
    }
    
    if(length(zeros)==0) {
      vec <- vec - 1
    } 
    
    else {
      vec[1:n_begin] <- vec[1:n_begin] - 1
      vec[zero_pos] <- 6
    }
    
  }
  
  return(vec)
}

# debugonce(fish_count)
result <- fish_count(input_6.1, max_days = 80)
answ_6.1 <- length(result)


# Q2 ####

result2 <- fish_count(input_6.1, max_days = 256)
answ_6.2 <- length(result)