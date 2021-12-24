library(tidyverse)
library(here)
library(glue)

wdir <- here::here("day3")

# Q1 ####

input_3.1 <- read_delim(glue("{wdir}/input_3.1.txt"), delim = " ", col_names = FALSE)

gamma_finder <- function(df) {
  
  split_df <- data.frame()
  for(i in 1:nrow(df)) {
    tot_chars <- nchar(df[i,1])
    for(j in 1:tot_chars) {
      split_df[i,j] <- as.numeric(substr(df[i,1], j, j))
    }
  }
  # return(split_df)
  
  mode_avg <- data.frame()
  for(k in 1:ncol(split_df)) {
    mode_avg[1,k] <- mean(split_df[,k])
  }
  # return(mode_avg)
  
  string_res <- ""
  for(m in 1:ncol(mode_avg)) {
    new_element <- round(mode_avg[1,m])
    string_final <- glue(string_res,new_element)
    string_res <- string_final
  }
  return(string_final)
  
}

gamma <- gamma_finder(input_3.1)
gamma_rate <- strtoi(gamma, base=2)

epsilon_finder <- function(char) {
  
  split_df <- data.frame()
  tot_chars <- nchar(char)
  for(i in 1:tot_chars) {
    split_df[1,i] <- as.numeric(substr(char, i, i))
    split_df[1,i] <- ifelse(split_df[1,i]==0, 1, 0)
  }
  
  string_res <- ""
  for(m in 1:ncol(split_df)) {
    new_element <- round(split_df[1,m])
    string_final <- glue(string_res,new_element)
    string_res <- string_final
  }
  return(string_final)
  
}

epsilon <- epsilon_finder(gamma)
epsilon_rate <- strtoi(epsilon, base=2)

answ_3.1 <- gamma_rate * epsilon_rate



# Q2 ####

input_3.2 <- read_delim(glue("{wdir}/input_3.1.txt"), delim = " ", col_names = FALSE)

oxygen_finder <- function(df) {
  
  split_df <- data.frame()
  for(i in 1:nrow(df)) {
    tot_chars <- nchar(df[i,1])
    for(j in 1:tot_chars) {
      split_df[i,j] <- as.numeric(substr(df[i,1], j, j))
    }
  }
  # return(split_df)
  
  split_df_copy <- split_df
  for(j in 1:ncol(split_df_copy)) {
    mode_col <- mean(split_df_copy[,j])
    mode_col <- ifelse(mode_col == 0.5, 0.51, mode_col)
    mode_col <- round(mode_col)
    split_df_copy <- split_df_copy %>% dplyr::filter(split_df_copy[,j]==mode_col)
    if(nrow(split_df_copy)==1) {break}
  }
  # return(split_df_copy)
  
  string_res <- ""
  for(m in 1:ncol(split_df_copy)) {
    new_element <- round(split_df_copy[1,m])
    string_final <- glue(string_res,new_element)
    string_res <- string_final
  }
  return(string_final)
  
}

oxygen <- oxygen_finder(input_3.2)
oxygen_generator_rating <- strtoi(oxygen, base=2)


co2_finder <- function(df) {
  
  split_df <- data.frame()
  for(i in 1:nrow(df)) {
    tot_chars <- nchar(df[i,1])
    for(j in 1:tot_chars) {
      split_df[i,j] <- as.numeric(substr(df[i,1], j, j))
    }
  }
  # return(split_df)
  
  split_df_copy <- split_df
  for(j in 1:ncol(split_df_copy)) {
    mode_col <- mean(split_df_copy[,j])
    mode_col <- ifelse(mode_col == 0.5, 0.51, mode_col)
    mode_col <- round(mode_col)
    mode_col <- ifelse(mode_col == 1, 0, 1)
    split_df_copy <- split_df_copy %>% dplyr::filter(split_df_copy[,j]==mode_col)
    if(nrow(split_df_copy)==1) {break}
  }
  # return(split_df_copy)
  
  string_res <- ""
  for(m in 1:ncol(split_df_copy)) {
    new_element <- round(split_df_copy[1,m])
    string_final <- glue(string_res,new_element)
    string_res <- string_final
  }
  return(string_final)
  
}

co2 <- co2_finder(input_3.2)
co2_scrubber_rating <- strtoi(co2, base=2)  

answ_3.2 <- oxygen_generator_rating * co2_scrubber_rating
