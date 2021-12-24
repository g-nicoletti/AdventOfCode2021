library(tidyverse)
library(here)
library(glue)

wdir <- here::here("day4")

# Q1 ####

input_4.1_draw <- t(read_delim(glue("{wdir}/input_4.1_draw.txt"), delim = ",", col_names = FALSE))
input_4.1_boards <- read_table(glue("{wdir}/input_4.1_boards.txt"), col_names = FALSE)

df_4.1_b <- 
  input_4.1_boards %>%
  dplyr::mutate(board = rep(1:(nrow(input_4.1_boards)/5), each=5))

df_4.1_survival <- 
  data.frame(
    X1 = rep(1, nrow(input_4.1_boards))
    , X2 = rep(1, nrow(input_4.1_boards))
    , X3 = rep(1, nrow(input_4.1_boards))
    , X4 = rep(1, nrow(input_4.1_boards))
    , X5 = rep(1, nrow(input_4.1_boards))
    , board = df_4.1_b$board
    )

winner_board <- function(df_b, df_s) {
  
  output <- list()
  output$surv <- list()
  # output$rank <- list()
  
  rank <- 
    data.frame(
      board = seq(1:max(df_b$board))
      , draw_number = 0
      , last_draw = -999
      )
  
  for(i in 1:max(df_b$board)) {
    print(glue("Analysing board number {i} of {max(df_b$board)}"))
    df <- df_b %>% dplyr::filter(board == i) %>%  dplyr::select(-contains("board"))
    surv <- df_s %>% dplyr::filter(board == i) %>% dplyr::select(-contains("board"))
    
    for(j in 1:length(input_4.1_draw)) {
      for(x in 1:nrow(df)) {
        for(y in 1:ncol(df)) {
          if(df[x,y]==input_4.1_draw[j]) {
            surv[x,y] <- 0
          } 
          
          tot_col <- sum(surv[,y])
          tot_row <- surv[x,] %>% rowwise() %>% dplyr::mutate(a_rowwise_col = sum(surv[x,])) %>% ungroup() %>% dplyr::select(contains("a_rowwise_col")) %>% as.numeric()
          
          if(tot_col == 0 | tot_row == 0) {
            
            if(rank[i,2] == 0) {
              rank[i,2] <- j
              rank[i,3] <- input_4.1_draw[j]
              output$surv[[i]] <- surv
              output$board[[i]] <- df
              
              break
            } 
          }
        }
      }
    }
  }
  
  # winner <- output[which.min(output$draw_number),]
  
  output$rank <- rank
  
  return(output)
}

winner <- winner_board(df_4.1_b, df_4.1_survival)
winner$rank[which.min(winner$rank$draw_number),]

unmarked_numbers <- winner[["board"]][[16]] * winner[["surv"]][[16]]

unmarked_numbers_vec <- c(unmarked_numbers$X1, unmarked_numbers$X2, unmarked_numbers$X3, unmarked_numbers$X4, unmarked_numbers$X5)

answ_4.1 <- sum(unmarked_numbers_vec) * 77


# Q2 ####
winner$rank[which.max(winner$rank$draw_number),]

unmarked_numbers <- winner[["board"]][[65]] * winner[["surv"]][[65]]

unmarked_numbers_vec <- c(unmarked_numbers$X1, unmarked_numbers$X2, unmarked_numbers$X3, unmarked_numbers$X4, unmarked_numbers$X5)

answ_4.2 <- sum(unmarked_numbers_vec) * 55
