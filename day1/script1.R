library(tidyverse)
library(here)
library(glue)

wdir <- here::here("day1")

# Q1 ####

input_1.1 <- scan(glue("{wdir}/input1_1.txt"))

pvs <- dplyr::lag(input_1.1)
df_1.1 <- cbind(input_1.1, pvs)

df_1.1 <-
  df_1.1 %>%
  as.data.frame() %>%
  dplyr::rename(curr = input_1.1) %>%
  dplyr::mutate(cmpr = ifelse(curr < pvs, "decrease", "increase")) %>%
  drop_na()

answ_1.1 <- sum(df_1.1$cmpr == "increase")

# Q2 ####

input_1.2 <- scan(glue("{wdir}/input_1.2.txt"))

pvs <- dplyr::lag(input_1.2)
fol <- dplyr::lead(input_1.2)
df_1.2 <- cbind(input_1.2, pvs, fol)

df_1.2 <-
  df_1.2 %>%
  as.data.frame() %>%
  # drop_na() %>%
  rowwise() %>%
  dplyr::mutate(tot_curr = sum(c(input_1.2, pvs, fol))) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(tot_pvs = lag(tot_curr)) %>%
  drop_na() %>% 
  dplyr::mutate(tot_cmpr = ifelse(tot_curr > tot_pvs, "increase", "decrease"))

answ_1.2 <- sum(df_1.2$tot_cmpr == "increase")
