library(tidyverse)
library(here)
library(glue)

wdir <- here::here("day2")

# Q1 ####

input_2.1 <- read_delim(glue("{wdir}/input_2.1.txt"), delim = " ", col_names = FALSE)

names(input_2.1) <- c("dir","value")

df_2.1 <- 
  input_2.1 %>% 
  dplyr::mutate(value_new = ifelse(dir == "up", -value, value)) %>%
  dplyr::summarise(hrz = sum(ifelse(dir == "forward", value_new, 0))
                   , vrt = sum(ifelse(dir != "forward", value_new, 0)))
  
answ_2.1 <- df_2.1$hrz * df_2.1$vrt

# Q2 ####

input_2.2 <- read_delim(glue("{wdir}/input_2.2.txt"), delim = " ", col_names = FALSE)

names(input_2.2) <- c("dir","value")

df_2.2 <- 
  input_2.2 %>%
  dplyr::mutate(aim_curr = ifelse(dir == "down", value, ifelse(dir == "up", -value, 0))) %>%
  dplyr::mutate(extra_hrz = ifelse(dir == "forward", value, 0)
                , aim = cumsum(aim_curr)
                # , aim_pvs_sum = if(is.na(dplyr::lag(aim_trns))) {0} else {dplyr::lag(aim_trns)} + aim_trns
                # , aim_curr = aim_pvs_sum + aim_trns
                , extra_vrt = ifelse(dir == "forward", value*aim, 0)
                , tot_hrz = cumsum(extra_hrz)
                , tot_vrt = cumsum(extra_vrt))

df_2.2 <- df_2.2[nrow(df_2.2),]

answ_2.2 <- df_2.2$tot_hrz * df_2.2$tot_vrt
