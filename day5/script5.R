library(tidyverse)
library(here)
library(glue)

wdir <- here::here("day5")

# Q1 ####

input_5.1 <- read_delim(glue("{wdir}/input_5.1.txt"), delim = ",", col_names = FALSE)

df_5.1 <- 
  input_5.1 %>% 
  separate(X2, c("test1", "test2")) %>%
  dplyr::rename(X1 = X1
                , Y1 = test1
                , X2 = test2
                , Y2 = X3) %>%
  dplyr::mutate(across(where(is.character), as.numeric)) %>%
  dplyr::filter(X1 == X2 | Y1 == Y2)

max_X <- max(c(df_5.1$X1, df_5.1$X2))
min_X <- min(c(df_5.1$X1, df_5.1$X2))

max_Y <- max(c(df_5.1$Y1, df_5.1$Y2))
min_Y <- min(c(df_5.1$Y1, df_5.1$Y2))

mat <- matrix(0, nrow = max_Y+1, ncol = max_X+1)
# mat[2,3]

overlapping_points <- function(m, coords) {
  
  for(i in 1:nrow(coords)) {
    coords_s <- coords[i,]
    if(coords_s$X1==coords_s$X2) {
      for(j in min(coords_s$Y1, coords_s$Y2):max(coords_s$Y1, coords_s$Y2)) {
        mat[coords_s$X1, j] <- mat[coords_s$X1, j] + 1
      }
      } else if(coords_s$Y1==coords_s$Y2) {
        for(k in min(coords_s$X1, coords_s$X2):max(coords_s$X1, coords_s$X2)) {
          mat[k, coords_s$Y1] <- mat[k, coords_s$Y1] + 1
          }
      }
    }
  return(mat)
}

# debugonce(overlapping_points)
result <- overlapping_points(mat, df_5.1)
answ_5.1 <- length(result[result>=2])


# Q2 ####

input_5.2 <- read_delim(glue("{wdir}/input_5.1.txt"), delim = ",", col_names = FALSE)

df_5.2 <- 
  input_5.2 %>% 
  separate(X2, c("test1", "test2")) %>%
  dplyr::rename(X1 = X1
                , Y1 = test1
                , X2 = test2
                , Y2 = X3) %>%
  dplyr::mutate(across(where(is.character), as.numeric)) 

max_X <- max(c(df_5.2$X1, df_5.2$X2))
min_X <- min(c(df_5.2$X1, df_5.2$X2))

max_Y <- max(c(df_5.2$Y1, df_5.2$Y2))
min_Y <- min(c(df_5.2$Y1, df_5.2$Y2))

mat_2 <- matrix(0, nrow = max_Y+1, ncol = max_X+1)
# mat[2,3]

overlapping_points_d <- function(m, coords) {
  
  for(i in 1:nrow(coords)) {
    coords_s <- coords[i,]
    
    if(coords_s$X1==coords_s$X2) { # x same coord
      for(j in min(coords_s$Y1, coords_s$Y2):max(coords_s$Y1, coords_s$Y2)) {
        mat[coords_s$X1, j] <- mat[coords_s$X1, j] + 1
      }
    }
    
    else if(coords_s$Y1==coords_s$Y2) { # y same coord
      for(k in min(coords_s$X1, coords_s$X2):max(coords_s$X1, coords_s$X2)) {
        mat[k, coords_s$Y1] <- mat[k, coords_s$Y1] + 1
      }
    } 
    
    else if(min(coords_s$X1, coords_s$X2) == coords_s$X1 & min(coords_s$Y1, coords_s$Y2) == coords_s$Y1) { # (1,1),(5,5) x incr, y incr
      m <- -1 
      while(coords_s$X1 + m <= coords_s$X2) {
        m <- m + 1
        mat[coords_s$X1 + m, coords_s$Y1 + m] <- mat[coords_s$X1 + m, coords_s$Y1 + m] + 1
        }
      } 
    
    else if (min(coords_s$X1, coords_s$X2) == coords_s$X1 & min(coords_s$Y1, coords_s$Y2) == coords_s$Y2) { # (3,5),(5,3) x incr, y decr
        m <- -1 
        while(coords_s$X1 + m <= coords_s$X2) {
          m <- m + 1
          mat[coords_s$X1 + m, coords_s$Y1 - m] <- mat[coords_s$X1 + m, coords_s$Y1 - m] + 1
        }
      } 
    
    else if(min(coords_s$Y1, coords_s$Y2) == coords_s$Y1) { # x decr, y incr
      m <- -1
      while(coords_s$Y1 + m <= coords_s$Y2) {
        m <- m + 1
        mat[coords_s$X1 - m, coords_s$Y1 + m] <- mat[coords_s$X1 - m, coords_s$Y1 + m] + 1
      }}
    
    else {
      m <- -1
      while(coords_s$Y1 - m <= coords_s$Y2) {
        m <- m + 1
        mat[coords_s$X1 - m, coords_s$Y1 - m] <- mat[coords_s$X1 - m, coords_s$Y1 - m] + 1
      }
    }
    }
  return(mat)
}

debugonce(overlapping_points_d)
result_5.2 <- overlapping_points_d(mat_2, df_5.2)
answ_5.2 <- length(result[result>=2])
