#!/usr/bin/Rscript --vanilla

## script to read and clean up hobo data 
## for temperature, relative humidity and dew point data 
## measured during drying
## Summer 2021 and summer 2022

library(dplyr)

###################################################################################################
# read weather data from files produced by hobo logger
###################################################################################################
read_hobo_file <- function(filename) {
  hobo <- read.csv(filename, skip=1, header=FALSE)
  names(hobo)[2:5] <- c("date_time", "temp", "rh", "dewpt") # eliminated 
  # the row column and grabbed the next four columns
  hobo <- hobo %>% 
    dplyr::select(date_time, temp, rh, dewpt) %>%
    return(hobo) 
}

######################################################################################################
# Concatenating hobo files
######################################################################################################

concat_hobo_files <- function(filelist){
  l <- lapply(filelist, read_hobo_file)
  r <- bind_rows(l)
  return(r)
}


###########################################################################
# Flam-psi
#############################################################################

hobo_bench_drying <- concat_hobo_files(list.files("./data/bench_drying_hobos",
                                                  full.names = TRUE,recursive = TRUE,
                                                  pattern=".*csv"))



range(hobo_bench_drying$temp, na.rm = TRUE)

range(hobo_bench_drying$rh, na.rm = TRUE)



