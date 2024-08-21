#!/usr/bin/Rscript --vanilla

# Adapted from shrubflam repo (Shrub Flammability project) Dylan Schwilk, Azaj
# Mahmud 2022

# Read the thermocouple data, in multiple csv files saved from the HOBO
# software.

TZ = "CST6CDT"
DATA_CACHE_DIR <- "./results"

####################################################################

burn_trials <- read.csv("./data/2023/burn_trials.csv", stringsAsFactors = FALSE)

burn_trials_2024 <- read.csv("./data/2024/burn_trials.csv", stringsAsFactors = FALSE)

#####################################################################
# Cleaning trial data
#####################################################################

burn_trials <- burn_trials %>%
  mutate(hobo_start_time = mdy_hm(str_c(date, " ", start_time), tz = TZ) +
      dseconds(120 + ignition_delay)) %>%
  mutate(hobo_start_time = if_else(self_ignition == 1, mdy_hm(str_c(date, " ", start_time), tz = TZ) + dseconds(self_ig_start),
                                   hobo_start_time)) %>%
  mutate(hobo_end_time = hobo_start_time +
      dseconds(flame_duration)) %>%
  mutate(interval = lubridate::interval(hobo_start_time, hobo_end_time))

burn_trials_2024 <- burn_trials_2024 %>%
  filter(! sample_id %in% c("FT07", "ST21", "ST28")) %>%
  mutate(hobo_start_time = mdy_hm(str_c(date, " ",start_time), tz = TZ) +
           dseconds(120 + ignition_delay),
         hobo_end_time = hobo_start_time + dseconds(flame_duration),
         interval = lubridate::interval(hobo_start_time, hobo_end_time))


####################################################################
# Function for reading a single hobo csv file
####################################################################

read_hobo_file <- function(filename) {
  hobo <- read.csv(filename, skip = 2, header = FALSE)
  names(hobo)[1:3] <- c("row", "time", "temp")
  hobo <- hobo %>% dplyr::select(time, temp) %>%
    # we use floor_date() below to round to seconds so we can line up our
    # measurements across HOBOs
    mutate(time = floor_date(mdy_hms(time, tz = TZ), "second"))
  return(hobo)
}

#####################################################################
# Function for concatenating all the hobo files
#####################################################################

concat_hobo_files <- function(filelist, label) {
  l <- lapply(filelist, read_hobo_file)
  r <- bind_rows(l)
  names(r) <- c("time", label)
  return(r)
}



###############################################################################
# We could grab the column name from the file name and do this in one go, but
# this also works.
###############################################################################
# Grabbing all the hobo files from left
########################################################################

flam_left <- concat_hobo_files(list.files("./data/2023/burn_hobos",
                                          full.names = TRUE, recursive = TRUE,
                                          pattern = "flam.left*.csv"),
                               "flam_left")


flam_left_2024 <- concat_hobo_files(list.files("./data/2024/burn_hobos",
                                          full.names = TRUE, recursive = TRUE,
                                          pattern = "flam.left*.csv"),
                               "flam_left")



#####################################################################
# Grabbing all the hobo files from mid
#####################################################################

flam_mid <- concat_hobo_files(list.files("./data/2023/burn_hobos",
                                         full.names = TRUE, recursive = TRUE,
                                         pattern = "flam.mid*.csv"),
                              "flam_mid")

flam_mid_2024 <- concat_hobo_files(list.files("./data/2024/burn_hobos",
                                         full.names = TRUE, recursive = TRUE,
                                         pattern = "flam.mid*.csv"),
                              "flam_mid")


#####################################################################
# Grabbing all the hobo files from right
#####################################################################

flam_right <- concat_hobo_files(list.files("./data/2023/burn_hobos",
                                           full.names = TRUE, recursive = TRUE,
                                           pattern = "flam.right*.csv"),
                                "flam_right")



flam_right_2024 <- concat_hobo_files(list.files("./data/2024/burn_hobos",
                                           full.names = TRUE, recursive = TRUE,
                                           pattern = "flam.right*.csv"),
                                "flam_right")

#####################################################################
# Getting all the hobo files in a single data frame
#####################################################################

hobos <- full_join(flam_left, flam_mid, by = "time") %>% 
  full_join(flam_right,  by = "time")

hobos_2024 <- full_join(flam_left_2024, flam_mid_2024, by = "time") %>% 
  full_join(flam_right_2024,  by = "time")



#####################################################################
# Function to assign the labels after matching the trails time hobos
#####################################################################

get_trial_label <- function(time, burn_trials_data) {
  matches <- time %within% burn_trials_data$interval
  if(!any(matches)) return(NA)
  # Return the sample_id corresponding to the match
  return(burn_trials_data$sample_id[which.max(matches)])
}


#####################################################################
# Assigning the labels
#####################################################################

hobos$sample_id <- unlist(sapply(hobos$time, get_trial_label, burn_trials_data = burn_trials))

unique(hobos$sample_id) 
length(unique(hobos$sample_id))

hobos_2024$sample_id <- unlist(sapply(hobos_2024$time, get_trial_label, burn_trials_data = burn_trials_2024))

unique(hobos_2024$sample_id) 
length(unique(hobos_2024$sample_id))  

#####################################################################
# Getting the hobos as long format to summarise the data by 
# three thermocouples
#####################################################################

hobos_long <- hobos %>%
  rbind(hobos_2024) %>%
  gather(key = "position",
         value = "temperature", -time, -sample_id)

#####################################################################
# Summarising the hobo data
#####################################################################

hobo_temp_sum <- hobos_long %>%  filter(sample_id != "NA") %>%
  group_by(sample_id, position) %>%
  summarise(dur_100 = sum(temperature > 100),
            degsec_100= sum(temperature[temperature >100]),
            peak_temp = max(temperature),
            peak_time = time[which(peak_temp == temperature)[1]],
            num_NA = sum(is.na(temperature))) %>% ungroup()
 

dim(burn_trials) 
dim(hobo_temp_sum) 


###############################################################################
# Need to make the summarise data wider in order to merge with the other trial
# data to perform the PCA. Code below makes decision to just take averagevalues
# across three positions per trial. Reconsider this.
###############################################################################
hobos_wider <- hobo_temp_sum %>%
  group_by(sample_id) %>%
  summarise(dur_100 = mean(dur_100),
            peak_temp = max(peak_temp),
            degsec_100 = max(degsec_100))

dim(hobos_wider)

saveRDS(hobos_wider, file.path(DATA_CACHE_DIR, "hobos_wider.RDS"))

########################################################################
# Cleaning the environment
########################################################################
rm("concat_hobo_files", "get_trial_label", "read_hobo_file",
   "hobos_long", "flam_right","flam_mid","flam_left","TZ",
   "hobo_temp_sum", "hobos", "hobos_2024",  "flam_right_2024","flam_mid_2024","flam_left_2024",
   "burn_trials", "burn_trials_2024")

