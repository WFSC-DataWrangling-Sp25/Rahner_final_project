#Final Project
#Lyra Rahner
#5/4/25
#lyrarahner@arizona.edu

#read in libraries 
library(tidyverse)
library(dendroTools)
library(readxl)
library(dplR)




###############################################################################
############### working with prec data first ##################################
###############################################################################

#read in climate data 
prec <- read_excel("data_raw/Daily precipitation.xlsx")

#rename col names in prec to be tidy 

prec <- prec %>% 
  rename(year = Year,
         month = Month,
         day = Day,
         prec_cm = `Precipitation in inches`)


#i want the dates to be in date format now

#prec <-prec %>%
#  mutate(date = make_date(year, month, day)) %>% 
#  select (date, prec_cm)
#as_tibble(prec)
  
#transform data to wide format
#daily_precipitation <- data_transform(prec, format = 'daily', date_format = 'ymd')
#as_tibble(daily_precipitation)

#glimps data to see how to see how many Na im working with 
#glimpse_daily_data(env_data = daily_precipitation, na.color = "red") + 
#  theme(legend.position = "bottom")

#looks like there are alot of NA and also a period of data missing in the 40s
#going to replace the NA values with the mean from each column 
#daily_precipitation <- daily_precipitation %>%
#  mutate(across(everything(), ~replace_na(., mean(., na.rm = TRUE))))

#look at data again
#glimpse_daily_data(env_data = daily_precipitation, na.color = "red") + 
#  theme(legend.position = "bottom")

#going to get rid of data before 1950 going to make a new df because its easier
#than undoing everything 

pre <-prec %>% filter(year>=1950) %>% 
  filter(year<=2021) %>% 
  mutate(date = make_date(year, month, day)) %>% 
  select (date, prec_cm)
as_tibble(pre)

#transform to wide format 
daily_precipitation <- data_transform(pre, format = 'daily', 
                                      date_format = 'ymd')
as_tibble(daily_precipitation)

#look at data 
glimpse_daily_data(env_data = daily_precipitation, na.color = "red") + 
  theme(legend.position = "bottom")

#get rid of na by replacing with col average
daily_precipitation <- daily_precipitation %>%
  mutate(across(everything(), ~replace_na(., mean(., na.rm = TRUE))))

#look at data 
glimpse_daily_data(env_data = daily_precipitation, na.color = "red") + 
  theme(legend.position = "bottom")

#looks good moving on
################################################################################
############################## TEMP DATA #######################################
################################################################################
temp <- read_excel("data_raw/Temp_data_GHCN-D_station_code_USC00295084_LOS_ALAMOS_NM_US.xlsx",
                   skip= 19, col_names = FALSE)

#fix formatting of tep seperate by space 

colnames(temp) <- "Data"

temp <- temp %>% 
  separate(Data, into = c("year", "month", "day", "temp_c"),
           sep = "\\s+", remove = TRUE)

#temp <-temp %>%
#  mutate(date = make_date(year, month, day)) %>% 
#  select (date, temp_c)
#as_tibble(temp)

#temp <- temp %>%
#  mutate(temp_c = as.numeric(temp_c))
#as_tibble(temp)

#transform to wide format 
#daily_temp <- data_transform(temp, format = 'daily', 
#                                      date_format = 'ymd')
#as_tibble(daily_temp)

#look at data 
#glimpse_daily_data(env_data = daily_temp, na.color = "red") + 
#  theme(legend.position = "bottom")

#okay also missing a chunk of data in the 1940s and weird NA values
#before 1950 so going to start after 1950

temp <-temp %>% filter(year>=1950) %>% 
  filter(year<=2021) %>% 
  mutate(date = make_date(year, month, day)) %>% 
  select (date, temp_c)
as_tibble(temp)

#make temp numeric
temp <- temp %>%
  mutate(temp_c = as.numeric(temp_c))
as_tibble(temp)

#transform to wide format 
daily_temp <- data_transform(temp, format = 'daily', 
                             date_format = 'ymd')
as_tibble(daily_temp)

#look at data 
glimpse_daily_data(env_data = daily_temp, na.color = "red") + 
  theme(legend.position = "bottom")


#get rid of na by replacing with col average
daily_temp <- daily_temp %>%
  mutate(across(everything(), ~replace_na(., mean(., na.rm = TRUE))))

#look at data 
glimpse_daily_data(env_data = daily_temp, na.color = "red") + 
  theme(legend.position = "bottom")

#yay looks good now lets do some tree stuff 

############################################################################
############################# TREE MAGIC ###################################
############################################################################
#load libraries
library(dplR)
library(stringr)

# make list of rwl files
rwl_files <- list.files("data_raw", pattern = ".rwl", full.names = TRUE)

# make empty list to hold data
all_data_list <- list()

# read in each file
for (file in rwl_files) {
  rwl <- read.rwl(file)
  all_data_list[[file]] <- rwl
}

# Combine into one data frame 
combined_rwl <- combine.rwl(all_data_list)

# separate into aboveground data (stem) and belowground (roots)
aboveground_cols <- str_detect(names(combined_rwl), "L|BA|BB|MA|MB|TA|TB|P")
above_ground <- combined_rwl[, aboveground_cols]
below_ground <- combined_rwl[, !aboveground_cols]


#detrend data and make chronologies for above ground and belowground data 

#check correlation 
corr.rwl.seg(above_ground, seg.length = 10, bin.floor = 0, pcrit = 1.66) #looks good
#quick plot
plot(above_ground, plot.type = "spag")

#aboveground chronology 
detrend_above <- detrend(above_ground, method = "Spline", nyrs = 20)

above_chron <- chron(detrend_above)

plot(above_chron)

#beloground 
#check correlation 
corr.rwl.seg(below_ground, seg.length = 10, bin.floor = 0, pcrit = 1.676) #looks good
#quick plot
plot(below_ground, plot.type = "spag")


detrend_below <- detrend(below_ground, method = "Spline")

roots_chron <- chron(detrend_below)

plot(roots_chron)

#################################################################################
#great we have chronologies now we need to compare to climate data ##############
#################################################################################

# PRECIPITATION FIRST

#waring these figures take forever to load 

#above ground

above_chron_daily <- above_chron %>%
  mutate(year = as.numeric(rownames(.))) %>%
  filter(year >= 1950 & year <= 2021) %>% 
  select("std")


above.chrn_response_pre <- daily_response(response = above_chron_daily, env_data = daily_precipitation,
                                   method = "cor", lower_limit = 1, upper_limit = 275,
                                   row_names_subset = TRUE, previous_year = TRUE,
                                   remove_insignificant = TRUE, alpha = 0.05, 
                                   subset_years = c(1951, 2021),
                                   aggregate_function = 'sum', reference_window="end")

above.chrn_response_pre
summary(above.chrn_response_pre)
plot(above.chrn_response_pre, type = 1)
plot(above.chrn_response_pre, type = 2)


#below ground 

roots_chrn_daily <- roots_chron %>%
  mutate(year = as.numeric(rownames(.))) %>%
  filter(year >= 1950 & year <= 2021) %>% 
  select("std")


roots.chrn_response_pre <- daily_response(response = roots_chrn_daily, env_data = daily_precipitation,
                                      method = "cor", lower_limit = 1, upper_limit = 275,
                                      row_names_subset = TRUE, previous_year = TRUE,
                                      remove_insignificant = TRUE, alpha = 0.05, 
                                      subset_years = c(1951, 2021),
                                      aggregate_function = 'sum', reference_window="end")

roots.chrn_response_pre
summary(roots.chrn_response_pre)
plot(roots.chrn_response_pre, type = 1)
plot(roots.chrn_response_pre, type = 2)


# TEMPERATURE

#above ground

above.chrn_response_temp <- daily_response(response = above_chron_daily, env_data = daily_temp,
                                      method = "cor", lower_limit = 1, upper_limit = 275,
                                      row_names_subset = TRUE, previous_year = TRUE,
                                      remove_insignificant = TRUE, alpha = 0.05, 
                                      subset_years = c(1951, 2021),
                                      aggregate_function = 'sum', reference_window="end")

above.chrn_response_temp
summary(above.chrn_response_temp)
plot(above.chrn_response_temp, type = 1)
plot(above.chrn_response_temp, type = 2)


#below ground 

roots.chrn_response_temp <- daily_response(response = roots_chrn_daily, env_data = daily_temp,
                                      method = "cor", lower_limit = 1, upper_limit = 275,
                                      row_names_subset = TRUE, previous_year = TRUE,
                                      remove_insignificant = TRUE, alpha = 0.05, 
                                      subset_years = c(1951, 2021),
                                      aggregate_function = 'sum', reference_window="end")

roots.chrn_response_temp
summary(roots.chrn_response_temp)
plot(roots.chrn_response_temp, type = 1)
plot(roots.chrn_response_temp, type = 2)

#######################################################################################
################### SUPERPOSED EPOCH ANALYSIS #########################################
#######################################################################################

#start with precipitation

#change format of climate data have date, value, year and month

as.tibble(pre)

library(lubridate)
month(pre$date)

prism_met <- pre%>%
  mutate(month = month(date), year = year(date)) %>%
  relocate(month, .after = year) %>%
  subset(year >= 1950 & year < 2021) %>%
  rename(prec = )


# converts to date format
prism_met$date <- date(prism_met$date)

# add in columns for different precipitation timing
prism_met <- mutate(prism_met,
                    season = case_when(
                      month(date) %in% c(7,8,9) ~ "prev_mons",
                      month(date) %in% c(10) ~ "prev_fall",
                      month(date) %in% c(11, 12,1, 2,3) ~ "winter",
                      month(date) %in% c(4,5,6) ~ "pre_mons",
                      T ~ NA_character_))


#okay thats as far as a got i still need to separate the ppt data based on 
#ppt period and then make the figures 

#and also add code to save all my outputs and clean data so i dont have to run 
#this a million times 




