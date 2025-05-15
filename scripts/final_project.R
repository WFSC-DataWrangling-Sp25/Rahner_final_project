#Final Project for WFSC data wrangling
#Lyra Rahner
#5/4/25
#lyrarahner@arizona.edu

#read in libraries 
library(tidyverse)
library(dendroTools)
library(readxl)
library(dplR)

######################################################################################
# Steps for the analysis                                                             #
# 1. Clean up climate data                                                           #
# 2. Clean up tree ring data                                                         #
# 3. Create daily comparisons between above and below ground chronologies to         #
#    temperature and precipitation data.                                             #
# 4. Run superposed epoch analysis to determine if different drought timing have     #
#    different impacts on tree growth.                                               #
######################################################################################

######################################################################################
############# working with precipitation data first ##################################
######################################################################################

#read in climate data 
prec <- read_excel("data_raw/Daily precipitation.xlsx")

#rename column names in precipitation data to be tidy 
#CLASS CONTENT: the rename function is in Week 6: Tidy Data
prec <- prec %>% 
  rename(year = Year,
         month = Month,
         day = Day,
         prec_cm = `Precipitation in inches`)

#this code is commented out but I need to keep it so I know why I decided to get
#rid of data before 1950 later 

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

#going to get rid of data before 1950
#CLASS CONTENT: filter, mutate, select content from week 6: tidy data
#CLASS CONTENT: select and pipe: week 3: Data tables 
pre <-prec %>% filter(year>=1950) %>% 
  filter(year<=2021) %>% #filter out years after 1950
  mutate(date = make_date(year, month, day)) %>% #make a date column with y/m/d
  select (date, prec_cm) #select date and precipitation data
as_tibble(pre)

#transform to wide format using dendrotools 
daily_precipitation <- data_transform(pre, format = 'daily', 
                                      date_format = 'ymd')
as_tibble(daily_precipitation)

#look at data - why is this saving a plot????
glimpse_daily_data(env_data = daily_precipitation, na.color = "red") + 
  theme(legend.position = "bottom")

#get rid of na by replacing with col average
daily_precipitation <- daily_precipitation %>%
  mutate(across(everything(), ~replace_na(., mean(., na.rm = TRUE))))

#look at data 
glimpse_daily_data(env_data = daily_precipitation, na.color = "red") + 
  theme(legend.position = "bottom")

#save ppt data 
write_csv(daily_precipitation, "data_clean/daily_precipitation.csv")

#looks good moving on
###############################################################################################
############################## TEMP DATA ######################################################
###############################################################################################

temp <- read_excel("data_raw/Temp_data_GHCN-D_station_code_USC00295084_LOS_ALAMOS_NM_US.xlsx",
                   skip= 19, col_names = FALSE)

#fix formatting of tep separate by space 

colnames(temp) <- "Data" #set a name to the column with all the data so i can work with it

#CLASS CONTENT: separate function is content from week 6: tidy data
temp <- temp %>% 
  separate(Data, into = c("year", "month", "day", "temp_c"), #separate into multiple columns 
           sep = "\\s+", remove = TRUE)

#okay also missing a chunk of data in the 1940s and weird NA values
#before 1950 so going to start after 1950

#CLASS CONTENT: from week 6: tidy data
temp <-temp %>% filter(year>=1950) %>% #filter years after 1950
  filter(year<=2021) %>% 
  mutate(date = make_date(year, month, day)) %>% #make data column 
  select (date, temp_c) #select temp and data column
as_tibble(temp)

#week 6: tidy data
#make temp numeric
temp <- temp %>%
  mutate(temp_c = as.numeric(temp_c))
as_tibble(temp)


#transform to wide format using dendrotools 
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

#save temp data 
write_csv(daily_temp, "data_clean/daily_temp.csv")

#yay looks good now lets do some tree stuff 

#######################################################################################################
######################### MAKING CHRONOLOGIES #########################################################
#######################################################################################################
#load libraries
library(dplR)
library(stringr)

#make list of rwl files
rwl_files <- list.files("data_raw", pattern = ".rwl", full.names = TRUE)

#make empty list to hold data
all_data_list <- list()

#CLASS CONTENT: the following loop contains information from week 13: iteration
#make for loop to read in data 
for (i in 1:length(rwl_files)) { #set length of the loop to be length of rwl_files to avoid hard coding 
  rwl <- read.rwl(paste0(rwl_files[i])) #loop through list of rwl files and read them in 
  all_data_list[[i]] <- rwl #save each read in rwl file to the empty list all_data_list
}

# Combine into one data frame using dplR function
combined_rwl <- combine.rwl(all_data_list)

#CLASS CONTENT: the following 3 lines contain content from week 7: Dates and Strings
# separate rwl files into aboveground data (stem) and belowground (roots) - "L|BA|BB|MA|MB|TA|TB|P" 
#are only in the column names of above ground samples so using this pattern will separate them 
aboveground_cols <- str_detect(names(combined_rwl), "L|BA|BB|MA|MB|TA|TB|P")
above_ground <- combined_rwl[, aboveground_cols] 
below_ground <- combined_rwl[, !aboveground_cols]

#######################################################################################
# make chronology for above ground data 
###############

#check correlation 
corr.rwl.seg(above_ground, seg.length = 10, bin.floor = 0, pcrit = 0.166) #looks good
plot(above_ground, plot.type = "spag") #look at data

#detrend aboveground chronology 
detrend_above <- detrend(above_ground, method = "Spline", nyrs = 20)
above_chron <- chron(detrend_above) #make chronology

plot(above_chron) #plot chronology

#save above ground chron 
write_csv(above_chron, "data_clean/above_ground_crn.csv")

##################################################################################
# make chronology for belowground data 
#############

#check correlation 
corr.rwl.seg(below_ground, seg.length = 10, bin.floor = 0, pcrit = .1676) #looks good
plot(below_ground, plot.type = "spag") #plot
detrend_below <- detrend(below_ground, method = "Spline") #detrend 

#make chronology 
roots_chron <- chron(detrend_below)
plot(roots_chron)

#save chronology 
write_csv(roots_chron, "data_clean/below_ground_crn.csv")

#############################################################################################
################### Compare chronologies to climate data ####################################
#############################################################################################

# PRECIPITATION FIRST

#WARNING this analysis takes forever to load and doesn't have to do with any class content
#i just have this in here because they are figures I'm going to use for my thesis 

#above ground
above_chron_daily <- above_chron %>%
  mutate(year = as.numeric(rownames(.))) %>% #make year numeric 
  filter(year >= 1950 & year <= 2021) %>% #make chronology same length as climate data
  select("std")#select standarised ring index 


#run analysis - this function is from dendrotools it compaires correlates ring width to precipitation 
#with different running window lengths 
above.chrn_response_pre <- daily_response(response = above_chron_daily, env_data = daily_precipitation, #add climate and tree ring data
                                   method = "cor", lower_limit = 1, upper_limit = 275,#set range of window lengths 
                                   row_names_subset = TRUE, previous_year = TRUE, #do analysis on curent and previous year 
                                   remove_insignificant = TRUE, alpha = 0.05, #set so nothing under 0.05 will show up
                                   subset_years = c(1951, 2021), #select years for correlation 
                                   aggregate_function = 'sum', reference_window="end") #set end of window to corelate to 

above.chrn_response_pre
summary(above.chrn_response_pre)

#plot the daily correlation analysis
plot(above.chrn_response_pre, type = 1)
plot(above.chrn_response_pre, type = 2)

#save the output 
png("outputs/above_chrn_response_type2.png", width = 800, height = 600)
plot(above.chrn_response_pre, type = 2)
dev.off()

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

png("outputs/roots_response_ppt.png", width = 800, height = 600)
plot(roots.chrn_response_pre, type = 2)
dev.off()


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

#save plot 
png("outputs/above_response_temp.png", width = 800, height = 600)
plot(above.chrn_response_temp, type = 2)
dev.off()


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

png("outputs/roots_response_temp.png", width = 800, height = 600)
plot(roots.chrn_response_temp, type = 2)
dev.off()

###########################################################################################
################### SUPERPOSED EPOCH ANALYSIS #############################################
###########################################################################################
# In this section I'm running a superposed epoch analysis between my two chronologies and #
# precipitation data, I am comparing the impact of drought during different precipitation #
# periods on tree ring growth- I want to see if trees are more sensitive to drought at    #
# different precipitation periods                                                         #
# the periods I am using are: previous monsoon, previous fall, winter, pre monsoon,       #
# wateryear, and monsoon                                                                  #
###########################################################################################

#read library
library(lubridate)

##########################
head(pre)

#CLASS CONTENT: week 7: dates and times
#change format of climate data to have date, value, year and month
ppt_sea_prep <- pre%>% #make new data frame to add the different precipitation periods too 
  mutate(month = month(date), 
         year = year(date)) %>% #make month and year column 
  relocate(month, .after = year) #put month after year 

head(ppt_sea_prep) #look at data
tail(ppt_sea_prep)

# add in column for different precipitation timing
ppt_sea_prep <- mutate(ppt_sea_prep,
                    season = case_when(
                      month(date) %in% c(7,8,9) ~ "prev_mons",
                      month(date) %in% c(10) ~ "prev_fall",
                      month(date) %in% c(11, 12,1, 2,3) ~ "winter",
                      month(date) %in% c(4,5,6) ~ "pre_mons",
                      T ~ NA_character_))


#create wateryear column
wateryear_breaks <- seq(as.Date("1950-10-01"), length=72, by="year") #make a vector of water years from 1950-2021 
years.wateryear.breaks = as.numeric(format(wateryear_breaks,"%Y")) #make year numeric 
labels.wateryear = years.wateryear.breaks[2:length(wateryear_breaks)]  #label water yeas
ppt_sea_prep$wateryear <- cut(ppt_sea_prep$date, wateryear_breaks,labels=labels.wateryear)#add water year to ppt_sea_prep data frame

#make wateryear data frame that has total precipitation for each water year
wateryear <- ppt_sea_prep %>%
  group_by(wateryear) %>% 
  summarise(wateryear_ppt=sum(prec_cm)) 


#SEASONS 
#LAG weather
prev_mons <- ppt_sea_prep %>% 
  filter(season == 'prev_mons') %>% #take only prev_mons
  group_by(year) %>% 
  summarise(prev_mons_ppt=sum(prec_cm)) %>% #make column with total precipitation for prev_mons
  mutate(prev_mons_ppt = lag(prev_mons_ppt, default = NA))%>% #shift ppt values down by one so you get the lag effect on the next year 
  mutate(year = factor(year)) #make year factor 
head(prev_mons)

#do the same for fall 
prev_fall <- ppt_sea_prep %>% 
  filter(season == 'prev_fall')  %>%
  group_by(year)%>% 
  summarise(prev_fall_ppt=sum(prec_cm))%>%
  mutate(prev_fall_ppt = lag(prev_fall_ppt, default = NA))%>%
  mutate(year = factor(year))

#WINTER
winter <- ppt_sea_prep %>% 
  filter(season == 'winter')

#do the same for winter as for water year 
winter_breaks <- seq(as.Date("1950-11-01"), length=72, by="year")
years.winter.breaks = as.numeric(format(winter_breaks,"%Y"))
labels.winter = years.winter.breaks[2:length(winter_breaks)]
winter$winter <- cut(winter$date, winter_breaks,labels=labels.winter)

#make winter df with total winter ppt
winter <- winter %>% group_by(winter) %>% 
  summarise(winter_ppt=sum(prec_cm)) 


#CURRENT weather
#make date frame for pre monsoon weather 
pre_mons <- ppt_sea_prep %>% 
  filter(season == 'pre_mons') %>% 
  group_by(year) %>% 
  summarise(pre_mons_ppt=sum(prec_cm)) %>%
  mutate(year = factor(year))

#make data frame for monsoon
monsoon <- ppt_sea_prep %>% 
  filter(month >= 7 & month <= 9)%>% 
  group_by(year)%>% 
  summarise(monsoon_ppt=sum(prec_cm)) %>%
  mutate(year = factor(year))

#check data 
head(monsoon)

##########################################################################################
#the date is a factor and i need it to be numeric because i was running into problems when
#trying to plot with year as a factor 

#convert everything to numeric 
prev_mons$year <- as.numeric(as.character(prev_mons$year))
head(prev_mons)
prev_fall$year <- as.numeric(as.character(prev_fall$year))
winter$winter <- as.numeric(as.character(winter$winter))
pre_mons$year <- as.numeric(as.character(pre_mons$year))
monsoon$year <- as.numeric(as.character(monsoon$year))
wateryear$wateryear <- as.numeric(as.character(wateryear$wateryear))

#CLASS CONTENT: week 4: Groups and joins 
#add everything into one dataframe to use for plotting
summary_ppt_sea <- prev_mons %>%
  left_join(prev_fall, by = "year") %>%
  left_join(winter, by = c("year" = "winter")) %>%
  left_join(pre_mons, by = "year") %>%
  left_join(monsoon, by = "year") %>%
  left_join(wateryear, by = c("year" = "wateryear")) %>%
  mutate(year = as.numeric(year))
head(summary_ppt_sea)

#save this datafram 
write_csv(summary_ppt_sea, "data_clean/ppt_data_for_sea.csv")

##############################################################################################
# for the SEA analysis I am going to use the top 10 percentiles drought years in each of the #
# time periods                                                                               #
# below I make data frames with each precipitation windows drought years                     #
##############################################################################################

##########
#make data frame for driest winter years 
winter_drought_yrs <- summary_ppt_sea %>%
  select(year,winter_ppt) %>%
  arrange(winter_ppt) %>% #arrange by smallest to largest
  slice(1:(n() * 0.10)) #select only top 10 driest 
print(winter_drought_yrs)

###########
#do same for wateryear
wateryear_drought_yrs <- summary_ppt_sea %>%
  select(year,wateryear_ppt)%>% #select columns
  arrange(wateryear_ppt) %>% #arrange by smallest to largest
  slice(1:(n() * 0.10)) #select only top 10 driest 

############
#prev monsoon
prev_mons_drought_yrs <- summary_ppt_sea %>%
  select(year, prev_mons_ppt) %>% #select columns
  arrange(prev_mons_ppt) %>% #arrange by smallest to largest
  slice(1:(n() * 0.10)) #select only top 10 driest 

############
#Prev fall
prev_fall_drought_yrs <- summary_ppt_sea %>%
  select(year, prev_fall_ppt) %>% #select columns
  arrange(prev_fall_ppt) %>% #arrange by smallest to largest
  slice(1:(n() * 0.10)) #select only top 10 driest 

############
#pre mons
pre_mons_drought_yrs <- summary_ppt_sea %>%
  select(year, pre_mons_ppt) %>%
  arrange(pre_mons_ppt) %>% #arrange by smallest to largest
  slice(1:(n() * 0.10)) #select only top 10 driest 

############
#monsoon 
monsoon_drought_yrs <- summary_ppt_sea %>%
  select(year, monsoon_ppt) %>% 
  arrange(monsoon_ppt) %>% #arrange by smallest to largest
  slice(1:(n() * 0.10)) #select only top 10 driest 

##########################################################################################
#make lists for the function and loop 
#########

#make a list of drought periods
drought_periods <- list(
  "Water Year Precipitation" = wateryear_drought_yrs$year,
  "Winter Precipitation" = winter_drought_yrs$year,
  "Pre-Monsoon Precipitation" = pre_mons_drought_yrs$year,
  "Previous Monsoon Precipitation" = prev_mons_drought_yrs$year,
  "Previous Fall Precipitation" = prev_fall_drought_yrs$year,
  "Monsoon Precipitation" = monsoon_drought_yrs$year
)

#make a list of chronologies 
chronologies <- list(
  "Aboveground" = above_chron,
  "Belowground" = roots_chron
)

#CLASS CONTENT: Week 11: Functions 
#make a function for SEA analysis, structure of function came from chatGPT
# function works by 
# 1. takes arguments of chronology name, period name, drought years, chronology date
# 2. takes all the years that are not drought years, makes a mean and sd from chronology data in those years
# 3. runs the superposed epoch analysis for the drought years per drought period 
# 4. put the sea results into a data frame 
# 5. plots the results of the sea analysis 

run_sea_plot <- function(chron, period, years, chronology_data) {
  normal <- chronology_data %>%
    rownames_to_column("year") %>% #make year column
    filter(!year %in% years) %>% #keep only non drought years
    mutate(std = as.numeric(std)) #make std numeric 
  
  mn <- mean(normal$std, na.rm = TRUE) #calculate non drought mean
  sdev <- sd(normal$std, na.rm = TRUE) #calculate non drought standard deviation 
  
  #do sea analysis 
  sea_result <- sea(chronology_data, years, lag = 2, resample = 1000) 
  
  #make dataframe for sea
  sea_df <- data.frame(
    lag = sea_result$lag, #make lag column (2 year lag)
    response = sea_result$se.unscaled, #make sea response column
    p_value = sea_result$p, #make p value column
    sig = sea_result$p < 0.10, #make significance column
    upper = mn + sdev, #1 standard deviation above mean
    lower = mn - sdev #one standard deviation below the mean 
  )

  #CLASS Content: week 5: data visualization   
  #code for plots
  ggplot(sea_df, aes(x = lag, y = response)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "blue", alpha = 0.1) + #sd shading
    geom_line(color = "red", size = 1.5) + #sea line
    geom_hline(yintercept = mn, linetype = "dashed", color = "red") + #mean line
    geom_point(aes(color = sig, shape = sig), size = 3) + #set shape and color to be based on significance of p value 
    scale_color_manual(values = c("FALSE" = "black", "TRUE" = "darkgreen")) + #color based on p value 
    scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 8)) + #shape based on p value 8 = star 
    labs(
      title = paste0(period_name, " - ", chron_name), #add tiles that have drought period and chronologies separated by dash 
      x = "Superposed Epoch (lag)", #x label 
      y = "RWI" #y label 
    ) +
    theme_classic(base_size = 14) #set theme 
}

#CLASS CONTENT week 13: Iteration
# Loop over each combination of chronology and drought period - structure of loop came from chatGPT
all_plots <- list() #make a empty list for plots to be stored in

for (chron_name in names(chronologies)) { #set up the first loop to look into the chronologies list
  for (period_name in names(drought_periods)) { #second layer to look in the drought periods list per chronology 
    chron_data <- chronologies[[chron_name]] #ruining through the chronologies list looking at column chron_name
    drought_years <- drought_periods[[period_name]] #running through drought_periods list looking at period_name
    
    #making the plot for each chronology in each drought period using the function that was just made 
    plot <- run_sea_plot(chron_name, period_name, drought_years, chron_data) 
    
    # Store plot with a unique name
    plot_key <- paste(chron_name, period_name, sep = "_")#saving plots with unique name based on chron_name and drought period
    all_plots[[plot_key]] <- plot #saving each plot into the all_plots list 
  }
}


#week 13: Iteration 
# Save each plot in the list - Structure of loop came from chatGPT
for (plot_name in names(all_plots)) {
  ggsave(
    filename = paste0("outputs/", plot_name, ".png"),  #save each plot as PNG 
    plot = all_plots[[plot_name]], 
    width = 8, height = 6, dpi = 300 #set dimensions 
  )
}



#look at all the plots 
walk(all_plots, print)

##########################################################################################################
