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

#fix formatting of tep separate by space 

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
for (i in 1:length(rwl_files)) {
  rwl <- read.rwl(paste0(rwl_files[i]))
  all_data_list[[i]] <- rwl
}

# Combine into one data frame 
combined_rwl <- combine.rwl(all_data_list)

# separate into aboveground data (stem) and belowground (roots)
aboveground_cols <- str_detect(names(combined_rwl), "L|BA|BB|MA|MB|TA|TB|P")
above_ground <- combined_rwl[, aboveground_cols]
below_ground <- combined_rwl[, !aboveground_cols]


#detrend data and make chronologies for above ground and belowground data 

#check correlation 
corr.rwl.seg(above_ground, seg.length = 10, bin.floor = 0, pcrit = 0.166) #looks good
#quick plot
plot(above_ground, plot.type = "spag")

#aboveground chronology 
detrend_above <- detrend(above_ground, method = "Spline", nyrs = 20)

above_chron <- chron(detrend_above)

plot(above_chron)

#beloground 
#check correlation 
corr.rwl.seg(below_ground, seg.length = 10, bin.floor = 0, pcrit = .1676) #looks good
#quick plot
plot(below_ground, plot.type = "spag")

detrend_below <- detrend(below_ground, method = "Spline")

inter_cor_value <- interseries.cor(detrend_below)

mean_rbar <- mean(inter_cor_value$res.cor, na.rm = TRUE)
print(mean_rbar)

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

#######################################################################################
################### SUPERPOSED EPOCH ANALYSIS #########################################
#######################################################################################

#start with precipitation

#change format of climate data have date, value, year and month

head(pre)

library(lubridate)
month(pre$date)

ppt_sea_prep <- pre%>%
  mutate(month = month(date), year = year(date)) %>%
  relocate(month, .after = year) %>%
  subset(year >= 1950 & year < 2021) %>%
  rename(prec = )


# converts to date format
ppt_sea_prep$date <- date(ppt_sea_prep$date)
head(ppt_sea_prep)

# add in columns for different precipitation timing
ppt_sea_prep <- mutate(ppt_sea_prep,
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

# add in columns october 1st to september 30th
wateryear_breaks <- seq(as.Date("1900-10-01"), length=122, by="year")  
years.wateryear.breaks = as.numeric(format(wateryear_breaks,"%Y"))
labels.wateryear = years.wateryear.breaks[2:length(wateryear_breaks)]  # Why are we leaving off the first year in the water years label?
ppt_sea_prep$wateryear <- cut(ppt_sea_prep$date, wateryear_breaks,labels=labels.wateryear)

wateryear <- ppt_sea_prep %>%
  group_by(wateryear) %>% 
  summarise(wateryear_ppt=sum(prec_cm)) 


#seasons
#LAG weather
prev_mons <- ppt_sea_prep %>% 
  filter(season == 'prev_mons') %>%
  group_by(year) %>% 
  summarise(prev_mons_ppt=sum(prec_cm)) %>%
  mutate(prev_mons_ppt = lag(prev_mons_ppt, default = NA))%>%
  mutate(year = factor(year))
head(prev_mons)


prev_fall <- ppt_sea_prep %>% 
  filter(season == 'prev_fall')  %>%
  group_by(year)%>% 
  summarise(prev_fall_ppt=sum(prec_cm))%>%
  mutate(prev_fall_ppt = lag(prev_fall_ppt, default = NA))%>%
  mutate(year = factor(year))

#WINTER
winter <- ppt_sea_prep %>% 
  filter(season == 'winter')

winter_breaks <- seq(as.Date("1900-11-01"), length=122, by="year")
years.winter.breaks = as.numeric(format(winter_breaks,"%Y"))
labels.winter = years.winter.breaks[2:length(winter_breaks)]
winter$winter <- cut(winter$date, winter_breaks,labels=labels.winter)

winter <- winter %>% group_by(winter) %>% 
  summarise(winter_ppt=sum(prec_cm)) 


#CURRENT weather
pre_mons <- ppt_sea_prep %>% filter(season == 'pre_mons') %>% 
  group_by(year) %>% 
  summarise(pre_mons_ppt=sum(prec_cm)) %>%
  mutate(year = factor(year))

#monsoon
monsoon <- ppt_sea_prep %>% filter(month >= 7 & month <= 9)%>% 
  group_by(year)%>% 
  summarise(monsoon_ppt=sum(prec_cm)) %>%
  mutate(year = factor(year))

head(monsoon)

#the date is a factor and i need it to be numeric so plotting and everything works
#out 

prev_mons$year <- as.numeric(as.character(prev_mons$year))
head(prev_mons)
prev_fall$year <- as.numeric(as.character(prev_fall$year))
winter$winter <- as.numeric(as.character(winter$winter))
pre_mons$year <- as.numeric(as.character(pre_mons$year))
monsoon$year <- as.numeric(as.character(monsoon$year))
wateryear$wateryear <- as.numeric(as.character(wateryear$wateryear))

#add everything into one dataframe to use for plotting
summarise_ppt_sea_prep <- prev_mons %>%
  left_join(prev_fall, by = "year") %>%
  left_join(winter, by = c("year" = "winter")) %>%
  left_join(pre_mons, by = "year") %>%
  left_join(monsoon, by = "year") %>%
  left_join(wateryear, by = c("year" = "wateryear")) %>%
  mutate(year = as.numeric(year))
head(summarise_ppt_sea_prep)

#for the SPE analysis I am going to use top 10 percenties of drought in each of the 
#time periods to see what timing of drought has the largest effect on tree grwoth 

##########
#driest winter years 
top_10_percent_lowest_PPT_winter <- summarise_ppt_sea_prep %>%
  select(year,winter_ppt) %>%
  arrange(winter_ppt) %>%
  slice(1:(n() * 0.10))
print(top_10_percent_lowest_PPT_winter)

###########
#do same for wateryear
top_10_percent_lowest_PPT_wateryear <- summarise_ppt_sea_prep %>%
  select(year,wateryear_ppt)%>% #select columns
  arrange(wateryear_ppt) %>% #arrange by smallest to largest
  slice(1:(n() * 0.10))

############
#prev monsoon
top_10_percent_lowest_PPT_prev_mons <- summarise_ppt_sea_prep %>%
  select(year, prev_mons_ppt) %>%        # Select columns
  arrange(prev_mons_ppt) %>%             # Arrange by smallest to largest
  slice(1:(n() * 0.10))


############
#Prev fall
top_10_percent_lowest_PPT_prev_fall <- summarise_ppt_sea_prep %>%
  select(year, prev_fall_ppt) %>%        # Select columns
  arrange(prev_fall_ppt) %>%             # Arrange by smallest to largest
  slice(1:(n() * 0.10))

############
#pre mons
top_10_percent_lowest_PPT_pre_mons <- summarise_ppt_sea_prep %>%
  select(year, pre_mons_ppt) %>%        # Select columns
  arrange(pre_mons_ppt) %>%             # Arrange by smallest to largest
  slice(1:(n() * 0.10))

############
#monsoon 
top_10_percent_lowest_PPT_monsoon <- summarise_ppt_sea_prep %>%
  select(year, monsoon_ppt) %>%        # Select columns
  arrange(monsoon_ppt) %>%             # Arrange by smallest to largest
  slice(1:(n() * 0.10))

#################
#for loop withing a for loop that i created with the help of chat gpt 


#make a list of drought periods
drought_periods <- list(
  "Water Year Precipitation" = top_10_percent_lowest_PPT_wateryear$year,
  "Winter Precipitation" = top_10_percent_lowest_PPT_winter$year,
  "Pre-Monsoon Precipitation" = top_10_percent_lowest_PPT_pre_mons$year,
  "Previous Monsoon Precipitation" = top_10_percent_lowest_PPT_prev_mons$year,
  "Previous Fall Precipitation" = top_10_percent_lowest_PPT_prev_fall$year,
  "Monsoon Precipitation" = top_10_percent_lowest_PPT_monsoon$year
)

#make a list of chronologies 
chronologies <- list(
  "Aboveground" = above_chron,
  "Belowground" = roots_chron
)

#make a function for SEA analysis 
run_sea_plot <- function(chron_name, period_name, years, chronology_data) {
  #filter drought years to find mean and sd
  normal <- chronology_data %>%
    rownames_to_column("year") %>%
    filter(!year %in% years) %>%
    mutate(std = as.numeric(std))
  
  mn <- mean(normal$std, na.rm = TRUE)
  sdev <- sd(normal$std, na.rm = TRUE)
  
  #do sea analysis 
  sea_result <- sea(chronology_data, years, lag = 2, resample = 1000)
  
  #make datafram for sea
  sea_df <- data.frame(
    lag = sea_result$lag,
    response = sea_result$se.unscaled,
    p_value = sea_result$p,
    sig = sea_result$p < 0.10,
    upper = mn + sdev,
    lower = mn - sdev
  )
  
  #code for plots
  ggplot(sea_df, aes(x = lag, y = response)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "blue", alpha = 0.1) + #sd shading
    geom_line(color = "red", size = 1.5) + #sea line
    geom_hline(yintercept = mn, linetype = "dashed", color = "red") + #mean line
    geom_point(aes(color = sig, shape = sig), size = 3) + #shapes based on p value
    scale_color_manual(values = c("FALSE" = "black", "TRUE" = "darkgreen")) +
    scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 8)) +
    labs(
      title = paste0(period_name, " - ", chron_name),
      x = "Superposed Epoch (lag)",
      y = "RWI"
    ) +
    theme_classic(base_size = 14)
}

# Loop over each combination of chronology and drought period
all_plots <- list()
for (chron_name in names(chronologies)) {
  for (period_name in names(drought_periods)) {
    chron_data <- chronologies[[chron_name]]
    drought_years <- drought_periods[[period_name]]
    
    plot <- run_sea_plot(chron_name, period_name, drought_years, chron_data)
    
    # Store plot with a unique name
    plot_key <- paste(chron_name, period_name, sep = "_")
    all_plots[[plot_key]] <- plot
  }
}

walk(all_plots, print)

print(all_plots)

# Save each plot in the list
for (plot_name in names(all_plots)) {
  ggsave(
    filename = paste0("outputs/", plot_name, ".png"),  # save as PNG
    plot = all_plots[[plot_name]],
    width = 8, height = 6, dpi = 300
  )
}
