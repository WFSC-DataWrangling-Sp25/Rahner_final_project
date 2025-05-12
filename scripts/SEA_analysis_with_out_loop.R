
################################################################################
#make vectors that just have the dryiest years for each time period
wateryear_PPT <- top_10_percent_lowest_PPT_wateryear$year
monsoon_PPT <-top_10_percent_lowest_PPT_monsoon$year
pre_mons_PPT <-top_10_percent_lowest_PPT_pre_mons$year
prev_fall_PPT <- top_10_percent_lowest_PPT_prev_fall$year
prev_mons_PPT <-top_10_percent_lowest_PPT_prev_mons$year
winter_PPT <- top_10_percent_lowest_PPT_winter$year

################################################################################

#now we are going to do the analysis 
#the chronologies we are going to use are
#   above_chron (above ground tree rings)
#   roots_chron (roots tree ring)

#start with the aboveground chronology

###################################################
################### WATER YEAR ####################
###################################################

#wateryear 
normal <- above_chron%>% 
  rownames_to_column()%>%   
  rename(year = rowname)%>%
  filter(!year %in% wateryear_PPT) #filter for the drought water year drought years
mean <- mean(normal$std, na.rm = TRUE)
sd <- sd(normal$std,na.rm=TRUE)

##do the SEA analysis, going to use two lag years, and do 1000 randomizations
sea <- sea(above_chron, wateryear_PPT, lag = 2, resample = 1000)

#make data frame for the SEA results
sea_water_yr <- data.frame(
  lag = sea$lag,
  response = sea$se.unscaled,  #Extract mean tree growth response
  p_value = sea$p,
  sig = sea$p < 0.10,  #set p value sig to < .1 
  upper = mean + sd,
  lower = mean - sd 
)


############


ggplot(sea_water_yr, aes(x = lag, y = response)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "blue", alpha = 0.1) + #add the shading for standard devation based on the upper and lower limits 
  geom_line(color = "red", size = 1.5) +  #add the SEA line
  geom_hline(yintercept = mean, linetype = "dashed", color = "red") + #add mean
  
  #distinguish the significant points based on p value
  geom_point(aes(color = sig, shape = sig), size = 3) +
  scale_color_manual(values = c("FALSE" = "black", "TRUE" = "black")) +
  scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 8)) +
  labs(
    title = "Water Year Precipitation",
    x = "Superposed Epoch (lag)",
    y = "RWI",
  ) +
  theme_classic(base_size = 14)

###################################################
##################### WINTER ######################
###################################################

normal <- above_chron%>% 
  rownames_to_column()%>%   
  rename(year = rowname)%>%
  filter(!year %in% winter_PPT) #filter for the drought water year drought years
mean <- mean(normal$std, na.rm = TRUE)
sd <- sd(normal$std,na.rm=TRUE)

##do the SEA analysis, going to use two lag years, and do 1000 randomizations
sea <- sea(above_chron, winter_PPT, lag = 2, resample = 1000)

#make data frame for the SEA results
sea_winter_yr <- data.frame(
  lag = sea$lag,
  response = sea$se.unscaled,  #Extract mean tree growth response
  p_value = sea$p,
  sig = sea$p < 0.10,  #set p value sig to < .1 
  upper = mean + sd,
  lower = mean - sd 
)


############


ggplot(sea_winter_yr, aes(x = lag, y = response)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "blue", alpha = 0.1) + #add the shading for standard devation based on the upper and lower limits 
  geom_line(color = "red", size = 1.5) +  #add the SEA line
  geom_hline(yintercept = mean, linetype = "dashed", color = "red") + #add mean
  
  #distinguish the significant points based on p value
  geom_point(aes(color = sig, shape = sig), size = 3) +
  scale_color_manual(values = c("FALSE" = "black", "TRUE" = "black")) +
  scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 8)) +
  labs(
    title = "Winter Precipitation",
    x = "Superposed Epoch (lag)",
    y = "RWI",
  ) +
  theme_classic(base_size = 14)

###################################################
################ PRE MONSOON ######################
###################################################

normal <- above_chron%>% 
  rownames_to_column()%>%   
  rename(year = rowname)%>%
  filter(!year %in% pre_mons_PPT) #filter for the drought water year drought years
mean <- mean(normal$std, na.rm = TRUE)
sd <- sd(normal$std,na.rm=TRUE)

##do the SEA analysis, going to use two lag years, and do 1000 randomizations
sea <- sea(above_chron, pre_mons_PPT, lag = 2, resample = 1000)

#make data frame for the SEA results
sea_pre_mons_PPT <- data.frame(
  lag = sea$lag,
  response = sea$se.unscaled,  #Extract mean tree growth response
  p_value = sea$p,
  sig = sea$p < 0.10,  #set p value sig to < .1 
  upper = mean + sd,
  lower = mean - sd 
)


############


ggplot(sea_pre_mons_PPT, aes(x = lag, y = response)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "blue", alpha = 0.1) + #add the shading for standard devation based on the upper and lower limits 
  geom_line(color = "red", size = 1.5) +  #add the SEA line
  geom_hline(yintercept = mean, linetype = "dashed", color = "red") + #add mean
  
  #distinguish the significant points based on p value
  geom_point(aes(color = sig, shape = sig), size = 3) +
  scale_color_manual(values = c("FALSE" = "black", "TRUE" = "black")) +
  scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 8)) +
  labs(
    title = "Pre-Monsoon Precipitation",
    x = "Superposed Epoch (lag)",
    y = "RWI",
  ) +
  theme_classic(base_size = 14)

###################################################
################ PREVEOUS MONSOON #################
###################################################

normal <- above_chron%>% 
  rownames_to_column()%>%   
  rename(year = rowname)%>%
  filter(!year %in% prev_mons_PPT) #filter for the drought water year drought years
mean <- mean(normal$std, na.rm = TRUE)
sd <- sd(normal$std,na.rm=TRUE)

##do the SEA analysis, going to use two lag years, and do 1000 randomizations
sea <- sea(above_chron, prev_mons_PPT, lag = 2, resample = 1000)

#make data frame for the SEA results
sea_prev_mons_PPT <- data.frame(
  lag = sea$lag,
  response = sea$se.unscaled,  #Extract mean tree growth response
  p_value = sea$p,
  sig = sea$p < 0.10,  #set p value sig to < .1 
  upper = mean + sd,
  lower = mean - sd 
)


############


ggplot(sea_prev_mons_PPT, aes(x = lag, y = response)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "blue", alpha = 0.1) + #add the shading for standard devation based on the upper and lower limits 
  geom_line(color = "red", size = 1.5) +  #add the SEA line
  geom_hline(yintercept = mean, linetype = "dashed", color = "red") + #add mean
  
  #distinguish the significant points based on p value
  geom_point(aes(color = sig, shape = sig), size = 3) +
  scale_color_manual(values = c("FALSE" = "black", "TRUE" = "black")) +
  scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 8)) +
  labs(
    title = "Preveous Monsoon Precipitation",
    x = "Superposed Epoch (lag)",
    y = "RWI",
  ) +
  theme_classic(base_size = 14)

###################################################
################ PREVEOUS FALL ####################
###################################################

normal <- above_chron%>% 
  rownames_to_column()%>%   
  rename(year = rowname)%>%
  filter(!year %in% prev_fall_PPT) #filter for the drought water year drought years
mean <- mean(normal$std, na.rm = TRUE)
sd <- sd(normal$std,na.rm=TRUE)

##do the SEA analysis, going to use two lag years, and do 1000 randomizations
sea <- sea(above_chron, prev_fall_PPT, lag = 2, resample = 1000)

#make data frame for the SEA results
sea_prev_fall_PPT <- data.frame(
  lag = sea$lag,
  response = sea$se.unscaled,  #Extract mean tree growth response
  p_value = sea$p,
  sig = sea$p < 0.10,  #set p value sig to < .1 
  upper = mean + sd,
  lower = mean - sd 
)


############


ggplot(sea_prev_fall_PPT, aes(x = lag, y = response)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "blue", alpha = 0.1) + #add the shading for standard devation based on the upper and lower limits 
  geom_line(color = "red", size = 1.5) +  #add the SEA line
  geom_hline(yintercept = mean, linetype = "dashed", color = "red") + #add mean
  
  #distinguish the significant points based on p value
  geom_point(aes(color = sig, shape = sig), size = 3) +
  scale_color_manual(values = c("FALSE" = "black", "TRUE" = "black")) +
  scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 8)) +
  labs(
    title = "Preveous fall Precipitation",
    x = "Superposed Epoch (lag)",
    y = "RWI",
  ) +
  theme_classic(base_size = 14)

###################################################
#################### MONSOON ######################
###################################################

normal <- above_chron%>% 
  rownames_to_column()%>%   
  rename(year = rowname)%>%
  filter(!year %in% monsoon_PPT) #filter for the drought water year drought years
mean <- mean(normal$std, na.rm = TRUE)
sd <- sd(normal$std,na.rm=TRUE)

##do the SEA analysis, going to use two lag years, and do 1000 randomizations
sea <- sea(above_chron, monsoon_PPT, lag = 2, resample = 1000)

#make data frame for the SEA results
sea_monsoon_PPT <- data.frame(
  lag = sea$lag,
  response = sea$se.unscaled,  #Extract mean tree growth response
  p_value = sea$p,
  sig = sea$p < 0.10,  #set p value sig to < .1 
  upper = mean + sd,
  lower = mean - sd 
)


############


ggplot(sea_monsoon_PPT, aes(x = lag, y = response)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "blue", alpha = 0.1) + #add the shading for standard devation based on the upper and lower limits 
  geom_line(color = "red", size = 1.5) +  #add the SEA line
  geom_hline(yintercept = mean, linetype = "dashed", color = "red") + #add mean
  
  #distinguish the significant points based on p value
  geom_point(aes(color = sig, shape = sig), size = 3) +
  scale_color_manual(values = c("FALSE" = "black", "TRUE" = "black")) +
  scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 8)) +
  labs(
    title = "Monsoon Precipitation",
    x = "Superposed Epoch (lag)",
    y = "RWI",
  ) +
  theme_classic(base_size = 14)

################################################################################
################################################################################

#blow ground plots 

###################################################
################### WATER YEAR ####################
###################################################

#wateryear 
normal <- roots_chron%>% 
  rownames_to_column()%>%   
  rename(year = rowname)%>%
  filter(!year %in% wateryear_PPT) #filter for the drought water year drought years
mean <- mean(normal$std, na.rm = TRUE)
sd <- sd(normal$std,na.rm=TRUE)

##do the SEA analysis, going to use two lag years, and do 1000 randomizations
sea <- sea(roots_chron, wateryear_PPT, lag = 2, resample = 1000)

#make data frame for the SEA results
sea_water_yr <- data.frame(
  lag = sea$lag,
  response = sea$se.unscaled,  #Extract mean tree growth response
  p_value = sea$p,
  sig = sea$p < 0.10,  #set p value sig to < .1 
  upper = mean + sd,
  lower = mean - sd 
)


############


ggplot(sea_water_yr, aes(x = lag, y = response)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "blue", alpha = 0.1) + #add the shading for standard devation based on the upper and lower limits 
  geom_line(color = "red", size = 1.5) +  #add the SEA line
  geom_hline(yintercept = mean, linetype = "dashed", color = "red") + #add mean
  
  #distinguish the significant points based on p value
  geom_point(aes(color = sig, shape = sig), size = 3) +
  scale_color_manual(values = c("FALSE" = "black", "TRUE" = "black")) +
  scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 8)) +
  labs(
    title = "Water Year Precipitation",
    x = "Superposed Epoch (lag)",
    y = "RWI",
  ) +
  theme_classic(base_size = 14)

###################################################
##################### WINTER ######################
###################################################

normal <- roots_chron%>% 
  rownames_to_column()%>%   
  rename(year = rowname)%>%
  filter(!year %in% winter_PPT) #filter for the drought water year drought years
mean <- mean(normal$std, na.rm = TRUE)
sd <- sd(normal$std,na.rm=TRUE)

##do the SEA analysis, going to use two lag years, and do 1000 randomizations
sea <- sea(roots_chron, winter_PPT, lag = 2, resample = 1000)

#make data frame for the SEA results
sea_winter_yr <- data.frame(
  lag = sea$lag,
  response = sea$se.unscaled,  #Extract mean tree growth response
  p_value = sea$p,
  sig = sea$p < 0.10,  #set p value sig to < .1 
  upper = mean + sd,
  lower = mean - sd 
)


############


ggplot(sea_winter_yr, aes(x = lag, y = response)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "blue", alpha = 0.1) + #add the shading for standard devation based on the upper and lower limits 
  geom_line(color = "red", size = 1.5) +  #add the SEA line
  geom_hline(yintercept = mean, linetype = "dashed", color = "red") + #add mean
  
  #distinguish the significant points based on p value
  geom_point(aes(color = sig, shape = sig), size = 3) +
  scale_color_manual(values = c("FALSE" = "black", "TRUE" = "black")) +
  scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 8)) +
  labs(
    title = "Winter Precipitation",
    x = "Superposed Epoch (lag)",
    y = "RWI",
  ) +
  theme_classic(base_size = 14)

###################################################
################ PRE MONSOON ######################
###################################################

normal <- roots_chron%>% 
  rownames_to_column()%>%   
  rename(year = rowname)%>%
  filter(!year %in% pre_mons_PPT) #filter for the drought water year drought years
mean <- mean(normal$std, na.rm = TRUE)
sd <- sd(normal$std,na.rm=TRUE)

##do the SEA analysis, going to use two lag years, and do 1000 randomizations
sea <- sea(roots_chron, pre_mons_PPT, lag = 2, resample = 1000)

#make data frame for the SEA results
sea_pre_mons_PPT <- data.frame(
  lag = sea$lag,
  response = sea$se.unscaled,  #Extract mean tree growth response
  p_value = sea$p,
  sig = sea$p < 0.10,  #set p value sig to < .1 
  upper = mean + sd,
  lower = mean - sd 
)


############


ggplot(sea_pre_mons_PPT, aes(x = lag, y = response)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "blue", alpha = 0.1) + #add the shading for standard devation based on the upper and lower limits 
  geom_line(color = "red", size = 1.5) +  #add the SEA line
  geom_hline(yintercept = mean, linetype = "dashed", color = "red") + #add mean
  
  #distinguish the significant points based on p value
  geom_point(aes(color = sig, shape = sig), size = 3) +
  scale_color_manual(values = c("FALSE" = "black", "TRUE" = "black")) +
  scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 8)) +
  labs(
    title = "Pre-Monsoon Precipitation",
    x = "Superposed Epoch (lag)",
    y = "RWI",
  ) +
  theme_classic(base_size = 14)

###################################################
################ PREVEOUS MONSOON #################
###################################################

normal <- roots_chron%>% 
  rownames_to_column()%>%   
  rename(year = rowname)%>%
  filter(!year %in% prev_mons_PPT) #filter for the drought water year drought years
mean <- mean(normal$std, na.rm = TRUE)
sd <- sd(normal$std,na.rm=TRUE)

##do the SEA analysis, going to use two lag years, and do 1000 randomizations
sea <- sea(roots_chron, prev_mons_PPT, lag = 2, resample = 1000)

#make data frame for the SEA results
sea_prev_mons_PPT <- data.frame(
  lag = sea$lag,
  response = sea$se.unscaled,  #Extract mean tree growth response
  p_value = sea$p,
  sig = sea$p < 0.10,  #set p value sig to < .1 
  upper = mean + sd,
  lower = mean - sd 
)


############


ggplot(sea_prev_mons_PPT, aes(x = lag, y = response)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "blue", alpha = 0.1) + #add the shading for standard devation based on the upper and lower limits 
  geom_line(color = "red", size = 1.5) +  #add the SEA line
  geom_hline(yintercept = mean, linetype = "dashed", color = "red") + #add mean
  
  #distinguish the significant points based on p value
  geom_point(aes(color = sig, shape = sig), size = 3) +
  scale_color_manual(values = c("FALSE" = "black", "TRUE" = "black")) +
  scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 8)) +
  labs(
    title = "Preveous Monsoon Precipitation",
    x = "Superposed Epoch (lag)",
    y = "RWI",
  ) +
  theme_classic(base_size = 14)

###################################################
################ PREVEOUS FALL ####################
###################################################

normal <- roots_chron%>% 
  rownames_to_column()%>%   
  rename(year = rowname)%>%
  filter(!year %in% prev_fall_PPT) #filter for the drought water year drought years
mean <- mean(normal$std, na.rm = TRUE)
sd <- sd(normal$std,na.rm=TRUE)

##do the SEA analysis, going to use two lag years, and do 1000 randomizations
sea <- sea(roots_chron, prev_fall_PPT, lag = 2, resample = 1000)

#make data frame for the SEA results
sea_prev_fall_PPT <- data.frame(
  lag = sea$lag,
  response = sea$se.unscaled,  #Extract mean tree growth response
  p_value = sea$p,
  sig = sea$p < 0.10,  #set p value sig to < .1 
  upper = mean + sd,
  lower = mean - sd 
)


############


ggplot(sea_prev_fall_PPT, aes(x = lag, y = response)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "blue", alpha = 0.1) + #add the shading for standard devation based on the upper and lower limits 
  geom_line(color = "red", size = 1.5) +  #add the SEA line
  geom_hline(yintercept = mean, linetype = "dashed", color = "red") + #add mean
  
  #distinguish the significant points based on p value
  geom_point(aes(color = sig, shape = sig), size = 3) +
  scale_color_manual(values = c("FALSE" = "black", "TRUE" = "black")) +
  scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 8)) +
  labs(
    title = "Preveous fall Precipitation",
    x = "Superposed Epoch (lag)",
    y = "RWI",
  ) +
  theme_classic(base_size = 14)

###################################################
#################### MONSOON ######################
###################################################

normal <- roots_chron%>% 
  rownames_to_column()%>%   
  rename(year = rowname)%>%
  filter(!year %in% monsoon_PPT) #filter for the drought water year drought years
mean <- mean(normal$std, na.rm = TRUE)
sd <- sd(normal$std,na.rm=TRUE)

##do the SEA analysis, going to use two lag years, and do 1000 randomizations
sea <- sea(roots_chron, monsoon_PPT, lag = 2, resample = 1000)

#make data frame for the SEA results
sea_monsoon_PPT <- data.frame(
  lag = sea$lag,
  response = sea$se.unscaled,  #Extract mean tree growth response
  p_value = sea$p,
  sig = sea$p < 0.10,  #set p value sig to < .1 
  upper = mean + sd,
  lower = mean - sd 
)


############


ggplot(sea_monsoon_PPT, aes(x = lag, y = response)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "blue", alpha = 0.1) + #add the shading for standard devation based on the upper and lower limits 
  geom_line(color = "red", size = 1.5) +  #add the SEA line
  geom_hline(yintercept = mean, linetype = "dashed", color = "red") + #add mean
  
  #distinguish the significant points based on p value
  geom_point(aes(color = sig, shape = sig), size = 3) +
  scale_color_manual(values = c("FALSE" = "black", "TRUE" = "black")) +
  scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 8)) +
  labs(
    title = "Monsoon Precipitation",
    x = "Superposed Epoch (lag)",
    y = "RWI",
  ) +
  theme_classic(base_size = 14)


