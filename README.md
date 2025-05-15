# Rahner_final_project

Final project for WFSC data wrangling. Investigating tree growth climate relationships both above and below ground.

Data includes: tree ring data as well as daily temperature and precipitation data. Tree ring data comes from Bandelier National Monument. Climate data comes from weather station USC00295084 LOS_ALAMOS,\_NM United States downloaded from <https://climexp.knmi.nl/start.cgi>. Tree ring data are in .rwl files and temperature and precipitation are in excel spreadsheets.

In the sub directory there are 4 folders:

1.  data_clean: Data that is correctly structured for analysis. This includes the above and below ground chronologies, the daily temperature and precipitation data, and the precipitation data for the superposed epoch analysis.
2.  data_raw: Raw data that has not been processed for analysis. Contains raw ring width data and climate data.
3.  outputs: Plots made during analysis.
4.  scripts: Code used for analysis, script: final_project.R is where all the code is for this project. Also in scripts in my final project proposal called: FinalProjectProposal.Rmd

Class content in the code by line:

31: Week 6: Tidy Data, rename function

68: Week 3: Data tables, select function and pipe

164: Week 13: iteration, for loop to read in data

175: Week 7: Dates and Strings, str_detect function also use week 7 content in line 334

429: week 4: Groups and joins, left_join function

519: Week 11: Functions

543: week 5: data visualization, ggplot

Note: I use content from different weeks repeatedly so I only mentioned the first time it was used.
