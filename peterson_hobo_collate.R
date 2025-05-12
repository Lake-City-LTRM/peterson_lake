#Load Libraries ----
library(tidyverse)
library(lubridate)


#A. PETERSON HOBO DATA ----

#__1. Read .csv Data ----

#____a) Winter DO/Temperature Hobos----

### PL1, PL2, PL3: 6 winter files each ###

### PL1 ###
PL1_win_1617 <- read_csv('data/2016-17W_9652.csv', skip = 2,  col_select= 1:4, col_names=c("RecNum", "Date.Time", "DO", "TempF"))
PL1_win_1718 <- read_csv('data/2017-18W_9652.csv', skip = 2,  col_select= 1:4, col_names=c("RecNum", "Date.Time", "DO", "TempF"))
PL1_win_1819 <- read_csv('data/2018-19W_9652.csv', skip = 2,  col_select= 1:4, col_names=c("RecNum", "Date.Time", "DO", "TempF"))
PL1_win_1920 <- read_csv('data/2019-20W_9652.csv', skip = 2,  col_select= 1:4, col_names=c("RecNum", "Date.Time", "DO", "TempF"))
PL1_win_2021 <- read_csv('data/2020-21W_9652.csv', skip = 2,  col_select= 1:4, col_names=c("RecNum", "Date.Time", "DO", "TempF"))
PL1_win_2122 <- read_csv('data/2021-22W_9652.csv', skip = 2,  col_select= 1:4, col_names=c("RecNum", "Date.Time", "DO", "TempF"))

### PL2 ###
PL2_win_1617 <- read_csv('data/2016-17W_9653.csv', skip = 2,  col_select= 1:4, col_names=c("RecNum", "Date.Time", "DO", "TempF"))
PL2_win_1718 <- read_csv('data/2017-18W_9653.csv', skip = 2,  col_select= 1:4, col_names=c("RecNum", "Date.Time", "DO", "TempF"))
PL2_win_1819 <- read_csv('data/2018-19W_9653.csv', skip = 2,  col_select= 1:4, col_names=c("RecNum", "Date.Time", "DO", "TempF"))
PL2_win_1920 <- read_csv('data/2019-20W_9653.csv', skip = 2,  col_select= 1:4, col_names=c("RecNum", "Date.Time", "DO", "TempF"))
PL2_win_2021 <- read_csv('data/2020-21W_9653.csv', skip = 2,  col_select= 1:4, col_names=c("RecNum", "Date.Time", "DO", "TempF"))
PL2_win_2122 <- read_csv('data/2021-22W_9653.csv', skip = 2,  col_select= 1:4, col_names=c("RecNum", "Date.Time", "DO", "TempF"))

### PL3 ###
PL3_win_1617 <- read_csv('data/2016-17W_9654.csv', skip = 2,  col_select= 1:4, col_names=c("RecNum", "Date.Time", "DO", "TempF"))
PL3_win_1718 <- read_csv('data/2017-18W_9654.csv', skip = 2,  col_select= 1:4, col_names=c("RecNum", "Date.Time", "DO", "TempF"))
PL3_win_1819 <- read_csv('data/2018-19W_9654.csv', skip = 2,  col_select= 1:4, col_names=c("RecNum", "Date.Time", "DO", "TempF"))
PL3_win_1920 <- read_csv('data/2019-20W_9654.csv', skip = 2,  col_select= 1:4, col_names=c("RecNum", "Date.Time", "DO", "TempF"))
PL3_win_2021 <- read_csv('data/2020-21W_9654.csv', skip = 2,  col_select= 1:4, col_names=c("RecNum", "Date.Time", "DO", "TempF"))
PL3_win_2122 <- read_csv('data/2021-22W_9654.csv', skip = 2,  col_select= 1:4, col_names=c("RecNum", "Date.Time", "DO", "TempF"))

#__2. Format Data ----

#____a) add 'station' column----

### PL1 ###
PL1_win_1617$station <- "PL1"
PL1_win_1718$station <- "PL1"
PL1_win_1819$station <- "PL1"
PL1_win_1920$station <- "PL1"
PL1_win_2021$station <- "PL1"
PL1_win_2122$station <- "PL1"

### PL2 ###
PL2_win_1617$station <- "PL2"
PL2_win_1718$station <- "PL2"
PL2_win_1819$station <- "PL2"
PL2_win_1920$station <- "PL2"
PL2_win_2021$station <- "PL2"
PL2_win_2122$station <- "PL2"

### PL3 ###
PL3_win_1617$station <- "PL3"
PL3_win_1718$station <- "PL3"
PL3_win_1819$station <- "PL3"
PL3_win_1920$station <- "PL3"
PL3_win_2021$station <- "PL3"
PL3_win_2122$station <- "PL3"

#____b) add 'winterID' column----

### PL1 ###
PL1_win_1617$winterID <- 1617
PL1_win_1718$winterID <- 1718
PL1_win_1819$winterID <- 1819
PL1_win_1920$winterID <- 1920
PL1_win_2021$winterID <- 2021
PL1_win_2122$winterID <- 2122

### PL2 ###
PL2_win_1617$winterID <- 1617
PL2_win_1718$winterID <- 1718
PL2_win_1819$winterID <- 1819
PL2_win_1920$winterID <- 1920
PL2_win_2021$winterID <- 2021
PL2_win_2122$winterID <- 2122

### PL3 ###
PL3_win_1617$winterID <- 1617
PL3_win_1718$winterID <- 1718
PL3_win_1819$winterID <- 1819
PL3_win_1920$winterID <- 1920
PL3_win_2021$winterID <- 2021
PL3_win_2122$winterID <- 2122




#__3. combine dataframes----

#____a) one frame for each station----

PL1_hobo_winter_1621 <- rbind(PL1_win_1617, PL1_win_1718, PL1_win_1819, PL1_win_1920, PL1_win_2021, PL1_win_2122)
PL2_hobo_winter_1621 <- rbind(PL2_win_1617, PL2_win_1718, PL2_win_1819, PL2_win_1920, PL2_win_2021, PL2_win_2122)
PL3_hobo_winter_1621 <- rbind(PL3_win_1617, PL3_win_1718, PL3_win_1819, PL3_win_1920, PL3_win_2021, PL3_win_2122)

#____b) all stations combined ----

PL123_hobo_winter_1621 <-rbind(PL1_hobo_winter_1621, PL2_hobo_winter_1621, PL3_hobo_winter_1621)


#__4. Write combined data file to .csv in project folder----

write_csv(PL123_hobo_winter_1621, 'hobo_pl123_winter_2016-21.csv')

#__5. Clean Environment----

### remove PL1 dataframes ###
rm(PL1_win_1617)
rm(PL1_win_1718)
rm(PL1_win_1819)
rm(PL1_win_1920)
rm(PL1_win_2021)
rm(PL1_win_2122)

### remove PL2 dataframes ###
rm(PL2_win_1617)
rm(PL2_win_1718)
rm(PL2_win_1819)
rm(PL2_win_1920)
rm(PL2_win_2021)
rm(PL2_win_2122)

### remove PL3 dataframes ###
rm(PL3_win_1617)
rm(PL3_win_1718)
rm(PL3_win_1819)
rm(PL3_win_1920)
rm(PL3_win_2021)
rm(PL3_win_2122)

### remove individual station combined years dataframes ###
rm(PL1_hobo_winter_1621)
rm(PL2_hobo_winter_1621)
rm(PL3_hobo_winter_1621)
