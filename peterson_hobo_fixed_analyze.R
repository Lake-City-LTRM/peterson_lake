#Load Libraries ----
library(tidyverse)
library(lubridate)

# A. COLLATED HOBO DATA----

PL123_hobo_winter_1621 <- read_csv('hobo_pl123_winter_2016-21.csv')

#__1. ADD COLUMNS----

#____a) add project 'phase' columns----

#______i) 'pre' or 'post' project----
PL123_hobo_winter_1621 <- PL123_hobo_winter_1621 %>%
  mutate(phase = case_when(winterID == 1617 | winterID == 1718 | winterID == 1819 ~ "pre",
                           winterID == 1920 | winterID == 2021 | winterID == 2122 ~ "post"))

#______ii) 'phase_year' column----
PL123_hobo_winter_1621 <- PL123_hobo_winter_1621 %>%
  mutate(phase_year = case_when(winterID == 1617 ~ "3-Pre",  winterID == 1718 ~ "2-Pre",  winterID == 1819 ~ "1-Pre",
                           winterID == 1920 ~ "1-Post", winterID == 2021 ~ "2-Post" , winterID == 2122 ~ "3-Post"))



#____b) add some dates and times ----

### Date ###

### Base R ###
PL123_hobo_winter_1621$date <-as.POSIXct(PL123_hobo_winter_1621$Date.Time, format = "%m/%d/%Y")

### Lubridate package ###
PL123_hobo_winter_1621$ymd <- ymd(PL123_hobo_winter_1621$date)

### Year ###
PL123_hobo_winter_1621 <- PL123_hobo_winter_1621  %>%
  mutate(year = year(ymd) )

### Month ###
PL123_hobo_winter_1621 <- PL123_hobo_winter_1621  %>%
  mutate(month = month(ymd) )

### Day (of month) ###
PL123_hobo_winter_1621 <- PL123_hobo_winter_1621 %>%
  mutate(day = day(ymd))

### Julian Day ###
PL123_hobo_winter_1621 <- PL123_hobo_winter_1621 %>%
  mutate(julian = yday(ymd))

### Hour and Minute ###
PL123_hobo_winter_1621$date_time <-as.POSIXct(PL123_hobo_winter_1621$Date.Time, format = "%m/%d/%Y %H:%M")
PL123_hobo_winter_1621$hr <- hour(PL123_hobo_winter_1621$date_time)
PL123_hobo_winter_1621$min <- minute(PL123_hobo_winter_1621$date_time) 

### Winter Days 1-187 (Nov 1 - April 30) ### 
PL123_hobo_winter_1621$WinDay <- with(PL123_hobo_winter_1621,
                                 ifelse(month == 11, julian - 304,
                                        ifelse(month == 12, julian - 304,
                                               ifelse(month == 1, julian + 61,
                                                      ifelse(month == 2, julian + 61,
                                                             ifelse(month == 3, julian + 61,
                                                                    ifelse(month == 4, julian + 61, julian + 61)))))))


#____c) convert fahrenheit to celsius----

PL123_hobo_winter_1621$TempC <- (PL123_hobo_winter_1621$TempF - 32) / 1.8


#__2. Parse Winter Data----

### filter for Dec-Feb data and remove NA values###
PL123_hobo_DJF <- PL123_hobo_winter_1621 %>% filter (WinDay > 30 & WinDay < 122)

### since there are 22 instances where DO and TempC are NA in winter 2016-17 at PL1,2 3)...###
### remove NA records ###

PL123_hobo_DJF <- PL123_hobo_DJF %>% drop_na()

### PL2 and Pl3 data loggers crashed during winter 2020-21 ###

min(PL123_hobo_DJF$TempC)
# -511.6

### remove records where DO < -1.0 ###
PL123_hobo_DJF <- PL123_hobo_DJF %>% filter (DO > -2.0)

#__3. Plots----

#____a) DO individual years----

### calculate mean daily DO ###
PL123_hobo_do <- PL123_hobo_DJF %>%
  group_by(station, winterID, WinDay) %>%
  summarise(n=n(), meanDO = mean(DO)) 

### Plot ###
plot_do_PL123_hobo <- ggplot(PL123_hobo_do, aes(x=WinDay, y=meanDO, color=station)) +
  geom_line(size = 1) +
  theme_bw() +
  scale_x_continuous(limits = c(31, 122), breaks = seq(31, 122, by = 30),
                     labels=c("Dec", "Jan", "Feb", "Mar"),
                     name = ("Month")) +
  ylim(-1,22) +
  geom_hline(yintercept=1.5, linetype="dashed", color = "blue") +
  geom_hline(yintercept=3.0, linetype="dashed", color = "orange") +
  geom_hline(yintercept=5.0, linetype="dashed", color = "red") +
  labs(y="DO (mg/L)") +
  facet_wrap(~winterID)

plot_do_PL123_hobo + scale_color_manual(values=c("red", "darkgray", "black")) +
  ggtitle("Winter Dissolved Oxygen (Dec-Feb): PL1,PL2, PL3")

#____b) DO pre vs post----

### heat maps ###




#B. PETERSON FIXED SITE DATA----
#__1. Read .csv Data ----

### set working directory ###

### can get rid of this once Git is set up  ##

setwd("c:/eric/R/Git/peterson")

### PL1,PL2, PL3, PL4 ###
PL1234_fixed_data <- read.csv('fixed_site_data.csv')


#__2. Clean data----

### rename 'D.O'. column to 'DO' ###
PL1234_fixed_data <- PL1234_fixed_data %>% rename("DO" = "D.O.")

#__3. Add some columns----

#____a) dates and times----

### Date Using Base R ###
PL1234_fixed_data$date <-as.POSIXct(PL1234_fixed_data$DATE, format = "%m/%d/%Y")

### Date using Lubridate package ###
PL1234_fixed_data$ymd <- ymd(PL1234_fixed_data$date)

### YMD ###
PL1234_fixed_data <- PL1234_fixed_data  %>%
  mutate(ymd = ymd(date) )

### Year ###
PL1234_fixed_data <- PL1234_fixed_data  %>%
  mutate(year = year(date) )

### month ###
PL1234_fixed_data <- PL1234_fixed_data  %>%
  mutate(month = month(date) )

### day ###
PL1234_fixed_data <- PL1234_fixed_data %>%
  mutate(day = day(date))


#__4. Extract Winter Data----
PL1234_fixed_DJF <- PL1234_fixed_data %>%
  filter(month == 12 | month == 1 | month == 2)
  
PL1234_fixed_DJF_hsi <- dplyr:: select(PL1234_fixed_DJF , c('Location', 'Z', 'Zmax', 'Zice', 'Temp', 'D.O.', 
                                                               'Vel', 'date', 'ymd','year', 'month', 'day' ))

#____a) add available water depth 'awd' column----

### first replace Zice 'NA' with 0 ###
dat_peterson_fixed_hsi$Zice[is.na(dat_peterson_fixed_hsi$Zice)] <- 0

### then add 'awd' columns = Zmax - (Zice/100)  ###
dat_peterson_fixed_hsi <- dat_peterson_fixed_hsi %>%
  mutate(awd= Zmax-(Zice/100))




#__5. Get awd and velocity winter data----
### to later add to hobo data ###

#____a) summarise data

### awd by station, year, month ###
pl1234_depth <- dat_peterson_fixed_hsi %>%
  group_by(Location, year, month) %>%
  summarise(meanAWD = mean(awd))

pl1234_depth_winter <- pl1234_depth %>%
  filter(month == 12 | month == 1 | month == 2)

### identify missing awd values
awd_pivot <- pl1234_depth_winter %>%
  mutate(year_month = paste(year, month, sep = "_")) %>%
  pivot_wider(names_from = year_month, values_from = meanAWD)

### missing december 2017, 2018, 2020, 2021...use jan of next year ###
#dec2017 = jan2018: PL1 = 1.86; PL2 = 1.83; PL3 = 
#dec2018 = jan2019 = 
#dec2020 = jan2021 = 
#dec2021 = jan2022 = 


### add and populate 'station' column ###


dat_peterson_fixed_hsi <- dat_peterson_fixed_hsi %>%
  mutate(Vtemp = case_when(Temp <= 1.0 ~ Temp * 0.5, 
                           Temp >= 1.0 & Temp <= 3.9 ~ 0.1667 * Temp + 0.3333, 
                           Temp >= 4.0 ~ 1.0 ))

### vel ###
dat_fixed_vel <- dat_peterson_fixed_hsi %>%
  filter(Vel != "NA")

pl1234_vel <- dat_fixed_vel %>%
  group_by(Location, year, month) %>%
  summarise(meanVEL = mean(Vel))

pl1234_vel_winter <- pl1234_vel %>%
  filter(month == 12 | month == 1 | month == 2)



#____b) combine depth and velocity data----

pl1234_awd_vel_winter <- left_join(pl1234_depth_winter , pl1234_vel_winter, 
                                   by=c("Location", "year", "month"))

### add and populate 'station' column based on 'Location' ###
pl1234_awd_vel_winter <- pl1234_awd_vel_winter %>%
  mutate(station = case_when(Location == "PL01.0P" ~ "PL1", 
                             Location == "PL02.0P" ~ "PL2",
                             Location == "PL03.0P" ~ "PL3",
                             Location == "PL04.0P" ~ "PL4"))




#__4. Calculate HSI scores----

### calculate available water depth in new column ###
dat_peterson_fixed_hsi <- dat_peterson_fixed_hsi %>% 
  mutate (awd = (Zmax - (Zice / 100)))

### calculate individual vector scores ###

### awd vector score ###
dat_peterson_fixed_hsi <- dat_peterson_fixed_hsi %>% 
  mutate (Vawd = (awd * 0.5))

### velocity vector score ###
dat_peterson_fixed_hsi$Vvel[dat_peterson_fixed_hsi$Vel == 0] <- 1
dat_peterson_fixed_hsi$Vvel[dat_peterson_fixed_hsi$Vel == 0.01] <- 0.7
dat_peterson_fixed_hsi$Vvel[dat_peterson_fixed_hsi$Vel == 0.02] <- 0.4
dat_peterson_fixed_hsi$Vvel[dat_peterson_fixed_hsi$Vel >= 0.03] <- 0.1

### DO vector score ###
dat_peterson_fixed_hsi$Vdo[dat_peterson_fixed_hsi$DO <= 1.5 ] <- 0.1
dat_peterson_fixed_hsi$Vdo[dat_peterson_fixed_hsi$DO >= 1.6 & dat_peterson_fixed_hsi$DO <= 3.0] <- 0.4
dat_peterson_fixed_hsi$Vdo[dat_peterson_fixed_hsi$DO >- 3.1 & dat_peterson_fixed_hsi$DO <= 4.9] <- 0.7
dat_peterson_fixed_hsi$Vdo[dat_peterson_fixed_hsi$DO >= 5.0 ] <- 1.0

### Temperature vector score ###
dat_peterson_fixed_hsi <- dat_peterson_fixed_hsi %>%
  mutate(Vtemp = case_when(Temp <= 1.0 ~ Temp * 0.5, 
                           Temp >= 1.0 & Temp <= 3.9 ~ 0.1667 * Temp + 0.3333, 
                           Temp >= 4.0 ~ 1.0 ))

### cap all Vawd scores at 1.0 ###
dat_peterson_fixed_hsi$Vawd[dat_peterson_fixed_hsi$Vawd > 1.0 ] <- 1.0



### HSI ###

# HSI = ((Vawd * ( (2*Vdo) + Vtemp) / 3)^2 * Vvel)^0.25 

dat_peterson_fixed_hsi <- dat_peterson_fixed_hsi %>%
  mutate(Vwq = ( (2 * Vdo) + Vtemp) / 3) %>%
  mutate(HSI = (Vwq^2 * Vvel * Vawd)^0.25)

### Alt Vwq formula that uses low DO and Temp criteria

dat_peterson_fixed_hsi <- dat_peterson_fixed_hsi %>%
  mutate(Vwq2 = ifelse(Vdo < 0.41 | Vtemp < 0.41, pmin(Vdo, Vtemp), ( (2 * Vdo) + Vtemp) / 3)) %>%
  mutate(HSI2 = (Vwq2^2 * Vvel * Vawd)^0.25)




#__5. Parse winter HSI scores----

#____a) exclude NA----
### exclude records where HSI is 'NA' ###
dat_peterson_fixed_hsi_rmNA <- subset(dat_peterson_fixed_hsi, !is.na(HSI))

#____b) subset winter records----
### subset for months 12, 1, 2
dat_peterson_fixed_hsi_djf <- subset(dat_peterson_fixed_hsi_rmNA, month == 12 | month == 1 | month == 2 )

#____c) add winter Id "wint' column----

### e.g. Dec 2016 - Feb = 2016 then 'wint' = 2016) ###
dat_peterson_fixed_hsi_djf <- dat_peterson_fixed_hsi_djf %>%
  mutate(wint = ifelse(month == 12 , year, 
                       ifelse(month != 12, year-1, 0)))

#____d) extract bottom values ----
dat_fixed_hs1_djf_bottom <- dat_peterson_fixed_hsi_djf %>%
  filter(Z > 0.21)

### count records by winter and location ###

### surface and bottom ###
peterson_fixed_winter_data <- dat_peterson_fixed_hsi_djf %>%
  group_by(Location, wint) %>%
  summarise(n=n())

### bottom only ###
peterson_fixed_winter_data_bottom <- dat_fixed_hs1_djf_bottom %>%
  group_by(Location, wint) %>%
  summarise(n=n())


#____e) plots----

### compare HSI and HSI2, particularly 2016-17 winter

### all Locations

scatter_fixed_winter_hsi <- ggplot(dat_fixed_hs1_djf_bottom, aes(x=wint, y=HSI)) +
  geom_point() +
  geom_smooth(method="loess") +
  facet_wrap(~Location)

scatter_fixed_winter_hsi + ggtitle("HSI scores without the minimum DO/Temp cutoff")

scatter_fixed_winter_hsi2 <- ggplot(dat_fixed_hs1_djf_bottom, aes(x=wint, y=HSI2)) +
  geom_point() +
  geom_smooth(method="loess") +
  facet_wrap(~Location)

scatter_fixed_winter_hsi + ggtitle("HSI scores WITH the proper minimum DO/Temp cutoff")



### PL3

dat_PL3_hsi <- dat_peterson_fixed_hsi %>% filter(Location == 'PL03.0P')

dat_PL3_hsi_rmna <- subset(dat_PL3_hsi, !is.na(HSI))

scatter_PL3 <- ggplot(dat_PL3_hsi_rmna, aes(x=date, y=HSI)) +
  geom_point()

### winter HSI scores ###
scatter_hsi_station <- ggplot(dat_peterson_fixed_hsi_djf, aes(year, y=HSI)) +
  geom_point() +
  geom_smooth(method="loess") +
  facet_wrap(~Location)

scatter_hsi_station + ggtitle("HSI scores at Peterson Fixed Sites by Winter (Dec-Feb)")

# C. COMBINE HOBO WITH FIXED AWD, VEL----

PL123_hobo_awd_vel <- left_join(PL123_DJF_rmna, pl1234_awd_vel_winter, 
                                by = c("station", "year", "month"))

#__1. PL123 Hobo HSI calculations----

### awd vector score ###
PL123_hobo_awd_vel <- PL123_hobo_awd_vel %>% 
  mutate (Vawd = (meanAWD * 0.5))

### velocity vector score ###

### round velocity to nearest 0.01
PL123_hobo_awd_vel <- PL123_hobo_awd_vel %>%
  mutate(meanVEL = round(meanVEL, 2))

PL123_hobo_awd_vel$Vvel[PL123_hobo_awd_vel$meanVEL == 0] <- 1
PL123_hobo_awd_vel$Vvel[PL123_hobo_awd_vel$meanVEL == 0.01] <- 0.7
PL123_hobo_awd_vel$Vvel[PL123_hobo_awd_vel$meanVEL == 0.02] <- 0.4
PL123_hobo_awd_vel$Vvel[PL123_hobo_awd_vel$meanVEL >= 0.03] <- 0.1

### DO vector score ###
PL123_hobo_awd_vel$Vdo[PL123_hobo_awd_vel$DO <= 1.5 ] <- 0.1
PL123_hobo_awd_vel$Vdo[PL123_hobo_awd_vel$DO >= 1.6 & PL123_hobo_awd_vel$DO <= 3.0] <- 0.4
PL123_hobo_awd_vel$Vdo[PL123_hobo_awd_vel$DO >- 3.1 & PL123_hobo_awd_vel$DO <= 4.9] <- 0.7
PL123_hobo_awd_vel$Vdo[PL123_hobo_awd_vel$DO >= 5.0 ] <- 1.0

### Temperature vector score ###
PL123_hobo_awd_vel <- PL123_hobo_awd_vel %>%
  mutate(Vtemp = case_when(TempC <= 1.0 ~ TempC * 0.5, 
                           TempC >= 1.0 & TempC <= 3.9 ~ 0.1667 * TempC + 0.3333, 
                           TempC >= 4.0 ~ 1.0 ))

### cap all Vawd scores at 1.0 ###
PL123_hobo_awd_vel$Vawd[PL123_hobo_awd_vel$Vawd > 1.0 ] <- 1.0



### HSI ###

# HSI = ((Vawd * ( (2*Vdo) + Vtemp) / 3)^2 * Vvel)^0.25 

PL123_hobo_awd_vel <- PL123_hobo_awd_vel %>%
  mutate(Vwq = ( (2 * Vdo) + Vtemp) / 3) %>%
  mutate(HSI = (Vwq^2 * Vvel * Vawd)^0.25)

### Alt Vwq formula that uses low DO and Temp criteria

PL123_hobo_awd_vel <- PL123_hobo_awd_vel %>%
  mutate(Vwq2 = ifelse(Vdo < 0.41 | Vtemp < 0.41, pmin(Vdo, Vtemp), ( (2 * Vdo) + Vtemp) / 3)) %>%
  mutate(HSI2 = (Vwq2^2 * Vvel * Vawd)^0.25)



dat_peterson_fixed_awd_summary <- dat_peterson_fixed_hsi_djf %>%
  group_by(Location, winterID, month) %>%
  summarise(min = min(awd), max = max(awd))

#__2. Plots----

###



## add concurrent discharge 
## plot vel ~ Q by location 

