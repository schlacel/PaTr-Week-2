####Task 0 - Import your Data ##################################################

## Load the necessary libraries ################################################

library(readr)        # to import tabular data (e.g. csv)
library(dplyr)        # to manipulate (tabular) data
library(ggplot2)      # to visualize data
library(sf)           # to handle spatial vector data
library(terra)        # To handle raster data
library(lubridate)    # To handle dates and times

## Import the downloaded csv ###################################################

wildschwein_BE <- read_delim("wildschwein_BE_2056.csv",",")

wildschwein_BE <- st_as_sf(wildschwein_BE, coords = c("E", "N"), crs = 2056, remove = FALSE)

####Task 1 - Getting an overwiew ###############################################

#Calculate the time difference between subsequent rows as described in the demo

wildschwein_BE<-wildschwein_BE%>%
  group_by(TierID)%>%
  mutate(timelag=as.integer(difftime(lead(DatetimeUTC), DatetimeUTC,units="secs")))

View(wildschwein_BE)

#Now inspect your data in more detail. Try to answer the following questions:

##How many individuals were tracked?
table(wildschwein_BE$TierID)

ggplot(data=wildschwein_BE)+
  geom_line(mapping=aes(x=wildschwein_BE$DatetimeUTC, y=wildschwein_BE$TierID))
#three individuals were tracked 


##For how long were the individual tracked? Are there gaps?
Max_a<-  max(wildschwein_BE$DatetimeUTC)
Max_a
Min_a <- min(wildschwein_BE$DatetimeUTC)
difftime(Max_a,Min_a,units = "days")
#the individuals were tracked for 338.6 days


##Were all individuals tracked concurrently or sequentially?
ggplot(data=wildschwein_BE)+
  geom_line(mapping=aes(x=wildschwein_BE$DatetimeUTC,y=wildschwein_BE$timelag,colour=wildschwein_BE$TierID))
#They were tracked concurrently 



##What is the temporal sampling interval between the locations?

wildschwein_BE%>%
  group_by(TierID)%>%
  summarise(median(timelag,na.rm=TRUE))

ggplot(data=wildschwein_BE)+
  geom_bar(mapping=aes(x=timelag))+
  scale_y_log10()+
  xlim(0,15000)+
  theme_classic()

#the median for all individuals is around 900



####Task 2 - Driving movement parameters I Speed################################

#calculating eclidean distance (steplength)
wildschwein_BE <- wildschwein_BE %>%
  group_by(TierID) %>%
  mutate(euclidean = sqrt((E- lead(E,1))^2 + (N -lead(N,1))^2))

View(wildschwein_BE)

#calculating speed
wildschwein_BE <- wildschwein_BE %>%
  mutate(speed=euclidean/timelag)

View(wildschwein_BE)

#speed unit = m/S


####Task 3 - Cross-scale movement analysis######################################

caro <- read_delim("caro60.csv",",")
caro <- st_as_sf(caro, coords = c("E", "N"), crs = 2056, remove = FALSE)

View(caro)

caro_3 <- caro[seq(1,nrow(caro),by=3),]
caro_6 <- caro[seq(1,nrow(caro),by=6),]
caro_9 <- caro[seq(1,nrow(caro),by=9),]

nrow(caro)
nrow(caro_3)
nrow(caro_6)
nrow(caro_9)

##Timelag
caro<-caro%>%
  mutate(timelag=as.integer(difftime(lead(DatetimeUTC), DatetimeUTC,units="secs")))

caro_3<-caro_3%>%
  mutate(timelag=as.integer(difftime(lead(DatetimeUTC), DatetimeUTC,units="secs")))

caro_6<-caro_6%>%
  mutate(timelag=as.integer(difftime(lead(DatetimeUTC), DatetimeUTC,units="secs")))

caro_9<-caro_9%>%
  mutate(timelag=as.integer(difftime(lead(DatetimeUTC), DatetimeUTC,units="secs")))

View(caro_3)

#calculating eclidean distance (steplength)
caro <- caro %>%
  mutate(euclidean = sqrt((E- lead(E,1))^2 + (N -lead(N,1))^2))

caro_3 <- caro_3 %>%
  mutate(euclidean = sqrt((E- lead(E,1))^2 + (N -lead(N,1))^2))

caro_6<- caro_6 %>%
  mutate(euclidean = sqrt((E- lead(E,1))^2 + (N -lead(N,1))^2))

caro_9 <- caro_9 %>%
  mutate(euclidean = sqrt((E- lead(E,1))^2 + (N -lead(N,1))^2))

#calculating speed
caro <- caro %>%
  mutate(speed=euclidean/timelag)

caro_3 <- caro_3 %>%
  mutate(speed=euclidean/timelag)

caro_6 <- caro_6 %>%
  mutate(speed=euclidean/timelag)

caro_9 <- caro_9 %>%
  mutate(speed=euclidean/timelag)

View(caro_9)

##Line plots

ggplot(data=caro,mapping=aes(x=E, y=N))+
  geom_path(data=caro,mapping=aes(color="1 minute"))+
  geom_path(data = caro_3, mapping=aes(color="3 minutes"))+
  geom_point(data=caro,mapping=aes(color="1 minute"))+
  geom_point(data = caro_3, mapping=aes(color="3 minutes"))+
  theme_light()+
  labs(color="Trajectory", title = "Comparing original- with 3 minutes-resampled data")+
  theme(title=element_text(size=8))

ggplot(data = caro,mapping=aes(x=E, y=N))+
  geom_path(data= caro,mapping=aes(color="1 minute"))+
  geom_path(data = caro_6, mapping=aes(color="6 minutes"))+
  geom_point(data=caro,mapping=aes(color="1 minute"))+
  geom_point(data = caro_6, mapping=aes(color="6 minutes"))+
  theme_light()+
  labs(color="Trajectory", title = "Comparing original- with 6 minutes-resampled data")+
  theme(title=element_text(size=8))

ggplot(data = caro,mapping=aes(x=E, y=N))+
  geom_path(data= caro,mapping=aes(color="1 minute"))+
  geom_path(data = caro_9, mapping=aes(color="9 minutes"))+
  geom_point(data=caro,mapping=aes(color="1 minute"))+
  geom_point(data = caro_9, mapping=aes(color="9 minutes"))+
  theme_light()+
  labs(color="Trajectory", title = "Comparing original- with 9 minutes-resampled data")+
  theme(title=element_text(size=8))

ggplot(data = caro,mapping=aes(x=DatetimeUTC, y=speed))+
  geom_line(data=caro, mapping=aes(colour="1 minute"))+
  geom_line(data=caro_3, mapping=aes(colour="3 minute"))+
  geom_line(data=caro_6, mapping=aes(colour="6 minute"))+
  geom_line(data=caro_9, mapping=aes(colour="9 minute"))+
  theme_light()+
  labs(color="Trajectory", title = "Comparing derived speed at different sampling intervals")+
  theme(title=element_text(size=8))+
  xlab("Time")+
  ylab("Speed (m/s)")

####Task 4 - Deriving movement parameters II: Rolling window functions##########

library(zoo)
?zoo
?rollmean

library(zoo)

library(zoo)

example <- rnorm(10)
example
rollmean(example,k = 3,fill = NA,align = "left")
##  [1]  0.93634335  0.31709038  0.02370048  0.67869801  0.73369105  0.50401344
##  [7] -0.56144365 -0.56902598          NA          NA
rollmean(example,k = 4,fill = NA,align = "left")
##  [1]  0.6775521  0.2045005  0.5848215  0.5255629  0.3446928  0.1459635
##  [7] -0.4102301         NA         NA         NA

#Rolling window
caro_win <- caro
caro_win$k2 <- rollmean(caro_win$speed, k=2, fill = NA, align = "left")
caro_win$k4 <- rollmean(caro_win$speed, k=4, fill = NA, align = "left")
caro_win$k6 <- rollmean(caro_win$speed, k=6, fill = NA, align = "left")
caro_win$k8 <- rollmean(caro_win$speed, k=8, fill = NA, align = "left")
caro_win$k10 <- rollmean(caro_win$speed, k=10, fill = NA, align = "left")
View(caro_win)

#Visualization
ggplot(data = caro_win,mapping=aes(x=DatetimeUTC, y=speed))+
  geom_line(alpha=0.5)+
  geom_line(data=caro_win, mapping=aes(y=k2, color="k2"))+
  geom_line(data=caro_win, mapping=aes(y=k4, color="k4"))+
  geom_line(data=caro_win, mapping=aes(y=k6, color="k6"))+
  geom_line(data=caro_win, mapping=aes(y=k8, color="k8"))+
  geom_line(data=caro_win, mapping=aes(y=k10, color="k10"))+
  theme_light()+
  labs(color="Window size", title = "Comparing derived speed at different window sizes")+
  theme(title=element_text(size=8))+
  xlab("Time")+
  ylab("Speed (m/s)")
