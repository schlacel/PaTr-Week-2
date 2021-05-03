####Task 0 - Import your Data ##################################################

## Load the necessary libraries ################################################

library(readr)        # to import tabular data (e.g. csv)
library(dplyr)        # to manipulate (tabular) data
library(ggplot2)      # to visualize data
library(sf)           # to handle spatial vector data
library(terra)        # To handle raster data
library(lubridate)    # To handle dates and times

## Import the downloaded csv ###################################################

wildschwein_BE <- read_delim("wildschwein_BE_2056.csv",",") # adjust path

wildschwein_BE <- st_as_sf(wildschwein_BE, coords = c("E", "N"), crs = 2056, remove = FALSE)

####Task 1 - Getting an overwiew ###############################################

#Calculate the time difference between subsequent rows as described in the demo

wildschwein_BE<- mutate(wildschwein_BE, timelag=as.integer(difftime(lead(DatetimeUTC), DatetimeUTC,units(secs))))

#Now inspect your data in more detail. Try to answer the following questions:

#How many individuals were tracked?
table(wildschwein_BE$TierID)
#three individuals were tracked 

ggplot(data=wildschwein_BE)+
  geom_line(mapping=aes(x=wildschwein_BE$DatetimeUTC, y=wildschwein_BE$TierID))

ggplot(data=wildschwein_BE)+
  geom_histogram(mapping=aes(x=wildschwein_BE$timelag,y=count(wildschwein_BE$TierName)))
?count


View(wildschwein_BE)
#For how long were the individual tracked? Are there gaps?
#Were all individuals tracked concurrently or sequentially?
#What is the temporal sampling interval between the locations?

wildschwein_BE


