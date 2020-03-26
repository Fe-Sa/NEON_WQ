# NEON Water Quality Project
# Felipe Sanchez


library(neonUtilities)
library(tidyverse)
library(anytime)
library(lubridate)
library(zoo)
library(usethis)

BARC.WQ <- loadByProduct(dpID = "DP1.20288.001",
                         site = "BARC",
                         startdate = "2018-01",
                         enddate = "2018-12")


view(BARC.WQ$waq_instantaneous)
list2env(BARC.WQ,.GlobalEnv)

str(waq_instantaneous)

# Convert date from factor to POSIXct 
waq_instantaneous$startDateTime <- as.POSIXct(waq_instantaneous$startDateTime,
                                              format = "%Y-%m-%dT%H:%M",
                                              tz = "GMT")
# Ask about this tomorrow                                            
head(waq_instantaneous$startDateTime)
waq_instantaneous$startDateTime <- format(waq_instantaneous$startDateTime, tz = "America/New_York")
head(waq_instantaneous$startDateTime)

# Ask about the depth column

# Remove unwanted columns
barcWQ <- BARC.WQ$waq_instantaneous[-c(1,2,3,4,6,7,8,9,11,12,14,15,17,18,20,21,23,24,26,27,29,30,31,32)]
names(barcWQ)


# Create a year column
barcWQ$year <- year(barcWQ$startDateTime)
names(barcWQ)

## Create a month column
barcWQ$month <- month(barcWQ$startDateTime)

## Create a weekly column
barcWQ$week <- week(barcWQ$startDateTime)

# Create a julian day column
barcWQ$julianD <- yday(barcWQ$startDateTime)

# daily Ave.
barcWQ.da<- barcWQ %>%
  group_by(year, julianD) %>%
  summarise_all(funs(mean),na.rm=T)

barcWQ.da[,4:10] <- round(barcWQ.da[,4:10],digits = 1)

# Linearly Interpolate missing values with zoo package
barcWQ.da1<-na.approx(barcWQ.da)
barcWQ.da1 <- as.data.frame(barcWQ.da1)

# daily Max
barcWQ <- barcWQ[-1] # remove StartdateTime to run max

barcWQ.dmax <- barcWQ %>%
  group_by(year, julianD) %>%
  summarise_all(funs(max),na.rm=T)

barcWQ.dmax[mapply(is.infinite,barcWQ.dmax)] <- NA

# daily Min
barcWQ.dmin <- barcWQ %>%
  group_by(year, julianD) %>%
  summarise_all(funs(min),na.rm=T)

barcWQ.dmin[mapply(is.infinite,barcWQ.dmin)] <- NA

# monthly Ave.
barcWQ.mA<- barcWQ.da %>%
  group_by(year, month) %>%
  summarise_all(funs(mean), na.rm = TRUE)

barcWQ.mA <- barcWQ.mA[-c(3,4,12)]
barcWQ.mA[,3:9] <- round(barcWQ.mA[,3:9],digits = 1)

str(barcWQ.mA)

# Plots of WQ - meh, needs a lot of work

barcWQ.da$month <- as.factor(barcWQ.da$month) 

barcWQ.da1$month <- as.character(barcWQ.da1$month)

ggplot(data = barcWQ.da) +
  geom_smooth(mapping= aes(x = barcWQ.da$julianD, y = barcWQ.da$specificConductance)) +
  facet_grid(~barcWQ.da$month)


              