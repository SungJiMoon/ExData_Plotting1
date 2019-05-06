#Exploratory data analysis week 1 project
setwd("F:\\R\\Exploratory data analysis\\week1")
library(zip)
library(data.table) ; library(dplyr)

#download the dataset
fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
download.file(fileUrl, destfile="week1.zip")


#unzip the dataset
unzip("week1.zip")


#load the dataset to R
data <- fread("household_power_consumption.txt")




#converst date
data_1 <- na.omit(data)


Date_Time <- with(data_1, strptime(paste(Date,Time),"%d/%m/%Y %H:%M:%S") )


#date to week
week <- Date_Time$wday
data_1$week_transformed <- week


#date to numeric
Date <- as.Date(Date_Time)

Date <- unclass(Date)

data_1$Date_transformed <- Date


#restrict to the period
data_2 <- dplyr::filter(data_1, Date_transformed >= unclass(as.Date("2007-02-01")) & Date_transformed <= unclass(as.Date("2007-02-02"))) %>%
    dplyr::mutate(Global_active_power = as.numeric(Global_active_power),
                  Sub_metering_1 = as.numeric(Sub_metering_1),
                  Sub_metering_2 = as.numeric(Sub_metering_2),
                  Sub_metering_3 = as.numeric(Sub_metering_3),
                  Voltage = as.numeric(Voltage)) %>%
    #mutate week information
    dplyr::mutate(week = ifelse(week_transformed == 0,"Sun",
                                ifelse(week_transformed== 1, "Mon",
                                       ifelse(week_transformed==2, "Tue",
                                              ifelse(week_transformed==3, "Wed",
                                                     ifelse(week_transformed==4, "Thu",
                                                            ifelse(week_transformed==5,"Fri",
                                                                   ifelse(week_transformed==6,"Sat",NA)))))))) 


#calculate time period to plot time series data
Date_Time_1 <- with(data_2, strptime(paste(Date,Time),"%d/%m/%Y %H:%M:%S") )

time_gap <- difftime(Date_Time_1,Date_Time_1[1], units = c("mins"))

data_2$time_gap <-time_gap

#for x.aixs tip mark
Fri <- strptime(paste("2/2/2007","00:00:00"),"%d/%m/%Y %H:%M:%S")
Fri <- difftime(Fri,Date_Time_1[1] , units=c("mins"))

Sat <- strptime(paste("3/2/2007","00:00:00"),"%d/%m/%Y %H:%M:%S")                 
Sat <- difftime(Sat,Date_Time_1[1] , units=c("mins"))




#plot3

png(file = "plot3.png", 
    width = 480, 
    height = 480)
plot(data_2$time_gap,data_2$Sub_metering_1
     ,type="l"
     ,ylab ="Energy sub metering"
     ,xlab=""
     ,xaxt="n" #suppress axis representation
     
)

lines(data_2$time_gap, data_2$Sub_metering_2
      ,type="l"
      ,ylab ="Energy sub metering"
      ,xlab=""
      ,xaxt="n" #suppress axis representation
      ,col="red"
)

lines(data_2$time_gap, data_2$Sub_metering_3
      ,type="l"
      ,ylab ="Energy sub metering"
      ,xlab=""
      ,xaxt="n" #suppress axis representation
      ,col="blue"
)

axis(1, at=c(0,Fri,Sat), labels=c("Thu","Fri","Sat"))
legend("topright"
       , legend=c("Sun_metering_1", "Sun_metering_2", "Sun_metering_3" )
       , lty = 1
       , col = c("black","blue","red"))


dev.off()

colnames(data_2)
