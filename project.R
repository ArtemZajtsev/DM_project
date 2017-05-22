#Download and cleaning data
data <- read.csv("./flights.csv")

cleanedData <- data[data$CANCELLED != 1,]
cleanedData <- data[data$DIVERTED != 1,]

uniqueAirports <- unique(unlist(list(data$ORIGIN_AIRPORT, data$DESTINATION_AIRPORT)))

airportFlightsCounter <- function(airports) {
  busyAirports <- c()
  index <- 0
  for(airport in airports) {
    amount_of_flights <- (nrow(cleanedData[cleanedData$ORIGIN_AIRPORT == airport,])+nrow(cleanedData[cleanedData$DESTINATION_AIRPORT == airport,]))/365
    if(amount_of_flights > 20) {
      busyAirports <- c(busyAirports, airport)
    }    
  }  
  return(busyAirports)
}

airportWithMoreThen20Flights <- airportFlightsCounter(uniqueAirports)

library(dplyr)

cleanedData <- filter(cleanedData, cleanedData$ORIGIN_AIRPORT %in% airportWithMoreThen20Flights | cleanedData$DESTINATION_AIRPORT %in% airportWithMoreThen20Flights)

write.csv(cleanedData,file='cleanedData.csv',quote = FALSE,row.names = FALSE)

# Plots 
library(ggplot2)

#plot 1a. Departure Delay Histrogram density ~ min 

ggplot(data=cleanedData, aes(x= cleanedData$DEPARTURE_DELAY, y = ..density..)) + 
    geom_histogram(
                   binwidth = 3,
                   col="blue", 
                   fill="darkblue", 
                   alpha = .7) + 
    labs(title="2015 Departure Delay Distribution") +
    labs(x="Min", y="Density") +
    xlim(c(-20,130)) +
    ylim(c(0,0.08))+
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))

# plot 1b. Arrival Delay Histrogram density ~ min 

ggplot(data=cleanedData, aes(x= cleanedData$ARRIVAL_DELAY, y = ..density..)) + 
  geom_histogram(
    binwidth = 3,
    col="goldenrod", 
    fill="goldenrod1", 
    alpha = .7) + 
  labs(title="2015 Arrival Delay Distribution") +
  labs(x="Min", y="Density") +
  xlim(c(-20,130)) +
  ylim(c(0,0.04))+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))


#Plot 2a. Avg delay - ORIGIN airports (4 big airports)

origin_airoports = subset(cleanedData, cleanedData$ORIGIN_AIRPORT %in% c("SAN", "JFK", "MIA", "ATL")) 
delay_origin_airports = origin_airoports[,c("ORIGIN_AIRPORT", "MONTH", "DEPARTURE_DELAY")]

aggregated_airports = aggregate(DEPARTURE_DELAY ~ MONTH + ORIGIN_AIRPORT, delay_origin_airports, mean)

ggplot(aggregated_airports, aes(MONTH , DEPARTURE_DELAY, colour= ORIGIN_AIRPORT)) +
 geom_line(size=2) +
 theme_bw() +
 ggtitle("Average delay in origin airports")+
labs(x="Month", y="Departure delay (min)") +
  scale_x_continuous(breaks=seq(1.0,12.0,by=1),labels=month.abb)

#Plot 2b. Avg delay - DESTINATION airports (4 big airports)

destination_airoports = subset(cleanedData, cleanedData$DESTINATION_AIRPORT %in% c("SAN", "JFK", "MIA", "ATL")) 
delay_destination_airports = destination_airoports[,c("DESTINATION_AIRPORT", "MONTH", "ARRIVAL_DELAY")]

aggregated_airports = aggregate(ARRIVAL_DELAY ~  DESTINATION_AIRPORT + MONTH, delay_destination_airports, mean)
aggregated_airports = aggregate(ARRIVAL_DELAY ~  DESTINATION_AIRPORT + MONTH, delay_destination_airports, mean)

ggplot(aggregated_airports, aes(MONTH , ARRIVAL_DELAY, colour= DESTINATION_AIRPORT)) +
  geom_line(size=2) +
  theme_bw() +
  ggtitle("Average delay in destination airports")+
  labs( y="Arrival delay (min)")+
  scale_x_continuous(breaks=seq(1.0,12.0,by=1),labels=month.abb) 

#Plot 3a.  Month - delay ORIGIN

aggregated_by_month = aggregate(DEPARTURE_DELAY ~ MONTH, cleanedData, mean)

plot3a = ggplot(aggregated_by_month, aes(as.factor(MONTH) , DEPARTURE_DELAY)) +
  geom_col(size=2,fill = "blue") +
  theme_bw() +
  ylim(c(-1,15))+
  ggtitle("Average delay in origin airports")+
  labs(x="Month", y="Departure delay (min)") +
  scale_x_discrete(labels=month.abb)



#Plot 3b.  Month - delay DESTINATION

aggregated_by_month = aggregate(ARRIVAL_DELAY ~ MONTH, cleanedData, mean)
month_num = as.numeric(cleanedData$MONTH)
plot3b = ggplot(aggregated_by_month, aes(as.factor(MONTH) , ARRIVAL_DELAY)) +
  geom_col(size=2,fill = "blue") +
  theme_bw() +
  ylim(c(-1,15))+
  ggtitle("Average delay in destination airports")+
  scale_x_discrete(labels=month.abb)+
  labs(x="Month", y="Arrival delay (min)") 

library(gridExtra)
grid.arrange(plot3a,plot3b,ncol=2)

#Plot 4a.  WEEkday - delay ORIGIN

aggregated_by_WEEkday = aggregate(DEPARTURE_DELAY ~ DAY_OF_WEEK, cleanedData, mean)

plot4a = ggplot(aggregated_by_WEEkday, aes(as.factor(DAY_OF_WEEK), DEPARTURE_DELAY)) +
  geom_col(size=2,fill = "green") +
  theme_bw() +
  ylim(c(-1,15))+
  ggtitle("Average delay in origin airports divided by weekday")+
  labs(x="Day of week", y="Departure delay (min)") +
  scale_x_discrete(breaks=1:7,labels=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))

#Plot 4b.  WEEkday - delay DESTINATION

aggregated_by_WEEkday = aggregate(ARRIVAL_DELAY ~ DAY_OF_WEEK, cleanedData, mean)

plot4b = ggplot(aggregated_by_WEEkday, aes(as.factor(DAY_OF_WEEK), ARRIVAL_DELAY)) +
  geom_col(size=2,fill = "green") +
  theme_bw() +
  ylim(c(-1,15))+
  ggtitle("Average delay in destination airports divided by weekday")+
  labs(x="Day of week", y="Departure delay (min)") +
  scale_x_discrete(breaks=1:7,labels=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))

grid.arrange(plot4a,plot4b,ncol=2)
