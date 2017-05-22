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

#Departure Delay Histrogram density ~ min 

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

#Arrival Delay Histrogram density ~ min 

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

