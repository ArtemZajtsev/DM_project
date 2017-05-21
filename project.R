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
