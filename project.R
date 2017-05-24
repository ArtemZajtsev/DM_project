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
  scale_x_continuous(breaks=seq(1.0,12.0,by=1),labels=month.abb)+
  theme(plot.title = element_text(hjust = 0.5))

#Plot 2b. Avg delay - DESTINATION airports (4 big airports)

destination_airoports = subset(cleanedData, cleanedData$DESTINATION_AIRPORT %in% c("SAN", "JFK", "MIA", "ATL")) 
delay_destination_airports = destination_airoports[,c("DESTINATION_AIRPORT", "MONTH", "ARRIVAL_DELAY")]

aggregated_airports = aggregate(ARRIVAL_DELAY ~  DESTINATION_AIRPORT + MONTH, delay_destination_airports, mean)
aggregated_airports = aggregate(ARRIVAL_DELAY ~  DESTINATION_AIRPORT + MONTH, delay_destination_airports, mean)

ggplot(aggregated_airports, aes(MONTH , ARRIVAL_DELAY, colour= DESTINATION_AIRPORT)) +
  geom_line(size=2) +
  theme_bw() +
  ggtitle("Average delay in destination airports")+
  labs(x="Month", y="Arrival delay (min)")+
  scale_x_continuous(breaks=seq(1.0,12.0,by=1),labels=month.abb) 

#Plot 3a.  Month - delay ORIGIN

aggregated_by_month = aggregate(DEPARTURE_DELAY ~ MONTH, cleanedData, mean)

plot3a = ggplot(aggregated_by_month, aes(as.factor(MONTH) , DEPARTURE_DELAY)) +
  geom_col(size=1, col="blue",fill = " dark blue", alpha = .7) +
  theme_bw() +
  ylim(c(-1,15))+
  ggtitle("Average departure delay by month")+
  labs(x="Month", y="Departure delay (min)") +
  scale_x_discrete(labels=month.abb)+
  theme(plot.title = element_text(hjust = 0.5))



#Plot 3b.  Month - delay DESTINATION

aggregated_by_month = aggregate(ARRIVAL_DELAY ~ MONTH, cleanedData, mean)
month_num = as.numeric(cleanedData$MONTH)
plot3b = ggplot(aggregated_by_month, aes(as.factor(MONTH) , ARRIVAL_DELAY)) +
  geom_col(size=1, col="goldenrod",fill = " goldenrod1", alpha = .7) +
  theme_bw() +
  ylim(c(-1,15))+
  ggtitle("Average arival delay by month")+
  scale_x_discrete(labels=month.abb)+
  labs(x="Month", y="Arrival delay (min)")+
  theme(plot.title = element_text(hjust = 0.5))

library(gridExtra)
grid.arrange(plot3a,plot3b,ncol=2)

#Plot 4a.  WEEkday - delay ORIGIN

aggregated_by_WEEkday = aggregate(DEPARTURE_DELAY ~ DAY_OF_WEEK, cleanedData, mean)

plot4a = ggplot(aggregated_by_WEEkday, aes(as.factor(DAY_OF_WEEK), DEPARTURE_DELAY)) +
  geom_col(size=1, col="blue",fill = " dark blue", alpha = .7) +
  theme_bw() +
  ylim(c(-1,15))+
  ggtitle("Average departure delay by weekday")+
  labs(x="Day of week", y="Departure delay (min)") +
  scale_x_discrete(breaks=1:7,labels=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))+
  theme(plot.title = element_text(hjust = 0.5))

#Plot 4b.  WEEkday - delay DESTINATION

aggregated_by_WEEkday = aggregate(ARRIVAL_DELAY ~ DAY_OF_WEEK, cleanedData, mean)

plot4b = ggplot(aggregated_by_WEEkday, aes(as.factor(DAY_OF_WEEK), ARRIVAL_DELAY)) +
  geom_col(size=1, col="goldenrod",fill = " goldenrod1", alpha = .7) +
  theme_bw() +
  ylim(c(-1,15))+
  ggtitle("Average arrival delay by weekday")+
  labs(x="Day of week", y="Departure delay (min)") +
  scale_x_discrete(breaks=1:7,labels=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))+
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(plot4a,plot4b,ncol=2)

#clean date time and convert it to the right format
parse_time = function(x) {
  fixed_time = ifelse(nchar(x)[1]==3,paste0("0",x),x)
  t2 <- format(strptime(fixed_time, format = "%H"), "%H")
  return(t2)
}

aggregateByHour <- aggregate(DEPARTURE_DELAY ~ SCHEDULED_DEPARTURE, cleanedData, mean)

aggregateByHour$SCHEDULED_DEPARTURE <- sapply(aggregateByHour$SCHEDULED_DEPARTURE, parse_time)

aggregateByHour$SCHEDULED_DEPARTURE[is.na(aggregateByHour$SCHEDULED_DEPARTURE)] <-'00'

aggregateByHour <- aggregate(DEPARTURE_DELAY ~ SCHEDULED_DEPARTURE, aggregateByHour, mean)

plot5a = ggplot(aggregateByHour, aes(as.factor(SCHEDULED_DEPARTURE), DEPARTURE_DELAY)) +
  geom_col(size=1,fill = " dark blue", alpha = .7) +
  theme_bw() +
  ylim(c(0,18))+
  ggtitle("Average departure delay by hour")+
  labs(x="Hour", y="Departure delay (min)") +
  theme(plot.title = element_text(hjust = 0.5))


aggregateByHourARRIVAL <- aggregate(ARRIVAL_DELAY ~ SCHEDULED_ARRIVAL, cleanedData, mean)

aggregateByHourARRIVAL$SCHEDULED_ARRIVAL <- sapply(aggregateByHourARRIVAL$SCHEDULED_ARRIVAL, parse_time)

aggregateByHourARRIVAL$SCHEDULED_ARRIVAL[is.na(aggregateByHourARRIVAL$SCHEDULED_ARRIVAL)] <-'00'

aggregateByHourARRIVAL <- aggregate(ARRIVAL_DELAY ~ SCHEDULED_ARRIVAL, aggregateByHourARRIVAL, mean)


plot5b = ggplot(aggregateByHourARRIVAL, aes(as.factor(SCHEDULED_ARRIVAL), ARRIVAL_DELAY)) +
  geom_col(size=1,fill = "goldenrod1", alpha = .7) +
  theme_bw() +
  ylim(c(-1.25,10))+
  ggtitle("Average arrival delay by hour")+
  labs(x="Hour", y="Arrival delay (min)") +
  theme(plot.title = element_text(hjust = 0.5))

# Airlines

aggregateByAirlinesArrival <- aggregate(ARRIVAL_DELAY ~ AIRLINE, cleanedData, mean)

plot6a = ggplot(aggregateByAirlinesArrival, aes(as.factor(AIRLINE), ARRIVAL_DELAY)) +
  geom_col(size=1, fill="dark blue", alpha = .7) +
  theme_bw() +
  ylim(c(-1.5,15))+
  ggtitle("Average arrival delay by airline")+
  labs(x="Airline", y="Arrival delay (min)") +
  theme(plot.title = element_text(hjust = 0.5))

aggregateByAirlinesDeparture <- aggregate(DEPARTURE_DELAY ~ AIRLINE, cleanedData, mean)

plot6b = ggplot(aggregateByAirlinesDeparture, aes(as.factor(AIRLINE), DEPARTURE_DELAY)) +
  geom_col(size=1, fill="goldenrod1", alpha = .7) +
  theme_bw() +
  ylim(c(-1.5,16))+
  ggtitle("Average Departure delay by airline")+
  labs(x="Airline", y="Deaprture delay (min)") +
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(plot6a,plot6b,ncol=2)

# Classification
library(randomForest)
library(gbm)
library(dplyr)
library(readr)
cleanedData <- read_csv("D:/study/DM/DM_project/cleanedData.csv")
cleanedData = cleanedData[!is.na(cleanedData$ARRIVAL_DELAY), ]
isDelayed = function(t)
{
  ifelse(t > 15, 1, 0)
}
cleanedData = mutate(cleanedData, isDelayed  = isDelayed(cleanedData$ARRIVAL_DELAY))
jfk_dataset = filter(cleanedData, cleanedData$ORIGIN_AIRPORT %in% c("JFK"))
mia_dataset = filter(cleanedData, cleanedData$ORIGIN_AIRPORT %in% c("MIA"))
san_dataset = filter(cleanedData, cleanedData$ORIGIN_AIRPORT %in% c("SAN"))
atl_dataset = filter(cleanedData, cleanedData$ORIGIN_AIRPORT %in% c("ATL"))
rw_nmb_jfk = nrow(jfk_dataset)
rw_nmb_mia = nrow(mia_dataset)
rw_nmb_san = nrow(san_dataset)
rw_nmb_atl = nrow(atl_dataset)

jfk_train_data = jfk_dataset[1:(rw_nmb_jfk * 0.8),]
jfk_test_data = jfk_dataset [(rw_nmb_jfk * 0.8):rw_nmb_jfk,]

mia_train_data = mia_dataset[1:(rw_nmb_mia * 0.8),]
mia_test_data = mia_dataset [(rw_nmb_mia * 0.8):rw_nmb_mia,]

san_train_data = san_dataset[1:(rw_nmb_san * 0.8),]
san_test_data = san_dataset [(rw_nmb_san * 0.8):rw_nmb_san,]

atl_train_data = atl_dataset[1:(rw_nmb_atl * 0.8),]
atl_test_data = atl_dataset [(rw_nmb_atl * 0.8):rw_nmb_atl,]

library(data.table)
library(dplyr)
library(party)
library(readr)
library(rpart)
model_jfk <-rpart(as.factor(isDelayed)~
                   MONTH + 
                   DAY +
                   DAY_OF_WEEK +
                   AIRLINE +
                   ORIGIN_AIRPORT +
                   DEPARTURE_TIME +
                   AIR_TIME +
                   DESTINATION_AIRPORT +
                   SCHEDULED_DEPARTURE +
                   DISTANCE + 
                   SCHEDULED_ARRIVAL
                   ,data = jfk_train_data,control = rpart.control(misplit = 60,cp = 0))

model_mia <-rpart(as.factor(isDelayed)~
                    MONTH + 
                    DAY +
                    DAY_OF_WEEK +
                    AIRLINE +
                    ORIGIN_AIRPORT +
                    DEPARTURE_TIME +
                    AIR_TIME +
                    SCHEDULED_DEPARTURE +
                    DISTANCE + 
                    SCHEDULED_ARRIVAL
                  ,data = mia_train_data,control = rpart.control(misplit = 60,cp = 0))
model_san <-rpart(as.factor(isDelayed)~
                    MONTH + 
                    DAY +
                    DAY_OF_WEEK +
                    AIRLINE +
                    ORIGIN_AIRPORT +
                    DEPARTURE_TIME +
                    AIR_TIME +
                    SCHEDULED_DEPARTURE +
                    DISTANCE + 
                    SCHEDULED_ARRIVAL
                  ,data = san_train_data,control = rpart.control(misplit = 60,cp = 0))

model_atl <-rpart(as.factor(isDelayed)~
                    MONTH + 
                    DAY +
                    DAY_OF_WEEK +
                    AIRLINE +
                    ORIGIN_AIRPORT +
                    DEPARTURE_TIME +
                    AIR_TIME +
                    SCHEDULED_DEPARTURE +
                    DISTANCE + 
                    SCHEDULED_ARRIVAL
                  ,data = atl_train_data,control = rpart.control(misplit = 60,cp = 0))

predict_atl = predict(model_atl, newdata = atl_test_data, type = "class")
predict_san = predict(model_san, newdata = san_test_data, type = "class")

predict_mia = predict(model_mia, newdata = mia_test_data, type = "class")

predict_jfk = predict(model_jfk, newdata = jfk_test_data, type = "class")

library(hydroGOF)
library(ROCR)

drawROCR = function(prd,testdataset)
{
  predictions = c(prd)
  pred=prediction(predictions,testdataset$isDelayed)
  perf_AUC=performance(pred,"auc")
  AUC=perf_AUC@y.values[[1]]
  perf_ROC=performance(pred,"tpr","fpr")
  perf_ROC
  
}

jfk = drawROCR(predict_jfk,jfk_test_data)
atl = drawROCR(predict_atl,atl_test_data)
san = drawROCR(predict_san,san_test_data)
mia = drawROCR(predict_mia,mia_test_data)

plotCombine = function(a,b,c,d)
{
  plot(a, col = "green",main ="ROC Curve",lwd = 2)
  plot(b, col = "blue",add = TRUE,lwd = 2)
  plot(c, col = "purple",add = TRUE,lwd = 2)
  plot(d, col = "pink",add = TRUE,lwd = 2)
  legend("bottomright", c("JFK: 0.758","MIA: 0.830","SAN: 0.852","ATL: 0.860"), lty=c(1,1), lwd=c(2.5,2.5),col=c("green","blue","purple","pink"))
}
plotCombine(jfk,mia,san,atl)

predictions=c(predict_jfk)
pred=prediction(predictions,jfk_test_data$isDelayed)
perf_AUC=performance(pred,"auc")
AUC=perf_AUC@y.values[[1]]

perf_ROC=performance(pred,"tpr","fpr")
plot(perf_ROC, main="ROC plot")
text(0.5,0.5,paste("AUC = ",format(AUC, digits=8, scientific=FALSE)))


rmse(test_data$isDelayed,as.numeric(predict_dt))
predict_dt
str(predict_dt)
str(test_data$isDelayed)
write.csv(newdataset,file='dataset.csv',quote = FALSE,row.names = TRUE)


summary(newdataset)


rw_nmb = nrow(cleanedData) # 80% of dataset 
test_data = cleanedData[(rw_nmb * 0.8):rw_nmb,]

train_data = select(cleanedData[1:(rw_nmb * 0.8),] #select 80% of data set
                    ,MONTH
                    ,DAY
                    ,DAY_OF_WEEK
                    ,AIRLINE
                    ,ORIGIN_AIRPORT
                    ,DESTINATION_AIRPORT
                    ,SCHEDULED_DEPARTURE
                    ,SCHEDULED_ARRIVAL
                    ,isDelayed
                   
)

#summary(is.na(train_data$ARRIVAL_DELAY))

write.csv(train_data,file='trainData.csv',quote = FALSE,row.names = TRUE)

ntrees  = 6
set.seed(6)

model = gbm.fit(
  x = train_data
  , y = isDelayed,
  distribution = "gaussian",
  n.trees = ntrees,
  shrinkage = 0.01,
  interaction.depth = 3,
  n.minobsinnode = 12,
  nTrain = round(rw_nmb * 0.8*0.8),
  verbose = TRUE.
)
