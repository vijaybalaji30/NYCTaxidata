install.packages("plyr")
install.packages("dplyr")
install.packages("maptools")
install.packages("ggplot2")
install.packages("rgdal")
install.packages("randomForest")
install.packages("hydroGOF")
install.packages("data.table")
library(data.table)
library(plyr)
library(dplyr)
library(maptools)
library(ggplot2)
library(rgdal)
library(randomForest)
library(MASS)
library(hydroGOF)
#Q1------------------------------------------------------------------------------------------
trip_data <- fread("https://s3.amazonaws.com/nyc-tlc/trip+data/green_tripdata_2015-09.csv")
#Read 1494926 rows and 21 (of 21) columns from 0.223 GB file in 00:00:17
trip_data <- tbl_df(trip_data)

#Q2------------------------------------------------------------------------------------------

qplot(trip_data$Trip_distance,binwidth=0.4,main="Histogram depicting Trip Distance",xlab="Trip Distance",fill=I("blue"),col=I("red"))
qplot(trip_data$Trip_distance,binwidth=1,main="Histogram depicting Trip Distance",xlab="Trip Distance",fill=I("blue"),col=I("red"),xlim=c(0,30))
#most trips range between 0 and 10 miles, with journeys of 0-3 miles having most frequency
#as trip distance increases, frequency decreases. This can possibly mean that Green Taxis are typically used for shorter rides.
#only 467 trips have had trips longer than 30 miles

#Q3------------------------------------------------------------------------------------------
#extract pickupdate from each row
datepickup = strptime(trip_data$lpep_pickup_datetime,format="%Y-%m-%d %H:%M:%S",tz="EST")
#extract hour from the pickupdate
hourpickup = strftime(datepickup,"%H",tz="EST")
#add another column called hour in the data frame
trip_data = cbind(trip_data,hour=hourpickup)
## tapply(Summary Variable, Group Variable, Function)
meantripdistancebyhour = tapply(trip_data$Trip_distance, trip_data$hour, FUN=mean)
plot(meantripdistancebyhour,xlab="hour",ylab="mean trip distance")
mediantripdistancebyhour = tapply(trip_data$Trip_distance, trip_data$hour, FUN=median)
plot(mediantripdistancebyhour,xlab="hour",ylab="median trip distance")
#Q3b--------------------------------

location_lookup = read.csv("taxi+_zone_lookup.csv",header=TRUE)
#airports have location id of 1,132,138
#this is the first time i've ever done spatial analysis,looked up a lot of this online
#http://gis.stackexchange.com/questions/124295/convert-coordinates-from-readshapepoly-in-r-to-long-lat-coordinates
area <- readOGR(".", "taxi_zones")
area_longlat <- spTransform(area, CRS("+proj=longlat +datum=WGS84"))
coords_longlat <- tbl_df(coordinates(area_longlat))
colnames(coords_longlat) = c("longitude","latitude")
#definition of near an airport: +/- 0.05 latitudes or longitudes
checknearairport=function(rowoftrips){
  pickuplong=rowoftrips[1]
  pickuplat=rowoftrips[2]
  droplong=rowoftrips[3]
  droplat=rowoftrips[4]
  if ((abs(pickuplong-coords_longlat$longitude[1])<=0.05 && abs(pickuplat-coords_longlat$latitude[1])<=0.05) ||((abs(droplong-coords_longlat$longitude[1]))<=0.05 && abs(droplat-coords_longlat$latitude[1])<=0.05)){return(1)}
  else if ((abs(pickuplong-coords_longlat$longitude[132])<=0.05 && abs(pickuplat-coords_longlat$latitude[132])<=0.05) ||((abs(droplong-coords_longlat$longitude[132]))<=0.05 && abs(droplat-coords_longlat$latitude[132])<=0.05)){return(132)}
  else if ((abs(pickuplong-coords_longlat$longitude[138])<=0.05 && abs(pickuplat-coords_longlat$latitude[138])<=0.05) ||((abs(droplong-coords_longlat$longitude[138]))<=0.05 && abs(droplat-coords_longlat$latitude[138])<=0.05)){return(138)}
  else {return(0)}
}
#tripnear=vector()
#for (i in 1479196:length(trip_data$VendorID)){
  #tripnear[i]=checknearairport(trip_data$Pickup_longitude[i],trip_data$Pickup_latitude[i],trip_data$Dropoff_longitude[i],trip_data$Dropoff_latitude[i])
#}
trip_data$tripnear = apply(trip_data[,6:9],1,checknearairport)

indexoftripnearairports = which(trip_data$tripnear!=0)
numberoftripnearairpots = length(indexoftripnearairports)
percentageoftripsnearairports = length(indexoftripnearairports)/length(trip_data$tripnear)
averagefareofairporttrips = mean(trip_data$Fare_amount[indexoftripnearairports])
averagetipofairporttrips = mean(trip_data$Tip_amount[indexoftripnearairports])
averagetipofothertrips = mean(trip_data$Tip_amount[-indexoftripnearairports])
averagefareofothertrips = mean(trip_data$Fare_amount[-indexoftripnearairports])
tippingratio = (averagetipofothertrips/averagefareofothertrips)/(averagetipofairporttrips/averagefareofairporttrips)
# people tend to tip better in trips not involving airports since the tippingratio is well above 1. This may be because people 
# are usually on a budget while travelling
meanpassengercountofairporttrips = mean(trip_data$Passenger_count[indexoftripnearairports])
meanpassengercountofothertrips = mean(trip_data$Passenger_count[-indexoftripnearairports])
#mean passengercount of trips near aiports is higher than the trips which are not near airports. This may mean that
#trips to or from airports may involve groups of people
#the fact that even though there are more passengers but tipping is worse is surprising.
#Q4----------------------------------------------------------------------
#use only the data which actually has a fare amount
trainingdata = trip_data[which(trip_data$Tip_amount>=1 & trip_data$Tip_amount<=2 & trip_data$Fare_amount>=10 & trip_data$Fare_amount<=20),]
testdata = trip_data[which(trip_data$Fare_amount>20 & trip_data$Fare_amount<=25 & trip_data$Tip_amount>=2 & trip_data$Tip_amount<=2.5),]
meantipbyfare = mean(trainingdata$Tip_amount/trainingdata$Fare_amount)
trainingdata$tippercentage = trainingdata$Tip_amount/trainingdata$Fare_amount
testdata$tippercentage = testdata$Tip_amount/testdata$Fare_amount
#regression
lmtip=lm(formula=tippercentage~Fare_amount,data=trainingdata)
summarylmtip = summary(lmtip)
stderrorlmtip = summarylmtip$coefficients[,2]
predicted_tips_lm=predict(lmtip,data.frame(Fare_amount=testdata$Fare_amount))
rmse_lm = rmse(predicted_tips_lm,testdata$tippercentage)
#randomforest
randomforesttip=randomForest(x=tbl_df(trainingdata$Fare_amount),y=trainingdata$tippercentage,importance = TRUE,ntree=20)
precentvarofrf=randomforesttip$rsq[length(randomforesttip$rsq)]*100
predicted_tips_rf=predict(randomforesttip,tbl_df(testdata$Fare_amount))
rmse_rf = rmse(predicted_tips_rf,testdata$tippercentage)

#randomforest turns out to work better than linear regression due to lesser root mean squared error

##Q5a----------------------------------------------------------------------
#speed=distance/time
#let's find out time duration of each trip
datedropoff = strptime(trip_data$Lpep_dropoff_datetime,format="%Y-%m-%d %H:%M:%S",tz="EST")
trip_data$tripduration = as.numeric(datedropoff-datepickup)
#get the week number out of the date
trip_data$weekoftrip=as.numeric(strftime(datepickup,format = "%W",tz="EST"))
#get the minimum week so that we can subtract that and get a better index of weeks
trip_data$weekoftrip = trip_data$weekoftrip - min(trip_data$weekoftrip) +1
#make it a factor
trip_data$weekoftrip = as.factor(trip_data$weekoftrip)
#take only those data which has a trip duration
speeddata = trip_data[which(trip_data$tripduration!=0),]
#trip speed in mph
speeddata$tripspeed = (speeddata$Trip_distance/speeddata$tripduration)*60*60
meanspeedbyweek = tapply(speeddata$tripspeed,speeddata$weekoftrip,FUN=mean)
week1 = fitdistr(speeddata$tripspeed[which(speeddata$weekoftrip==1)],"normal")
week2 = fitdistr(speeddata$tripspeed[which(speeddata$weekoftrip==2)],"normal")
week3 = fitdistr(speeddata$tripspeed[which(speeddata$weekoftrip==3)],"normal")
week4 = fitdistr(speeddata$tripspeed[which(speeddata$weekoftrip==4)],"normal")
week5 = fitdistr(speeddata$tripspeed[which(speeddata$weekoftrip==5)],"normal")
w1 = speeddata$tripspeed[which(speeddata$weekoftrip==1)]
w2 = speeddata$tripspeed[which(speeddata$weekoftrip==2)]
w3 = speeddata$tripspeed[which(speeddata$weekoftrip==3)]
w4 = speeddata$tripspeed[which(speeddata$weekoftrip==4)]
w5 = speeddata$tripspeed[which(speeddata$weekoftrip==5)]
minlength = min(length(w1),length(w2),length(w3),length(w4),length(w5))
set.seed("1234")
#considering samples of minlength because data frames need to have equal number of rows, and trips grouped by week have unequal rows
weekdata = data.frame(w1=sample(w1,minlength),w2=sample(w2,minlength),w3=sample(w3,minlength),w4=sample(w4,minlength),w5=sample(w5,minlength))
weekcombo = combn(ncol(weekdata),2)

outputmatrix = adply(weekcombo,2,function(x){
  testresults = t.test(weekdata[,x[1]],weekdata[,x[2]])
  output = data.frame("week_a"=colnames(weekdata)[x[1]],"week_b"=colnames(weekdata)[x[2]],
                      "t_value"=testresults$statistic,
                      "p_value"=testresults$p.value)
  return(output)
})
#most of the p-values are greater than 0.05 so the speeds are more or less the same across each week of november

dateofspeeddata = strptime(speeddata$Lpep_dropoff_datetime,format="%Y-%m-%d %H:%M:%S",tz="EST")
speeddata$timeofday = strftime(dateofspeeddata,format = "%H:%M:%S",tz="EST")

converttoseconds = function(time){
  timestring = unlist(strsplit(time,':')); 
  seconds = (as.numeric(timestring[3]) +as.numeric(timestring[2])*60 + as.numeric(timestring[1])*3600);
  return (seconds)
}
speeddata$timeofday = unlist(lapply(speeddata$timeofday,converttoseconds))
timefit = lm(tripspeed~timeofday,data=speeddata)
speeddata$predictedspeed = predict(timefit,data.frame(timeofday = speeddata$timeofday))
summary(timefit)
#Hypothesis:average speed decreases as time increases. This may be due to the fact that traffic increases as the day goes by.
