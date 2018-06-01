################################### UBER ASSIGNMENT ########################################
#*******************************        Sahana K        ************************************
############################################################################################


#Set the directory Path
#setwd("C:/Data_Science/UBER assignment")

#remove the earlier data present
rm(list=ls())

#**** Install the necessary packages ****
install.packages("lubridate")
install.packages("dplyr")
install.packages("reshape2")
install.packages("ggplot2")

#**** Load the required packages ****
library(lubridate)
library(dplyr)
library(reshape2)
library(ggplot2)

#*********************************************************************************************


#**** Read the input file from the directory and assign to the variable Input **** 

Input<-read.csv("Uber Request Data.csv")
View(Input)

#################### Data Cleansing and preparation : #######################

# The file is provided with 6 columns and has 6745 records

#checking for duplicate Request id
Input[which(duplicated(Input$Request.id)),]
# - No duplicate Requests in the file given. This becomes the primary key for the file . 	

#Checking for NA values in the given columns :
Input[which(is.na(Input$Request.id)==TRUE),]       
Input[which(is.na(Input$Driver.id)==TRUE),]        
Input[which(is.na(Input$Pickup.point)==TRUE),]     
Input[which(is.na(Input$Status)==TRUE),]           
Input[which(is.na(Input$Request.timestamp)==TRUE),]        
Input[which(is.na(Input$Drop.timestamp)==TRUE),]   

# From the above check on NA values in the file , only columns Driver.id and Drop.timestamp has NA values.
# These values seem to be proper as they are present for only those rows with Status column as "no cars available"
# Hence these NA rows are retained as such in the data .


#*********************************************************************************************

#The Request.timestamp and Drop.timestamp columns do not have all their values in standard format. 

#***** Timestamp formatting ****

Input$Request.timestamp<-parse_date_time(x = Input$Request.timestamp,
                                         orders = c("dmy HMS", "dmy HM"),
                                         locale = "eng")

Input$Drop.timestamp<-parse_date_time(x = Input$Drop.timestamp,
                                      orders = c("dmy HMS", "dmy HM"),
                                      locale = "eng")


#*********************************************************************************************

#**** Deriving columns for analysis ****

Input$Req.hour<-hour(Input$Request.timestamp)


#################### Data Analysis  #######################


#Frequency of request during diff hours
ggplot(Input,aes(Input$Req.hour))+geom_bar(stat="count",fill="Navy Blue")+scale_x_continuous(name="Request Hours in a day",breaks=seq(0,23,1)) + geom_text(stat='count',aes(label=..count..),vjust=-1) + ggtitle("Cab request pattern during day(hrs)")

#   The above plot shows the time period at which the most number of request are made overall. More requests are during 
#	5-9 in the morning and 17 - 21 during the evening


#split the frequency from city and airport
ggplot(Input,aes(x=Req.hour))+geom_bar(fill="Brown")+facet_wrap(~Pickup.point) + scale_x_continuous(name="Request Hours in a day",breaks=seq(0,23,1)) + ggtitle("Requests at different hours at Airport and City")
# -- Gives the info abt the time frame where the demand is more . Airport request come during 17 - 21 in the evening 
#and 5 - 9 in the morning request for city are more.


#################### Visually identify the most pressing problems for Uber  #######################

#Frequency of requests that get 'cancelled' or show 'no cabs available'
ggplot(subset(Input,Input$Status != "Trip Completed"),aes(x=Status))+geom_bar(stat="count",fill="Light Blue")+facet_wrap(~Pickup.point)+geom_text(stat='count',aes(label=..count..),vjust=-1)

#Identify the status of the request for both pickup points at different time slots
ggplot(subset(Input,Input$Status != "Trip Completed"),aes(x=Req.hour,fill=Status))+geom_bar(stat="count")+facet_wrap(~Pickup.point)+scale_x_continuous(name="Request Hour",breaks=seq(0,23,1))+scale_y_continuous(name="Number of Requests ")+ggtitle("Frequency of requests that gets cancelled or show 'no cars available'")  
# -- Gives the info abt the time frame where the demand is more

#***Peak timeslots

cityToairport <- subset(Input, Input$Pickup.point == "City")
cityToairport_requests <- nrow(cityToairport) #3507 
sort(table(cityToairport$Req.hour), decreasing=TRUE)[1:6] # peak hours 05  08  09  07  06  10
#For City pickups, 05-10am is the peak hour of booking.

airportTocity <- subset(Input, Input$Pickup.point == "Airport")
airportTocity_requests <- nrow(airportTocity) #3238
sort(table(airportTocity$Req.hour), decreasing=TRUE)[1:6] # peak hours 18  20  19  21  17  22  23 
#For Airport pickups, 05-10pm is the peak hour of booking.


# Creating TimeSlots   
#	0-4 --> Mid Night
#	4-8 --> Early Morning
#	8-12 --> Morning
#	12-16 --> After noon
#	16-20 --> Evening
#	20-24 --> Night

Input$TimeSlot<- ifelse(Input$Req.hour %in% 0:4, "mid night",                                                         
                      ifelse(Input$Req.hour %in% 4:8, "early morning",                                                                
                         ifelse(Input$Req.hour %in% 8:12, "morning",                                                                       
                          ifelse(Input$Req.hour %in% 12:16, "after noon",                                                                              
                           ifelse(Input$Req.hour %in% 16:20, "evening", "night")))))

ggplot(subset(Input,Input$Status != "Trip Completed"),aes(x=TimeSlot,fill=Status))+geom_bar(stat="count")+facet_wrap(~Pickup.point)+scale_x_discrete(name="TimeSlots")+scale_y_continuous(name="Number of Requests ")+ggtitle("Frequency of requests that get cancelled or show 'no cars available")  


#*********************************************************************************************

#Conclusion :
# From the plot visuals it is evident that the most problematic timeslot and request is as below:
    # Evening --> Majorly due to unavailability of cabs for airport pickup
    # Early Morning --> Majorly due to cancellation of cabs for city pickup


#################### Identify gap between supply and demand  #######################

#***Supply-Demand gap: 

#Airport to City at peak hour - Evening

airport_evening<-Input[which(Input$Pickup.point=="Airport" & Input$TimeSlot=="evening"),]
airport_gap<-length(airport_evening$Request.id)-nrow(airport_evening[which(airport_evening$Status=="Trip Completed"),])
airportgap_percentage <- (airport_gap/(length(airport_evening$Request.id)))*100
airportgap_percentage
#Gap is supply vs demand at the evening time slot for Airport pickup is :1145 ==> 78.58%

#City to Airport at peak hour - Early Morning
City_morning<-Input[which(Input$Pickup.point=="City" & Input$TimeSlot=="early morning"),]
City_gap<-length(City_morning$Request.id)-nrow(City_morning[which(City_morning$Status=="Trip Completed"),])
Citygap_percentage <- (City_gap/(length(City_morning$Request.id)))*100
Citygap_percentage
#Gap is supply vs demand at the early morning time slot for City pickup is : 962 ==> 72.05%

#***Supply-Demand Gap plot:

Evening <- group_by(Input[which(Input$Pickup.point=="Airport" & Input$TimeSlot=="evening"),], Pickup.point,TimeSlot) 
sd_gap_eve <- summarise(Evening, Demand = length(which(!is.na(Request.id))), Supply = length(which(Status=="Trip Completed")), Supply_Demand_Gap = (length(which(!is.na(Status))) - length(which(Status=="Trip Completed"))))
transpose_Gap_eve <- melt(sd_gap_eve, id.vars=c('TimeSlot','Pickup.point'))
ggplot(transpose_Gap_eve, aes(x=TimeSlot, y=value, fill=variable)) + geom_bar(stat='identity', position='dodge') + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title = "Supply Demand Gap for Airport pickup during peak hour", y="Request") + facet_wrap(~Pickup.point)

Morning <- group_by(Input[which(Input$Pickup.point=="City" & Input$TimeSlot=="early morning"),], Pickup.point,TimeSlot) 
sd_gap_mor <- summarise(Morning, Demand = length(which(!is.na(Request.id))), Supply = length(which(Status=="Trip Completed")), Supply_Demand_Gap = (length(which(!is.na(Status))) - length(which(Status=="Trip Completed"))))
transpose_Gap_mor <- melt(sd_gap_mor, id.vars=c('TimeSlot','Pickup.point'))
ggplot(transpose_Gap_mor, aes(x=TimeSlot, y=value, fill=variable)) + geom_bar(stat='identity', position='dodge') + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title = "Supply Demand Gap for City pickup during peak hour", y="Request") + facet_wrap(~Pickup.point)

#*********************************************************************************************

SUPvsDem <- group_by(Input, Pickup.point,TimeSlot) 
supply_demand_gap <- summarise(SUPvsDem, Demand = length(which(!is.na(Request.id))), Supply = length(which(Status=="Trip Completed")), Supply_Demand_Gap = (length(which(!is.na(Status))) - length(which(Status=="Trip Completed"))))
transpose_Gap <- melt(supply_demand_gap, id.vars=c('TimeSlot','Pickup.point'))

ggplot(transpose_Gap, aes(x=TimeSlot, y=value, fill=variable)) + geom_bar(stat='identity', position='dodge') + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title = "Supply Demand Gap for pickup points", y="Request") + facet_wrap(~Pickup.point)


#*********************************************************************************************

#Conclusion :
     # Time slots when the highest gap exists --> Evening from airport and Early morning from city


################################### END ########################################
