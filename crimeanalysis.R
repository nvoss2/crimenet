#import the data
path<-"~/myscripts/masters/Baton_Rouge_Crime_Incidents.csv"
df<-read.csv(path,header=TRUE,sep=",")
#preprocessing steps

#filter out crimes that were not committed
dfcommit<-subset(df,COMMITTED=="COMMITTED")
dfcommit$GEOLOCATION<-as.character(dfcommit$GEOLOCATION)
sprintf("# of rows in Dataframe: %s",nrow(dfcommit))
#get a count of crimes by day
dfcommit$Date<-as.Date(dfcommit$OFFENSE.DATE,format="%m/%d/%Y")
#remove useless columns
dfcommit<-subset(dfcommit,select=-c("OFFENSE.DATE","CITY","STATE"))
dfcommit$OFFENSE.DATE<-NULL
dfcommit<-subset(dfcommit,Date<=as.Date("2017-09-30"))
df_by_day<-aggregate(dfcommit$COMMITTED,by=list(dfcommit$Date),FUN=length)
#change the column headers
names(df_by_day)<-c("Date","count")
#line chart
library(ggplot2)
plot<-ggplot(df_by_day,aes(x=Date,y=count))+
  geom_line(color="#F2CA27",size=0.1)+
  geom_smooth(color="#1B1B1B")+
  scale_x_date(date_breaks="2 years",date_labels="%Y")+
  labs(x="Date crime was committed",y="# of crimes",title="Daily crimes committed in Baton Rouge")

#get hour from time committed
dfcommit$Hour<-format(strptime(dfcommit$OFFENSE.TIME,"%H%M"),"%H")
#get weekday from date
dfcommit$DayOfWeek<-weekdays(dfcommit$Date)
#group by day of week and hour
dfy_by_hour<-aggregate(dfcommit$COMMITTED,by=list(dfcommit$DayOfWeek,dfcommit$Hour),FUN=length)
names(dfy_by_hour)<-c("DayOfWeek","Hour","Count")
dfy_by_hour$DayOfWeek<-factor(dfy_by_hour$DayOfWeek,levels=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))
#gradient tile plot
plot2<-ggplot(dfy_by_hour,aes(x=Hour,y=DayOfWeek,fill=Count))+
  geom_tile()+
  labs(x="Hour Crime was Committed",y="Day of Week",title="# of Crimes by Hour of Each of Week")+
  scale_fill_gradient(low="white",high="#27AE60")

#group by type of crime
df_crimes<-aggregate(dfcommit$COMMITTED,by=list(dfcommit$CRIME),FUN=length)
names(df_crimes)<-c("Crime","count")
library(plyr)
#sort by count in descending order
df_crimes<-arrange(df_crimes,desc(count))

#also group by times of day etc.
df_crimes_day<-aggregate(dfcommit$COMMITTED,by=list(dfcommit$CRIME,dfcommit$DayOfWeek,dfcommit$Hour),FUN=length)
names(df_crimes_day)<-c("Crime","DayOfWeek","Hour","count")
df_crimes_day$DayOfWeek<-factor(df_crimes_day$DayOfWeek,levels=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))

#trying to normalize values
library(dplyr)
df_crimes_day<-df_crimes_day %>% group_by(Crime) %>% mutate(norm=count/sum(count))
#heatmap plot
plot3<-ggplot(df_crimes_day,aes(x=Hour,y=DayOfWeek,fill=norm))+
  geom_tile()+
  labs(x="Hour Crime was Committed (CST)",y="Day of Week",title="# of Crimes by Crime Type Each Hour of Each Day")+
  scale_fill_gradient(low="white",high="#2980B9")+
  facet_wrap(~Crime,nrow=6)

#try a few more
#group by month
dfcommit$month<-months(dfcommit$Date)
df_crime_month<-aggregate(dfcommit$COMMITTED,by=list(dfcommit$DayOfWeek,dfcommit$month,dfcommit$Hour),FUN=length)
names(df_crime_month)<-c("DayOfWeek","Month","Hour","count")
df_crime_month$DayOfWeek<-factor(df_crime_month$DayOfWeek,levels=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))
df_crime_month$Month<-factor(df_crime_month$Month,levels=c("January","February","March","April","May","June","July","August","September","October","November","December"))

#try to make geomap

# geocodeAddress <- function(address) {
#   require(RJSONIO)
#   url <- "http://maps.google.com/maps/api/geocode/json?address="
#   url <- URLencode(paste(url, address, "&sensor=false", sep = ""))
#   x <- fromJSON(url, simplify = FALSE)
#   if (x$status == "OK") {
#     out <- c(x$results[[1]]$geometry$location$lng,
#              x$results[[1]]$geometry$location$lat)
#   } else {
#     out <- NA
#   }
#   Sys.sleep(0.2)  # API only allows 5 requests per second
#   out
# }

dfcommit$geo1<-NA
dfcommit$X<-as.numeric(0)
dfcommit$Y<-as.numeric(0)
for (i in 1:nrow(dfcommit))
{
  s<-str_extract(dfcommit$GEOLOCATION[i],"\\(.*\\,.*\\)")
  if(!is.na(s))
  {
    dfcommit$geo1[i]<-s
    s<-gsub("\\(","",s)
    s<-gsub("\\)","",s)
    s<-trimws(s)
    
    l<-strsplit(s,",")[[1]]
    dfcommit$X[i]<-l[2]
    dfcommit$Y[i]<-l[1]
  }
}

dfgeo<-subset(dfcommit,!is.na(geo1))
dfgeo$X<-as.numeric(dfgeo$X)
dfgeo$Y<-as.numeric(dfgeo$Y)
library(ggmap)
#baton rouge geocoordinates: -91.221832,30.344436,-90.998726,30.559214
bbox<-c(-91.221832,30.344436,-90.998726,30.559214)
#load map of Baton Rouge
brmap<-get_map(location=bbox,source="stamen",maptype="toner-lite")
#plotting crime points on map
plotmap<-ggmap(brmap)+
  geom_point(data=dfgeo,aes(x=X,y=Y),color="#27AE60",size=0.5,alpha=0.01)+
  labs(title="Location of Crimes in Baton Rouge Jan. 2011 - Sep. 2017")

#filter by type of crime
plotmap2<-ggmap(brmap)+
  geom_point(data=dfgeo,aes(x=X,y=Y,color=CRIME),size=0.4,alpha=0.02)+
  labs(title="Location of Crimes in Baton Rouge by Crime Type Jan. 2011 - Sep. 2017")+
  facet_wrap(~CRIME,nrow=4)+
  theme(legend.position = "none")

#filter by hour of the day
plotmap3<-ggmap(brmap)+
  geom_point(data=dfgeo,aes(x=X,y=Y,color=Hour),size=0.4,alpha=0.02)+
  labs(title="Location of Crimes in Baton Rouge by Hour Jan. 2011 - Sep. 2017")+
  facet_wrap(~Hour,nrow=4)+
  theme(legend.position = "none")



# The MIT License (MIT)
# 
# Copyright (c) 2017 Max Woolf
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the “Software”), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
#   
#   The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.