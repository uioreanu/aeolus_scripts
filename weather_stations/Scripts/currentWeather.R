list.of.packages = c("rjson","XML","RMySQL")
new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(rjson)
library(XML)
library(RMySQL)
homeDir = "~/Desktop/aeolus_scripts/weather_stations"
setwd(homeDir)

ENV = fromJSON(file="environment_variables.json")
dbPass = ENV[["PASSWD"]]
dbUsr = ENV[["USER"]]
dbName =  ENV[["DB"]]
dbHost = ENV[["HOST"]]

db = dbConnect(MySQL(), user=dbUsr ,password=dbPass ,dbname=dbName,host=dbHost)


#so pass each field center lat lng here in here:, and produce that using gCentroid on the entire fieldBlockGeometry
dat = fromJSON(file="http://forecast.weather.gov/MapClick.php?lat=38.4247341&lon=-86.9624086&FcstType=json")

stationID = dat$currentobservation$id
stationLocation = c(dat$location$longitude,dat$location$latitude)
currentObservations = dat$currentobservation

currentTextDescription = dat$data$text[1]
vagueWeatherDescription = dat$data$weather
timePeriods = dat$time$startValidTime
fullOut = c()

timeToSearch = "4:00" #4am, its on military time
searchTable = paste("Output/",stationID,".csv",sep="")
if(file.exists(searchTable )==F){
	#potentially hang on to these tables and re-use the tables without having to re-scrape these too much
	 nextLevelURL = paste("http://forecast.weather.gov/data/obhistory/",stationID,".html",sep="")
	 doc = htmlParse(readLines(nextLevelURL))
	 tableNodes = getNodeSet(doc, "//table")
	 tb = readHTMLTable(doc=tableNodes[[4]],header=T) 
	 write.csv(tb,paste("Output/",stationID,".csv",sep=""),row.names=F)

 }else{
 	tb = read.csv(searchTable ,stringsAsFactors=F)
 }
 
currentDay = format(Sys.Date(),"%d")
currentMonth = format(Sys.Date(),"%m")
currentYear = format(Sys.Date(),"%Y")
lastMonth = as.numeric(currentMonth)-1
lastYear = currentYear
if(currentMonth == "1") lastMonth = 12
if(currentMonth == "1") lastYear = as.numeric(currentYear)-1

workTable= tb[grep("^[0-9]*$",tb[,1]),]
subtractDays = as.numeric(currentDay)-as.numeric(as.character(workTable[,1]))
newDates = Sys.Date()-subtractDays
if(any(subtractDays<1)){
	modifyIndex = which(subtractDays<1)
	changeMonthDates = as.Date(paste(lastYear,"-",lastMonth,"-",workTable[modifyIndex,1],sep=""))
	newDates[modifyIndex] = changeMonthDates
}

dateTime = format(paste(newDates," ",workTable[,2],":00",sep="")) #will prb need mod
colPull = c(1,3,4,5,6,7,11,16)
outputTable = workTable[,colPull]
outputTable[,1] = dateTime

#add station id to output table!!!!###
#outputTable should go to a db after checking and removing duplicates based on station id an d
#need a more sophisticated way of getting exact date of prediction,
#them, check database for existing prediction for that date/station, and only update the ones that don't match

 #get current day of month, look for the first occurence of that along with a time before cront ask started (4am)

#ehh just find the first occurence of 4am, and then go down the next occurence of 4 am, and figure out the date of the first
#this bit may not be necessary

convertedTimes = strptime(tb[,2],format="%H:%M")
selectFirst = which(convertedTimes <strptime(timeToSearch,format="%H:%M"))[1]
selectSecond = which(convertedTimes==convertedTimes[selectFirst])[2]
rangeTable = c(selectFirst:selectSecond)
totalRainfallInPeriod = sum(as.numeric(as.character(tb[rangeTable,16])),na.rm=T)

thisOut = c(timePeriods[1],vagueWeatherDescription[1],currentTextDescription[1],totalRainfallInPeriod)
fullOut = rbind(fullOut,thisOut)
colnames(fullOut) = c("Valid Time","Short Description","Long Description","24 Hour Rainfall (in)")

#ehh, probably just rainfall and text description
#what does output look like
Sys.sleep(2)
#at the end of all this, delete the tempTables

dbDisconnect(db)