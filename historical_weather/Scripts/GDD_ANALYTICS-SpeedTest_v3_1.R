list.of.packages = c("zoo","kriging","RMySQL","akima","automap","rgeos","rjson")
new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages,repos="http://cran.rstudio.com/")

library(zoo)
library(kriging)
library(RMySQL)
library(akima)
library(zoo)
library(rjson)
library(automap)  #best kriging library, but kriging has extrapolation problems. Maybe use kriging for temperature and inverse distance for rainfall
library(rgeos)

homeDir = "~/Desktop/aeolus_scripts/historical_weather"
setwd(homeDir)
if(file.exists("weatherCSV")==F) dir.create("weatherCSV")
source("Scripts/repairLatLng.R")

rawStations = read.csv("station_locationByID.csv",stringsAsFactors=F)
colnames(rawStations) =c("ID","Lat","Lng","Altitude","State","Name","Something")
#every day loop throigh all years and days of year, if didnt add any new fields, will only actually need to do do one date
currentYear = as.numeric(strsplit(as.character(Sys.time()),"-")[[1]][1])
#for(yearSelect in c(2008,2009,2010,2011,2012,2013)){
	
	
#first, get list of all possible dates (2010- yesterday)
#get list of all active field ids from fields table
#for each field id, check which dates don't already exist in weatherdata table
allDates = as.character(seq(as.Date("2010/1/1"), as.Date(Sys.Date()-3), "days"))

ENV = fromJSON(file="environment_variables.json")
dbPass = ENV[["PASSWD"]]
dbUsr = ENV[["USER"]]
dbName =  ENV[["DB"]]
dbHost = ENV[["HOST"]]

repairLatLng(dbPass,dbUsr,dbName,dbHost )

db = dbConnect(MySQL(), user=dbUsr ,password=dbPass ,dbname=dbName,host=dbHost)

query = paste("SELECT fieldID FROM fields where fieldBlockGeometry != ''")
rs = dbSendQuery(db, query)
fields = fetch(rs, n=-1)
fields = fields[,1]

#loop through field, keep client name
query = paste("SELECT name,fieldID,clientID,defaultLatitude,defaultLongitude FROM fields")
rs = dbSendQuery(db, query)
fields = fetch(rs, n=-1)


fields = fields[apply(fields,1,function(x) all(is.na(x["defaultLongitude"])==F)),]
rownames(fields) = fields[,"fieldID"]
rawFields = fields

jobList = list()

for(eachField in fields[,"fieldID"]){
	query = paste("SELECT date FROM weatherdata where field_id = '",eachField,"'",sep="")
	rs = dbSendQuery(db, query)
	fieldDates =  fetch(rs, n=-1)
	if(nrow(fieldDates)>1){
		stillNeedIndex = which(allDates %in% fieldDates[,1] == F)
	}else{
		stillNeedIndex = 1:length(allDates)
	}
	if(length(stillNeedIndex)){
	
		jobList[[length(jobList)+1]] = cbind(eachField,allDates[stillNeedIndex])
	}
}
dbDisconnect(db)

allJobs = do.call("rbind",jobList)
allJobs = tryCatch(allJobs[order(allJobs[,2]),],error = function(e) e)

breakOut = F
if(inherits(allJobs,"error")){
  breakOut=T
}


#prioritize doing todays stuff first		
		
for(yearSelect in c(2015:2010)){
  if(breakOut) break
  startYear = Sys.time()
	if(file.exists("Data")==F) dir.create("Data")
	if(as.numeric(substr(Sys.time(),0,4))==yearSelect | file.exists(paste("Data/",yearSelect,".csv",sep=""))==F){
	  download.file(paste("ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/by_year/",yearSelect,".csv.gz",sep=""),paste("Data/",yearSelect,".csv.gz",sep=""))
	  #set overwrite option for gunzip
	  system(paste("gunzip -f Data/",yearSelect,".csv.gz",sep="")) 
	}
	
	#for(jobDate in unique(allJobs[,2])){
	
	if(any(grepl(paste("^",yearSelect,sep=""),allJobs[,2]))==F) next
	datesForThisYear = unique(allJobs[grep(paste("^",yearSelect,sep=""),allJobs[,2]),2])
  if(yearSelect==substr(Sys.time(),0,4)){
  
    keepDates = c(1:max(1,length(datesForThisYear)-3))
    dateForThisYear = datesForThisYear[keepDates]
  }
  #if current year, perhaps should only look at days earlier than 2 days ago
	
	rm(rawDat)
	rm(dat)
	gc()
	rawDat = read.csv(paste("Data/",yearSelect,".csv",sep=""),stringsAsFactors=F)
	#keeps = grep("TMAX|TMIN|PRCP",rawDat[,3],perl=T)
	#dat = dat[keeps,]
	
	  
	#fieldIDsToGrab = allJobs[which(allJobs[,2]==as.Date(eachDate,format="%Y%m%d")),1]
	#fieldIDsToGrab = paste("'",paste(fieldIDsToGrab,collapse="','"),"'",sep="")
	#query = paste("select fieldID,defaultLatitude,defaultLongitude from fields where fieldID in (",fieldIDsToGrab,")",sep="")
	#rs = dbSendQuery(db, query)
	#fieldsExtract = fetch(rs, n=-1)
	#if(nrow(fieldsExtract)<1) fieldsExtract = NULL
	fieldsExtract = NULL
	 tempExtract = SpatialPointsDataFrame(SpatialPoints(cbind(fieldsExtract[,"defaultLongitude"],fieldsExtract[,"defaultLatitude"])),as.data.frame(matrix(ncol=1,fieldsExtract[,"fieldID"])))

    #in real life, look for existing rasters, if don't exist, create them
	availableDates = unique(rawDat[,2])
    availableDates = as.Date(as.character(availableDates),format="%Y%m%d")
  
	availableDates = datesForThisYear[which(as.Date(datesForThisYear) %in% availableDates)]
	if(length(availableDates)<1) next
	#so available dates should come from the 'allJobs' list, instead of from the rawDat
	availableDates = rev(availableDates)  #prioritize doing most recent...
	
	for(eachDate in availableDates){
		#for weatherdata database, date need to be in format 2014-01-01 (year-month-day)
		startTime = Sys.time()
	
	  if(length(list.files("Interpolation_Store",pattern=paste("^",eachDate,sep="")))>1) next	  
	  pickDates = grep(gsub("-","",eachDate),rawDat[,2],perl=T,useBytes=T)
	  dateRestrictedDat = rawDat[pickDates,]
	  eligible = by(dateRestrictedDat,dateRestrictedDat[,1],function(x) c(x[1,1],length(grepl("TMAX|TMIN|PRCP",x[,3]))==3))
	  eligible = by(dateRestrictedDat,dateRestrictedDat[,1],function(x) {
	  	if(length(grep("TMAX|TMIN|PRCP",x[,3]))==3){
	  		c(x[1,1],x[grep("TMAX",x[,3]),4],x[grep("TMIN",x[,3]),4],x[grep("PRCP",x[,3]),4])
	  	}else{
	  		F
	  		} 
	  })
	  eligibleID = do.call("rbind",eligible)
	  eligibleID = eligibleID[which(eligibleID[,1]!="FALSE"),]
	  rownames(eligibleID) = eligibleID[,1]
	 #dateRestrictedDat = dateRestrictedDat[which(dateRestrictedDat[,1] %in%   eligibleID[which(eligibleID[,2]=="TRUE"),1]),] 
	  
	  #use station data to create a spatialPoints data frame
	  restrictedStations = rawStations[which(rawStations[,1] %in% unique(eligibleID[,1])),]
	  allPointsData = list()
	  for(z in 1:nrow(restrictedStations)){
	  	 xx = SpatialPoints(cbind(restrictedStations[z,"Lng"],restrictedStations[z,"Lat"]))
	  	 yy = SpatialPointsDataFrame(xx,as.data.frame(matrix(nrow=1,as.numeric(eligibleID[restrictedStations[z,1],2:4]))))
	  	 allPointsData[[z]] = yy 
	  }
	  
	  fullStationDataFrame = do.call("rbind",allPointsData)
	  proj4string(fullStationDataFrame) = "+proj=longlat +datum=WGS84"
	  	  
	  saveData = list()
	 colnames(fullStationDataFrame@data) = c("TMAX","TMIN","PRCP") 
	 for(metric in colnames(fullStationDataFrame@data)){
	 	z2 = interp(fullStationDataFrame@coords[,1],fullStationDataFrame@coords[,2],fullStationDataFrame@data[,metric],xo =seq(-180,180,by=.1),yo=seq(-90,90,by=.1),linear=T,duplicate="strip")
 	
		testRaster = raster(z2)
		writeRaster(testRaster,paste("Interpolation_Store/",eachDate,"_",metric,sep=""),overwrite=T)
		
		if(is.null(fieldsExtract)){
			next
		}else{
		
	
		#in rea life, remove the next and extract the points necessary here in a csv
				 
		 
		 theseValues = extract(testRaster, tempExtract)
		 saveData[[length(saveData)+1]] = theseValues
		  
		  
	
		 
		 }
	 }
	 if(is.null(fieldsExtract)) next
	 
	 	 dataOut = do.call("cbind",saveData)
		dataOut = as.data.frame(cbind(id=0,field_id=fieldsExtract[,"fieldID"],Date=eachDate,dataOut))
		dataOut[,"Date"] = as.Date(as.character(dataOut[,"Date"]),format="%Y%m%d")
		dataOut[,"id"] = NA
		colnames(dataOut) = c("id","field_id","date","t_max","t_min","precip")
		write.csv(dataOut,paste(homeDir,"/weatherCSV/",as.numeric(Sys.time()),".csv",sep=""),row.names=F)
	 
	 
	#extract the associated values at each latlng location from this test raster
	#do that for all weather variables
	#save the 
	
	


	  
	  
	  
}

endTimeAll = Sys.time()
print(endTimeAll-startYear)

}

	


#}
