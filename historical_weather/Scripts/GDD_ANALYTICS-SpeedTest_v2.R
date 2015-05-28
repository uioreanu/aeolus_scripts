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
#restrict these to premium users with planting dates
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
	

	fieldClustering = rawFields[c("defaultLongitude","defaultLatitude")]
	#fieldClustering = fieldClustering[apply(fieldClustering,1,function(x) all(is.na(x)==F)),]
	numClusters = max(floor(nrow(fieldClustering)/100),1)
	while(T){
		clusteringResult = kmeans(fieldClustering,numClusters)  #and keep doing this until cluster size gets smaller
		numClusters = numClusters+1
		if(numClusters>35) break
		if(all(clusteringResult[["withinss"]]<300)) break
	}
	
	fieldClusterList = list()
	for(fieldClusterIndex in unique(clusteringResult$cluster)){
		fieldClusterList[[fieldClusterIndex]] = rawFields[as.numeric(as.character(which(clusteringResult$cluster==fieldClusterIndex ))),"fieldID"]
	}
	#rawFields = fields
	availableDates = unique(rawDat[,2])
  availableDates = as.Date(as.character(availableDates),format="%Y%m%d")
  
	availableDates = datesForThisYear[which(as.Date(datesForThisYear) %in% availableDates)]
	if(length(availableDates)<1) next
	#so available dates should come from the 'allJobs' list, instead of from the rawDat
	availableDates = rev(availableDates)  #prioritize doing most recent...
	
	for(eachDate in availableDates){
		#for weatherdata database, date need to be in format 2014-01-01 (year-month-day)
		startTime = Sys.time()
	
    eligibleFieldIds = allJobs[which(allJobs[,2]==eachDate),1]
		fields = rawFields
		
    fields = fields[which(fields[,"fieldID"] %in% eligibleFieldIds),]
    if(nrow(fields)<1) next
		#query weather database to find out which field IDs have this date #####
		#if any field is done already, remove those from the fieldSelect below ####
	   #check which field ids in fieldClusterList need update, if none need update, go next
	  pickDates = grep(gsub("-","",eachDate),rawDat[,2],perl=T,useBytes=T)
	  dateRestrictedDat = rawDat[pickDates,]
    rawDat = rawDat[-pickDates,]
		#could potentially remove these entries from rawDat each time if are confident won't have to re-run for this year..
		
		saveData = list()
		for(fieldSelectIndex in 1:length(fieldClusterList)){
			
		  #check which field ids in fieldClusterList need update, if none need update, go next
		  fieldClusterFiltered = fieldClusterList[[fieldSelectIndex]][which(fieldClusterList[[fieldSelectIndex]]%in% fields[,"fieldID"])]
		  if(length(fieldClusterFiltered)){
        fieldSelect = as.character(fieldClusterFiltered)
        print("wut is the rpoblem")
		  }else{
        next
		  }
		  
		  keyParams = c("Date","TMAX","TMIN","PRCP")
		 
		  lat = fields[fieldSelect,"defaultLatitude"]
		  lng = fields[fieldSelect,"defaultLongitude"]
		  if(any(is.na(c(lat,lng)))) next
		    
		  minMaxTemps = matrix(nrow=length(fieldSelect),ncol=5)
		  colnames(minMaxTemps) = c("Date","fieldID","TMAX","TMIN","PRCP")
		  allDates1 = eachDate #as.numeric(paste(yearSelect,format(strptime(dateRange, format="%j"), format="%m%d"),sep=""))
		  if(any(allDates1 %in% availableDates)==F) next
		  #so really just select allDates as the dates with no values for which there is data
		  minMaxTemps[,1] = allDates1
		  minMaxTemps[,2] = fields[fieldSelect,"fieldID"]
		  rownames(minMaxTemps) =fields[fieldSelect,"fieldID"]
      skipAll = F
		  for(tempType in c("TMAX","TMIN","PRCP")){	
		    krigInputs = list()
		    
		    keeps = grep(tempType,dateRestrictedDat[,3],perl=T)
		    
		    dat = dateRestrictedDat[keeps,]
		    if(nrow(dat)<3) break
		      
		    matches = which(rawStations[,1] %in% unique(dat[,1]))
		    stations = rawStations[matches,]
		    
		    stationPoints = SpatialPoints(stations[c("Lng","Lat")],proj4string=CRS("+proj=longlat +datum=WGS84"))
		    bufferSizes = rep(.5,times=length(lng))
		    while(T){
        while(T){
			    fieldLocs = gBuffer(SpatialPoints(cbind(lng,lat),proj4string=CRS("+proj=longlat +datum=WGS84")),width=bufferSizes,byid=T)
			    returnArray = gIntersects(stationPoints,fieldLocs,byid=T)
			    eachField = apply(returnArray,1,function(x) length(which(x==T))>2)
			  	    
			    if(all(eachField)) {
			   		break 	
			    }else{
			    	increaseBuffer = which(eachField==F)
			    	bufferSizes[increaseBuffer] = bufferSizes[increaseBuffer]+.5
			    	}
			    	
			    if(any(bufferSizes>5)){
			    	print("give it up")
			    	break
			    }
		    }
        if(all(bufferSizes>3)) skipAll=T
        if(skipAll==T) break
		    uniqueFields = c()
		    for(i in 1:nrow(returnArray)){
		   		uniqueFields = unique(c(uniqueFields,which(returnArray[i,]==T)))
		    }	
		    
		    closestStations = stations[uniqueFields,]
        #if min lat is not > min field and min lng is not < 
		    tryThis = function(minMaxTemps){
		    #what we really want our stations with a bounding box around each point..
		    temp = dat[which(dat[,1] %in% closestStations[,1]),]    
		    temp = temp[is.na(temp[,1])==F,]
		    krigInputs[[1]] = temp
		    workDat = krigInputs[[1]]
			rownames(workDat) = workDat[,1]
			workDat = workDat[closestStations[,1],]
		
		    x =  closestStations[,"Lng"]
		    y =  closestStations[,"Lat"]
		    
		    if(tempType == "PRCP"){
			    pixelRange = max(abs(abs(range(x)[1])-abs(range(x)[2])),abs(abs(range(y)[1])-abs(range(y)[2])))/.05 #want ~.05 degrees per pixel	
			    krigs = list()
			    for(k in 1:length(krigInputs)){
			      #workDat = krigInputs[[k]]
			      #rownames(workDat) = workDat[,1]
			      #workDat = workDat[closestStations[,2],]
			      if(nrow(workDat)<3) {
			        krigs[[names(krigInputs)[k]]] = NA
			        next
			        }
			      pixelRange = max(abs(abs(range(x)[1])-abs(range(x)[2])),abs(abs(range(y)[1])-abs(range(y)[2])))/.05 #want ~.05 degrees per pixel	
			   	  interTemp = tryCatch(interp(x,y,workDat[,4],xo=seq(min(x),max(x),by=.05),yo=seq(min(y),max(y),by=.05),linear=T,duplicate="strip",extrap=F),error = function(e) e)
			      if(inherits(interTemp,"error")) interTemp = NA
			      krigs[[1]] =   interTemp
			      #krigs[[names(krigInputs)[k]]] = krigPlot
			      
			    }
			      dateTrack = c()
		
			   	  z = 1
			      thisDate = names(krigs)[z]
			      dateTrack = c(dateTrack,thisDate)
			      if(is.na(krigs[[z]])){
			        
			        selectedTemp = krigInputs[[z]][1,5]
			      }else{
			       
			        pointSelect = expand.grid(krigs[[z]]$x,krigs[[z]]$y)
			       #try something like this instead to get all the selectTimeIndex for all fields in interpolation apply(sapply(centers,function(x) abs(x-lats)),1,which.min)
					
					selectTempIndex = apply(cbind(lng,lat),1,function(x) {which.min( (pointSelect[,1]-x[1])^2+(pointSelect[,2]-x[2])^2)})
			        #selectTempIndex=which.min(((pointSelect[,1]-lng)^2+(pointSelect[,2]-lat)^2)^.5)
			        fieldSelecter = 1
			        for(selectTemp in selectTempIndex){
			        	colSelect = which(krigs[[z]]$y%in%pointSelect[selectTemp,2])
			       		rowSelect = which(krigs[[z]]$x%in%pointSelect[selectTemp,1])
			      
			    
			        	selectedTemp = krigs[[z]]$z[rowSelect,colSelect]
			        	#if(is.na(selectedTemp)==F) stop("success?")
			        	#if is na, then just use value of nearest airport to point
			        	if(is.na(selectedTemp)){
			        		closestStationID = closestStations[which.min((closestStations[,"Lat"]-pointSelect[selectTemp,2])^2+(closestStations[,"Lng"]-pointSelect[selectTemp,1])^2),"ID"]
			        		selectedTemp	= dat[which(dat[,1]==closestStationID),4]
			        	}
			        	minMaxTemps[fieldSelecter,tempType] = round(selectedTemp)/10	
			        	fieldSelecter =  fieldSelecter+1
			        }
			        
			      }
		    
		    }
		    #just use interp from akima package
		    if(tempType !="PRCP"){
			    krigDat = as.data.frame(cbind(x,y,workDat[,4]))
			    colnames(krigDat) = c("Lng","Lat","Weather")
			    coordinates(krigDat)  =~ Lng+Lat
			    output = autoKrige(Weather~1,krigDat,SpatialPoints(cbind(lng,lat)))
			    predictions = output$krige_output@data[,1]   #this would potentially be a lot easier, test speeds
			    minMaxTemps[,tempType] = round(predictions)/10 
		    }
      return(minMaxTemps)
		  }
			if(skipAll==T) break
			minMaxTemps1 = tryCatch(tryThis(minMaxTemps),error = function(e) e)
			if(inherits(minMaxTemps1,"error")==F){
			  minMaxTemps = minMaxTemps1
        break 
			}else{
       bufferSizes = bufferSizes+1
			}
		}
		if(skipAll==T) break
		  }
		if(skipAll==F){
		  saveData[[length(saveData)+1]] = minMaxTemps 
		}
	
	}
  if(length(saveData)<1) next
	#end date loop
	dataOut = do.call("rbind",saveData)
	dataOut = as.data.frame(cbind(id=0,dataOut[,c("fieldID","Date","TMAX","TMIN","PRCP")]))
	dataOut[,"Date"] = as.Date(as.character(dataOut[,"Date"]),format="%Y-%m-%d")
	dataOut[,"id"] = NA
	colnames(dataOut) = c("id","field_id","date","t_max","t_min","precip")
	write.csv(dataOut,paste(homeDir,"/weatherCSV/",as.numeric(Sys.time()),".csv",sep=""),row.names=F)
	
}

endTimeAll = Sys.time()
print(endTimeAll-startYear)

}

	


#}
