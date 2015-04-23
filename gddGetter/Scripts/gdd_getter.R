
setwd("~/Desktop/aeolus_scripts/gddGetter")
list.of.packages = c("RMySQL","mailR","maptools","RColorBrewer","sp","rgeos","rjson")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages,repos="http://cran.rstudio.com/")

library(RMySQL)
library(mailR)
library(maptools)
library(sp)
library(RColorBrewer)
library(rgeos)
library(rjson)

setwd("~/Desktop/aeolus_scripts/gddGetter/Output")

removeOldFiles = list.files()
for(eachFile in removeOldFiles){
	unlink(eachFile)
}
	
gdd_calculate = function(weather,max_threshold,min_threshold){
	#assumes input array starts at plant date,
	dailyGDD = matrix(ncol=1,apply(weather,1,function(temp){
		(((max(min_threshold,min(max_threshold,as.numeric(temp["t_max"])))+min(max_threshold,max(min_threshold,as.numeric(temp["t_min"])))))/2)-min_threshold
	}))
	cumulativeGDD = cumsum(dailyGDD)
	
	output = cbind(dailyGDD,cumulativeGDD)
	colnames(output) = c("Daily","Cumulative")
	return(output)
}

setwd("~/Desktop/aeolus_scripts/gddGetter")

cornDates = read.csv("Input/corn_imaging_periods.csv",stringsAsFactors=F)

ENV = fromJSON(file="environment_variables.json")
db = dbConnect(MySQL(), user=ENV[["USER"]],password=ENV[["PASSWD"]],dbname=ENV[["DB"]],host=ENV[["HOST"]])


query = paste("SELECT fieldID,name,defaultLatitude,defaultLongitude,planting_date from fields")
rs = dbSendQuery(db, query)
allFields = fetch(rs, n=-1)

fullOutput = c()

for(eachFieldIndex in 1:nrow(allFields)){
	fieldID = allFields[eachFieldIndex,"fieldID"]
	name = allFields[eachFieldIndex,"name"]
	lat =  allFields[eachFieldIndex,"defaultLatitude"]
	lng =   allFields[eachFieldIndex,"defaultLongitude"]
	plantDat =  allFields[eachFieldIndex,"planting_date"]


	### #need to look up plant date, if that fails, look for USDA estimate for that state, if that fails use April 1 ###
	if(is.null(plantDat)) next
	query = paste("SELECT * from weatherdata where field_id = '",fieldID,"' AND date >= '",as.Date(plantDat),"'",sep="")
	rs = dbSendQuery(db, query)
	weather = fetch(rs, n=-1)
	
	if(nrow(weather)<1) {
		print(paste("No weather date for fieldID ",fieldID))
		next
	}
	
	query = paste("SELECT crop_id from fields_crops where fields_id = '",fieldID,"'",sep="")
	rs = dbSendQuery(db, query)
	crop = fetch(rs, n=-1)
	crop = matrix(nrow=1,"corn")
	
	if(nrow(crop)<1){
		print(paste("No crop type for fieldID ",fieldID))
		next
	}
	
	gdd_profile = gdd_calculate(weather,30,10)
	stage_gdd_mapping = read.csv("Input/110DayCorn.csv",stringsAsFactors=F)  #need smarter way of choosing this reference
	stage_gdd_mapping[,"Estimate"] = gsub("[^0-9]","",stage_gdd_mapping[,"Estimate"])
	
	firstOccurences = c()
	for(eachStage in unique(stage_gdd_mapping[,"Stage"])){
		firstMatch = which(stage_gdd_mapping[,"Stage"]==eachStage)[1]
		
		firstOccurences = rbind(firstOccurences,stage_gdd_mapping[firstMatch,c("Stage","Estimate")])
	}
	firstOccurences = as.data.frame(as.matrix(firstOccurences))
	firstOccurences[,2] = as.numeric(as.character(firstOccurences[,2]))
	firstOccurences[,1] = as.character(firstOccurences[,1])
	#forecast next 5 days by taking the average of last 5 days and extending
#	totalGDD = gdd_profile[nrow(gdd_profile),"Cumulative"]+(3*)
	timeLag = as.numeric(Sys.Date()-max(as.Date(weather[,"date"])))
	totalGDD = gdd_profile[nrow(gdd_profile),"Cumulative"]+(timeLag*gdd_profile[nrow(gdd_profile),"Daily"])
	
	if(length(which(cornDates[,"GDD.End"]>totalGDD))){
		
		
		query = paste("SELECT * from weatherdata where field_id = '",fieldID,"' AND date >= '",(Sys.Date()-365),"' AND date <='",Sys.Date()-330,"'",sep="")
		rs = dbSendQuery(db, query)
		weather = fetch(rs, n=-1)
		
		gdd_forecast_profile = gdd_calculate(weather,30,10)
	
	
	     GDDtilNextImage = cornDates[which(cornDates[,"GDD.End"]>totalGDD)[1],"GDD.Start"]-totalGDD
	    daysTilNextImage = which(gdd_forecast_profile[,"Cumulative"]>GDDtilNextImage)[1]
	     if(length( daysTilNextImage )){
		     nextEventName = cornDates[which(cornDates[,"GDD.End"]>totalGDD)[1],"Imaging.Event"]
			dateOfImagingEvent = as.character(Sys.Date()+daysTilNextImage)
		}else{
			daysTilNextImage = "More than 30 days"
			dateOfImagingEvent = "More than 30 days"			
			}
		}else{
		nextEventName = "Complete"
		dateOfImagingEvent = "Complete"
		daysTilNextImage = "Complete"
		next
	}
	
	day5Extend = totalGDD + (5*gdd_profile[nrow(gdd_profile),"Daily"])
	currentStage = which(firstOccurences[,2]>totalGDD)[1]-1
	comingSoonStage = which(firstOccurences[,2]>day5Extend)[1]-1
	
	tempOut = c(as.character(Sys.Date()),fieldID,name,totalGDD,firstOccurences[currentStage,"Stage"],firstOccurences[comingSoonStage,"Stage"],  nextEventName ,dateOfImagingEvent,daysTilNextImage,lat,lng)
	
	fullOutput = rbind(fullOutput,tempOut)

}
if(nrow(fullOutput)<1) stop("No data")
colnames(fullOutput) = c("Estimate Date","fieldID","fieldName","Estimated GDD","Current Stage","Stage In Next 5 Days","Next Imaging Event Stage","Date of next Image Event","Days till Flight","Lat","Lng")


#subset of fullOutput goes to gdd table

tempDat = as.data.frame(fullOutput)
rownames(tempDat) = tempDat[,"fieldID"]



pins = SpatialPointsDataFrame(SpatialPoints(cbind(as.numeric(fullOutput[,"Lng"]),as.numeric(fullOutput[,"Lat"]))),data=tempDat)
proj4string(pins) = "+proj=longlat +datum=WGS84" 
#now to make this into a kml shape file with colored points
#make kml from scratch..
pinData = as.data.frame(as.matrix(cbind(pins@data,"colorCurrent Stage"="Black","colorStage In Next 5 Days"="Black")))
pinData[,"colorCurrent Stage"] = as.character(pinData[,"colorCurrent Stage"])
pinData[,"colorStage In Next 5 Days"] = as.character(pinData[,"colorStage In Next 5 Days"])
pinData[,"Current Stage"] = gsub("^$","Not Emerged",pinData[,"Current Stage"])
pinData[,"Stage In Next 5 Days"] = gsub("^$","Not Emerged",pinData[,"Stage In Next 5 Days"])


library(RColorBrewer)
allStages = unique(c(as.character(unique(pinData[,"Current Stage"])),as.character(unique(pinData[,"Stage In Next 5 Days"]))))
colors = brewer.pal(length(allStages),"RdYlGn")
if(length(colors)<length(allStages)) colors = rep(colors,times=ceiling(length(allStages)/length(colors)))

for(i in 1:length(allStages)){
	pinData[which(pinData[,"Current Stage"]==allStages[i]),"colorCurrent Stage"] = paste(64,substr(colors[i],6,7),substr(colors[i],4,5),substr(colors[i],2,3),sep="") 
	pinData[which(pinData[,"Stage In Next 5 Days"]==allStages[i]),"colorStage In Next 5 Days"] = paste(64,substr(colors[i],6,7),substr(colors[i],4,5),substr(colors[i],2,3),sep="") 
}

pins@data = pinData
stageEstimate_outputName = paste("Output/CropStageEstimate_",as.character(Sys.Date()),".kml",sep="")
tf <- stageEstimate_outputName 
kmlFile <- file(tf, "w")

cat(kmlPolygon(kmlname="PolygonViz")$header, 
file=kmlFile, sep="\n")
    
for(stageForecast in c("Current Stage","Stage In Next 5 Days")){
		cat(paste("<Folder><name>",stageForecast,"</name>",sep=""),   file=kmlFile, sep="\n")
	for(k in unique(pins@data[,stageForecast])){    
		cat(paste("<Folder><name>",k,"</name>",sep=""),   file=kmlFile, sep="\n")
		tempD = pins@data[which(pins@data[,stageForecast]==k),]
	    for(i in 1:nrow(tempD )){
		    	addText = paste('
		  <Placemark> <name> ',gsub("&","",tempD [i,"fieldName"]),' </name> <Style id="Mavrx Airport">      <IconStyle>        <Icon>          <href>http://www.clker.com/cliparts/4/5/c/0/12249615002008292168Japanese_Map_symbol_(Orchard).svg.med.png</href>             <scale> 0.5 </scale></Icon> <color> ',tempD [i,paste("color",stageForecast,sep="")],'</color> </IconStyle>    </Style>  <Point><coordinates> ',tempD [i,"Lng"],',',tempD [i,"Lat"],',0</coordinates> </Point> </Placemark>',sep='')
		cat(addText,file=kmlFile,sep="\n")
		}
		cat("</Folder>",   file=kmlFile, sep="\n")
	}
	cat("</Folder>",   file=kmlFile, sep="\n")
}

cat(kmlPolygon()$footer, file=kmlFile, sep="\n")
close(kmlFile)


#just plot all fields we have in db with pins
tf <- paste("Output/AllFieldsPin_",as.character(Sys.Date()),".kml",sep="")
kmlFile <- file(tf, "w")

cat(kmlPolygon(kmlname=paste("All_Fields_",as.character(Sys.Date())))$header, 
file=kmlFile, sep="\n")

 for(i in 1:nrow(allFields )){
		   addText = paste('
		  <Placemark> <name> ',gsub("&","",allFields[i,"name"]),' </name> <Style id="Mavrx Airport">      <IconStyle>        <Icon>          <href>http://www.clker.com/cliparts/4/5/c/0/12249615002008292168Japanese_Map_symbol_(Orchard).svg.med.png</href>             <scale> 0.5 </scale></Icon> <color> 501400E6</color> </IconStyle>    </Style>  <Point><coordinates> ',allFields[i,"defaultLongitude"],',',allFields[i,"defaultLatitude"],',0</coordinates> </Point> </Placemark>',sep='')
		cat(addText,file=kmlFile,sep="\n")
		}


cat(kmlPolygon()$footer, file=kmlFile, sep="\n")
close(kmlFile)

csv_outputName = gsub(".kml$",".csv",stageEstimate_outputName)
write.csv(pinData[,1:9],csv_outputName,row.names=F)

#make these read from external

emailUser = ENV[["EMAIL_USER"]]
emailPassword = ENV[["EMAIL_PASSWORD"]]
fromAddr  = ENV[["FROM"]]
toAddr = ENV[["TO"]]
		
send.mail(from = fromAddr,
          to = toAddr,
          subject = paste("GDD Update for ",Sys.Date(),sep=""),
          body = "See attached.",
          html = TRUE,
          smtp = list(host.name = "email-smtp.us-east-1.amazonaws.com", port = 587, user.name = emailUser, passwd = emailPassword , ssl = TRUE),
          attach.files = c(stageEstimate_outputName,csv_outputName),
          authenticate = TRUE,
          send = TRUE)



dbDisconnect(db)


#make a kml output to just show all fields as well

#map this to csv list of gdd ranges