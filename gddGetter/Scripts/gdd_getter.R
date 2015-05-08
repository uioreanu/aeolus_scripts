
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


get_average_weather = function(weather,plantDate){
	thisYear = format(plantDate,"%Y")
	saveColNames = colnames(weather)
	weather[,"date"] = paste(format(as.Date(weather[,"date"]),"%m"),"-",format(as.Date(weather[,"date"]),"%d"),sep="")
	output = by(weather,weather[,"date"],function(x){
		return(as.data.frame(matrix(nrow=1,c(x[1,1],x[1,2],paste(thisYear,"-",x[1,3],sep=""),mean(x[,4],na.rm=T),mean(x[,5],na.rm=T),mean(x[,6],na.rm=T))),stringsAsFactors=F))
	
	})
	
	output = do.call("rbind",output)
	output[,4] = as.numeric(output[,4])
	output[,5] = as.numeric(output[,5])
	output[,6] = as.numeric(output[,6])
	output = output[which(as.Date(output[,3])>=plantDate),]
	colnames(output) = saveColNames
	return(output)
	
}

setwd("~/Desktop/aeolus_scripts/gddGetter")

cornDates = read.csv("Input/corn_imaging_periods.csv",stringsAsFactors=F)

ENV = fromJSON(file="environment_variables.json")
db = dbConnect(MySQL(), user=ENV[["USER"]],password=ENV[["PASSWD"]],dbname=ENV[["DB"]],host=ENV[["HOST"]])


query = paste("SELECT fieldID,name,defaultLatitude,defaultLongitude,planting_date from fields where is_active =1")
rs = dbSendQuery(db, query)
allFields = fetch(rs, n=-1)

fullOutput = c()
stageList = c()

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
	
	#stop("add here")
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
		
		query = paste("SELECT * from weatherdata where field_id = '",fieldID,"' AND date <= '",as.Date(paste(format(Sys.Date(),"%Y"),"-01-01",sep="")),"'",sep="")
		rs = dbSendQuery(db, query)
		weather = fetch(rs, n=-1)
		weather = get_average_weather(weather,as.Date(plantDat))
		weather = weather[which(weather[,3]>=Sys.Date()),]
		gdd_forecast_profile = gdd_calculate(weather,30,10)+totalGDD
		imagingAdditions = c()
		imageAdditionNames  = c()
		for(imagePeriod in 1:nrow(cornDates)){
			selectFirst = which(gdd_forecast_profile[,2]>cornDates[imagePeriod,1])[1]
			imageDate = as.Date(weather[selectFirst,3])
			daysTil = max(0,as.numeric(imageDate-Sys.Date()))
			imagingAdditions = c(imagingAdditions,cornDates[imagePeriod,3],as.character(imageDate),daysTil)
			imageAdditionNames = c(	imageAdditionNames,c("Stage",paste(cornDates[imagePeriod,3],"Date"),paste(cornDates[imagePeriod,3],"Days Til") ))
			
			if(length(stageList[[cornDates[imagePeriod,3]]])){
				stageList[[cornDates[imagePeriod,3]]] = rbind(stageList[[cornDates[imagePeriod,3]]],c(lat,lng,name,fieldID,daysTil,as.character(imageDate)))

			}else{
				stageList[[cornDates[imagePeriod,3]]] = c(lat,lng,name,fieldID,daysTil,as.character(imageDate))
				}
			
		}
		
		#find date ranges for corndat in all of these and days till
		
	
	
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
	
	tempOut = c(as.character(Sys.Date()),fieldID,name,lat,lng,plantDat,totalGDD,imagingAdditions)	
	fullOutput = rbind(fullOutput,tempOut)

}
if(nrow(fullOutput)<1) stop("No data")
colnames(fullOutput) = c("Estimate Date","fieldID","fieldName","lat","lng","plantDat","Estimated GDD",imageAdditionNames)#"Current Stage","Stage In Next 5 Days","Next Imaging Event Stage","Date of next Image Event","Days till Flight","Lat","Lng","Plant Date")


#subset of fullOutput goes to gdd table
if(F){
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
	
	
			
	send.mail(from = fromAddr,
	          to = toAddr,
	          subject = paste("GDD Update for ",Sys.Date(),sep=""),
	          body = "See attached.",
	          html = TRUE,
	          smtp = list(host.name = "smtp.sendgrid.net", port = 2525, user.name = emailUser, passwd = emailPassword , ssl = F),
	          attach.files = NULL,#c(stageEstimate_outputName,csv_outputName),
	          authenticate = TRUE,
	          send = TRUE)
	

}
dbDisconnect(db)




#have a list with each entry being a crop type/stage
#cycle through list, create folder with sunlist name
#determine color scheme for fields in the subfolder, with red being needs imaging soon, green being far away

KML_OutputName = paste("Output/All_Imaging_Periods_",as.character(Sys.Date()),".kml",sep="")

tf <- KML_OutputName 
kmlFile <- file(tf, "w")
cat(kmlPolygon(kmlname="All_Imaging_Periods")$header, file=kmlFile, sep="\n")

for(k in 1:length(stageList)){

	
	cat(paste("<Folder><name>",names(stageList)[k],"</name>",sep=""),   file=kmlFile, sep="\n")
	

	tempDat = stageList[[k]]
	tempDat = tempDat[order(as.numeric(tempDat[,5])),]
	tempDat = tempDat[is.na(tempDat[,5])==F,]
	if(nrow(tempDat)<1) next

	colors = brewer.pal(10,"RdYlGn")
	colorBins = seq(min(as.numeric(tempDat[,5])),max(as.numeric(tempDat[,5])),length.out = length(colors))
	colorsAdd = rep(0,times=nrow(tempDat))
	for(i in 2:length(colorBins)){
		indices = which(as.numeric(tempDat[,5])>colorBins[i-1] & as.numeric(tempDat[,5])<=colorBins[i] )
		if(i == 2) indices = c(indices,which(as.numeric(tempDat[,5])==min(as.numeric(tempDat[,5]))))
		if(length(indices)){
			colorsAdd[indices] = colors[i-1]
		}
	}
	if(any(tempDat[,5]=="0")){
		colorsAdd[which(tempDat[,5]=="0")] = "#000000"
	}
	colorsAdd =  paste(99,substr(colorsAdd,6,7),substr(colorsAdd,4,5),substr(colorsAdd,2,3),sep="") 
	tempDat = cbind(tempDat,colorsAdd)
	
	colnames(tempDat) = c("defaultLatitude","defaultLongitude","name","fieldID","dayTill","Imaging-Date","color")

	#attach colors
	
 for(i in 1:nrow(tempDat)){
		   addText = paste('
		  <Placemark> <name> ',paste(gsub("&","",tempDat[i,"name"]),": ",tempDat[i,"Imaging-Date"],sep=""),' </name> <Style id="Mavrx Airport">      <IconStyle>        <Icon>          <href>http://www.clker.com/cliparts/4/5/c/0/12249615002008292168Japanese_Map_symbol_(Orchard).svg.med.png</href>             <scale> 0.5 </scale></Icon> <color>', tempDat[i,"color"],'</color> </IconStyle>    </Style>  <Point><coordinates> ',tempDat[i,"defaultLongitude"],',',tempDat[i,"defaultLatitude"],',0</coordinates> </Point> </Placemark>',sep='')
		cat(addText,file=kmlFile,sep="\n")
		}
	
		cat("</Folder>",   file=kmlFile, sep="\n")
		
		csvOut = write.csv(tempDat[,c("name","fieldID","dayTill","Imaging-Date")],paste("Output/",names(stageList)[k],"_",Sys.Date(),".csv",sep=""),row.names=F)

	
	
	
}

cat(kmlPolygon()$footer, file=kmlFile, sep="\n")
close(kmlFile)


allOut = c()
for(z in 1:length(stageList)){
	temp = stageList[[z]][,3:4]
	plantings = c()
	for(i in 1:nrow(temp)){
		plantings = c(plantings,fullOutput[which(fullOutput[,2]==temp[i,2]),"plantDat"])
		
		
	}
	daysFromPlant = as.Date(stageList[[z]][,6])-as.Date(plantings)
	temp = cbind(temp,plantings,daysFromPlant,stageList[[z]][,5:6],
names(stageList)[z])
  temp = temp[order(as.numeric(temp[,5])),]
	
	allOut = rbind(allOut,temp,"")
}


colnames(allOut) = c("Field Name","Field ID","Plant Date","Days from Plant to Imaging Period","Days from Current Day until Imaging event","Predicted Date of Imaging Event","Imaging Event Name")

allDatesCSV_Name = paste("Output/allFieldsDates_",Sys.Date(),".csv",sep="")
write.csv(allOut,allDatesCSV_Name ,row.names=F)


emailUser = ENV[["EMAIL_USER"]]
emailPassword = ENV[["EMAIL_PASSWORD"]]
fromAddr  = ENV[["FROM"]]
toAddr = ENV[["TO"]]

send.mail(from = fromAddr,
          to = toAddr,
          subject = paste("GDD Update for ",Sys.Date(),sep=""),
          body = "Autogenerated Report on Estimated Crop Stages. See attached.",
          html = TRUE,
          smtp = list(host.name = "smtp.sendgrid.net", port = 2525, user.name = emailUser, passwd = emailPassword , ssl = F),
          attach.files = c(KML_OutputName ,allDatesCSV_Name),
          authenticate = TRUE,
          send = TRUE)
          

#upload to google docs
list.of.packages = c("devtools")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages,repos="http://cran.rstudio.com/")
library(devtools)

list.of.packages = c("RGoogleDocs")
if(length(new.packages)) {
	install_github("RGoogleDocs", "duncantl")
}
library(RGoogleDocs)
          
          
driveAuthUser  = ENV[["FROM"]]    
driveAuthSecret = ENV[["DRIVE"]]        
 
 con = getGoogleDocsConnection(getGoogleAuth(driveAuthUser , driveAuthSecret ))
 docs = getDocs(con)
 docName = paste("GDDs_",as.character(Sys.Date()),sep="")
 tmp <- uploadDoc(allDatesCSV_Name, con, name =  docName, type = "csv",folder=I(docs$GDDs@content["src"]))
          
          
 #now send to google drive
 
          



#make a kml output to just show all fields as well

#map this to csv list of gdd ranges