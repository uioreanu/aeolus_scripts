getCloudForecast = function(db,allOut,chooseRow,currentDate,limitedForecast,weatherNodeMapping=NULL){
	
	#if the predict date is within 4 days of current, then do this, otherwise, do not
	if(allOut[chooseRow,"fieldID"]==""){
		return(rep("",times=15))
		next
	}
	if(limitedForecast==T){
		targetDate =as.Date(allOut[chooseRow,"Predicted Date of Imaging Event"])
	}else{
		targetDate = currentDate
		}
		
		
	if((as.Date(targetDate)-currentDate)> 4 | (as.Date(targetDate)-currentDate)<0) {
		return(rep("",times=15))
		next
	}
	if(is.null(weatherNodeMapping)==F){
		latLngs = matrix(nrow=1,weatherNodeMapping[chooseRow,])
		colnames(latLngs) = colnames(weatherNodeMapping)
		latLngs[1,1] = as.integer(latLngs[1,1])

	}else{
		query = paste("Select defaultLongitude,defaultLatitude from fields where fieldID ='",allOut[chooseRow,"fieldID"],"'",sep="")
		rs = dbSendQuery(db, query)
		latLngs = fetch(rs, n=-1)
	}
	
	#find the date for the given field somehow, prb put this in gddGetter
	#for each field ID, find the defaultLng/defaultLat, use that to query this webpage
	doc = htmlParse(readLines(paste("http://forecast.weather.gov/MapClick.php?lat=",round(latLngs[1,"defaultLatitude"],5),"&lon=",round(latLngs[1,"defaultLongitude"],5),"&FcstType=digitalDWML",sep="")))
	tableNodes = getNodeSet(doc, "//time-layout //start-valid-time")
	dates = unlist(lapply(tableNodes,function(x) strftime(xmlValue(x))))
	times = unlist(lapply(tableNodes,function(x) strsplit(as.character(strptime(strsplit(as.character(xmlValue(x)),"T")[[1]][2],format="%H:%M:%S"))," ")[[1]][2]))
	if(any(is.na(times))){
		times[which(is.na(times)==T)] = "00:00:00"
	}
	tableNodes = getNodeSet(doc, "//cloud-amount //value")
	clouds = unlist(lapply(tableNodes,function(x) xmlValue(x)))
	
	temperature = getNodeSet(doc, "//temperature //value")
	temperature  = unlist(lapply(	temperature ,function(x) (5/9*(as.numeric(xmlValue(x))-32))))
	
	selectFutureDates = which(as.Date(dates)>Sys.Date())
	stepSeq = seq(selectFutureDates[1],length(dates),by=24)
	
	dailyMinMax = c()
	for(zz in 2:length(stepSeq)){
		tempTemp = temperature[stepSeq[zz-1]:stepSeq[zz]]
		dailyMinMax = rbind(dailyMinMax,c(zz,allOut[chooseRow,"fieldID"],unique(dates[stepSeq[zz-1]])[1],max(tempTemp,na.rm=T),min(tempTemp,na.rm=T),0))
	}
	dailyMinMax = as.data.frame(matrix(ncol=6,dailyMinMax))
	colnames(dailyMinMax) = c("id","field_id","date","t_max","t_min","precip")
	
	output = matrix(nrow=length(dates),ncol=3)
	output[,1] = dates
	output[,2] = times
	output[,3] = clouds
	
	dayOffsetIndexList = c(-1,0,1,2,3)
	
	tempOutput = c()
	for(offsetAmount in dayOffsetIndexList){
		
		dayOffsetIndex = which(as.Date(output[,1])-targetDate == offsetAmount)
		temp = output[dayOffsetIndex,]
		if(length(temp)){
			morning = which(temp[,2] %in% c("08:00:00","09:00:00","10:00:00","11:00:00"))
			midday = which(temp[,2] %in% c("12:00:00","13:00:00","14:00:00"))
			afternoon = which(temp[,2] %in% c("15:00:00","16:00:00","17:00:00"))
			morning = as.integer(mean(as.numeric(temp[morning,3]),is.na=T))
			midday = as.integer(mean(as.numeric(temp[midday,3]),is.na=T))
			afternoon = as.integer(mean(as.numeric(temp[afternoon,3]),is.na=T))
			 	tempOutput = c(tempOutput,morning,midday,afternoon)
		}else{
				tempOutput = c(tempOutput,NA,NA,NA)
		}
	}
	
	Sys.sleep(2)
	return(list("clouds"=tempOutput,"minMaxPredict"=dailyMinMax))
}

#if cloud cover forecast appears on the target date
