library(XML)
library(date)
#setwd("~/Desktop/landsat_test/cropSchedule")
url = "http://usda.mannlib.cornell.edu/MannUsda/viewDocumentInfo.do?documentID=1048"

dat = readLines(url)
#xmlDat = xmlTreeParse(url)
year = Sys.Date()
year = strsplit(as.character(year),"-")[[1]][1]
#year = c(2014)
fileElements = grep("fileElement",dat)
temp = dat[fileElements]
for(x in temp[grep(paste(year,".zip",sep=""),temp)]){
	Sys.sleep(5)
	#x = temp[grep("2014.zip",temp)][1]
	match = gregexpr("http://.*?zip?",x)
	downloadLink = substr(x,match[[1]],match[[1]]+attributes(match[[1]])$match.length)
   
   	#save this to file tracker, if it exists, stop
   	if(file.exists(paste("cropSchedule/",year,"/",sep=""))==F) dir.create(paste("cropSchedule/",year,"/",sep=""),recursive=T)
   	checkHistory = tryCatch(read.csv(paste("cropSchedule/",year,"/pastScrapes.csv",sep=""),stringsAsFactors=F),error = function(e) e)
   	if(inherits(checkHistory,"error")){
   		checkHistory = matrix(ncol=1,"")
   	}else{
   		
   		}
   	checkHistory = as.matrix(checkHistory,ncol=1)
   	if(downloadLink %in% checkHistory) next
   	checkHistory = rbind(checkHistory,downloadLink)
   	checkHistory = checkHistory[duplicated(checkHistory)==F,]
   	checkHistory = checkHistory[is.na(checkHistory)==F]

   	
   	write.csv(checkHistory,paste("cropSchedule/",year,"/pastScrapes.csv",sep=""),row.names=F)
   	
    download.file(downloadLink,"temp.zip")
    unzip("temp.zip",exdir="tempFolder")
    unlink("temp.zip")
    
    #list all csvs
    csvs = list.files("tempFolder",pattern=".csv$")
    for(i in csvs){
    	 if(i == "prog_all_tables.csv") next

    	 cropDat = read.csv(paste("tempFolder/",i,sep=""),stringsAsFactors=F,header=F)
    	 test = cropDat[2,3]
    	 test=gsub("\\\x96","",test)
    	 #Encoding(test) = "UTF-8"
    	 if(grepl("Harvested|Emerged",test)==F) next
	 
	 #stop("got it")
	 	 type = gsub("  Selected States","",test)
	 	 crop = gsub("Harvested|Emerged","",type)
	 	 crop = gsub("^ *| *$","",crop)
	 	 
	 	 activity =  gregexpr("Harvested|Emerged",type)
	 	 activityStart = activity[[1]][1]
	 	 stop = activityStart+attributes(activity[[1]])$match.length
		 activity = substr(type,activityStart,stop)
	 
    	 dataLines = grep("d",cropDat[,2])
    	 output = cbind(cropDat[dataLines,3],crop,activity,cropDat[dataLines,6])
    	 blanks = which(output[,1]=="")
    	 if(length(blanks)>0){
    	 	output = output[1:(blanks-1),]
    	 }
    	 aggregates = grep("^[0-9]",output[,1])
    	 if(length(aggregates)>0) output = ouput[-aggregates,]
    	  
    	 if(length(blanks)>0){
    	 	output = output[1:(blanks-1),]
    	 }
    	 tDate = paste(cropDat[dataLines[1]-4,6],cropDat[dataLines[1]-3,6])
    	 doy = strptime(as.date(tDate), "%d %b %Y")$yday+1
    	 #if(doy == 229) stop(i)
    	 #need to calculate this DOY
    	 
    	 output[,4] = gsub("[^0-9]","0",output[,4])
    	 colnames(output) = c("State","Crop","Activity","Doy")
    	 if(any(as.numeric(output[,4])>50)){
    	 		scheduleDat = tryCatch(read.csv(paste("cropSchedule/",year,"/cropSchedule.csv",sep=""),stringsAsFactors=F),error = function(e) e)
    	 		if(inherits(scheduleDat,"error")){
    	 			scheduleDat = c()
    	 			
    	 		}

    	 	#take the state and type for each match over 50%, compare the the csv of that crop type, look for the state and type match, if they aren't in the table yet, add appropriate row or column and the number, if the row and column do exist, and no value exists, update the cell with this value, after done for all >50% matches, save that csv
    	 	selects = which(as.numeric(output[,4])>50)
    	 	for(k in selects){
    	 	    	 		
    	 		workDat = output[k,]
    	 		update = which(scheduleDat[,1]==as.character(workDat[1]) & scheduleDat[,2]==as.character(workDat[2]) & scheduleDat[,3]==as.character(workDat[3]) & scheduleDat[,5]== year)
    	 	   if(length(update)==1){
    	 	   		if(as.numeric(scheduleDat[update,4])>doy){

    	 			scheduleDat[update,] = c(workDat[1:3],doy,year)
    	 			}
    	 		}else{
    	 			scheduleDat = rbind(scheduleDat,c(workDat[1:3],doy,year))
    	 			
    	 		}
    	 	}
    	 	
    	 	colnames(scheduleDat) = c("State","Crop","Activity","Doy","Year")
    	 	write.csv(scheduleDat,paste("cropSchedule/",year,"/cropSchedule.csv",sep=""),row.names=F)	
    	 	
    	 }
    	 #need to convert to day of year
	
    }
    
    #remove old folder
    files = list.files("tempFolder")
    for(z in files){
    	unlink(paste("tempFolder/",z,sep=""))
    }
    file.remove("tempFolder")
    
}