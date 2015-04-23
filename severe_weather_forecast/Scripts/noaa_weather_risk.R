homeDir = "~/Desktop/aeolus_scripts/severe_weather_forecast"
setwd(homeDir )

list.of.packages = c("sp","rgdal","rgeos","RMySQL")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages,repos="http://cran.rstudio.com/")

library(sp)
library(rgdal)
library(rgeos)
library(RMySQL)

#http://www.spc.noaa.gov/
#shapefiles are at:
#http://www.spc.noaa.gov/products/outlook/archive/[year]/day1otlk_[yearmonthday]_2000-shp.zip

counties = readOGR(dsn="Input/tl_2014_us_state",layer="tl_2014_us_county")
counties = spTransform(counties,CRS("+proj=longlat +datum=WGS84"))
currentDate = gsub("-","",Sys.Date())
currentYear = strsplit(as.character(Sys.Date()),"-")[[1]][1]

#can make these more sophistacted by scraping http://www.spc.noaa.gov/products/outlook/archive/2015/
getMostRecent = readLines("http://www.spc.noaa.gov/products/outlook/archive/2015/")
tagStripped = gsub("<[^>]*>","",getMostRecent)
urlTypes = rbind(c("day1otlk",1700),c("day2otlk",1700),c("day3otlk","0730"))

for(urlType in 1:nrow(urlTypes)){
	urlMatches = tagStripped[grep(paste(urlTypes[urlType,1],"_",currentDate,".*-shp.zip",sep=""),tagStripped)]
	urlMatches = urlMatches[length(urlMatches)]
	timeUpdated = strsplit(urlMatches,"-shp.zip")[[1]][1]
	timeUpdated = strsplit(timeUpdated,"_")[[1]]
	timeUpdated = timeUpdated[length(timeUpdated)]
	urlTypes[urlType,2] = timeUpdated
	}
#urlTypes = rbind(c("day1otlk",1700),c("day2otlk",1700),c("day3otlk","0730"))
#need to make/get a comprehensive legend to interpret the DN values from these shapefiles
#one day outlook is broken down into "category" and the hail,wind,tornade (and signigicant categories there have values just in terms of % probability)
#2,3 day outlooks etc have just the category level and the probability of sever weather event as a probability (so tornade,hail,wind aren't disaggregated yet)

category_legend = cbind(c(2:7),c("Thunderstorm","Marginal Severe Weather","Slight Severe Weather","Enhanced Severe Weather","Moderate Severe Weather","High Severe Weather"))
rownames(category_legend) = category_legend[,1]
allURLs = list()
for(urlType in 1:nrow(urlTypes)){
	
	url = paste("http://www.spc.noaa.gov/products/outlook/archive/",currentYear,"/",urlTypes[urlType,1],"_",currentDate,"_",urlTypes[urlType,2],"-shp.zip",sep="")
	#url = paste("http://www.spc.noaa.gov/products/outlook/archive/",currentYear,"/day2otlk_",currentDate,"_1700-shp.zip",sep="")
	#clear everything in input folder
	download.file(url,destfile=paste("Input/Zips/",urlTypes[urlType,1],".zip",sep=""))
	#unzip it 
	unlink("Input/Unzips/*",recursive=T)
	unzip(paste("Input/Zips/",urlTypes[urlType,1],".zip",sep=""),exdir="Input/Unzips")
	#look in the folder, for each data type, extract polygon object, conver to well known text (probably), look for which fields in database fall within
	
	allFiles = list.files("Input/Unzips",pattern="shp")
	#in categorical table _cat DN 2 = tstrm, 3 marginal, 4 slight, 5 enhanced, 6 moderate, 7 high?
	weatherList = list()
	for(eachFile in allFiles){
		weatherType = strsplit(eachFile,"_")[[1]]
		weatherType = gsub(".shp","",weatherType[length(weatherType)])
		dat = readOGR(dsn="Input/Unzips",layer=gsub(".shp","",eachFile))
		dat = spTransform(dat,CRS("+proj=longlat +datum=WGS84"))
		
		matches = as.data.frame(as.matrix(gIntersects(dat,counties,byid=T)))
		for(i in 1:ncol(matches)){
			updates = which(matches[,i]==T)
			if(length(updates)){
				matches[updates,i] = dat@data[i,"DN"]
			}
		}
		selects = which(apply(matches,1,function(x) any(x!=F))==T)
		
		if(length(selects)){
			countiesWithWeather = counties[selects,]
			temp = countiesWithWeather@data 
			if(ncol(matches)>1){
				temp1 = apply(matches[selects,],1,function(x) max(x,na.rm=T))
			}else{
				temp1 = matches[selects,]
				}
			
			if(weatherType=="cat") temp1 = category_legend[as.character(temp1),2]
		
			weatherList[[weatherType ]] = cbind(countiesWithWeather@data[,1:6],weatherVal=temp1)
		}
		#so would need to query current properties and all counties based on this..
		
		
	}
	allURLs[[urlTypes[urlType,1]]] = weatherList
	Sys.sleep(5)
}



#ok, now to map to our actual fields, this is pretty slow but whatever


currentDate = Sys.Date()

ENV = fromJSON(file="environment_variables.json")
dbPass = ENV[["PASSWD"]]
dbUsr = ENV[["USER"]]
dbName =  ENV[["DB"]]
dbHost = ENV[["HOST"]]

db = dbConnect(MySQL(), user=dbUsr ,password=dbPass ,dbname=dbName,host=dbHost)

query = paste("SELECT COUNT(*) from fields where fieldBlockGeometry != ''",sep="")  #just do a limit and offset here...  #really whose soil updated field is F
rs = dbSendQuery(db, query)
totalRowsToUpdate = as.numeric(fetch(rs, n=-1))
limitStep = 100
stepLoop = seq(0,totalRowsToUpdate,by=limitStep)
clientFieldWeatherEventList = list()
for(i in stepLoop){
	query = paste("SELECT fieldID,name,clientID,AsText(fieldBlockGeometry) As geom FROM fields  where fieldBlockGeometry != '' LIMIT ",limitStep," OFFSET ",i,sep="")  #just do a limit and offset here...
	rs = dbSendQuery(db, query)
	field_data = fetch(rs, n=-1)
	labelPoints = c()
	for(j in 1:nrow(field_data)){
		if(is.null(field_data[j,"geom"])) next
		tempGEOM = readWKT(field_data[j,"geom"])
		labelPoints  = rbind(labelPoints ,tempGEOM@polygons[[1]]@labpt)
		
		}
		labelPoints = SpatialPoints(labelPoints)
		proj4string(labelPoints) = "+proj=longlat +datum=WGS84"
		
		#proj4string(tempGEOM) = "+proj=longlat +datum=WGS84"
		matchCounty = over(labelPoints,counties)
		#matchCounty = gIntersects(tempGEOM,counties,byid=T)
		for(j in 1:nrow(matchCounty)){
		geoidMatch = as.character(matchCounty[j,"GEOID"]		)
		if(length(geoidMatch)<1) next
		if(is.na(geoidMatch)) next
		matches = c()
		for(dayAhead in 1:length(allURLs)){
		    forecastDate = currentDate + (dayAhead-1)

			for(weatherEvent in 1:length(allURLs[[dayAhead]])){
				select = grep(geoidMatch,allURLs[[dayAhead]][[weatherEvent]][,"GEOID"])
				if(length(select)){
					matches = rbind(matches,matrix(nrow=1,c(field_data[j,c("fieldID")],as.character(currentDate),names(allURLs[[dayAhead]])[[weatherEvent]],as.character(allURLs[[dayAhead]][[weatherEvent]][select,"weatherVal"]),as.character(forecastDate)))) #probably worth noting the airport, or really just doing this over in service airport locations and assuming similar weather for all fields in range
				}
			}
		}
		if(is.null(matches)==F){
			clientFieldWeatherEventList[[length(clientFieldWeatherEventList)+1]] = matches
		}
	}
	
}

fieldSummary= do.call("rbind",clientFieldWeatherEventList)
fieldSummary = cbind(id=NA,fieldSummary)
fieldSummary = as.data.frame(matrix(ncol=6,fieldSummary))

colnames(fieldSummary) = c("id","field_id","date","forecast_type","forecast_value","prediction_date")
fieldSummary[,"date"] = as.Date(fieldSummary[,"date"])
fieldSummary[,"prediction_date"] = as.Date(fieldSummary[,"prediction_date"])
fieldSummary[,"field_id"] = as.numeric(as.character(fieldSummary[,"field_id"]))
setwd(homeDir )

write.csv(fieldSummary,paste(homeDir,"/severWeatherOutlookByfield.csv",sep=""),row.names=F)

query = paste("LOAD DATA LOCAL INFILE '",homeDir,"/severWeatherOutlookByfield.csv' INTO TABLE weatherforecast FIELDS TERMINATED BY ',' LINES TERMINATED BY '\n' IGNORE 1 LINES SET id = NULL",sep="")
 
rs = dbSendQuery(db, query)

dbDisconnect(db)



#want the output to be a row for each field that has at least one weather event forecasts in next 3 days, and a column for every potential weather category and the weather value itself (if one exists)
