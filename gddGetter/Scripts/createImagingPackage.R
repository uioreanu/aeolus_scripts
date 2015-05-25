#populate client -> field -> field imaging package list

#list all clients
#set each client to 3 images, and 6 for a few exceptions
#come up with a package type to event name sceme

list.of.packages = c("devtools","rjson","RMySQL")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages,repos="http://cran.rstudio.com/")
library(devtools)

list.of.packages = c("RGoogleDocs")
if(length(new.packages)) {
	install_github("RGoogleDocs", "duncantl")
}

library(RGoogleDocs)
library(rjson)	
library(RMySQL)

ENV = fromJSON(file="environment_variables.json")
driveAuthUser  = ENV[["FROM"]]    
driveAuthSecret = ENV[["DRIVE"]]     

db = dbConnect(MySQL(), user=ENV[["USER"]],password=ENV[["PASSWD"]],dbname=ENV[["DB"]],host=ENV[["HOST"]])

query = paste("Select name,clientID from clients")
rs = dbSendQuery(db, query)
data= fetch(rs, n=-1)


allPackages = list()
allPackages[["6-Image"]] =  c("First Vegetative","Second Vegetative","Third Vegetative","Fourth Vegetative","Fifth Vegetative","Sixth Vegetative")
allPackages[["3-Image"]] =  c("First Vegetative","Third Vegetative","Fifth Vegetative")
allPackages[["Stand-Count-Special"]] = c("Stand Count","Third Vegetative","Fifth Vegetative")

con = getGoogleDocsConnection(getGoogleAuth(driveAuthUser, driveAuthSecret ))
docs = getDocs(con)

trickShit = getWorksheets("Trick Shit (Field Exception Imaging Package)", con = getGoogleDocsConnection(getGoogleAuth(driveAuthUser, driveAuthSecret ,"wise")))
allSheets = names(trickShit )
#then split out GDDs, and find the most recent date available
#get the most recent, assuming the formatting is always month-day fro the current year
firstSheet = allSheets[1]
trickShit = sheetAsMatrix(trickShit [[firstSheet]], header = TRUE, as.data.frame = T, trim = TRUE,stringsAsFactors=F)

allImagingRuns = c()
for(eachClient in 1:nrow(data)){
	
	query = paste("Select fieldID,name from fields where planting_date != 'NULL' AND is_active = '1' AND clientID ='",data[eachClient,2],"'",sep="")
	rs = dbSendQuery(db, query)
	fields= fetch(rs, n=-1)
	#this package type should be user specific--or field specific for some users, christ I dont know
	packageType = allPackages[["3-Image"]]	
	if(data[eachClient,2] %in% c(74:89)) packageType = allPackages[["Stand-Count-Special"]]
	if(nrow(fields)<1) next
	for(k in 1:nrow(fields)){
		thisPackageType = packageType
		if(fields[k,"fieldID"] %in% trickShit[,"fieldID"]){
			thisPackageType = allPackages[["6-Image"]]
		}
		allImagingRuns  = rbind(allImagingRuns,cbind(fields[k,],thisPackageType))
	}
}
colnames(allImagingRuns) = c("fieldID","name","packageType")
#review trick shit table here and update based on that
writeName = paste("ImagingPackagesLookup/ClientPackagesSaveForUpload_",as.numeric(Sys.time()),".csv",sep="")
write.csv(allImagingRuns,writeName,row.names=F)

con = getGoogleDocsConnection(getGoogleAuth(driveAuthUser, driveAuthSecret ))
docs = getDocs(con)

trickShit = getWorksheets("Trick Shit (Field Exception Imaging Package)", con = getGoogleDocsConnection(getGoogleAuth(driveAuthUser, driveAuthSecret ,"wise")))
#loopthrough trickShit and replace 3 image package with a 6imagePackage..


outputName = "ClientImagingPackages22"
if(is.null(docs[[outputName]])==F){
	
	#then download a copy and delete, then upload new one
	ts = getWorksheets(outputName, con = getGoogleDocsConnection(getGoogleAuth(driveAuthUser, driveAuthSecret ,"wise")))
	allSheets = names(ts)
	#then split out GDDs, and find the most recent date available
	#get the most recent, assuming the formatting is always month-day fro the current year
	firstSheet = allSheets[1]
	dat = sheetAsMatrix(ts[[firstSheet]], header = TRUE, as.data.frame = T, trim = TRUE,stringsAsFactors=F)
	write.csv(dat,paste("ImagingPackagesLookup/ClientPackagesSaveDownload_",as.numeric(Sys.time()),".csv",sep=""),row.names=F)
	removed = deleteDoc(outputName,con)
}

tmp <- uploadDoc(writeName , con, name = outputName, type = "csv",folder=I(docs[["Client Imaging Order Tracking"]]@content["src"]))
dbDisconnect(db)

#save this to a unique 
#so the gdd getter will reference ClientImagingPackages22 when writing out the GDD, and the pilot managment airport_field_gdd script will do that as well just in case


#NEED A RELIABLE WAY TO TRACK COMPLETED FIELDS, AND KNOW HOW TO SKIP THOSE IN AIRPORT_FIELD_GDD







