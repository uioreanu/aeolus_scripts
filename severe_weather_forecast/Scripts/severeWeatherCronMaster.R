#this is how we are going to lock cron tabs
#http://www.adminschoice.com/crontab-quick-reference
#in your crontab -e put an entry like this
#* 18 * * * Rscript ~/Desktop/cronTaskMasterForWeather_wLockFile.R >/dev/null 2>&1   #this one will run once a day at 6pm pacific time
#where cronTaskMasterForWeather_wLockfile.R is this script, that does the lock file checking and runs the appropriate scripts
#to disable a cron task, just go in the editor and put a pund sign # before the entry
#for every script that gets called by the cron task, wrap it in this

#perhaps also set MAILTO="" in the crontask tab

#use export EDITOR=nano in terminal to make that default editor for doing crontab -e

#50,51,52,53 * * * * Rscript ~/Desktop/crontasking/lockFileProcedure.R >/dev/null 2>&1  

#50 10 * * * Rscript ~/Desktop/severeWeatherOutlook/Scripts/severeWeatherCronMaster.R >/dev/null 2>&1  # this will run every day at 10:50 am for now..



#maybe a tryCatch around this whole thing?...
#judge available memory as well?
###IMPORTANT###NEED TO SPECIFY CURRENT FOLDER FOR EACH SCRIPT, wont use relative to this script location unless you cd to that location as part of the cron task itself (seperate command line commands with &&)
homeDir = "~/Desktop/aeolus_scripts/severe_weather_forecast"
setwd(homeDir)
startingCronTaskDir = getwd()
list.of.packages = c("rjson")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages)) install.packages(new.packages,repos="http://cran.rstudio.com/")
library(rjson)

cat(paste(startingCronTaskDir,"\n",sep=""),file=paste(homeDir,"/log.txt",sep=""))

lockCheck = system("mkdir lockfileFolder")
cat(paste("check value of folder: ",lockCheck,"\n",sep=""),file=paste(homeDir,"/log.txt",sep=""),append=T)

response = system("df -k",intern=T)
freeSpace = as.numeric(strsplit(response[2]," +")[[1]][4])

if(lockCheck ==0 & freeSpace>10000){ #not another one of these tasks running and have 20gb free  #probably make custom limit for each type of cron script
	#run the actual script
	#and actually, check for at least 20gb of disk space here as well
	#maybe write a textfile in the lockfileFolder saying when the task began
	#system("rm lockfileFolder")
	cat(paste("starting: ",Sys.time(),"\n",sep=""),file=paste(homeDir,"/log.txt",sep=""),append=T)
	
	result=T
	result = tryCatch(source("Scripts/noaa_weather_risk.R"),error=function(e) e)  #this would be the true master script here, any error would get filtered here
	cat(paste("finished, removing lockfile: ",Sys.time(),"\n",sep=""),file=paste(homeDir,"/log.txt",sep=""),append=T)

	#only remove lockfile if runs successfully
	#make set back to starting directort
	if(inherits(result,"error")==F){
		setwd(homeDir )
		unlink("lockfileFolder",recursive=T)	
	#within corn task, if the cront ask is updating records in database that it used for an input, then make sure those records are the same before they get updated 
	}else{
		setwd(homeDir)
		errorText = paste("cron task call script had an error, blocking future tasks until resolved: ",Sys.time(),"\n",sep="")
		vars = fromJSON(file="environment_variables.json")
		emailUser = vars[["EMAIL_USER"]]
		emailPassword = vars[["EMAIL_PASSWORD"]]
		fromAddr= vars[["FROM"]]
		toAddr= vars[["TO"]]

		cat(errorText ,file=paste(homeDir,"/log.txt",sep=""),append=T)
		sendEmail(fromAdd, toAddr,emailUser,emailPassword ,"Severe Weather Outlook Cron Task Error During Processing",errorText)
	
		cat(paste("cron task call script had an error, blocking future tasks until resolved: ",Sys.time(),"\n",sep=""),file=paste(homeDir,"/log.txt",sep=""),append=T)

		}
}else{
	
	setwd(startingCronTaskDir)
	if(lockCheck !=0){
		cat(paste("failed because of lockfile: ",Sys.time(),"\n",sep=""),file=paste(homeDir,"/log.txt",sep=""),append=T)
	}else{
		cat(paste("failed because of inadequate disk space: ",Sys.time(),"\n",sep=""),file=paste(homeDir,"/log.txt",sep=""),append=T)

		}
	#send an email saying it overan
	#system("rm lockfileFolder")
	}








#cron tasks
#hourly soil layers
#hourly elevation layers
#daily noaa weather alerts
#daily noaa gdd info/historical weather for new fields (need to work on making this a lot faster)
#potentially some sort of bi-hourly custom data formatting
#daily landsat download and layer addition
#daily management zones..?
#daily flight schedules?



