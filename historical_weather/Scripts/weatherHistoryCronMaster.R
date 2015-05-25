
sendEmailError = function(errorText,errorSubject){
	
	  #errorText = paste("cron task call script had an error on uploading, blocking future tasks until resolved: ",Sys.time(),"\n",sep="")
	  vars = fromJSON(file="environment_variables.json")
	  emailUser = vars[["EMAIL_USER"]]
	  emailPassword = vars[["EMAIL_PASSWORD"]]
	  fromAddr= vars[["FROM"]]
	  toAddr= vars[["TO"]]


	  cat(errorText ,file=paste(homeDir,"/log.txt",sep=""),append=T)
	  sendEmail(fromAddr, toAddr,emailUser,emailPassword ,"Weather History Cron Task Error During Upload",errorText)
	
}


homeDir = "~/Desktop/aeolus_scripts/historical_weather"  #dont end this with a slash
setwd(homeDir)
startingCronTaskDir = getwd()
source("Scripts/send_email_mavrx.R")
list.of.packages = c("rjson")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages)) install.packages(new.packages,repos="http://cran.rstudio.com/")
library(rjson)

cat(paste(startingCronTaskDir,"\n",sep=""),file=paste(homeDir,"/log.txt",sep=""))

lockCheck = system("mkdir lockfileFolder")
cat(paste("check value of folder: ",lockCheck,"\n",sep=""),file=paste(homeDir,"/log.txt",sep=""),append=T)

response = system("df -k",intern=T)
freeSpace = as.numeric(strsplit(response[2]," +")[[1]][4])

startProcessingDate = Sys.Date()

if(lockCheck ==0 & freeSpace>10000){ #not another one of these tasks running and have 20gb free  #probably make custom limit for each type of cron script
	#run the actual script
	#and actually, check for at least 20gb of disk space here as well
	#maybe write a textfile in the lockfileFolder saying when the task began
	#system("rm lockfileFolder")
	cat(paste("starting: ",Sys.time(),"\n",sep=""),file=paste(homeDir,"/log.txt",sep=""),append=T)
	
	result=T
	result = tryCatch(source("Scripts/GDD_ANALYTICS-SpeedTest_v2.R"),error=function(e) e)  #this would be the true master script here, any error would get filtered here
    
	#only remove lockfile if runs successfully
	#make set back to starting directort
	if(inherits(result,"error")==F){
		setwd(homeDir )
		cat(paste("main task finished succesfully, moving to upload result ",Sys.time(),"\n",sep=""),file=paste(homeDir,"/log.txt",sep=""),append=T)

		resultUpload = tryCatch(source("Scripts/uploadCSV_toDB.R"),error=function(e) e)  #this would be the true master script here, any error would get filtered here
		if(inherits(resultUpload,"error")==F){
		  cat(paste("finished, removing lockfile: ",Sys.time(),"\n",sep=""),file=paste(homeDir,"/log.txt",sep=""),append=T)
		  
		  setwd(homeDir)
		  unlink("lockfileFolder",recursive=T)	

		  if(T){
		  	source("../gddGetter/Scripts/gdd_getter.R")
		  	calculateAndEmailGDDProgress = tryCatch(runGDD(startProcessingDate),error=function(e) e)
		  	setwd(homeDir)

		  	if(inherits(calculateAndEmailGDDProgress,"error")==F){
				cat(paste("finished, removing lockfile: ",Sys.time(),"\n",sep=""),file=paste(homeDir,"/log.txt",sep=""),append=T)
		  		unlink("lockfileFolder",recursive=T)	

		  	}else{
		  		errorText = paste("Cron task failed during gdd update: ",Sys.time(),"\n",sep="")
		 		errorSubject = "GDD calculator error"
				  
		  		sendEmailError( errorText,errorSubject) 
 				cat(paste("error with gdd progress update: ",Sys.time(),"\n",sep=""),file=paste(homeDir,"/log.txt",sep=""),append=T)
		  	}
		 }
		}else{
		  setwd(homeDir)
		  errorText = paste("Cron task failed during upload of csv to weather db: ",Sys.time(),"\n",sep="")
		  errorSubject = "Historical weather upload error"
				  
		  sendEmailError( errorText,errorSubject) 
	
	
		  cat(errorText ,file=paste(homeDir,"/log.txt",sep=""),append=T)	
		  cat(paste("cron task call script had an error on csv upload, blocking future tasks until resolved: ",Sys.time(),"\n",sep=""),file=paste(homeDir,"/log.txt",sep=""),append=T)
		  
		}
	#within corn task, if the cront ask is updating records in database that it used for an input, then make sure those records are the same before they get updated 
	}else{
		  setwd(homeDir)
		  errorText = paste("cron task main call script had an error, blocking future tasks until resolved: ",Sys.time(),"\n",sep="")
		  errorSubject = "Weather History Cron Task Error During Processing"
				  
		  sendEmailError( errorText,errorSubject) 


		  cat(errorText ,file=paste(homeDir,"/log.txt",sep=""),append=T)
		  cat(paste("cron task call script had an error, blocking future tasks until resolved: ",Sys.time(),"\n",sep=""),file=paste(homeDir,"/log.txt",sep=""),append=T)
		}
}else{
	
	setwd(homeDir)
	if(lockCheck !=0){
		cat(paste("failed because of lockfile: ",Sys.time(),"\n",sep=""),file=paste(homeDir,"/log.txt",sep=""),append=T)
	}else{
		cat(paste("failed because of inadequate disk space: ",Sys.time(),"\n",sep=""),file=paste(homeDir,"/log.txt",sep=""),append=T)

		}
	#send an email saying it overan
	#system("rm lockfileFolder")
	}




