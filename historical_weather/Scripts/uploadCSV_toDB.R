list.of.packages = c("rjson","RMySQL")
new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

homeDir = "~/Desktop/aeolus_scripts/historical_weather"
setwd(homeDir)
library(RMySQL)
library(rjson)

  
ENV = fromJSON(file="environment_variables.json")
dbPass = ENV[["PASSWD"]]
dbUsr = ENV[["USER"]]
dbName =  ENV[["DB"]]
dbHost = ENV[["HOST"]]

db = dbConnect(MySQL(), user=dbUsr ,password=dbPass ,dbname=dbName,host=dbHost)


 setwd(paste(homeDir,"/weatherCSV",sep=""))

 allFiles = list.files()
 finishedDates = c()
 query = "SELECT fieldID FROM fields"
 rs = dbSendQuery(db,query)
 fieldIDsTrue =  fetch(rs, n=-1)
 for(eachFile in allFiles){
   dat = read.csv(eachFile,stringsAsFactor=F)
   theseDates = unique(dat[,"date"])
    
   if(any(theseDates %in% finishedDates)){
     print(paste("skipping ",eachFile," ",theseDates[1],sep=""))
     next
   }
    
   removeOldFields = c()
   for(i in 1:nrow(dat)){
     if(all(dat[i,2] %in% fieldIDsTrue[,1])==F)  removeOldFields =unique(c(removeOldFields,i))
   }
   if(length(removeOldFields)) dat = dat[-removeOldFields,]
   dat[,"date"] = as.Date(dat[,"date"])
   write.csv(dat,eachFile,row.names=F)
    
    
   finishedDates = unique(c(finishedDates,theseDates))
   query = paste("LOAD DATA LOCAL INFILE '",homeDir,"/weatherCSV/",eachFile,"' INTO TABLE weatherdata FIELDS TERMINATED BY ',' LINES TERMINATED BY '\n' IGNORE 1 LINES SET id = NULL",sep="")
    
    rs = dbSendQuery(db, query)
    write.csv(finishedDates,paste(homeDir,"/finishedDates.txt",sep=""),row.names=F)
    unlink(eachFile)
 }
  
