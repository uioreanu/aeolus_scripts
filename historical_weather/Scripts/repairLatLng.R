repairLatLng = function(dbPass,dbUsr,dbName,dbHost ){
	
	list.of.packages = c("RMySQL","rgeos","sp")
	new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
	if(length(new.packages)) install.packages(new.packages,repos="http://cran.rstudio.com/")
	
	library(RMySQL)
	library(rgeos)
	library(sp)
	
	db = dbConnect(MySQL(), user=dbUsr ,password=dbPass ,dbname=dbName,host=dbHost)
	
	query = paste("SELECT fieldID FROM fields WHERE fieldBlockGeometry IS NOT NULL AND defaultLatitude IS NULL")
	rs = dbSendQuery(db, query)
	fields = fetch(rs, n=-1)
	
	if(nrow(fields)>0){
		for(i in 1:nrow(fields)){
			eachField = fields[i,]
			query = paste("SELECT fieldID,AsText(fieldBlockGeometry) as geom from fields where fieldID ='",eachField,"'",sep="")
			rs = dbSendQuery(db, query)
			thisField = fetch(rs, n=-1)
			geom = thisField[,"geom"]
			dat = readWKT(geom)
			center = gCentroid(dat)@coords
			defaultLngVal = center[1]
			defaultLatVal = center[2]
			
			updateStatement = paste("UPDATE fields set defaultLatitude = '",defaultLatVal,"', defaultLongitude=' ",defaultLngVal,"' WHERE fieldID ='",eachField,"' LIMIT 1",sep="")
			rs = dbSendQuery(db, updateStatement )
			
		}
	}
	
	dbDisconnect(db)

}