list.of.packages = c("mailR")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages,repos="http://cran.rstudio.com/")
	
library(mailR)
	
sendEmail = function(toAddr,fromAddr,username,password,subjectText,bodyText){
	send.mail(from = toAddr,
	          to = c(fromAddr),
	          subject = subjectText,
	          body = bodyText,
	          html = TRUE,
	          smtp = list(host.name = "email-smtp.us-east-1.amazonaws.com", port = 587, user.name = username, passwd = password, ssl = TRUE),
	          attach.files = stageEstimate_outputName,
	          authenticate = TRUE,
	          send = TRUE)
}

