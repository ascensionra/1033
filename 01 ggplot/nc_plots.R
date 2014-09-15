library("ggplot2")
options(java.parameters="-Xmx2g")
library(rJava)
library(RJDBC)
library(maps)
library(Formula)
library(Hmisc)

jdbcDriver <- JDBC(driverClass="oracle.jdbc.OracleDriver", classPath="C:/Program Files/Java/jdk1.8.0_20/ojdbc6.jar")

# In the following, use your username and password instead of "CS347_prof", "orcl_prof" once you have an Oracle account
possibleError <- tryCatch(
  jdbcConnection <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@128.83.138.158:1521:orcl", "C##CS347_jsm3287", "orcl_jsm3287"),
  error=function(e) e
)
if(!inherits(possibleError, "error")){
  merged <- dbGetQuery(jdbcConnection, "select * from inventory, transactions where transactions.nsn = inventory.nsn and transactions.\"State\" <> 'PR' and transactions.\"State\" <> 'VI' and transactions.\"State\" <> 'DC' and transactions.\"State\" <> 'GU'")
  dbDisconnect(jdbcConnection)
}
states=map_data("state")
county=map_data("county")

#Color Slider
low <- rgb(255,204,153,255,maxColorValue=255)
high <- "red"


#USED THIS TO BUILD A HISTOGRAM WEIGHT DATA FRAME
h <- ggplot(data=merged) 
h <- h + geom_histogram(aes(x=State, fill=..count.., weight=Quantity*Acquisition_Cost))
hg <- ggplot_build(h)

#USED THIS TO CONVERT THE LIST TO A DATA FRAME 
sdata <- as.data.frame(hg$data[1])
#s <- ggplot(data=sdata) + geom_histogram(stat="identity",aes(x=group,fill=y,y=y))
#s

#USED THIS TO CAPITALIZE THE STATES COLUMN FOR ABBREVIATION MAKING
states$region <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", states$region, perl=TRUE)
#states$region

#USED THIS TO ABBREVIATE THE STATES COLUMN
states$abb <- state.abb[match(states$region,state.name)]

#USED THIS TO ADD ABBREVIATIONS TO SDATA
sdata$abb <- state.abb[order(state.abb)]


#USED THIS TO MERGE SDATA WITH STATES BY ABB
comb <- merge(states,sdata,by="abb")

#USED THIS TO SORT BY ORDER FOR PROPER DRAWING
attach(comb)
scomb <- comb[order(order),]
detach(comb)

#USED THIS TO PLOT THE TEMP MAP
t<- ggplot()
t<-t+geom_polygon(data=scomb,aes(x=long,y=lat,group=group.x,fill=count),color="black")
t + scale_fill_gradient("$ Spent", low = low, high = high)



###################COUNTY DATA#####################
#USED THIS TO CAPITALIZE COUNTY DATA
county$subregion <- toupper(county$subregion)

#USED THIS TO BUILD A HISTOGRAM WEIGHTED DATA FRAME BY COUNTY
h2 <- ggplot(data=merged) 
h2 <- h2 + geom_histogram(aes(x=County, fill=..count.., weight=Quantity*Acquisition_Cost))
hg2 <- ggplot_build(h2)

#USED THIS TO CONVERT THE LIST TO A DATA FRAME 
cdata <- as.data.frame(hg2$data[1])

#USED THIS TO ADD County COLUMN TO CDATA
countyList <- merged
attach(countyList)
counties <- countyList[order(County),]
detach(countyList)
counties <- unique(counties$County)

cdata$County <- counties

#USED THIS TO RENAME subregion to County
names(county)[names(county)=="subregion"] <- "County"

#USED THIS TO MERGE SDATA WITH STATES BY ABB
comb <- merge(county,cdata,by="County")

#USED THIS TO SORT BY ORDER FOR PROPER DRAWING
attach(comb)
scomb <- comb[order(order),]
detach(comb)

#USED THIS TO PLOT THE TEMP MAP
t2<- ggplot()
t2 <-t2 +geom_polygon(data=states,aes(x=long,y=lat,group=group),fill=low)
t2<-t2+geom_polygon(data=scomb,aes(x=long,y=lat,group=group.x,fill=count),color="grey30")
t2 <-t2 +geom_polygon(data=states,aes(x=long,y=lat,group=group),fill=rgb(0,0,0,0),color="black")
t2 + scale_fill_gradient("$ Spent", low = low, high = high)

## RUN ABOVE






