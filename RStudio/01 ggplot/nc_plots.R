library("ggplot2")
options(java.parameters="-Xmx2g")
library(rJava)
library(RJDBC)
library(maps)
library(Formula)
library(Hmisc)
library(wordcloud)
library(RColorBrewer)

jdbcDriver <- JDBC(driverClass="oracle.jdbc.OracleDriver", classPath="C:/Program Files/Java/jdk1.8.0_20/ojdbc6.jar")

# In the following, use your username and password instead of "CS347_prof", "orcl_prof" once you have an Oracle account
possibleError <- tryCatch(
  jdbcConnection <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@128.83.138.158:1521:orcl", "C##CS347_jsm3287", "orcl_jsm3287"),
  error=function(e) e
)
if(!inherits(possibleError, "error")){
  alias <- dbGetQuery(jdbcConnection, "select * from aliases")
  merged <- dbGetQuery(jdbcConnection, "select * from inventory, transactions where transactions.nsn = inventory.nsn and transactions.\"State\" <> 'PR' and transactions.\"State\" <> 'VI' and transactions.\"State\" <> 'DC' and transactions.\"State\" <> 'GU'")
  dbDisconnect(jdbcConnection)
}
# START HERE FOR REBUILDING
states=map_data("state")
county=map_data("county")

#Color Slider
low <- rgb(255,234,180,255,maxColorValue=255)
high <- "red"
countylines <- rgb(30,30,30,150,maxColorValue=255)


#USED THIS TO BUILD A HISTOGRAM WEIGHT DATA FRAME
h <- ggplot(data=merged) 
h <- h + geom_histogram(aes(x=State, fill=..count.., weight=Quantity*Acquisition_Cost/1000000))
h + scale_fill_gradient("$ in millions", low = low, high = high)
hg <- ggplot_build(h)


#USED THIS TO CONVERT THE LIST TO A DATA FRAME 
sdata <- as.data.frame(hg$data[1])

#USED THIS TO CAPITALIZE THE STATES COLUMN FOR ABBREVIATION MAKING
states$region <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", states$region, perl=TRUE)

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
t<-t+geom_polygon(data=scomb,aes(x=long,y=lat,group=group.x,fill=count),size=0.5,color="black")
t + scale_fill_gradient("$ in millions", low = low, high = high)



###################COUNTY DATA#####################



#USED THIS TO BUILD A HISTOGRAM WEIGHTED DATA FRAME BY COUNTY
merged$CST <- paste(merged$County,merged$State,sep=", ")
h2 <- ggplot(data=merged) 
h2 <- h2 + geom_histogram(aes(x=CST, fill=..count.., weight=Quantity*Acquisition_Cost/1000000))
h2 + scale_fill_gradient("$ in millions", low = low, high = high)
hg2 <- ggplot_build(h2)

#USED THIS TO CONVERT THE LIST TO A DATA FRAME 
cdata <- as.data.frame(hg2$data[1])

#USED THIS TO ADD County COLUMN TO CDATA
countyList <- merged
attach(countyList)
counties <- countyList[order(CST),]
detach(countyList)
counties <- unique(counties$CST)
cdata$CST <- counties

#USED THIS TO CAPITALIZE COUNTY DATA
county$subregion <- toupper(county$subregion)

#USED THIS TO CAPITALIZE THE STATES COLUMN FOR ABBREVIATION MAKING
county$region <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", county$region, perl=TRUE)

#USED THIS TO ABBREVIATE THE STATES COLUMN
county$region <- state.abb[match(county$region,state.name)]

county$CST <- paste(county$subregion,county$region,sep=", ")

#USED THIS TO MERGE SDATA WITH STATES BY ABB
comb <- merge(county,cdata,by="CST")

#USED THIS TO SORT BY ORDER FOR PROPER DRAWING
attach(comb)
scomb <- comb[order(order),]
detach(comb)

#USED THIS TO PLOT THE TEMP MAP
t2<- ggplot()
t2 <-t2 +geom_polygon(data=states,aes(x=long,y=lat,group=group),fill=low)
t2<-t2+geom_polygon(data=scomb,aes(x=long,y=lat,group=group.x,fill=count),color=countylines)
t2 <-t2 +geom_polygon(data=states,aes(x=long,y=lat,group=group),size=0.5,fill=rgb(0,0,0,0),color="black")
t2 + scale_fill_gradient("$ in millions", low = low, high = high) 

# HEATMAP AND STATE AND COUNTY ABOVE

# $ SPENT DATA FOR TOP COUNTIES BELOW
topcounties <- subset(scomb,select=c(CST,subregion,region,count))
attach(topcounties)
tmp <- topcounties[order(-count),]
detach(topcounties)
topcounties <- tmp
top10counties <- head(unique(topcounties),10)

t10 <- ggplot(data=top10counties) 
t10 <- t10 + geom_histogram(aes(x=CST, fill=..count.., weight=count))
t10 + scale_fill_gradient("$ in millions", low = low, high = high)

################CLASS NAME DATA ANALYSIS#################
catData <- subset(merge(merged,alias,by="FEDERAL_SUPPLY_CATEGORY"),select=c(ALIAS,Quantity,Acquisition_Cost))

attach(catData);
cats <- catData[order(ALIAS),]
detach(catData);
cats <- unique(cats$ALIAS);

hcd <- ggplot(data=catData)
hcd <- hcd + geom_histogram(aes(x=ALIAS, fill=..count..,weight=Quantity*Acquisition_Cost/1000))
hcd + scale_fill_gradient("$ spent per category", low = low, high = high)

catDataW <- as.data.frame(ggplot_build(hcd)$data[1])
catDataW$ALIAS <- cats

catDataW$freq <- as.integer(catDataW$count)
catDataW$freq <- sqrt(catDataW$freq)
catDataW$ALIAS[69] <- "VEHICLES"

pal2=brewer.pal(8,"Dark2")

wordcloud(catDataW$ALIAS, catDataW$freq,scale=c(2,0.2), max.words=80, 
          random.order=FALSE,random.color=FALSE, rot.per=0.35, colors=pal2)



