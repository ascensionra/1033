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
  #transactions <- dbGetQuery(jdbcConnection, "select * from transactions")
  #inventory <- dbGetQuery(jdbcConnection, "select * from inventory")
  merged <- dbGetQuery(jdbcConnection, "select * from inventory, transactions where transactions.nsn = inventory.nsn and transactions.\"State\" <> 'PR' and transactions.\"State\" <> 'VI' and transactions.\"State\" <> 'DC' and transactions.\"State\" <> 'GU'")
  dbDisconnect(jdbcConnection)
}
head(merged)
states=map_data("state")
head(states)



h <- ggplot(data=merged) 
h <- h + geom_histogram(aes(x=State, fill=..count.., weight=Quantity*Acquisition_Cost))
hg <- ggplot_build(h)

sdata <- as.data.frame(hg$data[1])
s <- ggplot(data=sdata) + geom_histogram(stat="identity",aes(x=group,fill=y,y=y))
s

#USED THIS TO CAPITALIZE THE STATES COLUMN FOR ABBREVIATION MAKING
states$region <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", states$region, perl=TRUE)
states$region

#USED THIS TO ABBREVIATE THE STATES COLUMN
states$abb <- state.abb[match(states$region,state.name)]
states$abb
head(states)

#USED THIS TO ADD ABBREVIATIONS TO SDATA
sdata$abb <- state.abb[order(state.abb)]
head(sdata)


#USED THIS TO MERGE SDATA WITH STATES BY ABB
comb <- merge(states,sdata,by="abb")
head(comb)

#USED THIS TO SORT BY ORDER FOR PROPER DRAWING
attach(comb)
scomb <- comb[order(order),]
detach(comb)
head(scomb)

#USED THIS TO PLOT THE TEMP MAP
t<- ggplot()
t<-t+geom_polygon(data=scomb,aes(x=long,y=lat,group=group.x,fill=count),color="black")
t + scale_fill_gradient("$ Spent", low = "green", high = "red")

## RUN ABOVE






