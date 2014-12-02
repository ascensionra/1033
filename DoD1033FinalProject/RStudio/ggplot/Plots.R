library("ggplot2")
options(java.parameters="-Xmx2g")
library(rJava)
library(RJDBC)

jdbcDriver <- JDBC(driverClass="oracle.jdbc.OracleDriver", classPath="C:/Program Files/Java/jdk1.8.0_20/ojdbc6.jar")

# In the following, use your username and password instead of "CS347_prof", "orcl_prof" once you have an Oracle account
possibleError <- tryCatch(
  jdbcConnection <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@128.83.138.158:1521:orcl", "C##cs347_jsm3287", "orcl_jsm3287"),
  error=function(e) e
)
if(!inherits(possibleError, "error")){
  transactions <- dbGetQuery(jdbcConnection, "select * from transactions")
  dbDisconnect(jdbcConnection)
}
head(transactions)

ggplot(data = transactions) + geom_histogram(aes(x = carat))
ggplot(data = transactions) + geom_density(aes(x = carat, fill = "gray50"))
ggplot(transactions, aes(x = carat, y = price)) + geom_point()
p <- ggplot(transactions, aes(x = carat, y = price)) + geom_point(aes(color = color))
p + facet_wrap(~color) # For ~, see http://stat.ethz.ch/R-manual/R-patched/library/base/html/tilde.html and http://stat.ethz.ch/R-manual/R-patched/library/stats/html/formula.html
p + facet_grid(cut ~ clarity)
p <- ggplot(transactions, aes(x = carat)) + geom_histogram(aes(color = color), binwidth = max(transactions$carat)/30)
p + facet_wrap(~color) 
p + facet_grid(cut ~ clarity)


