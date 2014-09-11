library("ggplot2")
options(java.parameters="-Xmx2g")
library(rJava)
library(RJDBC)

jdbcDriver <- JDBC(driverClass="oracle.jdbc.OracleDriver", classPath="/usr/lib/jvm/java-7-openjdk-i386/ojdbc6.jar")

# In the following, use your username and password instead of "CS347_prof", "orcl_prof" once you have an Oracle account
possibleError <- tryCatch(
  jdbcConnection <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@128.83.138.158:1521:orcl", "C##cs347_jsm3287", "orcl_jsm3287"),
  error=function(e) e
)
if(!inherits(possibleError, "error")){
  inventory <- dbGetQuery(jdbcConnection, "select * from inventory")
  dbDisconnect(jdbcConnection)
}
head(inventory)

ggplot(data = diamonds) + geom_histogram(aes(x = carat))
ggplot(data = diamonds) + geom_density(aes(x = carat, fill = "gray50"))
ggplot(diamonds, aes(x = carat, y = price)) + geom_point()
p <- ggplot(diamonds, aes(x = carat, y = price)) + geom_point(aes(color = color))
p + facet_wrap(~color) # For ~, see http://stat.ethz.ch/R-manual/R-patched/library/base/html/tilde.html and http://stat.ethz.ch/R-manual/R-patched/library/stats/html/formula.html
p + facet_grid(cut ~ clarity)
p <- ggplot(diamonds, aes(x = carat)) + geom_histogram(aes(color = color), binwidth = max(diamonds$carat)/30)
p + facet_wrap(~color) 
p + facet_grid(cut ~ clarity)


