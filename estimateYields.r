library(RMySQL)
library(EGRET)
library(lubridate)
all_cons <- dbListConnections(MySQL())
for(con in all_cons)
	dbDisconnect(con)

debugPrint<-FALSE
info<-TRUE
limit = "limit 1000;"

mydb = dbConnect(MySQL(), user='jch223', password='temp1234', dbname='bond2', host='localhost')

query <- paste("select * from testDateDataForPricing ", limit, sep="")

data <- dbGetQuery(mydb, query)

data$spreadAboveTreasury <- data$Yield-as.numeric(data$treasuryYield)
#taking xintq divided by EBITDA and EBIT
data$niq=as.numeric(data$niq)
data$dpq=as.numeric(data$dpq)
data$xintq=as.numeric(data$xintq)
data$EBITDA= data$xintq/(data$niq+data$dpq)
data$niq=data$xintq/(data$niq)

names(data)[which(names(data)=="niq")]="EBIT"

data$timeToMaturity=as.numeric(data$timeToMaturity)

coefficients <- lm(spreadAboveTreasury ~ EBITDA + EBIT + timeToMaturity, data=data)
coefficients




estimatesOfSpread=(coef1)+ (data$EBITDA*coef2) + (data$EBIT*coef3) + (data$timeToMaturity*coef4)
data$estimatesOfSpread=estimatesOfSpread

if(info)
{
summary(coefficients)
}

maxIndex= which(coefficients$residuals==max(coefficients$residuals))
minIndex= which(coefficients$residuals==min(coefficients$residuals))

if(info)
{
print(data[maxIndex,])
print(data[minIndex,])

}

