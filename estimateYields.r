library(RMySQL)
library(EGRET)
library(lubridate)
all_cons <- dbListConnections(MySQL())
for(con in all_cons)
	dbDisconnect(con)

debugPrint<-FALSE
info<-TRUE
limit = ";"

mydb = dbConnect(MySQL(), user='jch223', password='temp1234', dbname='bond2', host='localhost')

query <- paste("select * from testDateDataForPricing ", limit, sep="")

data <- dbGetQuery(mydb, query)

data$spreadAboveTreasury <- data$Yield-as.numeric(data$treasuryYield)
#taking xintq divided by EBITDA and EBIT
yindices<-which(data$CALLABLE=="Y")
nindices<-which(data$CALLABLE=="N")
if(info)
{
print(paste("number of yindices:", length(yindices), "number of nindices:", length(nindices)))
}
data$CALLABLE[yindices]=1
data$CALLABLE[nindices]=0

print(paste("Number of indices where no callable", length(which(data$CALLABLE==0))))

callableAndAbovePar<-which(data$CALLABLE==1 & data$Clean_Price>=100)
notCallableAndAbovePar<-which(!(data$CALLABLE==1 & data$Clean_Price>=100))

data$PAR=data$ISSUER_CUSIP
data$PAR[callableAndAbovePar]=1
data$PAR[notCallableAndAbovePar]=0
#getting rid of all instances where there is no reported niq
data$niq=as.numeric(data$niq)
data$dpq=as.numeric(data$dpq)
data$xintq=as.numeric(data$xintq)
data$EBITDA= data$xintq/(data$niq+data$dpq)
data$niq=data$xintq/(data$niq)

names(data)[which(names(data)=="niq")]="EBIT"

data$timeToMaturity=as.numeric(data$timeToMaturity)



print(paste("Number of indices where no callable", length(which(data$CALLABLE==0))))
print(head(data[which(data$CALLABLE==0),],n=20))
data<-data[,c("Clean_Price","PAR","yyyymmdd","CALLABLE","spreadAboveTreasury","timeToMaturity","EBIT","EBITDA","ISSUER_CUSIP","ISSUE_CUSIP","treasuryYield","whichTreasury","MATURITY")]
data<-data[complete.cases(data),]
print(head(data[,c("PAR", "CALLABLE","timeToMaturity", "EBIT", "EBITDA")], n=20))
print(paste("Number of indices where no callable", length(which(data$CALLABLE==0))))
coefficients <- lm(spreadAboveTreasury ~ EBITDA + EBIT + timeToMaturity + CALLABLE + PAR , data=data)
coefficients


data$estimates <- data$spreadAboveTreasury 
data$estimates <- data$estimates + coefficients$residuals


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

tblName<- "outputFromYieldEstimates"
dbWriteTable(mydb, tblName,data, overwrite=TRUE, append=FALSE)
dbDisconnect(mydb)
