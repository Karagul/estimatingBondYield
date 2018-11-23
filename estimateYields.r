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
#setting this value to one if it is callable and trading above 100 (those indices gathered 3 lines above), if not 0
data$PAR[callableAndAbovePar]=1
data$PAR[notCallableAndAbovePar]=0
#getting rid of all instances where there is no reported niq
data$niq=as.numeric(data$niq)
data$dpq=as.numeric(data$dpq)
data$xintq=as.numeric(data$xintq)
#calculating netInterestIncome Over EBITDA and EBIT
data$NetInterestOverEBITDA= data$xintq/(data$niq+data$dpq)
data$NetInterestOverNiq=data$xintq/(data$niq)

names(data)[which(names(data)=="NetInterestOverNiq")]="NetInterestOverEBIT"

data$timeToMaturity=as.numeric(data$timeToMaturity)



print(paste("Number of indices where no callable", length(which(data$CALLABLE==0))))
print(head(data[which(data$CALLABLE==0),],n=20))
data<-data[,c("Clean_Price","PAR","yyyymmdd","CALLABLE","spreadAboveTreasury","timeToMaturity","NetInterestOverEBIT","NetInterestOverEBITDA","ISSUER_CUSIP","ISSUE_CUSIP","treasuryYield","whichTreasury","MATURITY","SIC_CODE")]
data<-data[complete.cases(data),]
print(head(data[,c("PAR", "CALLABLE","timeToMaturity", "NetInterestOverEBIT", "NetInterestOverEBITDA")], n=20))
print(paste("Number of indices where no callable", length(which(data$CALLABLE==0))))
#calculating the average spreadAboveTreasuryForEachSicCode
sicCodeAverages<- aggregate(data[, c("spreadAboveTreasury")], list(data$SIC_CODE),mean)
print(sicCodeAverages)
coefficients <- lm(spreadAboveTreasury ~ NetInterestOverEBITDA + NetInterestOverEBIT + timeToMaturity + CALLABLE + PAR , data=data)
coefficients


data$estimates <- data$spreadAboveTreasury 
data$estimates <- data$estimates + coefficients$residuals

print(paste("Old Residuals: ", var(coefficients$residuals)))


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

#setting estimatedSpreadOverTreasury to spreadOverTreasury of bond issued by SAME COMPANY that was traded in previous 30 days, since there is no need to estimate it, since we have a near perfect estimate
earliestTradeOfBondOfCertainCusip<-aggregate(data[,c("yyyymmdd")],list(data$ISSUER_CUSIP), min)
for(i in 1:length(data))
{
earliestTradeOfBondThatSharesSameCusip=earliestTradeOfBondOfCertainCusip$x[which(earliestTradeOfBondOfCertainCusip$Group.1 == data$ISSUER_CUSIP[i])]

if(data$yyyymmdd[i]>earliestTradeOfBondThatSharesSameCusip){
data$estimates[i]= data$spreadAboveTreasury[which((data$ISSUER_CUSIP == data$ISSUER_CUSIP[i])&(data$yyyymmdd==earliestTradeOfBondThatSharesSameCusip))[1]]
}
}
 
print(paste("New Average Residuals: ", var(data$estimates-data$spreadAboveTreasury)))


tblName<- "outputFromYieldEstimates"
dbWriteTable(mydb, tblName,data, overwrite=TRUE, append=FALSE)
dbDisconnect(mydb)
