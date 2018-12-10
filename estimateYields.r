library(RMySQL)
library(EGRET)
library(lubridate)
all_cons <- dbListConnections(MySQL())
for(con in all_cons)
	dbDisconnect(con)

debugPrint<-FALSE
info<-FALSE
limit = ";"

mydb = dbConnect(MySQL(), user='jch223', password='temp1234', dbname='bond2', host='localhost')

query <- paste("select * from testDateDataForPricing ", limit, sep="")

data <- dbGetQuery(mydb, query)

#data$spreadAboveTreasury <- data$Yield-as.numeric(data$treasuryYield)
#taking xintq divided by EBITDA and EBIT
yindices<-which(data$CALLABLE=="Y")
nindices<-which(data$CALLABLE=="N")
if(info)
{
print(paste("number of yindices:", length(yindices), "number of nindices:", length(nindices)))
}
data$CALLABLE[yindices]=1
data$CALLABLE[nindices]=0

if(info)
{
print(paste("Number of indices where no callable", length(which(data$CALLABLE==0))))
}
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


if(info)
{
print(paste("Number of indices where no callable", length(which(data$CALLABLE==0))))
print(head(data[which(data$CALLABLE==0),],n=20))
}
data<-data[,c("Clean_Price","PAR","yyyymmdd","CALLABLE","spreadAboveTreasury","timeToMaturity","NetInterestOverEBIT","NetInterestOverEBITDA","ISSUER_CUSIP","ISSUE_CUSIP","treasuryYield","whichTreasury","MATURITY","SIC_CODE","Yield")]
data<-data[complete.cases(data),]
if(info)
{
print(head(data[,c("PAR", "CALLABLE","timeToMaturity", "NetInterestOverEBIT", "NetInterestOverEBITDA")], n=20))
print(paste("Number of indices where no callable", length(which(data$CALLABLE==0))))
}
#calculating the average spreadAboveTreasuryForEachSicCode
sicCodeAverages<- aggregate(data[, c("spreadAboveTreasury")], list(data$SIC_CODE),mean)
if(info)
{
print(sicCodeAverages)
}
#seperating set into bonds that have same bond traded in previous 30 days and those that didn't

earliestTradeOfBondOfCertainCusip<-aggregate(data[,c("yyyymmdd")],list(data$ISSUER_CUSIP), min)
names(earliestTradeOfBondOfCertainCusip)=c("ISSUER_CUSIP", "minTradeDateForCusip")
data <- merge(data, earliestTradeOfBondOfCertainCusip, by=c("ISSUER_CUSIP"))
indicesWithBondTradedOfSameCusip <- which(data$yyyymmdd>data$minTradeDateForCusip)
notIndices <- which(!(data$yyyymmdd>data$minTradeDateForCusip))
data$group= numeric(length=length(data$ISSUER_CUSIP))
data$group[notIndices]=1
data$group[indicesWithBondTradedOfSameCusip]=0

#-----------------------------------------------------------------------------#
#adding residuals to data 

data$residuals=NA


#regression for bonds that do not have bond of same cusip traded in 30 day period
coefficients <- lm(spreadAboveTreasury ~ NetInterestOverEBITDA + NetInterestOverEBIT + timeToMaturity + CALLABLE + PAR , data=data[notIndices,])

print("coefficients for indices that DO NOT  have same bond traded in 30 day period")
summary(coefficients)


data[notIndices,]$residuals=coefficients$residuals

print(length(coefficients$residuals))
print(length(data[notIndices,]$residuals))

maxIndex= which(data[notIndices,]$residuals==max(coefficients$residuals))
minIndex= which(data[notIndices,]$residuals==min(coefficients$residuals))


print(data[notIndices,][maxIndex,])
print(data[notIndices,][minIndex,])

#regression for bonds that do have bond of same cusip traded in 30 day period
coefficients <- lm(spreadAboveTreasury ~ NetInterestOverEBITDA + NetInterestOverEBIT + timeToMaturity + CALLABLE + PAR , data=data[indicesWithBondTradedOfSameCusip,])


print("coefficients for indices that DO have same bond traded in 30 day period")

summary(coefficients)

data[indicesWithBondTradedOfSameCusip,]$residuals=coefficients$residuals

maxIndex= which(data[indicesWithBondTradedOfSameCusip,]$residuals==max(coefficients$residuals))
minIndex= which(data[indicesWithBondTradedOfSameCusip,]$residuals==min(coefficients$residuals))



print(data[indicesWithBondTradedOfSameCusip,][maxIndex,])
print(data[indicesWithBondTradedOfSameCusip,][minIndex,])

#getting the actual estimated spread over treasury and writing it to database
data$estimates <- data$spreadAboveTreasury 
data$estimates <- data$estimates + coefficients$residuals

tblName<- "outputFromYieldEstimates"
dbWriteTable(mydb, tblName,data, overwrite=TRUE, append=FALSE)
dbDisconnect(mydb)
