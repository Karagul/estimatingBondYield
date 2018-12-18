library(RMySQL)
library(EGRET)
library(lubridate)
library(plyr)
all_cons <- dbListConnections(MySQL())
for(con in all_cons)
	dbDisconnect(con)

debugPrint<-FALSE
info<-FALSE
limit = ";"


startDate<-as.Date("2011-01-01")
endDate<-as.Date("2011-12-31") 


mydb = dbConnect(MySQL(), user='jch223', password='temp1234', dbname='bond2', host='localhost')

query <- paste("select * from testDateDataForPricing ", limit, sep="")

entireYearOfData <- dbGetQuery(mydb, query)
entireYearOfData<- entireYearOfData[!duplicated(entireYearOfData),]
print(paste("Length of entire year of data: ", length(entireYearOfData$yyyymmdd)))
createTable=1
entireYearOfData$yyyymmdd=as.numeric(entireYearOfData$yyyymmdd)
print(entireYearOfData$yyyymmdd)
while(startDate<=endDate)
{

estimatingDate=startDate
thirtyDaysBefore=estimatingDate-30
estimatingDate=as.numeric(format(strptime(estimatingDate, "%Y-%m-%d"), "%Y%m%d"))
thirtyDaysBefore=as.numeric(format(strptime(thirtyDaysBefore, "%Y-%m-%d"), "%Y%m%d"))

indicesThatFitCriteria=which(entireYearOfData$yyyymmdd<=estimatingDate & entireYearOfData$yyyymmdd>=thirtyDaysBefore)
fullThirtyDayPeriod<-entireYearOfData[indicesThatFitCriteria,]
#getting only the bonds traded on the estimating date
priorData<-fullThirtyDayPeriod[which(fullThirtyDayPeriod$yyyymmdd!=estimatingDate),]
data<-fullThirtyDayPeriod[which(fullThirtyDayPeriod$yyyymmdd==estimatingDate),]

print(paste("Date: ",estimatingDate , " 30 Day Period: " , length(fullThirtyDayPeriod$yyyymmdd), " Estimating date: ", length(data$yyyymmdd)))


if(length(data$yyyymmdd)>0)
{
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
#checking to make sure there is anything in data set after checking for complete cases
if(length(data$yyyymmdd)>0)
{
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
	#getting min trading date of each bond in 30 day period
earliestTradeOfBondOfCertainCusip<-aggregate(fullThirtyDayPeriod[,c("yyyymmdd")],list(fullThirtyDayPeriod$ISSUER_CUSIP), min)
names(earliestTradeOfBondOfCertainCusip)=c("ISSUER_CUSIP", "minTradeDateForCusip")
data <- merge(data, earliestTradeOfBondOfCertainCusip, by=c("ISSUER_CUSIP"))
indicesWithBondTradedOfSameCusip <- which(data$yyyymmdd>data$minTradeDateForCusip)
notIndices <- which(!(data$yyyymmdd>data$minTradeDateForCusip))

print(paste("Date, ", estimatingDate , " : With bond in prior 30: ", length(indicesWithBondTradedOfSameCusip), " Without bond traded in prior 30: ", length(notIndices)))

#-----------------------------------------------------------------------------#

#---------------------------------------------------------------------------------#
#pairing up most recent trade to estimating date for each cusip with the spreadOverTreasury for that day
mostRecentTradeToEstimatingDate<-aggregate(priorData[,c("yyyymmdd")], list(priorData$ISSUER_CUSIP),max)

priorData<-unique(priorData[,c("ISSUER_CUSIP", "yyyymmdd", "spreadAboveTreasury")])
names(priorData)<- c("ISSUER_CUSIP", "yyyymmdd", "spreadAboveTreasury")
names(mostRecentTradeToEstimatingDate)=c("ISSUER_CUSIP", "yyyymmdd")
mostRecentTradeToEstimatingDate<-join(mostRecentTradeToEstimatingDate, priorData, by=c("ISSUER_CUSIP", "yyyymmdd"), type="left", match="first")

#---------------------------------------------------------------------------------#


names(mostRecentTradeToEstimatingDate)=c("ISSUER_CUSIP", "yyyymmdd", "prevSpreadAboveTreasury")
bondsWithPrevTrade=data[indicesWithBondTradedOfSameCusip,]
bondsWithPrevTrade<-merge(bondsWithPrevTrade, mostRecentTradeToEstimatingDate, by=c("ISSUER_CUSIP"))

bondsWithPrevTrade<-bondsWithPrevTrade[!duplicated(bondsWithPrevTrade),]

#regression for bonds that do have bond of same cusip traded in 30 day period
coefficients <- lm(spreadAboveTreasury ~ NetInterestOverEBITDA + NetInterestOverEBIT + timeToMaturity + CALLABLE + PAR + prevSpreadAboveTreasury, data=bondsWithPrevTrade)
write( append(quantile(coefficients$residuals), estimatingDate, after=5), file="residualDistribution", ncolumns=6 , append=TRUE)
#getting the actual estimated spread over treasury and writing it to database
bondsWithPrevTrade$estimates <- bondsWithPrevTrade$spreadAboveTreasury 
print(paste("prevTrade: ", length(bondsWithPrevTrade$estimates), " coefficients ", length(coefficients$residuals)))
bondsWithPrevTrade$estimates <- bondsWithPrevTrade$estimates + coefficients$residuals

tblName<- "outputFromYieldEstimates"
if(createTable==1)
{
dbWriteTable(mydb, tblName,bondsWithPrevTrade, overwrite=TRUE, append=FALSE)
createTable=0
}
else
{
dbWriteTable(mydb,tblName, bondsWithPrevTrade, overwrite=FALSE, append=TRUE)
}
}
}
startDate<-startDate + 1
}
dbDisconnect(mydb)
