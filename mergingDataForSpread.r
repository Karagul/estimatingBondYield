library(RMySQL)
library(EGRET)
library(lubridate)
all_cons <- dbListConnections(MySQL())
for(con in all_cons)
	dbDisconnect(con)

#input date you would like to estimate for here
estimatingDate <- as.Date("2008-03-05")
thirtyDaysPrior <- estimatingDate-30
debugPrint<-FALSE
info<-TRUE
debugPrint2<-FALSE
limit = ";"
mydb = dbConnect(MySQL(), user='jch223', password='temp1234', dbname='bond2', host='localhost')
query <-paste("select * from c2c", format(strptime(estimatingDate, "%Y-%m-%d"),"%Y"),"e", " where yyyymmdd<=", format(strptime(estimatingDate, "%Y-%m-%d"), "%Y%m%d"), " and yyyymmdd>=",format(strptime(thirtyDaysPrior, "%Y-%m-%d"), "%Y%m%d"), limit , sep="")
if(debugPrint)
{
print(query)
}
prices <- dbGetQuery( mydb,query) 
if(debugPrint)
{
head(prices, n=10)
print(paste("Number of rows from c2c: ", nrow(prices) , sep=""))
}
if(info)
{
print(paste("Original Size of Prices:", nrow(prices)))
}
names(prices)[names(prices)=="Cusip"]= "ISSUER_CUSIP"
prices$ISSUE_CUSIP=substring(prices$ISSUER_CUSIP, 7, 9)
prices$ISSUER_CUSIP=substring(prices$ISSUER_CUSIP, 1, 6)
if(debugPrint)
{
head(prices, n=10)
}

query<- "select ISSUER_CUSIP, ISSUE_CUSIP, MATURITY from Issue_Information_1980_201803"

issueInformation <- dbGetQuery(mydb, query)

prices<-merge(prices,issueInformation, by=c("ISSUER_CUSIP", "ISSUE_CUSIP"))
if(info)
{
print(paste("Size of Prices after merge with issue information:", nrow(prices)))
}
if(debugPrint)
{
head(prices, n=10)
}

query<- "select * from treasuryBondInformation"

treasuryInfo <- dbGetQuery(mydb, query)

oldColumnNames= c("NFCP_M1", "NFCP_M2", "D_CP_M3", "D_CP_M6", "TB_Y1", "TCMNOM_Y2", "TCMNOM_Y3", "TCMNOM_Y5", "TCMNOM_Y7","TCMNOM_Y10","TCMNOM_Y20", "TCMNOM_Y30")
newColumnNames= c("1monthMaturity", "2monthMaturity", "3monthMaturity", "6monthMaturity", "1yearMaturity", "2yearMaturity", "3yearMaturity", "5yearMaturity", "7yearMaturity", "10yearMaturity", "20yearMaturity", "30yearMaturity")
additionalMonths= c(1, 2, 3, 6, 12, 24, 36, 60, 84, 120, 240, 360)

for (i in 1:length(newColumnNames))
{
newName<-newColumnNames[i]
newMonth<-additionalMonths[i]
treasuryInfo[, newName]<-as.Date(treasuryInfo$date, "%Y%m%d")
if(debugPrint)
{
print(paste("formatted date 1st row: ", treasuryInfo[1, newName]))
print(paste("new Month: ", newMonth))
}
month(treasuryInfo[, newName]) <- month(treasuryInfo[,newName]) + newMonth

treasuryInfo[, newName]<-format(strptime(treasuryInfo[,newName], "%Y-%m-%d"), "%Y%m%d")
}

if(debugPrint)
{
head(treasuryInfo, n=10)
}

for(i in 1:length(prices))
{
	maturity<- as.numeric(prices$MATURITY[i])
	curYYYYMMDD=prices$yyyymmdd[i]
	if(debugPrint)
	{
		print(paste("Current Date", curYYYYMMDD))
		print(paste("Maturity", maturity))
	}
	lowestIndex=1;
	treasuryInfoForDay<-treasuryInfo[which(treasuryInfo$date==curYYYYMMDD),newColumnNames]	
	oldColumns<-treasuryInfo[which(treasuryInfo$date==curYYYYMMDD),oldColumnNames]
	if(debugPrint)
	{
		print(paste("treasury Info for the Day:", curYYYYMMDD))
		print(treasuryInfoForDay)
		print(paste("TreasuryInfo First Index", treasuryInfoForDay[lowestIndex]))
	}
	originalNull=FALSE
	#checking if first yield (M1) is null
	if(oldColumns[1,oldColumnNames[1]]=="")
	{
		originalNull=TRUE
	}
	for (x in 2:length(newColumnNames))
	{
		curLowDifference=abs(maturity-as.numeric(treasuryInfoForDay[lowestIndex]))
		curDifference=abs(maturity-as.numeric(treasuryInfoForDay[x]))
		isNull=FALSE
		#have to check if actual yield posted for treasury bond with most similar maturity	
		if(oldColumns[1,oldColumnNames[x]]=="")
		{
		isNull=TRUE
		}

		if(curDifference<curLowDifference && !isNull)
		{
		lowestIndex=x
		}
		else if(originalNull && !isNull)
		{
		lowestIndex=x
		}

	}
	if(debugPrint)
	{
		print(paste("Lowest Index:", newColumnNames[lowestIndex]))
	}	
	prices$treasuryYield[i]=oldColumns[1,oldColumnNames[lowestIndex]]
	prices$whichTreasury[i]=newColumnNames[lowestIndex]
}

badDataPoints<-prices[which(prices$treasuryYield==""),]

if(info)
{
print(paste("Number of Rows of final Table:", nrow(prices)))

head(prices,n=10)
print(paste("Bad Data Points:", nrow(badDataPoints)))
}

tblName <- "testDateDataForPricing"
dbWriteTable(mydb, tblName, prices, overwrite=TRUE, append=FALSE)
dbDisconnect(mydb)



