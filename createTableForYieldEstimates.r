library(RMySQL)
library(EGRET)
library(lubridate)
library(mondate)
all_cons <- dbListConnections(MySQL())
for(con in all_cons)
	dbDisconnect(con)
debugPrint4<-FALSE
#input date you would like to estimate for here
startDate <- as.Date("2011-01-01")
nextYear = year(startDate)+1
#determining which calendar quartaer the date falls in
thirtyDaysPrior <- startDate-30
debugPrint<-FALSE
info<-TRUE
debugPrint2<-FALSE
limit = " ;"
mydb = dbConnect(MySQL(), user='jch223', password='temp1234', dbname='bond2', host='localhost')
#query for getting trades from the C2C tables
query <-paste("select * from c2c2002_201803 where yyyymmdd<=", nextYear, "0101 and yyyymmdd>=",format(strptime(thirtyDaysPrior, "%Y-%m-%d"), "%Y%m%d")," ", limit , sep="")
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
#breaking up CUSIP into ISSUE_CUSIP and ISSUER_CUSIP
prices$ISSUE_CUSIP=substring(prices$Cusip, 7, 9)
#prices$ISSUER_CUSIP=substring(prices$ISSUER_CUSIP, 1, 6)
if(debugPrint)
{
head(prices, n=10)
}
#query for getting maturity dates for bonds from Issue_Information table
query<-"SELECT * FROM Issue_Information_1980_201803 WHERE COUNTRY_DOMICILE = 'USA' AND FOREIGN_CURRENCY = 'N' AND BOND_TYPE NOT IN ('PSTK', 'PS','EMTN','MBS', 'TPCS', 'CCOV') AND INTEREST_FREQUENCY = 2 AND (PUTABLE <> 'Y' OR PUTABLE IS NULL) AND INDUSTRY_CODE<40 AND OFFERING_PRICE<101 AND OFFERING_PRICE>99 AND OFFERING_AMT>0 AND OFFERING_DATE IS NOT NULL"
issueInformation <- dbGetQuery(mydb, query)
if(debugPrint)
{
head(issueInformation, n=10)
}
names(prices)
names(issueInformation)


prices<-merge(issueInformation, prices,  by.x=c("ISSUER_CUSIP", "ISSUE_CUSIP"), by.y=c("ISSUER_CUSIP","ISSUE_CUSIP"))
if(info)
{
print(paste("Size of Prices after merge with issue information:", nrow(prices)))
}
if(debugPrint)
{
head(prices, n=10)
}
#query for getting company information (ie: net income) from compustatFinal table, getting all for this year, and only Q4 for the previous year, since we have to match the prior 30 days we are taking 
query<- paste("select cusip, xintq, niq, dpq, datacqtr from compustatFinal where substring(datacqtr,1,4)=\'",nextYear-1, "\' or datacqtr=\'",nextYear-2, "Q4\' ", limit,  sep="")


print(query)
compustatData<- dbGetQuery(mydb, query)

if(info)
{
print(paste("Length of Compustat Data:", nrow(compustatData)))
}

compustatData$cusip=substr(compustatData$cusip, 1, 6)

names(compustatData)[which(names(compustatData)=="cusip")]<- "ISSUER_CUSIP"
#associating each trading date with a calendar quarter

quartersForMerging<-data.frame(month=character(12), quarter=character(12))
quartersForMerging$month<- c('01','02','03','04','05','06','07','08','09','10','11','12')
quartersForMerging$quarter<- c('Q1','Q1','Q1','Q2','Q2','Q2','Q3','Q3','Q3','Q4','Q4','Q4')
prices$month=  sprintf("%02d", floor(prices$yyyymmdd/100)%%100)

if(1)
{
head(prices[which(prices$yyyymmdd<20100101),], n=10)
}
prices$year= floor(prices$yyyymmdd/10000)

if(debugPrint)
{
head(prices, n=10)
}
prices=merge(prices,quartersForMerging, by=c("month"))
prices$datacqtr= paste(prices$year, prices$quarter, sep="")

if(1)
{
head(prices[which(prices$yyyymmdd<20100101),], n=10)
}

prices <- merge(prices,compustatData, by=c("ISSUER_CUSIP","datacqtr"))


if(info)
{
print(paste("Length of prices after merge with compustat:", nrow(prices)))
}


if(info)
{
print(paste("Length of compustatData:", nrow(compustatData)))
}

#query for getting info such as if bond is callable, and the call date that would produce worst yield from accrued_ytw table
query <- paste("select ISSUER_CUSIP, ISSUE_CUSIP, CALLABLE, YTW, call_date_worst, trans_dt from accrued_ytw2002_2018  where trans_dt<=", nextYear, "0101 and trans_dt>=",format(strptime(thirtyDaysPrior, "%Y-%m-%d"), "%Y%m%d")," ", limit , sep="")

yieldToWorst <- dbGetQuery(mydb, query)



prices$ISSUE_CUSIP=substr(prices$ISSUE_CUSIP, 1, 2)


head(prices,n=10)

names(yieldToWorst)[which(names(yieldToWorst)=="trans_dt")] = "yyyymmdd"


head(yieldToWorst, n=10)

prices <- merge(prices, yieldToWorst, by=c("ISSUER_CUSIP", "ISSUE_CUSIP", "yyyymmdd"))

if(info)
{
print(paste("Length of YieldToWorst Query:", nrow(yieldToWorst)))
print(paste("Length of Prices After merging with yieldToWorst:", nrow(prices)))
}
#getting treasury yield information
query<- "select * from treasuryBondInformation"

treasuryInfo <- dbGetQuery(mydb, query)

#original column names in treasuryInformation
oldColumnNames= c("NFCP_M1", "NFCP_M2", "D_CP_M3", "D_CP_M6", "TB_Y1", "TCMNOM_Y2", "TCMNOM_Y3", "TCMNOM_Y5", "TCMNOM_Y7","TCMNOM_Y10","TCMNOM_Y20", "TCMNOM_Y30")
newColumnNames= c("1monthMaturity", "2monthMaturity", "3monthMaturity", "6monthMaturity", "1yearMaturity", "2yearMaturity", "3yearMaturity", "5yearMaturity", "7yearMaturity", "10yearMaturity", "20yearMaturity", "30yearMaturity")
additionalMonths= c(1, 2, 3, 6, 12, 24, 36, 60, 84, 120, 240, 360)

#this first for loop, takes each date in the treausuryInfo, and calculates the maturity 
#of the 1 months, 2 months, 3months,... 30 years bonds
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
treasuryInfo[, newName] <- treasuryInfo[,newName] %m+% months(newMonth)
if(debugPrint4)
{
print(treasuryInfo[,newName])
}
treasuryInfo[, newName]<-format(strptime(treasuryInfo[,newName], "%Y-%m-%d"), "%Y%m%d")
}

if(debugPrint)
{
head(treasuryInfo, n=10)
}

#goes through each trade
for(i in 1:length(prices$MATURITY))
{
	maturity<- as.numeric(prices$MATURITY[i])
	curYYYYMMDD=prices$yyyymmdd[i]
	if(debugPrint)
	{
		print(paste("Current Date", curYYYYMMDD))
		print(paste("Maturity", maturity))
	}
	#initially assume 1 month treasury bond has smallest difference in maturity with coorporate bond's maturity
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
	#goes through the maturity dates of each treasuryBond (1 month, 2 month, ...., 30 year) and tries to match the maturity of the
	#coorporate bond in the outer for loop with the treasuryBond with most similar maturity
	for (x in 2:length(newColumnNames))
	{
		curLowDifference=abs(maturity-as.numeric(treasuryInfoForDay[lowestIndex]))
		curDifference=abs(maturity-as.numeric(treasuryInfoForDay[x]))
		
		#print(paste("Lowest Index:", lowestIndex, "curIndex:", x, "\ncurrentLowestIndex:", curLowDifference, "curDifference:", curDifference))
		
		isNull=FALSE
		#have to make sure a yield was actually posted for treasury bond
		if(oldColumns[1,oldColumnNames[x]]=="")
		{
		isNull=TRUE
		}

		#if the current treasury bond has a smaller difference in maturity, and it had a reported yield, set it to be the 
		#lowest index

		if(is.na(curDifference))
		{
		print(paste("X", x))
		print(paste("Length of new columns:", newColumnNames))
		print(paste("Yields:", oldColumns))
		print(paste("Maturities:", treasuryInfoForDay))
		}

		if(curDifference<curLowDifference && !isNull)
		{
		lowestIndex=x
		}
		
		#if the 1 month treasury bond is null, we are setting the lowestIndex to first yield that was reported, and then continuing with for loop
		else if(lowestIndex==1 && originalNull && !isNull)
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

#setting call_date_worst as maturity date
maturityDates<-as.Date(as.character(prices$call_date_worst), "%Y%m%d")
currentDates<-as.Date(as.character(prices$yyyymmdd), "%Y%m%d")

#calculating timeToMaturity
timeToMaturity<-maturityDates-currentDates
maturityDates<-as.Date(as.character(prices$MATURITY), "%Y%m%d")
#recalculating timeToMaturity using actual maturity dates for those bonds that are not callable (ie: dont have call_date_worsts)
timeToMaturity[which(is.na(prices$call_date_worst))]<-maturityDates[which(is.na(prices$call_date_worst))]-currentDates[which(is.na(prices$call_date_worst))]
prices$timeToMaturity=timeToMaturity

prices$spreadAboveTreasury <- prices$Yield - as.numeric(prices$treasuryYield)

tblName <- "testDateDataForPricing"
dbWriteTable(mydb, tblName, prices, overwrite=TRUE, append=FALSE)
dbDisconnect(mydb)



