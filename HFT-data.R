#Creates a table of montly Observations of stocks, summary statistics from the table, and

setwd("~/HFT") #Sets working direcotry to ~/HFT

#options(digits = 4, scipen = 10, max.print = 10000)

library(lubridate) # Loads lubridate functions used later
library(tidyverse) # Loads tidyverse functions
library(mice) #loads mice functions
library(openxlsx)
#library(foreign) # Loads foreign function

HFQ_stocks <- read.csv("BigHFT.csv") #reads the data into variable HFQ_stocks

HFQ_stocks$DATE <- ymd(HFQ_stocks$DATE) # Changes the dates to lubridate format

HFQ_stocks <- mutate(HFQ_stocks, 'hfqToTrades' = hfq/trades) # Creates hfqToTrades collumn
original <- HFQ_stocks #saves the original dataset as a reference
original

colnames(HFQ_stocks)[7] <- "Volatility" #renames RET as Volatitlity as sd will be taken in aggregate

HFQ_stocks$Month <- month(HFQ_stocks$DATE,label = FALSE) # Creates month column
HFQ_stocks$Year <- year(HFQ_stocks$DATE) # Creates year column



md.pattern(HFQ_stocks) #shows pattern of missingness

#missingness <- HFQ_stocks[which(is.na(HFQ_stocks), arr.ind=TRUE)[,1],]

#CustomTable <- function(X) {sort(table(X), decreasing = TRUE)[1:10]}
#lapply(missingness, CustomTable)

#HFQ_stocks_NM <- HFQ_stocks[complete.cases(HFQ_stocks), ]

FirstObs <- function(X) {head(X,1)} #Creates first Observation function

Monthly_trades <- aggregate(trades~Month+Year+SYM_ROOT+PERMNO,data = HFQ_stocks,FUN = mean)# Aggregates trades by the variables and reports mean
Monthly_hfq <- aggregate(hfq~Month+Year+SYM_ROOT+PERMNO,data = HFQ_stocks,FUN = mean)# Aggregates hfq by the variables and saves mean in seperate table
Monthly_Volatility <- aggregate(Volatility~Month+Year+SYM_ROOT+PERMNO,data = HFQ_stocks,FUN = sd)# Aggregates RET by the variables and saves mean in seperate table
Monthly_FirstObsRETX <- aggregate(RETX~Month+Year+SYM_ROOT+PERMNO,data = HFQ_stocks,FUN = FirstObs)# Aggregates RETX by the variables and saves mean in seperate table
Monthly_FirstObsPrice <- aggregate(PRC~Month+Year+SYM_ROOT+PERMNO,data = HFQ_stocks,FUN = FirstObs)# Aggregates PRC by the variables and saves var in seperate table
Monthly_VOL <- aggregate(VOL~Month+Year+SYM_ROOT+PERMNO,data = HFQ_stocks,FUN = mean)# Aggregates VOL by the variables and saves mean in seperate table
Monthly_FirstObsmktcap <- aggregate(mktcap~Month+Year+SYM_ROOT+PERMNO,data = HFQ_stocks,FUN = FirstObs)# Aggregates mktcap by the variables and saves var in seperate table
Monthly_hfqToTrades <- aggregate(hfqToTrades~+Year+Month+SYM_ROOT+PERMNO,data = HFQ_stocks,FUN = mean)# Aggregates ratio of hfq to trades by the variables and saves mean in seperate table

Monthly_data <- merge(x = Monthly_hfq, y = Monthly_trades, by = c("Month", "Year", "SYM_ROOT", "PERMNO"), all = TRUE)
Monthly_data <- merge(x = Monthly_data, y = Monthly_Volatility, by = c("Month", "Year", "SYM_ROOT", "PERMNO"), all = TRUE)
Monthly_data <- merge(x = Monthly_data, y = Monthly_FirstObsRETX, by = c("Month", "Year", "SYM_ROOT", "PERMNO"), all = TRUE)
Monthly_data <- merge(x = Monthly_data, y = Monthly_FirstObsPrice, by = c("Month", "Year", "SYM_ROOT", "PERMNO"), all = TRUE)
Monthly_data <- merge(x = Monthly_data, y = Monthly_VOL, by = c("Month", "Year", "SYM_ROOT", "PERMNO"), all = TRUE)
Monthly_data <- merge(x = Monthly_data, y = Monthly_FirstObsmktcap, by = c("Month", "Year", "SYM_ROOT", "PERMNO"), all = TRUE)
Monthly_data <- merge(x = Monthly_data, y = Monthly_hfqToTrades, by = c("Month", "Year", "SYM_ROOT", "PERMNO"), all = TRUE)
Monthly_data <- mutate(Monthly_data, "Laggedmktcap" = mktcap/(1+RETX))
Monthly_data <- mutate(Monthly_data, "LaggedPrice" = PRC/(1+RETX))
Monthly_data <- mutate(Monthly_data, "lnOfLaggedmktcap" = log(Laggedmktcap))

Monthly_data <- mutate(Monthly_data, "lnOfTrades" = log(trades))
Monthly_data <- mutate(Monthly_data, "lnOfVol" = log(VOL))

md.pattern(Monthly_data)

# log() is ln

Monthly_data$Month <- factor(Monthly_data$Month, ordered = TRUE, levels = 1:12)

Monthly_data <- Monthly_data[order( Monthly_data[,2], Monthly_data[,1] ),]

options(max.print = 1000)

Monthly_data <- Monthly_data[,-c(8,9,11)]

MonthlyRef <- Monthly_data

#Monthly_data_filtered = Monthly_data[Monthly_data["LaggedPrice"] >= 5]

Monthly_data <- Monthly_data[which(Monthly_data$LaggedPrice >= 5), ]
Monthly_data <- Monthly_data[which(!is.na(Monthly_data$LaggedPrice)), ]

Monthly_data <- Monthly_data[which(!is.na(Monthly_data$Volatility)),]

Monthly_data <- mutate(Monthly_data, "lnOfLaggedPrice" = log(LaggedPrice))

row.names(Monthly_data) <- 1:nrow(Monthly_data)

md.pattern(Monthly_data)


#p = 1
#while(p < (nrow(Monthly_data) - 1)){
 # if(is.na(Monthly_data$LaggedPrice[p]) == TRUE){
  #  Monthly_data <- Monthly_data[-p, ]
  #}else{
   # if(Monthly_data$LaggedPrice[p] < 5){
    #  Monthly_data <- Monthly_data[-p, ]
    #}else{
     #p <- p + 1
#    }
 # }
#}

#p = 1
#while(p < (nrow(Monthly_data) - 1)){
#  if(is.na(Monthly_data$Volatility[p]) == TRUE){
#    Monthly_data <- Monthly_data[-p, ]
#  }else{
#    p <- p + 1
#  }
#}

#md.pattern(Monthly_data)

write.csv(Monthly_data, "Monthly_Data-NoMissingNoPennyStock.csv")
write.xlsx(Monthly_data, "Monthly_Data.xlsx")

options(max.print = 10000)

#missingPrices <- which(is.na(Monthly_data$LaggedPrice), arr.ind=TRUE) #this check works! All missing Observations for laggedprices are deleted by preceding loop
#pennyStockPrices <- which(Monthly_data$LaggedPrice < 5) #this check works! All LaggedPrices less than 5 are deleted by the preceding loop

#OrderedMonthlyData <- read.xlsx(xlsxFile = "OrderedMonthlyData.xlsx")

correlationsMatrix <- data.frame(month = numeric(), year = numeric(), variables = character(), hfq = numeric(), trades = numeric(), 
Volatility = numeric(), volume = numeric(), hfqToTrades = numeric(), laggedMarketCap = numeric(), 
laggedPrice = numeric(), lnOfLaggedmktcap = numeric(), lnOfTrades = numeric(), lnOfVol = numeric(), lnOfLaggedPrice = numeric())

tempdf <- data.frame(hfq = numeric(), trades = numeric(), Volatility = numeric(), 
volume = numeric(), hfqToTrades = numeric(), laggedMarketCap = numeric(), laggedPrice = numeric(), 
lnOfLaggedmktcap = numeric(), lnOfTrades = numeric(), lnOfVol = numeric(), lnOflaggedPrice = numeric())

datedf <- data.frame(month = numeric(11), year = numeric(11), variables = character(11))
datedf$variables <- c("hfq", "trades", "Volatility", "VOL", "hfqToTrades", "Laggedmktcap", "LaggedPrice", "lnOfLaggedmketcap", "lnOfTrades", "lnOfVol", "lnOfLaggedPrice")


y <- 2016:2018
m <- 1:12
cntr <- 1
for(x in y){
  for(z in m){
    tempdf <- Monthly_data[which(Monthly_data$Month == z & Monthly_data$Year == x), c(5:15)]
    
    tempdf2 <- cor(tempdf)
    
    datedf$month <- z
    datedf$year <- x
    
    tempdf3 <- cbind(datedf, tempdf2)
    
    correlationsMatrix <- rbind(correlationsMatrix, tempdf3)
  }
}

row.names(correlationsMatrix) <- 1:nrow(correlationsMatrix)

correlationsMatrix

write.xlsx(correlationsMatrix, "CorrelationsMatrix.xlsx", row.names = TRUE)

options(scipen = 9)

library(regclass)

all_correlations(Monthly_data, sorted = "magnitude")

options(max.print = 10000, scipen = 999)







names <- colnames(Monthly_data)

names <- names[c(-1,-2,-3,-4, -5)]

current <- aggregate(hfq~Month+Year, data = Monthly_data, FUN = mean)
names(current)[3] <- "mean"
current$variable <- "hfq"

for (x in names) {
  SumStats <- aggregate(get(x)~Month+Year, data = Monthly_data, FUN = mean)
  names(SumStats)[3] <- "mean"
  SumStats$variable <- x
  current <- rbind(current,SumStats)
}

current2 <- aggregate(hfq~Month+Year, data = Monthly_data, FUN = sd)
names(current2)[3] <- "sd"
current2$variable <- "hfq"

for (x in names) {
  SumStats <- aggregate(get(x)~Month+Year, data = Monthly_data, FUN = sd)
  names(SumStats)[3] <- "sd"
  SumStats$variable <- x
  current2 <- rbind(current2,SumStats)
}

current3 <- aggregate(hfq~Month+Year, data = Monthly_data, FUN = length)
names(current3)[3] <- "Obs"
current3$variable <- "hfq"

for (x in names) {
  SumStats <- aggregate(get(x)~Month+Year, data = Monthly_data, FUN = length)
  names(SumStats)[3] <- "Obs"
  SumStats$variable <- x
  current3 <- rbind(current3,SumStats)
}

Quan5 <- function(X) {quantile(X,.05)}
Quan25 <- function(X) {quantile(X,.25)}
Quan50 <- function(X) {quantile(X,.50)}
Quan75 <- function(X) {quantile(X,.75)}
Quan95 <- function(X) {quantile(X,.95)}

current4 <- aggregate(hfq~Month+Year, data = Monthly_data, FUN = Quan5)
names(current4)[3] <- "5P"
current4$variable <- "hfq"

for (x in names) {
  SumStats <- aggregate(get(x)~Month+Year, data = Monthly_data, FUN = Quan5)
  names(SumStats)[3] <- "5P"
  SumStats$variable <- x
  current4 <- rbind(current4,SumStats)
}

current5 <- aggregate(hfq~Month+Year, data = Monthly_data, FUN = Quan25)
names(current5)[3] <- "25P"
current5$variable <- "hfq"

for (x in names) {
  SumStats <- aggregate(get(x)~Month+Year, data = Monthly_data, FUN = Quan25)
  names(SumStats)[3] <- "25P"
  SumStats$variable <- x
  current5 <- rbind(current5,SumStats)
}

current6 <- aggregate(hfq~Month+Year, data = Monthly_data, FUN = Quan50)
names(current6)[3] <- "50P"
current6$variable <- "hfq"

for (x in names) {
  SumStats <- aggregate(get(x)~Month+Year, data = Monthly_data, FUN = Quan50)
  names(SumStats)[3] <- "50P"
  SumStats$variable <- x
  current6 <- rbind(current6,SumStats)
}

current7 <- aggregate(hfq~Month+Year, data = Monthly_data, FUN = Quan75)
names(current7)[3] <- "75P"
current7$variable <- "hfq"

for (x in names) {
  SumStats <- aggregate(get(x)~Month+Year, data = Monthly_data, FUN = Quan75)
  names(SumStats)[3] <- "75P"
  SumStats$variable <- x
  current7 <- rbind(current7,SumStats)
}

current8 <- aggregate(hfq~Month+Year, data = Monthly_data, FUN = Quan95)
names(current8)[3] <- "95P"
current8$variable <- "hfq"

for (x in names) {
  SumStats <- aggregate(get(x)~Month+Year, data = Monthly_data, FUN = Quan95)
  names(SumStats)[3] <- "95P"
  SumStats$variable <- x
  current8 <- rbind(current8,SumStats)
}

SumStats <- cbind(current[c(1,2,4,3)], current2[3], current3[3], current4[3], current5[3], current6[3], current7[3], current8[3])

options(max.print = 10000, scipen = 999, digits = 3)

write.xlsx(SumStats, "SumStats.xlsx")

SumStats






