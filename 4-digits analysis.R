##1.2. Creation of Variety per country ----
#this is a very messy code, but it works;
rm(list=ls())
library(stringr)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
maindata <- read.csv("Data_4digits_analysis/IPCs_AI csv.csv", sep = ";", header = TRUE)
maindata$Subclass3 <- substr(maindata$ipc_class_symbol,1,4)

ChinesePatents <- maindata[which(maindata$ctry_code == 'CN'), ]
SouthKoreaPatents <- maindata[which(maindata$ctry_code == 'KR'), ]
USPatents <- maindata[which(maindata$ctry_code == 'US'), ]
JapanPatents <- maindata[which(maindata$ctry_code == 'JP'), ]

#include missing years:
T <- 1960:2018
write.csv2(T, file = "Data_4digits_analysis/1.csv", row.names = TRUE)
T <-read.csv("Data_4digits_analysis/1.csv", sep = ";", header = TRUE)
names(T)[names(T) == 'x'] <- 'priority_year'
China_AddMissingYears <- merge(ChinesePatents, T, all=TRUE, by="priority_year")
SouthKorea_AddMissingYears <- merge(SouthKoreaPatents, T, all=TRUE, by="priority_year")
US_AddMissingYears <- merge(USPatents, T, all=TRUE, by="priority_year")
Japan_AddMissingYears <- merge(JapanPatents, T, all=TRUE, by="priority_year")
maindata <- merge(maindata, T, all=TRUE, by="priority_year")

write.csv2(table(maindata$Subclass3, maindata$priority_year), file = "Data_4digits_analysis/4digits.csv", row.names = TRUE)
write.csv2(China_AddMissingYears, file = "Data_4digits_analysis/China_MergedPrioritiesandIPCcodes.csv", row.names = TRUE)
write.csv2(SouthKorea_AddMissingYears, file = "Data_4digits_analysis/SouthKorea_MergedPrioritiesandIPCcodes.csv", row.names = TRUE)
write.csv2(US_AddMissingYears, file = "Data_4digits_analysis/US_MergedPrioritiesandIPCcodes.csv", row.names = TRUE)
write.csv2(Japan_AddMissingYears, file = "Data_4digits_analysis/Japan_MergedPrioritiesandIPCcodes.csv", row.names = TRUE)

#1.2.1. CHINA IPC------
rm(list=ls())

China_MergedPrioritiesandIPCcodes <-read.csv("Data_4digits_analysis/China_MergedPrioritiesandIPCcodes.csv", sep = ";", header = TRUE)
China_MergedPrioritiesandIPCcodes <- China_MergedPrioritiesandIPCcodes[,(-1)]
write.csv2(table(China_MergedPrioritiesandIPCcodes$Subclass3,China_MergedPrioritiesandIPCcodes$priority_year), file = "Data_4digits_analysis/Chinatt2.csv")
China_IPC4digitsOverYears <-read.csv("Data_4digits_analysis/Chinatt2.csv", sep = ";", header = T)

China_Codes4digits <- China_IPC4digitsOverYears[,1]
China_NumberOfOccurrences4digits <- China_IPC4digitsOverYears[,(-1)]

FunctionforCombiningCodesWithValues <- function(a,b) {
  for (i in 1:dim(a)[2]){
    a[,i] <- paste(b,a[,i],sep = "-")
  }
  return(cbind(a[,1:i]))
}

write.csv2(FunctionforCombiningCodesWithValues(China_NumberOfOccurrences4digits,China_Codes4digits), file = "Data_4digits_analysis/Chinatt78.csv", row.names = TRUE)
rm(list=ls())
China_aggdata <-read.csv("Data_4digits_analysis/Chinatt78.csv", sep = ";", header = TRUE)
China_aggdata <- China_aggdata[,(-1)]

FunctionGrabCodes <- function(x) {
  grep("-0", x, invert=T)
}

j <- list(c("tete"))

ff <- function (r) {
  for (i in 1:dim(r)[2]){
    hh <- i
    j[[hh]] <- (substr(r[FunctionGrabCodes(r[,hh]), hh],1,4))
  }
  return(rbind(j[1:i]))
}

China_t1 <- ff(China_aggdata)

Years <- (1960:2018)
Years <- cbind(Years)

gg <- function (r,v) {
  for (i in 1:dim(v)[1]){
    hh <- i
    tx[hh] <- paste( unlist(r[[hh]]), collapse=' ')
  }
  return(cbind(tx[1:i]))
}
tx <- 't'

China_p <- cbind(Years, gg(China_t1,Years))
write.csv2(China_p, file = "Data_4digits_analysis/ChinaYearsAndIPCcodes4digits.csv", row.names = TRUE)
rm(list=ls())

China_p <-read.csv("Data_4digits_analysis/ChinaYearsAndIPCcodes4digits.csv", sep = ";", header = TRUE)
China_p <- China_p[,(-1)]

tx <- 't'
uu <- function (r) {
  for (i in 1:dim(r)[1]){
    tx[i] <- paste(r[i,2],tx[i-1],sep = " ")
  }
  return(cbind(tx[1:i]))  
}

China_p$accumulated<- uu(China_p)
China_p$accumulated <- gsub("\\s+", " ", str_trim(China_p$accumulated))

China_conv4dig <- function (r) {
  for (i in 1:dim(r)[1]){
    tx[i+1] <- (length(setdiff(unlist(strsplit(as.character(r[i+1,dim(r)[2]]), "\\ ")), 
                               unlist(strsplit(as.character(r[i, dim(r)[2]]), "\\ ")))))
  }
  tx[1] <- 0
  return(cbind(tx[1:i]))
}

China_p$accumulated2 <- China_conv4dig(China_p)
write.csv2(China_p, file = "Data_4digits_analysis/China4DigitsCodesAccumulated.csv", row.names = TRUE)

#1.2.2. South Korea IPC------
rm(list=ls())

SouthKorea_MergedPrioritiesandIPCcodes <-read.csv("Data_4digits_analysis/SouthKorea_MergedPrioritiesandIPCcodes.csv", sep = ";", header = TRUE)
SouthKorea_MergedPrioritiesandIPCcodes <- SouthKorea_MergedPrioritiesandIPCcodes[,(-1)]
write.csv2(table(SouthKorea_MergedPrioritiesandIPCcodes$Subclass3,SouthKorea_MergedPrioritiesandIPCcodes$priority_year), file = "Data_4digits_analysis/SouthKoreatt2.csv")
SouthKorea_IPC4digitsOverYears <-read.csv("Data_4digits_analysis/SouthKoreatt2.csv", sep = ";", header = T)

SouthKorea_Codes4digits <- SouthKorea_IPC4digitsOverYears[,1]
SouthKorea_NumberOfOccurrences4digits <- SouthKorea_IPC4digitsOverYears[,(-1)]

FunctionforCombiningCodesWithValues <- function(a,b) {
  for (i in 1:dim(a)[2]){
    a[,i] <- paste(b,a[,i],sep = "-")
  }
  return(cbind(a[,1:i]))
}

write.csv2(FunctionforCombiningCodesWithValues(SouthKorea_NumberOfOccurrences4digits,SouthKorea_Codes4digits), file = "Data_4digits_analysis/SouthKoreatt78.csv", row.names = TRUE)
rm(list=ls())
SouthKorea_aggdata <-read.csv("Data_4digits_analysis/SouthKoreatt78.csv", sep = ";", header = TRUE)
SouthKorea_aggdata <- SouthKorea_aggdata[,(-1)]

FunctionGrabCodes <- function(x) {
  grep("-0", x, invert=T)
}

j <- list(c("tete"))

ff <- function (r) {
  for (i in 1:dim(r)[2]){
    hh <- i
    j[[hh]] <- (substr(r[FunctionGrabCodes(r[,hh]), hh],1,4))
  }
  return(rbind(j[1:i]))
}

SouthKorea_t1 <- ff(SouthKorea_aggdata)

Years <- (1960:2018)
Years <- cbind(Years)

gg <- function (r,v) {
  for (i in 1:dim(v)[1]){
    hh <- i
    tx[hh] <- paste( unlist(r[[hh]]), collapse=' ')
  }
  return(cbind(tx[1:i]))
}
tx <- 't'

SouthKorea_p <- cbind(Years, gg(SouthKorea_t1,Years))
write.csv2(SouthKorea_p, file = "Data_4digits_analysis/SouthKoreaYearsAndIPCcodes4digits.csv", row.names = TRUE)
rm(list=ls())

SouthKorea_p <-read.csv("Data_4digits_analysis/SouthKoreaYearsAndIPCcodes4digits.csv", sep = ";", header = TRUE)
SouthKorea_p <- SouthKorea_p[,(-1)]

tx <- 't'
uu <- function (r) {
  for (i in 1:dim(r)[1]){
    tx[i] <- paste(r[i,2],tx[i-1],sep = " ")
  }
  return(cbind(tx[1:i]))  
}

SouthKorea_p$accumulated<- uu(SouthKorea_p)
SouthKorea_p$accumulated <- gsub("\\s+", " ", str_trim(SouthKorea_p$accumulated))

SouthKorea_conv4dig <- function (r) {
  for (i in 1:dim(r)[1]){
    tx[i+1] <- (length(setdiff(unlist(strsplit(as.character(r[i+1,dim(r)[2]]), "\\ ")), 
                               unlist(strsplit(as.character(r[i, dim(r)[2]]), "\\ ")))))
  }
  tx[1] <- 0
  return(cbind(tx[1:i]))
}

SouthKorea_p$accumulated2 <- SouthKorea_conv4dig(SouthKorea_p)
write.csv2(SouthKorea_p, file = "Data_4digits_analysis/SouthKorea4DigitsCodesAccumulated.csv", row.names = TRUE)

#1.2.3.US IPC------
rm(list=ls())

US_MergedPrioritiesandIPCcodes <-read.csv("Data_4digits_analysis/US_MergedPrioritiesandIPCcodes.csv", sep = ";", header = TRUE)
US_MergedPrioritiesandIPCcodes <- US_MergedPrioritiesandIPCcodes[,(-1)]
write.csv2(table(US_MergedPrioritiesandIPCcodes$Subclass3,US_MergedPrioritiesandIPCcodes$priority_year), file = "Data_4digits_analysis/UStt2.csv")
US_IPC4digitsOverYears <-read.csv("Data_4digits_analysis/UStt2.csv", sep = ";", header = T)

US_Codes4digits <- US_IPC4digitsOverYears[,1]
US_NumberOfOccurrences4digits <- US_IPC4digitsOverYears[,(-1)]

FunctionforCombiningCodesWithValues <- function(a,b) {
  for (i in 1:dim(a)[2]){
    a[,i] <- paste(b,a[,i],sep = "-")
  }
  return(cbind(a[,1:i]))
}

write.csv2(FunctionforCombiningCodesWithValues(US_NumberOfOccurrences4digits,US_Codes4digits), file = "Data_4digits_analysis/UStt78.csv", row.names = TRUE)
rm(list=ls())
US_aggdata <-read.csv("Data_4digits_analysis/UStt78.csv", sep = ";", header = TRUE)
US_aggdata <- US_aggdata[,(-1)]

FunctionGrabCodes <- function(x) {
  grep("-0", x, invert=T)
}

j <- list(c("tete"))

ff <- function (r) {
  for (i in 1:dim(r)[2]){
    hh <- i
    j[[hh]] <- (substr(r[FunctionGrabCodes(r[,hh]), hh],1,4))
  }
  return(rbind(j[1:i]))
}

US_t1 <- ff(US_aggdata)

Years <- (1960:2018)
Years <- cbind(Years)

gg <- function (r,v) {
  for (i in 1:dim(v)[1]){
    hh <- i
    tx[hh] <- paste( unlist(r[[hh]]), collapse=' ')
  }
  return(cbind(tx[1:i]))
}
tx <- 't'

US_p <- cbind(Years, gg(US_t1,Years))
write.csv2(US_p, file = "Data_4digits_analysis/USYearsAndIPCcodes4digits.csv", row.names = TRUE)
rm(list=ls())

US_p <-read.csv("Data_4digits_analysis/USYearsAndIPCcodes4digits.csv", sep = ";", header = TRUE)
US_p <- US_p[,(-1)]

tx <- 't'
uu <- function (r) {
  for (i in 1:dim(r)[1]){
    tx[i] <- paste(r[i,2],tx[i-1],sep = " ")
  }
  return(cbind(tx[1:i]))  
}

US_p$accumulated<- uu(US_p)
US_p$accumulated <- gsub("\\s+", " ", str_trim(US_p$accumulated))

US_conv4dig <- function (r) {
  for (i in 1:dim(r)[1]){
    tx[i+1] <- (length(setdiff(unlist(strsplit(as.character(r[i+1,dim(r)[2]]), "\\ ")), 
                               unlist(strsplit(as.character(r[i, dim(r)[2]]), "\\ ")))))
  }
  tx[1] <- 0
  return(cbind(tx[1:i]))
}

US_p$accumulated2 <- US_conv4dig(US_p)
write.csv2(US_p, file = "Data_4digits_analysis/US4DigitsCodesAccumulated.csv", row.names = TRUE)

#1.2.4.Japan IPC------
rm(list=ls())

Japan_MergedPrioritiesandIPCcodes <-read.csv("Data_4digits_analysis/Japan_MergedPrioritiesandIPCcodes.csv", sep = ";", header = TRUE)
Japan_MergedPrioritiesandIPCcodes <- Japan_MergedPrioritiesandIPCcodes[,(-1)]
write.csv2(table(Japan_MergedPrioritiesandIPCcodes$Subclass3,Japan_MergedPrioritiesandIPCcodes$priority_year), file = "Data_4digits_analysis/Japantt2.csv")
Japan_IPC4digitsOverYears <-read.csv("Data_4digits_analysis/Japantt2.csv", sep = ";", header = T)

Japan_Codes4digits <- Japan_IPC4digitsOverYears[,1]
Japan_NumberOfOccurrences4digits <- Japan_IPC4digitsOverYears[,(-1)]

FunctionforCombiningCodesWithValues <- function(a,b) {
  for (i in 1:dim(a)[2]){
    a[,i] <- paste(b,a[,i],sep = "-")
  }
  return(cbind(a[,1:i]))
}

write.csv2(FunctionforCombiningCodesWithValues(Japan_NumberOfOccurrences4digits,Japan_Codes4digits), file = "Data_4digits_analysis/Japantt78.csv", row.names = TRUE)
rm(list=ls())
Japan_aggdata <-read.csv("Data_4digits_analysis/Japantt78.csv", sep = ";", header = TRUE)
Japan_aggdata <- Japan_aggdata[,(-1)]

FunctionGrabCodes <- function(x) {
  grep("-0", x, invert=T)
}

j <- list(c("tete"))

ff <- function (r) {
  for (i in 1:dim(r)[2]){
    hh <- i
    j[[hh]] <- (substr(r[FunctionGrabCodes(r[,hh]), hh],1,4))
  }
  return(rbind(j[1:i]))
}

Japan_t1 <- ff(Japan_aggdata)

Years <- (1960:2018)
Years <- cbind(Years)

gg <- function (r,v) {
  for (i in 1:dim(v)[1]){
    hh <- i
    tx[hh] <- paste( unlist(r[[hh]]), collapse=' ')
  }
  return(cbind(tx[1:i]))
}
tx <- 't'

Japan_p <- cbind(Years, gg(Japan_t1,Years))
write.csv2(Japan_p, file = "Data_4digits_analysis/JapanYearsAndIPCcodes4digits.csv", row.names = TRUE)
rm(list=ls())

Japan_p <-read.csv("Data_4digits_analysis/JapanYearsAndIPCcodes4digits.csv", sep = ";", header = TRUE)
Japan_p <- Japan_p[,(-1)]

tx <- 't'
uu <- function (r) {
  for (i in 1:dim(r)[1]){
    tx[i] <- paste(r[i,2],tx[i-1],sep = " ")
  }
  return(cbind(tx[1:i]))  
}

Japan_p$accumulated<- uu(Japan_p)
Japan_p$accumulated <- gsub("\\s+", " ", str_trim(Japan_p$accumulated))

Japan_conv4dig <- function (r) {
  for (i in 1:dim(r)[1]){
    tx[i+1] <- (length(setdiff(unlist(strsplit(as.character(r[i+1,dim(r)[2]]), "\\ ")), 
                               unlist(strsplit(as.character(r[i, dim(r)[2]]), "\\ ")))))
  }
  tx[1] <- 0
  return(cbind(tx[1:i]))
}

Japan_p$accumulated2 <- Japan_conv4dig(Japan_p)
write.csv2(Japan_p, file = "Data_4digits_analysis/Japan4DigitsCodesAccumulated.csv", row.names = TRUE)

###1.2.3. WORLD IPC-----
rm(list=ls())

IPC4digitsOverYears <-read.csv("Data_4digits_analysis/4digits.csv", sep = ";", header = TRUE)
Codes4digits <- IPC4digitsOverYears[,1]
NumberOfOccurrences4digits <- IPC4digitsOverYears[,(-1)]

FunctionforCombiningCodesWithValues <- function(a,b) {
  for (i in 1:dim(a)[2]){
    a[,i] <- paste(b,a[,i],sep = "-")
  }
  return(cbind(a[,1:i]))
}

write.csv2(FunctionforCombiningCodesWithValues(NumberOfOccurrences4digits,Codes4digits), file = "Data_4digits_analysis/4digitstt77.csv", row.names = TRUE)
rm(list=ls())

aggdata <-read.csv("Data_4digits_analysis/4digitstt77.csv", sep = ";", header = TRUE)
aggdata <- aggdata[,(-1)]

FunctionGrabCodes <- function(x) {
  grep("-0", x, invert=T)
}

FunctionforCombiningCodesWithValues <- function(a,b) {
  for (i in 1:dim(a)[2]){
    a[,i] <- paste(b,a[,i],sep = "-")
  }
  return(cbind(a[,1:i]))
}

j <- list(c("tete"))

ff <- function (r) {
  for (i in 1:dim(r)[2]){
    hh <- i
    j[[hh]] <- (substr(aggdata[FunctionGrabCodes(aggdata[,hh]), hh],1,4))
  }
  return(rbind(j[1:i]))
}

t1<- ff(aggdata)

Years <- (1960:2018)
Years <- cbind(Years)

gg <- function (r,v) {
  for (i in 1:dim(v)[1]){
    hh <- i
    tx[hh] <- paste( unlist(r[[hh]]), collapse=' ')
  }
  return(cbind(tx[1:i]))
}
tx <- 't'
p <- cbind(Years, gg(t1,Years))
write.csv2(p, file = "Data_4digits_analysis/YearsAndIPCcodes4digits.csv", row.names = TRUE)

rm(list=ls())
p <-read.csv("Data_4digits_analysis/YearsAndIPCcodes4digits.csv", sep = ";", header = TRUE)
p <- p[,(-1)]

tx <- 't'
uu <- function (r) {
  for (i in 1:dim(r)[1]){
    tx[i] <- paste(r[i,2],tx[i-1],sep = " ")
  }
  return(cbind(tx[1:i]))  
}

p$accumulated<- uu(p)
p$accumulated <- gsub("\\s+", " ", str_trim(p$accumulated))

conv4dig <- function (r) {
  for (i in 1:dim(r)[1]){
    tx[i+1] <- (length(setdiff(unlist(strsplit(as.character(r[i+1,dim(r)[2]]), "\\ ")), 
                               unlist(strsplit(as.character(r[i, dim(r)[2]]), "\\ ")))))
  }
  tx[1] <- 0
  return(cbind(tx[1:i]))
}
p$accumulated2 <- conv4dig(p)
write.csv2(p, file = "Data_4digits_analysis/4DigitsCodesAccumulated.csv", row.names = TRUE)

# AGGREGATED and COUNTRY data 
rm(list=ls())
World_p <-read.csv("Data_4digits_analysis/4DigitsCodesAccumulated.csv", sep = ";", header = TRUE)
World_p <- World_p[,c(-1),]
names(World_p) <- c("Years", "World_NewCodeYear", "World_Accumulated", "World_NumberOfNewCodes")

China_p <- read.csv("Data_4digits_analysis/China4DigitsCodesAccumulated.csv", sep = ";", header = TRUE)
China_p <- China_p[,(-1)]
names(China_p) <- c("Years", "China_NewCodeYear", "China_Accumulated", "China_NumberOfNewCodes")

SouthKorea_p <- read.csv("Data_4digits_analysis/SouthKorea4DigitsCodesAccumulated.csv", sep = ";", header = TRUE)
SouthKorea_p <- SouthKorea_p[,(-1)]
names(SouthKorea_p) <- c("Years", "SouthKorea_NewCodeYear", "SouthKorea_Accumulated", "SouthKorea_NumberOfNewCodes")

US_p <- read.csv("Data_4digits_analysis/US4DigitsCodesAccumulated.csv", sep = ";", header = TRUE)
US_p <- US_p[,c(-1)]
names(US_p) <- c("Years", "US_NewCodeYear", "US_Accumulated", "US_NumberOfNewCodes")

Japan_p <- read.csv("Data_4digits_analysis/Japan4DigitsCodesAccumulated.csv", sep = ";", header = TRUE)
Japan_p <- Japan_p[,c(-1)]
names(Japan_p) <- c("Years", "Japan_NewCodeYear", "Japan_Accumulated", "Japan_NumberOfNewCodes")

All_data <- merge(World_p, c(China_p, SouthKorea_p, US_p, Japan_p), by= "Years")
All_data <- All_data[,c((-8),(-12),(-16))]
write.csv2(All_data, file = "Data_4digits_analysis/Alldata4digits.csv", row.names = TRUE)

# New combinations regarding the aggregated corpus of knowledge
rm(list=ls())
All_data <-read.csv("Data_4digits_analysis/Alldata4digits.csv", sep = ";", header = TRUE)

tx <- 'tst'

ComparCountries <- function (W,C) {
  for (i in 1:59){
    tx[i+1] <- (length(setdiff(unlist(strsplit(as.character(C[i+1]), "\\ ")), 
                               unlist(strsplit(as.character(W[i]), "\\ ")))))
  }
  tx[1] <- 0
  return(cbind(tx[1:i]))
}


All_data$China_NewRegisters <- ComparCountries(All_data$World_Accumulated, All_data$China_Accumulated)
All_data$SouthKorea_NewRegisters <- ComparCountries(All_data$World_Accumulated, All_data$SouthKorea_Accumulated)
All_data$US_NewRegisters <- ComparCountries(All_data$World_Accumulated, All_data$US_Accumulated)
All_data$Japan_NewRegisters <- ComparCountries(All_data$World_Accumulated, All_data$Japan_Accumulated)

write.csv2(All_data, file = "Data_4digits_analysis/AlldataIPCANDCountriesFirstcombinations.csv", row.names = TRUE)

#1.3.Visualisation ----
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
DataforCalculationPat <- read.csv("Data_4digits_analysis/AlldataIPCANDCountriesFirstcombinations.csv", sep = ";", header = TRUE, dec = ",")

NewVariety <- DataforCalculationPat[,c(3,6,19:22)]#
names(NewVariety) <- c("Years", "World", "China", "SouthKorea", "US", "Japan")
AccumulatedVariety <- DataforCalculationPat[,c(3,6,9,12,15,18)]
names(AccumulatedVariety) <- c("Years", "World", "China", "SouthKorea", "US", "Japan")

SummarizedData<- (NewVariety %>% gather(key = Years, value = "New_Variety"))
Years <- c(1960:2018, 1960:2018, 1960:2018, 1960:2018, 1960:2018)

DataforCalculationPat2<- cbind(Years,SummarizedData)
names(DataforCalculationPat2) <- c("Years", "Country", "Value")
DataforCalculationPat2$Parameter <- "New variety"

AccumulatedVariety$Sum_World <- cumsum(AccumulatedVariety$World)
AccumulatedVariety$Sum_China <- cumsum(AccumulatedVariety$China)
AccumulatedVariety$Sum_SouthKorea <- cumsum(AccumulatedVariety$SouthKorea)
AccumulatedVariety$Sum_US <- cumsum(AccumulatedVariety$US)
AccumulatedVariety$Sum_Japan <- cumsum(AccumulatedVariety$Japan)

DataforCalculationPat3 <- AccumulatedVariety[,c(1,7:11)]
SummarizedData2<- (DataforCalculationPat3 %>% gather(key = Years, value = "Acc_Variety"))
DataforCalculationPat3<- cbind(Years,SummarizedData2)
names(DataforCalculationPat3) <- c("Years", "Country", "Value")
DataforCalculationPat3$Parameter <- "Accumulated variety"

DataforCalculationPat <- rbind(DataforCalculationPat3, DataforCalculationPat2)

library(ggplot2)
library(plyr)

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

DataforCalculationPat <-DataforCalculationPat[DataforCalculationPat$Years > 1969,]
DataforCalculationNewregistersPat <- DataforCalculationPat[DataforCalculationPat$Parameter == "New variety",]

#excluir world:
DataforCalculationNewregistersPat <- DataforCalculationNewregistersPat[DataforCalculationNewregistersPat$Country != "World",]

DataforCalculationAccumulatedPat <- DataforCalculationPat[DataforCalculationPat$Parameter == "Accumulated variety",]

fnewpat2 <- ggplot(data=DataforCalculationNewregistersPat, aes(x=Years, y=Value, group=Country, colour=Country)) +
  geom_line(size=1.2) +
  geom_point(size=3) +
  ggtitle("a) First occurrence of new 4-digits IPC codes involving AI priority patents over the years") +
  xlab(NULL) +
  ylab("Number of new 4-digits IPC codes")+ 
  scale_colour_manual(values = c("red3", "darkorchid4", "chartreuse4", "dodgerblue2", "black"))+
  scale_x_continuous(breaks = c(1974, 1988, 2003, 2018), limits=c(1970, 2018))

faccpat2 <- ggplot(data=DataforCalculationAccumulatedPat, aes(x=Years, y=Value, group=Country, colour=Country)) +
  geom_line(size=1.2) +
  geom_point(size=3)+
  ggtitle("b) Accumulated number of 4-digits IPC codes involving AI priority patents  over the years") +
  xlab(NULL) +
  ylab("Accumulated number of 4-digits IPC codes")+ 
  scale_colour_manual(values = c("red3", "darkorchid4", "chartreuse4", "dodgerblue2", "black"))+
  scale_x_continuous(breaks = c(1974, 1988, 2003, 2018), limits=c(1970, 2018)) #1974, 1988, 2003, 2018

tiff("Data_4digits_analysis/PlotVariety.tiff", width = 14, height = 7, units = 'in', res = 300)
multiplot(fnewpat2, faccpat2, cols=1)
dev.off()

