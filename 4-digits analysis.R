library(EconGeo) # Economic Geography functions
library("data.table") #for reading the big files using fread and for replacing countries names (by AI_pat for example)
library(tidyverse) # Collection of all the good stuff like dplyr, ggplot2 ect.
library(magrittr) # For extra-piping operators (eg. %<>%)
library(stringr) #for separating the IPC codes in subclasses
#1.FIRST PART: RCA General versus AI specific----
rm(list=ls())

group_by_applnID <- function (data){
  data %>%
    group_by(appln_id) %>%
    mutate(field_weight = 1 / n()) %>%
    ungroup()
}

group_by_ctry_and_IPC <- function (data){
  data %<>%
    group_by(ctry_code, Subclass) %>%
    summarise(n_tech_reg = sum(field_weight)) %>%
    ungroup() %>%
    drop_na() 
}

#1.1.First Period ----
#1.1.1.General Perspective ----
#For the first period, which goes from 1974 to 1988, we need only the dataset from Part2:
setwd("C:/Users/mathe/OneDrive/Área de Trabalho")
c <- 97664418 -80000000
IPC_all_patents_Part1 <- fread("All_patents_and_IPC_codes_Part2.csv", header = F, nrow = 20000000)
IPC_all_patents_Part2 <- fread("All_patents_and_IPC_codes_Part2.csv", header = F, nrow = 20000000, skip = 20000000)
IPC_all_patents_Part3 <- fread("All_patents_and_IPC_codes_Part2.csv", header = F, nrow = 20000000, skip = 40000000)
IPC_all_patents_Part4 <- fread("All_patents_and_IPC_codes_Part2.csv", header = F, nrow = 20000000, skip = 60000000)
IPC_all_patents_Part5 <- fread("All_patents_and_IPC_codes_Part2.csv", header = F, nrow = c, skip = 80000000)

names(IPC_all_patents_Part1) <- c("appln_id", "ctry_code", "ipc_class_symbol", "priority_year")
names(IPC_all_patents_Part2) <- c("appln_id", "ctry_code", "ipc_class_symbol", "priority_year")
names(IPC_all_patents_Part3) <- c("appln_id", "ctry_code", "ipc_class_symbol", "priority_year")
names(IPC_all_patents_Part4) <- c("appln_id", "ctry_code", "ipc_class_symbol", "priority_year")
names(IPC_all_patents_Part5) <- c("appln_id", "ctry_code", "ipc_class_symbol", "priority_year")

#we want to pick only the registers from the period we want (from 1974 to 1988, including both cited years)
a = 1973
b = 1989

IPC_all_patents_Part1 <- IPC_all_patents_Part1[IPC_all_patents_Part1$priority_year < b,]
IPC_all_patents_Part1 <- IPC_all_patents_Part1[IPC_all_patents_Part1$priority_year > a,]

IPC_all_patents_Part2 <- IPC_all_patents_Part2[IPC_all_patents_Part2$priority_year < b,]
IPC_all_patents_Part2 <- IPC_all_patents_Part2[IPC_all_patents_Part2$priority_year > a,]

IPC_all_patents_Part3 <- IPC_all_patents_Part3[IPC_all_patents_Part3$priority_year < b,]
IPC_all_patents_Part3 <- IPC_all_patents_Part3[IPC_all_patents_Part3$priority_year > a,]

IPC_all_patents_Part4 <- IPC_all_patents_Part4[IPC_all_patents_Part4$priority_year < b,]
IPC_all_patents_Part4 <- IPC_all_patents_Part4[IPC_all_patents_Part4$priority_year > a,]

IPC_all_patents_Part5 <- IPC_all_patents_Part5[IPC_all_patents_Part5$priority_year < b,]
IPC_all_patents_Part5 <- IPC_all_patents_Part5[IPC_all_patents_Part5$priority_year > a,]

#let's drop the columns we won't use (priority_year):
IPC_all_patents_Part1 <- IPC_all_patents_Part1[, c((-4))]
IPC_all_patents_Part2 <- IPC_all_patents_Part2[, c((-4))]
IPC_all_patents_Part3 <- IPC_all_patents_Part3[, c((-4))]
IPC_all_patents_Part4 <- IPC_all_patents_Part4[, c((-4))]
IPC_all_patents_Part5 <- IPC_all_patents_Part5[, c((-4))]

#we combine the 3 files:
IPC_all_patents_FirstPeriod <- rbind(IPC_all_patents_Part1, IPC_all_patents_Part2, IPC_all_patents_Part3, IPC_all_patents_Part4, IPC_all_patents_Part5)
#and exclude the 3 big ones we just used, so we have back our memory:
rm(IPC_all_patents_Part1, IPC_all_patents_Part2, IPC_all_patents_Part3, IPC_all_patents_Part4, IPC_all_patents_Part5)

#we pick just the subclass for analysis:
IPC_all_patents_FirstPeriod$Subclass <- substr(IPC_all_patents_FirstPeriod$ipc_class_symbol,1,4)

#now we apply the 2 functions we created at the beginning of this section:
reg_tech1 <- group_by_applnID(IPC_all_patents_FirstPeriod)
rm(IPC_all_patents_FirstPeriod)
reg_tech1 <- group_by_ctry_and_IPC(reg_tech1)

mat_reg_tech1 <- reg_tech1 %>%
  arrange(Subclass, ctry_code) %>%
  pivot_wider(names_from = Subclass, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

mat_reg_tech1 %<>% remove_rownames %>% column_to_rownames(var="ctry_code") %>%
  as.matrix() %>%
  round()

reg_RCA1 <- mat_reg_tech1 %>% location.quotient(binary = F) %>% 
  as.data.frame() %>% 
  rownames_to_column("ctry_code") %>% 
  as_tibble() %>% 
  gather(key = "Subclass", value = "RCA", -ctry_code) %>%
  arrange(ctry_code, Subclass)

#1.1.2. AI perspective ----
#For the first period, which goes from 1974 to 1988, we need only the dataset from Part2:
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

patents_AI_specific_1st <- read.csv("Data_4digits_analysis/IPCs_AI csv.csv", sep = ";", header = TRUE, dec=",")

a = 1973
b = 1989

patents_AI_specific_1st <- patents_AI_specific_1st[patents_AI_specific_1st$priority_year < b,]
patents_AI_specific_1st <- patents_AI_specific_1st[patents_AI_specific_1st$priority_year > a,]

#now we pick just the subclass for analysis:
patents_AI_specific_1st$Subclass <- substr(patents_AI_specific_1st$ipc_class_symbol,1,4)

#and apply the 2 functions we created at the beginning of this code:
reg_tech1 <- group_by_applnID(patents_AI_specific_1st)
rm(patents_AI_specific_1st)
reg_tech1 <- group_by_ctry_and_IPC(reg_tech1)

mat_reg_tech1 <- reg_tech1 %>%
  arrange(Subclass, ctry_code) %>%
  pivot_wider(names_from = Subclass, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

mat_reg_tech1 %<>% remove_rownames %>% column_to_rownames(var="ctry_code") %>%
  as.matrix() %>%
  round()

reg_RCA1_AIspecific <- mat_reg_tech1 %>% location.quotient(binary = F) %>% 
  as.data.frame() %>% 
  rownames_to_column("ctry_code") %>% 
  as_tibble() %>% 
  gather(key = "Subclass", value = "RCA", -ctry_code) %>%
  arrange(ctry_code, Subclass)

Data1period <- merge(reg_RCA1, reg_RCA1_AIspecific, all=T, by=c("ctry_code", "Subclass"))
Data1period$Period <- "1974-1988"
names(Data1period) <- c("ctry_code", "Subclass", "RCA_Gen", "RCA_AI", "Period")
write.csv2(Data1period, file = "Data_4digits_analysis/Data1period_RCA.csv", row.names = F)
rm(mat_reg_tech1)
#1.2.Second Period ----
#1.2.1.General Perspective ----
#For the second period, which goes from 1989 to 2003, we need only the dataset from Part2:
setwd("C:/Users/mathe/OneDrive/Área de Trabalho")
c <- 97664418 -80000000
IPC_all_patents_Part1 <- fread("All_patents_and_IPC_codes_Part2.csv", header = F, nrow = 20000000)
IPC_all_patents_Part2 <- fread("All_patents_and_IPC_codes_Part2.csv", header = F, nrow = 20000000, skip = 20000000)
IPC_all_patents_Part3 <- fread("All_patents_and_IPC_codes_Part2.csv", header = F, nrow = 20000000, skip = 40000000)
IPC_all_patents_Part4 <- fread("All_patents_and_IPC_codes_Part2.csv", header = F, nrow = 20000000, skip = 60000000)
IPC_all_patents_Part5 <- fread("All_patents_and_IPC_codes_Part2.csv", header = F, nrow = c, skip = 80000000)

names(IPC_all_patents_Part1) <- c("appln_id", "ctry_code", "ipc_class_symbol", "priority_year")
names(IPC_all_patents_Part2) <- c("appln_id", "ctry_code", "ipc_class_symbol", "priority_year")
names(IPC_all_patents_Part3) <- c("appln_id", "ctry_code", "ipc_class_symbol", "priority_year")
names(IPC_all_patents_Part4) <- c("appln_id", "ctry_code", "ipc_class_symbol", "priority_year")
names(IPC_all_patents_Part5) <- c("appln_id", "ctry_code", "ipc_class_symbol", "priority_year")

#we want to pick only the registers from the period we want (from 1989 to 2003, including both cited years)
a = 1988
b = 2004

IPC_all_patents_Part1 <- IPC_all_patents_Part1[IPC_all_patents_Part1$priority_year < b,]
IPC_all_patents_Part1 <- IPC_all_patents_Part1[IPC_all_patents_Part1$priority_year > a,]

IPC_all_patents_Part2 <- IPC_all_patents_Part2[IPC_all_patents_Part2$priority_year < b,]
IPC_all_patents_Part2 <- IPC_all_patents_Part2[IPC_all_patents_Part2$priority_year > a,]

IPC_all_patents_Part3 <- IPC_all_patents_Part3[IPC_all_patents_Part3$priority_year < b,]
IPC_all_patents_Part3 <- IPC_all_patents_Part3[IPC_all_patents_Part3$priority_year > a,]

IPC_all_patents_Part4 <- IPC_all_patents_Part4[IPC_all_patents_Part4$priority_year < b,]
IPC_all_patents_Part4 <- IPC_all_patents_Part4[IPC_all_patents_Part4$priority_year > a,]

IPC_all_patents_Part5 <- IPC_all_patents_Part5[IPC_all_patents_Part5$priority_year < b,]
IPC_all_patents_Part5 <- IPC_all_patents_Part5[IPC_all_patents_Part5$priority_year > a,]

#let's drop the column we won't use (priority_year):
IPC_all_patents_Part1 <- IPC_all_patents_Part1[, c((-4))]
IPC_all_patents_Part2 <- IPC_all_patents_Part2[, c((-4))]
IPC_all_patents_Part3 <- IPC_all_patents_Part3[, c((-4))]
IPC_all_patents_Part4 <- IPC_all_patents_Part4[, c((-4))]
IPC_all_patents_Part5 <- IPC_all_patents_Part5[, c((-4))]

#we combine the 5 files:
IPC_all_patents_SecondPeriod <- rbind(IPC_all_patents_Part1, IPC_all_patents_Part2, IPC_all_patents_Part3, IPC_all_patents_Part4, IPC_all_patents_Part5)
#and exclude the 5 big ones we just used, so we have back our memory:
rm(IPC_all_patents_Part1, IPC_all_patents_Part2, IPC_all_patents_Part3, IPC_all_patents_Part4, IPC_all_patents_Part5)

#we pick just the subclass for analysis:
IPC_all_patents_SecondPeriod$Subclass <- substr(IPC_all_patents_SecondPeriod$ipc_class_symbol,1,4)

#now we apply the 2 functions we created at the beginning of this section:
reg_tech2 <- group_by_applnID(IPC_all_patents_SecondPeriod)
rm(IPC_all_patents_SecondPeriod)
reg_tech2 <- group_by_ctry_and_IPC(reg_tech2)

mat_reg_tech2 <- reg_tech2 %>%
  arrange(Subclass, ctry_code) %>%
  pivot_wider(names_from = Subclass, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

mat_reg_tech2 %<>% remove_rownames %>% column_to_rownames(var="ctry_code") %>%
  as.matrix() %>%
  round()

reg_RCA2 <- mat_reg_tech2 %>% location.quotient(binary = F) %>% 
  as.data.frame() %>% 
  rownames_to_column("ctry_code") %>% 
  as_tibble() %>% 
  gather(key = "Subclass", value = "RCA", -ctry_code) %>%
  arrange(ctry_code, Subclass)

#1.2.2. AI perspective ----
#For the first period, which goes from 1989 to 2003, we need only the dataset from Part2:
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

patents_AI_specific_2nd <- read.csv("Data_4digits_analysis/IPCs_AI csv.csv", sep = ";", header = TRUE, dec=",")

a = 1988
b = 2004

patents_AI_specific_2nd <- patents_AI_specific_2nd[patents_AI_specific_2nd$priority_year < b,]
patents_AI_specific_2nd <- patents_AI_specific_2nd[patents_AI_specific_2nd$priority_year > a,]

#now we pick just the subclass for analysis:
patents_AI_specific_2nd$Subclass <- substr(patents_AI_specific_2nd$ipc_class_symbol,1,4)

#and apply the 2 functions we created at the beginning of this code:
reg_tech2 <- group_by_applnID(patents_AI_specific_2nd)
rm(patents_AI_specific_2nd)
reg_tech2 <- group_by_ctry_and_IPC(reg_tech2)

mat_reg_tech2 <- reg_tech2 %>%
  arrange(Subclass, ctry_code) %>%
  pivot_wider(names_from = Subclass, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

mat_reg_tech2 %<>% remove_rownames %>% column_to_rownames(var="ctry_code") %>%
  as.matrix() %>%
  round()

reg_RCA2_AIspecific <- mat_reg_tech2 %>% location.quotient(binary = F) %>% 
  as.data.frame() %>% 
  rownames_to_column("ctry_code") %>% 
  as_tibble() %>% 
  gather(key = "Subclass", value = "RCA", -ctry_code) %>%
  arrange(ctry_code, Subclass)

Data2period <- merge(reg_RCA2, reg_RCA2_AIspecific, all=T, by=c("ctry_code", "Subclass"))
Data2period$Period <- "1989-2003"
names(Data2period) <- c("ctry_code", "Subclass", "RCA_Gen", "RCA_AI", "Period")
write.csv2(Data2period, file = "Data_4digits_analysis/Data2period_RCA.csv", row.names = F)
rm(mat_reg_tech2)

#1.3.Third Period -----
#1.3.1. General Perspective ----
#For the third period, which goes from 2004 to 2018, we  need only the dataset from Part1. This
#specific dataset only has patents from 2004 to 2018, so we don't have to filter it. But calculating
#the reg_tech is very computationally expansive, so we divide that into 6 parts.
setwd("C:/Users/mathe/OneDrive/Área de Trabalho")
c <- 120419184-100000000
IPC_all_patents_Part1 <- fread("All_patents_and_IPC_codes_Part1.csv", header = F, nrow = 20000000)
IPC_all_patents_Part2 <- fread("All_patents_and_IPC_codes_Part1.csv", header = F, nrow = 20000000, skip = 20000000)
IPC_all_patents_Part3 <- fread("All_patents_and_IPC_codes_Part1.csv", header = F, nrow = 20000000, skip = 40000000)

names(IPC_all_patents_Part1) <- c("appln_id", "ctry_code", "ipc_class_symbol", "priority_year")
names(IPC_all_patents_Part2) <- c("appln_id", "ctry_code", "ipc_class_symbol", "priority_year")
names(IPC_all_patents_Part3) <- c("appln_id", "ctry_code", "ipc_class_symbol", "priority_year")

IPC_all_patents_Part1 <- IPC_all_patents_Part1[, c((-4))]
IPC_all_patents_Part2 <- IPC_all_patents_Part2[, c((-4))]
IPC_all_patents_Part3 <- IPC_all_patents_Part3[, c((-4))]

IPC_all_patents_Part1$Subclass <- substr(IPC_all_patents_Part1$ipc_class_symbol,1,4)
IPC_all_patents_Part2$Subclass <- substr(IPC_all_patents_Part2$ipc_class_symbol,1,4)
IPC_all_patents_Part3$Subclass <- substr(IPC_all_patents_Part3$ipc_class_symbol,1,4)

#here we divide our calculations of the reg_tech (which was not necessary on the 2 previous periods)
reg_tech4 <- group_by_applnID(IPC_all_patents_Part1)
rm(IPC_all_patents_Part1)
reg_tech4 <- group_by_ctry_and_IPC(reg_tech4)

reg_tech5 <- group_by_applnID(IPC_all_patents_Part2)
rm(IPC_all_patents_Part2)
reg_tech5 <- group_by_ctry_and_IPC(reg_tech5)

reg_tech6 <- group_by_applnID(IPC_all_patents_Part3)
rm(IPC_all_patents_Part3)
reg_tech6 <- group_by_ctry_and_IPC(reg_tech6)

#now we merge them
reg_tech3 <- merge(reg_tech4, reg_tech5, all=T, by=c("ctry_code", "Subclass"))
reg_tech3 <- merge(reg_tech3, reg_tech6, all=T, by=c("ctry_code", "Subclass"))
rm(reg_tech4, reg_tech5, reg_tech6)
#second part:
IPC_all_patents_Part4 <- fread("All_patents_and_IPC_codes_Part1.csv", header = F, nrow = 20000000, skip = 60000000)
IPC_all_patents_Part5 <- fread("All_patents_and_IPC_codes_Part1.csv", header = F, nrow = 20000000, skip = 80000000)
IPC_all_patents_Part6 <- fread("All_patents_and_IPC_codes_Part1.csv", header = F, nrow = c, skip = 100000000)

names(IPC_all_patents_Part4) <- c("appln_id", "ctry_code", "ipc_class_symbol", "priority_year")
names(IPC_all_patents_Part5) <- c("appln_id", "ctry_code", "ipc_class_symbol", "priority_year")
names(IPC_all_patents_Part6) <- c("appln_id", "ctry_code", "ipc_class_symbol", "priority_year")

IPC_all_patents_Part4 <- IPC_all_patents_Part4[, c((-4))]
IPC_all_patents_Part5 <- IPC_all_patents_Part5[, c((-4))]
IPC_all_patents_Part6 <- IPC_all_patents_Part6[, c((-4))]

IPC_all_patents_Part4$Subclass <- substr(IPC_all_patents_Part4$ipc_class_symbol,1,4)
IPC_all_patents_Part5$Subclass <- substr(IPC_all_patents_Part5$ipc_class_symbol,1,4)
IPC_all_patents_Part6$Subclass <- substr(IPC_all_patents_Part6$ipc_class_symbol,1,4)

reg_tech7 <- group_by_applnID(IPC_all_patents_Part4)
rm(IPC_all_patents_Part4)
reg_tech7 <- group_by_ctry_and_IPC(reg_tech7)

reg_tech8 <- group_by_applnID(IPC_all_patents_Part5)
rm(IPC_all_patents_Part5)
reg_tech8 <- group_by_ctry_and_IPC(reg_tech8)

reg_tech9 <- group_by_applnID(IPC_all_patents_Part6)
rm(IPC_all_patents_Part6)
reg_tech9 <- group_by_ctry_and_IPC(reg_tech9)

reg_tech4 <- merge(reg_tech7, reg_tech8, all=T, by=c("ctry_code", "Subclass"))
reg_tech4 <- merge(reg_tech4, reg_tech9, all=T, by=c("ctry_code", "Subclass"))

rm(reg_tech7, reg_tech8, reg_tech9)

#replace NAs, so we don't have problems when summing:
reg_tech3[is.na(reg_tech3)] <- 0
reg_tech4[is.na(reg_tech4)] <- 0

#do the summ, exclude the tables used, and rename the dataset accordingly:
reg_tech3$sum <- rowSums(reg_tech3[,c(3:5)])
reg_tech3 <- reg_tech3[, c((-3), (-4), (-5))]
names(reg_tech3) <- c("ctry_code", "Subclass", "n_tech_reg")

reg_tech4$sum <- rowSums(reg_tech4[,c(3:5)])
reg_tech4 <- reg_tech4[, c((-3), (-4), (-5))]
names(reg_tech4) <- c("ctry_code", "Subclass", "n_tech_reg")

reg_tech5 <- merge(reg_tech3, reg_tech4, all=T, by=c("ctry_code", "Subclass"))
reg_tech5[is.na(reg_tech5)] <- 0
reg_tech5$sum <- rowSums(reg_tech5[,c(3:4)])
reg_tech5 <- reg_tech5[, c((-3), (-4))]
names(reg_tech5) <- c("ctry_code", "Subclass", "n_tech_reg")

mat_reg_tech3 <- reg_tech4 %>%
  arrange(Subclass, ctry_code) %>%
  pivot_wider(names_from = Subclass, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

mat_reg_tech3 %<>% remove_rownames %>% column_to_rownames(var="ctry_code") %>%
  as.matrix() %>%
  round()

reg_RCA3 <- mat_reg_tech3 %>% location.quotient(binary = F) %>% 
  as.data.frame() %>% 
  rownames_to_column("ctry_code") %>% 
  as_tibble() %>% 
  gather(key = "Subclass", value = "RCA", -ctry_code) %>%
  arrange(ctry_code, Subclass)

#1.3.2. AI Perspective  ----
#For the third period, which goes from 2004 to 2018:
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
patents_AI_specific_3rd <- read.csv("Data_4digits_analysis/IPCs_AI csv.csv", sep = ";", header = TRUE, dec=",")

a = 2003
b = 2019

patents_AI_specific_3rd <- patents_AI_specific_3rd[patents_AI_specific_3rd$priority_year < b,]
patents_AI_specific_3rd <- patents_AI_specific_3rd[patents_AI_specific_3rd$priority_year > a,]

#now we pick just the subclass for analysis:
patents_AI_specific_3rd$Subclass <- substr(patents_AI_specific_3rd$ipc_class_symbol,1,4)

#let's drop the columns we won't use:
patents_AI_specific_3rd <- patents_AI_specific_3rd[, c((-2), (-3), (-6), (-7), (-8), (-10))]

#now we apply the 2 functions we created at the beginning of this section:
reg_tech3 <- group_by_applnID(patents_AI_specific_3rd)
rm(patents_AI_specific_3rd)
reg_tech3 <- group_by_ctry_and_IPC(reg_tech3)

###Third Period:
mat_reg_tech3 <- reg_tech3 %>%
  arrange(Subclass, ctry_code) %>%
  pivot_wider(names_from = Subclass, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

mat_reg_tech3 %<>% remove_rownames %>% column_to_rownames(var="ctry_code") %>%
  as.matrix() %>%
  round()

reg_RCA3_AIspecific <- mat_reg_tech3 %>% location.quotient(binary = F) %>% 
  as.data.frame() %>% 
  rownames_to_column("ctry_code") %>% 
  as_tibble() %>% 
  gather(key = "Subclass", value = "RCA", -ctry_code) %>%
  arrange(ctry_code, Subclass)

Data3period <- merge(reg_RCA3, reg_RCA3_AIspecific, all=T, by=c("ctry_code", "Subclass"))
Data3period$Period <- "2004-2018"
names(Data3period) <- c("ctry_code", "Subclass", "RCA_Gen", "RCA_AI", "Period")
write.csv2(Data3period, file = "Data_4digits_analysis/Data3period_RCA.csv", row.names = F)

IPC_RCAs <- rbind(Data1period, Data2period, Data3period)
write.csv2(IPC_RCAs, file = "Data_4digits_analysis/IPC_RCAs.csv", row.names = F)
#1.4.Visualisation RCAs ----
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

library(plyr)

#Select the 4 countries we want
IPC_RCAs_Top4 <- IPC_RCAs[IPC_RCAs$ctry_code == "CN" | 
                            IPC_RCAs$ctry_code == "KR"| 
                            IPC_RCAs$ctry_code == "US"| 
                            IPC_RCAs$ctry_code == "JP", ]

IPC_RCAs_Top4$ctry_code <- as.vector(IPC_RCAs_Top4$ctry_code)

#add new label data:
IPC2LabelData <- read.csv("Data_4digits_analysis/Summary IPC labels.csv", sep = ";", header = TRUE, dec = ",")
IPC_RCAs_Top4$Label <- IPC2LabelData$Summary[match(IPC_RCAs_Top4$Subclass, IPC2LabelData$Subclass)]

#select only the top10 labels
IPC_RCAs_Top5 <- IPC_RCAs_Top4
IPC_RCAs_Top4<- IPC_RCAs_Top4[rowSums(is.na(IPC_RCAs_Top4)) == 0,]

#replace names:
IPC_RCAs_Top4$ctry_code <- gsub("US", "USA", str_trim(IPC_RCAs_Top4$ctry_code))
IPC_RCAs_Top4$ctry_code <- gsub("CN", "China", str_trim(IPC_RCAs_Top4$ctry_code))
IPC_RCAs_Top4$ctry_code <- gsub("JP", "Japan", str_trim(IPC_RCAs_Top4$ctry_code))
IPC_RCAs_Top4$ctry_code <- gsub("KR", "South Korea", str_trim(IPC_RCAs_Top4$ctry_code))

#Figure General:
Gen <- ddply(IPC_RCAs_Top4, c("Period", "Label"), summarise, Value.mean=log10(mean(RCA_Gen)))
FigGen <- ggplot(IPC_RCAs_Top4,aes(x = log10(RCA_Gen), y=ctry_code, color=Period)) + geom_count(shape=19, alpha=1/1.4, size=4) +
  facet_wrap(~Label, ncol = 5) +
  geom_vline(data=Gen, aes(xintercept=Value.mean,  colour=Period),
             linetype="dashed", size=1) +
  ggtitle("Countries' Performance by IPC code - General Perspective") +
  xlab("LOG10 of the Country' Revealed Comparative Advantage (RCA) index") +
  ylab(NULL)

#Figure AI:
Ais <- ddply(IPC_RCAs_Top4, c("Period", "Label"), summarise, Value.mean=log10(mean(RCA_AI)))
FigAI <- ggplot(IPC_RCAs_Top4,aes(x = log10(RCA_AI), y=ctry_code, color=Period)) + geom_count(shape=19, alpha=1/1.4, size=4) +
  facet_wrap(~Label, ncol = 5) +
  geom_vline(data=Ais, aes(xintercept=Value.mean,  colour=Period),
             linetype="dashed", size=1) +
  ggtitle("Countries' Performance by IPC code - AI-specific Perspective") +
  xlab("LOG10 of the Country' Revealed Comparative Advantage (RCA) index") +
  ylab(NULL)

tiff("Data_4digits_analysis/Plot_IPC_RCA.jpg", width = 14, height = 7, units = 'in', res = 200)
multiplot(FigGen, FigAI, cols=1)
dev.off()
#2.SECOND PART: Variety with AI----
##2.1. Creation of Variety per country ----
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

#2.1.1. CHINA IPC------
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

#2.1.2. South Korea IPC------
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

#2.1.3.US IPC------
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

#2.1.4.Japan IPC------
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

###2.1.5. WORLD IPC-----
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

#2.2.Visualisation Variety----
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

