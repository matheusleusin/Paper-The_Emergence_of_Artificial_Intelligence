library(EconGeo) # Economic Geography functions
library("data.table") #for reading the big files using fread and for replacing countries names (by AI_pat for example)
library(tidyverse) # Collection of all the good stuff like dplyr, ggplot2 ect.
library(magrittr) # For extra-piping operators (eg. %<>%)
library(stringr) #for separating the IPC codes in subclasses

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

#1.First Period ----
#1.1.General Perspective ----
#For the first period, which goes from 1974 to 1988, we need only the dataset from Part2:
setwd("C:/Users/Matheus/Desktop") 
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

rownames(mat_reg_tech1) <- mat_reg_tech1 %>% pull(ctry_code)

mat_reg_tech1 %<>% select(-ctry_code) %>%
  as.matrix() %>%
  round()

reg_RCA1 <- mat_reg_tech1 %>% location.quotient(binary = F) %>% 
  as.data.frame() %>% 
  rownames_to_column("ctry_code") %>% 
  as_tibble() %>% 
  gather(key = "Subclass", value = "RCA", -ctry_code) %>%
  arrange(ctry_code, Subclass)

#1.2. AI perspective ----
#For the first period, which goes from 1974 to 1988, we need only the dataset from Part2:
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

patents_AI_specific_1st <- read.csv("Data_IPC/IPCs_AI.csv", sep = ";", header = TRUE, dec=",")

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

rownames(mat_reg_tech1) <- mat_reg_tech1 %>% pull(ctry_code)

mat_reg_tech1 %<>% select(-ctry_code) %>%
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
write.csv2(Data1period, file = "Data_IPC/Data1period_RCA.csv", row.names = F)
rm(mat_reg_tech1)
#2.Second Period ----
#2.1.General Perspective ----
#For the second period, which goes from 1989 to 2003, we need only the dataset from Part2:
setwd("C:/Users/Matheus/Desktop") 
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

rownames(mat_reg_tech2) <- mat_reg_tech2 %>% pull(ctry_code)

mat_reg_tech2 %<>% select(-ctry_code) %>%
  as.matrix() %>%
  round()

reg_RCA2 <- mat_reg_tech2 %>% location.quotient(binary = F) %>% 
  as.data.frame() %>% 
  rownames_to_column("ctry_code") %>% 
  as_tibble() %>% 
  gather(key = "Subclass", value = "RCA", -ctry_code) %>%
  arrange(ctry_code, Subclass)

#2.2. AI perspective ----
#For the first period, which goes from 1989 to 2003, we need only the dataset from Part2:
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

patents_AI_specific_2nd <- read.csv("Data_IPC/IPCs_AI.csv", sep = ";", header = TRUE, dec=",")

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

rownames(mat_reg_tech2) <- mat_reg_tech2 %>% pull(ctry_code)

mat_reg_tech2 %<>% select(-ctry_code) %>%
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
write.csv2(Data2period, file = "Data_IPC/Data2period_RCA.csv", row.names = F)
rm(mat_reg_tech2)

#3.Third Period -----
#3.1. General Perspective ----
#For the third period, which goes from 2004 to 2018, we  need only the dataset from Part1. This
#specific dataset only has patents from 2004 to 2018, so we don't have to filter it. But calculating
#the reg_tech is very computationally expansive, so we divide that into 6 parts.
setwd("C:/Users/Matheus/Desktop") 
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

rownames(mat_reg_tech3) <- mat_reg_tech3 %>% pull(ctry_code)

mat_reg_tech3 %<>% select(-ctry_code) %>%
  as.matrix() %>%
  round()

reg_RCA3 <- mat_reg_tech3 %>% location.quotient(binary = F) %>% 
  as.data.frame() %>% 
  rownames_to_column("ctry_code") %>% 
  as_tibble() %>% 
  gather(key = "Subclass", value = "RCA", -ctry_code) %>%
  arrange(ctry_code, Subclass)

#3.2. AI Perspective  ----
#For the third period, which goes from 2004 to 2018:
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
patents_AI_specific_3rd <- read.csv("Data_IPC/IPCs_AI.csv", sep = ";", header = TRUE, dec=",")

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

rownames(mat_reg_tech3) <- mat_reg_tech3 %>% pull(ctry_code)

mat_reg_tech3 %<>% select(-ctry_code) %>%
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
write.csv2(Data3period, file = "Data_IPC/Data3period_RCA.csv", row.names = F)

IPC_RCAs <- rbind(Data1period, Data2period, Data3period)
write.csv2(IPC_RCAs, file = "Data_IPC/IPC_RCAs.csv", row.names = F)
#1.4.Visualization RCAs ----

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
IPC2LabelData <- read.csv("Data_IPC/Summary IPC labels.csv", sep = ";", header = TRUE, dec = ",")
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

tiff("Figures_IPC/Plot_IPC_RCA.jpg", width = 14, height = 7, units = 'in', res = 200)
multiplot(FigGen, FigAI, cols=1)
dev.off()
