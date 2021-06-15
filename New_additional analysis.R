library(tidyverse) # Collection of all the good stuff like dplyr, ggplot2 ect.
library(magrittr) # For extra-piping operators (eg. %<>%)

# Network specific
library(tidygraph) # For tidy-style graph manipulation
library(ggraph) # For ggplot2 style graph plotting

library(EconGeo) # Economic Geography functions
library(data.table) #for reading the big files using fread and for replacing countries names (by AI_pat for example)

library(netrankr) #library for calculating pagerank related indicators (i.e. centrality_closeness_harmonic and centrality_closeness_residual)

library(dplyr)
library(tidyr)
library(ggrepel)
library(scales) #for scaling without cutting data out
library(patchwork) #for cutting out the X labs while keeping the legend
library(RColorBrewer)

library(ggforce) #for using geom_mark_hull

library(stringr) #for separating the IPC codes in subclasses
library(magrittr) # For extra-piping operators (eg. %<>%)

#update R studio shell (if needed, in the case of cloning this repository)
#shell("git remote add origin https://github.com/matheusleusin/Paper-The_Emergence_of_Artificial_Intelligence.git",intern = TRUE)
#shell("git push -u origin master",intern = TRUE)

#1. FIRST PART: Technological spaces -----
#In this first part, we calculate and plot the global technological space, and the technological spaces for 
#countries and AI;

#1.1. Sparse matrix -----
#On this first part we will create the sparse matrix, calculate the similarity matrix and save it in a csv file 
#named "Matrix_IPC"
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#1.FIRST PART: RCA General versus AI specific----
group_by_applnID <- function (data){
  data %>%
    group_by(appln_id) %>%
    mutate(field_weight = 1 / n()) %>%
    ungroup()
}

group_by_ctry_and_IPC <- function (data){
  data %<>%
    group_by(ctry_code, techn_field_nr) %>%
    summarise(n_tech_reg = sum(field_weight)) %>%
    ungroup() %>%
    drop_na() 
}

#1.1.First Period ----
#1.1.1.General Perspective ----
#For the first period, which goes from 1974 to 1988, we need only the dataset from Part2:
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("large_files")

c <- 45182803 -40000000
IPC_all_patents_Part1 <- fread("All_patents_and_IPCs_Part2.csv", header = F, nrow = 20000000)
IPC_all_patents_Part2 <- fread("All_patents_and_IPCs_Part2.csv", header = F, nrow = 20000000, skip = 20000000)
IPC_all_patents_Part3 <- fread("All_patents_and_IPCs_Part2.csv", header = F, nrow = c, skip = 40000000)

names(IPC_all_patents_Part1) <- c("appln_id", "ctry_code", "techn_field_nr", "weight", "priority_year")
names(IPC_all_patents_Part2) <- c("appln_id", "ctry_code", "techn_field_nr", "weight", "priority_year")
names(IPC_all_patents_Part3) <- c("appln_id", "ctry_code", "techn_field_nr", "weight", "priority_year")

#we want to pick only the registers from the period we want (from 1974 to 1988, including both cited years)
a = 1973
b = 1989

IPC_all_patents_Part1 <- IPC_all_patents_Part1[IPC_all_patents_Part1$priority_year < b,]
IPC_all_patents_Part1 <- IPC_all_patents_Part1[IPC_all_patents_Part1$priority_year > a,]

IPC_all_patents_Part2 <- IPC_all_patents_Part2[IPC_all_patents_Part2$priority_year < b,]
IPC_all_patents_Part2 <- IPC_all_patents_Part2[IPC_all_patents_Part2$priority_year > a,]

IPC_all_patents_Part3 <- IPC_all_patents_Part3[IPC_all_patents_Part3$priority_year < b,]
IPC_all_patents_Part3 <- IPC_all_patents_Part3[IPC_all_patents_Part3$priority_year > a,]

#let's drop the columns we won't use (priority_year):
IPC_all_patents_Part1 <- IPC_all_patents_Part1[, c((-4), (-5))]
IPC_all_patents_Part2 <- IPC_all_patents_Part2[, c((-4), (-5))]
IPC_all_patents_Part3 <- IPC_all_patents_Part3[, c((-4), (-5))]

#we combine the 3 files:
IPC_all_patents_FirstPeriod <- rbind(IPC_all_patents_Part1, IPC_all_patents_Part2, IPC_all_patents_Part3)
#and exclude the 3 big ones we just used, so we have back our memory:
rm(IPC_all_patents_Part1, IPC_all_patents_Part2, IPC_all_patents_Part3)

length(unique(IPC_all_patents_FirstPeriod$appln_id))#4816006 unique publication numbers (i.e., unique priorities) out of
length(IPC_all_patents_FirstPeriod$appln_id) #13279651 lines of data

#now we apply the 2 functions we created at the beginning of this section:
reg_tech1 <- group_by_applnID(IPC_all_patents_FirstPeriod)
#rm(IPC_all_patents_FirstPeriod)
reg_tech1 <- group_by_ctry_and_IPC(reg_tech1)

mat_reg_tech1 <- reg_tech1 %>%
  arrange(techn_field_nr, ctry_code) %>%
  pivot_wider(names_from = techn_field_nr, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

mat_reg_tech1 %<>% remove_rownames %>% column_to_rownames(var="ctry_code") %>%
  as.matrix() %>%
  round()

reg_RCA1 <- mat_reg_tech1 %>% location.quotient(binary = F) %>% 
  as.data.frame() %>% 
  rownames_to_column("ctry_code") %>% 
  as_tibble() %>% 
  gather(key = "techn_field_nr", value = "RCA", -ctry_code) %>%
  arrange(ctry_code, techn_field_nr)

#1.1.2. AI perspective ----
#For the first period, which goes from 1974 to 1988, we need only the dataset from Part2:
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
patents_AI_specific_1st <- read.csv("other_files/IPCs_AI.csv", sep = ";", header = TRUE, dec=",")

a = 1973
b = 1989

patents_AI_specific_1st <- patents_AI_specific_1st[patents_AI_specific_1st$priority_year < b,]
patents_AI_specific_1st <- patents_AI_specific_1st[patents_AI_specific_1st$priority_year > a,]

#add technological fields numbers to AI patents
patents_AI_specific_1st <- distinct(patents_AI_specific_1st, appln_id, .keep_all = TRUE)[,c(1,3)]
patents_AI_specific_1st <- left_join(patents_AI_specific_1st, IPC_all_patents_FirstPeriod, by = "appln_id")
length(unique(patents_AI_specific_1st$appln_id)) #435

#and apply the 2 functions we created at the beginning of this code:
reg_tech1 <- group_by_applnID(patents_AI_specific_1st)
rm(patents_AI_specific_1st)
reg_tech1 <- group_by_ctry_and_IPC(reg_tech1)

mat_reg_tech1 <- reg_tech1 %>%
  arrange(techn_field_nr, ctry_code) %>%
  pivot_wider(names_from = techn_field_nr, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

mat_reg_tech1 %<>% remove_rownames %>% column_to_rownames(var="ctry_code") %>%
  as.matrix() %>%
  round()

reg_RCA1_AIspecific <- mat_reg_tech1 %>% location.quotient(binary = F) %>% 
  as.data.frame() %>% 
  rownames_to_column("ctry_code") %>% 
  as_tibble() %>% 
  gather(key = "techn_field_nr", value = "RCA", -ctry_code) %>%
  arrange(ctry_code, techn_field_nr)

Data1period <- merge(reg_RCA1, reg_RCA1_AIspecific, all=T, by=c("ctry_code", "techn_field_nr"))
Data1period$Period <- "1974-1988"
names(Data1period) <- c("ctry_code", "techn_field_nr", "RCA_Gen", "RCA_AI", "Period")
write.csv2(Data1period, file = "Files_created_with_the_code/data/files_code_Fields_analysis/Data1period_RCA.csv", row.names = F)
rm(mat_reg_tech1, IPC_all_patents_FirstPeriod)

#1.2.Second Period ----
#1.2.1.General Perspective ----
#For the second period, which goes from 1989 to 2003, we need only the dataset from Part2:
setwd("large_files")
c <- 45182803 -40000000
IPC_all_patents_Part1 <- fread("All_patents_and_IPCs_Part2.csv", header = F, nrow = 20000000)
IPC_all_patents_Part2 <- fread("All_patents_and_IPCs_Part2.csv", header = F, nrow = 20000000, skip = 20000000)
IPC_all_patents_Part3 <- fread("All_patents_and_IPCs_Part2.csv", header = F, nrow = c, skip = 40000000)

names(IPC_all_patents_Part1) <- c("appln_id", "ctry_code", "techn_field_nr", "weight", "priority_year")
names(IPC_all_patents_Part2) <- c("appln_id", "ctry_code", "techn_field_nr", "weight", "priority_year")
names(IPC_all_patents_Part3) <- c("appln_id", "ctry_code", "techn_field_nr", "weight", "priority_year")

a = 1988
b = 2004

IPC_all_patents_Part1 <- IPC_all_patents_Part1[IPC_all_patents_Part1$priority_year < b,]
IPC_all_patents_Part1 <- IPC_all_patents_Part1[IPC_all_patents_Part1$priority_year > a,]

IPC_all_patents_Part2 <- IPC_all_patents_Part2[IPC_all_patents_Part2$priority_year < b,]
IPC_all_patents_Part2 <- IPC_all_patents_Part2[IPC_all_patents_Part2$priority_year > a,]

IPC_all_patents_Part3 <- IPC_all_patents_Part3[IPC_all_patents_Part3$priority_year < b,]
IPC_all_patents_Part3 <- IPC_all_patents_Part3[IPC_all_patents_Part3$priority_year > a,]

#let's drop again the columns we won't use (weight and priority_year):
IPC_all_patents_Part1 <- IPC_all_patents_Part1[, c((-4), (-5))]
IPC_all_patents_Part2 <- IPC_all_patents_Part2[, c((-4), (-5))]
IPC_all_patents_Part3 <- IPC_all_patents_Part3[, c((-4), (-5))]

#we combine the 3 files:
IPC_all_patents_SecondPeriod <- rbind(IPC_all_patents_Part1, IPC_all_patents_Part2, IPC_all_patents_Part3)
#and exclude the 3 big ones we just used, so we have back our memory:
rm(IPC_all_patents_Part1, IPC_all_patents_Part2, IPC_all_patents_Part3)

length(unique(IPC_all_patents_SecondPeriod$appln_id))#8848446 unique publication numbers (i.e., unique priorities) out of
length(IPC_all_patents_SecondPeriod$appln_id) #28281321 lines of data

#now we apply the 2 functions we created at the beginning of this section:
reg_tech2 <- group_by_applnID(IPC_all_patents_SecondPeriod)
reg_tech2 <- group_by_ctry_and_IPC(reg_tech2)

mat_reg_tech2 <- reg_tech2 %>%
  arrange(techn_field_nr, ctry_code) %>%
  pivot_wider(names_from = techn_field_nr, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

mat_reg_tech2 %<>% remove_rownames %>% column_to_rownames(var="ctry_code") %>%
  as.matrix() %>%
  round()

reg_RCA2 <- mat_reg_tech2 %>% location.quotient(binary = F) %>% 
  as.data.frame() %>% 
  rownames_to_column("ctry_code") %>% 
  as_tibble() %>% 
  gather(key = "techn_field_nr", value = "RCA", -ctry_code) %>%
  arrange(ctry_code, techn_field_nr)

#1.2.2. AI perspective ----
#For the first period, which goes from 1989 to 2003, we need only the dataset from Part2:
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

patents_AI_specific_2nd <- read.csv("other_files/IPCs_AI.csv", sep = ";", header = TRUE, dec=",")

a = 1988
b = 2004

patents_AI_specific_2nd <- patents_AI_specific_2nd[patents_AI_specific_2nd$priority_year < b,]
patents_AI_specific_2nd <- patents_AI_specific_2nd[patents_AI_specific_2nd$priority_year > a,]

#add technological fields numbers to AI patents
patents_AI_specific_2nd <- distinct(patents_AI_specific_2nd, appln_id, .keep_all = TRUE)[,c(1,3)]
patents_AI_specific_2nd <- left_join(patents_AI_specific_2nd, IPC_all_patents_SecondPeriod, by = "appln_id")
length(unique(patents_AI_specific_2nd$appln_id)) #7887

#and apply the 2 functions we created at the beginning of this code:
reg_tech2 <- group_by_applnID(patents_AI_specific_2nd)
rm(patents_AI_specific_2nd)
reg_tech2 <- group_by_ctry_and_IPC(reg_tech2)

mat_reg_tech2 <- reg_tech2 %>%
  arrange(techn_field_nr, ctry_code) %>%
  pivot_wider(names_from = techn_field_nr, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

mat_reg_tech2 %<>% remove_rownames %>% column_to_rownames(var="ctry_code") %>%
  as.matrix() %>%
  round()

reg_RCA2_AIspecific <- mat_reg_tech2 %>% location.quotient(binary = F) %>% 
  as.data.frame() %>% 
  rownames_to_column("ctry_code") %>% 
  as_tibble() %>% 
  gather(key = "techn_field_nr", value = "RCA", -ctry_code) %>%
  arrange(ctry_code, techn_field_nr)

Data2period <- merge(reg_RCA2, reg_RCA2_AIspecific, all=T, by=c("ctry_code", "techn_field_nr"))
Data2period$Period <- "1989-2003"
names(Data2period) <- c("ctry_code", "techn_field_nr", "RCA_Gen", "RCA_AI", "Period")
write.csv2(Data2period, file = "Files_created_with_the_code/data/files_code_Fields_analysis/Data2period_RCA.csv", row.names = F)
rm(mat_reg_tech2, IPC_all_patents_SecondPeriod)

#1.3.Third Period -----
#1.3.1. General Perspective ----
#For the third period, which goes from 2004 to 2018, we  need only the dataset from Part1. This
#specific dataset only has patents from 2004 to 2018, so we don't have to filter it. But calculating
#the reg_tech is very computationally expansive, so we divide that into 6 parts.
setwd("large_files")
c <- 58841893-40000000
IPC_all_patents_Part1 <- fread("All_patents_and_IPCs_Part1.csv", header = F, nrow = 20000000)
IPC_all_patents_Part2 <- fread("All_patents_and_IPCs_Part1.csv", header = F, nrow = 20000000, skip = 20000000)
IPC_all_patents_Part3 <- fread("All_patents_and_IPCs_Part1.csv", header = F, nrow = c, skip = 40000000)

names(IPC_all_patents_Part1) <- c("appln_id", "ctry_code", "techn_field_nr", "weight", "priority_year")
names(IPC_all_patents_Part2) <- c("appln_id", "ctry_code", "techn_field_nr", "weight", "priority_year")
names(IPC_all_patents_Part3) <- c("appln_id", "ctry_code", "techn_field_nr", "weight", "priority_year")

IPC_all_patents_Part1 <- IPC_all_patents_Part1[, c((-4), (-5))]
IPC_all_patents_Part2 <- IPC_all_patents_Part2[, c((-4), (-5))]
IPC_all_patents_Part3 <- IPC_all_patents_Part3[, c((-4), (-5))]

#the three lines below are used just to get the number of priorities from the third period:
ThirdDataset <- rbind(IPC_all_patents_Part1, IPC_all_patents_Part2,IPC_all_patents_Part3)
length(unique(ThirdDataset$appln_id)) #16270598 priorities
rm(ThirdDataset)
#here we divide our calculations of the reg_tech (which was not necessary on the 2 previous periods)
#For countries
reg_tech4 <- group_by_applnID(IPC_all_patents_Part1)
reg_tech4 <- group_by_ctry_and_IPC(reg_tech4)

reg_tech5 <- group_by_applnID(IPC_all_patents_Part2)
reg_tech5 <- group_by_ctry_and_IPC(reg_tech5)

reg_tech6 <- group_by_applnID(IPC_all_patents_Part3)
reg_tech6 <- group_by_ctry_and_IPC(reg_tech6)

#now we merge them
tabledata2 <- merge(reg_tech4, reg_tech5, all=T, by=c("ctry_code", "techn_field_nr"))
tabledata2 <- merge(tabledata2, reg_tech6, all=T, by=c("ctry_code", "techn_field_nr"))

#remove the big files
rm(reg_tech4, reg_tech5, reg_tech6)

#replace NAs, so we don't have problems when summing:
tabledata2[is.na(tabledata2)] <- 0

#do the summ, exclude the tables used, and rename the dataset accordingly:
tabledata2$sum <- rowSums(tabledata2[,c(3:5)])
tabledata2 <- tabledata2[, c((-3), (-4), (-5))]
names(tabledata2) <- c("ctry_code", "techn_field_nr", "n_tech_reg")

mat_reg_tech3 <- tabledata2 %>% #tabledata2
  arrange(techn_field_nr, ctry_code) %>%
  pivot_wider(names_from = techn_field_nr, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

mat_reg_tech3 %<>% remove_rownames %>% column_to_rownames(var="ctry_code") %>%
  as.matrix() %>%
  round()

reg_RCA3 <- mat_reg_tech3 %>% location.quotient(binary = F) %>% 
  as.data.frame() %>% 
  rownames_to_column("ctry_code") %>% 
  as_tibble() %>% 
  gather(key = "techn_field_nr", value = "RCA", -ctry_code) %>%
  arrange(ctry_code, techn_field_nr)

#1.3.2. AI Perspective  ----
#For the third period, which goes from 2004 to 2018:
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
patents_AI_specific_3rd <- read.csv("other_files/IPCs_AI.csv", sep = ";", header = TRUE, dec=",")

a = 2003
b = 2019

patents_AI_specific_3rd <- patents_AI_specific_3rd[patents_AI_specific_3rd$priority_year < b,]
patents_AI_specific_3rd <- patents_AI_specific_3rd[patents_AI_specific_3rd$priority_year > a,]

#add technological fields numbers to AI patents
patents_AI_specific_3rd <- distinct(patents_AI_specific_3rd, appln_id, .keep_all = TRUE)[,c(1,3)]

IPC_all_patents_ThirdPeriod <- rbind(IPC_all_patents_Part1, IPC_all_patents_Part2, IPC_all_patents_Part3)
rm(IPC_all_patents_Part1, IPC_all_patents_Part2, IPC_all_patents_Part3)

patents_AI_specific_3rd <- left_join(patents_AI_specific_3rd, IPC_all_patents_ThirdPeriod, by = "appln_id")
length(unique(patents_AI_specific_3rd$appln_id)) #34576

#now we apply the 2 functions we created at the beginning of this section:
reg_tech3 <- group_by_applnID(patents_AI_specific_3rd)
rm(patents_AI_specific_3rd)
reg_tech3 <- group_by_ctry_and_IPC(reg_tech3)

###Third Period:
mat_reg_tech3 <- reg_tech3 %>%
  arrange(techn_field_nr, ctry_code) %>%
  pivot_wider(names_from = techn_field_nr, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

mat_reg_tech3 %<>% remove_rownames %>% column_to_rownames(var="ctry_code") %>%
  as.matrix() %>%
  round()

reg_RCA3_AIspecific <- mat_reg_tech3 %>% location.quotient(binary = F) %>% 
  as.data.frame() %>% 
  rownames_to_column("ctry_code") %>% 
  as_tibble() %>% 
  gather(key = "techn_field_nr", value = "RCA", -ctry_code) %>%
  arrange(ctry_code, techn_field_nr)

Data3period <- merge(reg_RCA3, reg_RCA3_AIspecific, all=T, by=c("ctry_code", "techn_field_nr"))
Data3period$Period <- "2004-2018"
names(Data3period) <- c("ctry_code", "techn_field_nr", "RCA_Gen", "RCA_AI", "Period")
write.csv2(Data3period, file = "Files_created_with_the_code/data/files_code_Fields_analysis/Data3period_RCA.csv", row.names = F)

IPC_RCAs <- rbind(Data1period, Data2period, Data3period)
write.csv2(IPC_RCAs, file = "Files_created_with_the_code/data/files_code_Fields_analysis/IPC_RCAs.csv", row.names = F)
#1.4.Summary RCAs ----
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(plyr)

IPC_RCAs <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/IPC_RCAs.csv", sep = ";", header = TRUE, dec=",")

#Select the 4 countries we want
IPC_RCAs_Top4 <- IPC_RCAs[IPC_RCAs$ctry_code == "CN" | 
                            IPC_RCAs$ctry_code == "KR"| 
                            IPC_RCAs$ctry_code == "US"| 
                            IPC_RCAs$ctry_code == "JP", ]

IPC_RCAs_Top4$ctry_code <- as.vector(IPC_RCAs_Top4$ctry_code)

#add new label data:
IPC_names <- read.csv("other_files/ipc_technology.csv", sep = ";", header = TRUE)%>%
  select(field_nr, sector, field_name) %>%
  distinct(field_nr, .keep_all = TRUE) %>%
  mutate(techn_field_nr = field_nr) %>%
  arrange(techn_field_nr)

#add field_name
IPC_RCAs_Top4$Label <- IPC_names$field_name[match(IPC_RCAs_Top4$techn_field_nr, IPC_names$techn_field_nr)]

#replace NAs by 0
IPC_RCAs_Top4[is.na(IPC_RCAs_Top4)] <- 0

IPC_RCAs_Top4$Round_general<-ifelse(IPC_RCAs_Top4$RCA_Gen <1, 0,1)
IPC_RCAs_Top4$Round_AI<-ifelse(IPC_RCAs_Top4$RCA_AI <1, 0,1)

IPC_RCAs_Top4$Total_RCA <- IPC_RCAs_Top4$Round_general + IPC_RCAs_Top4$Round_AI
write.csv2(IPC_RCAs_Top4, file = "Files_created_with_the_code/data/files_code_Fields_analysis/RCA_4countries_detailed.csv", row.names = F)

#2.Create new technological space ----
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#Now we load all the files we already saved. We start by loading the janitor library, which is used here for converting
#the first column of data to row names.
library(janitor)
#now we load the similarity matrix which was saved in line 139:
matrix2 <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/Matrix_IPC.csv", sep = ";", header = F)
matrix2 <- matrix2 %>%
  row_to_names(row_number = 1)
matrix <- matrix2[,-1]
rownames(matrix) <- matrix2[,1]
matrix <- as.matrix(matrix)
mat_tech_AI_Final <- matrix
mat_tech_rel_AI <- mat_tech_AI_Final %>% 
  relatedness(method = "cosine")

IPC_names <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/Specializations_All_periods_IPC.csv", sep = ";", header = TRUE)%>%
  select(techn_field_nr, sector, field_name, Category) %>%
  distinct(techn_field_nr, .keep_all = TRUE) %>%
  mutate(techn_field_nr = techn_field_nr) %>%
  arrange(techn_field_nr)

g_tech_AI <- mat_tech_rel_AI %>% as_tbl_graph(directed = FALSE) %N>%
  left_join(IPC_names %>% mutate(techn_field_nr = techn_field_nr %>% as.character()), by = c("name" = "techn_field_nr")) %>%
  mutate(dgr = centrality_eigen(weights = weight)) %E>%
  filter(weight >= mean(weight))

coords_tech_AI <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/coords_tech_AI_layout1.csv", sep = ";", header = T, dec=",")

IPC_RCAs_Top4 <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/RCA_4countries_detailed.csv", sep = ";", header = TRUE, dec=",")
IPC_RCAs_Top4$Total_RCA <- as.factor(IPC_RCAs_Top4$Total_RCA)
IPC_RCAs_Top4$Period_sim <- as.numeric(factor(IPC_RCAs_Top4$Period,levels=unique(IPC_RCAs_Top4$Period)))
IPC_RCAs_Top4$techn_field_nr <- as.character(IPC_RCAs_Top4$techn_field_nr)

AI_RCA <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/Specializations_All_periods_IPC.csv", sep = ";", header = TRUE, dec=",")
AI_RCA$Period_sim <- as.numeric(factor(AI_RCA$Period,levels=unique(AI_RCA$Period)))
AI_RCA <- AI_RCA[,c(2,9,13)]
AI_RCA$techn_field_nr <- as.character(AI_RCA$techn_field_nr)
names(AI_RCA) <- c("techn_field_nr", "RCA_AI_Period", "Period_sim")
IPC_RCAs_Top4 <- left_join(IPC_RCAs_Top4, AI_RCA, by = c("techn_field_nr", "Period_sim"))


#fix Total_RCA:
#so, if Total_RCA_2 = 0, no specialization of the country at all, Total_RCA_2 = 1, general, Total_RCA_2 = 2, ONLY AI;
#Total_RCA_2 = 3, BOTH AI AND GENERAL
IPC_RCAs_Top4$Total_RCA_2 <- IPC_RCAs_Top4$Round_general + 2*IPC_RCAs_Top4$Round_AI

#add degree of complexity per year;

General <- 
g_tech_AI %>%
  ggraph(layout =  coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.4, colour = "grey") + 
  geom_node_point(aes(fill = sector, size = 1000^dgr, shape= sector))+ # labs(fill = "Dose (mg)") 
  scale_shape_manual(values=c(21, 22, 23, 24, 25)) + scale_size("Degree", range = c(2, 12)) + 
  geom_node_text(aes(label = field_name), size = 4, repel = TRUE) + 
  theme_graph()+
  ggtitle("Technology Space: IPC Technology fields") + 
  theme(legend.title = element_text(size = 14),
        legend.text = element_text(size = 10)) + 
  guides(colour = guide_legend(override.aes = list(size=10)))+
  geom_mark_hull(aes(x = x, y=y, colour = sector, fill= sector,
                     linetype = sector), alpha = 0.15, expand = unit(2.5, "mm"), size = 1) 


jpeg("Files_created_with_the_code/figures/new_figures/General.jpg", width = 14, height = 10, units = 'in', res = 300)
General 
dev.off()
rm(General)

#per country
country_select <- c("CN", "US", "JP", "KR")
i = 1
#Option 1
g_tech_AI %N>%
  left_join(IPC_RCAs_Top4 %>% filter(ctry_code == country_select[i] & IPC_RCAs_Top4$Period_sim ==3) %>% 
              select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "grey") +
  geom_node_point(aes(colour = factor(Total_RCA_2), size = factor(Total_RCA_2), shape= factor(Total_RCA_2))) +
  scale_shape_manual(values=c(15, 16, 17, 18)) + labs(color   = "RCA")+ 
  scale_size_manual(values=c(3, 3,3,10))+
  geom_node_text(aes(filter=Round_general > .9, label = field_name), size = 5, repel = TRUE) +
  scale_color_manual(values=c("gray90", "green4", "gray90", "blue"))+
  theme_graph() +
  ggtitle("IPC Technology Space: China (1974-1988)")

#option 2: colours
g_tech_AI %N>%
  left_join(IPC_RCAs_Top4 %>% filter(ctry_code == country_select[i] & IPC_RCAs_Top4$Period_sim ==3) %>% 
              select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "grey") +
  geom_node_point(aes(fill = factor(Total_RCA_2), size = factor(Total_RCA_2), shape= factor(Total_RCA_2))) +
  scale_shape_manual(values=c(21, 22, 23, 24)) + labs(color   = "RCA")+ 
  scale_size_manual(values=c(3, 3, 3, 10))+
  geom_node_text(aes(filter=Total_RCA_2 > .9, label = field_name), size = 5, repel = TRUE) +
  scale_fill_manual(values=c("#999999", "#FFCC00", "#FF9900", "#FF3300"))+
  theme_graph() +
  ggtitle("IPC Technology Space: China (1974-1988)")

#option 3: colours
g_tech_AI %N>%
  left_join(IPC_RCAs_Top4 %>% filter(ctry_code == country_select[i] & IPC_RCAs_Top4$Period_sim ==3) %>% 
              select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "#CCCCCC") +
  geom_node_point(aes(fill = factor(Total_RCA_2), size = factor(Total_RCA_2), shape= factor(Total_RCA_2))) +
  scale_shape_manual(values=c(21, 22, 23, 24)) + labs(color   = "RCA")+ 
  scale_size_manual(values=c(3, 3, 3, 8))+
  geom_node_text(aes(filter=Total_RCA_2 == 1 | Total_RCA_2 == 2, label = field_name), size = 5, repel = TRUE) +
  scale_fill_manual(values=c("#999999", "#33CCCC", "#0033ff", "#0099cc"))+
  theme_graph() +
  ggtitle("IPC Technology Space: China (1974-1988)")

#option 4: colours
g_tech_AI %N>%
  left_join(IPC_RCAs_Top4 %>% filter(ctry_code == country_select[i] & IPC_RCAs_Top4$Period_sim ==3) %>% 
              select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "#CCCCCC") +
  geom_node_point(aes(fill = factor(Total_RCA_2), size = factor(Total_RCA_2), shape= factor(Total_RCA_2))) +
  scale_shape_manual(values=c(21, 22, 23, 24)) + labs(color   = "RCA")+ 
  scale_size_manual(values=c(3, 3, 3, 8))+
  geom_node_text(aes(filter=Total_RCA_2 == 1 | Total_RCA_2 == 2, label = field_name), size = 5, repel = TRUE) +
  scale_fill_manual(values=c("#999999", "#3399FF", "#0066CC", "#0000FF"))+
  theme_graph() +
  ggtitle("IPC Technology Space: China (1974-1988)")

#option 5: colours
g_tech_AI %N>%
  left_join(IPC_RCAs_Top4 %>% filter(ctry_code == country_select[i] & IPC_RCAs_Top4$Period_sim ==3) %>% 
              select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "#CCCCCC") +
  geom_node_point(aes(fill = factor(Total_RCA_2), size = factor(Total_RCA_2), shape= factor(Total_RCA_2))) +
  scale_shape_manual(values=c(21, 22, 23, 24)) + labs(color   = "RCA")+ 
  scale_size_manual(values=c(3, 3, 3, 8))+
  geom_node_text(aes(filter=Total_RCA_2 == 1 | Total_RCA_2 == 2, label = field_name), size = 5, repel = TRUE) +
  scale_fill_manual(values=c("#999999", "#FF9966", "#FF6633", "#CC3300"))+
  theme_graph() +
  ggtitle("IPC Technology Space: China (1974-1988)")

#option 6: colours
g_tech_AI %N>%
  left_join(IPC_RCAs_Top4 %>% filter(ctry_code == country_select[i] & IPC_RCAs_Top4$Period_sim ==3) %>% 
              select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "#CCCCCC") +
  geom_node_point(aes(fill = factor(Total_RCA_2), size = factor(Total_RCA_2), shape= factor(Total_RCA_2))) +
  scale_shape_manual(values=c(21, 22, 23, 24)) + labs(color   = "RCA")+ 
  scale_size_manual(values=c(3, 3, 3, 8))+
  geom_node_text(aes(filter=Total_RCA_2 == 1 | Total_RCA_2 == 2, label = field_name), size = 5, repel = TRUE) +
  guides(colour = guide_legend(override.aes = list(size=10)))+
  theme_graph() +
  ggtitle("IPC Technology Space: China (1974-1988)")

#option 7: cluster
g_tech_AI %N>%
  left_join(IPC_RCAs_Top4 %>% filter(ctry_code == country_select[i] & IPC_RCAs_Top4$Period_sim ==3) %>% 
              select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "#CCCCCC") +
  geom_node_point(aes(fill = factor(Total_RCA_2), size = factor(Total_RCA_2), shape= factor(Total_RCA_2))) +
  scale_shape_manual(values=c(21, 22, 23, 24)) + labs(color   = "RCA")+ 
  scale_size_manual(values=c(3, 3, 3, 8))+
  geom_node_text(aes(filter=Total_RCA_2 == 1 | Total_RCA_2 == 2, label = field_name), size = 5, repel = TRUE) +
  scale_fill_manual(values=c("#999999", "#3399FF", "#0066CC", "#0000FF"))+
  theme_graph() +
  ggtitle("IPC Technology Space: China (1974-1988)") +
  geom_mark_hull(aes(x = x, y=y, filter=RCA_AI_Period > .99), expand = unit(2.5, "mm"), color = "dodgerblue1",size = 1.5, linetype = "dashed") 

#Option 8: Just AI fields highlighted
g_tech_AI %N>%
  left_join(IPC_RCAs_Top4 %>% filter(ctry_code == country_select[i] & IPC_RCAs_Top4$Period_sim ==3) %>% 
              select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "#CCCCCC") +
  geom_node_point(aes(fill = factor(Total_RCA_2), size = factor(Total_RCA_2), shape= factor(Total_RCA_2))) +
  scale_shape_manual(values=c(21, 22, 23, 24)) + labs(color   = "RCA")+ 
  scale_size_manual(values=c(3, 3, 3, 8))+
  geom_node_text(aes(filter=RCA_AI_Period > .99, label = field_name), size = 5, repel = TRUE) +
  scale_fill_manual(values=c("#999999", "#3399FF", "#0066CC", "#0000FF"))+
  theme_graph() +
  ggtitle("IPC Technology Space: China (1974-1988)") #+
  #geom_mark_hull(aes(x = x, y=y, filter=RCA_AI_Period > .99), expand = unit(2.5, "mm"), color = "dodgerblue1",size = 1.5, linetype = "dashed") 

##Option 9: colours and period p
p = 1
g_tech_AI %N>%
  left_join(IPC_RCAs_Top4 %>% filter(ctry_code == country_select[i] & IPC_RCAs_Top4$Period_sim == p) %>% 
              select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "#CCCCCC") +
  geom_node_point(aes(fill = factor(Total_RCA_2), size = factor(Total_RCA_2), shape= factor(Total_RCA_2))) +
  scale_shape_manual(values=c(21, 22, 23, 24)) + labs(color   = "RCA")+ 
  scale_size_manual(values=c(3, 3, 3, 8))+
  geom_node_text(aes(filter=RCA_AI_Period > .99, label = field_name), size = 5, repel = TRUE) +
  scale_fill_manual(values=c("#999999", "#FF3300", "#3399FF", "#009900"))+
  theme_graph() +
  ggtitle("IPC Technology Space: China (1974-1988)") #


display.brewer.pal(n = 8, name = 'Dark2')

#2.1.Print figures----
#The figures I'm using now are based on the general static technological space (g_tech_AI); let's see if I can get the
#degrees of each technological space in new variables (g_tech_AI1, g_tech_AI2, g_tech_AI3)

#2.1.1.First Country-----
#i=1 (CN) and p = 1
i=1
p=1

China_1st<-
g_tech_AI %N>%
  left_join(IPC_RCAs_Top4 %>% filter(ctry_code == country_select[i] & IPC_RCAs_Top4$Period_sim == p) %>% 
              select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "#CCCCCC") +
  geom_node_point(aes(fill = factor(Total_RCA_2), size = factor(Total_RCA_2), shape= factor(Total_RCA_2))) +
  scale_shape_manual(values=c(21, 22, 23, 24)) + labs(color   = "RCA")+ 
  scale_size_manual(values=c(3, 3, 3, 8))+
  geom_node_text(aes(filter=RCA_AI_Period > .99, label = field_name), size = 5, repel = TRUE) +
  scale_fill_manual(values=c("#999999", "#FF3300", "#3399FF", "#009900"))+
  theme_graph() +
  ggtitle("IPC Technology Space: China (1974-1988)") #

p=2
China_2nd<-g_tech_AI %N>%
  left_join(IPC_RCAs_Top4 %>% filter(ctry_code == country_select[i] & IPC_RCAs_Top4$Period_sim == p) %>% 
              select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "#CCCCCC") +
  geom_node_point(aes(fill = factor(Total_RCA_2), size = factor(Total_RCA_2), shape= factor(Total_RCA_2))) +
  scale_shape_manual(values=c(21, 22, 23, 24)) + labs(color   = "RCA")+ 
  scale_size_manual(values=c(3, 3, 3, 8))+
  geom_node_text(aes(filter=RCA_AI_Period > .99, label = field_name), size = 5, repel = TRUE) +
  scale_fill_manual(values=c("#999999", "#FF3300", "#3399FF", "#009900"))+
  theme_graph() +
  ggtitle("IPC Technology Space: China (1989-2003)") #

p =3
China_3rd<-g_tech_AI %N>%
  left_join(IPC_RCAs_Top4 %>% filter(ctry_code == country_select[i] & IPC_RCAs_Top4$Period_sim == p) %>% 
              select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "#CCCCCC") +
  geom_node_point(aes(fill = factor(Total_RCA_2), size = factor(Total_RCA_2), shape= factor(Total_RCA_2))) +
  scale_shape_manual(values=c(21, 22, 23, 24)) + labs(color   = "RCA")+ 
  scale_size_manual(values=c(3, 3, 3, 8))+
  geom_node_text(aes(filter=RCA_AI_Period > .99, label = field_name), size = 5, repel = TRUE) +
  scale_fill_manual(values=c("#999999", "#FF3300", "#3399FF", "#009900"))+
  theme_graph() +
  ggtitle("IPC Technology Space: China (2004-2018)") #

jpeg("Files_created_with_the_code/figures/new_figures/China_1st.jpg", width = 14, height = 10, units = 'in', res = 300)
China_1st 
dev.off()

jpeg("Files_created_with_the_code/figures/new_figures/China_2nd.jpg", width = 14, height = 10, units = 'in', res = 300)
China_2nd 
dev.off()

jpeg("Files_created_with_the_code/figures/new_figures/China_3rd.jpg", width = 14, height = 10, units = 'in', res = 300)
China_3rd 
dev.off()

#2nd option (new):
p=1
China_1st_2nd<-
  g_tech_AI %N>%
  left_join(IPC_RCAs_Top4 %>% filter(ctry_code == country_select[i] & IPC_RCAs_Top4$Period_sim == p) %>% 
              select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "#CCCCCC") +
  geom_node_point(aes(fill = factor(Total_RCA_2), size = 1000^dgr, shape= factor(Total_RCA_2))) +
  scale_shape_manual(values=c(21, 22, 23, 24)) + labs(color   = "RCA") + scale_size("Degree", range = c(2, 12))+ 
  geom_node_text(aes(filter=RCA_AI_Period > .99, label = field_name), size = 5, repel = TRUE) +
  scale_fill_manual(values=c("#999999", "#FF3300", "#3399FF", "#009900"))+
  theme_graph() +
  ggtitle("IPC Technology Space: China (1974-1988)")

p=2
China_2nd_2nd<-
  g_tech_AI %N>%
  left_join(IPC_RCAs_Top4 %>% filter(ctry_code == country_select[i] & IPC_RCAs_Top4$Period_sim == p) %>% 
              select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "#CCCCCC") +
  geom_node_point(aes(fill = factor(Total_RCA_2), size = 1000^dgr, shape= factor(Total_RCA_2))) +
  scale_shape_manual(values=c(21, 22, 23, 24)) + labs(color   = "RCA") + scale_size("Degree", range = c(2, 12))+ 
  geom_node_text(aes(filter=RCA_AI_Period > .99, label = field_name), size = 5, repel = TRUE) +
  scale_fill_manual(values=c("#999999", "#FF3300", "#3399FF", "#009900"))+
  theme_graph() +
  ggtitle("IPC Technology Space: China (1989-2003)")

p=3
China_3rd_2nd<-
  g_tech_AI %N>%
  left_join(IPC_RCAs_Top4 %>% filter(ctry_code == country_select[i] & IPC_RCAs_Top4$Period_sim == p) %>% 
              select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "#CCCCCC") +
  geom_node_point(aes(fill = factor(Total_RCA_2), size = 1000^dgr, shape= factor(Total_RCA_2))) +
  scale_shape_manual(values=c(21, 22, 23, 24)) + labs(color   = "RCA") + scale_size("Degree", range = c(2, 12))+ 
  geom_node_text(aes(filter=RCA_AI_Period > .99, label = field_name), size = 5, repel = TRUE) +
  scale_fill_manual(values=c("#999999", "#FF3300", "#3399FF", "#009900"))+
  theme_graph() +
  ggtitle("IPC Technology Space: China (2004-2018)")

jpeg("Files_created_with_the_code/figures/new_figures/new_China_1st.jpg", width = 14, height = 10, units = 'in', res = 300)
China_1st_2nd 
dev.off()

jpeg("Files_created_with_the_code/figures/new_figures/new_China_2nd.jpg", width = 14, height = 10, units = 'in', res = 300)
China_2nd_2nd  
dev.off()

jpeg("Files_created_with_the_code/figures/new_figures/new_China_3rd.jpg", width = 14, height = 10, units = 'in', res = 300)
China_3rd_2nd  
dev.off()


#2.1.2.Second Country-----
#i=2 (US) and p = 1
i=2
p=1

USA_1st<-g_tech_AI %N>%
  left_join(IPC_RCAs_Top4 %>% filter(ctry_code == country_select[i] & IPC_RCAs_Top4$Period_sim == p) %>% 
              select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "#CCCCCC") +
  geom_node_point(aes(fill = factor(Total_RCA_2), size = factor(Total_RCA_2), shape= factor(Total_RCA_2))) +
  scale_shape_manual(values=c(21, 22, 23, 24)) + labs(color   = "RCA")+ 
  scale_size_manual(values=c(3, 3, 3, 8))+
  geom_node_text(aes(filter=RCA_AI_Period > .99, label = field_name), size = 5, repel = TRUE) +
  scale_fill_manual(values=c("#999999", "#FF3300", "#3399FF", "#009900"))+
  theme_graph() +
  ggtitle("IPC Technology Space: USA (1974-1988)") #

p=2
USA_2nd<-g_tech_AI %N>%
  left_join(IPC_RCAs_Top4 %>% filter(ctry_code == country_select[i] & IPC_RCAs_Top4$Period_sim == p) %>% 
              select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "#CCCCCC") +
  geom_node_point(aes(fill = factor(Total_RCA_2), size = factor(Total_RCA_2), shape= factor(Total_RCA_2))) +
  scale_shape_manual(values=c(21, 22, 23, 24)) + labs(color   = "RCA")+ 
  scale_size_manual(values=c(3, 3, 3, 8))+
  geom_node_text(aes(filter=RCA_AI_Period > .99, label = field_name), size = 5, repel = TRUE) +
  scale_fill_manual(values=c("#999999", "#FF3300", "#3399FF", "#009900"))+
  theme_graph() +
  ggtitle("IPC Technology Space: USA (1989-2003)") #

p =3
USA_3rd<-g_tech_AI %N>%
  left_join(IPC_RCAs_Top4 %>% filter(ctry_code == country_select[i] & IPC_RCAs_Top4$Period_sim == p) %>% 
              select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "#CCCCCC") +
  geom_node_point(aes(fill = factor(Total_RCA_2), size = factor(Total_RCA_2), shape= factor(Total_RCA_2))) +
  scale_shape_manual(values=c(21, 22, 23, 24)) + labs(color   = "RCA")+ 
  scale_size_manual(values=c(3, 3, 3, 8))+
  geom_node_text(aes(filter=RCA_AI_Period > .99, label = field_name), size = 5, repel = TRUE) +
  scale_fill_manual(values=c("#999999", "#FF3300", "#3399FF", "#009900"))+
  theme_graph() +
  ggtitle("IPC Technology Space: USA (2004-2018)") #

jpeg("Files_created_with_the_code/figures/new_figures/USA_1st.jpg", width = 14, height = 10, units = 'in', res = 300)
USA_1st 
dev.off()

jpeg("Files_created_with_the_code/figures/new_figures/USA_2nd.jpg", width = 14, height = 10, units = 'in', res = 300)
USA_2nd 
dev.off()

jpeg("Files_created_with_the_code/figures/new_figures/USA_3rd.jpg", width = 14, height = 10, units = 'in', res = 300)
USA_3rd 
dev.off()

#2nd option (new):
i=2
p=1
USA_1st_2nd<-
  g_tech_AI %N>%
  left_join(IPC_RCAs_Top4 %>% filter(ctry_code == country_select[i] & IPC_RCAs_Top4$Period_sim == p) %>% 
              select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "#CCCCCC") +
  geom_node_point(aes(fill = factor(Total_RCA_2), size = 1000^dgr, shape= factor(Total_RCA_2))) +
  scale_shape_manual(values=c(21, 22, 23, 24)) + labs(color   = "RCA") + scale_size("Degree", range = c(2, 12))+ 
  geom_node_text(aes(filter=RCA_AI_Period > .99, label = field_name), size = 5, repel = TRUE) +
  scale_fill_manual(values=c("#999999", "#FF3300", "#3399FF", "#009900"))+
  theme_graph() +
  ggtitle("IPC Technology Space: USA (1974-1988)")

p=2
USA_2nd_2nd<-
  g_tech_AI %N>%
  left_join(IPC_RCAs_Top4 %>% filter(ctry_code == country_select[i] & IPC_RCAs_Top4$Period_sim == p) %>% 
              select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "#CCCCCC") +
  geom_node_point(aes(fill = factor(Total_RCA_2), size = 1000^dgr, shape= factor(Total_RCA_2))) +
  scale_shape_manual(values=c(21, 22, 23, 24)) + labs(color   = "RCA") + scale_size("Degree", range = c(2, 12))+ 
  geom_node_text(aes(filter=RCA_AI_Period > .99, label = field_name), size = 5, repel = TRUE) +
  scale_fill_manual(values=c("#999999", "#FF3300", "#3399FF", "#009900"))+
  theme_graph() +
  ggtitle("IPC Technology Space: USA (1989-2003)")

p=3
USA_3rd_2nd<-
  g_tech_AI %N>%
  left_join(IPC_RCAs_Top4 %>% filter(ctry_code == country_select[i] & IPC_RCAs_Top4$Period_sim == p) %>% 
              select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "#CCCCCC") +
  geom_node_point(aes(fill = factor(Total_RCA_2), size = 1000^dgr, shape= factor(Total_RCA_2))) +
  scale_shape_manual(values=c(21, 22, 23, 24)) + labs(color   = "RCA") + scale_size("Degree", range = c(2, 12))+ 
  geom_node_text(aes(filter=RCA_AI_Period > .99, label = field_name), size = 5, repel = TRUE) +
  scale_fill_manual(values=c("#999999", "#FF3300", "#3399FF", "#009900"))+
  theme_graph() +
  ggtitle("IPC Technology Space: USA (2004-2018)")

jpeg("Files_created_with_the_code/figures/new_figures/new_USA_1st.jpg", width = 14, height = 10, units = 'in', res = 300)
USA_1st_2nd 
dev.off()

jpeg("Files_created_with_the_code/figures/new_figures/new_USA_2nd.jpg", width = 14, height = 10, units = 'in', res = 300)
USA_2nd_2nd  
dev.off()

jpeg("Files_created_with_the_code/figures/new_figures/new_USA_3rd.jpg", width = 14, height = 10, units = 'in', res = 300)
USA_3rd_2nd  
dev.off()

#2.1.3.Third Country-----
#i=3 (JP) and p = 1
i=3
p=1

Japan_1st<-g_tech_AI %N>%
  left_join(IPC_RCAs_Top4 %>% filter(ctry_code == country_select[i] & IPC_RCAs_Top4$Period_sim == p) %>% 
              select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "#CCCCCC") +
  geom_node_point(aes(fill = factor(Total_RCA_2), size = factor(Total_RCA_2), shape= factor(Total_RCA_2))) +
  scale_shape_manual(values=c(21, 22, 23, 24)) + labs(color   = "RCA")+ 
  scale_size_manual(values=c(3, 3, 3, 8))+
  geom_node_text(aes(filter=RCA_AI_Period > .99, label = field_name), size = 5, repel = TRUE) +
  scale_fill_manual(values=c("#999999", "#FF3300", "#3399FF", "#009900"))+
  theme_graph() +
  ggtitle("IPC Technology Space: Japan (1974-1988)") #

p=2
Japan_2nd<-g_tech_AI %N>%
  left_join(IPC_RCAs_Top4 %>% filter(ctry_code == country_select[i] & IPC_RCAs_Top4$Period_sim == p) %>% 
              select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "#CCCCCC") +
  geom_node_point(aes(fill = factor(Total_RCA_2), size = factor(Total_RCA_2), shape= factor(Total_RCA_2))) +
  scale_shape_manual(values=c(21, 22, 23, 24)) + labs(color   = "RCA")+ 
  scale_size_manual(values=c(3, 3, 3, 8))+
  geom_node_text(aes(filter=RCA_AI_Period > .99, label = field_name), size = 5, repel = TRUE) +
  scale_fill_manual(values=c("#999999", "#FF3300", "#3399FF", "#009900"))+
  theme_graph() +
  ggtitle("IPC Technology Space: Japan (1989-2003)") #

p =3
Japan_3rd<-g_tech_AI %N>%
  left_join(IPC_RCAs_Top4 %>% filter(ctry_code == country_select[i] & IPC_RCAs_Top4$Period_sim == p) %>% 
              select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "#CCCCCC") +
  geom_node_point(aes(fill = factor(Total_RCA_2), size = factor(Total_RCA_2), shape= factor(Total_RCA_2))) +
  scale_shape_manual(values=c(21, 22, 23, 24)) + labs(color   = "RCA")+ 
  scale_size_manual(values=c(3, 3, 3, 8))+
  geom_node_text(aes(filter=RCA_AI_Period > .99, label = field_name), size = 5, repel = TRUE) +
  scale_fill_manual(values=c("#999999", "#FF3300", "#3399FF", "#009900"))+
  theme_graph() +
  ggtitle("IPC Technology Space: Japan (2004-2018)") #

jpeg("Files_created_with_the_code/figures/new_figures/Japan_1st.jpg", width = 14, height = 10, units = 'in', res = 300)
Japan_1st 
dev.off()

jpeg("Files_created_with_the_code/figures/new_figures/Japan_2nd.jpg", width = 14, height = 10, units = 'in', res = 300)
Japan_2nd 
dev.off()

jpeg("Files_created_with_the_code/figures/new_figures/Japan_3rd.jpg", width = 14, height = 10, units = 'in', res = 300)
Japan_3rd 
dev.off()

#2nd option (new):
i=3
p=1
Japan_1st_2nd<-
  g_tech_AI %N>%
  left_join(IPC_RCAs_Top4 %>% filter(ctry_code == country_select[i] & IPC_RCAs_Top4$Period_sim == p) %>% 
              select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "#CCCCCC") +
  geom_node_point(aes(fill = factor(Total_RCA_2), size = 1000^dgr, shape= factor(Total_RCA_2))) +
  scale_shape_manual(values=c(21, 22, 23, 24)) + labs(color   = "RCA") + scale_size("Degree", range = c(2, 12))+ 
  geom_node_text(aes(filter=RCA_AI_Period > .99, label = field_name), size = 5, repel = TRUE) +
  scale_fill_manual(values=c("#999999", "#FF3300", "#3399FF", "#009900"))+
  theme_graph() +
  ggtitle("IPC Technology Space: Japan (1974-1988)")

p=2
Japan_2nd_2nd<-
  g_tech_AI %N>%
  left_join(IPC_RCAs_Top4 %>% filter(ctry_code == country_select[i] & IPC_RCAs_Top4$Period_sim == p) %>% 
              select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "#CCCCCC") +
  geom_node_point(aes(fill = factor(Total_RCA_2), size = 1000^dgr, shape= factor(Total_RCA_2))) +
  scale_shape_manual(values=c(21, 22, 23, 24)) + labs(color   = "RCA") + scale_size("Degree", range = c(2, 12))+ 
  geom_node_text(aes(filter=RCA_AI_Period > .99, label = field_name), size = 5, repel = TRUE) +
  scale_fill_manual(values=c("#999999", "#FF3300", "#3399FF", "#009900"))+
  theme_graph() +
  ggtitle("IPC Technology Space: Japan (1989-2003)")

p=3
Japan_3rd_2nd<-
  g_tech_AI %N>%
  left_join(IPC_RCAs_Top4 %>% filter(ctry_code == country_select[i] & IPC_RCAs_Top4$Period_sim == p) %>% 
              select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "#CCCCCC") +
  geom_node_point(aes(fill = factor(Total_RCA_2), size = 1000^dgr, shape= factor(Total_RCA_2))) +
  scale_shape_manual(values=c(21, 22, 23, 24)) + labs(color   = "RCA") + scale_size("Degree", range = c(2, 12))+ 
  geom_node_text(aes(filter=RCA_AI_Period > .99, label = field_name), size = 5, repel = TRUE) +
  scale_fill_manual(values=c("#999999", "#FF3300", "#3399FF", "#009900"))+
  theme_graph() +
  ggtitle("IPC Technology Space: Japan (2004-2018)")

jpeg("Files_created_with_the_code/figures/new_figures/new_Japan_1st.jpg", width = 14, height = 10, units = 'in', res = 300)
Japan_1st_2nd 
dev.off()

jpeg("Files_created_with_the_code/figures/new_figures/new_Japan_2nd.jpg", width = 14, height = 10, units = 'in', res = 300)
Japan_2nd_2nd  
dev.off()

jpeg("Files_created_with_the_code/figures/new_figures/new_Japan_3rd.jpg", width = 14, height = 10, units = 'in', res = 300)
Japan_3rd_2nd  
dev.off()


#2.1.4.Fourth Country-----
#i=4 (KR) and p = 1
i=4
p=1

South_Korea_1st<-g_tech_AI %N>%
  left_join(IPC_RCAs_Top4 %>% filter(ctry_code == country_select[i] & IPC_RCAs_Top4$Period_sim == p) %>% 
              select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "#CCCCCC") +
  geom_node_point(aes(fill = factor(Total_RCA_2), size = factor(Total_RCA_2), shape= factor(Total_RCA_2))) +
  scale_shape_manual(values=c(21, 22, 23, 24)) + labs(color   = "RCA")+ 
  scale_size_manual(values=c(3, 3, 3, 8))+
  geom_node_text(aes(filter=RCA_AI_Period > .99, label = field_name), size = 5, repel = TRUE) +
  scale_fill_manual(values=c("#999999", "#FF3300", "#3399FF", "#009900"))+
  theme_graph() +
  ggtitle("IPC Technology Space: South Korea (1974-1988)") #

p=2
South_Korea_2nd<-g_tech_AI %N>%
  left_join(IPC_RCAs_Top4 %>% filter(ctry_code == country_select[i] & IPC_RCAs_Top4$Period_sim == p) %>% 
              select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "#CCCCCC") +
  geom_node_point(aes(fill = factor(Total_RCA_2), size = factor(Total_RCA_2), shape= factor(Total_RCA_2))) +
  scale_shape_manual(values=c(21, 22, 23, 24)) + labs(color   = "RCA")+ 
  scale_size_manual(values=c(3, 3, 3, 8))+
  geom_node_text(aes(filter=RCA_AI_Period > .99, label = field_name), size = 5, repel = TRUE) +
  scale_fill_manual(values=c("#999999", "#FF3300", "#3399FF", "#009900"))+
  theme_graph() +
  ggtitle("IPC Technology Space: South Korea (1989-2003)") #

p =3
South_Korea_3rd<-g_tech_AI %N>%
  left_join(IPC_RCAs_Top4 %>% filter(ctry_code == country_select[i] & IPC_RCAs_Top4$Period_sim == p) %>% 
              select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "#CCCCCC") +
  geom_node_point(aes(fill = factor(Total_RCA_2), size = factor(Total_RCA_2), shape= factor(Total_RCA_2))) +
  scale_shape_manual(values=c(21, 22, 23, 24)) + labs(color   = "RCA")+ 
  scale_size_manual(values=c(3, 3, 3, 8))+
  geom_node_text(aes(filter=RCA_AI_Period > .99, label = field_name), size = 5, repel = TRUE) +
  scale_fill_manual(values=c("#999999", "#FF3300", "#3399FF", "#009900"))+
  theme_graph() +
  ggtitle("IPC Technology Space: South Korea (2004-2018)") #

jpeg("Files_created_with_the_code/figures/new_figures/South_Korea_1st.jpg", width = 14, height = 10, units = 'in', res = 300)
South_Korea_1st 
dev.off()

jpeg("Files_created_with_the_code/figures/new_figures/South_Korea_2nd.jpg", width = 14, height = 10, units = 'in', res = 300)
South_Korea_2nd 
dev.off()

jpeg("Files_created_with_the_code/figures/new_figures/South_Korea_3rd.jpg", width = 14, height = 10, units = 'in', res = 300)
South_Korea_3rd 
dev.off()

#2nd option (new):
i=4
p=1
SouthKorea_1st_2nd<-
  g_tech_AI %N>%
  left_join(IPC_RCAs_Top4 %>% filter(ctry_code == country_select[i] & IPC_RCAs_Top4$Period_sim == p) %>% 
              select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "#CCCCCC") +
  geom_node_point(aes(fill = factor(Total_RCA_2), size = 1000^dgr, shape= factor(Total_RCA_2))) +
  scale_shape_manual(values=c(21, 22, 23, 24)) + labs(color   = "RCA") + scale_size("Degree", range = c(2, 12))+ 
  geom_node_text(aes(filter=RCA_AI_Period > .99, label = field_name), size = 5, repel = TRUE) +
  scale_fill_manual(values=c("#999999", "#FF3300", "#3399FF", "#009900"))+
  theme_graph() +
  ggtitle("IPC Technology Space: South Korea (1974-1988)")

p=2
SouthKorea_2nd_2nd<-
  g_tech_AI %N>%
  left_join(IPC_RCAs_Top4 %>% filter(ctry_code == country_select[i] & IPC_RCAs_Top4$Period_sim == p) %>% 
              select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "#CCCCCC") +
  geom_node_point(aes(fill = factor(Total_RCA_2), size = 1000^dgr, shape= factor(Total_RCA_2))) +
  scale_shape_manual(values=c(21, 22, 23, 24)) + labs(color   = "RCA") + scale_size("Degree", range = c(2, 12))+ 
  geom_node_text(aes(filter=RCA_AI_Period > .99, label = field_name), size = 5, repel = TRUE) +
  scale_fill_manual(values=c("#999999", "#FF3300", "#3399FF", "#009900"))+
  theme_graph() +
  ggtitle("IPC Technology Space: South Korea (1989-2003)")

p=3
SouthKorea_3rd_2nd<-
  g_tech_AI %N>%
  left_join(IPC_RCAs_Top4 %>% filter(ctry_code == country_select[i] & IPC_RCAs_Top4$Period_sim == p) %>% 
              select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "#CCCCCC") +
  geom_node_point(aes(fill = factor(Total_RCA_2), size = 1000^dgr, shape= factor(Total_RCA_2))) +
  scale_shape_manual(values=c(21, 22, 23, 24)) + labs(color   = "RCA") + scale_size("Degree", range = c(2, 12))+ 
  geom_node_text(aes(filter=RCA_AI_Period > .99, label = field_name), size = 5, repel = TRUE) +
  scale_fill_manual(values=c("#999999", "#FF3300", "#3399FF", "#009900"))+
  theme_graph() +
  ggtitle("IPC Technology Space: South Korea (2004-2018)")

jpeg("Files_created_with_the_code/figures/new_figures/new_SouthKorea_1st.jpg", width = 14, height = 10, units = 'in', res = 300)
SouthKorea_1st_2nd 
dev.off()

jpeg("Files_created_with_the_code/figures/new_figures/new_SouthKorea_2nd.jpg", width = 14, height = 10, units = 'in', res = 300)
SouthKorea_2nd_2nd  
dev.off()

jpeg("Files_created_with_the_code/figures/new_figures/new_SouthKorea_3rd.jpg", width = 14, height = 10, units = 'in', res = 300)
SouthKorea_3rd_2nd  
dev.off()

#3.AI networks ----
#3.1.Get the AI matrix, based on its technological fields ----
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("large_files")
IPC_all_patents_Part1 <- fread("All_patents_and_IPCs_Part1.csv", header = F)
IPC_all_patents_Part2 <- fread("All_patents_and_IPCs_Part2.csv", header = F)

table(IPC_all_patents_Part1$V5)
table(IPC_all_patents_Part2$V5)

IPC_all_patents_complete <- rbind(IPC_all_patents_Part1,IPC_all_patents_Part2)
rm(IPC_all_patents_Part1,IPC_all_patents_Part2)
names(IPC_all_patents_complete) <- c("appln_id", "ctry_code", "techn_field_nr", "weight", "priority_year")

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
patents_AI <- read.csv("other_files/IPCs_AI.csv", sep = ";", header = TRUE, dec=",")

#add technological fields numbers to AI patents
patents_AI <- distinct(patents_AI, appln_id, .keep_all = TRUE)[,c(1,3)]
patents_AI <- left_join(patents_AI, IPC_all_patents_complete, by = "appln_id")
rm(IPC_all_patents_complete)
write.csv2(patents_AI, file = "Files_created_with_the_code/data/files_code_Fields_analysis/AI_ALL_patents.csv", row.names = F)

#3.1.1.First period----
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
patents_AI_specific_1st <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/AI_ALL_patents.csv", sep = ";", header = TRUE, dec=",")

a = 1973
b = 1989

patents_AI_specific_1st <- patents_AI_specific_1st[patents_AI_specific_1st$priority_year < b,]
patents_AI_specific_1st <- patents_AI_specific_1st[patents_AI_specific_1st$priority_year > a,]

length(unique(patents_AI_specific_1st$appln_id)) #436
patents_AI_specific_1st <- patents_AI_specific_1st[is.na(patents_AI_specific_1st$appln_id)==F,]

create_sparse_matrix <- function(i.input, j.input){
  require(Matrix)
  mat <- spMatrix(nrow = i.input %>% n_distinct(),
                  ncol = j.input %>% n_distinct(),
                  i = i.input %>% factor() %>% as.numeric(),
                  j = j.input %>% factor() %>% as.numeric(),
                  x = rep(1, i.input %>% length() ) )
  
  row.names(mat) <- i.input %>% factor() %>% levels()
  colnames(mat) <- j.input %>% factor() %>% levels()
  return(mat)
}

mat_tech_AI <- create_sparse_matrix(i = patents_AI_specific_1st %>% pull(appln_id),
                                     j = patents_AI_specific_1st %>% pull(techn_field_nr))

mat_tech_AI %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_rel_AI <- mat_tech_AI %>% 
  relatedness(method = "cosine")

IPC_names <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/Specializations_All_periods_IPC.csv", sep = ";", header = TRUE)%>%
  select(techn_field_nr, sector, field_name, Category) %>%
  distinct(techn_field_nr, .keep_all = TRUE) %>%
  mutate(techn_field_nr = techn_field_nr) %>%
  arrange(techn_field_nr)

KnowlComp_1st <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/KnowledgeComp_1st.csv", sep = ";", header = TRUE, dec=",")
KnowlComp_1st <- KnowlComp_1st[,c(1,6)]#
names(KnowlComp_1st) <- c("techn_field_nr", "Complexity")
IPC_names <- left_join(IPC_names, KnowlComp_1st, by = "techn_field_nr")

g_tech_AI <- mat_tech_rel_AI %>% as_tbl_graph(directed = FALSE) %N>%
  left_join(IPC_names %>% mutate(techn_field_nr = techn_field_nr %>% as.character()), by = c("name" = "techn_field_nr")) %>%
  mutate(dgr = centrality_eigen(weights = weight)) %E>%
  filter(weight >= mean(weight))

#Create the Coordinates
coords_tech_AI <- g_tech_AI %>% igraph::layout.fruchterman.reingold() %>% as_tibble()
colnames(coords_tech_AI) <- c("x", "y")
#let's take a look at the most and less complex IPC fields:
g_tech_AI %N>% 
  arrange(desc(dgr)) %>%
  as_tibble() %>%
  slice(1:10)

g_tech_AI %N>% 
  arrange(dgr) %>%
  as_tibble() %>%
  slice(1:10)

AI_RCA <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/Specializations_All_periods_IPC.csv", sep = ";", header = TRUE, dec=",")
AI_RCA$Period_sim <- as.numeric(factor(AI_RCA$Period,levels=unique(AI_RCA$Period)))
AI_RCA <- AI_RCA[,c(2,9,13)]
AI_RCA$techn_field_nr <- as.character(AI_RCA$techn_field_nr)
names(AI_RCA) <- c("techn_field_nr", "RCA_AI_Period", "Period_sim")
AI_RCA$Binary <- ifelse(AI_RCA$RCA_AI_Period < 1, 0,1)

g_tech_AI %>%
  ggraph(layout =  coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.4, colour = "grey") + 
  geom_node_point(aes(fill = sector, size = dgr, shape= sector))+ 
  scale_shape_manual(values=c(21, 22, 23, 24, 25)) + scale_size(range = c(2, 10)) +
  geom_node_text(aes(label = field_name), size = 4, repel = TRUE) + 
  theme_graph()+
  ggtitle("Technology Space: IPC codes") + 
  theme(legend.title = element_text(size = 14),
        legend.text = element_text(size = 10)) + 
  guides(colour = guide_legend(override.aes = list(size=10)))+
  geom_mark_hull(aes(x = x, y=y, colour = sector, fill= sector,
                     linetype = sector), alpha = 0.15, expand = unit(2.5, "mm"), size = 1) 

AI_RCA1 <- AI_RCA[AI_RCA$Period_sim == 1,]
#First period p = 1
p=1

AI_RCA_1st <- 
g_tech_AI %N>%
  left_join(AI_RCA1, filter=AI_RCA$Period_sim ==p, by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "#CCCCCC") +
  geom_node_point(aes(fill = sector, size = RCA_AI_Period, shape= sector)) +
  scale_shape_manual(values=c(21, 22, 23, 24, 25)) + labs(color   = "RCA")+ scale_size("RCA", range = c(2, 12)) +
 # scale_size_manual(values=c(3, 8))+
  geom_node_text(aes(filter=Binary > .99, label = field_name), size = 5, repel = TRUE) + #filter=Binary > .99, 
 # scale_fill_manual(values=c("#999999", "#3399FF"))+ 
  theme_graph() +
  ggtitle("IPC Technology Space: AI (1974-1988)") #

AI_dgr_1st <- 
g_tech_AI %N>%
  left_join(AI_RCA1, filter=AI_RCA$Period_sim ==p, by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "#CCCCCC") +
  geom_node_point(aes(fill = sector, size = 1000^dgr, shape= sector)) +
  scale_shape_manual(values=c(21, 22, 23, 24, 25)) + labs(color   = "RCA")+ scale_size("Degree", range = c(2, 12)) +
  #scale_size_manual(values=c(3, 8))+
  geom_node_text(aes(filter=Binary > .99, label = field_name), size = 5, repel = TRUE) + #filter=Binary > .99, 
  # scale_fill_manual(values=c("#999999", "#3399FF"))+ 
  theme_graph() +
  ggtitle("IPC Technology Space: AI (1974-1988)") #

jpeg("Files_created_with_the_code/figures/new_figures/AI_RCA_1st.jpg", width = 14, height = 10, units = 'in', res = 300)
AI_RCA_1st 
dev.off()

jpeg("Files_created_with_the_code/figures/new_figures/AI_dgr_1st.jpg", width = 14, height = 10, units = 'in', res = 300)
AI_dgr_1st 
dev.off()

AI_com_1st <- g_tech_AI %N>%
  left_join(AI_RCA1, filter=AI_RCA$Period_sim ==p, by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "#CCCCCC") +
  geom_node_point(aes(fill = sector, size = (100-Complexity), shape= sector)) +
  scale_shape_manual(values=c(21, 22, 23, 24, 25)) + labs(color   = "RCA")+ scale_size(range = c(1, 15)) +
    geom_label_repel(aes(x=x, y=y, label = ifelse(Binary >= 1,field_name,''), size = 10, colour = sector),nudge_y = -.5,fontface = 'bold') + #fill=sector, segment.color = "red"
    geom_label_repel(aes(x=x, y=y,label = ifelse((100-Complexity) >= 0,as.character(round((100-Complexity),2)),''), size = 9),nudge_y = .3) +
    theme_graph() +
  ggtitle("IPC Technology Space: AI (1974-1988)") #

jpeg("Files_created_with_the_code/figures/new_figures/AI_comp_1st.jpg", width = 14, height = 10, units = 'in', res = 300)
AI_com_1st 
dev.off()

#calculate network measures:
AI_1st_networkMeasures <- 
g_tech_AI %N>%
  left_join(AI_RCA1, by = c("name" = "techn_field_nr")) %>%
  mutate(n_neighbors = local_size(mindist = 1),
         weighted_degree = centrality_degree() / local_ave_degree(),
         triangles = local_triangles(),
         centrality_alpha= centrality_alpha(),
         centrality_authority = centrality_authority(),
         centrality_betweenness = centrality_betweenness(),
         centrality_power=centrality_power(),
         centrality_closeness = centrality_closeness(),
         centrality_eigen = centrality_eigen(),
         centrality_hub= centrality_hub(),
         centrality_subgraph=centrality_subgraph(),
         centrality_degree=centrality_degree(),
         centrality_closeness_harmonic=centrality_closeness_harmonic(),
         centrality_closeness_residual=centrality_closeness_residual(),
         centrality_betweenness_network=centrality_betweenness_network(),
  ) %>%
  as_tibble() 
library(openxlsx) #for writing the excel file
write.xlsx(AI_1st_networkMeasures, file = "Files_created_with_the_code/data/files_code_Fields_analysis/AI_1st_networkMeasures.xlsx", row.names = F)
rm(AI_1st_networkMeasures)

#save a new network just to check all the names later:
AI_RCA_1st_allnames <-
g_tech_AI %N>%
  left_join(AI_RCA1, by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "#CCCCCC") +
  geom_node_point(aes(fill = sector, size = RCA_AI_Period, shape= sector)) +
  scale_shape_manual(values=c(21, 22, 23, 24, 25)) + labs(color   = "RCA")+ scale_size(range = c(1, 12)) +
  geom_node_text(aes(label = field_name), size = 5, repel = TRUE) + #filter=Binary > .99, 
  theme_graph() +
  ggtitle("IPC Technology Space: AI (1974-1988)") #
jpeg("Files_created_with_the_code/figures/new_figures/AI_RCA_1st_allnames.jpg", width = 14, height = 10, units = 'in', res = 300)
AI_RCA_1st_allnames 
dev.off()
rm(AI_RCA_1st_allnames)

#other examples:
g_tech_AI %N>%
  left_join(AI_RCA1, filter=AI_RCA$Period_sim ==p, by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "#CCCCCC") +
  geom_node_point(aes(fill = sector, size = factor(Binary), shape= sector)) +
  scale_shape_manual(values=c(21, 22, 23, 24, 25)) + labs(color   = "RCA")+ 
  scale_size_manual(values=c(3, 8))+
  geom_node_text(aes(filter=Binary > .99, label = field_name), size = 5, repel = TRUE) + #filter=Binary > .99, 
  # scale_fill_manual(values=c("#999999", "#3399FF"))+ 
  theme_graph() +
  ggtitle("IPC Technology Space: AI (1974-1988)") #


g_tech_AI %N>%
  left_join(AI_RCA1, filter=AI_RCA$Period_sim ==p, by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "#CCCCCC") +
  geom_node_point(aes(fill = factor(Binary), size = factor(Binary), shape= factor(Binary))) +
  scale_shape_manual(values=c(21, 22)) + labs(color   = "RCA")+ 
  scale_size_manual(values=c(3, 8))+
  geom_node_text(aes(filter=Binary > .99, label = field_name), size = 5, repel = TRUE) + #filter=Binary > .99, 
  scale_fill_manual(values=c("#999999", "#3399FF"))+ 
  theme_graph() +
  ggtitle("IPC Technology Space: China (1974-1988)") #

rm(AI_dgr_1st, AI_RCA_1st, patents_AI_specific_1st, coords_tech_AI, g_tech_AI, mat_tech_AI, mat_tech_rel_AI, AI_RCA1,
   KnowlComp_1st, AI_com_1st)

#3.1.2.Second period-----
patents_AI_specific_2nd <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/AI_ALL_patents.csv", sep = ";", header = TRUE, dec=",")

a = 1988
b = 2004

patents_AI_specific_2nd <- patents_AI_specific_2nd[patents_AI_specific_2nd$priority_year < b,]
patents_AI_specific_2nd <- patents_AI_specific_2nd[patents_AI_specific_2nd$priority_year > a,]

length(unique(patents_AI_specific_2nd$appln_id)) #7888
patents_AI_specific_2nd <- patents_AI_specific_2nd[is.na(patents_AI_specific_2nd$appln_id)==F,]

mat_tech_AI <- create_sparse_matrix(i = patents_AI_specific_2nd %>% pull(appln_id),
                                    j = patents_AI_specific_2nd %>% pull(techn_field_nr))

mat_tech_AI %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_rel_AI <- mat_tech_AI %>% 
  relatedness(method = "cosine")

KnowlComp_2nd <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/KnowledgeComp_2nd.csv", sep = ";", header = TRUE, dec=",")
KnowlComp_2nd <- KnowlComp_2nd[,c(1,6)]#
names(KnowlComp_2nd) <- c("techn_field_nr", "Complexity")
IPC_names <- IPC_names[,c(-5)]
IPC_names <- left_join(IPC_names, KnowlComp_2nd, by = "techn_field_nr")

g_tech_AI <- mat_tech_rel_AI %>% as_tbl_graph(directed = FALSE) %N>%
  left_join(IPC_names %>% mutate(techn_field_nr = techn_field_nr %>% as.character()), by = c("name" = "techn_field_nr")) %>%
  mutate(dgr = centrality_eigen(weights = weight)) %E>%
  filter(weight >= mean(weight))

#Create the Coordinates
coords_tech_AI <- g_tech_AI %>% igraph::layout.fruchterman.reingold() %>% as_tibble()
colnames(coords_tech_AI) <- c("x", "y")
#let's take a look at the most and less complex IPC fields:
g_tech_AI %N>% 
  arrange(desc(dgr)) %>%
  as_tibble() %>%
  slice(1:10)

g_tech_AI %N>% 
  arrange(dgr) %>%
  as_tibble() %>%
  slice(1:10)

g_tech_AI %>%
  ggraph(layout =  coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.4, colour = "grey") + 
  geom_node_point(aes(fill = sector, size = dgr, shape= sector))+ 
  scale_shape_manual(values=c(21, 22, 23, 24, 25)) + scale_size(range = c(2, 10)) +
  geom_node_text(aes(label = field_name), size = 4, repel = TRUE) + 
  theme_graph()+
  ggtitle("Technology Space: IPC codes") + 
  theme(legend.title = element_text(size = 14),
        legend.text = element_text(size = 10)) + 
  guides(colour = guide_legend(override.aes = list(size=10)))+
  geom_mark_hull(aes(x = x, y=y, colour = sector, fill= sector,
                     linetype = sector), alpha = 0.15, expand = unit(2.5, "mm"), size = 1) 


AI_RCA2 <- AI_RCA[AI_RCA$Period_sim == 2,]
#period = 2
p=2

AI_RCA_2nd <- 
  g_tech_AI %N>%
  left_join(AI_RCA2, by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "#CCCCCC") +
  geom_node_point(aes(fill = sector, size = RCA_AI_Period, shape= sector)) +
  scale_shape_manual(values=c(21, 22, 23, 24, 25)) + labs(color   = "RCA")+ scale_size(range = c(1, 12)) +
  geom_node_text(aes(filter=Binary > .99, label = field_name), size = 5, repel = TRUE) +
  theme_graph() +
  ggtitle("IPC Technology Space: AI (1989-2003)") #

AI_dgr_2nd <- 
  g_tech_AI %N>%
  left_join(AI_RCA2, by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "#CCCCCC") +
  geom_node_point(aes(fill = sector, size = 1000^dgr, shape= sector)) +
  scale_shape_manual(values=c(21, 22, 23, 24, 25)) + scale_size("Degree",range = c(2, 12)) +
  geom_node_text(aes(filter=Binary > .99, label = field_name), size = 5, repel = TRUE) + 
  theme_graph() +
  ggtitle("IPC Technology Space: AI (1989-2003)") #

jpeg("Files_created_with_the_code/figures/new_figures/AI_RCA_2nd.jpg", width = 14, height = 10, units = 'in', res = 300)
AI_RCA_2nd 
dev.off()

jpeg("Files_created_with_the_code/figures/new_figures/AI_dgr_2nd.jpg", width = 14, height = 10, units = 'in', res = 300)
AI_dgr_2nd 
dev.off()

AI_com_2nd <- 
g_tech_AI %N>%
  left_join(AI_RCA2, filter=AI_RCA$Period_sim ==p, by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "#CCCCCC") +
  geom_node_point(aes(fill = sector, size = (100-Complexity), shape= sector)) +
  scale_shape_manual(values=c(21, 22, 23, 24, 25)) + labs(color   = "RCA")+ scale_size(range = c(1, 15)) +
  geom_label_repel(aes(x=x, y=y,label = ifelse((100-Complexity) >= 0,as.character(round((100-Complexity),2)),''), size = 8),nudge_x =  .7) +
  geom_label_repel(aes(x=x, y=y, label = ifelse(Binary >= 1,field_name,''), size = 10, colour = sector),fontface = 'bold', nudge_y = -5, nudge_x =  -3) + 
  theme_graph() +
  ggtitle("IPC Technology Space: AI (1989-2003)") #

jpeg("Files_created_with_the_code/figures/new_figures/AI_comp_2nd.jpg", width = 14, height = 10, units = 'in', res = 300)
AI_com_2nd 
dev.off()

#calculate network measures:
AI_2nd_networkMeasures <- 
  g_tech_AI %N>%
  left_join(AI_RCA2, by = c("name" = "techn_field_nr")) %>%
  mutate(n_neighbors = local_size(mindist = 1),
         weighted_degree = centrality_degree() / local_ave_degree(),
         triangles = local_triangles(),
         centrality_alpha= centrality_alpha(),
         centrality_authority = centrality_authority(),
         centrality_betweenness = centrality_betweenness(),
         centrality_power=centrality_power(),
         centrality_closeness = centrality_closeness(),
         centrality_eigen = centrality_eigen(),
         centrality_hub= centrality_hub(),
         centrality_subgraph=centrality_subgraph(),
         centrality_degree=centrality_degree(),
         centrality_closeness_harmonic=centrality_closeness_harmonic(),
         centrality_closeness_residual=centrality_closeness_residual(),
         centrality_betweenness_network=centrality_betweenness_network(),
  ) %>%
  as_tibble() 
write.xlsx(AI_2nd_networkMeasures, file = "Files_created_with_the_code/data/files_code_Fields_analysis/AI_2nd_networkMeasures.xlsx", row.names = F)
rm(AI_2nd_networkMeasures)

#save a new network just to check all the names later:
AI_RCA_2nd_allnames <-
  g_tech_AI %N>%
  left_join(AI_RCA2, by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "#CCCCCC") +
  geom_node_point(aes(fill = sector, size = RCA_AI_Period, shape= sector)) +
  scale_shape_manual(values=c(21, 22, 23, 24, 25)) + labs(color   = "RCA")+ scale_size(range = c(1, 12)) +
  geom_node_text(aes(label = field_name), size = 5, repel = TRUE) + #filter=Binary > .99, 
  theme_graph() +
  ggtitle("IPC Technology Space: AI (1974-1988)") #
jpeg("Files_created_with_the_code/figures/new_figures/AI_RCA_2nd_allnames.jpg", width = 14, height = 10, units = 'in', res = 300)
AI_RCA_2nd_allnames 
dev.off()
rm(AI_RCA_2nd_allnames)

rm(AI_dgr_2nd, AI_RCA_2nd, patents_AI_specific_2nd, coords_tech_AI, g_tech_AI, mat_tech_AI, mat_tech_rel_AI, AI_RCA2,
   AI_com_2nd, KnowlComp_2nd)

#3.1.3.Third period-----
patents_AI_specific_3rd <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/AI_ALL_patents.csv", sep = ";", header = TRUE, dec=",")

a = 2003
b = 2019

patents_AI_specific_3rd <- patents_AI_specific_3rd[patents_AI_specific_3rd$priority_year < b,]
patents_AI_specific_3rd <- patents_AI_specific_3rd[patents_AI_specific_3rd$priority_year > a,]

length(unique(patents_AI_specific_3rd$appln_id)) #34576
patents_AI_specific_3rd <- patents_AI_specific_3rd[is.na(patents_AI_specific_3rd$appln_id)==F,]

mat_tech_AI <- create_sparse_matrix(i = patents_AI_specific_3rd %>% pull(appln_id),
                                    j = patents_AI_specific_3rd %>% pull(techn_field_nr))

mat_tech_AI %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_rel_AI <- mat_tech_AI %>% 
  relatedness(method = "cosine")

KnowlComp_3rd <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/KnowledgeComp_3rd.csv", sep = ";", header = TRUE, dec=",")
KnowlComp_3rd <- KnowlComp_3rd[,c(1,6)]#
names(KnowlComp_3rd) <- c("techn_field_nr", "Complexity")
IPC_names <- IPC_names[,c(-5)]
IPC_names <- left_join(IPC_names, KnowlComp_3rd, by = "techn_field_nr")

g_tech_AI <- mat_tech_rel_AI %>% as_tbl_graph(directed = FALSE) %N>%
  left_join(IPC_names %>% mutate(techn_field_nr = techn_field_nr %>% as.character()), by = c("name" = "techn_field_nr")) %>%
  mutate(dgr = centrality_eigen(weights = weight)) %E>%
  filter(weight >= mean(weight))

#Create the Coordinates
coords_tech_AI <- g_tech_AI %>% igraph::layout.fruchterman.reingold() %>% as_tibble()
colnames(coords_tech_AI) <- c("x", "y")
#let's take a look at the most and less complex IPC fields:
g_tech_AI %N>% 
  arrange(desc(dgr)) %>%
  as_tibble() %>%
  slice(1:10)

g_tech_AI %N>% 
  arrange(dgr) %>%
  as_tibble() %>%
  slice(1:10)

g_tech_AI %>%
  ggraph(layout =  coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.4, colour = "grey") + 
  geom_node_point(aes(fill = sector, size = dgr, shape= sector))+ 
  scale_shape_manual(values=c(21, 22, 23, 24, 25)) + scale_size(range = c(2, 10)) +
  geom_node_text(aes(label = field_name), size = 4, repel = TRUE) + 
  theme_graph()+
  ggtitle("Technology Space: IPC codes") + 
  theme(legend.title = element_text(size = 14),
        legend.text = element_text(size = 10)) + 
  guides(colour = guide_legend(override.aes = list(size=10)))+
  geom_mark_hull(aes(x = x, y=y, colour = sector, fill= sector,
                     linetype = sector), alpha = 0.15, expand = unit(2.5, "mm"), size = 1) 

#period = 3
p=3
AI_RCA3 <- AI_RCA[AI_RCA$Period_sim == 3,]

AI_RCA_3rd <- 
  g_tech_AI %N>%
  left_join(AI_RCA3, by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "#CCCCCC") +
  geom_node_point(aes(fill = sector, size = RCA_AI_Period, shape= sector)) +
  scale_shape_manual(values=c(21, 22, 23, 24, 25)) + labs(color   = "RCA")+ scale_size(range = c(1, 12)) +
  geom_node_text(aes(filter=Binary > .99, label = field_name), size = 5, repel = TRUE) +
  theme_graph() +
  ggtitle("IPC Technology Space: AI (2004-2018)") #

AI_dgr_3rd <- 
  g_tech_AI %N>%
  left_join(AI_RCA3, by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "#CCCCCC") +
  geom_node_point(aes(fill = sector, size = 1000^dgr, shape= sector)) +
  scale_shape_manual(values=c(21, 22, 23, 24, 25)) + scale_size("Degree", range = c(2, 12)) +
  geom_node_text(aes(filter=Binary > .99, label = field_name), size = 5, repel = TRUE) + 
  theme_graph() +
  ggtitle("IPC Technology Space: AI (2004-2018)") #

jpeg("Files_created_with_the_code/figures/new_figures/AI_RCA_3rd.jpg", width = 14, height = 10, units = 'in', res = 300)
AI_RCA_3rd 
dev.off()

jpeg("Files_created_with_the_code/figures/new_figures/AI_dgr_3rd.jpg", width = 14, height = 10, units = 'in', res = 300)
AI_dgr_3rd 
dev.off()

AI_com_3rd <- 
g_tech_AI %N>%
  left_join(AI_RCA3, filter=AI_RCA$Period_sim ==p, by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "#CCCCCC") +
  geom_node_point(aes(fill = sector, size = (100-Complexity), shape= sector)) +
  scale_shape_manual(values=c(21, 22, 23, 24, 25)) + labs(color   = "RCA")+ scale_size(range = c(1, 15)) +
  geom_label_repel(aes(x=x, y=y,label = ifelse((100-Complexity) >= 0,as.character(round((100-Complexity),2)),''), size = 8),nudge_x =  .7) +
  geom_label_repel(aes(x=x, y=y, label = ifelse(Binary >= 1 & sector == 'Electrical engineering',field_name,''), size = 10, colour = sector),nudge_y = -4, nudge_x =  -6,fontface = 'bold') + 
  geom_label_repel(aes(x=x, y=y, label = ifelse(Binary >= 1 & sector != 'Electrical engineering',field_name,''), size = 10, colour = sector),nudge_y = 5, nudge_x =  6,fontface = 'bold') +
  theme_graph() +
  ggtitle("IPC Technology Space: AI (2004-2018)") #

jpeg("Files_created_with_the_code/figures/new_figures/AI_comp_3rd.jpg", width = 14, height = 10, units = 'in', res = 300)
AI_com_3rd 
dev.off()

#calculate network measures:
AI_3rd_networkMeasures <- 
  g_tech_AI %N>%
  left_join(AI_RCA3, by = c("name" = "techn_field_nr")) %>%
  mutate(n_neighbors = local_size(mindist = 1),
         weighted_degree = centrality_degree() / local_ave_degree(),
         triangles = local_triangles(),
         centrality_alpha= centrality_alpha(),
         centrality_authority = centrality_authority(),
         centrality_betweenness = centrality_betweenness(),
         centrality_power=centrality_power(),
         centrality_closeness = centrality_closeness(),
         centrality_eigen = centrality_eigen(),
         centrality_hub= centrality_hub(),
         centrality_subgraph=centrality_subgraph(),
         centrality_degree=centrality_degree(),
         centrality_closeness_harmonic=centrality_closeness_harmonic(),
         centrality_closeness_residual=centrality_closeness_residual(),
         centrality_betweenness_network=centrality_betweenness_network(),
  ) %>%
  as_tibble() 
write.xlsx(AI_3rd_networkMeasures, file = "Files_created_with_the_code/data/files_code_Fields_analysis/AI_3rd_networkMeasures.xlsx", row.names = F)
rm(AI_3rd_networkMeasures)

#save a new network just to check all the names later:
AI_RCA_3rd_allnames <-
  g_tech_AI %N>%
  left_join(AI_RCA3, by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "#CCCCCC") +
  geom_node_point(aes(fill = sector, size = RCA_AI_Period, shape= sector)) +
  scale_shape_manual(values=c(21, 22, 23, 24, 25)) + labs(color   = "RCA")+ scale_size(range = c(1, 12)) +
  geom_node_text(aes(label = field_name), size = 5, repel = TRUE) + #filter=Binary > .99, 
  theme_graph() +
  ggtitle("IPC Technology Space: AI (1974-1988)") #
jpeg("Files_created_with_the_code/figures/new_figures/AI_RCA_3rd_allnames.jpg", width = 14, height = 10, units = 'in', res = 300)
AI_RCA_3rd_allnames 
dev.off()
rm(AI_RCA_3rd_allnames)

rm(AI_dgr_3rd, AI_RCA_3rd, patents_AI_specific_3rd, coords_tech_AI, g_tech_AI, mat_tech_AI, mat_tech_rel_AI, AI_RCA3)

#put all the network metrics together:
library(readxl)
AI_1st_networkMeasures<-read_excel("Files_created_with_the_code/data/files_code_Fields_analysis/AI_1st_networkMeasures.xlsx")
AI_2nd_networkMeasures<-read_excel("Files_created_with_the_code/data/files_code_Fields_analysis/AI_2nd_networkMeasures.xlsx")
AI_3rd_networkMeasures<-read_excel("Files_created_with_the_code/data/files_code_Fields_analysis/AI_3rd_networkMeasures.xlsx")
AI_networkMeasures <- rbind(AI_1st_networkMeasures,AI_2nd_networkMeasures,AI_3rd_networkMeasures)
rm(AI_1st_networkMeasures,AI_2nd_networkMeasures,AI_3rd_networkMeasures)
write.xlsx(AI_networkMeasures, file = "Files_created_with_the_code/data/files_code_Fields_analysis/AI_networkMeasures_allperiods.xlsx", row.names = F)
rm(AI_networkMeasures)

#add AI cluster?
#add labels for what 0 to 3 mean (if Total_RCA_2 = 0, no specialization of the country at all, Total_RCA_2 = 1, 
#general, Total_RCA_2 = 2, ONLY AI; Total_RCA_2 = 3, BOTH AI AND GENERAL) 

#4.New calculation relatedness----
#4.1.First period ----
rm(list=ls())

#read the functions from before
group_by_applnID <- function (data){
  data %>%
    group_by(appln_id) %>%
    mutate(field_weight = 1 / n()) %>%
    ungroup()
}

group_by_ctry_and_IPC <- function (data){
  data %<>%
    group_by(ctry_code, techn_field_nr) %>%
    summarise(n_tech_reg = sum(field_weight)) %>%
    ungroup() %>%
    drop_na() 
}

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("large_files")

c <- 45182803 -40000000
IPC_all_patents_Part1 <- fread("All_patents_and_IPCs_Part2.csv", header = F, nrow = 20000000)
IPC_all_patents_Part2 <- fread("All_patents_and_IPCs_Part2.csv", header = F, nrow = 20000000, skip = 20000000)
IPC_all_patents_Part3 <- fread("All_patents_and_IPCs_Part2.csv", header = F, nrow = c, skip = 40000000)

names(IPC_all_patents_Part1) <- c("appln_id", "ctry_code", "techn_field_nr", "weight", "priority_year")
names(IPC_all_patents_Part2) <- c("appln_id", "ctry_code", "techn_field_nr", "weight", "priority_year")
names(IPC_all_patents_Part3) <- c("appln_id", "ctry_code", "techn_field_nr", "weight", "priority_year")

#we want to pick only the registers from the period we want (from 1974 to 1988, including both cited years)
a = 1973
b = 1989

IPC_all_patents_Part1 <- IPC_all_patents_Part1[IPC_all_patents_Part1$priority_year < b,]
IPC_all_patents_Part1 <- IPC_all_patents_Part1[IPC_all_patents_Part1$priority_year > a,]

IPC_all_patents_Part2 <- IPC_all_patents_Part2[IPC_all_patents_Part2$priority_year < b,]
IPC_all_patents_Part2 <- IPC_all_patents_Part2[IPC_all_patents_Part2$priority_year > a,]

IPC_all_patents_Part3 <- IPC_all_patents_Part3[IPC_all_patents_Part3$priority_year < b,]
IPC_all_patents_Part3 <- IPC_all_patents_Part3[IPC_all_patents_Part3$priority_year > a,]

#let's drop the columns we won't use (priority_year):
IPC_all_patents_Part1 <- IPC_all_patents_Part1[, c((-4), (-5))]
IPC_all_patents_Part2 <- IPC_all_patents_Part2[, c((-4), (-5))]
IPC_all_patents_Part3 <- IPC_all_patents_Part3[, c((-4), (-5))]

#we combine the 3 files:
IPC_all_patents_FirstPeriod <- rbind(IPC_all_patents_Part1, IPC_all_patents_Part2, IPC_all_patents_Part3)
#and exclude the 3 big ones we just used, so we have back our memory:
rm(IPC_all_patents_Part1, IPC_all_patents_Part2, IPC_all_patents_Part3)

length(unique(IPC_all_patents_FirstPeriod$appln_id))#4816006 unique publication numbers (i.e., unique priorities) out of
length(IPC_all_patents_FirstPeriod$appln_id) #13279651 lines of data

#now we apply the 2 functions we created at the beginning of this section:
reg_tech1 <- group_by_applnID(IPC_all_patents_FirstPeriod)
reg_tech1 <- group_by_ctry_and_IPC(reg_tech1)

mat_reg_tech1 <- reg_tech1 %>%
  arrange(techn_field_nr, ctry_code) %>%
  pivot_wider(names_from = techn_field_nr, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

mat_reg_tech1 %<>% remove_rownames %>% column_to_rownames(var="ctry_code") %>%
  as.matrix() %>%
  round()

reg_RCA_1stPeriod <- mat_reg_tech1 %>% location.quotient(binary = T) %>% 
  as.data.frame() %>% 
  rownames_to_column("ctry_code") %>% 
  as_tibble() %>% 
  gather(key = "techn_field_nr", value = "RCA", -ctry_code) %>%
  arrange(ctry_code, techn_field_nr)

###Now for AI:
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
reg_tech1_AI <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/reg_techAI_FirstPeriod.csv", sep = ";", header = TRUE, dec=",")

mat_reg_tech1_AI <- reg_tech1_AI %>%
  arrange(techn_field_nr, ctry_code) %>%
  pivot_wider(names_from = techn_field_nr, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

mat_reg_tech1_AI %<>% remove_rownames %>% column_to_rownames(var="ctry_code") %>%
  as.matrix() %>%
  round()

reg_RCA1_AI <- mat_reg_tech1_AI %>% location.quotient(binary = T) %>% 
  as.data.frame() %>% 
  rownames_to_column("ctry_code") %>% 
  as_tibble() %>% 
  gather(key = "techn_field_nr", value = "RCA", -ctry_code) %>%
  arrange(ctry_code, techn_field_nr)

reg_RCA1_AI <- reg_RCA1_AI[reg_RCA1_AI$ctry_code == "AI_pat",]

###end of AI part, let's merge it with the previous dataset:
reg_RCA_1stPeriod <- rbind(reg_RCA_1stPeriod, reg_RCA1_AI)
rm(reg_RCA1_AI, mat_reg_tech1_AI, reg_tech1_AI)

reg_RCA_1stPeriod %<>%
  arrange(techn_field_nr, ctry_code) %>%
  pivot_wider(names_from = techn_field_nr, values_from = RCA, values_fill = list(RCA = 0))

reg_RCA_1stPeriod %<>% remove_rownames %>% column_to_rownames(var="ctry_code") %>%
  as.matrix() %>%
  round()

#now, we calculate the technological matrix for the considered period (or, we can load it from the files we have; 
#we can look at it per period or for the whole period);

#first, we create the function we need:
create_sparse_matrix <- function(i.input, j.input){
  require(Matrix)
  mat <- spMatrix(nrow = i.input %>% n_distinct(),
                  ncol = j.input %>% n_distinct(),
                  i = i.input %>% factor() %>% as.numeric(),
                  j = j.input %>% factor() %>% as.numeric(),
                  x = rep(1, i.input %>% length() ) )
  
  row.names(mat) <- i.input %>% factor() %>% levels()
  colnames(mat) <- j.input %>% factor() %>% levels()
  return(mat)
}

Relatedness_1stPeriod <- create_sparse_matrix(i = IPC_all_patents_FirstPeriod %>% pull(appln_id),
                                           j = IPC_all_patents_FirstPeriod %>% pull(techn_field_nr))

Relatedness_1stPeriod %<>% 
  crossprod() %>% 
  as.matrix() 
  

Relatedness_1stPeriod_asso <- relatedness(Relatedness_1stPeriod, method = "association")
Relatedness_1stPeriod_jacc <- relatedness(Relatedness_1stPeriod, method = "Jaccard")
Relatedness_1stPeriod_prob <- relatedness(Relatedness_1stPeriod, method = "prob")
Relatedness_1stPeriod_cos <- relatedness(Relatedness_1stPeriod, method = "cosine")

Relatedness_Association <- relatedness.density.int.avg(reg_RCA_1stPeriod,Relatedness_1stPeriod_asso)
Relatedness_Association <- as.data.frame(Relatedness_Association)

Relatedness_Jaccard <- relatedness.density.int.avg(reg_RCA_1stPeriod,Relatedness_1stPeriod_jacc)
Relatedness_Jaccard <- as.data.frame(Relatedness_Jaccard)

Relatedness_Prob <- relatedness.density.int.avg(reg_RCA_1stPeriod,Relatedness_1stPeriod_prob)
Relatedness_Prob <- as.data.frame(Relatedness_Prob)

Relatedness_Cosine <- relatedness.density.int.avg(reg_RCA_1stPeriod,Relatedness_1stPeriod_cos)
Relatedness_Cosine <- as.data.frame(Relatedness_Cosine)

Relatedness_1stPeriod <- cbind(Relatedness_Association, Relatedness_Jaccard, Relatedness_Prob, Relatedness_Cosine)
rm(Relatedness_Association, Relatedness_Jaccard, Relatedness_Prob, Relatedness_Cosine)
rm(IPC_all_patents_FirstPeriod, mat_reg_tech1, reg_RCA_1stPeriod, reg_tech1, Relatedness_1stPeriod_asso,
   Relatedness_1stPeriod_jacc, Relatedness_1stPeriod_prob, Relatedness_1stPeriod_cos)

Relatedness_1stPeriod$Period <- "1st"

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
write.csv2(Relatedness_1stPeriod, file = "Files_created_with_the_code/data/new_analysis/Relatedness_1st.csv", row.names = TRUE)

#4.2.Second period ----
setwd("large_files")
c <- 45182803 -40000000
IPC_all_patents_Part1 <- fread("All_patents_and_IPCs_Part2.csv", header = F, nrow = 20000000)
IPC_all_patents_Part2 <- fread("All_patents_and_IPCs_Part2.csv", header = F, nrow = 20000000, skip = 20000000)
IPC_all_patents_Part3 <- fread("All_patents_and_IPCs_Part2.csv", header = F, nrow = c, skip = 40000000)

names(IPC_all_patents_Part1) <- c("appln_id", "ctry_code", "techn_field_nr", "weight", "priority_year")
names(IPC_all_patents_Part2) <- c("appln_id", "ctry_code", "techn_field_nr", "weight", "priority_year")
names(IPC_all_patents_Part3) <- c("appln_id", "ctry_code", "techn_field_nr", "weight", "priority_year")

a = 1988
b = 2004

IPC_all_patents_Part1 <- IPC_all_patents_Part1[IPC_all_patents_Part1$priority_year < b,]
IPC_all_patents_Part1 <- IPC_all_patents_Part1[IPC_all_patents_Part1$priority_year > a,]

IPC_all_patents_Part2 <- IPC_all_patents_Part2[IPC_all_patents_Part2$priority_year < b,]
IPC_all_patents_Part2 <- IPC_all_patents_Part2[IPC_all_patents_Part2$priority_year > a,]

IPC_all_patents_Part3 <- IPC_all_patents_Part3[IPC_all_patents_Part3$priority_year < b,]
IPC_all_patents_Part3 <- IPC_all_patents_Part3[IPC_all_patents_Part3$priority_year > a,]

#let's drop the columns we won't use (priority_year):
IPC_all_patents_Part1 <- IPC_all_patents_Part1[, c((-4), (-5))]
IPC_all_patents_Part2 <- IPC_all_patents_Part2[, c((-4), (-5))]
IPC_all_patents_Part3 <- IPC_all_patents_Part3[, c((-4), (-5))]

#we combine the 3 files:
IPC_all_patents_SecondPeriod <- rbind(IPC_all_patents_Part1, IPC_all_patents_Part2, IPC_all_patents_Part3)
#and exclude the 3 big ones we just used, so we have back our memory:
rm(IPC_all_patents_Part1, IPC_all_patents_Part2, IPC_all_patents_Part3)

length(unique(IPC_all_patents_SecondPeriod$appln_id))#8848446 unique publication numbers (i.e., unique priorities) out of
length(IPC_all_patents_SecondPeriod$appln_id) #28281321 lines of data

#now we apply the 2 functions we created at the beginning of this section:
reg_tech2 <- group_by_applnID(IPC_all_patents_SecondPeriod)
reg_tech2 <- group_by_ctry_and_IPC(reg_tech2)

mat_reg_tech2 <- reg_tech2 %>%
  arrange(techn_field_nr, ctry_code) %>%
  pivot_wider(names_from = techn_field_nr, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

mat_reg_tech2 %<>% remove_rownames %>% column_to_rownames(var="ctry_code") %>%
  as.matrix() %>%
  round()

reg_RCA_2ndPeriod <- mat_reg_tech2 %>% location.quotient(binary = T) %>% 
  as.data.frame() %>% 
  rownames_to_column("ctry_code") %>% 
  as_tibble() %>% 
  gather(key = "techn_field_nr", value = "RCA", -ctry_code) %>%
  arrange(ctry_code, techn_field_nr)

###Now for AI:
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
reg_tech2_AI <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/reg_techAI_SecondPeriod.csv", sep = ";", header = TRUE, dec=",")

mat_reg_tech2_AI <- reg_tech2_AI %>%
  arrange(techn_field_nr, ctry_code) %>%
  pivot_wider(names_from = techn_field_nr, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

mat_reg_tech2_AI %<>% remove_rownames %>% column_to_rownames(var="ctry_code") %>%
  as.matrix() %>%
  round()

reg_RCA2_AI <- mat_reg_tech2_AI %>% location.quotient(binary = T) %>% 
  as.data.frame() %>% 
  rownames_to_column("ctry_code") %>% 
  as_tibble() %>% 
  gather(key = "techn_field_nr", value = "RCA", -ctry_code) %>%
  arrange(ctry_code, techn_field_nr)

reg_RCA2_AI <- reg_RCA2_AI[reg_RCA2_AI$ctry_code == "AI_pat",]

###end of AI part, let's merge it with the previous dataset:
reg_RCA_2ndPeriod <- rbind(reg_RCA_2ndPeriod, reg_RCA2_AI)
rm(reg_RCA2_AI, mat_reg_tech2_AI, reg_tech2_AI)

reg_RCA_2ndPeriod %<>%
  arrange(techn_field_nr, ctry_code) %>%
  pivot_wider(names_from = techn_field_nr, values_from = RCA, values_fill = list(RCA = 0))

reg_RCA_2ndPeriod %<>% remove_rownames %>% column_to_rownames(var="ctry_code") %>%
  as.matrix() %>%
  round()

#now, we calculate the technological matrix for the considered period
Relatedness_2ndPeriod <- create_sparse_matrix(i = IPC_all_patents_SecondPeriod %>% pull(appln_id),
                                              j = IPC_all_patents_SecondPeriod %>% pull(techn_field_nr))

Relatedness_2ndPeriod %<>% 
  crossprod() %>% 
  as.matrix() 


Relatedness_2ndPeriod_asso <- relatedness(Relatedness_2ndPeriod, method = "association")
Relatedness_2ndPeriod_jacc <- relatedness(Relatedness_2ndPeriod, method = "Jaccard")
Relatedness_2ndPeriod_prob <- relatedness(Relatedness_2ndPeriod, method = "prob")
Relatedness_2ndPeriod_cos <- relatedness(Relatedness_2ndPeriod, method = "cosine")

Relatedness_Association <- relatedness.density.int.avg(reg_RCA_2ndPeriod,Relatedness_2ndPeriod_asso)
Relatedness_Association <- as.data.frame(Relatedness_Association)

Relatedness_Jaccard <- relatedness.density.int.avg(reg_RCA_2ndPeriod,Relatedness_2ndPeriod_jacc)
Relatedness_Jaccard <- as.data.frame(Relatedness_Jaccard)

Relatedness_Prob <- relatedness.density.int.avg(reg_RCA_2ndPeriod,Relatedness_2ndPeriod_prob)
Relatedness_Prob <- as.data.frame(Relatedness_Prob)

Relatedness_Cosine <- relatedness.density.int.avg(reg_RCA_2ndPeriod,Relatedness_2ndPeriod_cos)
Relatedness_Cosine <- as.data.frame(Relatedness_Cosine)

Relatedness_2ndPeriod <- cbind(Relatedness_Association, Relatedness_Jaccard, Relatedness_Prob, Relatedness_Cosine)
rm(Relatedness_Association, Relatedness_Jaccard, Relatedness_Prob, Relatedness_Cosine)
rm(IPC_all_patents_SecondPeriod, mat_reg_tech2, reg_RCA_2ndPeriod, reg_tech2, Relatedness_2ndPeriod_asso,
   Relatedness_2ndPeriod_jacc, Relatedness_2ndPeriod_prob, Relatedness_2ndPeriod_cos)

Relatedness_2ndPeriod$Period <- "2nd"

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
write.csv2(Relatedness_2ndPeriod, file = "Files_created_with_the_code/data/new_analysis/Relatedness_2nd.csv", row.names = TRUE)

#4.3.Third period ----
#For the third period, which goes from 2004 to 2018, we  need only the dataset from Part1. This
#specific dataset only has patents from 2004 to 2018, so we don't have to filter it.
setwd("large_files")
c <- 58841893-40000000
IPC_all_patents_Part1 <- fread("All_patents_and_IPCs_Part1.csv", header = F, nrow = 20000000)
IPC_all_patents_Part2 <- fread("All_patents_and_IPCs_Part1.csv", header = F, nrow = 20000000, skip = 20000000)
IPC_all_patents_Part3 <- fread("All_patents_and_IPCs_Part1.csv", header = F, nrow = c, skip = 40000000)

names(IPC_all_patents_Part1) <- c("appln_id", "ctry_code", "techn_field_nr", "weight", "priority_year")
names(IPC_all_patents_Part2) <- c("appln_id", "ctry_code", "techn_field_nr", "weight", "priority_year")
names(IPC_all_patents_Part3) <- c("appln_id", "ctry_code", "techn_field_nr", "weight", "priority_year")

IPC_all_patents_Part1 <- IPC_all_patents_Part1[, c((-4), (-5))]
IPC_all_patents_Part2 <- IPC_all_patents_Part2[, c((-4), (-5))]
IPC_all_patents_Part3 <- IPC_all_patents_Part3[, c((-4), (-5))]

#we combine the 3 files:
IPC_all_patents_ThirdPeriod <- rbind(IPC_all_patents_Part1, IPC_all_patents_Part2, IPC_all_patents_Part3)
#and exclude the 3 big ones we just used, so we have back our memory:
rm(IPC_all_patents_Part1, IPC_all_patents_Part2, IPC_all_patents_Part3)

length(unique(IPC_all_patents_ThirdPeriod$appln_id))#16270598 unique publication numbers (i.e., unique priorities) out of
length(IPC_all_patents_ThirdPeriod$appln_id) #58841893 lines of data

#now we apply the 2 functions we created at the beginning of this section:
reg_tech3 <- group_by_applnID(IPC_all_patents_ThirdPeriod)
reg_tech3 <- group_by_ctry_and_IPC(reg_tech3)

mat_reg_tech3 <- reg_tech3 %>%
  arrange(techn_field_nr, ctry_code) %>%
  pivot_wider(names_from = techn_field_nr, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

mat_reg_tech3 %<>% remove_rownames %>% column_to_rownames(var="ctry_code") %>%
  as.matrix() %>%
  round()

reg_RCA_3rdPeriod <- mat_reg_tech3 %>% location.quotient(binary = T) %>% 
  as.data.frame() %>% 
  rownames_to_column("ctry_code") %>% 
  as_tibble() %>% 
  gather(key = "techn_field_nr", value = "RCA", -ctry_code) %>%
  arrange(ctry_code, techn_field_nr)

###Now for AI:
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
reg_tech3_AI <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/reg_techAI_ThirdPeriod.csv", sep = ";", header = TRUE, dec=",")

mat_reg_tech3_AI <- reg_tech3_AI %>%
  arrange(techn_field_nr, ctry_code) %>%
  pivot_wider(names_from = techn_field_nr, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

mat_reg_tech3_AI %<>% remove_rownames %>% column_to_rownames(var="ctry_code") %>%
  as.matrix() %>%
  round()

reg_RCA3_AI <- mat_reg_tech3_AI %>% location.quotient(binary = T) %>% 
  as.data.frame() %>% 
  rownames_to_column("ctry_code") %>% 
  as_tibble() %>% 
  gather(key = "techn_field_nr", value = "RCA", -ctry_code) %>%
  arrange(ctry_code, techn_field_nr)

reg_RCA3_AI <- reg_RCA3_AI[reg_RCA3_AI$ctry_code == "AI_pat",]

###end of AI part, let's merge it with the previous dataset:
reg_RCA_3rdPeriod <- rbind(reg_RCA_3rdPeriod, reg_RCA3_AI)
rm(reg_RCA3_AI, mat_reg_tech3_AI, reg_tech3_AI)

reg_RCA_3rdPeriod %<>%
  arrange(techn_field_nr, ctry_code) %>%
  pivot_wider(names_from = techn_field_nr, values_from = RCA, values_fill = list(RCA = 0))

reg_RCA_3rdPeriod %<>% remove_rownames %>% column_to_rownames(var="ctry_code") %>%
  as.matrix() %>%
  round()

#now, we calculate the technological matrix for the considered period
Relatedness_3rdPeriod <- create_sparse_matrix(i = IPC_all_patents_ThirdPeriod %>% pull(appln_id),
                                              j = IPC_all_patents_ThirdPeriod %>% pull(techn_field_nr))

Relatedness_3rdPeriod %<>% 
  crossprod() %>% 
  as.matrix() 

Relatedness_3rdPeriod_asso <- relatedness(Relatedness_3rdPeriod, method = "association")
Relatedness_3rdPeriod_jacc <- relatedness(Relatedness_3rdPeriod, method = "Jaccard")
Relatedness_3rdPeriod_prob <- relatedness(Relatedness_3rdPeriod, method = "prob")
Relatedness_3rdPeriod_cos <- relatedness(Relatedness_3rdPeriod, method = "cosine")

Relatedness_Association <- relatedness.density.int.avg(reg_RCA_3rdPeriod,Relatedness_3rdPeriod_asso)
Relatedness_Association <- as.data.frame(Relatedness_Association)

Relatedness_Jaccard <- relatedness.density.int.avg(reg_RCA_3rdPeriod,Relatedness_3rdPeriod_jacc)
Relatedness_Jaccard <- as.data.frame(Relatedness_Jaccard)

Relatedness_Prob <- relatedness.density.int.avg(reg_RCA_3rdPeriod,Relatedness_3rdPeriod_prob)
Relatedness_Prob <- as.data.frame(Relatedness_Prob)

Relatedness_Cosine <- relatedness.density.int.avg(reg_RCA_3rdPeriod,Relatedness_3rdPeriod_cos)
Relatedness_Cosine <- as.data.frame(Relatedness_Cosine)

Relatedness_3rdPeriod <- cbind(Relatedness_Association, Relatedness_Jaccard, Relatedness_Prob, Relatedness_Cosine)
rm(Relatedness_Association, Relatedness_Jaccard, Relatedness_Prob, Relatedness_Cosine)
rm(IPC_all_patents_ThirdPeriod, mat_reg_tech3, reg_RCA_3rdPeriod, reg_tech3, Relatedness_3rdPeriod_asso,
   Relatedness_3rdPeriod_jacc, Relatedness_3rdPeriod_prob, Relatedness_3rdPeriod_cos)

Relatedness_3rdPeriod$Period <- "3rd"

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
write.csv2(Relatedness_3rdPeriod, file = "Files_created_with_the_code/data/new_analysis/Relatedness_3rd.csv", row.names = TRUE)

Relatedness_Allperiods <- rbind(Relatedness_1stPeriod, Relatedness_2ndPeriod, Relatedness_3rdPeriod)
write.csv2(Relatedness_Allperiods, file = "Files_created_with_the_code/data/new_analysis/Relatedness_3periods_2.csv", row.names = TRUE)

#test1 <- read.csv("Files_created_with_the_code/data/new_analysis/Relatedness_3periods.csv", sep = ";", header = TRUE, dec=",")

#5.New Figures indicators 3 (AI) and 7 (countries) ----
#NOT USED FOR NOW
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

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

#5.1.Relatedness ----
#Relatedness_2nd <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/Relatedness_2nd_period_IPC.csv", sep = ";", header = TRUE, dec=",")
#Relatedness_2nd <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/Relatedness_2nd_period_IPC.csv", sep = ";", header = TRUE, dec=",")
Relatedness <- read.csv("Files_created_with_the_code/data/new_analysis/Relatedness_3periods_2.csv", sep = ";", header = TRUE, dec=",")

Relatedness$Period <- gsub("1st", "Period 1 (1974-1988)", str_trim(Relatedness$Period))
Relatedness$Period <- gsub("2nd", "Period 2 (1989-2003)", str_trim(Relatedness$Period))
Relatedness$Period <- gsub("3rd", "Period 3 (2004-2018)", str_trim(Relatedness$Period))

#drops 1s and 2s from the countries/AI names
Relatedness$X <- gsub("1", "", str_trim(Relatedness$X))
Relatedness$X <- gsub("2", "", str_trim(Relatedness$X))

Relatedness_AI <- Relatedness[Relatedness$X == "AI_pat", ]
Relatedness_countries <- Relatedness[Relatedness$X == "CN" | Relatedness$X == "KR"|
                                       Relatedness$X == "US" | Relatedness$X == "JP", ]
rm(Relatedness)
#coloured figures
library(RColorBrewer)
library(scales) #for scaling without cutting data out
library(tidyverse) #
Rel_byP_c_Colour <- 
  ggplot(Relatedness_countries, aes(x=X, y=Relatedness_Cosine, fill=Period)) +
  geom_bar(stat="identity", position=position_dodge(), show.legend = F)+theme_minimal()+ xlab(NULL) + ylab("Relatedness") +
      theme_classic() +
  ggtitle("Countries density relatedness") + 
  scale_y_continuous(limits=c(30,60),oob = rescale_none) + 
  scale_fill_brewer(palette = "YlOrRd")

Rel_byAI_c_Colour<- 
ggplot(Relatedness_AI, aes(x=X, y=Relatedness_Cosine, fill=Period)) +
  geom_bar(stat="identity", position=position_dodge(), show.legend = F)+theme_minimal() + labs(x = "") +
  scale_x_discrete(labels = NULL) + ylab("Relatedness") +
  theme_classic() +
  ggtitle("AI density relatedness")+
  #scale_y_continuous(limits=c(.1,2.35),oob = rescale_none) + 
  scale_fill_brewer(palette = "YlOrRd")

#5.2.Knowld Comp. AI -----
KnowlComp_1st_AI <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/KnowledgeComp_PerCountry_1st_All_RCAs_AI.csv", sep = ";", header = TRUE, dec=",")
KnowlComp_2nd_AI <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/KnowledgeComp_PerCountry_2nd_All_RCAs_AI.csv", sep = ";", header = TRUE, dec=",")
KnowlComp_3rd_AI <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/KnowledgeComp_PerCountry_3rd_All_RCAs_AI.csv", sep = ";", header = TRUE, dec=",")

KnowlComp_1st_AI$Period <- "Period 1 (1974-1988)"
KnowlComp_2nd_AI$Period <- "Period 2 (1989-2003)"
KnowlComp_3rd_AI$Period <- "Period 3 (2004-2018)"
KnowledgeCompl_AI <- rbind(KnowlComp_1st_AI, KnowlComp_2nd_AI, KnowlComp_3rd_AI)
rm(KnowlComp_1st_AI, KnowlComp_2nd_AI, KnowlComp_3rd_AI)
KnowledgeCompl_AI <- KnowledgeCompl_AI[KnowledgeCompl_AI$X == "AI_pat3", ]
KnowledgeCompl_AI$"Overall Complexity" = rowSums(KnowledgeCompl_AI[,c(2:36)])

KnowledgeCompl_AI<- KnowledgeCompl_AI[,c(1, 38, 39)] #[,c(1, 38, 40)]
names(KnowledgeCompl_AI) <- c("Country", "Period", "Value")
KnowledgeCompl_AI$Indicator <- "Overall Complexity"

KnowledgeCompl_AI$Country <- gsub("AI_pat3", "AI", str_trim(KnowledgeCompl_AI$Country))

Comp_byAI_c_Colour<- 
  ggplot(KnowledgeCompl_AI, aes(x=Country, y=Value, fill=Period)) +
  geom_bar(stat="identity", position=position_dodge())+theme_minimal() + labs(x = "") +
  scale_x_discrete(labels = NULL) + ylab("Knowledge Complexity (MORt)") +
  theme_classic() + theme(legend.position="bottom") +
  ggtitle("AI Knowledge Complexity")+ 
  scale_fill_brewer(palette = "YlOrRd")

#5.3.Knowld Comp. Countries -----
KnowlComp_1st <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/KnowledgeComp_1st_Morc.csv", sep = ";", header = T, dec=",")
KnowlComp_2nd <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/KnowledgeComp_2nd_Morc.csv", sep = ";", header = TRUE, dec=",")
KnowlComp_3rd <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/KnowledgeComp_3rd_Morc.csv", sep = ";", header = TRUE, dec=",")

names(KnowlComp_1st) <- c("Country", "MORc", "RCA", "RCA_step0", "RCA_step1", "RCA_step2")
names(KnowlComp_2nd) <- c("Country", "MORc", "RCA", "RCA_step0", "RCA_step1", "RCA_step2")
names(KnowlComp_3rd) <- c("Country", "MORc", "RCA", "RCA_step0", "RCA_step1", "RCA_step2")

KnowlComp_1st$Period <- "Period 1 (1974-1988)"
KnowlComp_2nd$Period <- "Period 2 (1989-2003)"
KnowlComp_3rd$Period <- "Period 3 (2004-2018)"

KnowledgeCompl <- rbind(KnowlComp_1st, KnowlComp_2nd, KnowlComp_3rd)
rm(KnowlComp_1st, KnowlComp_2nd, KnowlComp_3rd)
KnowledgeCompl$Category <- "Overall Complexity"

KnowledgeCompl<-KnowledgeCompl[KnowledgeCompl$Country == "US"|
                                                   KnowledgeCompl$Country == "CN"|
                                                   KnowledgeCompl$Country == "KR"|
                                                   KnowledgeCompl$Country == "JP",]

#figure colour:
Comp_byP_c_Colour <-
  ggplot(KnowledgeCompl, aes(x=Country, y=RCA_step1, fill=Period)) +
  geom_bar(stat="identity", position=position_dodge())+theme_minimal()+ xlab(NULL) + ylab("Knowledge Complexity (MORc)") +
  theme_classic()  + theme(legend.position="bottom") +
  ggtitle("Countries Knowledge Complexity") + 
  scale_fill_brewer(palette = "YlOrRd")

#regular resolution:
jpeg("Files_created_with_the_code/figures/new_figures/Fig3_Relatedness_and_Complex_AICol.jpg", width = 6, height = 8, units = 'in', res = 300)
multiplot(Rel_byAI_c_Colour, Comp_byAI_c_Colour, cols=1) 
dev.off()

jpeg("Files_created_with_the_code/figures/new_figures/Fig7_Relatedness_and_Complex_Morc_countriesColour.jpg", width = 6, height = 8, units = 'in', res = 300)
multiplot(Rel_byP_c_Colour, Comp_byP_c_Colour, cols=1) 
dev.off()

#5.4.Figure Discussions (9) ----
#scatter plot using data from All_data_knowlComp_Morc and Relatedness
Relatedness_countries2 <- Relatedness_countries
Relatedness_countries2 <- Relatedness_countries2[,c(1,5,6)]
names(Relatedness_countries2) <- c("Country", "Relatedness", "Period")
KnowledgeCompl2 <- left_join(KnowledgeCompl, Relatedness_countries2, by = c("Country", "Period"))

library(ggraph) # For ggplot2 style graph plotting
Option1_countries <-
KnowledgeCompl2 %>%
  ggplot(aes(x=RCA_step1, y=Relatedness, color=Period, shape = Country)) +
  geom_point(size=10) + 
  geom_path(color="black", linetype = "dashed", arrow = arrow(angle = 15, type = "closed"), size=1) +
  scale_shape_manual(values=c(16, 15, 17, 18)) +
  xlab("Knowledge complexity (MORc)") +
  ylab("Relatedness") +
  ggtitle("Technological development of AI-leading countries") +
  theme_classic() + #theme(legend.position="bottom") +
  geom_node_text(aes(x=RCA_step1,y=Relatedness, label = Country), size = 8, repel = TRUE) + 
  scale_color_brewer(palette = "YlOrRd")

Option2_countries <-
KnowledgeCompl2 %>%
  ggplot(aes(x=Relatedness, y=RCA_step1, color=Period, shape = Country)) +
  geom_point(size=10) + 
  geom_path(color="black", linetype = "dashed", arrow = arrow(angle = 15, type = "closed"), size=1) +
  scale_shape_manual(values=c(16, 15, 17, 18)) +
  xlab("Relatedness") +
  ylab("Knowledge complexity (MORc)") +
  ggtitle("Technological development of AI-leading countries") +
  theme_classic() + #theme(legend.position="bottom") +
  geom_node_text(aes(x=Relatedness,y=RCA_step1, label = Country), size = 8, repel = TRUE) + 
  scale_color_brewer(palette = "YlOrRd")

jpeg("Files_created_with_the_code/figures/new_figures/Fig9_Overall_countries_option1.jpg", width = 12, height = 5, units = 'in', res = 300)
Option1_countries
dev.off()

jpeg("Files_created_with_the_code/figures/new_figures/Fig9_Overall_countries_option2.jpg", width = 12, height = 5, units = 'in', res = 300)
Option2_countries
dev.off()

rm(Option1_countries, Option2_countries, Relatedness_countries2,KnowledgeCompl2)

#for AI:
Relatedness_AI2 <- Relatedness_AI
Relatedness_AI2 <- Relatedness_AI2[,c(1,5,6)]
names(Relatedness_AI2) <- c("Country", "Relatedness", "Period")
Relatedness_AI2$Country <- gsub("AI_pat", "AI", str_trim(Relatedness_AI2$Country))
KnowledgeCompl_AI2 <- left_join(KnowledgeCompl_AI, Relatedness_AI2, by = c("Country", "Period"))

Option1_AI <-
  KnowledgeCompl_AI2 %>%
  ggplot(aes(x=Value, y=Relatedness, color=Period, shape = Country)) +
  geom_point(size=10) + 
  geom_path(color="black", linetype = "dashed", arrow = arrow(angle = 15, type = "closed"), size=1) +
  scale_shape_manual(values=c(16, 15, 17, 18)) +
  xlab("Knowledge complexity (MORc)") +
  ylab("Relatedness") +
  ggtitle("Technological development of AI") +
  theme_classic() + #theme(legend.position="bottom") +
  geom_node_text(aes(x=Value,y=Relatedness, label = Country), size = 8, repel = TRUE) + 
  scale_color_brewer(palette = "YlOrRd")

Option2_AI <-
  KnowledgeCompl_AI2 %>%
  ggplot(aes(x=Relatedness, y=Value, color=Period, shape = Country)) +
  geom_point(size=10) + 
  geom_path(color="black", linetype = "dashed", arrow = arrow(angle = 15, type = "closed"), size=1) +
  scale_shape_manual(values=c(16, 15, 17, 18)) +
  xlab("Relatedness") +
  ylab("Knowledge complexity (MORc)") +
  ggtitle("Technological development of AI") +
  theme_classic() + #theme(legend.position="bottom") +
  geom_node_text(aes(x=Relatedness,y=Value, label = Country), size = 8, repel = TRUE) + 
  scale_color_brewer(palette = "YlOrRd")

jpeg("Files_created_with_the_code/figures/new_figures/Fig8_Overall_AI_option1.jpg", width = 12, height = 5, units = 'in', res = 300)
Option1_AI
dev.off()

jpeg("Files_created_with_the_code/figures/new_figures/Fig8_Overall_AI_option2.jpg", width = 12, height = 5, units = 'in', res = 300)
Option2_AI
dev.off()

rm(Option1_AI, Option2_AI, Relatedness_AI2,KnowledgeCompl_AI2)



#6.Overlapping specializations ----
#6.1.Technological fields ----
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
IPC_RCAs_Top4 <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/RCA_4countries_detailed.csv", sep = ";", header = TRUE, dec=",")
IPC_RCAs_Top4$Total_RCA <- as.factor(IPC_RCAs_Top4$Total_RCA)
IPC_RCAs_Top4$Period_sim <- as.numeric(factor(IPC_RCAs_Top4$Period,levels=unique(IPC_RCAs_Top4$Period)))
IPC_RCAs_Top4$techn_field_nr <- as.character(IPC_RCAs_Top4$techn_field_nr)

#replace names:
IPC_RCAs_Top4$ctry_code <- gsub("US", "USA", str_trim(IPC_RCAs_Top4$ctry_code))
IPC_RCAs_Top4$ctry_code <- gsub("CN", "China", str_trim(IPC_RCAs_Top4$ctry_code))
IPC_RCAs_Top4$ctry_code <- gsub("JP", "Japan", str_trim(IPC_RCAs_Top4$ctry_code))
IPC_RCAs_Top4$ctry_code <- gsub("KR", "South Korea", str_trim(IPC_RCAs_Top4$ctry_code))

AI_RCA <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/Specializations_All_periods_IPC.csv", sep = ";", header = TRUE, dec=",")
AI_RCA$Period_sim <- as.numeric(factor(AI_RCA$Period,levels=unique(AI_RCA$Period)))
AI_RCA <- AI_RCA[,c(2,9,13)]
AI_RCA$techn_field_nr <- as.character(AI_RCA$techn_field_nr)
names(AI_RCA) <- c("techn_field_nr", "RCA_AI_Period", "Period_sim")
IPC_RCAs_Top4 <- left_join(IPC_RCAs_Top4, AI_RCA, by = c("techn_field_nr", "Period_sim"))

#fix Total_RCA:
#so, if Total_RCA_2 = 0, no specialization of the country at all, Total_RCA_2 = 1, general, Total_RCA_2 = 2, ONLY AI;
#Total_RCA_2 = 3, BOTH AI AND GENERAL
IPC_RCAs_Top4$Total_RCA_2 <- IPC_RCAs_Top4$Round_general + 2*IPC_RCAs_Top4$Round_AI
rm(AI_RCA)

IPC_RCAs_Top4$Coiciding <- ifelse(IPC_RCAs_Top4$Total_RCA_2 ==3,1,0)
IPC_RCAs_Top4$justGeneral <- ifelse(IPC_RCAs_Top4$Total_RCA_2 ==1,1,0)
IPC_RCAs_Top4$OnlyAI <- ifelse(IPC_RCAs_Top4$Total_RCA_2 ==2,1,0)

#now, create a file per country per period, where I sum over the 3 columns;
#SummaryAllData<-distinct(SummaryData, Company, .keep_all = TRUE)
IPC_RCAs <- IPC_RCAs_Top4
IPC_RCAs %<>% 
  group_by(ctry_code,Period) %>% 
  mutate(Share_coinciding = sum(Coiciding)/(sum(Coiciding)+sum(justGeneral))) %>% 
  mutate(Share_OnlyAI = sum(OnlyAI)/(sum(OnlyAI)+sum(Coiciding))) %>% 
  mutate(sum_coinciding = sum(Coiciding)) %>% 
  mutate(sum_justGeneral = sum(justGeneral)) %>%
  mutate(sum_OnlyAI = sum(OnlyAI)) %>%
  ungroup()

SummaryAllData<-distinct(IPC_RCAs, ctry_code, Period, .keep_all = TRUE) 

OverlapTechn<-
ggplot(data=SummaryAllData, aes(x=Period, y=Share_coinciding, group=ctry_code, shape = ctry_code, color=ctry_code)) +
  geom_point(aes(fill = ctry_code), size=8) + 
  scale_shape_manual(values=c(21, 22, 23, 24)) +
  xlab("Period") +
  ylab("Share of coinciding specializations (%)") +
  ggtitle("Overlapping capabilities for technological fields") +
  theme_classic() +
  geom_line(aes(color=ctry_code), linetype = "dashed", size=1.5)+
  #geom_step(aes(color=ctry_code), linetype = "dashed")+
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("#99CC00", "#66CC33", "#336600", "#66FF66")) +
  scale_color_manual(values = c("#99CC00", "#66CC33", "#336600", "#66FF66")) 

OverlapAI<-
ggplot(data=SummaryAllData, aes(x=Period, y=Share_OnlyAI, group=ctry_code, shape = ctry_code, color=ctry_code)) +
  geom_point(aes(fill = ctry_code),size=8) + 
  scale_shape_manual(values=c(21, 22, 23, 24)) +
  xlab("Period") +
  ylab("Share of non-coinciding specializations (%)") +
  ggtitle("Non-overlapping capabilities for technological fields") +
  theme_classic() +
  geom_line(aes(color=ctry_code), linetype = "dashed", size=1.5)+ 
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(values = c("#0066CC", "#006699", "#003366", "#3399FF")) +
  scale_color_manual(values = c("#0066CC", "#006699", "#003366", "#3399FF")) 

jpeg("Files_created_with_the_code/figures/new_figures/Fig6_OverlapTechn.jpg", width = 8, height = 6, units = 'in', res = 300)
OverlapTechn 
dev.off()

jpeg("Files_created_with_the_code/figures/new_figures/Fig6_OverlapAI.jpg", width = 8, height = 6, units = 'in', res = 300)
OverlapAI 
dev.off()


#plus, convert to %
#improve colours;

#6.2.4-digits ----
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#IPC_RCAs_Top4 <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/RCA_4countries_detailed.csv", sep = ";", header = TRUE, dec=",")
IPC_RCAs <- read.csv("Files_created_with_the_code/data/files_code_4-digits_analysis/IPC_RCAs.csv", sep = ";", header = TRUE, dec=",")

#Select the 4 countries we want
IPC_RCAs_Top4 <- IPC_RCAs[IPC_RCAs$ctry_code == "CN" | 
                            IPC_RCAs$ctry_code == "KR"| 
                            IPC_RCAs$ctry_code == "US"| 
                            IPC_RCAs$ctry_code == "JP", ]
rm(IPC_RCAs)

#replace names:
IPC_RCAs_Top4$ctry_code <- gsub("US", "USA", str_trim(IPC_RCAs_Top4$ctry_code))
IPC_RCAs_Top4$ctry_code <- gsub("CN", "China", str_trim(IPC_RCAs_Top4$ctry_code))
IPC_RCAs_Top4$ctry_code <- gsub("JP", "Japan", str_trim(IPC_RCAs_Top4$ctry_code))
IPC_RCAs_Top4$ctry_code <- gsub("KR", "South Korea", str_trim(IPC_RCAs_Top4$ctry_code))

IPC_RCAs_Top4$Period_sim <- as.numeric(factor(IPC_RCAs_Top4$Period,levels=unique(IPC_RCAs_Top4$Period)))

#replace NAs by 0:
#replace NAs, so we don't have problems when summing:
IPC_RCAs_Top4[is.na(IPC_RCAs_Top4)] <- 0

#make the numbers binary
IPC_RCAs_Top4$RCA_Gen2 <- ifelse(IPC_RCAs_Top4$RCA_Gen >=1,1,0)
IPC_RCAs_Top4$RCA_AI2 <- ifelse(IPC_RCAs_Top4$RCA_AI >=1,1,0)

#fix Total_RCA:
#so, if Total_RCA_2 = 0, no specialization of the country at all, Total_RCA_2 = 1, general, Total_RCA_2 = 2, ONLY AI;
#Total_RCA_2 = 3, BOTH AI AND GENERAL
IPC_RCAs_Top4$Total_RCA_2 <- IPC_RCAs_Top4$RCA_Gen2 + 2*IPC_RCAs_Top4$RCA_AI2

IPC_RCAs_Top4$Coiciding <- ifelse(IPC_RCAs_Top4$Total_RCA_2 ==3,1,0)
IPC_RCAs_Top4$justGeneral <- ifelse(IPC_RCAs_Top4$Total_RCA_2 ==1,1,0)
IPC_RCAs_Top4$OnlyAI <- ifelse(IPC_RCAs_Top4$Total_RCA_2 ==2,1,0)

#now, create a file per country per period, where I sum over the 3 columns;
#SummaryAllData<-distinct(SummaryData, Company, .keep_all = TRUE)
IPC_RCAs <- IPC_RCAs_Top4
IPC_RCAs %<>% 
  group_by(ctry_code,Period) %>% 
  mutate(Share_coinciding = sum(Coiciding)/(sum(Coiciding)+sum(justGeneral))) %>% 
  mutate(Share_OnlyAI = sum(OnlyAI)/(sum(OnlyAI)+sum(Coiciding))) %>% 
  mutate(sum_coinciding = sum(Coiciding)) %>% 
  mutate(sum_justGeneral = sum(justGeneral)) %>%
  mutate(sum_OnlyAI = sum(OnlyAI)) %>%
  ungroup()

SummaryAllData4dig<-distinct(IPC_RCAs, ctry_code, Period, .keep_all = TRUE) 

OverlapTechn<-
  ggplot(data=SummaryAllData4dig, aes(x=Period, y=Share_coinciding, group=ctry_code, shape = ctry_code, color=ctry_code)) +
  geom_point(aes(fill = ctry_code), size=8) + 
  scale_shape_manual(values=c(21, 22, 23, 24)) +
  xlab("Period") +
  ylab("Share of coinciding specializations (%)") +
  ggtitle("Overlapping capabilities for 4-digits IPC codes") +
  theme_classic() +
  geom_line(aes(color=ctry_code), linetype = "dashed", size=1.5)+
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("#99CC00", "#66CC33", "#336600", "#66FF66")) +
  scale_color_manual(values = c("#99CC00", "#66CC33", "#336600", "#66FF66")) 

OverlapAI<-
  ggplot(data=SummaryAllData4dig, aes(x=Period, y=Share_OnlyAI, group=ctry_code, shape = ctry_code, color=ctry_code)) +
  geom_point(aes(fill = ctry_code),size=8) + 
  scale_shape_manual(values=c(21, 22, 23, 24)) +
  xlab("Period") +
  ylab("Share of non-coinciding specializations (%)") +
  ggtitle("Non-overlapping capabilities for 4-digits IPC codes") +
  theme_classic() +
  geom_line(aes(color=ctry_code), linetype = "dashed", size=1.5)+ 
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(values = c("#0066CC", "#006699", "#003366", "#3399FF")) +
  scale_color_manual(values = c("#0066CC", "#006699", "#003366", "#3399FF")) 

jpeg("Files_created_with_the_code/figures/new_figures/Fig7_OverlapTechn.jpg", width = 8, height = 6, units = 'in', res = 300)
OverlapTechn 
dev.off()

jpeg("Files_created_with_the_code/figures/new_figures/Fig7_OverlapAI.jpg", width = 8, height = 6, units = 'in', res = 300)
OverlapAI 
dev.off()
