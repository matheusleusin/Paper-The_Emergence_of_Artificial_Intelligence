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
setwd("large_files")

#Now we will load a first big file containing all priorities and their related IPC codes published in or after 2004. This file has
#58,841,893 lines. I will read it in 3 parts:
c <- 58841893-40000000
IPC_all_patents_Part1 <- fread("All_patents_and_IPCs_Part1.csv", header = F, nrow = 20000000)
IPC_all_patents_Part2 <- fread("All_patents_and_IPCs_Part1.csv", header = F, nrow = 20000000, skip = 20000000)
IPC_all_patents_Part3 <- fread("All_patents_and_IPCs_Part1.csv", header = F, nrow = c, skip = 40000000)

names(IPC_all_patents_Part1) <- c("appln_id", "ctry_code", "techn_field_nr", "weight", "priority_year")
names(IPC_all_patents_Part2) <- c("appln_id", "ctry_code", "techn_field_nr", "weight", "priority_year")
names(IPC_all_patents_Part3) <- c("appln_id", "ctry_code", "techn_field_nr", "weight", "priority_year")

#set the working directory to the folder where we opened this code:
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

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

mat_tech_AI1 <- create_sparse_matrix(i = IPC_all_patents_Part1 %>% pull(appln_id),
                                     j = IPC_all_patents_Part1 %>% pull(techn_field_nr))

mat_tech_AI1 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_AI2 <- create_sparse_matrix(i = IPC_all_patents_Part2 %>% pull(appln_id),
                                     j = IPC_all_patents_Part2 %>% pull(techn_field_nr))

mat_tech_AI2 %<>% 
  crossprod() %>% 
  as.matrix()

mat_tech_AI3 <- create_sparse_matrix(i = IPC_all_patents_Part3 %>% pull(appln_id),
                                     j = IPC_all_patents_Part3 %>% pull(techn_field_nr))

mat_tech_AI3 %<>% 
  crossprod() %>% 
  as.matrix()

#now we create a function to put these 3 matriCes together (by sum):
add_matrices_3 <- function(matrix1, matrix2, matrix3) {
  a <- list(matrix1, matrix2, matrix3)
  cols <- sort(unique(unlist(lapply(a, colnames))))
  rows <- sort(unique(unlist(lapply(a, rownames))))
  out <- array(0, dim = c(length(rows), length(cols)), dimnames = list(rows,cols))
  for (m in a) out[rownames(m), colnames(m)] <- out[rownames(m), colnames(m)] + m
  out
}

#and the newly created function:
mat_tech_AI_Final1 <- add_matrices_3(mat_tech_AI1, mat_tech_AI2, mat_tech_AI3)

#now, we drop the big files and load the remaining ones;
rm(IPC_all_patents_Part1)
rm(IPC_all_patents_Part2)
rm(IPC_all_patents_Part3)

#We will load the second big file containing all priorities and their related IPC codes published in or before 2003. This file has
#45,182,803 lines;
setwd("large_files")
c <- 45182803 -40000000
IPC_all_patents_Part1 <- fread("All_patents_and_IPCs_Part2.csv", header = F, nrow = 20000000)
IPC_all_patents_Part2 <- fread("All_patents_and_IPCs_Part2.csv", header = F, nrow = 20000000, skip = 20000000)
IPC_all_patents_Part3 <- fread("All_patents_and_IPCs_Part2.csv", header = F, nrow = c, skip = 40000000)

names(IPC_all_patents_Part1) <- c("appln_id", "ctry_code", "techn_field_nr", "weight", "priority_year")
names(IPC_all_patents_Part2) <- c("appln_id", "ctry_code", "techn_field_nr", "weight", "priority_year")
names(IPC_all_patents_Part3) <- c("appln_id", "ctry_code", "techn_field_nr", "weight", "priority_year")

#set the working directory to the folder where we opened this code:
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#we do all again as before:
mat_tech_AI1 <- create_sparse_matrix(i = IPC_all_patents_Part1 %>% pull(appln_id),
                                     j = IPC_all_patents_Part1 %>% pull(techn_field_nr))

mat_tech_AI1 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_AI2 <- create_sparse_matrix(i = IPC_all_patents_Part2 %>% pull(appln_id),
                                     j = IPC_all_patents_Part2 %>% pull(techn_field_nr))

mat_tech_AI2 %<>% 
  crossprod() %>% 
  as.matrix()

mat_tech_AI3 <- create_sparse_matrix(i = IPC_all_patents_Part3 %>% pull(appln_id),
                                     j = IPC_all_patents_Part3 %>% pull(techn_field_nr))

mat_tech_AI3 %<>% 
  crossprod() %>% 
  as.matrix()

mat_tech_AI_Final2 <- add_matrices_3(mat_tech_AI1, mat_tech_AI2, mat_tech_AI3)

#now we create a function similar to the previous, but for summing 2 matrices:
add_matrices_2 <- function(matrix1, matrix2) {
  a <- list(matrix1, matrix2)
  cols <- sort(unique(unlist(lapply(a, colnames))))
  rows <- sort(unique(unlist(lapply(a, rownames))))
  out <- array(0, dim = c(length(rows), length(cols)), dimnames = list(rows,cols))
  for (m in a) out[rownames(m), colnames(m)] <- out[rownames(m), colnames(m)] + m
  out
}

#and summ both matrices (the one from the first big file with the one from the second big file)
mat_tech_AI_Final <- add_matrices_2(mat_tech_AI_Final1, mat_tech_AI_Final2)

#Finally, we save the file. We will use it in the section 1.3.
write.csv2(mat_tech_AI_Final, file = "Files_created_with_the_code/data/files_code_Fields_analysis/Matrix_IPC.csv", row.names = TRUE)

#1.2.Technology Space ----
#Now we will create the technology spaces, dividing them in 3 periods;
setwd("large_files")
rm(list=ls())
#First, we create the 2 functions we will use for every period:
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

#1.2.1.First Period ----
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

#let's drop the columns we won't use (weight and priority_year):
IPC_all_patents_Part1 <- IPC_all_patents_Part1[, c((-4), (-5))]
IPC_all_patents_Part2 <- IPC_all_patents_Part2[, c((-4), (-5))]
IPC_all_patents_Part3 <- IPC_all_patents_Part3[, c((-4), (-5))]

#we combine the 3 files:
IPC_all_patents_FirstPeriod <- rbind(IPC_all_patents_Part1, IPC_all_patents_Part2, IPC_all_patents_Part3)
#and exclude the 3 big ones we just used, so we have back our memory:
rm(IPC_all_patents_Part1, IPC_all_patents_Part2, IPC_all_patents_Part3)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#For all countries:
#now we apply the 2 functions we created at the beginning of this section:
reg_tech1 <- group_by_applnID(IPC_all_patents_FirstPeriod)
reg_tech1 <- group_by_ctry_and_IPC(reg_tech1)

#and save the final file, so we can use it again in section 1.3. (around the line 330)
write.csv2(reg_tech1, file = "Files_created_with_the_code/data/files_code_Fields_analysis/reg_tech_FirstPeriod.csv", row.names = F)

#For AI:
patents_AI_specific <- read.csv("other_files/IPCs_AI.csv", sep = ";", header = TRUE, dec=",")
patents_AI_specific$ctry_code <- as.vector(patents_AI_specific$ctry_code)
patents_AI_specific$ctry_code <- "AI_pat"

#now we replace the AI data on the IPC dataset;
setDT(patents_AI_specific)
setDT(IPC_all_patents_FirstPeriod)
IPC_all_patents_FirstPeriod[patents_AI_specific, on = c("appln_id"), ctry_code := i.ctry_code]

reg_tech_AI1 <- group_by_applnID(IPC_all_patents_FirstPeriod)
rm(IPC_all_patents_FirstPeriod)
reg_tech_AI1 <- group_by_ctry_and_IPC(reg_tech_AI1)

#and save the final file, so we can use it again in section 1.3. (around the line 330)
write.csv2(reg_tech_AI1, file = "Files_created_with_the_code/data/files_code_Fields_analysis/reg_techAI_FirstPeriod.csv", row.names = F)

#1.2.2.Second Period ----
#For the second period, which goes from 1989 to 2003, we again need only the dataset from Part2:
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

IPC_all_patents_SecondPeriod <- rbind(IPC_all_patents_Part1, IPC_all_patents_Part2, IPC_all_patents_Part3)
rm(IPC_all_patents_Part1, IPC_all_patents_Part2, IPC_all_patents_Part3)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#For all countries
reg_tech2 <- group_by_applnID(IPC_all_patents_SecondPeriod)
reg_tech2 <- group_by_ctry_and_IPC(reg_tech2)
write.csv2(reg_tech2, file = "Files_created_with_the_code/data/files_code_Fields_analysis/reg_tech_SecondPeriod.csv", row.names = F)

#For AI:
#we replace the AI data on the IPC dataset;
setDT(patents_AI_specific)
setDT(IPC_all_patents_SecondPeriod)
IPC_all_patents_SecondPeriod[patents_AI_specific, on = c("appln_id"), ctry_code := i.ctry_code]

reg_tech_AI2 <- group_by_applnID(IPC_all_patents_SecondPeriod)
rm(IPC_all_patents_SecondPeriod)
reg_tech_AI2 <- group_by_ctry_and_IPC(reg_tech_AI2)

#and save the final file, so we can use it again in section 1.3. (around the line 330)
write.csv2(reg_tech_AI2, file = "Files_created_with_the_code/data/files_code_Fields_analysis/reg_techAI_SecondPeriod.csv", row.names = F)

#1.2.3.Third Period ----
#For the third period, which goes from 2004 to 2018, we  need only the dataset from Part1. This
#specific dataset only has patents from 2004 to 2018, so we don't have to filter it. But calculating
#the reg_tech is very computationally expansive, so we have to divide that in 3 parts.
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

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
write.csv2(tabledata2, file = "Files_created_with_the_code/data/files_code_Fields_analysis/reg_tech_ThirdPeriod.csv", row.names = F)
rm(tabledata2)
#For AI:
setDT(patents_AI_specific)
setDT(IPC_all_patents_Part1)
setDT(IPC_all_patents_Part2)
setDT(IPC_all_patents_Part3)
IPC_all_patents_Part1[patents_AI_specific, on = c("appln_id"), ctry_code := i.ctry_code]
IPC_all_patents_Part2[patents_AI_specific, on = c("appln_id"), ctry_code := i.ctry_code]
IPC_all_patents_Part3[patents_AI_specific, on = c("appln_id"), ctry_code := i.ctry_code]

reg_tech_AI4 <- group_by_applnID(IPC_all_patents_Part1)
rm(IPC_all_patents_Part1)
reg_tech_AI4 <- group_by_ctry_and_IPC(reg_tech_AI4)

reg_tech_AI5 <- group_by_applnID(IPC_all_patents_Part2)
rm(IPC_all_patents_Part2)
reg_tech_AI5 <- group_by_ctry_and_IPC(reg_tech_AI5)

reg_tech_AI6 <- group_by_applnID(IPC_all_patents_Part3)
rm(IPC_all_patents_Part3)
reg_tech_AI6 <- group_by_ctry_and_IPC(reg_tech_AI6)

#now we merge them
tabledata_AI2 <- merge(reg_tech_AI4, reg_tech_AI5, all=T, by=c("ctry_code", "techn_field_nr"))
tabledata_AI2 <- merge(tabledata_AI2, reg_tech_AI6, all=T, by=c("ctry_code", "techn_field_nr"))

#remove the big files
rm(reg_tech_AI4, reg_tech_AI5, reg_tech_AI6)

#replace NAs, so we don't have problems when summing:
tabledata_AI2[is.na(tabledata_AI2)] <- 0

#do the summ, exclude the tables used, and rename the dataset accordingly:
tabledata_AI2$sum <- rowSums(tabledata_AI2[,c(3:5)])
tabledata_AI2 <- tabledata_AI2[, c((-3), (-4), (-5))]
names(tabledata_AI2) <- c("ctry_code", "techn_field_nr", "n_tech_reg")

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
write.csv2(tabledata_AI2, file = "Files_created_with_the_code/data/files_code_Fields_analysis/reg_techAI_ThirdPeriod.csv", row.names = F)

#1.3.Calculate the g_tech_AI ----
#1.3.1. Create a specialisations summary ----
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
reg_tech1_countries <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/reg_tech_FirstPeriod.csv", sep = ";", header = TRUE, dec=",")
#1.3.1.1. First Period Countries----
mat_reg_tech1_countries <- reg_tech1_countries %>%
  arrange(techn_field_nr, ctry_code) %>%
  pivot_wider(names_from = techn_field_nr, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

mat_reg_tech1_countries %<>% remove_rownames %>% column_to_rownames(var="ctry_code") %>%
  as.matrix() %>%
  round()

reg_RCA1_countries <- mat_reg_tech1_countries %>% location.quotient(binary = F) %>% 
  as.data.frame() %>% 
  rownames_to_column("ctry_code") %>% 
  as_tibble() %>% 
  gather(key = "techn_field_nr", value = "RCA", -ctry_code) %>%
  arrange(ctry_code, techn_field_nr)

###1.3.1.2. First Period AI----
reg_tech1_AI <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/reg_techAI_FirstPeriod.csv", sep = ";", header = TRUE, dec=",")

mat_reg_tech1_AI <- reg_tech1_AI %>%
  arrange(techn_field_nr, ctry_code) %>%
  pivot_wider(names_from = techn_field_nr, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

mat_reg_tech1_AI %<>% remove_rownames %>% column_to_rownames(var="ctry_code") %>%
  as.matrix() %>%
  round()

reg_RCA1_AI <- mat_reg_tech1_AI %>% location.quotient(binary = F) %>% 
  as.data.frame() %>% 
  rownames_to_column("ctry_code") %>% 
  as_tibble() %>% 
  gather(key = "techn_field_nr", value = "RCA", -ctry_code) %>%
  arrange(ctry_code, techn_field_nr)

IPC_names <- read.csv("other_files/ipc_technology.csv", sep = ";", header = TRUE)%>%
  select(field_nr, sector, field_name) %>%
  distinct(field_nr, .keep_all = TRUE) %>%
  mutate(techn_field_nr = field_nr) %>%
  arrange(techn_field_nr)
IPC_names <- IPC_names[,(-1)]
#select countries
US_first_period <- reg_RCA1_countries[,2:3][reg_RCA1_countries$ctry_code == "US",]
CN_first_period <- reg_RCA1_countries[,2:3][reg_RCA1_countries$ctry_code == "CN",]
KR_first_period <- reg_RCA1_countries[,2:3][reg_RCA1_countries$ctry_code == "KR",]
JP_first_period <- reg_RCA1_countries[,2:3][reg_RCA1_countries$ctry_code == "JP",]
AI_first_period <- reg_RCA1_AI[,2:3][reg_RCA1_AI$ctry_code == "AI_pat",]

First_period <- merge(merge(merge(merge(merge(
  IPC_names,US_first_period), CN_first_period, by = "techn_field_nr"), KR_first_period, by = "techn_field_nr"), 
  JP_first_period, by = "techn_field_nr"), AI_first_period, by = "techn_field_nr")
names(First_period) = c("techn_field_nr", "sector", "field_name", "RCA_US", "RCA_CN","RCA_KR","RCA_JP", "RCA_AI")
write.csv2(First_period, file = "Files_created_with_the_code/data/files_code_Fields_analysis/Metrics_First_period_complexity.csv", row.names = F)

#1.3.1.3. Second period countries ----
reg_tech2_countries <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/reg_tech_SecondPeriod.csv", sep = ";", header = TRUE, dec=",")
mat_reg_tech2_countries <- reg_tech2_countries %>%
  arrange(techn_field_nr, ctry_code) %>%
  pivot_wider(names_from = techn_field_nr, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

mat_reg_tech2_countries %<>% remove_rownames %>% column_to_rownames(var="ctry_code") %>%
  as.matrix() %>%
  round()

reg_RCA2_countries <- mat_reg_tech2_countries %>% location.quotient(binary = F) %>% 
  as.data.frame() %>% 
  rownames_to_column("ctry_code") %>% 
  as_tibble() %>% 
  gather(key = "techn_field_nr", value = "RCA", -ctry_code) %>%
  arrange(ctry_code, techn_field_nr)

###1.3.1.4. Second Period AI ----
reg_tech2_AI <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/reg_techAI_SecondPeriod.csv", sep = ";", header = TRUE, dec=",")

mat_reg_tech2_AI <- reg_tech2_AI %>%
  arrange(techn_field_nr, ctry_code) %>%
  pivot_wider(names_from = techn_field_nr, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

mat_reg_tech2_AI %<>% remove_rownames %>% column_to_rownames(var="ctry_code") %>%
  as.matrix() %>%
  round()

reg_RCA2_AI <- mat_reg_tech2_AI %>% location.quotient(binary = F) %>% 
  as.data.frame() %>% 
  rownames_to_column("ctry_code") %>% 
  as_tibble() %>% 
  gather(key = "techn_field_nr", value = "RCA", -ctry_code) %>%
  arrange(ctry_code, techn_field_nr)

#select countries
US_Second_period <- reg_RCA2_countries[,2:3][reg_RCA2_countries$ctry_code == "US",]
CN_Second_period <- reg_RCA2_countries[,2:3][reg_RCA2_countries$ctry_code == "CN",]
KR_Second_period <- reg_RCA2_countries[,2:3][reg_RCA2_countries$ctry_code == "KR",]
JP_Second_period <- reg_RCA2_countries[,2:3][reg_RCA2_countries$ctry_code == "JP",]
AI_Second_period <- reg_RCA2_AI[,2:3][reg_RCA2_AI$ctry_code == "AI_pat",]

Second_period <- merge(merge(merge(merge(merge(
  IPC_names,US_Second_period), CN_Second_period, by = "techn_field_nr"), KR_Second_period, by = "techn_field_nr"), 
  JP_Second_period, by = "techn_field_nr"), AI_Second_period, by = "techn_field_nr")
names(Second_period) = c("techn_field_nr", "sector", "field_name", "RCA_US", "RCA_CN","RCA_KR","RCA_JP", "RCA_AI")
write.csv2(Second_period, file = "Files_created_with_the_code/data/files_code_Fields_analysis/Metrics_Second_period_complexity.csv", row.names = F)

#1.3.1.5.Third period countries -----
reg_tech3_countries <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/reg_tech_ThirdPeriod.csv", sep = ";", header = TRUE, dec=",")
#4.3.1. Third Period Countries
mat_reg_tech3_countries <- reg_tech3_countries %>%
  arrange(techn_field_nr, ctry_code) %>%
  pivot_wider(names_from = techn_field_nr, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

mat_reg_tech3_countries %<>% remove_rownames %>% column_to_rownames(var="ctry_code") %>%
  as.matrix() %>%
  round()

reg_RCA3_countries <- mat_reg_tech3_countries %>% location.quotient(binary = F) %>% 
  as.data.frame() %>% 
  rownames_to_column("ctry_code") %>% 
  as_tibble() %>% 
  gather(key = "techn_field_nr", value = "RCA", -ctry_code) %>%
  arrange(ctry_code, techn_field_nr)

#1.3.1.6.Third Period AI-----
reg_tech3_AI <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/reg_techAI_ThirdPeriod.csv", sep = ";", header = TRUE, dec=",")

mat_reg_tech3_AI <- reg_tech3_AI %>%
  arrange(techn_field_nr, ctry_code) %>%
  pivot_wider(names_from = techn_field_nr, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

mat_reg_tech3_AI %<>% remove_rownames %>% column_to_rownames(var="ctry_code") %>%
  as.matrix() %>%
  round()

reg_RCA3_AI <- mat_reg_tech3_AI %>% location.quotient(binary = F) %>% 
  as.data.frame() %>% 
  rownames_to_column("ctry_code") %>% 
  as_tibble() %>% 
  gather(key = "techn_field_nr", value = "RCA", -ctry_code) %>%
  arrange(ctry_code, techn_field_nr)

#select countries
US_Third_period <- reg_RCA3_countries[,2:3][reg_RCA3_countries$ctry_code == "US",]
CN_Third_period <- reg_RCA3_countries[,2:3][reg_RCA3_countries$ctry_code == "CN",]
KR_Third_period <- reg_RCA3_countries[,2:3][reg_RCA3_countries$ctry_code == "KR",]
JP_Third_period <- reg_RCA3_countries[,2:3][reg_RCA3_countries$ctry_code == "JP",]
AI_Third_period <- reg_RCA3_AI[,2:3][reg_RCA3_AI$ctry_code == "AI_pat",]

Third_period <- merge(merge(merge(merge(merge(
  IPC_names,US_Third_period), CN_Third_period, by = "techn_field_nr"), KR_Third_period, by = "techn_field_nr"), 
  JP_Third_period, by = "techn_field_nr"), AI_Third_period, by = "techn_field_nr")
names(Third_period) = c("techn_field_nr", "sector", "field_name", "RCA_US", "RCA_CN","RCA_KR","RCA_JP", "RCA_AI")
write.csv2(Third_period, file = "Files_created_with_the_code/data/files_code_Fields_analysis/Metrics_Third_period_complexity.csv", row.names = F)

#put it all together
First_period <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/Metrics_First_period_complexity.csv", sep = ";", header = TRUE, dec=",")
Second_period <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/Metrics_Second_period_complexity.csv", sep = ";", header = TRUE, dec=",")
Third_period <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/Metrics_Third_period_complexity.csv", sep = ";", header = TRUE, dec=",")

First_period$Period <- "Period 1 (1974-1988)"
Second_period$Period <- "Period 2 (1989-2003)"
Third_period$Period <- "Period 3 (2004-2018)"

All_periods <- rbind(First_period, Second_period, Third_period)

All_periods$Category <- "Other"
Surr <- c(1, 2, 3, 13, 25, 34)
All_periods$Category[(All_periods$techn_field_nr %in% Surr)] <- "Surrounding fields"
AIrel<- c(11, 5, 4)
All_periods$Category[(All_periods$techn_field_nr %in% AIrel)] <- "AI-related fields"
AIcor<- c(6,7,10,12)
All_periods$Category[(All_periods$techn_field_nr %in% AIcor)] <- "AI-core fields"
All_periods$Category <- factor(All_periods$Category, levels = c("AI-core fields", "AI-related fields", "Surrounding fields", "Other"))
All_periods$Category2 <- All_periods$Category
All_periods$Category2 <- as.numeric(All_periods$Category2)
All_periods$Category2 <- as.numeric(All_periods$Category2)

write.csv2(All_periods, file = "Files_created_with_the_code/data/files_code_Fields_analysis/Specializations_All_periods_IPC.csv", row.names = TRUE)

#1.3.2. Create the technological space ----
#Now we load all the files we already saved. We start by loading the janitor library, which is used here for converting
#the first column of data to row names.
library(janitor)
rm(list=ls())
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

IPC_names$Category <- factor(IPC_names$Category, levels = c("AI-core fields", "AI-related fields",
                                                            "Surrounding fields", "Other"))

g_tech_AI <- mat_tech_rel_AI %>% as_tbl_graph(directed = FALSE) %N>%
  left_join(IPC_names %>% mutate(techn_field_nr = techn_field_nr %>% as.character()), by = c("name" = "techn_field_nr")) %>%
  mutate(dgr = centrality_eigen(weights = weight)) %E>%
  filter(weight >= mean(weight))

#1.3.3. Create the Coordinates -----
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

#1.3.1.Calculate reg_RCAs for countries ----
#Now we read the data per period and calculate the RCAs:
reg_tech1 <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/reg_tech_FirstPeriod.csv", sep = ";", header = TRUE, dec=",")
reg_tech2 <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/reg_tech_SecondPeriod.csv", sep = ";", header = TRUE, dec=",")
reg_tech3 <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/reg_tech_ThirdPeriod.csv", sep = ";", header = TRUE, dec=",")

###First Period:
mat_reg_tech1 <- reg_tech1 %>%
  arrange(techn_field_nr, ctry_code) %>%
  pivot_wider(names_from = techn_field_nr, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

mat_reg_tech1 %<>% remove_rownames %>% column_to_rownames(var="ctry_code") %>%
  as.matrix() %>%
  round()

reg_RCA1 <- mat_reg_tech1 %>% location.quotient(binary = TRUE) %>% 
  as.data.frame() %>% 
  rownames_to_column("ctry_code") %>% 
  as_tibble() %>% 
  gather(key = "techn_field_nr", value = "RCA", -ctry_code) %>%
  arrange(ctry_code, techn_field_nr)

###second period:
mat_reg_tech2 <- reg_tech2 %>%
  arrange(techn_field_nr, ctry_code) %>%
  pivot_wider(names_from = techn_field_nr, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

mat_reg_tech2 %<>% remove_rownames %>% column_to_rownames(var="ctry_code") %>%
  as.matrix() %>%
  round()

reg_RCA2 <- mat_reg_tech2 %>% location.quotient(binary = TRUE) %>% 
  as.data.frame() %>% 
  rownames_to_column("ctry_code") %>% 
  as_tibble() %>% 
  gather(key = "techn_field_nr", value = "RCA", -ctry_code) %>%
  arrange(ctry_code, techn_field_nr)

###Third Period:
mat_reg_tech3 <- reg_tech3 %>%
  arrange(techn_field_nr, ctry_code) %>%
  pivot_wider(names_from = techn_field_nr, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

mat_reg_tech3 %<>% remove_rownames %>% column_to_rownames(var="ctry_code") %>%
  as.matrix() %>%
  round()

reg_RCA3 <- mat_reg_tech3 %>% location.quotient(binary = TRUE) %>% 
  as.data.frame() %>% 
  rownames_to_column("ctry_code") %>% 
  as_tibble() %>% 
  gather(key = "techn_field_nr", value = "RCA", -ctry_code) %>%
  arrange(ctry_code, techn_field_nr)

#save the specialisations of countries
reg_top4countries <- merge(reg_RCA1, reg_RCA2, by = c("ctry_code", "techn_field_nr"))
reg_top4countries2 <- merge(reg_top4countries, reg_RCA3, by = c("ctry_code", "techn_field_nr"))
reg_top4countries3 <- reg_top4countries2[reg_top4countries2$ctry_code == "CN"|
                                           reg_top4countries2$ctry_code == "US"|
                                           reg_top4countries2$ctry_code == "JP"|
                                           reg_top4countries2$ctry_code == "KR",]
library(openxlsx)
write.xlsx(reg_top4countries3, file = "Files_created_with_the_code/data/files_code_Fields_analysis/Specialisations_4countries_3p.xlsx", row.names = F)
rm(reg_top4countries,reg_top4countries2,reg_top4countries3)

#1.3.3.Calculate reg_RCAs for AI ----
#Now we read the data per period and calculate the RCAs:
reg_tech1 <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/reg_techAI_FirstPeriod.csv", sep = ";", header = TRUE, dec=",")
reg_tech2 <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/reg_techAI_SecondPeriod.csv", sep = ";", header = TRUE, dec=",")
reg_tech3 <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/reg_techAI_ThirdPeriod.csv", sep = ";", header = TRUE, dec=",")

###First Period:
mat_reg_tech1 <- reg_tech1 %>%
  arrange(techn_field_nr, ctry_code) %>%
  pivot_wider(names_from = techn_field_nr, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

mat_reg_tech1 %<>% remove_rownames %>% column_to_rownames(var="ctry_code") %>%
  as.matrix() %>%
  round()

reg_RCA_AI1 <- mat_reg_tech1 %>% location.quotient(binary = TRUE) %>% 
  as.data.frame() %>% 
  rownames_to_column("ctry_code") %>% 
  as_tibble() %>% 
  gather(key = "techn_field_nr", value = "RCA", -ctry_code) %>%
  arrange(ctry_code, techn_field_nr)

###second period:
mat_reg_tech2 <- reg_tech2 %>%
  arrange(techn_field_nr, ctry_code) %>%
  pivot_wider(names_from = techn_field_nr, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

mat_reg_tech2 %<>% remove_rownames %>% column_to_rownames(var="ctry_code") %>%
  as.matrix() %>%
  round()

reg_RCA_AI2 <- mat_reg_tech2 %>% location.quotient(binary = TRUE) %>% 
  as.data.frame() %>% 
  rownames_to_column("ctry_code") %>% 
  as_tibble() %>% 
  gather(key = "techn_field_nr", value = "RCA", -ctry_code) %>%
  arrange(ctry_code, techn_field_nr)

###Third Period:
mat_reg_tech3 <- reg_tech3 %>%
  arrange(techn_field_nr, ctry_code) %>%
  pivot_wider(names_from = techn_field_nr, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

mat_reg_tech3 %<>% remove_rownames %>% column_to_rownames(var="ctry_code") %>%
  as.matrix() %>%
  round()

reg_RCA_AI3 <- mat_reg_tech3 %>% location.quotient(binary = TRUE) %>% 
  as.data.frame() %>% 
  rownames_to_column("ctry_code") %>% 
  as_tibble() %>% 
  gather(key = "techn_field_nr", value = "RCA", -ctry_code) %>%
  arrange(ctry_code, techn_field_nr)

#1.4. IPC Visualization General-----
#Finally, we start with the visualizations. For the Global perspective, considering the whole data, we have:

#the layout of the figure is random. I've already set one random layout, but you can keep
#running the two lines of code below until you find one you like (you can check if you like
#by looking at the next plot)
coords_tech_AI <- g_tech_AI %>% igraph::layout.fruchterman.reingold() %>% as_tibble()
colnames(coords_tech_AI) <- c("x", "y")

Figure1_Global_Technological_Space_colour <-
  g_tech_AI %>%
  ggraph(layout =  coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "grey") + 
  geom_node_point(aes(colour = sector, size = dgr, shape= sector))+ 
  scale_shape_manual(values=c(16, 16, 17, 18, 17)) + scale_size(range = c(2, 10)) +
  geom_node_text(aes(label = field_name), size = 4, repel = TRUE) + 
  theme_graph()+
  ggtitle("Technology Space: IPC codes") + 
  theme(
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 10)
  ) + guides(colour = guide_legend(override.aes = list(size=10))) 

#new addition:
coords_tech_AI <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/coords_tech_AI_layout1.csv", sep = ";", header = T, dec=",")
library(ggforce) #for using geom_mark_hull

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

#regular resolution: 
jpeg("Files_created_with_the_code/figures/regular_resolution_ALL_FIGURES_ARE_HERE/Fig1_Global_technological_space_colour.jpg", width = 14, height = 10, units = 'in', res = 300)
Figure1_Global_Technological_Space_colour 
dev.off()
  
#low resolution:  
jpeg("Files_created_with_the_code/figures/low_resolution/Fig1_Global_technological_space_colourLow.jpg", width = 14, height = 10, units = 'in', res = 72)
Figure1_Global_Technological_Space_colour 
dev.off()

#high resolution: 
jpeg("Files_created_with_the_code/figures/high_resolution/Fig1_Global_technological_space_colourHigh.jpg", width = 14, height = 10, units = 'in', res = 800)
Figure1_Global_Technological_Space_colour 
dev.off()

#1.4.1. IPC Visualization Per country-----
#Now we start the analysis per country:
#1st period
country_select <- c("CN", "US", "JP", "KR")
i = 1
IPC1Colour_FirstPeriod <- 
  g_tech_AI %N>%
  left_join(reg_RCA1 %>% filter(ctry_code == country_select[i]) %>% select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "grey") +
  geom_node_point(aes(colour = factor(RCA), size = Category, shape= Category)) +
  scale_shape_manual(values=c(15, 16, 17, 18)) + labs(color   = "RCA")+ 
    scale_size_manual(values=c(10, 10, 10, 4))+
  geom_node_text(aes(filter=RCA > .9, label = field_name), size = 5, repel = TRUE) +
  scale_color_manual(values=c("gray90", "green4"))+
  theme_graph() +
  ggtitle("IPC Technology Space: China (1974-1988)") 

i = 2
IPC2Colour_FirstPeriod <- 
  g_tech_AI %N>%
  left_join(reg_RCA1 %>% filter(ctry_code == country_select[i]) %>% select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "grey") +
  geom_node_point(aes(colour = factor(RCA), size = Category, shape= Category)) +
  scale_shape_manual(values=c(15, 16, 17, 18)) + labs(color   = "RCA")+ 
  scale_size_manual(values=c(10, 10, 10, 4))+
  geom_node_text(aes(filter=RCA > .9, label = field_name), size = 5, repel = TRUE) +
  scale_color_manual(values=c("gray90", "green4"))+
  theme_graph() +
  ggtitle("IPC Technology Space: USA (1974-1988)") 

i = 3
IPC3Colour_FirstPeriod <- 
  g_tech_AI %N>%
  left_join(reg_RCA1 %>% filter(ctry_code == country_select[i]) %>% select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "grey") +
  geom_node_point(aes(colour = factor(RCA), size = Category, shape= Category)) +
  scale_shape_manual(values=c(15, 16, 17, 18)) + labs(color   = "RCA")+ 
  scale_size_manual(values=c(10, 10, 10, 4))+
  geom_node_text(aes(filter=RCA > .9, label = field_name), size = 5, repel = TRUE) +
  scale_color_manual(values=c("gray90", "green4"))+
  theme_graph() +
  ggtitle("IPC Technology Space: Japan (1974-1988)") 

i = 4
IPC4Colour_FirstPeriod <- 
  g_tech_AI %N>%
  left_join(reg_RCA1 %>% filter(ctry_code == country_select[i]) %>% select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "grey") +
  geom_node_point(aes(colour = factor(RCA), size = Category, shape= Category)) +
  scale_shape_manual(values=c(15, 16, 17, 18)) + labs(color   = "RCA")+ 
  scale_size_manual(values=c(10, 10, 10, 4))+
  geom_node_text(aes(filter=RCA > .9, label = field_name), size = 5, repel = TRUE) +
  scale_color_manual(values=c("gray90", "green4"))+
  theme_graph() +
  ggtitle("IPC Technology Space: South Korea (1974-1988)") 

IPC1Label <- 
g_tech_AI %N>%
  left_join(reg_RCA1 %>% filter(ctry_code == country_select[i]) %>% select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "grey") +
  geom_node_point(aes(colour = factor(RCA), size = Category, shape= Category)) + 
  scale_shape_manual(values=c(15, 16, 17, 18)) + 
  labs(color   = "RCA")+ 
  scale_size_manual(values=c(15, 15, 15, 7))+
  geom_node_text(aes(filter=RCA > .9, label = field_name), size = 5, repel = TRUE) +
  scale_color_manual(values=c("gray90", "green4"))+
  #    scale_color_brewer(palette = "Paired")+
  theme_graph() +
  theme(text = element_text(size = 20)) +
  ggtitle("IPC Technology Space: China (1974-1988)") 

#THE LABEL:
jpeg("Files_created_with_the_code/figures/regular_resolution_ALL_FIGURES_ARE_HERE/Fig5_Label2.jpg", width = 14, height = 10, units = 'in', res = 300)
IPC1Label
dev.off()

#For saving the coloured pictures_Low resolution (72 pi):
jpeg("Files_created_with_the_code/figures/low_resolution/Fig5_colour_China_1stLow.jpg", width = 14, height = 10, units = 'in', res = 72)
IPC1Colour_FirstPeriod
dev.off()

jpeg("Files_created_with_the_code/figures/low_resolution/Fig5_colour_US_1stLow.jpg", width = 14, height = 10, units = 'in', res = 72)
IPC2Colour_FirstPeriod
dev.off()

jpeg("Files_created_with_the_code/figures/low_resolution/Fig5_colour_JP_1stLow.jpg", width = 14, height = 10, units = 'in', res = 72)
IPC3Colour_FirstPeriod
dev.off()

jpeg("Files_created_with_the_code/figures/low_resolution/Fig5_colour_KR_1stLow.jpg", width = 14, height = 10, units = 'in', res = 72)
IPC4Colour_FirstPeriod
dev.off()

#For saving the colored pictures_High resolution (800 pi):
jpeg("Files_created_with_the_code/figures/high_resolution/Fig5_colour_China_1stHigh.jpg", width = 14, height = 10, units = 'in', res = 800)
IPC1Colour_FirstPeriod
dev.off()

jpeg("Files_created_with_the_code/figures/high_resolution/Fig5_colour_US_1stHigh.jpg", width = 14, height = 10, units = 'in', res = 800)
IPC2Colour_FirstPeriod
dev.off()

jpeg("Files_created_with_the_code/figures/high_resolution/Fig5_colour_JP_1stHigh.jpg", width = 14, height = 10, units = 'in', res = 800)
IPC3Colour_FirstPeriod
dev.off()

jpeg("Files_created_with_the_code/figures/high_resolution/Fig5_colour_KR_1stHigh.jpg", width = 14, height = 10, units = 'in', res = 800)
IPC4Colour_FirstPeriod
dev.off()

#For saving the coloured picture in regular resolution (300 pi):
jpeg("Files_created_with_the_code/figures/regular_resolution_ALL_FIGURES_ARE_HERE/Fig5_colour_China_1st.jpg", width = 14, height = 10, units = 'in', res = 300)
IPC1Colour_FirstPeriod
dev.off()

jpeg("Files_created_with_the_code/figures/regular_resolution_ALL_FIGURES_ARE_HERE/Fig5_colour_US_1st.jpg", width = 14, height = 10, units = 'in', res = 300)
IPC2Colour_FirstPeriod
dev.off()

jpeg("Files_created_with_the_code/figures/regular_resolution_ALL_FIGURES_ARE_HERE/Fig5_colour_JP_1st.jpg", width = 14, height = 10, units = 'in', res = 300)
IPC3Colour_FirstPeriod
dev.off()

jpeg("Files_created_with_the_code/figures/regular_resolution_ALL_FIGURES_ARE_HERE/Fig5_colour_KR_1st.jpg", width = 14, height = 10, units = 'in', res = 300)
IPC4Colour_FirstPeriod
dev.off()

#Per Country 2nd period
i = 1
IPC1Colour_SecondPeriod <- 
  g_tech_AI %N>%
  left_join(reg_RCA2 %>% filter(ctry_code == country_select[i]) %>% select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "grey") +
  geom_node_point(aes(colour = factor(RCA), size = Category, shape= Category)) +
  scale_shape_manual(values=c(15, 16, 17, 18)) + labs(color   = "RCA")+ 
  scale_size_manual(values=c(10, 10, 10, 4))+
  geom_node_text(aes(filter=RCA > .9, label = field_name), size = 5, repel = TRUE) +
  scale_color_manual(values=c("gray90", "green4"))+
  theme_graph() +
  ggtitle("IPC Technology Space: China (1989-2003)") 

i = 2
IPC2Colour_SecondPeriod <- 
  g_tech_AI %N>%
  left_join(reg_RCA2 %>% filter(ctry_code == country_select[i]) %>% select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "grey") +
  geom_node_point(aes(colour = factor(RCA), size = Category, shape= Category)) +
  scale_shape_manual(values=c(15, 16, 17, 18)) + labs(color   = "RCA")+ 
  scale_size_manual(values=c(10, 10, 10, 4))+
  geom_node_text(aes(filter=RCA > .9, label = field_name), size = 5, repel = TRUE) +
  scale_color_manual(values=c("gray90", "green4"))+
  theme_graph() +
  ggtitle("IPC Technology Space: USA (1989-2003)") 

i = 3
IPC3Colour_SecondPeriod <- 
  g_tech_AI %N>%
  left_join(reg_RCA2 %>% filter(ctry_code == country_select[i]) %>% select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "grey") +
  geom_node_point(aes(colour = factor(RCA), size = Category, shape= Category)) +
  scale_shape_manual(values=c(15, 16, 17, 18)) + labs(color   = "RCA")+ 
  scale_size_manual(values=c(10, 10, 10, 4))+
  geom_node_text(aes(filter=RCA > .9, label = field_name), size = 5, repel = TRUE) +
  scale_color_manual(values=c("gray90", "green4"))+
  theme_graph() +
  ggtitle("IPC Technology Space: Japan (1989-2003)") 

i = 4
IPC4Colour_SecondPeriod <- 
  g_tech_AI %N>%
  left_join(reg_RCA2 %>% filter(ctry_code == country_select[i]) %>% select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "grey") +
  geom_node_point(aes(colour = factor(RCA), size = Category, shape= Category)) +
  scale_shape_manual(values=c(15, 16, 17, 18)) + labs(color   = "RCA")+ 
  scale_size_manual(values=c(10, 10, 10, 4))+
  geom_node_text(aes(filter=RCA > .9, label = field_name), size = 5, repel = TRUE) +
  scale_color_manual(values=c("gray90", "green4"))+
  theme_graph() +
  ggtitle("IPC Technology Space: South Korea (1989-2003)") 

#For saving the coloured pictures in high resol:
jpeg("Files_created_with_the_code/figures/high_resolution/Fig5_colour_China_2ndHigh.jpg", width = 14, height = 10, units = 'in', res = 800)
IPC1Colour_SecondPeriod
dev.off()

jpeg("Files_created_with_the_code/figures/high_resolution/Fig5_colour_US_2ndHigh.jpg", width = 14, height = 10, units = 'in', res = 800)
IPC2Colour_SecondPeriod
dev.off()

jpeg("Files_created_with_the_code/figures/high_resolution/Fig5_colour_JP_2ndHigh.jpg", width = 14, height = 10, units = 'in', res = 800)
IPC3Colour_SecondPeriod
dev.off()

jpeg("Files_created_with_the_code/figures/high_resolution/Fig5_colour_KR_2ndHigh.jpg", width = 14, height = 10, units = 'in', res = 800)
IPC4Colour_SecondPeriod
dev.off()

#For saving the coloured pictures in low resol:
jpeg("Files_created_with_the_code/figures/low_resolution/Fig5_colour_China_2ndLow.jpg", width = 14, height = 10, units = 'in', res = 72)
IPC1Colour_SecondPeriod
dev.off()

jpeg("Files_created_with_the_code/figures/low_resolution/Fig5_colour_US_2ndLow.jpg", width = 14, height = 10, units = 'in', res = 72)
IPC2Colour_SecondPeriod
dev.off()

jpeg("Files_created_with_the_code/figures/low_resolution/Fig5_colour_JP_2ndLow.jpg", width = 14, height = 10, units = 'in', res = 72)
IPC3Colour_SecondPeriod
dev.off()

jpeg("Files_created_with_the_code/figures/low_resolution/Fig5_colour_KR_2ndLow.jpg", width = 14, height = 10, units = 'in', res = 72)
IPC4Colour_SecondPeriod
dev.off()

#For saving the coloured pictures in regular resol:
jpeg("Files_created_with_the_code/figures/regular_resolution_ALL_FIGURES_ARE_HERE/Fig5_colour_China_2nd.jpg", width = 14, height = 10, units = 'in', res = 300)
IPC1Colour_SecondPeriod
dev.off()

jpeg("Files_created_with_the_code/figures/regular_resolution_ALL_FIGURES_ARE_HERE/Fig5_colour_US_2nd.jpg", width = 14, height = 10, units = 'in', res = 300)
IPC2Colour_SecondPeriod
dev.off()

jpeg("Files_created_with_the_code/figures/regular_resolution_ALL_FIGURES_ARE_HERE/Fig5_colour_JP_2nd.jpg", width = 14, height = 10, units = 'in', res = 300)
IPC3Colour_SecondPeriod
dev.off()

jpeg("Files_created_with_the_code/figures/regular_resolution_ALL_FIGURES_ARE_HERE/Fig5_colour_KR_2nd.jpg", width = 14, height = 10, units = 'in', res = 300)
IPC4Colour_SecondPeriod
dev.off()

#Per Country 3rd period
i = 1
IPC1Colour_ThirdPeriod <- 
  g_tech_AI %N>%
  left_join(reg_RCA3 %>% filter(ctry_code == country_select[i]) %>% select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "grey") +
  geom_node_point(aes(colour = factor(RCA), size = Category, shape= Category)) +
  scale_shape_manual(values=c(15, 16, 17, 18)) + labs(color   = "RCA")+ 
  scale_size_manual(values=c(10, 10, 10, 4))+
  geom_node_text(aes(filter=RCA > .9, label = field_name), size = 5, repel = TRUE) +
  scale_color_manual(values=c("gray90", "green4"))+
  theme_graph() +
  ggtitle("IPC Technology Space: China (2004-2018)") 

i = 2
IPC2Colour_ThirdPeriod <- 
  g_tech_AI %N>%
  left_join(reg_RCA3 %>% filter(ctry_code == country_select[i]) %>% select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "grey") +
  geom_node_point(aes(colour = factor(RCA), size = Category, shape= Category)) +
  scale_shape_manual(values=c(15, 16, 17, 18)) + labs(color   = "RCA")+ 
  scale_size_manual(values=c(10, 10, 10, 4))+
  geom_node_text(aes(filter=RCA > .9, label = field_name), size = 5, repel = TRUE) +
  scale_color_manual(values=c("gray90", "green4"))+
  theme_graph() +
  ggtitle("IPC Technology Space: USA (2004-2018)") 

i = 3
IPC3Colour_ThirdPeriod <- 
  g_tech_AI %N>%
  left_join(reg_RCA3 %>% filter(ctry_code == country_select[i]) %>% select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "grey") +
  geom_node_point(aes(colour = factor(RCA), size = Category, shape= Category)) +
  scale_shape_manual(values=c(15, 16, 17, 18)) + labs(color   = "RCA")+ 
  scale_size_manual(values=c(10, 10, 10, 4))+
  geom_node_text(aes(filter=RCA > .9, label = field_name), size = 5, repel = TRUE) +
  scale_color_manual(values=c("gray90", "green4"))+
  theme_graph() +
  ggtitle("IPC Technology Space: Japan (2004-2018)") 

i = 4
IPC4Colour_ThirdPeriod <- 
  g_tech_AI %N>%
  left_join(reg_RCA3 %>% filter(ctry_code == country_select[i]) %>% select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "grey") +
  geom_node_point(aes(colour = factor(RCA), size = Category, shape= Category)) +
  scale_shape_manual(values=c(15, 16, 17, 18)) + labs(color   = "RCA")+ 
  scale_size_manual(values=c(10, 10, 10, 4))+
  geom_node_text(aes(filter=RCA > .9, label = field_name), size = 5, repel = TRUE) +
  scale_color_manual(values=c("gray90", "green4"))+
  theme_graph() +
  ggtitle("IPC Technology Space: South Korea (2004-2018)") 

#For saving the coloured pictures in high resolution:
jpeg("Files_created_with_the_code/figures/high_resolution/Fig5_colour_China_3rdHigh.jpg", width = 14, height = 10, units = 'in', res = 800)
IPC1Colour_ThirdPeriod
dev.off()

jpeg("Files_created_with_the_code/figures/high_resolution/Fig5_colour_US_3rdHigh.jpg", width = 14, height = 10, units = 'in', res = 800)
IPC2Colour_ThirdPeriod
dev.off()

jpeg("Files_created_with_the_code/figures/high_resolution/Fig5_colour_JP_3rdHigh.jpg", width = 14, height = 10, units = 'in', res = 800)
IPC3Colour_ThirdPeriod
dev.off()

jpeg("Files_created_with_the_code/figures/high_resolution/Fig5_colour_KR_3rdHigh.jpg", width = 14, height = 10, units = 'in', res = 800)
IPC4Colour_ThirdPeriod
dev.off()

#For saving the coloured pictures in low resolution:
jpeg("Files_created_with_the_code/figures/low_resolution/Fig5_colour_China_3rdLow.jpg", width = 14, height = 10, units = 'in', res = 72)
IPC1Colour_ThirdPeriod
dev.off()

jpeg("Files_created_with_the_code/figures/low_resolution/Fig5_colour_US_3rdLow.jpg", width = 14, height = 10, units = 'in', res = 72)
IPC2Colour_ThirdPeriod
dev.off()

jpeg("Files_created_with_the_code/figures/low_resolution/Fig5_colour_JP_3rdLow.jpg", width = 14, height = 10, units = 'in', res = 72)
IPC3Colour_ThirdPeriod
dev.off()

jpeg("Files_created_with_the_code/figures/low_resolution/Fig5_colour_KR_3rdLow.jpg", width = 14, height = 10, units = 'in', res = 72)
IPC4Colour_ThirdPeriod
dev.off()

#For saving the coloured pictures in regular resolution:
jpeg("Files_created_with_the_code/figures/regular_resolution_ALL_FIGURES_ARE_HERE/Fig5_colour_China_3rd.jpg", width = 14, height = 10, units = 'in', res = 300)
IPC1Colour_ThirdPeriod
dev.off()

jpeg("Files_created_with_the_code/figures/regular_resolution_ALL_FIGURES_ARE_HERE/Fig5_colour_US_3rd.jpg", width = 14, height = 10, units = 'in', res = 300)
IPC2Colour_ThirdPeriod
dev.off()

jpeg("Files_created_with_the_code/figures/regular_resolution_ALL_FIGURES_ARE_HERE/Fig5_colour_JP_3rd.jpg", width = 14, height = 10, units = 'in', res = 300)
IPC3Colour_ThirdPeriod
dev.off()

jpeg("Files_created_with_the_code/figures/regular_resolution_ALL_FIGURES_ARE_HERE/Fig5_colour_KR_3rd.jpg", width = 14, height = 10, units = 'in', res = 300)
IPC4Colour_ThirdPeriod
dev.off()

#Now we'll plot the three periods at once per country. First we create a function for that:
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
#and now we apply this function to the previously created figures:
jpeg("Files_created_with_the_code/figures/regular_resolution_ALL_FIGURES_ARE_HERE/Fig5_colour_threeplotsCN.jpg", width = 12, height = 26, units = 'in', res = 300)
multiplot(IPC1Colour_FirstPeriod, IPC1Colour_SecondPeriod, IPC1Colour_ThirdPeriod, cols=1) 
dev.off()

jpeg("Files_created_with_the_code/figures/regular_resolution_ALL_FIGURES_ARE_HERE/Fig5_colour_threeplotsUS.jpg", width = 12, height = 26, units = 'in', res = 300)
multiplot(IPC2Colour_FirstPeriod, IPC2Colour_SecondPeriod, IPC2Colour_ThirdPeriod, cols=1) 
dev.off()

jpeg("Files_created_with_the_code/figures/regular_resolution_ALL_FIGURES_ARE_HERE/Fig5_colour_threeplotsJP.jpg", width = 12, height = 26, units = 'in', res = 300)
multiplot(IPC3Colour_FirstPeriod, IPC3Colour_SecondPeriod, IPC3Colour_ThirdPeriod, cols=1) 
dev.off()

jpeg("Files_created_with_the_code/figures/regular_resolution_ALL_FIGURES_ARE_HERE/Fig5_colour_threeplotsKR.jpg", width = 12, height = 26, units = 'in', res = 300)
multiplot(IPC4Colour_FirstPeriod, IPC4Colour_SecondPeriod, IPC4Colour_ThirdPeriod, cols=1) 
dev.off()

#1.4.2. IPC Visualization AI-----
#First period
country_select <- c("AI_pat")
i = 1
IPC_AI1Colour <- 
  g_tech_AI %N>%
  left_join(reg_RCA_AI1 %>% filter(ctry_code == country_select[i]) %>% select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "grey") + 
  geom_node_point(aes(colour = factor(RCA), size = dgr)) + labs(color   = "RCA")+ 
  geom_node_text(aes(filter=RCA > .9, label = field_name), size = 5, repel = TRUE) +
  scale_color_manual(values=c("gray90", "green4"))+
  theme_graph() +
  ggtitle("Technology Space: AI patents (1974-1988)") + guides(colour = guide_legend(override.aes = list(size=10)))

#colored high resolution:
jpeg("Files_created_with_the_code/figures/high_resolution/Fig2_colour_AI_1stHigh.jpg", width = 14, height = 10, units = 'in', res = 800)
IPC_AI1Colour
dev.off()

#colored low resolution:
jpeg("Files_created_with_the_code/figures/low_resolution/Fig2_colour_AI_1stLow.jpg", width = 14, height = 10, units = 'in', res = 72)
IPC_AI1Colour
dev.off()

#colored regular:
jpeg("Files_created_with_the_code/figures/regular_resolution_ALL_FIGURES_ARE_HERE/Fig2_colour_AI_1st.jpg", width = 14, height = 10, units = 'in', res = 300)
IPC_AI1Colour
dev.off()

#Second period
IPC_AI2Colour <- 
  g_tech_AI %N>%
  left_join(reg_RCA_AI2 %>% filter(ctry_code == country_select[i]) %>% select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "grey") + 
  geom_node_point(aes(colour = factor(RCA), size = dgr)) + labs(color   = "RCA")+ 
  geom_node_text(aes(filter=RCA > .9, label = field_name), size = 5, repel = TRUE) +
  scale_color_manual(values=c("gray90", "green4"))+
  theme_graph() +
  ggtitle("Technology Space: AI patents (1989-2003)") + guides(colour = guide_legend(override.aes = list(size=10)))

#colored high resolution:
jpeg("Files_created_with_the_code/figures/high_resolution/Fig2_colour_AI_2ndHigh.jpg", width = 14, height = 10, units = 'in', res = 800)
IPC_AI2Colour
dev.off()

#colored low resolution:
jpeg("Files_created_with_the_code/figures/low_resolution/Fig2_colour_AI_2ndLow.jpg", width = 14, height = 10, units = 'in', res = 72)
IPC_AI2Colour
dev.off()

#colored regular resolution:
jpeg("Files_created_with_the_code/figures/regular_resolution_ALL_FIGURES_ARE_HERE/Fig2_colour_AI_2nd.jpg", width = 14, height = 10, units = 'in', res = 300)
IPC_AI2Colour
dev.off()

#Third period
IPC_AI3Colour <- 
  g_tech_AI %N>%
  left_join(reg_RCA_AI3 %>% filter(ctry_code == country_select[i]) %>% select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "grey") + 
  geom_node_point(aes(colour = factor(RCA), size = dgr)) + labs(color   = "RCA")+ 
  geom_node_text(aes(filter=RCA > .9, label = field_name), size = 5, repel = TRUE) +
  scale_color_manual(values=c("gray90", "green4"))+
  theme_graph() +
  ggtitle("Technology Space: AI patents (2004-2018)") + guides(colour = guide_legend(override.aes = list(size=10))) 

#colored high resolution:
jpeg("Files_created_with_the_code/figures/high_resolution/Fig2_colour_AI_3rdHigh.jpg", width = 14, height = 10, units = 'in', res = 800)
IPC_AI3Colour
dev.off()

#colored low resolution:
jpeg("Files_created_with_the_code/figures/low_resolution/Fig2_colour_AI_3rdLow.jpg", width = 14, height = 10, units = 'in', res = 72)
IPC_AI3Colour
dev.off()

#colored regular resolution:
jpeg("Files_created_with_the_code/figures/regular_resolution_ALL_FIGURES_ARE_HERE/Fig2_colour_AI_3rd.jpg", width = 14, height = 10, units = 'in', res = 300)
IPC_AI3Colour
dev.off()

#all three together:
jpeg("Files_created_with_the_code/figures/regular_resolution_ALL_FIGURES_ARE_HERE/Fig2_colour_threeplotsAI.jpg", width = 12, height = 26, units = 'in', res = 300)
multiplot(IPC_AI1Colour, IPC_AI2Colour, IPC_AI3Colour, cols=1) 
dev.off()

#2. SECOND PART: Complexity and Relatedness ----
#In this part we calculate all the complexity and relatedness indicators used for discussing AI emergence and
#the effects of this emergence on countries

#Starting with a clean environment and the important libraries:
library(tidyverse) # Collection of all the good stuff like dplyr, ggplot2 ect.
library(magrittr) # For extra-piping operators (eg. %<>%)
# Network specific
library(ggraph) # For ggplot2 style graph plotting
library(tidygraph) # For tidy-style graph manipulation
library(EconGeo) # Economic Geography functions
library(data.table) #for reading the big files using fread and for replacing countries names (by AI_pat for example)
library(stringr) #for separating the IPC codes in subclasses
library(janitor) #used here for converting the first column of data to row names.
#for visualization:
library(ggrepel)
library(scales) #for scaling without cutting data out
library(patchwork) #for cutting out the X labs while keeping the legend

#2.1. First period ----
#2.1.1.Load the data we need and filter it -----
#The file for the first period is composed of 45,182,803 lines which we will read in 3 parts:
rm(list=ls())
setwd("large_files") #for loading the big files
c <- 45182803 -40000000
IPC_all_patents_Part1 <- fread("All_patents_and_IPCs_Part2.csv", header = F, nrow = 20000000)[ ,c(-4)]
IPC_all_patents_Part2 <- fread("All_patents_and_IPCs_Part2.csv", header = F, nrow = 20000000, skip = 20000000)[ ,c(-4)]
IPC_all_patents_Part3 <- fread("All_patents_and_IPCs_Part2.csv", header = F, nrow = c, skip = 40000000)[ ,c(-4)]

a = 1973
b = 1989

IPC_all_patents_Part1 <- IPC_all_patents_Part1[IPC_all_patents_Part1$V5 < b,]
IPC_all_patents_Part1 <- IPC_all_patents_Part1[IPC_all_patents_Part1$V5 > a,]

IPC_all_patents_Part2 <- IPC_all_patents_Part2[IPC_all_patents_Part2$V5 < b,]
IPC_all_patents_Part2 <- IPC_all_patents_Part2[IPC_all_patents_Part2$V5 > a,]

IPC_all_patents_Part3 <- IPC_all_patents_Part3[IPC_all_patents_Part3$V5 < b,]
IPC_all_patents_Part3 <- IPC_all_patents_Part3[IPC_all_patents_Part3$V5 > a,]

IPC_all_patents_1st <- rbind(IPC_all_patents_Part1, IPC_all_patents_Part2, IPC_all_patents_Part3)
rm(IPC_all_patents_Part1, IPC_all_patents_Part2, IPC_all_patents_Part3)

names(IPC_all_patents_1st) <- c("appln_id", "ctry_code", "techn_field_nr", "priority_year")
IPC_all_patents_1st$ctry_code2 <- IPC_all_patents_1st$ctry_code

#read AI patents
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
patents_AI_specific <- read.csv("other_files/IPCs_AI.csv", sep = ";", header = TRUE, dec=",")
patents_AI_specific$ctry_code <- as.vector(patents_AI_specific$ctry_code)
patents_AI_specific$ctry_code <- "AI_pat"
setDT(patents_AI_specific)
setDT(IPC_all_patents_1st)
IPC_all_patents_1st[patents_AI_specific, on = c("appln_id"), ctry_code2 := i.ctry_code]

IPC_all_patents_1st_US <- IPC_all_patents_1st[IPC_all_patents_1st$ctry_code == "US", ]
IPC_all_patents_1st_CN <- IPC_all_patents_1st[IPC_all_patents_1st$ctry_code == "CN", ]
IPC_all_patents_1st_KR <- IPC_all_patents_1st[IPC_all_patents_1st$ctry_code == "KR", ]
IPC_all_patents_1st_JP <- IPC_all_patents_1st[IPC_all_patents_1st$ctry_code == "JP", ]
IPC_all_patents_1st_AI <- IPC_all_patents_1st[IPC_all_patents_1st$ctry_code2 == "AI_pat", ]

#2.1.2. Calculate Complexity -----
IPC_all_patents_1st_In <- IPC_all_patents_1st[,c((-1), (-4), (-5))]
mat_1st <- as.data.frame(table(IPC_all_patents_1st_In$ctry_code, IPC_all_patents_1st_In$techn_field_nr))
mat_1st <- get.matrix(mat_1st)

#Considering technologies in general and testing steps (in the paper we use 2 steps for technology complexity and 1 for countries)
KnowledgeComp_1st <- as.data.frame(MORt(mat_1st))
KnowledgeComp_1st$Step0 <- MORt(mat_1st, steps = 0)
KnowledgeComp_1st$Step1 <- MORt(mat_1st, steps = 1)
KnowledgeComp_1st$Step2 <- MORt(mat_1st, steps = 2)

#let's see with binary values
mat_1st_RCAs <- location.quotient(mat_1st, binary = T)
KnowledgeComp_1st$RCA <- MORt(mat_1st_RCAs)
KnowledgeComp_1st$RCA_Step0 <- MORt(mat_1st_RCAs, steps = 0)
KnowledgeComp_1st$RCA_Step1 <- MORt(mat_1st_RCAs, steps = 1)
KnowledgeComp_1st$RCA_Step2 <- MORt(mat_1st_RCAs, steps = 2)
write.csv2(KnowledgeComp_1st, file = "Files_created_with_the_code/data/files_code_Fields_analysis/KnowledgeComp_1st.csv", row.names = TRUE)

#Considering Morc 1st period (thus, now we are considering countries; we calculate using all steps here, but in the paper we 
#use the results from 1 step)
KnowledgeComp_1st_Morc <- as.data.frame(MORc(mat_1st, RCA = T))
KnowledgeComp_1st_Morc$RCA <- MORc(mat_1st, RCA = T)
KnowledgeComp_1st_Morc$RCA_Step0 <- MORc(mat_1st, steps = 0, RCA = T)
KnowledgeComp_1st_Morc$RCA_Step1 <- MORc(mat_1st, steps = 1, RCA = T)
KnowledgeComp_1st_Morc$RCA_Step2 <- MORc(mat_1st, steps = 2, RCA = T)

#then select only the categories:
Top4_1st <- mat_1st[,c(6,7,10,12)]
Top3_1st <- mat_1st[,c(4,5,11)]
Surrounding_1st <- mat_1st[,c(1,2,3,4,13,25,34)]

KnowledgeComp_1st_Top4 <- as.data.frame(MORc(Top4_1st, RCA = T))
KnowledgeComp_1st_Top4$RCA <- MORc(Top4_1st, RCA = T)
KnowledgeComp_1st_Top4$RCA_Step0 <- MORc(Top4_1st, steps = 0, RCA = T)
KnowledgeComp_1st_Top4$RCA_Step1 <- MORc(Top4_1st, steps = 1, RCA = T)
KnowledgeComp_1st_Top4$RCA_Step2 <- MORc(Top4_1st, steps = 2, RCA = T)

KnowledgeComp_1st_Top3 <- as.data.frame(MORc(Top3_1st, RCA = T))
KnowledgeComp_1st_Top3$RCA <- MORc(Top3_1st, RCA = T)
KnowledgeComp_1st_Top3$RCA_Step0 <- MORc(Top3_1st, steps = 0, RCA = T)
KnowledgeComp_1st_Top3$RCA_Step1 <- MORc(Top3_1st, steps = 1, RCA = T)
KnowledgeComp_1st_Top3$RCA_Step2 <- MORc(Top3_1st, steps = 2, RCA = T)

KnowledgeComp_1st_Surr <- as.data.frame(MORc(Surrounding_1st, RCA = T))
KnowledgeComp_1st_Surr$RCA <- MORc(Surrounding_1st, RCA = T)
KnowledgeComp_1st_Surr$RCA_Step0 <- MORc(Surrounding_1st, steps = 0, RCA = T)
KnowledgeComp_1st_Surr$RCA_Step1 <- MORc(Surrounding_1st, steps = 1, RCA = T)
KnowledgeComp_1st_Surr$RCA_Step2 <- MORc(Surrounding_1st, steps = 2, RCA = T)

write.csv2(KnowledgeComp_1st_Morc, file = "Files_created_with_the_code/data/files_code_Fields_analysis/KnowledgeComp_1st_Morc.csv", row.names = T)
write.csv2(KnowledgeComp_1st_Top4, file = "Files_created_with_the_code/data/files_code_Fields_analysis/KnowledgeComp_1st_Top4.csv", row.names = T)
write.csv2(KnowledgeComp_1st_Top3, file = "Files_created_with_the_code/data/files_code_Fields_analysis/KnowledgeComp_1st_Top3.csv", row.names = T)
write.csv2(KnowledgeComp_1st_Surr, file = "Files_created_with_the_code/data/files_code_Fields_analysis/KnowledgeComp_1st_Surr.csv", row.names = T)

#For AI complexity and Indicators:
IPC_all_patents_1st_In <- IPC_all_patents_1st[,c((-1), (-4), (-2))]
mat_1st <- as.data.frame(table(IPC_all_patents_1st_In$ctry_code2, IPC_all_patents_1st_In$techn_field_nr))
mat_1st <- get.matrix(mat_1st)
mat_1st_RCAs <- location.quotient(mat_1st, binary = T)
#Considering RCAs:
KnowledgeComp_PerCountry_1st_RCA <- as.data.frame(mat_1st_RCAs*MORt(mat_1st_RCAs))
KnowledgeComp_PerCountry_1st_RCA$Step <- "NoStep"

KnowledgeComp_PerCountry_1st_Step0_RCA <- as.data.frame(mat_1st_RCAs*MORt(mat_1st_RCAs, steps = 0))
KnowledgeComp_PerCountry_1st_Step0_RCA$Step <- "Step0"

KnowledgeComp_PerCountry_1st_Step1_RCA <- as.data.frame(mat_1st_RCAs*MORt(mat_1st_RCAs, steps = 1))
KnowledgeComp_PerCountry_1st_Step1_RCA$Step <- "Step1"

KnowledgeComp_PerCountry_1st_Step2_RCA <- as.data.frame(mat_1st_RCAs*MORt(mat_1st_RCAs, steps = 2))
KnowledgeComp_PerCountry_1st_Step2_RCA$Step <- "Step2"

KnowledgeComp_PerCountry_1st_All_RCAs <- rbind(KnowledgeComp_PerCountry_1st_RCA, KnowledgeComp_PerCountry_1st_Step0_RCA,
                                               KnowledgeComp_PerCountry_1st_Step1_RCA, KnowledgeComp_PerCountry_1st_Step2_RCA)

write.csv2(KnowledgeComp_PerCountry_1st_All_RCAs, file = "Files_created_with_the_code/data/files_code_Fields_analysis/KnowledgeComp_PerCountry_1st_All_RCAs_AI.csv", row.names = TRUE)

#2.1.3. Calculate Relatedness -----
#create the function we need:
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

#Now we calculate by country, starting with the US:
mat_tech_1st_US <- create_sparse_matrix(i = IPC_all_patents_1st_US %>% pull(appln_id),
                                        j = IPC_all_patents_1st_US %>% pull(techn_field_nr))

mat_tech_1st_US %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_1st_US_rel_asso <- relatedness(mat_tech_1st_US, method = "association")
Relatedness_US <- as.data.frame(mean(mat_tech_1st_US_rel_asso))
rownames(Relatedness_US) <- c("US")
names(Relatedness_US) <- c("Association")
Relatedness_US$Period <- "1st"

#now we will select the categories; please note that, following the paper denomination, Top4 used here refers to the category
#AI-core fields; Top3 refers to AI-related fields and Top7 refers to Surrounding fields;

#then select only the top 4 areas ;
IPC_all_patents_1st_US_Top4 <- IPC_all_patents_1st_US[IPC_all_patents_1st_US$techn_field_nr == "6" | 
                                                        IPC_all_patents_1st_US$techn_field_nr == "7"| 
                                                        IPC_all_patents_1st_US$techn_field_nr == "10"| 
                                                        IPC_all_patents_1st_US$techn_field_nr == "12", ]

mat_tech_1st_US_Top4 <- create_sparse_matrix(i = IPC_all_patents_1st_US_Top4 %>% pull(appln_id),
                                             j = IPC_all_patents_1st_US_Top4 %>% pull(techn_field_nr))

mat_tech_1st_US_Top4 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_1st_US_Top4_rel_asso <- relatedness(mat_tech_1st_US_Top4, method = "association")
Relatedness_US$Association_top4 <- mean(mat_tech_1st_US_Top4_rel_asso)

#then select only the top 3 areas:
IPC_all_patents_1st_US_Top3 <- IPC_all_patents_1st_US[IPC_all_patents_1st_US$techn_field_nr == "4" | 
                                                        IPC_all_patents_1st_US$techn_field_nr == "5"| 
                                                        IPC_all_patents_1st_US$techn_field_nr == "11", ]

mat_tech_1st_US_Top3 <- create_sparse_matrix(i = IPC_all_patents_1st_US_Top3 %>% pull(appln_id),
                                             j = IPC_all_patents_1st_US_Top3 %>% pull(techn_field_nr))

mat_tech_1st_US_Top3 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_1st_US_Top3_rel_asso <- relatedness(mat_tech_1st_US_Top3, method = "association")
Relatedness_US$Association_Top3 <- mean(mat_tech_1st_US_Top3_rel_asso)

#and finally the top 7 non-AI areas:
IPC_all_patents_1st_US_Top7 <- IPC_all_patents_1st_US[IPC_all_patents_1st_US$techn_field_nr == "1"| 
                                                        IPC_all_patents_1st_US$techn_field_nr == "2"| 
                                                        IPC_all_patents_1st_US$techn_field_nr == "3"|
                                                        IPC_all_patents_1st_US$techn_field_nr == "13"| 
                                                        IPC_all_patents_1st_US$techn_field_nr == "25"| 
                                                        IPC_all_patents_1st_US$techn_field_nr == "34", ]

mat_tech_1st_US_Top7 <- create_sparse_matrix(i = IPC_all_patents_1st_US_Top7 %>% pull(appln_id),
                                             j = IPC_all_patents_1st_US_Top7 %>% pull(techn_field_nr))

mat_tech_1st_US_Top7 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_1st_US_Top7_rel_asso <- relatedness(mat_tech_1st_US_Top7, method = "association")
Relatedness_US$Association_Top7 <- mean(mat_tech_1st_US_Top7_rel_asso)

#China:
mat_tech_1st_CN <- create_sparse_matrix(i = IPC_all_patents_1st_CN %>% pull(appln_id),
                                        j = IPC_all_patents_1st_CN %>% pull(techn_field_nr))

mat_tech_1st_CN %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_1st_CN_rel_asso <- relatedness(mat_tech_1st_CN, method = "association")
Relatedness_CN <- as.data.frame(mean(mat_tech_1st_CN_rel_asso))
rownames(Relatedness_CN) <- c("CN")
names(Relatedness_CN) <- c("Association")
Relatedness_CN$Period <- "1st"

#then select only the top 4 areas ;
IPC_all_patents_1st_CN_Top4 <- IPC_all_patents_1st_CN[IPC_all_patents_1st_CN$techn_field_nr == "6" | 
                                                        IPC_all_patents_1st_CN$techn_field_nr == "7"| 
                                                        IPC_all_patents_1st_CN$techn_field_nr == "10"| 
                                                        IPC_all_patents_1st_CN$techn_field_nr == "12", ]

mat_tech_1st_CN_Top4 <- create_sparse_matrix(i = IPC_all_patents_1st_CN_Top4 %>% pull(appln_id),
                                             j = IPC_all_patents_1st_CN_Top4 %>% pull(techn_field_nr))

mat_tech_1st_CN_Top4 %<>% 
  crossprod() %>% 
  as.matrix() 
mat_tech_1st_CN_Top4_rel_asso <- relatedness(mat_tech_1st_CN_Top4, method = "association")
Relatedness_CN$Association_top4 <- mean(mat_tech_1st_CN_Top4_rel_asso)

#then select only the top 3 areas:
IPC_all_patents_1st_CN_Top3 <- IPC_all_patents_1st_CN[IPC_all_patents_1st_CN$techn_field_nr == "4" | 
                                                        IPC_all_patents_1st_CN$techn_field_nr == "5"| 
                                                        IPC_all_patents_1st_CN$techn_field_nr == "11", ]


mat_tech_1st_CN_Top3 <- create_sparse_matrix(i = IPC_all_patents_1st_CN_Top3 %>% pull(appln_id),
                                             j = IPC_all_patents_1st_CN_Top3 %>% pull(techn_field_nr))

mat_tech_1st_CN_Top3 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_1st_CN_Top3_rel_asso <- relatedness(mat_tech_1st_CN_Top3, method = "association")
Relatedness_CN$Association_Top3 <- mean(mat_tech_1st_CN_Top3_rel_asso)

#and finally the top 7 non-AI areas:
IPC_all_patents_1st_CN_Top7 <- IPC_all_patents_1st_CN[IPC_all_patents_1st_CN$techn_field_nr == "1"| 
                                                        IPC_all_patents_1st_CN$techn_field_nr == "2"| 
                                                        IPC_all_patents_1st_CN$techn_field_nr == "3"|
                                                        IPC_all_patents_1st_CN$techn_field_nr == "13"| 
                                                        IPC_all_patents_1st_CN$techn_field_nr == "25"| 
                                                        IPC_all_patents_1st_CN$techn_field_nr == "34", ]

mat_tech_1st_CN_Top7 <- create_sparse_matrix(i = IPC_all_patents_1st_CN_Top7 %>% pull(appln_id),
                                             j = IPC_all_patents_1st_CN_Top7 %>% pull(techn_field_nr))

mat_tech_1st_CN_Top7 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_1st_CN_Top7_rel_asso <- relatedness(mat_tech_1st_CN_Top7, method = "association")
Relatedness_CN$Association_Top7 <- mean(mat_tech_1st_CN_Top7_rel_asso)

#KR
mat_tech_1st_KR <- create_sparse_matrix(i = IPC_all_patents_1st_KR %>% pull(appln_id),
                                        j = IPC_all_patents_1st_KR %>% pull(techn_field_nr))

mat_tech_1st_KR %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_1st_KR_rel_asso <- relatedness(mat_tech_1st_KR, method = "association")
Relatedness_KR<- as.data.frame(mean(mat_tech_1st_KR_rel_asso))
rownames(Relatedness_KR) <- c("KR")
names(Relatedness_KR) <- c("Association")
Relatedness_KR$Period <- "1st"

#then select only the top 4 areas ;
IPC_all_patents_1st_KR_Top4 <- IPC_all_patents_1st_KR[IPC_all_patents_1st_KR$techn_field_nr == "6" | 
                                                        IPC_all_patents_1st_KR$techn_field_nr == "7"| 
                                                        IPC_all_patents_1st_KR$techn_field_nr == "10"| 
                                                        IPC_all_patents_1st_KR$techn_field_nr == "12", ]

mat_tech_1st_KR_Top4 <- create_sparse_matrix(i = IPC_all_patents_1st_KR_Top4 %>% pull(appln_id),
                                             j = IPC_all_patents_1st_KR_Top4 %>% pull(techn_field_nr))

mat_tech_1st_KR_Top4 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_1st_KR_Top4_rel_asso <- relatedness(mat_tech_1st_KR_Top4, method = "association")
Relatedness_KR$Association_top4 <- mean(mat_tech_1st_KR_Top4_rel_asso)

#then select only the top 3 areas:
IPC_all_patents_1st_KR_Top3 <- IPC_all_patents_1st_KR[IPC_all_patents_1st_KR$techn_field_nr == "4" | 
                                                        IPC_all_patents_1st_KR$techn_field_nr == "5"| 
                                                        IPC_all_patents_1st_KR$techn_field_nr == "11", ]


mat_tech_1st_KR_Top3 <- create_sparse_matrix(i = IPC_all_patents_1st_KR_Top3 %>% pull(appln_id),
                                             j = IPC_all_patents_1st_KR_Top3 %>% pull(techn_field_nr))

mat_tech_1st_KR_Top3 %<>% 
  crossprod() %>% 
  as.matrix() 


mat_tech_1st_KR_Top3_rel_asso <- relatedness(mat_tech_1st_KR_Top3, method = "association")
Relatedness_KR$Association_Top3 <- mean(mat_tech_1st_KR_Top3_rel_asso)

#and finally the top 7 non-AI areas:
IPC_all_patents_1st_KR_Top7 <- IPC_all_patents_1st_KR[IPC_all_patents_1st_KR$techn_field_nr == "1"| 
                                                        IPC_all_patents_1st_KR$techn_field_nr == "2"| 
                                                        IPC_all_patents_1st_KR$techn_field_nr == "3"|
                                                        IPC_all_patents_1st_KR$techn_field_nr == "13"| 
                                                        IPC_all_patents_1st_KR$techn_field_nr == "25"| 
                                                        IPC_all_patents_1st_KR$techn_field_nr == "34", ]

mat_tech_1st_KR_Top7 <- create_sparse_matrix(i = IPC_all_patents_1st_KR_Top7 %>% pull(appln_id),
                                             j = IPC_all_patents_1st_KR_Top7 %>% pull(techn_field_nr))

mat_tech_1st_KR_Top7 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_1st_KR_Top7_rel_asso <- relatedness(mat_tech_1st_KR_Top7, method = "association")
Relatedness_KR$Association_Top7 <- mean(mat_tech_1st_KR_Top7_rel_asso)

#Japan
mat_tech_1st_JP <- create_sparse_matrix(i = IPC_all_patents_1st_JP %>% pull(appln_id),
                                        j = IPC_all_patents_1st_JP %>% pull(techn_field_nr))

mat_tech_1st_JP %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_1st_JP_rel_asso <- relatedness(mat_tech_1st_JP, method = "association")
Relatedness_JP <- as.data.frame(mean(mat_tech_1st_JP_rel_asso))
rownames(Relatedness_JP) <- c("JP")
names(Relatedness_JP) <- c("Association")
Relatedness_JP$Period <- "1st"

#then select only the top 4 areas ;
IPC_all_patents_1st_JP_Top4 <- IPC_all_patents_1st_JP[IPC_all_patents_1st_JP$techn_field_nr == "6" | 
                                                        IPC_all_patents_1st_JP$techn_field_nr == "7"| 
                                                        IPC_all_patents_1st_JP$techn_field_nr == "10"| 
                                                        IPC_all_patents_1st_JP$techn_field_nr == "12", ]

mat_tech_1st_JP_Top4 <- create_sparse_matrix(i = IPC_all_patents_1st_JP_Top4 %>% pull(appln_id),
                                             j = IPC_all_patents_1st_JP_Top4 %>% pull(techn_field_nr))

mat_tech_1st_JP_Top4 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_1st_JP_Top4_rel_asso <- relatedness(mat_tech_1st_JP_Top4, method = "association")
Relatedness_JP$Association_top4 <- mean(mat_tech_1st_JP_Top4_rel_asso)

#then select only the top 3 areas:
IPC_all_patents_1st_JP_Top3 <- IPC_all_patents_1st_JP[IPC_all_patents_1st_JP$techn_field_nr == "4" | 
                                                        IPC_all_patents_1st_JP$techn_field_nr == "5"| 
                                                        IPC_all_patents_1st_JP$techn_field_nr == "11", ]

mat_tech_1st_JP_Top3 <- create_sparse_matrix(i = IPC_all_patents_1st_JP_Top3 %>% pull(appln_id),
                                             j = IPC_all_patents_1st_JP_Top3 %>% pull(techn_field_nr))

mat_tech_1st_JP_Top3 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_1st_JP_Top3_rel_asso <- relatedness(mat_tech_1st_JP_Top3, method = "association")
Relatedness_JP$Association_Top3 <- mean(mat_tech_1st_JP_Top3_rel_asso)

#and finally the top 7 non-AI areas:
IPC_all_patents_1st_JP_Top7 <- IPC_all_patents_1st_JP[IPC_all_patents_1st_JP$techn_field_nr == "1"| 
                                                        IPC_all_patents_1st_JP$techn_field_nr == "2"| 
                                                        IPC_all_patents_1st_JP$techn_field_nr == "3"|
                                                        IPC_all_patents_1st_JP$techn_field_nr == "13"| 
                                                        IPC_all_patents_1st_JP$techn_field_nr == "25"| 
                                                        IPC_all_patents_1st_JP$techn_field_nr == "34", ]

mat_tech_1st_JP_Top7 <- create_sparse_matrix(i = IPC_all_patents_1st_JP_Top7 %>% pull(appln_id),
                                             j = IPC_all_patents_1st_JP_Top7 %>% pull(techn_field_nr))

mat_tech_1st_JP_Top7 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_1st_JP_Top7_rel_asso <- relatedness(mat_tech_1st_JP_Top7, method = "association")
Relatedness_JP$Association_Top7 <- mean(mat_tech_1st_JP_Top7_rel_asso)

#AI
mat_tech_1st_AI <- create_sparse_matrix(i = IPC_all_patents_1st_AI %>% pull(appln_id),
                                        j = IPC_all_patents_1st_AI %>% pull(techn_field_nr))

mat_tech_1st_AI %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_1st_AI_rel_asso <- relatedness(mat_tech_1st_AI, method = "association")

Relatedness_AI <- as.data.frame(mean(mat_tech_1st_AI_rel_asso))
rownames(Relatedness_AI) <- c("AI")
names(Relatedness_AI) <- c("Association")
Relatedness_AI$Period <- "1st"

#then select only the top 4 areas ;
IPC_all_patents_1st_AI_Top4 <- IPC_all_patents_1st_AI[IPC_all_patents_1st_AI$techn_field_nr == "6" | 
                                                        IPC_all_patents_1st_AI$techn_field_nr == "7"| 
                                                        IPC_all_patents_1st_AI$techn_field_nr == "10"| 
                                                        IPC_all_patents_1st_AI$techn_field_nr == "12", ]

mat_tech_1st_AI_Top4 <- create_sparse_matrix(i = IPC_all_patents_1st_AI_Top4 %>% pull(appln_id),
                                             j = IPC_all_patents_1st_AI_Top4 %>% pull(techn_field_nr))

mat_tech_1st_AI_Top4 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_1st_AI_Top4_rel_asso <- relatedness(mat_tech_1st_AI_Top4, method = "association")
Relatedness_AI$Association_top4 <- mean(mat_tech_1st_AI_Top4_rel_asso)

#then select only the top 3 areas:
IPC_all_patents_1st_AI_Top3 <- IPC_all_patents_1st_AI[IPC_all_patents_1st_AI$techn_field_nr == "4" | 
                                                        IPC_all_patents_1st_AI$techn_field_nr == "5"| 
                                                        IPC_all_patents_1st_AI$techn_field_nr == "11", ]

mat_tech_1st_AI_Top3 <- create_sparse_matrix(i = IPC_all_patents_1st_AI_Top3 %>% pull(appln_id),
                                             j = IPC_all_patents_1st_AI_Top3 %>% pull(techn_field_nr))

mat_tech_1st_AI_Top3 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_1st_AI_Top3_rel_asso <- relatedness(mat_tech_1st_AI_Top3, method = "association")
Relatedness_AI$Association_Top3 <- mean(mat_tech_1st_AI_Top3_rel_asso)

#and finally the top 7 non-AI areas:
IPC_all_patents_1st_AI_Top7 <- IPC_all_patents_1st_AI[IPC_all_patents_1st_AI$techn_field_nr == "1"| 
                                                        IPC_all_patents_1st_AI$techn_field_nr == "2"| 
                                                        IPC_all_patents_1st_AI$techn_field_nr == "3"|
                                                        IPC_all_patents_1st_AI$techn_field_nr == "13"| 
                                                        IPC_all_patents_1st_AI$techn_field_nr == "25"| 
                                                        IPC_all_patents_1st_AI$techn_field_nr == "34", ]

mat_tech_1st_AI_Top7 <- create_sparse_matrix(i = IPC_all_patents_1st_AI_Top7 %>% pull(appln_id),
                                             j = IPC_all_patents_1st_AI_Top7 %>% pull(techn_field_nr))

mat_tech_1st_AI_Top7 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_1st_AI_Top7_rel_asso <- relatedness(mat_tech_1st_AI_Top7, method = "association")
Relatedness_AI$Association_Top7 <- mean(mat_tech_1st_AI_Top7_rel_asso)

#and we merge it all together:
Relatedness_FirstPeriod <- rbind(Relatedness_US, Relatedness_CN, Relatedness_KR, Relatedness_JP, Relatedness_AI)
Relatedness_FirstPeriod <- Relatedness_FirstPeriod[,c((2), (1), (3:5))]

write.csv2(Relatedness_FirstPeriod, file = "Files_created_with_the_code/data/files_code_Fields_analysis/Relatedness_1st_period_IPC.csv", row.names = TRUE)

#2.2. Second period ----
#Starting with an empty global environment:
rm(list=ls())
setwd("large_files")
#2.2.1.Load the data we need and filter it -----
#The file for the first period is composed of 45,182,803 lines which we will read in 3 parts:
c <- 45182803 -40000000
IPC_all_patents_Part1 <- fread("All_patents_and_IPCs_Part2.csv", header = F, nrow = 20000000)[ ,c(-4)]
IPC_all_patents_Part2 <- fread("All_patents_and_IPCs_Part2.csv", header = F, nrow = 20000000, skip = 20000000)[ ,c(-4)]
IPC_all_patents_Part3 <- fread("All_patents_and_IPCs_Part2.csv", header = F, nrow = c, skip = 40000000)[ ,c(-4)]

a = 1988
b = 2004

IPC_all_patents_Part1 <- IPC_all_patents_Part1[IPC_all_patents_Part1$V5 < b,]
IPC_all_patents_Part1 <- IPC_all_patents_Part1[IPC_all_patents_Part1$V5 > a,]

IPC_all_patents_Part2 <- IPC_all_patents_Part2[IPC_all_patents_Part2$V5 < b,]
IPC_all_patents_Part2 <- IPC_all_patents_Part2[IPC_all_patents_Part2$V5 > a,]

IPC_all_patents_Part3 <- IPC_all_patents_Part3[IPC_all_patents_Part3$V5 < b,]
IPC_all_patents_Part3 <- IPC_all_patents_Part3[IPC_all_patents_Part3$V5 > a,]

IPC_all_patents_2nd <- rbind(IPC_all_patents_Part1, IPC_all_patents_Part2, IPC_all_patents_Part3)
rm(IPC_all_patents_Part1, IPC_all_patents_Part2, IPC_all_patents_Part3)

names(IPC_all_patents_2nd) <- c("appln_id", "ctry_code", "techn_field_nr", "priority_year")
IPC_all_patents_2nd$ctry_code2 <- IPC_all_patents_2nd$ctry_code

#read AI patents
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
patents_AI_specific <- read.csv("other_files/IPCs_AI.csv", sep = ";", header = TRUE, dec=",")
patents_AI_specific$ctry_code <- as.vector(patents_AI_specific$ctry_code)
patents_AI_specific$ctry_code <- "AI_pat"
setDT(patents_AI_specific)
setDT(IPC_all_patents_2nd)
IPC_all_patents_2nd[patents_AI_specific, on = c("appln_id"), ctry_code2 := i.ctry_code]

IPC_all_patents_2nd_US <- IPC_all_patents_2nd[IPC_all_patents_2nd$ctry_code == "US", ]
IPC_all_patents_2nd_CN <- IPC_all_patents_2nd[IPC_all_patents_2nd$ctry_code == "CN", ]
IPC_all_patents_2nd_KR <- IPC_all_patents_2nd[IPC_all_patents_2nd$ctry_code == "KR", ]
IPC_all_patents_2nd_JP <- IPC_all_patents_2nd[IPC_all_patents_2nd$ctry_code == "JP", ]
IPC_all_patents_2nd_AI <- IPC_all_patents_2nd[IPC_all_patents_2nd$ctry_code2 == "AI_pat", ]

#2.2.2. Calculate Complexity -----
IPC_all_patents_2nd_In <- IPC_all_patents_2nd[,c((-1), (-4), (-5))]
mat_2nd <- as.data.frame(table(IPC_all_patents_2nd_In$ctry_code, IPC_all_patents_2nd_In$techn_field_nr))
mat_2nd <- get.matrix(mat_2nd)

#Considering technologies in general (used in Fig 8)
KnowledgeComp_2nd <- as.data.frame(MORt(mat_2nd))
KnowledgeComp_2nd$Step0 <- MORt(mat_2nd, steps = 0)
KnowledgeComp_2nd$Step1 <- MORt(mat_2nd, steps = 1)
KnowledgeComp_2nd$Step2 <- MORt(mat_2nd, steps = 2)

mat_2nd_RCAs <- location.quotient(mat_2nd, binary = T)
KnowledgeComp_2nd$RCA <- MORt(mat_2nd_RCAs)
KnowledgeComp_2nd$RCA_Step0 <- MORt(mat_2nd_RCAs, steps = 0)
KnowledgeComp_2nd$RCA_Step1 <- MORt(mat_2nd_RCAs, steps = 1)
KnowledgeComp_2nd$RCA_Step2 <- MORt(mat_2nd_RCAs, steps = 2)
write.csv2(KnowledgeComp_2nd, file = "Files_created_with_the_code/data/files_code_Fields_analysis/KnowledgeComp_2nd.csv", row.names = TRUE)

#Considering Morc 2nd period
KnowledgeComp_2nd_Morc <- as.data.frame(MORc(mat_2nd, RCA = T))
KnowledgeComp_2nd_Morc$RCA <- MORc(mat_2nd, RCA = T)
KnowledgeComp_2nd_Morc$RCA_Step0 <- MORc(mat_2nd, steps = 0, RCA = T)
KnowledgeComp_2nd_Morc$RCA_Step1 <- MORc(mat_2nd, steps = 1, RCA = T)
KnowledgeComp_2nd_Morc$RCA_Step2 <- MORc(mat_2nd, steps = 2, RCA = T)

#then select only the categories:
Top4_2nd <- mat_2nd[,c(6,7,10,12)]
Top3_2nd <- mat_2nd[,c(4,5,11)]
Surrounding_2nd <- mat_2nd[,c(1,2,3,4,13,25,34)]

KnowledgeComp_2nd_Top4 <- as.data.frame(MORc(Top4_2nd, RCA = T))
KnowledgeComp_2nd_Top4$RCA <- MORc(Top4_2nd, RCA = T)
KnowledgeComp_2nd_Top4$RCA_Step0 <- MORc(Top4_2nd, steps = 0, RCA = T)
KnowledgeComp_2nd_Top4$RCA_Step1 <- MORc(Top4_2nd, steps = 1, RCA = T)
KnowledgeComp_2nd_Top4$RCA_Step2 <- MORc(Top4_2nd, steps = 2, RCA = T)

KnowledgeComp_2nd_Top3 <- as.data.frame(MORc(Top3_2nd, RCA = T))
KnowledgeComp_2nd_Top3$RCA <- MORc(Top3_2nd, RCA = T)
KnowledgeComp_2nd_Top3$RCA_Step0 <- MORc(Top3_2nd, steps = 0, RCA = T)
KnowledgeComp_2nd_Top3$RCA_Step1 <- MORc(Top3_2nd, steps = 1, RCA = T)
KnowledgeComp_2nd_Top3$RCA_Step2 <- MORc(Top3_2nd, steps = 2, RCA = T)

KnowledgeComp_2nd_Surr <- as.data.frame(MORc(Surrounding_2nd, RCA = T))
KnowledgeComp_2nd_Surr$RCA <- MORc(Surrounding_2nd, RCA = T)
KnowledgeComp_2nd_Surr$RCA_Step0 <- MORc(Surrounding_2nd, steps = 0, RCA = T)
KnowledgeComp_2nd_Surr$RCA_Step1 <- MORc(Surrounding_2nd, steps = 1, RCA = T)
KnowledgeComp_2nd_Surr$RCA_Step2 <- MORc(Surrounding_2nd, steps = 2, RCA = T)

write.csv2(KnowledgeComp_2nd_Morc, file = "Files_created_with_the_code/data/files_code_Fields_analysis/KnowledgeComp_2nd_Morc.csv", row.names = T)
write.csv2(KnowledgeComp_2nd_Top4, file = "Files_created_with_the_code/data/files_code_Fields_analysis/KnowledgeComp_2nd_Top4.csv", row.names = T)
write.csv2(KnowledgeComp_2nd_Top3, file = "Files_created_with_the_code/data/files_code_Fields_analysis/KnowledgeComp_2nd_Top3.csv", row.names = T)
write.csv2(KnowledgeComp_2nd_Surr, file = "Files_created_with_the_code/data/files_code_Fields_analysis/KnowledgeComp_2nd_Surr.csv", row.names = T)

#For AI complexity and Indicators:
IPC_all_patents_2nd_In <- IPC_all_patents_2nd[,c((-1), (-4), (-2))]
mat_2nd <- as.data.frame(table(IPC_all_patents_2nd_In$ctry_code2, IPC_all_patents_2nd_In$techn_field_nr))
mat_2nd <- get.matrix(mat_2nd)
mat_2nd_RCAs <- location.quotient(mat_2nd, binary = T)

#Considering RCAs:
KnowledgeComp_PerCountry_2nd_RCA <- as.data.frame(mat_2nd_RCAs*MORt(mat_2nd_RCAs))
KnowledgeComp_PerCountry_2nd_RCA$Step <- "NoStep"

KnowledgeComp_PerCountry_2nd_Step0_RCA <- as.data.frame(mat_2nd_RCAs*MORt(mat_2nd_RCAs, steps = 0))
KnowledgeComp_PerCountry_2nd_Step0_RCA$Step <- "Step0"

KnowledgeComp_PerCountry_2nd_Step1_RCA <- as.data.frame(mat_2nd_RCAs*MORt(mat_2nd_RCAs, steps = 1))
KnowledgeComp_PerCountry_2nd_Step1_RCA$Step <- "Step1"

KnowledgeComp_PerCountry_2nd_Step2_RCA <- as.data.frame(mat_2nd_RCAs*MORt(mat_2nd_RCAs, steps = 2))
KnowledgeComp_PerCountry_2nd_Step2_RCA$Step <- "Step2"

KnowledgeComp_PerCountry_2nd_All_RCAs <- rbind(KnowledgeComp_PerCountry_2nd_RCA, KnowledgeComp_PerCountry_2nd_Step0_RCA,
                                               KnowledgeComp_PerCountry_2nd_Step1_RCA, KnowledgeComp_PerCountry_2nd_Step2_RCA)

write.csv2(KnowledgeComp_PerCountry_2nd_All_RCAs, file = "Files_created_with_the_code/data/files_code_Fields_analysis/KnowledgeComp_PerCountry_2nd_All_RCAs_AI.csv", row.names = TRUE)

#2.2.3. Calculate Relatedness -----
#create the function we need:
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

#Now we calculate by country, starting with the US:
mat_tech_2nd_US <- create_sparse_matrix(i = IPC_all_patents_2nd_US %>% pull(appln_id),
                                        j = IPC_all_patents_2nd_US %>% pull(techn_field_nr))

mat_tech_2nd_US %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_2nd_US_rel_asso <- relatedness(mat_tech_2nd_US, method = "association")
Relatedness_US <- as.data.frame(mean(mat_tech_2nd_US_rel_asso))
rownames(Relatedness_US) <- c("US")
names(Relatedness_US) <- c("Association")
Relatedness_US$Period <- "2nd"

#then select only the top 4 areas ;
IPC_all_patents_2nd_US_Top4 <- IPC_all_patents_2nd_US[IPC_all_patents_2nd_US$techn_field_nr == "6" | 
                                                        IPC_all_patents_2nd_US$techn_field_nr == "7"| 
                                                        IPC_all_patents_2nd_US$techn_field_nr == "10"| 
                                                        IPC_all_patents_2nd_US$techn_field_nr == "12", ]

mat_tech_2nd_US_Top4 <- create_sparse_matrix(i = IPC_all_patents_2nd_US_Top4 %>% pull(appln_id),
                                             j = IPC_all_patents_2nd_US_Top4 %>% pull(techn_field_nr))

mat_tech_2nd_US_Top4 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_2nd_US_Top4_rel_asso <- relatedness(mat_tech_2nd_US_Top4, method = "association")
Relatedness_US$Association_top4 <- mean(mat_tech_2nd_US_Top4_rel_asso)

#then select only the top 3 areas:
IPC_all_patents_2nd_US_Top3 <- IPC_all_patents_2nd_US[IPC_all_patents_2nd_US$techn_field_nr == "4" | 
                                                        IPC_all_patents_2nd_US$techn_field_nr == "5"| 
                                                        IPC_all_patents_2nd_US$techn_field_nr == "11", ]

mat_tech_2nd_US_Top3 <- create_sparse_matrix(i = IPC_all_patents_2nd_US_Top3 %>% pull(appln_id),
                                             j = IPC_all_patents_2nd_US_Top3 %>% pull(techn_field_nr))

mat_tech_2nd_US_Top3 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_2nd_US_Top3_rel_asso <- relatedness(mat_tech_2nd_US_Top3, method = "association")
Relatedness_US$Association_Top3 <- mean(mat_tech_2nd_US_Top3_rel_asso)

#and finally the top 7 non-AI areas:
IPC_all_patents_2nd_US_Top7 <- IPC_all_patents_2nd_US[IPC_all_patents_2nd_US$techn_field_nr == "1"| 
                                                        IPC_all_patents_2nd_US$techn_field_nr == "2"| 
                                                        IPC_all_patents_2nd_US$techn_field_nr == "3"|
                                                        IPC_all_patents_2nd_US$techn_field_nr == "13"| 
                                                        IPC_all_patents_2nd_US$techn_field_nr == "25"| 
                                                        IPC_all_patents_2nd_US$techn_field_nr == "34", ]

mat_tech_2nd_US_Top7 <- create_sparse_matrix(i = IPC_all_patents_2nd_US_Top7 %>% pull(appln_id),
                                             j = IPC_all_patents_2nd_US_Top7 %>% pull(techn_field_nr))

mat_tech_2nd_US_Top7 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_2nd_US_Top7_rel_asso <- relatedness(mat_tech_2nd_US_Top7, method = "association")
Relatedness_US$Association_Top7 <- mean(mat_tech_2nd_US_Top7_rel_asso)


#China:
mat_tech_2nd_CN <- create_sparse_matrix(i = IPC_all_patents_2nd_CN %>% pull(appln_id),
                                        j = IPC_all_patents_2nd_CN %>% pull(techn_field_nr))

mat_tech_2nd_CN %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_2nd_CN_rel_asso <- relatedness(mat_tech_2nd_CN, method = "association")
Relatedness_CN <- as.data.frame(mean(mat_tech_2nd_CN_rel_asso))
rownames(Relatedness_CN) <- c("CN")
names(Relatedness_CN) <- c("Association")
Relatedness_CN$Period <- "2nd"

#then select only the top 4 areas ;
IPC_all_patents_2nd_CN_Top4 <- IPC_all_patents_2nd_CN[IPC_all_patents_2nd_CN$techn_field_nr == "6" | 
                                                        IPC_all_patents_2nd_CN$techn_field_nr == "7"| 
                                                        IPC_all_patents_2nd_CN$techn_field_nr == "10"| 
                                                        IPC_all_patents_2nd_CN$techn_field_nr == "12", ]

mat_tech_2nd_CN_Top4 <- create_sparse_matrix(i = IPC_all_patents_2nd_CN_Top4 %>% pull(appln_id),
                                             j = IPC_all_patents_2nd_CN_Top4 %>% pull(techn_field_nr))

mat_tech_2nd_CN_Top4 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_2nd_CN_Top4_rel_asso <- relatedness(mat_tech_2nd_CN_Top4, method = "association")
Relatedness_CN$Association_top4 <- mean(mat_tech_2nd_CN_Top4_rel_asso)

#then select only the top 3 areas:
IPC_all_patents_2nd_CN_Top3 <- IPC_all_patents_2nd_CN[IPC_all_patents_2nd_CN$techn_field_nr == "4" | 
                                                        IPC_all_patents_2nd_CN$techn_field_nr == "5"| 
                                                        IPC_all_patents_2nd_CN$techn_field_nr == "11", ]

mat_tech_2nd_CN_Top3 <- create_sparse_matrix(i = IPC_all_patents_2nd_CN_Top3 %>% pull(appln_id),
                                             j = IPC_all_patents_2nd_CN_Top3 %>% pull(techn_field_nr))

mat_tech_2nd_CN_Top3 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_2nd_CN_Top3_rel_asso <- relatedness(mat_tech_2nd_CN_Top3, method = "association")
Relatedness_CN$Association_Top3 <- mean(mat_tech_2nd_CN_Top3_rel_asso)

#and finally the top 7 non-AI areas:
IPC_all_patents_2nd_CN_Top7 <- IPC_all_patents_2nd_CN[IPC_all_patents_2nd_CN$techn_field_nr == "1"| 
                                                        IPC_all_patents_2nd_CN$techn_field_nr == "2"| 
                                                        IPC_all_patents_2nd_CN$techn_field_nr == "3"|
                                                        IPC_all_patents_2nd_CN$techn_field_nr == "13"| 
                                                        IPC_all_patents_2nd_CN$techn_field_nr == "25"| 
                                                        IPC_all_patents_2nd_CN$techn_field_nr == "34", ]

mat_tech_2nd_CN_Top7 <- create_sparse_matrix(i = IPC_all_patents_2nd_CN_Top7 %>% pull(appln_id),
                                             j = IPC_all_patents_2nd_CN_Top7 %>% pull(techn_field_nr))

mat_tech_2nd_CN_Top7 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_2nd_CN_Top7_rel_asso <- relatedness(mat_tech_2nd_CN_Top7, method = "association")
Relatedness_CN$Association_Top7 <- mean(mat_tech_2nd_CN_Top7_rel_asso)

#KR
mat_tech_2nd_KR <- create_sparse_matrix(i = IPC_all_patents_2nd_KR %>% pull(appln_id),
                                        j = IPC_all_patents_2nd_KR %>% pull(techn_field_nr))

mat_tech_2nd_KR %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_2nd_KR_rel_asso <- relatedness(mat_tech_2nd_KR, method = "association")
Relatedness_KR <- as.data.frame(mean(mat_tech_2nd_KR_rel_asso))

rownames(Relatedness_KR) <- c("KR")
names(Relatedness_KR) <- c("Association")
Relatedness_KR$Period <- "2nd"

#then select only the top 4 areas ;
IPC_all_patents_2nd_KR_Top4 <- IPC_all_patents_2nd_KR[IPC_all_patents_2nd_KR$techn_field_nr == "6" | 
                                                        IPC_all_patents_2nd_KR$techn_field_nr == "7"| 
                                                        IPC_all_patents_2nd_KR$techn_field_nr == "10"| 
                                                        IPC_all_patents_2nd_KR$techn_field_nr == "12", ]

mat_tech_2nd_KR_Top4 <- create_sparse_matrix(i = IPC_all_patents_2nd_KR_Top4 %>% pull(appln_id),
                                             j = IPC_all_patents_2nd_KR_Top4 %>% pull(techn_field_nr))

mat_tech_2nd_KR_Top4 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_2nd_KR_Top4_rel_asso <- relatedness(mat_tech_2nd_KR_Top4, method = "association")
Relatedness_KR$Association_top4 <- mean(mat_tech_2nd_KR_Top4_rel_asso)

#then select only the top 3 areas:
IPC_all_patents_2nd_KR_Top3 <- IPC_all_patents_2nd_KR[IPC_all_patents_2nd_KR$techn_field_nr == "4" | 
                                                        IPC_all_patents_2nd_KR$techn_field_nr == "5"| 
                                                        IPC_all_patents_2nd_KR$techn_field_nr == "11", ]

mat_tech_2nd_KR_Top3 <- create_sparse_matrix(i = IPC_all_patents_2nd_KR_Top3 %>% pull(appln_id),
                                             j = IPC_all_patents_2nd_KR_Top3 %>% pull(techn_field_nr))

mat_tech_2nd_KR_Top3 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_2nd_KR_Top3_rel_asso <- relatedness(mat_tech_2nd_KR_Top3, method = "association")
Relatedness_KR$Association_Top3 <- mean(mat_tech_2nd_KR_Top3_rel_asso)

#and finally the top 7 non-AI areas:
IPC_all_patents_2nd_KR_Top7 <- IPC_all_patents_2nd_KR[IPC_all_patents_2nd_KR$techn_field_nr == "1"| 
                                                        IPC_all_patents_2nd_KR$techn_field_nr == "2"| 
                                                        IPC_all_patents_2nd_KR$techn_field_nr == "3"|
                                                        IPC_all_patents_2nd_KR$techn_field_nr == "13"| 
                                                        IPC_all_patents_2nd_KR$techn_field_nr == "25"| 
                                                        IPC_all_patents_2nd_KR$techn_field_nr == "34", ]

mat_tech_2nd_KR_Top7 <- create_sparse_matrix(i = IPC_all_patents_2nd_KR_Top7 %>% pull(appln_id),
                                             j = IPC_all_patents_2nd_KR_Top7 %>% pull(techn_field_nr))

mat_tech_2nd_KR_Top7 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_2nd_KR_Top7_rel_asso <- relatedness(mat_tech_2nd_KR_Top7, method = "association")
Relatedness_KR$Association_Top7 <- mean(mat_tech_2nd_KR_Top7_rel_asso)

#Japan
mat_tech_2nd_JP <- create_sparse_matrix(i = IPC_all_patents_2nd_JP %>% pull(appln_id),
                                        j = IPC_all_patents_2nd_JP %>% pull(techn_field_nr))

mat_tech_2nd_JP %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_2nd_JP_rel_asso <- relatedness(mat_tech_2nd_JP, method = "association")
Relatedness_JP <- as.data.frame(mean(mat_tech_2nd_JP_rel_asso))
rownames(Relatedness_JP) <- c("JP")
names(Relatedness_JP) <- c("Association")
Relatedness_JP$Period <- "2nd"

#then select only the top 4 areas ;
IPC_all_patents_2nd_JP_Top4 <- IPC_all_patents_2nd_JP[IPC_all_patents_2nd_JP$techn_field_nr == "6" | 
                                                        IPC_all_patents_2nd_JP$techn_field_nr == "7"| 
                                                        IPC_all_patents_2nd_JP$techn_field_nr == "10"| 
                                                        IPC_all_patents_2nd_JP$techn_field_nr == "12", ]

mat_tech_2nd_JP_Top4 <- create_sparse_matrix(i = IPC_all_patents_2nd_JP_Top4 %>% pull(appln_id),
                                             j = IPC_all_patents_2nd_JP_Top4 %>% pull(techn_field_nr))

mat_tech_2nd_JP_Top4 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_2nd_JP_Top4_rel_asso <- relatedness(mat_tech_2nd_JP_Top4, method = "association")
Relatedness_JP$Association_top4 <- mean(mat_tech_2nd_JP_Top4_rel_asso)

#then select only the top 3 areas:
IPC_all_patents_2nd_JP_Top3 <- IPC_all_patents_2nd_JP[IPC_all_patents_2nd_JP$techn_field_nr == "4" | 
                                                        IPC_all_patents_2nd_JP$techn_field_nr == "5"| 
                                                        IPC_all_patents_2nd_JP$techn_field_nr == "11", ]

mat_tech_2nd_JP_Top3 <- create_sparse_matrix(i = IPC_all_patents_2nd_JP_Top3 %>% pull(appln_id),
                                             j = IPC_all_patents_2nd_JP_Top3 %>% pull(techn_field_nr))

mat_tech_2nd_JP_Top3 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_2nd_JP_Top3_rel_asso <- relatedness(mat_tech_2nd_JP_Top3, method = "association")
Relatedness_JP$Association_Top3 <- mean(mat_tech_2nd_JP_Top3_rel_asso)

#and finally the top 7 non-AI areas:
IPC_all_patents_2nd_JP_Top7 <- IPC_all_patents_2nd_JP[IPC_all_patents_2nd_JP$techn_field_nr == "1"| 
                                                        IPC_all_patents_2nd_JP$techn_field_nr == "2"| 
                                                        IPC_all_patents_2nd_JP$techn_field_nr == "3"|
                                                        IPC_all_patents_2nd_JP$techn_field_nr == "13"| 
                                                        IPC_all_patents_2nd_JP$techn_field_nr == "25"| 
                                                        IPC_all_patents_2nd_JP$techn_field_nr == "34", ]

mat_tech_2nd_JP_Top7 <- create_sparse_matrix(i = IPC_all_patents_2nd_JP_Top7 %>% pull(appln_id),
                                             j = IPC_all_patents_2nd_JP_Top7 %>% pull(techn_field_nr))

mat_tech_2nd_JP_Top7 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_2nd_JP_Top7_rel_asso <- relatedness(mat_tech_2nd_JP_Top7, method = "association")
Relatedness_JP$Association_Top7 <- mean(mat_tech_2nd_JP_Top7_rel_asso)

#AI
mat_tech_2nd_AI <- create_sparse_matrix(i = IPC_all_patents_2nd_AI %>% pull(appln_id),
                                        j = IPC_all_patents_2nd_AI %>% pull(techn_field_nr))

mat_tech_2nd_AI %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_2nd_AI_rel_asso <- relatedness(mat_tech_2nd_AI, method = "association")
Relatedness_AI <- as.data.frame(mean(mat_tech_2nd_AI_rel_asso))

rownames(Relatedness_AI) <- c("AI")
names(Relatedness_AI) <- c("Association")
Relatedness_AI$Period <- "2nd"

#then select only the top 4 areas ;
IPC_all_patents_2nd_AI_Top4 <- IPC_all_patents_2nd_AI[IPC_all_patents_2nd_AI$techn_field_nr == "6" | 
                                                        IPC_all_patents_2nd_AI$techn_field_nr == "7"| 
                                                        IPC_all_patents_2nd_AI$techn_field_nr == "10"| 
                                                        IPC_all_patents_2nd_AI$techn_field_nr == "12", ]

mat_tech_2nd_AI_Top4 <- create_sparse_matrix(i = IPC_all_patents_2nd_AI_Top4 %>% pull(appln_id),
                                             j = IPC_all_patents_2nd_AI_Top4 %>% pull(techn_field_nr))

mat_tech_2nd_AI_Top4 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_2nd_AI_Top4_rel_asso <- relatedness(mat_tech_2nd_AI_Top4, method = "association")
Relatedness_AI$Association_top4 <- mean(mat_tech_2nd_AI_Top4_rel_asso)

#then select only the top 3 areas:
IPC_all_patents_2nd_AI_Top3 <- IPC_all_patents_2nd_AI[IPC_all_patents_2nd_AI$techn_field_nr == "4" | 
                                                        IPC_all_patents_2nd_AI$techn_field_nr == "5"| 
                                                        IPC_all_patents_2nd_AI$techn_field_nr == "11", ]

mat_tech_2nd_AI_Top3 <- create_sparse_matrix(i = IPC_all_patents_2nd_AI_Top3 %>% pull(appln_id),
                                             j = IPC_all_patents_2nd_AI_Top3 %>% pull(techn_field_nr))

mat_tech_2nd_AI_Top3 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_2nd_AI_Top3_rel_asso <- relatedness(mat_tech_2nd_AI_Top3, method = "association")
Relatedness_AI$Association_Top3 <- mean(mat_tech_2nd_AI_Top3_rel_asso)

#and finally the top 7 non-AI areas:
IPC_all_patents_2nd_AI_Top7 <- IPC_all_patents_2nd_AI[IPC_all_patents_2nd_AI$techn_field_nr == "1"| 
                                                        IPC_all_patents_2nd_AI$techn_field_nr == "2"| 
                                                        IPC_all_patents_2nd_AI$techn_field_nr == "3"|
                                                        IPC_all_patents_2nd_AI$techn_field_nr == "13"| 
                                                        IPC_all_patents_2nd_AI$techn_field_nr == "25"| 
                                                        IPC_all_patents_2nd_AI$techn_field_nr == "34", ]

mat_tech_2nd_AI_Top7 <- create_sparse_matrix(i = IPC_all_patents_2nd_AI_Top7 %>% pull(appln_id),
                                             j = IPC_all_patents_2nd_AI_Top7 %>% pull(techn_field_nr))

mat_tech_2nd_AI_Top7 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_2nd_AI_Top7_rel_asso <- relatedness(mat_tech_2nd_AI_Top7, method = "association")
Relatedness_AI$Association_Top7 <- mean(mat_tech_2nd_AI_Top7_rel_asso)

#and we merge it all together:
Relatedness_SecondPeriod <- rbind(Relatedness_US, Relatedness_CN, Relatedness_KR, Relatedness_JP, Relatedness_AI)
Relatedness_SecondPeriod <- Relatedness_SecondPeriod[,c((2), (1), (3:5))]

write.csv2(Relatedness_SecondPeriod, file = "Files_created_with_the_code/data/files_code_Fields_analysis/Relatedness_2nd_period_IPC.csv", row.names = TRUE)

#2.3. Third period ----
#Starting with an empty global environment:
rm(list=ls())
setwd("large_files")
#2.3.1.Load the data we need and filter it -----
#The file for the first period is composed of 58,841,893 lines which we will read in 3 parts:
c <- 58841893 -40000000
IPC_all_patents_Part1 <- fread("All_patents_and_IPCs_Part1.csv", header = F, nrow = 20000000)[ ,c(-4)]
IPC_all_patents_Part2 <- fread("All_patents_and_IPCs_Part1.csv", header = F, nrow = 20000000, skip = 20000000)[ ,c(-4)]
IPC_all_patents_Part3 <- fread("All_patents_and_IPCs_Part1.csv", header = F, nrow = c, skip = 40000000)[ ,c(-4)]

IPC_all_patents_3rd <- rbind(IPC_all_patents_Part1, IPC_all_patents_Part2, IPC_all_patents_Part3)
rm(IPC_all_patents_Part1, IPC_all_patents_Part2, IPC_all_patents_Part3)

names(IPC_all_patents_3rd) <- c("appln_id", "ctry_code", "techn_field_nr", "priority_year")
IPC_all_patents_3rd$ctry_code2 <- IPC_all_patents_3rd$ctry_code

#read AI patents
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
patents_AI_specific <- read.csv("other_files/IPCs_AI.csv", sep = ";", header = TRUE, dec=",")
patents_AI_specific$ctry_code <- as.vector(patents_AI_specific$ctry_code)
patents_AI_specific$ctry_code <- "AI_pat"
setDT(patents_AI_specific)
setDT(IPC_all_patents_3rd)
IPC_all_patents_3rd[patents_AI_specific, on = c("appln_id"), ctry_code2 := i.ctry_code]

IPC_all_patents_3rd_US <- IPC_all_patents_3rd[IPC_all_patents_3rd$ctry_code == "US", ]
IPC_all_patents_3rd_CN <- IPC_all_patents_3rd[IPC_all_patents_3rd$ctry_code == "CN", ]
IPC_all_patents_3rd_KR <- IPC_all_patents_3rd[IPC_all_patents_3rd$ctry_code == "KR", ]
IPC_all_patents_3rd_JP <- IPC_all_patents_3rd[IPC_all_patents_3rd$ctry_code == "JP", ]
IPC_all_patents_3rd_AI <- IPC_all_patents_3rd[IPC_all_patents_3rd$ctry_code2 == "AI_pat", ]

#2.2.2. Calculate Complexity -----
IPC_all_patents_3rd_In <- IPC_all_patents_3rd[,c((-1), (-4), (-5))]
mat_3rd <- as.data.frame(table(IPC_all_patents_3rd_In$ctry_code, IPC_all_patents_3rd_In$techn_field_nr))
mat_3rd <- get.matrix(mat_3rd)

#Considering technologies in general
KnowledgeComp_3rd <- as.data.frame(MORt(mat_3rd))
KnowledgeComp_3rd$Step0 <- MORt(mat_3rd, steps = 0)
KnowledgeComp_3rd$Step1 <- MORt(mat_3rd, steps = 1)
KnowledgeComp_3rd$Step2 <- MORt(mat_3rd, steps = 2)

mat_3rd_RCAs <- location.quotient(mat_3rd, binary = T)
KnowledgeComp_3rd$RCA <- MORt(mat_3rd_RCAs)
KnowledgeComp_3rd$RCA_Step0 <- MORt(mat_3rd_RCAs, steps = 0)
KnowledgeComp_3rd$RCA_Step1 <- MORt(mat_3rd_RCAs, steps = 1)
KnowledgeComp_3rd$RCA_Step2 <- MORt(mat_3rd_RCAs, steps = 2)
write.csv2(KnowledgeComp_3rd, file = "Files_created_with_the_code/data/files_code_Fields_analysis/KnowledgeComp_3rd.csv", row.names = TRUE)

#Considering Morc 3rd period for countries
KnowledgeComp_3rd_Morc <- as.data.frame(MORc(mat_3rd, RCA = T))
KnowledgeComp_3rd_Morc$RCA <- MORc(mat_3rd, RCA = T)
KnowledgeComp_3rd_Morc$RCA_Step0 <- MORc(mat_3rd, steps = 0, RCA = T)
KnowledgeComp_3rd_Morc$RCA_Step1 <- MORc(mat_3rd, steps = 1, RCA = T)
KnowledgeComp_3rd_Morc$RCA_Step2 <- MORc(mat_3rd, steps = 2, RCA = T)

#then select only the categories:
Top4_3rd <- mat_3rd[,c(6,7,10,12)]
Top3_3rd <- mat_3rd[,c(4,5,11)]
Surrounding_3rd <- mat_3rd[,c(1,2,3,4,13,25,34)]

KnowledgeComp_3rd_Top4 <- as.data.frame(MORc(Top4_3rd, RCA = T))
KnowledgeComp_3rd_Top4$RCA <- MORc(Top4_3rd, RCA = T)
KnowledgeComp_3rd_Top4$RCA_Step0 <- MORc(Top4_3rd, steps = 0, RCA = T)
KnowledgeComp_3rd_Top4$RCA_Step1 <- MORc(Top4_3rd, steps = 1, RCA = T)
KnowledgeComp_3rd_Top4$RCA_Step2 <- MORc(Top4_3rd, steps = 2, RCA = T)

KnowledgeComp_3rd_Top3 <- as.data.frame(MORc(Top3_3rd, RCA = T))
KnowledgeComp_3rd_Top3$RCA <- MORc(Top3_3rd, RCA = T)
KnowledgeComp_3rd_Top3$RCA_Step0 <- MORc(Top3_3rd, steps = 0, RCA = T)
KnowledgeComp_3rd_Top3$RCA_Step1 <- MORc(Top3_3rd, steps = 1, RCA = T)
KnowledgeComp_3rd_Top3$RCA_Step2 <- MORc(Top3_3rd, steps = 2, RCA = T)

KnowledgeComp_3rd_Surr <- as.data.frame(MORc(Surrounding_3rd, RCA = T))
KnowledgeComp_3rd_Surr$RCA <- MORc(Surrounding_3rd, RCA = T)
KnowledgeComp_3rd_Surr$RCA_Step0 <- MORc(Surrounding_3rd, steps = 0, RCA = T)
KnowledgeComp_3rd_Surr$RCA_Step1 <- MORc(Surrounding_3rd, steps = 1, RCA = T)
KnowledgeComp_3rd_Surr$RCA_Step2 <- MORc(Surrounding_3rd, steps = 2, RCA = T)

write.csv2(KnowledgeComp_3rd_Morc, file = "Files_created_with_the_code/data/files_code_Fields_analysis/KnowledgeComp_3rd_Morc.csv", row.names = T)
write.csv2(KnowledgeComp_3rd_Top4, file = "Files_created_with_the_code/data/files_code_Fields_analysis/KnowledgeComp_3rd_Top4.csv", row.names = T)
write.csv2(KnowledgeComp_3rd_Top3, file = "Files_created_with_the_code/data/files_code_Fields_analysis/KnowledgeComp_3rd_Top3.csv", row.names = T)
write.csv2(KnowledgeComp_3rd_Surr, file = "Files_created_with_the_code/data/files_code_Fields_analysis/KnowledgeComp_3rd_Surr.csv", row.names = T)

#For AI complexity and Indicators:
IPC_all_patents_3rd_In <- IPC_all_patents_3rd[,c((-1), (-4), (-2))]
mat_3rd <- as.data.frame(table(IPC_all_patents_3rd_In$ctry_code2, IPC_all_patents_3rd_In$techn_field_nr))
mat_3rd <- get.matrix(mat_3rd)
mat_3rd_RCAs <- location.quotient(mat_3rd, binary = T)

#Considering RCAs:
KnowledgeComp_PerCountry_3rd_RCA <- as.data.frame(mat_3rd_RCAs*MORt(mat_3rd_RCAs))
KnowledgeComp_PerCountry_3rd_RCA$Step <- "NoStep"

KnowledgeComp_PerCountry_3rd_Step0_RCA <- as.data.frame(mat_3rd_RCAs*MORt(mat_3rd_RCAs, steps = 0))
KnowledgeComp_PerCountry_3rd_Step0_RCA$Step <- "Step0"

KnowledgeComp_PerCountry_3rd_Step1_RCA <- as.data.frame(mat_3rd_RCAs*MORt(mat_3rd_RCAs, steps = 1))
KnowledgeComp_PerCountry_3rd_Step1_RCA$Step <- "Step1"

KnowledgeComp_PerCountry_3rd_Step2_RCA <- as.data.frame(mat_3rd_RCAs*MORt(mat_3rd_RCAs, steps = 2))
KnowledgeComp_PerCountry_3rd_Step2_RCA$Step <- "Step2"

KnowledgeComp_PerCountry_3rd_All_RCAs <- rbind(KnowledgeComp_PerCountry_3rd_RCA, KnowledgeComp_PerCountry_3rd_Step0_RCA,
                                               KnowledgeComp_PerCountry_3rd_Step1_RCA, KnowledgeComp_PerCountry_3rd_Step2_RCA)

write.csv2(KnowledgeComp_PerCountry_3rd_All_RCAs, file = "Files_created_with_the_code/data/files_code_Fields_analysis/KnowledgeComp_PerCountry_3rd_All_RCAs_AI.csv", row.names = TRUE)

#2.2.3. Calculate Relatedness -----
#create the function we need:
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

#Now we calculate by country, starting with the US:
mat_tech_3rd_US <- create_sparse_matrix(i = IPC_all_patents_3rd_US %>% pull(appln_id),
                                        j = IPC_all_patents_3rd_US %>% pull(techn_field_nr))

mat_tech_3rd_US %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_3rd_US_rel_asso <- relatedness(mat_tech_3rd_US, method = "association")
Relatedness_US <- as.data.frame(mean(mat_tech_3rd_US_rel_asso))
rownames(Relatedness_US) <- c("US")
names(Relatedness_US) <- c("Association")
Relatedness_US$Period <- "3rd"

#then select only the top 4 areas ;
IPC_all_patents_3rd_US_Top4 <- IPC_all_patents_3rd_US[IPC_all_patents_3rd_US$techn_field_nr == "6" | 
                                                        IPC_all_patents_3rd_US$techn_field_nr == "7"| 
                                                        IPC_all_patents_3rd_US$techn_field_nr == "10"| 
                                                        IPC_all_patents_3rd_US$techn_field_nr == "12", ]

mat_tech_3rd_US_Top4 <- create_sparse_matrix(i = IPC_all_patents_3rd_US_Top4 %>% pull(appln_id),
                                             j = IPC_all_patents_3rd_US_Top4 %>% pull(techn_field_nr))

mat_tech_3rd_US_Top4 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_3rd_US_Top4_rel_asso <- relatedness(mat_tech_3rd_US_Top4, method = "association")
Relatedness_US$Association_top4 <- mean(mat_tech_3rd_US_Top4_rel_asso)

#then select only the top 3 areas:
IPC_all_patents_3rd_US_Top3 <- IPC_all_patents_3rd_US[IPC_all_patents_3rd_US$techn_field_nr == "4" | 
                                                        IPC_all_patents_3rd_US$techn_field_nr == "5"| 
                                                        IPC_all_patents_3rd_US$techn_field_nr == "11", ]

mat_tech_3rd_US_Top3 <- create_sparse_matrix(i = IPC_all_patents_3rd_US_Top3 %>% pull(appln_id),
                                             j = IPC_all_patents_3rd_US_Top3 %>% pull(techn_field_nr))

mat_tech_3rd_US_Top3 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_3rd_US_Top3_rel_asso <- relatedness(mat_tech_3rd_US_Top3, method = "association")
Relatedness_US$Association_Top3 <- mean(mat_tech_3rd_US_Top3_rel_asso)

#and finally the top 7 non-AI areas:
IPC_all_patents_3rd_US_Top7 <- IPC_all_patents_3rd_US[IPC_all_patents_3rd_US$techn_field_nr == "1"| 
                                                        IPC_all_patents_3rd_US$techn_field_nr == "2"| 
                                                        IPC_all_patents_3rd_US$techn_field_nr == "3"|
                                                        IPC_all_patents_3rd_US$techn_field_nr == "13"| 
                                                        IPC_all_patents_3rd_US$techn_field_nr == "25"| 
                                                        IPC_all_patents_3rd_US$techn_field_nr == "34", ]

mat_tech_3rd_US_Top7 <- create_sparse_matrix(i = IPC_all_patents_3rd_US_Top7 %>% pull(appln_id),
                                             j = IPC_all_patents_3rd_US_Top7 %>% pull(techn_field_nr))

mat_tech_3rd_US_Top7 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_3rd_US_Top7_rel_asso <- relatedness(mat_tech_3rd_US_Top7, method = "association")
Relatedness_US$Association_Top7 <- mean(mat_tech_3rd_US_Top7_rel_asso)

#China:
mat_tech_3rd_CN <- create_sparse_matrix(i = IPC_all_patents_3rd_CN %>% pull(appln_id),
                                        j = IPC_all_patents_3rd_CN %>% pull(techn_field_nr))

mat_tech_3rd_CN %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_3rd_CN_rel_asso <- relatedness(mat_tech_3rd_CN, method = "association")
Relatedness_CN <- as.data.frame(mean(mat_tech_3rd_CN_rel_asso))

rownames(Relatedness_CN) <- c("CN")
names(Relatedness_CN) <- c("Association")
Relatedness_CN$Period <- "3rd"

#then select only the top 4 areas. But first, the global environment is already too full for the calculations.
#Let's clean it:
rm(IPC_all_patents_3rd, IPC_all_patents_3rd_US)
IPC_all_patents_3rd_CN_Top4 <- IPC_all_patents_3rd_CN[IPC_all_patents_3rd_CN$techn_field_nr == "6" | 
                                                        IPC_all_patents_3rd_CN$techn_field_nr == "7"| 
                                                        IPC_all_patents_3rd_CN$techn_field_nr == "10"| 
                                                        IPC_all_patents_3rd_CN$techn_field_nr == "12", ]

mat_tech_3rd_CN_Top4 <- create_sparse_matrix(i = IPC_all_patents_3rd_CN_Top4 %>% pull(appln_id),
                                             j = IPC_all_patents_3rd_CN_Top4 %>% pull(techn_field_nr))

mat_tech_3rd_CN_Top4 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_3rd_CN_Top4_rel_asso <- relatedness(mat_tech_3rd_CN_Top4, method = "association")
Relatedness_CN$Association_top4 <- mean(mat_tech_3rd_CN_Top4_rel_asso)

#then select only the top 3 areas:
IPC_all_patents_3rd_CN_Top3 <- IPC_all_patents_3rd_CN[IPC_all_patents_3rd_CN$techn_field_nr == "4" | 
                                                        IPC_all_patents_3rd_CN$techn_field_nr == "5"| 
                                                        IPC_all_patents_3rd_CN$techn_field_nr == "11", ]

mat_tech_3rd_CN_Top3 <- create_sparse_matrix(i = IPC_all_patents_3rd_CN_Top3 %>% pull(appln_id),
                                             j = IPC_all_patents_3rd_CN_Top3 %>% pull(techn_field_nr))

mat_tech_3rd_CN_Top3 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_3rd_CN_Top3_rel_asso <- relatedness(mat_tech_3rd_CN_Top3, method = "association")
Relatedness_CN$Association_Top3 <- mean(mat_tech_3rd_CN_Top3_rel_asso)

#and finally the top 7 non-AI areas:
rm(IPC_all_patents_3rd_CN_Top4, IPC_all_patents_3rd_CN_Top3, IPC_all_patents_3rd_US_Top3, 
   IPC_all_patents_3rd_US_Top7, IPC_all_patents_3rd_US_Top4)

IPC_all_patents_3rd_CN_Top7 <- IPC_all_patents_3rd_CN[IPC_all_patents_3rd_CN$techn_field_nr == "1"| 
                                                        IPC_all_patents_3rd_CN$techn_field_nr == "2"| 
                                                        IPC_all_patents_3rd_CN$techn_field_nr == "3"|
                                                        IPC_all_patents_3rd_CN$techn_field_nr == "13"| 
                                                        IPC_all_patents_3rd_CN$techn_field_nr == "25"| 
                                                        IPC_all_patents_3rd_CN$techn_field_nr == "34", ]

mat_tech_3rd_CN_Top7 <- create_sparse_matrix(i = IPC_all_patents_3rd_CN_Top7 %>% pull(appln_id),
                                             j = IPC_all_patents_3rd_CN_Top7 %>% pull(techn_field_nr))

mat_tech_3rd_CN_Top7 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_3rd_CN_Top7_rel_asso <- relatedness(mat_tech_3rd_CN_Top7, method = "association")
Relatedness_CN$Association_Top7 <- mean(mat_tech_3rd_CN_Top7_rel_asso)

#KR
rm(IPC_all_patents_3rd_CN)
mat_tech_3rd_KR <- create_sparse_matrix(i = IPC_all_patents_3rd_KR %>% pull(appln_id),
                                        j = IPC_all_patents_3rd_KR %>% pull(techn_field_nr))

mat_tech_3rd_KR %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_3rd_KR_rel_asso <- relatedness(mat_tech_3rd_KR, method = "association")
Relatedness_KR <- as.data.frame(mean(mat_tech_3rd_KR_rel_asso))

rownames(Relatedness_KR) <- c("KR")
names(Relatedness_KR) <- c("Association")
Relatedness_KR$Period <- "3rd"

#then select only the top 4 areas ;
IPC_all_patents_3rd_KR_Top4 <- IPC_all_patents_3rd_KR[IPC_all_patents_3rd_KR$techn_field_nr == "6" | 
                                                        IPC_all_patents_3rd_KR$techn_field_nr == "7"| 
                                                        IPC_all_patents_3rd_KR$techn_field_nr == "10"| 
                                                        IPC_all_patents_3rd_KR$techn_field_nr == "12", ]

mat_tech_3rd_KR_Top4 <- create_sparse_matrix(i = IPC_all_patents_3rd_KR_Top4 %>% pull(appln_id),
                                             j = IPC_all_patents_3rd_KR_Top4 %>% pull(techn_field_nr))

mat_tech_3rd_KR_Top4 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_3rd_KR_Top4_rel_asso <- relatedness(mat_tech_3rd_KR_Top4, method = "association")
Relatedness_KR$Association_top4 <- mean(mat_tech_3rd_KR_Top4_rel_asso)

#then select only the top 3 areas:
IPC_all_patents_3rd_KR_Top3 <- IPC_all_patents_3rd_KR[IPC_all_patents_3rd_KR$techn_field_nr == "4" | 
                                                        IPC_all_patents_3rd_KR$techn_field_nr == "5"| 
                                                        IPC_all_patents_3rd_KR$techn_field_nr == "11", ]

mat_tech_3rd_KR_Top3 <- create_sparse_matrix(i = IPC_all_patents_3rd_KR_Top3 %>% pull(appln_id),
                                             j = IPC_all_patents_3rd_KR_Top3 %>% pull(techn_field_nr))

mat_tech_3rd_KR_Top3 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_3rd_KR_Top3_rel_asso <- relatedness(mat_tech_3rd_KR_Top3, method = "association")
Relatedness_KR$Association_Top3 <- mean(mat_tech_3rd_KR_Top3_rel_asso)

#and finally the top 7 non-AI areas:
IPC_all_patents_3rd_KR_Top7 <- IPC_all_patents_3rd_KR[IPC_all_patents_3rd_KR$techn_field_nr == "1"| 
                                                        IPC_all_patents_3rd_KR$techn_field_nr == "2"| 
                                                        IPC_all_patents_3rd_KR$techn_field_nr == "3"|
                                                        IPC_all_patents_3rd_KR$techn_field_nr == "13"| 
                                                        IPC_all_patents_3rd_KR$techn_field_nr == "25"| 
                                                        IPC_all_patents_3rd_KR$techn_field_nr == "34", ]

mat_tech_3rd_KR_Top7 <- create_sparse_matrix(i = IPC_all_patents_3rd_KR_Top7 %>% pull(appln_id),
                                             j = IPC_all_patents_3rd_KR_Top7 %>% pull(techn_field_nr))

mat_tech_3rd_KR_Top7 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_3rd_KR_Top7_rel_asso <- relatedness(mat_tech_3rd_KR_Top7, method = "association")
Relatedness_KR$Association_Top7 <- mean(mat_tech_3rd_KR_Top7_rel_asso)

#Japan
mat_tech_3rd_JP <- create_sparse_matrix(i = IPC_all_patents_3rd_JP %>% pull(appln_id),
                                        j = IPC_all_patents_3rd_JP %>% pull(techn_field_nr))

mat_tech_3rd_JP %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_3rd_JP_rel_asso <- relatedness(mat_tech_3rd_JP, method = "association")
Relatedness_JP <- as.data.frame(mean(mat_tech_3rd_JP_rel_asso))

rownames(Relatedness_JP) <- c("JP")
names(Relatedness_JP) <- c("Association")
Relatedness_JP$Period <- "3rd"

#then select only the top 4 areas ;
IPC_all_patents_3rd_JP_Top4 <- IPC_all_patents_3rd_JP[IPC_all_patents_3rd_JP$techn_field_nr == "6" | 
                                                        IPC_all_patents_3rd_JP$techn_field_nr == "7"| 
                                                        IPC_all_patents_3rd_JP$techn_field_nr == "10"| 
                                                        IPC_all_patents_3rd_JP$techn_field_nr == "12", ]

mat_tech_3rd_JP_Top4 <- create_sparse_matrix(i = IPC_all_patents_3rd_JP_Top4 %>% pull(appln_id),
                                             j = IPC_all_patents_3rd_JP_Top4 %>% pull(techn_field_nr))

mat_tech_3rd_JP_Top4 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_3rd_JP_Top4_rel_asso <- relatedness(mat_tech_3rd_JP_Top4, method = "association")
Relatedness_JP$Association_top4 <- mean(mat_tech_3rd_JP_Top4_rel_asso)

#then select only the top 3 areas:
IPC_all_patents_3rd_JP_Top3 <- IPC_all_patents_3rd_JP[IPC_all_patents_3rd_JP$techn_field_nr == "4" | 
                                                        IPC_all_patents_3rd_JP$techn_field_nr == "5"| 
                                                        IPC_all_patents_3rd_JP$techn_field_nr == "11", ]

mat_tech_3rd_JP_Top3 <- create_sparse_matrix(i = IPC_all_patents_3rd_JP_Top3 %>% pull(appln_id),
                                             j = IPC_all_patents_3rd_JP_Top3 %>% pull(techn_field_nr))

mat_tech_3rd_JP_Top3 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_3rd_JP_Top3_rel_asso <- relatedness(mat_tech_3rd_JP_Top3, method = "association")
Relatedness_JP$Association_Top3 <- mean(mat_tech_3rd_JP_Top3_rel_asso)

#and finally the top 7 non-AI areas:
IPC_all_patents_3rd_JP_Top7 <- IPC_all_patents_3rd_JP[IPC_all_patents_3rd_JP$techn_field_nr == "1"| 
                                                        IPC_all_patents_3rd_JP$techn_field_nr == "2"| 
                                                        IPC_all_patents_3rd_JP$techn_field_nr == "3"|
                                                        IPC_all_patents_3rd_JP$techn_field_nr == "13"| 
                                                        IPC_all_patents_3rd_JP$techn_field_nr == "25"| 
                                                        IPC_all_patents_3rd_JP$techn_field_nr == "34", ]

mat_tech_3rd_JP_Top7 <- create_sparse_matrix(i = IPC_all_patents_3rd_JP_Top7 %>% pull(appln_id),
                                             j = IPC_all_patents_3rd_JP_Top7 %>% pull(techn_field_nr))

mat_tech_3rd_JP_Top7 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_3rd_JP_Top7_rel_asso <- relatedness(mat_tech_3rd_JP_Top7, method = "association")
Relatedness_JP$Association_Top7 <- mean(mat_tech_3rd_JP_Top7_rel_asso)

#AI
mat_tech_3rd_AI <- create_sparse_matrix(i = IPC_all_patents_3rd_AI %>% pull(appln_id),
                                        j = IPC_all_patents_3rd_AI %>% pull(techn_field_nr))

mat_tech_3rd_AI %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_3rd_AI_rel_asso <- relatedness(mat_tech_3rd_AI, method = "association")
Relatedness_AI <- as.data.frame(mean(mat_tech_3rd_AI_rel_asso))

rownames(Relatedness_AI) <- c("AI")
names(Relatedness_AI) <- c("Association")
Relatedness_AI$Period <- "3rd"

#then select only the top 4 areas ;
IPC_all_patents_3rd_AI_Top4 <- IPC_all_patents_3rd_AI[IPC_all_patents_3rd_AI$techn_field_nr == "6" | 
                                                        IPC_all_patents_3rd_AI$techn_field_nr == "7"| 
                                                        IPC_all_patents_3rd_AI$techn_field_nr == "10"| 
                                                        IPC_all_patents_3rd_AI$techn_field_nr == "12", ]

mat_tech_3rd_AI_Top4 <- create_sparse_matrix(i = IPC_all_patents_3rd_AI_Top4 %>% pull(appln_id),
                                             j = IPC_all_patents_3rd_AI_Top4 %>% pull(techn_field_nr))

mat_tech_3rd_AI_Top4 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_3rd_AI_Top4_rel_asso <- relatedness(mat_tech_3rd_AI_Top4, method = "association")
Relatedness_AI$Association_top4 <- mean(mat_tech_3rd_AI_Top4_rel_asso)

#then select only the top 3 areas:
IPC_all_patents_3rd_AI_Top3 <- IPC_all_patents_3rd_AI[IPC_all_patents_3rd_AI$techn_field_nr == "4" | 
                                                        IPC_all_patents_3rd_AI$techn_field_nr == "5"| 
                                                        IPC_all_patents_3rd_AI$techn_field_nr == "11", ]

mat_tech_3rd_AI_Top3 <- create_sparse_matrix(i = IPC_all_patents_3rd_AI_Top3 %>% pull(appln_id),
                                             j = IPC_all_patents_3rd_AI_Top3 %>% pull(techn_field_nr))

mat_tech_3rd_AI_Top3 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_3rd_AI_Top3_rel_asso <- relatedness(mat_tech_3rd_AI_Top3, method = "association")
Relatedness_AI$Association_Top3 <- mean(mat_tech_3rd_AI_Top3_rel_asso)

#and finally the top 7 non-AI areas:
IPC_all_patents_3rd_AI_Top7 <- IPC_all_patents_3rd_AI[IPC_all_patents_3rd_AI$techn_field_nr == "1"| 
                                                        IPC_all_patents_3rd_AI$techn_field_nr == "2"| 
                                                        IPC_all_patents_3rd_AI$techn_field_nr == "3"|
                                                        IPC_all_patents_3rd_AI$techn_field_nr == "13"| 
                                                        IPC_all_patents_3rd_AI$techn_field_nr == "25"| 
                                                        IPC_all_patents_3rd_AI$techn_field_nr == "34", ]

mat_tech_3rd_AI_Top7 <- create_sparse_matrix(i = IPC_all_patents_3rd_AI_Top7 %>% pull(appln_id),
                                             j = IPC_all_patents_3rd_AI_Top7 %>% pull(techn_field_nr))

mat_tech_3rd_AI_Top7 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_3rd_AI_Top7_rel_asso <- relatedness(mat_tech_3rd_AI_Top7, method = "association")
Relatedness_AI$Association_Top7 <- mean(mat_tech_3rd_AI_Top7_rel_asso)

#and we merge it all together:
Relatedness_ThirdPeriod <- rbind(Relatedness_US, Relatedness_CN, Relatedness_KR, Relatedness_JP, Relatedness_AI)
Relatedness_ThirdPeriod <- Relatedness_ThirdPeriod[,c((2), (1), (3:5))]

write.csv2(Relatedness_ThirdPeriod, file = "Files_created_with_the_code/data/files_code_Fields_analysis/Relatedness_3rd_period_IPC.csv", row.names = TRUE)

#2.4. Visualization ----
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

#2.4.1.Relatedness ----
Relatedness_1st <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/Relatedness_1st_period_IPC.csv", sep = ";", header = TRUE, dec=",")
Relatedness_2nd <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/Relatedness_2nd_period_IPC.csv", sep = ";", header = TRUE, dec=",")
Relatedness_3rd <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/Relatedness_3rd_period_IPC.csv", sep = ";", header = TRUE, dec=",")
Relatedness <- rbind(Relatedness_1st, Relatedness_2nd, Relatedness_3rd)
rm(Relatedness_1st, Relatedness_2nd, Relatedness_3rd)

Relatedness$Period <- gsub("1st", "Period 1 (1974-1988)", str_trim(Relatedness$Period))
Relatedness$Period <- gsub("2nd", "Period 2 (1989-2003)", str_trim(Relatedness$Period))
Relatedness$Period <- gsub("3rd", "Period 3 (2004-2018)", str_trim(Relatedness$Period))

Relatedness1 <- Relatedness[,c(1,2,3)]
names(Relatedness1) <- c("Country", "Period", "Value")
Relatedness1$Indicator <- "Overall Relatedness"

Relatedness2 <- Relatedness[,c(1,2,4)]
names(Relatedness2) <- c("Country", "Period", "Value")
Relatedness2$Indicator <- "AI-core fields"

Relatedness3 <- Relatedness[,c(1,2,5)]
names(Relatedness3) <- c("Country", "Period", "Value")
Relatedness3$Indicator <- "AI-related fields"

Relatedness4 <- Relatedness[,c(1,2,6)]
names(Relatedness4) <- c("Country", "Period", "Value")
Relatedness4$Indicator <- "Surrounding fields"

Relatedness <- rbind(Relatedness1, Relatedness2, Relatedness3, Relatedness4)
rm(Relatedness1, Relatedness2, Relatedness3, Relatedness4)

Relatedness$Indicator <- factor(Relatedness$Indicator, levels = c("Overall Relatedness", "AI-core fields",
                                                                    "AI-related fields", "Surrounding fields"))
Relatedness_AI <- Relatedness[Relatedness$Country == "AI", ]
Relatedness <- Relatedness[Relatedness$Country != "AI", ]

#coloured figures
library(RColorBrewer)
Rel_byP_c_Colour <- 
ggplot(Relatedness, aes(x=Country, y=Value, fill=Period)) +
  geom_bar(stat="identity", position=position_dodge(), show.legend = F)+theme_minimal()+ xlab(NULL) + ylab("Relatedness") +
  facet_wrap(~Indicator, ncol = 4) + theme_classic() +
  ggtitle("Countries Relatedness in the considered IPC fields") + 
  scale_y_continuous(limits=c(.45,1.5),oob = rescale_none) + 
  scale_fill_brewer(palette = "YlOrRd")

Rel_byAI_c_Colour<- ggplot(Relatedness_AI, aes(x=Country, y=Value, fill=Period)) +
  geom_bar(stat="identity", position=position_dodge(), show.legend = F)+theme_minimal() + labs(x = "") +
  scale_x_discrete(labels = NULL) + ylab("Relatedness") +
  facet_wrap(~Indicator, ncol = 4)  + theme_classic() +
  ggtitle("AI Relatedness in the considered IPC fields")+
  scale_y_continuous(limits=c(.1,2.35),oob = rescale_none) + 
  scale_fill_brewer(palette = "YlOrRd")


#2.4.2.Knowld Comp. AI -----
KnowlComp_1st_AI <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/KnowledgeComp_PerCountry_1st_All_RCAs_AI.csv", sep = ";", header = TRUE, dec=",")
KnowlComp_2nd_AI <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/KnowledgeComp_PerCountry_2nd_All_RCAs_AI.csv", sep = ";", header = TRUE, dec=",")
KnowlComp_3rd_AI <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/KnowledgeComp_PerCountry_3rd_All_RCAs_AI.csv", sep = ";", header = TRUE, dec=",")

KnowlComp_1st_AI$Period <- "Period 1 (1974-1988)"
KnowlComp_2nd_AI$Period <- "Period 2 (1989-2003)"
KnowlComp_3rd_AI$Period <- "Period 3 (2004-2018)"
KnowledgeCompl_AI <- rbind(KnowlComp_1st_AI, KnowlComp_2nd_AI, KnowlComp_3rd_AI)
rm(KnowlComp_1st_AI, KnowlComp_2nd_AI, KnowlComp_3rd_AI)
KnowledgeCompl_AI <- KnowledgeCompl_AI[KnowledgeCompl_AI$X == "AI_pat3", ]
KnowledgeCompl_AI$"AI-core fields" = rowSums(KnowledgeCompl_AI[,c("X6", "X7", "X10", "X12")])
KnowledgeCompl_AI$"Overall Complexity" = rowSums(KnowledgeCompl_AI[,c(2:36)])
KnowledgeCompl_AI$"AI-related fields" = rowSums(KnowledgeCompl_AI[,c("X11", "X4", "X5")])
KnowledgeCompl_AI$"Surrounding fields" = rowSums(KnowledgeCompl_AI[,c("X3", "X2", "X1", "X13", "X25", "X34")])

KnowledgeCompl_AI2<- KnowledgeCompl_AI[,c(1, 38, 39)]
names(KnowledgeCompl_AI2) <- c("Country", "Period", "Value")
KnowledgeCompl_AI2$Indicator <- "AI-core fields"

KnowledgeCompl_AI3<- KnowledgeCompl_AI[,c(1, 38, 40)]
names(KnowledgeCompl_AI3) <- c("Country", "Period", "Value")
KnowledgeCompl_AI3$Indicator <- "Overall Complexity"

KnowledgeCompl_AI4<- KnowledgeCompl_AI[,c(1, 38, 41)]
names(KnowledgeCompl_AI4) <- c("Country", "Period", "Value")
KnowledgeCompl_AI4$Indicator <- "AI-related fields"

KnowledgeCompl_AI5<- KnowledgeCompl_AI[,c(1, 38, 42)]
names(KnowledgeCompl_AI5) <- c("Country", "Period", "Value")
KnowledgeCompl_AI5$Indicator <- "Surrounding fields"

KnowledgeCompl_AI_all <- rbind(KnowledgeCompl_AI2, KnowledgeCompl_AI3, KnowledgeCompl_AI4, KnowledgeCompl_AI5)
KnowledgeCompl_AI_all$Country <- gsub("AI_pat3", "AI", str_trim(KnowledgeCompl_AI_all$Country))
KnowledgeCompl_AI_all$Indicator <- factor(KnowledgeCompl_AI_all$Indicator, levels = c("Overall Complexity", "AI-core fields",
                                                                                        "AI-related fields", "Surrounding fields"))
Comp_byAI_c_Colour<- 
  ggplot(KnowledgeCompl_AI_all, aes(x=Country, y=Value, fill=Period)) +
  geom_bar(stat="identity", position=position_dodge())+theme_minimal() + labs(x = "") +
  scale_x_discrete(labels = NULL) + ylab("Knowledge Complexity (MORt)") +
  facet_wrap(~Indicator, ncol = 4) +
  theme_classic() + theme(legend.position="bottom") +
  ggtitle("AI Knowledge Complexity in the considered IPC fields")+ 
  scale_fill_brewer(palette = "YlOrRd")

#2.4.3.Knowld Comp. Countries -----
KnowlComp_1st <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/KnowledgeComp_1st_Morc.csv", sep = ";", header = F, dec=".")
KnowlComp_2nd <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/KnowledgeComp_2nd_Morc.csv", sep = ";", header = TRUE, dec=".")
KnowlComp_3rd <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/KnowledgeComp_3rd_Morc.csv", sep = ";", header = TRUE, dec=".")

names(KnowlComp_1st) <- c("Country", "MORc", "RCA", "RCA_step0", "RCA_step1", "RCA_step2")
names(KnowlComp_2nd) <- c("Country", "MORc", "RCA", "RCA_step0", "RCA_step1", "RCA_step2")
names(KnowlComp_3rd) <- c("Country", "MORc", "RCA", "RCA_step0", "RCA_step1", "RCA_step2")

KnowlComp_1st$Period <- "Period 1 (1974-1988)"
KnowlComp_2nd$Period <- "Period 2 (1989-2003)"
KnowlComp_3rd$Period <- "Period 3 (2004-2018)"

KnowledgeCompl <- rbind(KnowlComp_1st, KnowlComp_2nd, KnowlComp_3rd)
rm(KnowlComp_1st, KnowlComp_2nd, KnowlComp_3rd)
KnowledgeCompl$Category <- "Overall Complexity"

KnowlComp_1st_Top4 <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/KnowledgeComp_1st_Top4.csv", sep = ";", header = F, dec=".")
KnowlComp_2nd_Top4 <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/KnowledgeComp_2nd_Top4.csv", sep = ";", header = TRUE, dec=".")
KnowlComp_3rd_Top4 <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/KnowledgeComp_3rd_Top4.csv", sep = ";", header = TRUE, dec=".")

names(KnowlComp_1st_Top4) <- c("Country", "MORc", "RCA", "RCA_step0", "RCA_step1", "RCA_step2")
names(KnowlComp_2nd_Top4) <- c("Country", "MORc", "RCA", "RCA_step0", "RCA_step1", "RCA_step2")
names(KnowlComp_3rd_Top4) <- c("Country", "MORc", "RCA", "RCA_step0", "RCA_step1", "RCA_step2")

KnowlComp_1st_Top4$Period <- "Period 1 (1974-1988)"
KnowlComp_2nd_Top4$Period <- "Period 2 (1989-2003)"
KnowlComp_3rd_Top4$Period <- "Period 3 (2004-2018)"

KnowledgeCompl_Top4 <- rbind(KnowlComp_1st_Top4, KnowlComp_2nd_Top4, KnowlComp_3rd_Top4)
rm(KnowlComp_1st_Top4, KnowlComp_2nd_Top4, KnowlComp_3rd_Top4)
KnowledgeCompl_Top4$Category <- "AI-core fields"

KnowlComp_1st_Top3 <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/KnowledgeComp_1st_Top3.csv", sep = ";", header = F, dec=".")
KnowlComp_2nd_Top3 <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/KnowledgeComp_2nd_Top3.csv", sep = ";", header = TRUE, dec=".")
KnowlComp_3rd_Top3 <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/KnowledgeComp_3rd_Top3.csv", sep = ";", header = TRUE, dec=".")

names(KnowlComp_1st_Top3) <- c("Country", "MORc", "RCA", "RCA_step0", "RCA_step1", "RCA_step2")
names(KnowlComp_2nd_Top3) <- c("Country", "MORc", "RCA", "RCA_step0", "RCA_step1", "RCA_step2")
names(KnowlComp_3rd_Top3) <- c("Country", "MORc", "RCA", "RCA_step0", "RCA_step1", "RCA_step2")

KnowlComp_1st_Top3$Period <- "Period 1 (1974-1988)"
KnowlComp_2nd_Top3$Period <- "Period 2 (1989-2003)"
KnowlComp_3rd_Top3$Period <- "Period 3 (2004-2018)"

KnowledgeCompl_Top3 <- rbind(KnowlComp_1st_Top3, KnowlComp_2nd_Top3, KnowlComp_3rd_Top3)
rm(KnowlComp_1st_Top3, KnowlComp_2nd_Top3, KnowlComp_3rd_Top3)
KnowledgeCompl_Top3$Category <- "AI-related fields"

KnowlComp_1st_Surr <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/KnowledgeComp_1st_Surr.csv", sep = ";", header = F, dec=".")
KnowlComp_2nd_Surr <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/KnowledgeComp_2nd_Surr.csv", sep = ";", header = TRUE, dec=".")
KnowlComp_3rd_Surr <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/KnowledgeComp_3rd_Surr.csv", sep = ";", header = TRUE, dec=".")

names(KnowlComp_1st_Surr) <- c("Country", "MORc", "RCA", "RCA_step0", "RCA_step1", "RCA_step2")
names(KnowlComp_2nd_Surr) <- c("Country", "MORc", "RCA", "RCA_step0", "RCA_step1", "RCA_step2")
names(KnowlComp_3rd_Surr) <- c("Country", "MORc", "RCA", "RCA_step0", "RCA_step1", "RCA_step2")

KnowlComp_1st_Surr$Period <- "Period 1 (1974-1988)"
KnowlComp_2nd_Surr$Period <- "Period 2 (1989-2003)"
KnowlComp_3rd_Surr$Period <- "Period 3 (2004-2018)"

KnowledgeCompl_Surr <- rbind(KnowlComp_1st_Surr, KnowlComp_2nd_Surr, KnowlComp_3rd_Surr)
rm(KnowlComp_1st_Surr, KnowlComp_2nd_Surr, KnowlComp_3rd_Surr)
KnowledgeCompl_Surr$Category <- "Surrounding fields"

All_data_knowlComp_Morc <- rbind(KnowledgeCompl, KnowledgeCompl_Top4, KnowledgeCompl_Top3, KnowledgeCompl_Surr)
rm(KnowledgeCompl, KnowledgeCompl_Top4, KnowledgeCompl_Top3, KnowledgeCompl_Surr)
All_data_knowlComp_Morc<-All_data_knowlComp_Morc[All_data_knowlComp_Morc$Country == "US"|
                                                   All_data_knowlComp_Morc$Country == "CN"|
                                                   All_data_knowlComp_Morc$Country == "KR"|
                                                   All_data_knowlComp_Morc$Country == "JP",]

write.csv2(All_data_knowlComp_Morc, file = "Files_created_with_the_code/data/files_code_Fields_analysis/All_data_knowlComp_Morc.csv", row.names = F)
All_data_knowlComp_Morc <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/All_data_knowlComp_Morc.csv", sep = ";", header = TRUE, dec=",")
All_data_knowlComp_Morc$Category <- factor(All_data_knowlComp_Morc$Category, levels = c("Overall Complexity", "AI-core fields",
                                                                        "AI-related fields", "Surrounding fields"))
#figure colour:
Comp_byP_c_Colour <-
  ggplot(All_data_knowlComp_Morc, aes(x=Country, y=RCA_step1, fill=Period)) +
  geom_bar(stat="identity", position=position_dodge())+theme_minimal()+ xlab(NULL) + ylab("Knowledge Complexity (MORc)") +
  facet_wrap(~Category, ncol = 4) +
  theme_classic()  + theme(legend.position="bottom") +
  ggtitle("Countries Knowledge Complexity in the considered IPC fields") + 
  scale_fill_brewer(palette = "YlOrRd")

#regular resolution:
jpeg("Files_created_with_the_code/figures/regular_resolution_ALL_FIGURES_ARE_HERE/Fig3_Relatedness_and_Complex_AICol.jpg", width = 8, height = 6, units = 'in', res = 300)
multiplot(Rel_byAI_c_Colour, Comp_byAI_c_Colour, cols=1) 
dev.off()

#high resolution
jpeg("Files_created_with_the_code/figures/high_resolution/Fig3_Relatedness_and_Complex_AIColHigh.jpg", width = 8, height = 6, units = 'in', res = 800)
multiplot(Rel_byAI_c_Colour, Comp_byAI_c_Colour, cols=1) 
dev.off()

#low resolution
jpeg("Files_created_with_the_code/figures/low_resolution/Fig3_Relatedness_and_Complex_AIColLow.jpg", width = 8, height = 6, units = 'in', res = 72)
multiplot(Rel_byAI_c_Colour, Comp_byAI_c_Colour, cols=1) 
dev.off()

#regular resolution
jpeg("Files_created_with_the_code/figures/regular_resolution_ALL_FIGURES_ARE_HERE/Fig7_Relatedness_and_Complex_Morc_countriesColour.jpg", width = 8, height = 6, units = 'in', res = 300)
multiplot(Rel_byP_c_Colour, Comp_byP_c_Colour, cols=1) 
dev.off()

#high resolution
jpeg("Files_created_with_the_code/figures/high_resolution/Fig7_Relatedness_and_Complex_Morc_countriesColourHigh.jpg", width = 8, height = 6, units = 'in', res = 800)
multiplot(Rel_byP_c_Colour, Comp_byP_c_Colour, cols=1) 
dev.off()

#low resolution
jpeg("Files_created_with_the_code/figures/low_resolution/Fig7_Relatedness_and_Complex_Morc_countriesColourLow.jpg", width = 8, height = 6, units = 'in', res = 72)
multiplot(Rel_byP_c_Colour, Comp_byP_c_Colour, cols=1) 
dev.off()

#2.5.FigureS Discussions (8 and 9) ----
#scatter plot using data from All_data_knowlComp_Morc and Relatedness
All_data_knowlComp_Morc2 <- All_data_knowlComp_Morc[,c((1),(5),(7:8))]
All_data_knowlComp_Morc2$Category <- gsub("Complexity", "Relatedness", str_trim(All_data_knowlComp_Morc2$Category))
colnames(All_data_knowlComp_Morc2)[grepl("Category", colnames(All_data_knowlComp_Morc2))] <- "Indicator"
Test2 <- left_join(All_data_knowlComp_Morc2, Relatedness, by=c("Country", "Period", "Indicator"))

Test3 <- Test2[Test2$Indicator == "Overall Relatedness",]

Overall_colour<- 
  Test3 %>%
  ggplot(aes(x=RCA_step1, y=Value, color=Period, shape = Country)) +
  geom_point(size=10) + 
  geom_path(color="black", linetype = "dashed", arrow = arrow(angle = 15, type = "closed"), size=1) +
  scale_shape_manual(values=c(16, 15, 17, 18)) +
  xlab("Knowledge complexity (MORc)") +
  ylab("Relatedness") +
  ggtitle("Technological development of AI-leading countries - Overall") +
  theme_classic() + #theme(legend.position="bottom") +
  geom_node_text(aes(x=RCA_step1,y=Value, label = Country), size = 8, repel = TRUE) + 
  scale_color_brewer(palette = "YlOrRd")

#for AI:
KnowledgeCompl_AI_all2 <- KnowledgeCompl_AI_all
KnowledgeCompl_AI_all2$Indicator <- gsub("Complexity", "Relatedness", str_trim(KnowledgeCompl_AI_all2$Indicator))
AI_data <- left_join(KnowledgeCompl_AI_all2, Relatedness_AI, by=c("Country", "Period", "Indicator"))
AI_data$Indicator <- gsub("Relatedness", " ", str_trim(AI_data$Indicator))
names(AI_data) <-c("Country","Period","Value.x","Category","Value.y")

AI_fig_colour<- 
  AI_data %>%
  ggplot(aes(x=(Value.x), y=(Value.y), color=Period, shape = Category)) +
  geom_point(size=10) + 
  geom_path(color="black", linetype = "dashed", arrow = arrow(angle = 15, type = "closed"), size=1) +
  scale_shape_manual(values=c(16, 15, 17, 18)) +
  xlab("Knowledge complexity (MORt)") +
  ylab("Relatedness") +
  ggtitle("Technological development of AI in the considered categories") +
  theme_classic() + scale_color_brewer(palette = "YlOrRd") #+ theme(legend.position="bottom")

#colour
jpeg("Files_created_with_the_code/figures/regular_resolution_ALL_FIGURES_ARE_HERE/Fig8_Overall_AI.jpg", width = 12, height = 5, units = 'in', res = 300)
AI_fig_colour
dev.off()

#colour high
jpeg("Files_created_with_the_code/figures/high_resolution/Fig8_Overall_AIhigh.jpg", width = 12, height = 5, units = 'in', res = 800)
AI_fig_colour
dev.off()

#colour low
jpeg("Files_created_with_the_code/figures/low_resolution/Fig8_Overall_AIlow.jpg", width = 12, height = 5, units = 'in', res = 72)
AI_fig_colour
dev.off()

#regular resol
jpeg("Files_created_with_the_code/figures/regular_resolution_ALL_FIGURES_ARE_HERE/Fig9_Overall_countries.jpg", width = 12, height = 5, units = 'in', res = 300)
Overall_colour
dev.off()

#colour high
jpeg("Files_created_with_the_code/figures/high_resolution/Fig9_Overall_countriesHigh.jpg", width = 12, height = 5, units = 'in', res = 800)
Overall_colour
dev.off()

#colour low
jpeg("Files_created_with_the_code/figures/low_resolution/Fig9_Overall_countriesLow.jpg", width = 12, height = 5, units = 'in', res = 72)
Overall_colour
dev.off()

rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
patents_AI_specific <- read.csv("other_files/IPCs_AI.csv", sep = ";", header = TRUE, dec=",")
patents_AI_specific <- patents_AI_specific[,c((1), (3:4))]

length(unique(patents_AI_specific$appln_id))

patents_AI_specific %<>% 
  #group_by(Publication_number) %>% 
  mutate(DistinctOwnerInf = !duplicated(appln_id)) %>%
  ungroup()

patents_AI_specific %<>% 
  group_by(appln_id) %>% 
  mutate(DistinctpatentOffice = !duplicated(patent_office)) %>%
  ungroup()

patents_AI_specific %<>% 
  group_by(appln_id) %>% 
  mutate(DistinctpatentOffice = n_distinct(patent_office, na.rm = T)) %>%
  ungroup()

table(patents_AI_specific$DistinctpatentOffice)
test<- patents_AI_specific[1,]

test$patent_office <- gsub("CN", "US", str_trim(test$patent_office))

patents_AI_specific2 <- rbind(patents_AI_specific, test)

patents_AI_specific2 %<>% 
  group_by(appln_id) %>% 
  mutate(DistinctpatentOffice = n_distinct(patent_office, na.rm = T)) %>%
  ungroup()

patents_AI_specific2[patents_AI_specific2$appln_id == "475222998",]
table(patents_AI_specific2$DistinctpatentOffice)
#thus, there is no patent with inventors from distinct patent offices in our dataset;

patents_AI_specific_simplified <- patents_AI_specific[patents_AI_specific$DistinctOwnerInf == T,]
patents_AI_specific_simplified2 <- patents_AI_specific2[patents_AI_specific2$DistinctOwnerInf == T,]
#it works!
patents_AI_specific_simplified_4<- patents_AI_specific_simplified[patents_AI_specific_simplified$patent_office == "CN" |
                                                                  patents_AI_specific_simplified$patent_office == "US"|
                                                                  patents_AI_specific_simplified$patent_office == "KR"|
                                                                  patents_AI_specific_simplified$patent_office == "JP", ]

patents_AI_specific_simplified_4$patent_office <- gsub("US", "USA", str_trim(patents_AI_specific_simplified_4$patent_office))
patents_AI_specific_simplified_4$patent_office <- gsub("CN", "China", str_trim(patents_AI_specific_simplified_4$patent_office))
patents_AI_specific_simplified_4$patent_office <- gsub("JP", "Japan", str_trim(patents_AI_specific_simplified_4$patent_office))
patents_AI_specific_simplified_4$patent_office <- gsub("KR", "South Korea", str_trim(patents_AI_specific_simplified_4$patent_office))

table(patents_AI_specific_simplified_4$patent_office)
Data <- as.data.frame(table(patents_AI_specific_simplified_4$patent_office, patents_AI_specific_simplified_4$priority_year))
names(Data) <- c("Country", "Year", "Number_of_AI_patents")

Data$Year <- as.Date(paste(Data$Year, 1, 1, sep = "-")) # beginning of year
Data$Year <- as.Date(paste(Data$Year, 12, 31, sep = "-"))
Data$Year <- as.numeric(format(Data$Year, "%Y"))

NewPatentsAI_colour <-
ggplot(data=Data, aes(x=Year, y=log10(Number_of_AI_patents), group=Country, colour=Country, shape=Country)) +
  geom_line(size=1.2, aes(linetype=Country)) +
  geom_point(size=8) +
  ggtitle("AI-patents registered per country") +
  xlab("Year") +
  ylab("Log10 of the number of new AI registers") + theme_classic() +
  scale_linetype_manual(values=c("twodash", "longdash", "solid", "solid")) +
  scale_shape_manual(values=c(16, 15, 17, 18)) + theme(legend.position="bottom") +
  theme(text = element_text(size = 25)) +
  scale_x_continuous(breaks = c(1974, 1988, 2003, 2018), limits=c(1970, 2018)) + scale_color_brewer(palette="Dark2")

jpeg("Files_created_with_the_code/figures/regular_resolution_ALL_FIGURES_ARE_HERE/Fig4_NewRegisters_percountry.jpg", width = 14, height = 10, units = 'in', res = 300)
NewPatentsAI_colour
dev.off()

#high resolution
jpeg("Files_created_with_the_code/figures/high_resolution/Fig4_NewRegisters_percountryHigh.jpg", width = 14, height = 10, units = 'in', res = 800)
NewPatentsAI_colour
dev.off()

#low resolution
jpeg("Files_created_with_the_code/figures/low_resolution/Fig4_NewRegisters_percountryLow.jpg", width = 14, height = 10, units = 'in', res = 72)
NewPatentsAI_colour
dev.off()
