library(tidyverse) # Collection of all the good stuff like dplyr, ggplot2 ect.
library(magrittr) # For extra-piping operators (eg. %<>%)

# Network specific
library(tidygraph) # For tidy-style graph manipulation
library(ggraph) # For ggplot2 style graph plotting

library(EconGeo) # Economic Geography functions
library(data.table) #for reading the big files using fread and for replacing countries names (by AI_pat for example)

library(netrankr) #library for calculating pagerank related indicators (i.e. centrality_closeness_harmonic and centrality_closeness_residual)

#1. FIRST PART: Technological spaces -----
#In this first part, we calculate and plot the global technological space, and the technological spaces for 
#countries and AI;

#1.1. Sparse matrix -----
#On this first part we will create the sparse matrix, calculate the similarity matrix and save it in a csv file 
#named "Matrix_IPC"
rm(list=ls())
setwd("C:/Users/mathe/OneDrive/Área de Trabalho") #for loading the big file

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

#now we create a function to put these 3 matrixes together (by summ):
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
setwd("C:/Users/mathe/OneDrive/Área de Trabalho")
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
write.csv2(mat_tech_AI_Final, file = "Data_Final_code/Matrix_IPC.csv", row.names = TRUE)

#1.2.Technology Space ----
#Now we will create the technology spaces, dividing them in 3 periods;
setwd("C:/Users/mathe/OneDrive/Área de Trabalho") 
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
setwd("C:/Users/mathe/OneDrive/Área de Trabalho") 
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
write.csv2(reg_tech1, file = "Data_Final_code/reg_tech_FirstPeriod.csv", row.names = F)

#For AI:
patents_AI_specific <- read.csv("Data_Final_code/IPCs_AI.csv", sep = ";", header = TRUE, dec=",")
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
write.csv2(reg_tech_AI1, file = "Data_Final_code/reg_techAI_FirstPeriod.csv", row.names = F)

#1.2.2.Second Period ----
#For the second period, which goes from 1989 to 2003, we again need only the dataset from Part2:
setwd("C:/Users/mathe/OneDrive/Área de Trabalho") 
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
write.csv2(reg_tech2, file = "Data_Final_code/reg_tech_SecondPeriod.csv", row.names = F)

#For AI:
#we replace the AI data on the IPC dataset;
setDT(patents_AI_specific)
setDT(IPC_all_patents_SecondPeriod)
IPC_all_patents_SecondPeriod[patents_AI_specific, on = c("appln_id"), ctry_code := i.ctry_code]

reg_tech_AI2 <- group_by_applnID(IPC_all_patents_SecondPeriod)
rm(IPC_all_patents_SecondPeriod)
reg_tech_AI2 <- group_by_ctry_and_IPC(reg_tech_AI2)

#and save the final file, so we can use it again in section 1.3. (around the line 330)
write.csv2(reg_tech_AI2, file = "Data_Final_code/reg_techAI_SecondPeriod.csv", row.names = F)

#1.2.3.Third Period ----
#For the third period, which goes from 2004 to 2018, we  need only the dataset from Part1. This
#specific dataset only has patents from 2004 to 2018, so we don't have to filter it. But calculating
#the reg_tech is very computationally expansive, so we have to divide that in 3 parts.
setwd("C:/Users/mathe/OneDrive/Área de Trabalho") 
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
write.csv2(tabledata2, file = "Data_Final_code/reg_tech_ThirdPeriod.csv", row.names = F)
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
write.csv2(tabledata_AI2, file = "Data_Final_code/reg_techAI_ThirdPeriod.csv", row.names = F)

#1.3.Calculate the g_tech_AI ----
#Now we load all the files we already saved. We start by loading the janitor library, which is used here for converting
#the first column of data to row names.
library(janitor)
rm(list=ls())
#now we load the similarity matrix which was saved in line 139:
matrix2 <- read.csv("Data_Final_code/Matrix_IPC.csv", sep = ";", header = F)
matrix2 <- matrix2 %>%
  row_to_names(row_number = 1)
matrix <- matrix2[,-1]
rownames(matrix) <- matrix2[,1]
matrix <- as.matrix(matrix)
mat_tech_AI_Final <- matrix

mat_tech_rel_AI <- mat_tech_AI_Final %>% 
  relatedness(method = "cosine")
write.table(mat_tech_rel_AI, file = "Data_Final_code/Relatedness_Allperiods2.csv", row.names = F, dec = ".")

IPC_names <- read.csv("Data_Final_code/ipc_technology.csv", sep = ";", header = TRUE)%>%
  select(field_nr, sector, field_name) %>%
  distinct(field_nr, .keep_all = TRUE) %>%
  mutate(field_nr = field_nr) %>%
  arrange(field_nr)

g_tech_AI <- mat_tech_rel_AI %>% as_tbl_graph(directed = FALSE) %N>%
  left_join(IPC_names %>% mutate(field_nr = field_nr %>% as.character()), by = c("name" = "field_nr")) %>%
  mutate(dgr = centrality_eigen(weights = weight)) %E>%
  filter(weight >= mean(weight))

coords_tech_AI <- g_tech_AI %>% igraph::layout.fruchterman.reingold() %>% as_tibble()
colnames(coords_tech_AI) <- c("x", "y")
write.csv2(coords_tech_AI, file = "Data_Final_code/coords_tech_AI.csv", row.names = T)
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
reg_tech1 <- read.csv("Data_Final_code/reg_tech_FirstPeriod.csv", sep = ";", header = TRUE, dec=",")
reg_tech2 <- read.csv("Data_Final_code/reg_tech_SecondPeriod.csv", sep = ";", header = TRUE, dec=",")
reg_tech3 <- read.csv("Data_Final_code/reg_tech_ThirdPeriod.csv", sep = ";", header = TRUE, dec=",")

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

#1.3.2.Calculate reg_RCAs for AI ----
#Now we read the data per period and calculate the RCAs:
reg_tech1 <- read.csv("Data_Final_code/reg_techAI_FirstPeriod.csv", sep = ";", header = TRUE, dec=",")
reg_tech2 <- read.csv("Data_Final_code/reg_techAI_SecondPeriod.csv", sep = ";", header = TRUE, dec=",")
reg_tech3 <- read.csv("Data_Final_code/reg_techAI_ThirdPeriod.csv", sep = ";", header = TRUE, dec=",")

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
Global_technological_space <- g_tech_AI %>%
  ggraph(layout =  coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "grey") + 
  geom_node_point(aes(colour = sector, size = dgr)) + 
  geom_node_text(aes(label = field_name), size = 4, repel = TRUE) +
  theme_graph()+
  ggtitle("Technology Space: IPC codes")

jpeg("Data_Final_code/Global_technological_space.jpg", width = 14, height = 10, units = 'in', res = 200)
Global_technological_space
dev.off()

#1.4.1. IPC Visualization Per country-----
#Now we start the analysis per country:
#1st period
country_select <- c("CN", "US", "JP", "KR")
i = 1

IPC1 <- g_tech_AI %N>%
  left_join(reg_RCA1 %>% filter(ctry_code == country_select[i]) %>% select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "grey") + 
  geom_node_point(aes(colour = RCA, size = dgr)) + 
  geom_node_text(aes(filter=RCA > .9, label = field_name), size = 5, repel = TRUE) +
  scale_color_gradient(low = "skyblue", high = "red") +
  theme_graph() +
  ggtitle("IPC Technology Space: China (1974-1988)")

i = 2
IPC2 <- g_tech_AI %N>%
  left_join(reg_RCA1 %>% filter(ctry_code == country_select[i]) %>% select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "grey") + 
  geom_node_point(aes(colour = RCA, size = dgr)) + 
  geom_node_text(aes(filter=RCA > .9, label = field_name), size = 5, repel = TRUE) +
  scale_color_gradient(low = "skyblue", high = "red") +
  theme_graph() +
  ggtitle("IPC Technology Space: USA (1974-1988)")

i = 3
IPC3 <- g_tech_AI %N>%
  left_join(reg_RCA1 %>% filter(ctry_code == country_select[i]) %>% select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "grey") + 
  geom_node_point(aes(colour = RCA, size = dgr)) + 
  geom_node_text(aes(filter=RCA > .9, label = field_name), size = 5, repel = TRUE) +
  scale_color_gradient(low = "skyblue", high = "red") +
  theme_graph() +
  ggtitle("IPC Technology Space: Japan (1974-1988)")

i = 4
IPC4 <- g_tech_AI %N>%
  left_join(reg_RCA1 %>% filter(ctry_code == country_select[i]) %>% select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "grey") + 
  geom_node_point(aes(colour = RCA, size = dgr)) + 
  geom_node_text(aes(filter=RCA > .9, label = field_name), size = 5, repel = TRUE) +
  scale_color_gradient(low = "skyblue", high = "red") +
  theme_graph() +
  ggtitle("IPC Technology Space: South Korea (1974-1988)")

#For saving the pictures:
jpeg("Data_Final_code/IPC_all_CN_persp_Period1.jpg", width = 14, height = 10, units = 'in', res = 200)
IPC1
dev.off()

jpeg("Data_Final_code/IPC_all_US_persp_Period1.jpg", width = 14, height = 10, units = 'in', res = 200)
IPC2
dev.off()

jpeg("Data_Final_code/IPC_all_JP_persp_Period1.jpg", width = 14, height = 10, units = 'in', res = 200)
IPC3
dev.off()

jpeg("Data_Final_code/IPC_all_KR_persp_Period1.jpg", width = 14, height = 10, units = 'in', res = 200)
IPC4
dev.off()

#Per Country 2nd period
i = 1
IPC1 <- g_tech_AI %N>%
  left_join(reg_RCA2 %>% filter(ctry_code == country_select[i]) %>% select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "grey") + 
  geom_node_point(aes(colour = RCA, size = dgr)) + 
  geom_node_text(aes(filter=RCA > .9, label = field_name), size = 5, repel = TRUE) +
  scale_color_gradient(low = "skyblue", high = "red") +
  theme_graph() +
  ggtitle("IPC Technology Space: China (1989-2003)")


i = 2
IPC2 <- g_tech_AI %N>%
  left_join(reg_RCA2 %>% filter(ctry_code == country_select[i]) %>% select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "grey") + 
  geom_node_point(aes(colour = RCA, size = dgr)) + 
  geom_node_text(aes(filter=RCA > .9, label = field_name), size = 5, repel = TRUE) +
  scale_color_gradient(low = "skyblue", high = "red") +
  theme_graph() +
  ggtitle("IPC Technology Space: USA (1989-2003)")

i = 3
IPC3 <- g_tech_AI %N>%
  left_join(reg_RCA2 %>% filter(ctry_code == country_select[i]) %>% select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "grey") + 
  geom_node_point(aes(colour = RCA, size = dgr)) + 
  geom_node_text(aes(filter=RCA > .9, label = field_name), size = 5, repel = TRUE) +
  scale_color_gradient(low = "skyblue", high = "red") +
  theme_graph() +
  ggtitle("IPC Technology Space: Japan (1989-2003)")

i = 4
IPC4 <- g_tech_AI %N>%
  left_join(reg_RCA2 %>% filter(ctry_code == country_select[i]) %>% select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "grey") + 
  geom_node_point(aes(colour = RCA, size = dgr)) + 
  geom_node_text(aes(filter=RCA > .9, label = field_name), size = 5, repel = TRUE) +
  scale_color_gradient(low = "skyblue", high = "red") +
  theme_graph() +
  ggtitle("IPC Technology Space: South Korea (1989-2003)")

#For saving the pictures:
jpeg("Data_Final_code/IPC_all_CN_persp_Period2.jpg", width = 14, height = 10, units = 'in', res = 200)
IPC1
dev.off()

jpeg("Data_Final_code/IPC_all_US_persp_Period2.jpg", width = 14, height = 10, units = 'in', res = 200)
IPC2
dev.off()

jpeg("Data_Final_code/IPC_all_JP_persp_Period2.jpg", width = 14, height = 10, units = 'in', res = 200)
IPC3
dev.off()

jpeg("Data_Final_code/IPC_all_KR_persp_Period2.jpg", width = 14, height = 10, units = 'in', res = 200)
IPC4
dev.off()

#Per Country 3rd period
i = 1
IPC1 <- g_tech_AI %N>%
  left_join(reg_RCA3 %>% filter(ctry_code == country_select[i]) %>% select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "grey") + 
  geom_node_point(aes(colour = RCA, size = dgr)) + 
  geom_node_text(aes(filter=RCA > .9, label = field_name), size = 5, repel = TRUE) +
  scale_color_gradient(low = "skyblue", high = "red") +
  theme_graph() +
  ggtitle("IPC Technology Space: China (2004-2018)")

i = 2
IPC2 <- g_tech_AI %N>%
  left_join(reg_RCA3 %>% filter(ctry_code == country_select[i]) %>% select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "grey") + 
  geom_node_point(aes(colour = RCA, size = dgr)) + 
  geom_node_text(aes(filter=RCA > .9, label = field_name), size = 5, repel = TRUE) +
  scale_color_gradient(low = "skyblue", high = "red") +
  theme_graph() +
  ggtitle("IPC Technology Space: USA (2004-2018)")

i = 3
IPC3 <- g_tech_AI %N>%
  left_join(reg_RCA3 %>% filter(ctry_code == country_select[i]) %>% select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "grey") + 
  geom_node_point(aes(colour = RCA, size = dgr)) + 
  geom_node_text(aes(filter=RCA > .9, label = field_name), size = 5, repel = TRUE) +
  scale_color_gradient(low = "skyblue", high = "red") +
  theme_graph() +
  ggtitle("IPC Technology Space: Japan (2004-2018)")

i = 4
IPC4 <- g_tech_AI %N>%
  left_join(reg_RCA3 %>% filter(ctry_code == country_select[i]) %>% select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "grey") + 
  geom_node_point(aes(colour = RCA, size = dgr)) + 
  geom_node_text(aes(filter=RCA > .9, label = field_name), size = 5, repel = TRUE) +
  scale_color_gradient(low = "skyblue", high = "red") +
  theme_graph() +
  ggtitle("IPC Technology Space: South Korea (2004-2018)")

#For saving the pictures:
jpeg("Data_Final_code/IPC_all_CN_persp_Period3.jpg", width = 14, height = 10, units = 'in', res = 200)
IPC1
dev.off()

jpeg("Data_Final_code/IPC_all_US_persp_Period3.jpg", width = 14, height = 10, units = 'in', res = 200)
IPC2
dev.off()

jpeg("Data_Final_code/IPC_all_JP_persp_Period3.jpg", width = 14, height = 10, units = 'in', res = 200)
IPC3
dev.off()

jpeg("Data_Final_code/IPC_all_KR_persp_Period3.jpg", width = 14, height = 10, units = 'in', res = 200)
IPC4
dev.off()

#1.4.2. IPC Visualization AI-----

#First period
country_select <- c("AI_pat")
i = 1
IPC_AI1 <- g_tech_AI %N>%
  left_join(reg_RCA_AI1 %>% filter(ctry_code == country_select[i]) %>% select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "grey") + 
  geom_node_point(aes(colour = RCA, size = dgr)) + 
  geom_node_text(aes(filter=RCA > .9, label = field_name), size = 5, repel = TRUE) +
  scale_color_gradient(low = "skyblue", high = "red") +
  theme_graph() +
  ggtitle("Technology Space: AI patents (1974-1988)")

jpeg("Data_Final_code/IPC_all_AIpatents_specific_Period1.jpg", width = 14, height = 10, units = 'in', res = 200)
IPC_AI1
dev.off()

#Second period
IPC_AI2 <- g_tech_AI %N>%
  left_join(reg_RCA_AI2 %>% filter(ctry_code == country_select[i]) %>% select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "grey") + 
  geom_node_point(aes(colour = RCA, size = dgr)) + 
  geom_node_text(aes(filter=RCA > .9, label = field_name), size = 5, repel = TRUE) +
  scale_color_gradient(low = "skyblue", high = "red") +
  theme_graph() +
  ggtitle("Technology Space: AI patents (1989-2003)")

jpeg("Data_Final_code/IPC_all_AIpatents_specific_Period2.jpg", width = 14, height = 10, units = 'in', res = 200)
IPC_AI2
dev.off()

#Third period
IPC_AI3 <- g_tech_AI %N>%
  left_join(reg_RCA_AI3 %>% filter(ctry_code == country_select[i]) %>% select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "grey") + 
  geom_node_point(aes(colour = RCA, size = dgr)) + 
  geom_node_text(aes(filter=RCA > .9, label = field_name), size = 5, repel = TRUE) +
  scale_color_gradient(low = "skyblue", high = "red") +
  theme_graph() +
  ggtitle("Technology Space: AI patents (2004-2018)")

jpeg("Data_Final_code/IPC_all_AIpatents_specific_Period3.jpg", width = 14, height = 10, units = 'in', res = 200)
IPC_AI3
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
setwd("C:/Users/mathe/OneDrive/Área de Trabalho") #for loading the big files
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
patents_AI_specific <- read.csv("Data_Final_code/IPCs_AI.csv", sep = ";", header = TRUE, dec=",")
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

#Considering technologies in general (used in Fig 8)
KnowledgeComp_1st <- as.data.frame(MORt(mat_1st))
KnowledgeComp_1st$Step0 <- MORt(mat_1st, steps = 0)
KnowledgeComp_1st$Step1 <- MORt(mat_1st, steps = 1)
KnowledgeComp_1st$Step2 <- MORt(mat_1st, steps = 2)

mat_1st_RCAs <- location.quotient(mat_1st, binary = T)
KnowledgeComp_1st$RCA <- MORt(mat_1st_RCAs)
KnowledgeComp_1st$RCA_Step0 <- MORt(mat_1st_RCAs, steps = 0)
KnowledgeComp_1st$RCA_Step1 <- MORt(mat_1st_RCAs, steps = 1)
KnowledgeComp_1st$RCA_Step2 <- MORt(mat_1st_RCAs, steps = 2)
write.csv2(KnowledgeComp_1st, file = "Data_Final_code/KnowledgeComp_1st.csv", row.names = TRUE)

#Considering Morc 1st period
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

write.csv2(KnowledgeComp_1st_Morc, file = "Data_Final_code/KnowledgeComp_1st_Morc.csv", row.names = T)
write.csv2(KnowledgeComp_1st_Top4, file = "Data_Final_code/KnowledgeComp_1st_Top4.csv", row.names = T)
write.csv2(KnowledgeComp_1st_Top3, file = "Data_Final_code/KnowledgeComp_1st_Top3.csv", row.names = T)
write.csv2(KnowledgeComp_1st_Surr, file = "Data_Final_code/KnowledgeComp_1st_Surr.csv", row.names = T)

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

write.csv2(KnowledgeComp_PerCountry_1st_All_RCAs, file = "Data_Final_code/KnowledgeComp_PerCountry_1st_All_RCAs_AI.csv", row.names = TRUE)

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

write.csv2(Relatedness_FirstPeriod, file = "Data_Final_code/Relatedness_1st_period_IPC.csv", row.names = TRUE)

#2.2. Second period ----
#Starting with an empty global environment:
rm(list=ls())
setwd("C:/Users/mathe/OneDrive/Área de Trabalho")
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
patents_AI_specific <- read.csv("Data_Final_code/IPCs_AI.csv", sep = ";", header = TRUE, dec=",")
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
write.csv2(KnowledgeComp_2nd, file = "Data_Final_code/KnowledgeComp_2nd.csv", row.names = TRUE)

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

write.csv2(KnowledgeComp_2nd_Morc, file = "Data_Final_code/KnowledgeComp_2nd_Morc.csv", row.names = T)
write.csv2(KnowledgeComp_2nd_Top4, file = "Data_Final_code/KnowledgeComp_2nd_Top4.csv", row.names = T)
write.csv2(KnowledgeComp_2nd_Top3, file = "Data_Final_code/KnowledgeComp_2nd_Top3.csv", row.names = T)
write.csv2(KnowledgeComp_2nd_Surr, file = "Data_Final_code/KnowledgeComp_2nd_Surr.csv", row.names = T)

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

write.csv2(KnowledgeComp_PerCountry_2nd_All_RCAs, file = "Data_Final_code/KnowledgeComp_PerCountry_2nd_All_RCAs_AI.csv", row.names = TRUE)

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

write.csv2(Relatedness_SecondPeriod, file = "Data_Final_code/Relatedness_2nd_period_IPC.csv", row.names = TRUE)

#2.3. Third period ----
#Starting with an empty global environment:
rm(list=ls())
setwd("C:/Users/mathe/OneDrive/Área de Trabalho")
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
patents_AI_specific <- read.csv("Data_Final_code/IPCs_AI.csv", sep = ";", header = TRUE, dec=",")
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

#Considering technologies in general (used in Fig 8)
KnowledgeComp_3rd <- as.data.frame(MORt(mat_3rd))
KnowledgeComp_3rd$Step0 <- MORt(mat_3rd, steps = 0)
KnowledgeComp_3rd$Step1 <- MORt(mat_3rd, steps = 1)
KnowledgeComp_3rd$Step2 <- MORt(mat_3rd, steps = 2)

mat_3rd_RCAs <- location.quotient(mat_3rd, binary = T)
KnowledgeComp_3rd$RCA <- MORt(mat_3rd_RCAs)
KnowledgeComp_3rd$RCA_Step0 <- MORt(mat_3rd_RCAs, steps = 0)
KnowledgeComp_3rd$RCA_Step1 <- MORt(mat_3rd_RCAs, steps = 1)
KnowledgeComp_3rd$RCA_Step2 <- MORt(mat_3rd_RCAs, steps = 2)
write.csv2(KnowledgeComp_3rd, file = "Data_Final_code/KnowledgeComp_3rd.csv", row.names = TRUE)

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

write.csv2(KnowledgeComp_3rd_Morc, file = "Data_Final_code/KnowledgeComp_3rd_Morc.csv", row.names = T)
write.csv2(KnowledgeComp_3rd_Top4, file = "Data_Final_code/KnowledgeComp_3rd_Top4.csv", row.names = T)
write.csv2(KnowledgeComp_3rd_Top3, file = "Data_Final_code/KnowledgeComp_3rd_Top3.csv", row.names = T)
write.csv2(KnowledgeComp_3rd_Surr, file = "Data_Final_code/KnowledgeComp_3rd_Surr.csv", row.names = T)

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

write.csv2(KnowledgeComp_PerCountry_3rd_All_RCAs, file = "Data_Final_code/KnowledgeComp_PerCountry_3rd_All_RCAs_AI.csv", row.names = TRUE)

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

write.csv2(Relatedness_ThirdPeriod, file = "Data_Final_code/Relatedness_3rd_period_IPC.csv", row.names = TRUE)

#2.3. Visualization ----
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

#2.3.1.Relatedness ----
Relatedness_1st <- read.csv("Data_Final_code/Relatedness_1st_period_IPC.csv", sep = ";", header = TRUE, dec=",")
Relatedness_2nd <- read.csv("Data_Final_code/Relatedness_2nd_period_IPC.csv", sep = ";", header = TRUE, dec=",")
Relatedness_3rd <- read.csv("Data_Final_code/Relatedness_3rd_period_IPC.csv", sep = ";", header = TRUE, dec=",")
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

Rel_byP_c <- ggplot(Relatedness, aes(x=Country, y=Value, fill=Period)) +
  geom_bar(stat="identity", position=position_dodge(), show.legend = F)+theme_minimal()+ xlab(NULL) + ylab("Relatedness") +
  facet_wrap(~Indicator, ncol = 4) +
  scale_fill_brewer(palette="Paired") + theme_classic() +
  ggtitle("Countries Relatedness in the considered IPC fields") + 
  scale_y_continuous(limits=c(.45,1.5),oob = rescale_none)

Rel_byAI_c<- ggplot(Relatedness_AI, aes(x=Country, y=Value, fill=Period)) +
  geom_bar(stat="identity", position=position_dodge(), show.legend = F)+theme_minimal() + labs(x = "") +
  scale_x_discrete(labels = NULL) + ylab("Relatedness") +
  facet_wrap(~Indicator, ncol = 4) +
  scale_fill_brewer(palette="Paired") + theme_classic() +
  ggtitle("AI Relatedness in the considered IPC fields")+
  scale_y_continuous(limits=c(.1,2.35),oob = rescale_none)

#2.3.2.Knowld Comp. AI -----
KnowlComp_1st_AI <- read.csv("Data_Final_code/KnowledgeComp_PerCountry_1st_All_RCAs_AI.csv", sep = ";", header = TRUE, dec=",")
KnowlComp_2nd_AI <- read.csv("Data_Final_code/KnowledgeComp_PerCountry_2nd_All_RCAs_AI.csv", sep = ";", header = TRUE, dec=",")
KnowlComp_3rd_AI <- read.csv("Data_Final_code/KnowledgeComp_PerCountry_3rd_All_RCAs_AI.csv", sep = ";", header = TRUE, dec=",")

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
Comp_byAI_c<- 
  ggplot(KnowledgeCompl_AI_all, aes(x=Country, y=Value, fill=Period)) +
  geom_bar(stat="identity", position=position_dodge())+theme_minimal() + labs(x = "") +
  scale_x_discrete(labels = NULL) + ylab("Knowledge Complexity (MORt)") +
  facet_wrap(~Indicator, ncol = 4) +
  scale_fill_brewer(palette="Paired") + theme_classic() + theme(legend.position="bottom") +
  ggtitle("AI Knowledge Complexity in the considered IPC fields")

#2.3.3.Knowld Comp. Countries -----
KnowlComp_1st <- read.csv("Data_Final_code/KnowledgeComp_1st_Morc.csv", sep = ";", header = F, dec=".")
KnowlComp_2nd <- read.csv("Data_Final_code/KnowledgeComp_2nd_Morc.csv", sep = ";", header = TRUE, dec=".")
KnowlComp_3rd <- read.csv("Data_Final_code/KnowledgeComp_3rd_Morc.csv", sep = ";", header = TRUE, dec=".")

names(KnowlComp_1st) <- c("Country", "MORc", "RCA", "RCA_step0", "RCA_step1", "RCA_step2")
names(KnowlComp_2nd) <- c("Country", "MORc", "RCA", "RCA_step0", "RCA_step1", "RCA_step2")
names(KnowlComp_3rd) <- c("Country", "MORc", "RCA", "RCA_step0", "RCA_step1", "RCA_step2")

KnowlComp_1st$Period <- "Period 1 (1974-1988)"
KnowlComp_2nd$Period <- "Period 2 (1989-2003)"
KnowlComp_3rd$Period <- "Period 3 (2004-2018)"

KnowledgeCompl <- rbind(KnowlComp_1st, KnowlComp_2nd, KnowlComp_3rd)
rm(KnowlComp_1st, KnowlComp_2nd, KnowlComp_3rd)
KnowledgeCompl$Category <- "Overall Complexity"

KnowlComp_1st_Top4 <- read.csv("Data_Final_code/KnowledgeComp_1st_Top4.csv", sep = ";", header = F, dec=".")
KnowlComp_2nd_Top4 <- read.csv("Data_Final_code/KnowledgeComp_2nd_Top4.csv", sep = ";", header = TRUE, dec=".")
KnowlComp_3rd_Top4 <- read.csv("Data_Final_code/KnowledgeComp_3rd_Top4.csv", sep = ";", header = TRUE, dec=".")

names(KnowlComp_1st_Top4) <- c("Country", "MORc", "RCA", "RCA_step0", "RCA_step1", "RCA_step2")
names(KnowlComp_2nd_Top4) <- c("Country", "MORc", "RCA", "RCA_step0", "RCA_step1", "RCA_step2")
names(KnowlComp_3rd_Top4) <- c("Country", "MORc", "RCA", "RCA_step0", "RCA_step1", "RCA_step2")

KnowlComp_1st_Top4$Period <- "Period 1 (1974-1988)"
KnowlComp_2nd_Top4$Period <- "Period 2 (1989-2003)"
KnowlComp_3rd_Top4$Period <- "Period 3 (2004-2018)"

KnowledgeCompl_Top4 <- rbind(KnowlComp_1st_Top4, KnowlComp_2nd_Top4, KnowlComp_3rd_Top4)
rm(KnowlComp_1st_Top4, KnowlComp_2nd_Top4, KnowlComp_3rd_Top4)
KnowledgeCompl_Top4$Category <- "AI-core fields"

KnowlComp_1st_Top3 <- read.csv("Data_Final_code/KnowledgeComp_1st_Top3.csv", sep = ";", header = F, dec=".")
KnowlComp_2nd_Top3 <- read.csv("Data_Final_code/KnowledgeComp_2nd_Top3.csv", sep = ";", header = TRUE, dec=".")
KnowlComp_3rd_Top3 <- read.csv("Data_Final_code/KnowledgeComp_3rd_Top3.csv", sep = ";", header = TRUE, dec=".")

names(KnowlComp_1st_Top3) <- c("Country", "MORc", "RCA", "RCA_step0", "RCA_step1", "RCA_step2")
names(KnowlComp_2nd_Top3) <- c("Country", "MORc", "RCA", "RCA_step0", "RCA_step1", "RCA_step2")
names(KnowlComp_3rd_Top3) <- c("Country", "MORc", "RCA", "RCA_step0", "RCA_step1", "RCA_step2")

KnowlComp_1st_Top3$Period <- "Period 1 (1974-1988)"
KnowlComp_2nd_Top3$Period <- "Period 2 (1989-2003)"
KnowlComp_3rd_Top3$Period <- "Period 3 (2004-2018)"

KnowledgeCompl_Top3 <- rbind(KnowlComp_1st_Top3, KnowlComp_2nd_Top3, KnowlComp_3rd_Top3)
rm(KnowlComp_1st_Top3, KnowlComp_2nd_Top3, KnowlComp_3rd_Top3)
KnowledgeCompl_Top3$Category <- "AI-related fields"

KnowlComp_1st_Surr <- read.csv("Data_Final_code/KnowledgeComp_1st_Surr.csv", sep = ";", header = F, dec=".")
KnowlComp_2nd_Surr <- read.csv("Data_Final_code/KnowledgeComp_2nd_Surr.csv", sep = ";", header = TRUE, dec=".")
KnowlComp_3rd_Surr <- read.csv("Data_Final_code/KnowledgeComp_3rd_Surr.csv", sep = ";", header = TRUE, dec=".")

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

write.csv2(All_data_knowlComp_Morc, file = "Data_Final_code/All_data_knowlComp_Morc.csv", row.names = F)
All_data_knowlComp_Morc <- read.csv("Data_Final_code/All_data_knowlComp_Morc.csv", sep = ";", header = TRUE, dec=",")
All_data_knowlComp_Morc$Category <- factor(All_data_knowlComp_Morc$Category, levels = c("Overall Complexity", "AI-core fields",
                                                                        "AI-related fields", "Surrounding fields"))

Comp_byP_c <-
  ggplot(All_data_knowlComp_Morc, aes(x=Country, y=RCA_step1, fill=Period)) +
  geom_bar(stat="identity", position=position_dodge())+theme_minimal()+ xlab(NULL) + ylab("Knowledge Complexity (MORc)") +
  facet_wrap(~Category, ncol = 4) +
  scale_fill_brewer(palette="Paired") + theme_classic()  + theme(legend.position="bottom") +
  ggtitle("Countries Knowledge Complexity in the considered IPC fields") 

tiff("Data_Final_code/Relatedness_and_Complex_AI.jpg", width = 8, height = 6, units = 'in', res = 200)
multiplot(Rel_byAI_c, Comp_byAI_c, cols=1) 
dev.off()

tiff("Data_Final_code/Relatedness_and_Complex_Morc_countries.jpg", width = 8, height = 6, units = 'in', res = 200)
multiplot(Rel_byP_c, Comp_byP_c, cols=1) 
dev.off()

#3. THIRD PART: Fig8 calculations----
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#3.1.Fig 8 ----
#3.1.1.Create specialisations summary ----
# Which is used for Fig8
reg_tech1_countries <- read.csv("Data_Final_code/reg_tech_FirstPeriod.csv", sep = ";", header = TRUE, dec=",")
#4.1.1. First Period Countries
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

###4.1.2. First Period AI
reg_tech1_AI <- read.csv("Data_Final_code/reg_techAI_FirstPeriod.csv", sep = ";", header = TRUE, dec=",")

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

IPC_names <- read.csv("Data_Final_code/ipc_technology.csv", sep = ";", header = TRUE)%>%
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
write.csv2(First_period, file = "Data_Final_code/Metrics_First_period_complexity.csv", row.names = F)

#4.2.Second period
reg_tech2_countries <- read.csv("Data_Final_code/reg_tech_SecondPeriod.csv", sep = ";", header = TRUE, dec=",")
#4.2.1. Second Period Countries
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

###4.2.2. Second Period AI
reg_tech2_AI <- read.csv("Data_Final_code/reg_techAI_SecondPeriod.csv", sep = ";", header = TRUE, dec=",")

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
write.csv2(Second_period, file = "Data_Final_code/Metrics_Second_period_complexity.csv", row.names = F)

#4.3.Third period
reg_tech3_countries <- read.csv("Data_Final_code/reg_tech_ThirdPeriod.csv", sep = ";", header = TRUE, dec=",")
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

###4.3.2. Third Period AI
reg_tech3_AI <- read.csv("Data_Final_code/reg_techAI_ThirdPeriod.csv", sep = ";", header = TRUE, dec=",")

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
write.csv2(Third_period, file = "Data_Final_code/Metrics_Third_period_complexity.csv", row.names = F)

#put it all together
First_period <- read.csv("Data_Final_code/Metrics_First_period_complexity.csv", sep = ";", header = TRUE, dec=",")
Second_period <- read.csv("Data_Final_code/Metrics_Second_period_complexity.csv", sep = ";", header = TRUE, dec=",")
Third_period <- read.csv("Data_Final_code/Metrics_Third_period_complexity.csv", sep = ";", header = TRUE, dec=",")

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

write.csv2(All_periods, file = "Data_Final_code/Specializations_All_periods_IPC.csv", row.names = TRUE)

#3.1.2.Plot Fig8 ----
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(tidyr)
library(ggrepel)

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

#KnowledgeCompl <- read.csv("Data_Final_code/All_data_knowlComp_Morc.csv", sep = ";", header = TRUE, dec=",")
KnowlComp_1st <- read.csv("Data_Final_code/KnowledgeComp_1st.csv", sep = ";", header = TRUE, dec=",")
KnowlComp_2nd <- read.csv("Data_Final_code/KnowledgeComp_2nd.csv", sep = ";", header = TRUE, dec=",")
KnowlComp_3rd <- read.csv("Data_Final_code/KnowledgeComp_3rd.csv", sep = ";", header = TRUE, dec=",")

KnowlComp_1st <- KnowlComp_1st %>% rename(MORt = MORt.mat_1st., techn_field_nr = X)
KnowlComp_2nd <- KnowlComp_2nd %>% rename(MORt = MORt.mat_2nd., techn_field_nr = X)
KnowlComp_3rd <- KnowlComp_3rd %>% rename(MORt = MORt.mat_3rd., techn_field_nr = X)

KnowlComp_1st$Period <- "Period 1 (1974-1988)"
KnowlComp_2nd$Period <- "Period 2 (1989-2003)"
KnowlComp_3rd$Period <- "Period 3 (2004-2018)"

All_KwnCom<- rbind(KnowlComp_1st, KnowlComp_2nd, KnowlComp_3rd)
rm(KnowlComp_1st, KnowlComp_2nd, KnowlComp_3rd)

Specialisations <- read.csv("Data_Final_code/Specializations_All_periods_IPC.csv", sep = ";", header = TRUE, dec=",")
Specialisations <- Specialisations[,(-1)]

# add row index so later spreading indexed correctly
Specialisations3<-Specialisations %>% rownames_to_column() %>% 
  # melt to long format
  gather(RCA_, value, -techn_field_nr, -field_name,-rowname, -Period, -Category, -Category2)

Specialisations3$RCA_ <- gsub("RCA_", "", str_trim(Specialisations3$RCA_))
Specialisations3 <- Specialisations3[,(-1)]
Specialisations3 <- Specialisations3 %>% rename(Country = RCA_, Specialisation = value)
Specialisations3$Binary <- ifelse(Specialisations3$Specialisation < 1,0,1)

All_data <- merge(All_KwnCom, Specialisations3, all=F, by=c("Period", "techn_field_nr"))
rm(Specialisations3, Specialisations,All_KwnCom)

All_data$Category <- factor(All_data$Category, levels = c("AI-core fields", "AI-related fields",
                                                                     "Surrounding fields", "Other"))

BinarySum<- aggregate(All_data[,16], list(All_data$Country, All_data$techn_field_nr), sum)
names(BinarySum)<- c("Country", "techn_field_nr", "SumBinary")

All_data <- merge(All_data, BinarySum, all=F, by=c("Country", "techn_field_nr"))
All_data$Specialisation <- as.numeric(All_data$Specialisation)

All_data_JP <- All_data[All_data$Country == "JP",]
All_data_CN <- All_data[All_data$Country == "CN",]
All_data_US <- All_data[All_data$Country == "US",]
All_data_KR <- All_data[All_data$Country == "KR",]

tiff("Data_Final_code/New_KnowledgeComplJP.jpg", width = 14, height = 8, units = 'in', res = 200)
ggplot(All_data_JP, aes(x=log10(Specialisation), y=-(RCA), label = '')) + 
  geom_point(aes(colour = Category, size = Specialisation),show.legend = T, stroke = 2) +  
  geom_text() +
  geom_vline(xintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2) +
  scale_color_brewer(palette="Dark2") + theme_minimal() +
  geom_label_repel(data = All_data_JP, aes(label = ifelse(Specialisation>1 & Category2<4,as.character(field_name),'')),nudge_x = -0.05, nudge_y = 10) +
  scale_size_continuous(range = c(1, 10)) +
  # ggtitle("Knowledge Complexity of technologies - Japan") +
  facet_wrap(~Period, ncol = 3) +
  xlab("Log10 of Japan's RCA in each technology") +
  ylab("Index of knowledge complexity of technology (MORt)")
dev.off()

tiff("Data_Final_code/New_KnowledgeComplCN.jpg", width = 14, height = 8, units = 'in', res = 200)
ggplot(All_data_CN, aes(x=log10(Specialisation), y=-(RCA), label = '')) + 
  geom_point(aes(colour = Category, size = Specialisation),show.legend = T, stroke = 2) +  
  geom_text() +
  geom_vline(xintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2) +
  scale_color_brewer(palette="Dark2") + theme_minimal() +
  geom_label_repel(data = All_data_CN, aes(label = ifelse(Specialisation>1 & Category2<4,as.character(field_name),'')),nudge_x = -0.05, nudge_y = 10) +
  scale_size_continuous(range = c(1, 10)) +
  # ggtitle("Knowledge Complexity of technologies - Japan") +
  facet_wrap(~Period, ncol = 3) +
  xlab("Log10 of China's RCA in each technology") +
  ylab("Index of knowledge complexity of technology (MORt)")
dev.off()

tiff("Data_Final_code/New_KnowledgeComplUS.jpg", width = 14, height = 8, units = 'in', res = 200)
ggplot(All_data_US, aes(x=log10(Specialisation), y=-(RCA), label = '')) + 
  geom_point(aes(colour = Category, size = Specialisation),show.legend = T, stroke = 2) +  
  geom_text() +
  geom_vline(xintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2) +
  scale_color_brewer(palette="Dark2") + theme_minimal() +
  geom_label_repel(data = All_data_US, aes(label = ifelse(Specialisation>1 & Category2<4,as.character(field_name),'')),nudge_x = -0.05, nudge_y = 10) +
  scale_size_continuous(range = c(1, 10)) +
  # ggtitle("Knowledge Complexity of technologies - Japan") +
  facet_wrap(~Period, ncol = 3) +
  xlab("Log10 of United State's RCA in each technology") +
  ylab("Index of knowledge complexity of technology (MORt)")
dev.off()

tiff("Data_Final_code/New_KnowledgeComplKR.jpg", width = 14, height = 8, units = 'in', res = 200)
ggplot(All_data_KR, aes(x=log10(Specialisation), y=-(RCA), label = '')) + 
  geom_point(aes(colour = Category, size = Specialisation),show.legend = T, stroke = 2) +  
  geom_text() +
  geom_vline(xintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2) +
  scale_color_brewer(palette="Dark2") + theme_minimal() +
  geom_label_repel(data = All_data_KR, aes(label = ifelse(Specialisation>1 & Category2<4,as.character(field_name),'')),nudge_x = -0.05, nudge_y = 10) +
  scale_size_continuous(range = c(1, 10)) +
  # ggtitle("Knowledge Complexity of technologies - Japan") +
  facet_wrap(~Period, ncol = 3) +
  xlab("Log10 of South Korea's RCA in each technology") +
  ylab("Index of knowledge complexity of technology (MORt)")
dev.off()
