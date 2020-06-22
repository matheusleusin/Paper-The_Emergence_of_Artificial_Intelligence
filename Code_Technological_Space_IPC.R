library(tidyverse) # Collection of all the good stuff like dplyr, ggplot2 ect.
library(magrittr) # For extra-piping operators (eg. %<>%)

# Network specific
library(tidygraph) # For tidy-style graph manipulation
library(ggraph) # For ggplot2 style graph plotting

library(devtools)
library(EconGeo) # Economic Geography functions

library(ggplot2)
library("data.table") #for reading the big files using fread and for replacing countries names (by AI_pat for example)

#1.NACE data -----
#1.1. Sparse matrix -----
#On this first part we will create the sparse matrix, calculate the similarity matrix and save it in a csv file 
#named "Matrix_Nace"
rm(list=ls())
setwd("C:/Users/Matheus/Desktop") #for loading the big file

#Now we will load a first big file containing all priorities and their related Nace codes published in or after 2004. This file has
#58,935,336 lines. I will read it in 3 parts:
c <- 58935336-40000000
Nace_all_patents_Part1 <- fread("All_patents_and_Naces_Part1.csv", header = F, nrow = 20000000)
Nace_all_patents_Part2 <- fread("All_patents_and_Naces_Part1.csv", header = F, nrow = 20000000, skip = 20000000)
Nace_all_patents_Part3 <- fread("All_patents_and_Naces_Part1.csv", header = F, nrow = c, skip = 40000000)

names(Nace_all_patents_Part1) <- c("appln_id", "ctry_code", "nace2_code", "weight", "priority_year")
names(Nace_all_patents_Part2) <- c("appln_id", "ctry_code", "nace2_code", "weight", "priority_year")
names(Nace_all_patents_Part3) <- c("appln_id", "ctry_code", "nace2_code", "weight", "priority_year")

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

mat_tech_AI1 <- create_sparse_matrix(i = Nace_all_patents_Part1 %>% pull(appln_id),
                                     j = Nace_all_patents_Part1 %>% pull(nace2_code))

mat_tech_AI1 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_AI2 <- create_sparse_matrix(i = Nace_all_patents_Part2 %>% pull(appln_id),
                                     j = Nace_all_patents_Part2 %>% pull(nace2_code))

mat_tech_AI2 %<>% 
  crossprod() %>% 
  as.matrix()

mat_tech_AI3 <- create_sparse_matrix(i = Nace_all_patents_Part3 %>% pull(appln_id),
                                     j = Nace_all_patents_Part3 %>% pull(nace2_code))

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
rm(Nace_all_patents_Part1)
rm(Nace_all_patents_Part2)
rm(Nace_all_patents_Part3)

#We will load the second big file containing all priorities and their related Nace codes published in or before 2003. This file has
#45,233,329 lines;
setwd("C:/Users/Matheus/Desktop")
c <- 45233329 -40000000
Nace_all_patents_Part1 <- fread("All_patents_and_Naces_Part2.csv", header = F, nrow = 20000000)
Nace_all_patents_Part2 <- fread("All_patents_and_Naces_Part2.csv", header = F, nrow = 20000000, skip = 20000000)
Nace_all_patents_Part3 <- fread("All_patents_and_Naces_Part2.csv", header = F, nrow = c, skip = 40000000)

names(Nace_all_patents_Part1) <- c("appln_id", "ctry_code", "nace2_code", "weight", "priority_year")
names(Nace_all_patents_Part2) <- c("appln_id", "ctry_code", "nace2_code", "weight", "priority_year")
names(Nace_all_patents_Part3) <- c("appln_id", "ctry_code", "nace2_code", "weight", "priority_year")

#set the working directory to the folder where we opened this code:
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#we do all again as before:
mat_tech_AI1 <- create_sparse_matrix(i = Nace_all_patents_Part1 %>% pull(appln_id),
                                     j = Nace_all_patents_Part1 %>% pull(nace2_code))

mat_tech_AI1 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_AI2 <- create_sparse_matrix(i = Nace_all_patents_Part2 %>% pull(appln_id),
                                     j = Nace_all_patents_Part2 %>% pull(nace2_code))

mat_tech_AI2 %<>% 
  crossprod() %>% 
  as.matrix()

mat_tech_AI3 <- create_sparse_matrix(i = Nace_all_patents_Part3 %>% pull(appln_id),
                                     j = Nace_all_patents_Part3 %>% pull(nace2_code))

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
write.csv2(mat_tech_AI_Final, file = "Data_Nace/Matrix_Nace.csv", row.names = TRUE)

#1.2.Technology Space ----
#Now we will create the technology spaces, dividing them in 3 periods;
setwd("C:/Users/Matheus/Desktop") 
rm(list=ls())
#First, we create the 2 functions we will use for every period:

group_by_applnID <- function (data){
  data %>%
    group_by(appln_id) %>%
    mutate(field_weight = 1 / n()) %>%
    ungroup()
}

group_by_ctry_and_nace <- function (data){
  data %<>%
    group_by(ctry_code, nace2_code) %>%
    summarise(n_tech_reg = sum(field_weight)) %>%
    ungroup() %>%
    drop_na() 
}

#1.2.1.First Period ----
#For the first period, which goes from 1974 to 1988, we need only the dataset from Part2:
setwd("C:/Users/Matheus/Desktop") 
c <- 45233329 -40000000
Nace_all_patents_Part1 <- fread("All_patents_and_Naces_Part2.csv", header = F, nrow = 20000000)
Nace_all_patents_Part2 <- fread("All_patents_and_Naces_Part2.csv", header = F, nrow = 20000000, skip = 20000000)
Nace_all_patents_Part3 <- fread("All_patents_and_Naces_Part2.csv", header = F, nrow = c, skip = 40000000)

names(Nace_all_patents_Part1) <- c("appln_id", "ctry_code", "nace2_code", "weight", "priority_year")
names(Nace_all_patents_Part2) <- c("appln_id", "ctry_code", "nace2_code", "weight", "priority_year")
names(Nace_all_patents_Part3) <- c("appln_id", "ctry_code", "nace2_code", "weight", "priority_year")

#we want to pick only the registers from the period we want (from 1974 to 1988, including both cited years)
a = 1973
b = 1989

Nace_all_patents_Part1 <- Nace_all_patents_Part1[Nace_all_patents_Part1$priority_year < b,]
Nace_all_patents_Part1 <- Nace_all_patents_Part1[Nace_all_patents_Part1$priority_year > a,]

Nace_all_patents_Part2 <- Nace_all_patents_Part2[Nace_all_patents_Part2$priority_year < b,]
Nace_all_patents_Part2 <- Nace_all_patents_Part2[Nace_all_patents_Part2$priority_year > a,]

Nace_all_patents_Part3 <- Nace_all_patents_Part3[Nace_all_patents_Part3$priority_year < b,]
Nace_all_patents_Part3 <- Nace_all_patents_Part3[Nace_all_patents_Part3$priority_year > a,]

#let's drop the columns we won't use (weight and priority_year):
Nace_all_patents_Part1 <- Nace_all_patents_Part1[, c((-4), (-5))]
Nace_all_patents_Part2 <- Nace_all_patents_Part2[, c((-4), (-5))]
Nace_all_patents_Part3 <- Nace_all_patents_Part3[, c((-4), (-5))]

#we combine the 3 files:
Nace_all_patents_FirstPeriod <- rbind(Nace_all_patents_Part1, Nace_all_patents_Part2, Nace_all_patents_Part3)
#and exclude the 3 big ones we just used, so we have back our memory:
rm(Nace_all_patents_Part1, Nace_all_patents_Part2, Nace_all_patents_Part3)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#now we apply the 2 functions we created at the beginning of this section:
reg_tech1 <- group_by_applnID(Nace_all_patents_FirstPeriod)
rm(Nace_all_patents_FirstPeriod)
reg_tech1 <- group_by_ctry_and_nace(reg_tech1)

#and save the final file, so we can use it again in section 1.3. (around the line 330)
write.csv2(reg_tech1, file = "Data_Nace/reg_tech_FirstPeriod.csv", row.names = F)

#1.2.2.Second Period ----
#For the second period, which goes from 1989 to 2003, we again need only the dataset from Part2:
setwd("C:/Users/Matheus/Desktop") 
c <- 45233329 -40000000
Nace_all_patents_Part1 <- fread("All_patents_and_Naces_Part2.csv", header = F, nrow = 20000000)
Nace_all_patents_Part2 <- fread("All_patents_and_Naces_Part2.csv", header = F, nrow = 20000000, skip = 20000000)
Nace_all_patents_Part3 <- fread("All_patents_and_Naces_Part2.csv", header = F, nrow = c, skip = 40000000)

names(Nace_all_patents_Part1) <- c("appln_id", "ctry_code", "nace2_code", "weight", "priority_year")
names(Nace_all_patents_Part2) <- c("appln_id", "ctry_code", "nace2_code", "weight", "priority_year")
names(Nace_all_patents_Part3) <- c("appln_id", "ctry_code", "nace2_code", "weight", "priority_year")

a = 1988
b = 2004

Nace_all_patents_Part1 <- Nace_all_patents_Part1[Nace_all_patents_Part1$priority_year < b,]
Nace_all_patents_Part1 <- Nace_all_patents_Part1[Nace_all_patents_Part1$priority_year > a,]

Nace_all_patents_Part2 <- Nace_all_patents_Part2[Nace_all_patents_Part2$priority_year < b,]
Nace_all_patents_Part2 <- Nace_all_patents_Part2[Nace_all_patents_Part2$priority_year > a,]

Nace_all_patents_Part3 <- Nace_all_patents_Part3[Nace_all_patents_Part3$priority_year < b,]
Nace_all_patents_Part3 <- Nace_all_patents_Part3[Nace_all_patents_Part3$priority_year > a,]

#let's drop again the columns we won't use (weight and priority_year):
Nace_all_patents_Part1 <- Nace_all_patents_Part1[, c((-4), (-5))]
Nace_all_patents_Part2 <- Nace_all_patents_Part2[, c((-4), (-5))]
Nace_all_patents_Part3 <- Nace_all_patents_Part3[, c((-4), (-5))]

Nace_all_patents_SecondPeriod <- rbind(Nace_all_patents_Part1, Nace_all_patents_Part2, Nace_all_patents_Part3)
rm(Nace_all_patents_Part1, Nace_all_patents_Part2, Nace_all_patents_Part3)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

reg_tech2 <- group_by_applnID(Nace_all_patents_SecondPeriod)
rm(Nace_all_patents_SecondPeriod)
reg_tech2 <- group_by_ctry_and_nace(reg_tech2)
write.csv2(reg_tech2, file = "Data_Nace/reg_tech_SecondPeriod.csv", row.names = F)

#1.2.3.Third Period ----
#For the third period, which goes from 2004 to 2018, we  need only the dataset from Part1. This
#specific dataset only has patents from 2004 to 2018, so we don't have to filter it. But calculating
#the reg_tech is very computationally expansive, so we have to divide that in 3 parts.
setwd("C:/Users/Matheus/Desktop") 
c <- 58935336-40000000
Nace_all_patents_Part1 <- fread("All_patents_and_Naces_Part1.csv", header = F, nrow = 20000000)
Nace_all_patents_Part2 <- fread("All_patents_and_Naces_Part1.csv", header = F, nrow = 20000000, skip = 20000000)
Nace_all_patents_Part3 <- fread("All_patents_and_Naces_Part1.csv", header = F, nrow = c, skip = 40000000)

names(Nace_all_patents_Part1) <- c("appln_id", "ctry_code", "nace2_code", "weight", "priority_year")
names(Nace_all_patents_Part2) <- c("appln_id", "ctry_code", "nace2_code", "weight", "priority_year")
names(Nace_all_patents_Part3) <- c("appln_id", "ctry_code", "nace2_code", "weight", "priority_year")

Nace_all_patents_Part1 <- Nace_all_patents_Part1[, c((-4), (-5))]
Nace_all_patents_Part2 <- Nace_all_patents_Part2[, c((-4), (-5))]
Nace_all_patents_Part3 <- Nace_all_patents_Part3[, c((-4), (-5))]

#here we divide our calculations of the reg_tech (which was not necessary on the 2 previous periods)
reg_tech4 <- group_by_applnID(Nace_all_patents_Part1)
rm(Nace_all_patents_Part1)
reg_tech4 <- group_by_ctry_and_nace(reg_tech4)

reg_tech5 <- group_by_applnID(Nace_all_patents_Part2)
rm(Nace_all_patents_Part2)
reg_tech5 <- group_by_ctry_and_nace(reg_tech5)

reg_tech6 <- group_by_applnID(Nace_all_patents_Part3)
rm(Nace_all_patents_Part3)
reg_tech6 <- group_by_ctry_and_nace(reg_tech6)

#now we merge them
tabledata2 <- merge(reg_tech4, reg_tech5, all=T, by=c("ctry_code", "nace2_code"))
tabledata2 <- merge(tabledata2, reg_tech6, all=T, by=c("ctry_code", "nace2_code"))

#remove the big files
rm(reg_tech4, reg_tech5, reg_tech6)

#replace NAs, so we don't have problems when summing:
tabledata2[is.na(tabledata2)] <- 0

#do the summ, exclude the tables used, and rename the dataset accordingly:
tabledata2$sum <- rowSums(tabledata2[,c(3:5)])
tabledata2 <- tabledata2[, c((-3), (-4), (-5))]
names(tabledata2) <- c("ctry_code", "nace2_code", "n_tech_reg")

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
write.csv2(tabledata2, file = "Data_Nace/reg_tech_ThirdPeriod.csv", row.names = F)

#1.3.Calculate the g_tech_AI ----