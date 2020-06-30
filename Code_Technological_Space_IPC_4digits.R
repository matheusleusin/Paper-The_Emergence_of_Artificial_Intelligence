library(tidyverse) # Collection of all the good stuff like dplyr, ggplot2 ect.
library(magrittr) # For extra-piping operators (eg. %<>%)

# Network specific
library(tidygraph) # For tidy-style graph manipulation
library(ggraph) # For ggplot2 style graph plotting

library(devtools)
library(EconGeo) # Economic Geography functions

library(ggplot2)
library("data.table") #for reading the big files using fread and for replacing countries names (by AI_pat for example)

library(stringr) #for separating the IPC codes in subclasses

#1.IPC data -----
#1.1. Sparse matrix -----
#On this first part we will create the sparse matrix, calculate the similarity matrix and save it in a csv file 
#named "Matrix_IPC"
rm(list=ls())
setwd("C:/Users/Matheus/Desktop") #for loading the big file

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

#we pick just the subclass for analysis:
IPC_all_patents_Part1$Subclass <- substr(IPC_all_patents_Part1$ipc_class_symbol,1,4)
IPC_all_patents_Part2$Subclass <- substr(IPC_all_patents_Part2$ipc_class_symbol,1,4)
IPC_all_patents_Part3$Subclass <- substr(IPC_all_patents_Part3$ipc_class_symbol,1,4)
IPC_all_patents_Part4$Subclass <- substr(IPC_all_patents_Part4$ipc_class_symbol,1,4)
IPC_all_patents_Part5$Subclass <- substr(IPC_all_patents_Part5$ipc_class_symbol,1,4)

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
                                     j = IPC_all_patents_Part1 %>% pull(Subclass))

mat_tech_AI1 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_AI2 <- create_sparse_matrix(i = IPC_all_patents_Part2 %>% pull(appln_id),
                                     j = IPC_all_patents_Part2 %>% pull(Subclass))

mat_tech_AI2 %<>% 
  crossprod() %>% 
  as.matrix()

mat_tech_AI3 <- create_sparse_matrix(i = IPC_all_patents_Part3 %>% pull(appln_id),
                                     j = IPC_all_patents_Part3 %>% pull(Subclass))

mat_tech_AI3 %<>% 
  crossprod() %>% 
  as.matrix()

mat_tech_AI4 <- create_sparse_matrix(i = IPC_all_patents_Part4 %>% pull(appln_id),
                                     j = IPC_all_patents_Part4 %>% pull(Subclass))

mat_tech_AI4 %<>% 
  crossprod() %>% 
  as.matrix()

mat_tech_AI5 <- create_sparse_matrix(i = IPC_all_patents_Part5 %>% pull(appln_id),
                                     j = IPC_all_patents_Part5 %>% pull(Subclass))

mat_tech_AI5 %<>% 
  crossprod() %>% 
  as.matrix()

#now we create a function to put these 3 matrixes together (by summ):
add_matrices_5 <- function(matrix1, matrix2, matrix3, matrix4, matrix5) {
  a <- list(matrix1, matrix2, matrix3, matrix4, matrix5)
  cols <- sort(unique(unlist(lapply(a, colnames))))
  rows <- sort(unique(unlist(lapply(a, rownames))))
  out <- array(0, dim = c(length(rows), length(cols)), dimnames = list(rows,cols))
  for (m in a) out[rownames(m), colnames(m)] <- out[rownames(m), colnames(m)] + m
  out
}

#and the newly created function:
mat_tech_AI_Final1 <- add_matrices_5(mat_tech_AI1, mat_tech_AI2, mat_tech_AI3, mat_tech_AI4, mat_tech_AI5)

#now, we drop the big files and load the remaining ones;
rm(IPC_all_patents_Part1, IPC_all_patents_Part2, IPC_all_patents_Part3, IPC_all_patents_Part4, IPC_all_patents_Part5)

#For the second period, which goes from 1989 to 2003, we need only the dataset from Part2:
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

#we pick just the subclass for analysis:
IPC_all_patents_Part1$Subclass <- substr(IPC_all_patents_Part1$ipc_class_symbol,1,4)
IPC_all_patents_Part2$Subclass <- substr(IPC_all_patents_Part2$ipc_class_symbol,1,4)
IPC_all_patents_Part3$Subclass <- substr(IPC_all_patents_Part3$ipc_class_symbol,1,4)
IPC_all_patents_Part4$Subclass <- substr(IPC_all_patents_Part4$ipc_class_symbol,1,4)
IPC_all_patents_Part5$Subclass <- substr(IPC_all_patents_Part5$ipc_class_symbol,1,4)

mat_tech_AI1 <- create_sparse_matrix(i = IPC_all_patents_Part1 %>% pull(appln_id),
                                     j = IPC_all_patents_Part1 %>% pull(Subclass))

mat_tech_AI1 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_AI2 <- create_sparse_matrix(i = IPC_all_patents_Part2 %>% pull(appln_id),
                                     j = IPC_all_patents_Part2 %>% pull(Subclass))

mat_tech_AI2 %<>% 
  crossprod() %>% 
  as.matrix()

mat_tech_AI3 <- create_sparse_matrix(i = IPC_all_patents_Part3 %>% pull(appln_id),
                                     j = IPC_all_patents_Part3 %>% pull(Subclass))

mat_tech_AI3 %<>% 
  crossprod() %>% 
  as.matrix()

mat_tech_AI4 <- create_sparse_matrix(i = IPC_all_patents_Part4 %>% pull(appln_id),
                                     j = IPC_all_patents_Part4 %>% pull(Subclass))

mat_tech_AI4 %<>% 
  crossprod() %>% 
  as.matrix()

mat_tech_AI5 <- create_sparse_matrix(i = IPC_all_patents_Part5 %>% pull(appln_id),
                                     j = IPC_all_patents_Part5 %>% pull(Subclass))

mat_tech_AI5 %<>% 
  crossprod() %>% 
  as.matrix()

#and the newly created function:
mat_tech_AI_Final2 <- add_matrices_5(mat_tech_AI1, mat_tech_AI2, mat_tech_AI3, mat_tech_AI4, mat_tech_AI5)

#now, we drop the big files and load the remaining ones;
rm(IPC_all_patents_Part1, IPC_all_patents_Part2, IPC_all_patents_Part3, IPC_all_patents_Part4, IPC_all_patents_Part5)

#For the third period, which goes from 2004 to 2018, we  need only the dataset from Part1. This
#specific dataset only has patents from 2004 to 2018, so we don't have to filter it.
setwd("C:/Users/Matheus/Desktop") 
c <- 120419184-96000000
IPC_all_patents_Part1 <- fread("All_patents_and_IPC_codes_Part1.csv", header = F, nrow = 24000000)
IPC_all_patents_Part2 <- fread("All_patents_and_IPC_codes_Part1.csv", header = F, nrow = 24000000, skip = 24000000)
IPC_all_patents_Part3 <- fread("All_patents_and_IPC_codes_Part1.csv", header = F, nrow = 24000000, skip = 48000000)
IPC_all_patents_Part4 <- fread("All_patents_and_IPC_codes_Part1.csv", header = F, nrow = 24000000, skip = 72000000)
IPC_all_patents_Part5 <- fread("All_patents_and_IPC_codes_Part1.csv", header = F, nrow = c, skip = 96000000)

names(IPC_all_patents_Part1) <- c("appln_id", "ctry_code", "ipc_class_symbol", "priority_year")
names(IPC_all_patents_Part2) <- c("appln_id", "ctry_code", "ipc_class_symbol", "priority_year")
names(IPC_all_patents_Part3) <- c("appln_id", "ctry_code", "ipc_class_symbol", "priority_year")
names(IPC_all_patents_Part4) <- c("appln_id", "ctry_code", "ipc_class_symbol", "priority_year")
names(IPC_all_patents_Part5) <- c("appln_id", "ctry_code", "ipc_class_symbol", "priority_year")

#we pick just the subclass for analysis:
IPC_all_patents_Part1$Subclass <- substr(IPC_all_patents_Part1$ipc_class_symbol,1,4)
IPC_all_patents_Part2$Subclass <- substr(IPC_all_patents_Part2$ipc_class_symbol,1,4)
IPC_all_patents_Part3$Subclass <- substr(IPC_all_patents_Part3$ipc_class_symbol,1,4)
IPC_all_patents_Part4$Subclass <- substr(IPC_all_patents_Part4$ipc_class_symbol,1,4)
IPC_all_patents_Part5$Subclass <- substr(IPC_all_patents_Part5$ipc_class_symbol,1,4)

mat_tech_AI1 <- create_sparse_matrix(i = IPC_all_patents_Part1 %>% pull(appln_id),
                                     j = IPC_all_patents_Part1 %>% pull(Subclass))

mat_tech_AI1 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_AI2 <- create_sparse_matrix(i = IPC_all_patents_Part2 %>% pull(appln_id),
                                     j = IPC_all_patents_Part2 %>% pull(Subclass))

mat_tech_AI2 %<>% 
  crossprod() %>% 
  as.matrix()

mat_tech_AI3 <- create_sparse_matrix(i = IPC_all_patents_Part3 %>% pull(appln_id),
                                     j = IPC_all_patents_Part3 %>% pull(Subclass))

mat_tech_AI3 %<>% 
  crossprod() %>% 
  as.matrix()

mat_tech_AI4 <- create_sparse_matrix(i = IPC_all_patents_Part4 %>% pull(appln_id),
                                     j = IPC_all_patents_Part4 %>% pull(Subclass))

mat_tech_AI4 %<>% 
  crossprod() %>% 
  as.matrix()

mat_tech_AI5 <- create_sparse_matrix(i = IPC_all_patents_Part5 %>% pull(appln_id),
                                     j = IPC_all_patents_Part5 %>% pull(Subclass))

mat_tech_AI5 %<>% 
  crossprod() %>% 
  as.matrix()

#and the newly created function:
mat_tech_AI_Final3 <- add_matrices_5(mat_tech_AI1, mat_tech_AI2, mat_tech_AI3, mat_tech_AI4, mat_tech_AI5)

#now, we drop the big files and load the remaining ones;
rm(IPC_all_patents_Part1, IPC_all_patents_Part2, IPC_all_patents_Part3, IPC_all_patents_Part4, IPC_all_patents_Part5)

#now we create a function similar to the previous, but for summing 3 matrices:
add_matrices_3 <- function(matrix1, matrix2, matrix3) {
  a <- list(matrix1, matrix2, matrix3)
  cols <- sort(unique(unlist(lapply(a, colnames))))
  rows <- sort(unique(unlist(lapply(a, rownames))))
  out <- array(0, dim = c(length(rows), length(cols)), dimnames = list(rows,cols))
  for (m in a) out[rownames(m), colnames(m)] <- out[rownames(m), colnames(m)] + m
  out
}

#and summ both matrices (the one from the first big file with the one from the second big file)
mat_tech_AI_Final <- add_matrices_3(mat_tech_AI_Final1, mat_tech_AI_Final2, mat_tech_AI_Final3)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#Finally, we save the file. We will use it in the section 1.3.
write.csv2(mat_tech_AI_Final, file = "Data_IPC_4digits/Matrix_IPC_4digits.csv", row.names = TRUE)

#1.2.Technology Space ----
#Now we will create the technology spaces, dividing them in 3 periods;
setwd("C:/Users/Matheus/Desktop")
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

#we pick just the subclass for analysis:
IPC_all_patents_Part1$Subclass <- substr(IPC_all_patents_Part1$ipc_class_symbol,1,4)
IPC_all_patents_Part2$Subclass <- substr(IPC_all_patents_Part2$ipc_class_symbol,1,4)
IPC_all_patents_Part3$Subclass <- substr(IPC_all_patents_Part3$ipc_class_symbol,1,4)
IPC_all_patents_Part4$Subclass <- substr(IPC_all_patents_Part4$ipc_class_symbol,1,4)
IPC_all_patents_Part5$Subclass <- substr(IPC_all_patents_Part5$ipc_class_symbol,1,4)

#we combine the 5 files:
IPC_all_patents_FirstPeriod <- rbind(IPC_all_patents_Part1, IPC_all_patents_Part2, IPC_all_patents_Part3, IPC_all_patents_Part4, IPC_all_patents_Part5)
#and exclude the 5 big ones we just used, so we have back our memory:
rm(IPC_all_patents_Part1, IPC_all_patents_Part2, IPC_all_patents_Part3, IPC_all_patents_Part4, IPC_all_patents_Part5)

#now we apply the 2 functions we created at the beginning of this section:
reg_tech1 <- group_by_applnID(IPC_all_patents_FirstPeriod)
rm(IPC_all_patents_FirstPeriod)
reg_tech1 <- group_by_ctry_and_IPC(reg_tech1)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#and save the final file, so we can use it again in section 1.3. (around the line 330)
write.csv2(reg_tech1, file = "Data_IPC_4digits/reg_tech_FirstPeriod_4digits.csv", row.names = F)

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

#we pick just the subclass for analysis:
IPC_all_patents_Part1$Subclass <- substr(IPC_all_patents_Part1$ipc_class_symbol,1,4)
IPC_all_patents_Part2$Subclass <- substr(IPC_all_patents_Part2$ipc_class_symbol,1,4)
IPC_all_patents_Part3$Subclass <- substr(IPC_all_patents_Part3$ipc_class_symbol,1,4)
IPC_all_patents_Part4$Subclass <- substr(IPC_all_patents_Part4$ipc_class_symbol,1,4)
IPC_all_patents_Part5$Subclass <- substr(IPC_all_patents_Part5$ipc_class_symbol,1,4)

IPC_all_patents_SecondPeriod <- rbind(IPC_all_patents_Part1, IPC_all_patents_Part2, IPC_all_patents_Part3, IPC_all_patents_Part4, IPC_all_patents_Part5)
#and exclude the 5 big ones we just used, so we have back our memory:
rm(IPC_all_patents_Part1, IPC_all_patents_Part2, IPC_all_patents_Part3, IPC_all_patents_Part4, IPC_all_patents_Part5)

#now we apply the 2 functions we created at the beginning of this section:
reg_tech2 <- group_by_applnID(IPC_all_patents_SecondPeriod)
rm(IPC_all_patents_SecondPeriod)
reg_tech2 <- group_by_ctry_and_IPC(reg_tech2)

#and save the final file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
write.csv2(reg_tech2, file = "Data_IPC_4digits/reg_tech_SecondPeriod_4digits.csv", row.names = F)

#third period, divided into 2 parts:
setwd("C:/Users/Matheus/Desktop") 
c <- 120419184-100000000
IPC_all_patents_Part1 <- fread("All_patents_and_IPC_codes_Part1.csv", header = F, nrow = 20000000)
IPC_all_patents_Part2 <- fread("All_patents_and_IPC_codes_Part1.csv", header = F, nrow = 20000000, skip = 20000000)
IPC_all_patents_Part3 <- fread("All_patents_and_IPC_codes_Part1.csv", header = F, nrow = 20000000, skip = 40000000)

names(IPC_all_patents_Part1) <- c("appln_id", "ctry_code", "ipc_class_symbol", "priority_year")
names(IPC_all_patents_Part2) <- c("appln_id", "ctry_code", "ipc_class_symbol", "priority_year")
names(IPC_all_patents_Part3) <- c("appln_id", "ctry_code", "ipc_class_symbol", "priority_year")

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

#and save the final file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
write.csv2(reg_tech5, file = "Data_IPC_4digits/reg_tech_ThirdPeriod_4digits.csv", row.names = F)

#1.3.Calculate the g_tech_AI ----
#Now we load all the files we already saved. We start by loading the janitor library, which is used here for converting
#the first column of data to row names.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(janitor)
rm(list=ls())
#now we load the similarity matrix. For some reason it gives an error when calculating the mat_tech_rel_AI if
#I don't replace the 4 digit codes by a number. So I did that and save it as Matrix_IPC_4digits2.
matrix2 <- read.csv("Data_IPC_4digits/Matrix_IPC_4digits2.csv", sep = ";", header = F)
matrix2 <- matrix2 %>%
  row_to_names(row_number = 1)
matrix <- matrix2[,-1]
rownames(matrix) <- matrix2[,1]
matrix <- as.matrix(matrix)
mat_tech_AI_Final <- matrix

mat_tech_rel_AI <- mat_tech_AI_Final %>% 
  relatedness(method = "cosine")

#now we load the original labels (which I used for generating the number used above)
Labels <- read.csv("Data_IPC_4digits/Labels_Matrix_IPC_4digits.csv", sep = ";", header = T)
Ipc_technology <- read.csv("Data_IPC/ipc_technology.csv", sep = ";", header = T)
Ipc_technology$sim_ipc_maingroup_symbol <- substr(Ipc_technology$ipc_maingroup_symbol,1,4)
Ipc_technology$Label <- Labels$Number[match(Ipc_technology$sim_ipc_maingroup_symbol, Labels$Subclass)]

IPC_names <- Ipc_technology%>%
  select(Label, sector, field_name) %>%
  distinct(Label, .keep_all = TRUE) %>%
  mutate(Label = Label) %>%
  arrange(Label)

g_tech_AI <- mat_tech_rel_AI %>% as_tbl_graph(directed = FALSE) %N>%
  left_join(IPC_names %>% mutate(Label = Label %>% as.character()), by = c("name" = "Label")) %>%
  mutate(dgr = centrality_eigen(weights = weight)) %E>%
  filter(weight >= mean(weight))

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

#Now we read the data per period and calculate the RCAs:
reg_tech1 <- read.csv("Data_IPC_4digits/reg_tech_FirstPeriod_4digits.csv", sep = ";", header = TRUE, dec=",")
reg_tech2 <- read.csv("Data_IPC_4digits/reg_tech_SecondPeriod_4digits.csv", sep = ";", header = TRUE, dec=",")
reg_tech3 <- read.csv("Data_IPC_4digits/reg_tech_ThirdPeriod_4digits.csv", sep = ";", header = TRUE, dec=",")

###First Period:
mat_reg_tech1 <- reg_tech1 %>%
  arrange(Subclass, ctry_code) %>%
  pivot_wider(names_from = Subclass, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

rownames(mat_reg_tech1) <- mat_reg_tech1 %>% pull(ctry_code)

mat_reg_tech1 %<>% select(-ctry_code) %>%
  as.matrix() %>%
  round()

reg_RCA1 <- mat_reg_tech1 %>% location.quotient(binary = TRUE) %>% 
  as.data.frame() %>% 
  rownames_to_column("ctry_code") %>% 
  as_tibble() %>% 
  gather(key = "Subclass", value = "RCA", -ctry_code) %>%
  arrange(ctry_code, Subclass)

Labels$Subclass <- as.vector(Labels$Subclass)
reg_RCA1$name <- Labels$Number[match(reg_RCA1$Subclass, Labels$Subclass)]

###second period:
mat_reg_tech2 <- reg_tech2 %>%
  arrange(Subclass, ctry_code) %>%
  pivot_wider(names_from = Subclass, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

rownames(mat_reg_tech2) <- mat_reg_tech2 %>% pull(ctry_code)

mat_reg_tech2 %<>% select(-ctry_code) %>%
  as.matrix() %>%
  round()

reg_RCA2 <- mat_reg_tech2 %>% location.quotient(binary = TRUE) %>% 
  as.data.frame() %>% 
  rownames_to_column("ctry_code") %>% 
  as_tibble() %>% 
  gather(key = "Subclass", value = "RCA", -ctry_code) %>%
  arrange(ctry_code, Subclass)

###Third Period:
mat_reg_tech3 <- reg_tech3 %>%
  arrange(Subclass, ctry_code) %>%
  pivot_wider(names_from = Subclass, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

rownames(mat_reg_tech3) <- mat_reg_tech3 %>% pull(ctry_code)

mat_reg_tech3 %<>% select(-ctry_code) %>%
  as.matrix() %>%
  round()

reg_RCA3 <- mat_reg_tech3 %>% location.quotient(binary = TRUE) %>% 
  as.data.frame() %>% 
  rownames_to_column("ctry_code") %>% 
  as_tibble() %>% 
  gather(key = "Subclass", value = "RCA", -ctry_code) %>%
  arrange(ctry_code, Subclass)

#1.4.IPC Visualization -----
#Finally, we start with the visualizations. For the Global perspective, considering the whole data, we have:
IPC_Total1 <-g_tech_AI %>%
  ggraph(layout =  coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "grey") + 
  geom_node_point(aes(colour = dgr, size = dgr)) + 
  geom_node_text(aes(label = name), size = 4, repel = TRUE) +
  theme_graph()+
  ggtitle("Technology Space: IPC codes")

IPC_Total2 <- g_tech_AI %>%
  ggraph(layout =  coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.1, colour = "black") + 
  geom_node_point(aes(colour = sector, size = dgr)) + 
# geom_node_text(aes(label = F), size = 4, repel = TRUE) +
  theme_graph()+
  ggtitle("Technology Space: 4-digits IPC codes")

IPC_Total3 <-g_tech_AI %>%
  ggraph(layout =  coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.005, colour = "grey") + 
  geom_node_point(aes(colour = dgr, size = dgr)) + 
# geom_node_text(aes(label = F), size = 4, repel = TRUE) +
  theme_graph()+
  ggtitle("Technology Space: 4-digits IPC codes")

IPC_Total4 <- g_tech_AI %>%
  ggraph(layout =  coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.1, colour = "grey48") + 
  geom_node_point(aes(colour = sector, size = dgr)) + 
  geom_node_text(aes(filter=dgr > .5, label = field_name), colour = "black", size = 5, repel = TRUE) +
  theme_graph()+
  ggtitle("Technology Space: 4-digits IPC codes")


#jpeg("Figures_IPC_4digits/IPC_all_option1_4digits.jpg", width = 14, height = 10, units = 'in', res = 200)
#IPC_Total1
#dev.off()

jpeg("Figures_IPC_4digits/IPC_all_option2_4digits.jpg", width = 14, height = 10, units = 'in', res = 200)
IPC_Total2
dev.off()

jpeg("Figures_IPC_4digits/IPC_all_option3_4digits.jpg", width = 14, height = 10, units = 'in', res = 200)
IPC_Total3
dev.off()

jpeg("Figures_IPC_4digits/IPC_all_option4_4digits.jpg", width = 14, height = 10, units = 'in', res = 200)
IPC_Total4
dev.off()

#Now we start the analysis per country:
#1st period
country_select <- c("CN", "US", "JP", "KR")
i = 1

reg_RCA1$name <- as.character(reg_RCA1$name)

IPC1 <- g_tech_AI %N>%
  left_join(reg_RCA1 %>% filter(ctry_code == country_select[i]) %>% select(-ctry_code), by = c("name" = "name")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.1, colour = "grey22") + 
  geom_node_point(aes(colour = RCA, size = dgr)) + 
  geom_node_text(aes(filter=dgr > .5, label = field_name), colour = "black", size = 5, repel = TRUE) +
  scale_color_gradient(low = "skyblue", high = "red") +
  theme_graph() +
  ggtitle("4-digits IPC Technology Space: China (1974-1988)")

g_tech_AI %N>% 
  arrange(desc(dgr)) %>%
  as_tibble() %>%
  slice(1:10)
write.csv2(reg_RCA1, file = "reg_RCA1_test.csv", row.names = TRUE)

jpeg("Figures_IPC_4digits/IPC_all_CN_persp_Period1.jpg", width = 14, height = 10, units = 'in', res = 200)
IPC1
dev.off()

IPC1B <- g_tech_AI %N>%
  left_join(reg_RCA1 %>% filter(ctry_code == country_select[i]) %>% select(-ctry_code), by = c("name" = "name")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.1, colour = "grey22") + 
  geom_node_point(aes(colour = RCA, size = dgr)) + 
  geom_node_text(aes(filter=RCA > .5, label = field_name), colour = "black", size = 5, repel = TRUE) +
  scale_color_gradient(low = "skyblue", high = "red") +
  theme_graph() +
  ggtitle("4-digits IPC Technology Space: China (1974-1988)")

jpeg("Figures_IPC_4digits/IPC_all_CN_persp_Period1b.jpg", width = 14, height = 10, units = 'in', res = 200)
IPC1B
dev.off()

i = 2
IPC2 <- g_tech_AI %N>%
  left_join(reg_RCA1 %>% filter(ctry_code == country_select[i]) %>% select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.1, colour = "grey48") + 
  geom_node_point(aes(colour = RCA, size = dgr)) + 
  geom_node_text(aes(filter=dgr > .5, label = field_name), colour = "black", size = 5, repel = TRUE) +
  scale_color_gradient(low = "skyblue", high = "red") +
  theme_graph() +
  ggtitle("4-digits IPC Technology Space: USA (1974-1988)")

i = 3
IPC3 <- g_tech_AI %N>%
  left_join(reg_RCA1 %>% filter(ctry_code == country_select[i]) %>% select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.1, colour = "grey48") + 
  geom_node_point(aes(colour = RCA, size = dgr)) + 
  geom_node_text(aes(filter=dgr > .5, label = field_name), colour = "black", size = 5, repel = TRUE) +
  scale_color_gradient(low = "skyblue", high = "red") +
  theme_graph() +
  ggtitle("4-digits IPC Technology Space: Japan (1974-1988)")

i = 4
IPC4 <- g_tech_AI %N>%
  left_join(reg_RCA1 %>% filter(ctry_code == country_select[i]) %>% select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.1, colour = "grey48") + 
  geom_node_point(aes(colour = RCA, size = dgr)) + 
  geom_node_text(aes(filter=dgr > .5, label = field_name), colour = "black", size = 5, repel = TRUE) +
  scale_color_gradient(low = "skyblue", high = "red") +
  theme_graph() +
  ggtitle("4-digits IPC Technology Space: South Korea (1974-1988)")


#For saving the pictures:
jpeg("Figures_IPC_4digits/IPC_all_CN_persp_Period1.jpg", width = 14, height = 10, units = 'in', res = 200)
IPC1
dev.off()

jpeg("Figures_IPC_4digits/IPC_all_US_persp_Period1.jpg", width = 14, height = 10, units = 'in', res = 200)
IPC2
dev.off()

jpeg("Figures_IPC_4digits/IPC_all_JP_persp_Period1.jpg", width = 14, height = 10, units = 'in', res = 200)
IPC3
dev.off()

jpeg("Figures_IPC_4digits/IPC_all_KR_persp_Period1.jpg", width = 14, height = 10, units = 'in', res = 200)
IPC4
dev.off()

#Per Country 2nd period
i = 1
IPC1 <- g_tech_AI %N>%
  left_join(reg_RCA2 %>% filter(ctry_code == country_select[i]) %>% select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.1, colour = "grey48") + 
  geom_node_point(aes(colour = RCA, size = dgr)) + 
# geom_node_text(aes(label = field_name), size = 5, repel = TRUE) +
  scale_color_gradient(low = "skyblue", high = "red") +
  theme_graph() +
  ggtitle("4-digits IPC Technology Space: China (1989-2003)")

jpeg("Figures_IPC_4digits/IPC_all_CN_persp_Period2.jpg", width = 14, height = 10, units = 'in', res = 200)
IPC1
dev.off()

i = 2
IPC2 <- g_tech_AI %N>%
  left_join(reg_RCA2 %>% filter(ctry_code == country_select[i]) %>% select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.1, colour = "grey48") + 
  geom_node_point(aes(colour = RCA, size = dgr)) + 
#  geom_node_text(aes(label = field_name), size = 5, repel = TRUE) +
  scale_color_gradient(low = "skyblue", high = "red") +
  theme_graph() +
  ggtitle("4-digits IPC Technology Space: USA (1989-2003)")

i = 3
IPC3 <- g_tech_AI %N>%
  left_join(reg_RCA2 %>% filter(ctry_code == country_select[i]) %>% select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.1, colour = "grey48") + 
  geom_node_point(aes(colour = RCA, size = dgr)) + 
#  geom_node_text(aes(label = field_name), size = 5, repel = TRUE) +
  scale_color_gradient(low = "skyblue", high = "red") +
  theme_graph() +
  ggtitle("4-digits IPC Technology Space: Japan (1989-2003)")

i = 4
IPC4 <- g_tech_AI %N>%
  left_join(reg_RCA2 %>% filter(ctry_code == country_select[i]) %>% select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.1, colour = "grey48") + 
  geom_node_point(aes(colour = RCA, size = dgr)) + 
#  geom_node_text(aes(label = field_name), size = 5, repel = TRUE) +
  scale_color_gradient(low = "skyblue", high = "red") +
  theme_graph() +
  ggtitle("4-digits IPC Technology Space: South Korea (1989-2003)")

#For saving the pictures:
jpeg("Figures_IPC_4digits/IPC_all_CN_persp_Period2.jpg", width = 14, height = 10, units = 'in', res = 200)
IPC1
dev.off()

jpeg("Figures_IPC_4digits/IPC_all_US_persp_Period2.jpg", width = 14, height = 10, units = 'in', res = 200)
IPC2
dev.off()

jpeg("Figures_IPC_4digits/IPC_all_JP_persp_Period2.jpg", width = 14, height = 10, units = 'in', res = 200)
IPC3
dev.off()

jpeg("Figures_IPC_4digits/IPC_all_KR_persp_Period2.jpg", width = 14, height = 10, units = 'in', res = 200)
IPC4
dev.off()

#Per Country 3rd period
i = 1
IPC1 <- g_tech_AI %N>%
  left_join(reg_RCA3 %>% filter(ctry_code == country_select[i]) %>% select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.1, colour = "grey48") + 
  geom_node_point(aes(colour = RCA, size = dgr)) + 
#  geom_node_text(aes(label = field_name), size = 5, repel = TRUE) +
  scale_color_gradient(low = "skyblue", high = "red") +
  theme_graph() +
  ggtitle("4-digits IPC Technology Space: China (2004-2018)")

jpeg("Figures_IPC_4digits/IPC_all_CN_persp_Period3.jpg", width = 14, height = 10, units = 'in', res = 200)
IPC1
dev.off()

i = 2
IPC2 <- g_tech_AI %N>%
  left_join(reg_RCA3 %>% filter(ctry_code == country_select[i]) %>% select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.1, colour = "grey48") + 
  geom_node_point(aes(colour = RCA, size = dgr)) + 
#  geom_node_text(aes(label = field_name), size = 5, repel = TRUE) +
  scale_color_gradient(low = "skyblue", high = "red") +
  theme_graph() +
  ggtitle("4-digits IPC Technology Space: USA (2004-2018)")

i = 3
IPC3 <- g_tech_AI %N>%
  left_join(reg_RCA3 %>% filter(ctry_code == country_select[i]) %>% select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.1, colour = "grey48") + 
  geom_node_point(aes(colour = RCA, size = dgr)) + 
#  geom_node_text(aes(label = field_name), size = 5, repel = TRUE) +
  scale_color_gradient(low = "skyblue", high = "red") +
  theme_graph() +
  ggtitle("4-digits IPC Technology Space: Japan (2004-2018)")

i = 4
IPC4 <- g_tech_AI %N>%
  left_join(reg_RCA3 %>% filter(ctry_code == country_select[i]) %>% select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.1, colour = "grey48") + 
  geom_node_point(aes(colour = RCA, size = dgr)) + 
#  geom_node_text(aes(label = field_name), size = 5, repel = TRUE) +
  scale_color_gradient(low = "skyblue", high = "red") +
  theme_graph() +
  ggtitle("4-digits IPC Technology Space: South Korea (2004-2018)")

#For saving the pictures:
jpeg("Figures_IPC_4digits/IPC_all_CN_persp_Period3.jpg", width = 14, height = 10, units = 'in', res = 200)
IPC1
dev.off()

jpeg("Figures_IPC_4digits/IPC_all_US_persp_Period3.jpg", width = 14, height = 10, units = 'in', res = 200)
IPC2
dev.off()

jpeg("Figures_IPC_4digits/IPC_all_JP_persp_Period3.jpg", width = 14, height = 10, units = 'in', res = 200)
IPC3
dev.off()

jpeg("Figures_IPC_4digits/IPC_all_KR_persp_Period3.jpg", width = 14, height = 10, units = 'in', res = 200)
IPC4
dev.off()
