library(tidyverse) # Collection of all the good stuff like dplyr, ggplot2 ect.
library(magrittr) # For extra-piping operators (eg. %<>%)

# Network specific
library(tidygraph) # For tidy-style graph manipulation
library(ggraph) # For ggplot2 style graph plotting

library(EconGeo) # Economic Geography functions
library(data.table) #for reading the big files using fread and for replacing countries names (by AI_pat for example)

library(netrankr) #library for calculating pagerank related indicators (i.e. centrality_closeness_harmonic and centrality_closeness_residual)

#1.IPC data -----
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

#now we apply the 2 functions we created at the beginning of this section:
reg_tech1 <- group_by_applnID(IPC_all_patents_FirstPeriod)
rm(IPC_all_patents_FirstPeriod)
reg_tech1 <- group_by_ctry_and_IPC(reg_tech1)

#and save the final file, so we can use it again in section 1.3. (around the line 330)
write.csv2(reg_tech1, file = "Data_Final_code/reg_tech_FirstPeriod.csv", row.names = F)

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

reg_tech2 <- group_by_applnID(IPC_all_patents_SecondPeriod)
rm(IPC_all_patents_SecondPeriod)
reg_tech2 <- group_by_ctry_and_IPC(reg_tech2)
write.csv2(reg_tech2, file = "Data_Final_code/reg_tech_SecondPeriod.csv", row.names = F)

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

#1.4.IPC Visualization -----
#Finally, we start with the visualizations. For the Global perspective, considering the whole data, we have:
Global_technological_space <- g_tech_AI %>%
  ggraph(layout =  coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "grey") + 
  geom_node_point(aes(colour = sector, size = dgr)) + 
  geom_node_text(aes(label = field_name), size = 4, repel = TRUE) +
  theme_graph()+
  ggtitle("Technology Space: IPC codes")

jpeg("Figures_IPC/Global_technological_space.jpg", width = 14, height = 10, units = 'in', res = 200)
IPC_Total1
dev.off()

jpeg("Figures_IPC/IPC_all_option2.jpg", width = 14, height = 10, units = 'in', res = 200)
IPC_Total2
dev.off()

#Now we start the analysis per country:
#1st period
country_select <- c("CN", "US", "JP", "KR")
i = 1
IPC1 <- g_tech_AI %N>%
  left_join(reg_RCA1 %>% filter(ctry_code == country_select[i]) %>% select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "grey") + 
  geom_node_point(aes(colour = RCA, size = dgr)) + 
  geom_node_text(aes(label = field_name), size = 5, repel = TRUE) +
  scale_color_gradient(low = "skyblue", high = "red") +
  theme_graph() +
  ggtitle("IPC Technology Space: China (1974-1988)")

#option "b" for China
IPC1b <- g_tech_AI %N>%
  left_join(reg_RCA1 %>% filter(ctry_code == country_select[i]) %>% select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "grey") + 
  geom_node_point(aes(colour = RCA, size = dgr)) + 
  geom_node_text(aes(filter=RCA > .9, label = field_name), size = 5, repel = TRUE) +
  scale_color_gradient(low = "skyblue", high = "red") +
  theme_graph() +
  ggtitle("IPC Technology Space: China (1974-1988)")

CN_1st <- g_tech_AI %N>%
  left_join(reg_RCA1 %>% filter(ctry_code == country_select[i]) %>% select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  filter(RCA == 1) %>%
  mutate(n_neighbors = local_size(mindist = 1),
         weighted_degree = centrality_degree() / local_ave_degree(),
         triangles = local_triangles(),
         #centrality_alpha= centrality_alpha(),
         centrality_authority = centrality_authority(),
         centrality_betweenness = centrality_betweenness(),
         #centrality_power=centrality_power(),
         centrality_closeness = centrality_closeness(),
         centrality_eigen = centrality_eigen(),
         centrality_hub= centrality_hub(),
         centrality_subgraph=centrality_subgraph(),
         centrality_degree=centrality_degree(),
         centrality_closeness_harmonic=centrality_closeness_harmonic(),
         centrality_closeness_residual=centrality_closeness_residual(),
         centrality_betweenness_network=centrality_betweenness_network(),
         #centrality_information=centrality_information()
  ) %>%
  as_tibble() 
CN_1st$Country <- country_select[i]

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

US_1st <- g_tech_AI %N>%
  left_join(reg_RCA1 %>% filter(ctry_code == country_select[i]) %>% select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  filter(RCA == 1) %>%
  mutate(n_neighbors = local_size(mindist = 1),
         weighted_degree = centrality_degree() / local_ave_degree(),
         triangles = local_triangles(),
         #centrality_alpha= centrality_alpha(),
         centrality_authority = centrality_authority(),
         centrality_betweenness = centrality_betweenness(),
         #centrality_power=centrality_power(),
         centrality_closeness = centrality_closeness(),
         centrality_eigen = centrality_eigen(),
         centrality_hub= centrality_hub(),
         centrality_subgraph=centrality_subgraph(),
         centrality_degree=centrality_degree(),
         centrality_closeness_harmonic=centrality_closeness_harmonic(),
         centrality_closeness_residual=centrality_closeness_residual(),
         centrality_betweenness_network=centrality_betweenness_network(),
         #centrality_information=centrality_information()
  ) %>%
  as_tibble() 
US_1st$Country <- country_select[i]

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

JP_1st <- g_tech_AI %N>%
  left_join(reg_RCA1 %>% filter(ctry_code == country_select[i]) %>% select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  filter(RCA == 1) %>%
  mutate(n_neighbors = local_size(mindist = 1),
         weighted_degree = centrality_degree() / local_ave_degree(),
         triangles = local_triangles(),
         #centrality_alpha= centrality_alpha(),
         centrality_authority = centrality_authority(),
         centrality_betweenness = centrality_betweenness(),
         #centrality_power=centrality_power(),
         centrality_closeness = centrality_closeness(),
         centrality_eigen = centrality_eigen(),
         centrality_hub= centrality_hub(),
         centrality_subgraph=centrality_subgraph(),
         centrality_degree=centrality_degree(),
         centrality_closeness_harmonic=centrality_closeness_harmonic(),
         centrality_closeness_residual=centrality_closeness_residual(),
         centrality_betweenness_network=centrality_betweenness_network(),
         #centrality_information=centrality_information()
  ) %>%
  as_tibble() 
JP_1st$Country <- country_select[i]

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

KR_1st <- g_tech_AI %N>%
  left_join(reg_RCA1 %>% filter(ctry_code == country_select[i]) %>% select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  filter(RCA == 1) %>%
  mutate(n_neighbors = local_size(mindist = 1),
         weighted_degree = centrality_degree() / local_ave_degree(),
         triangles = local_triangles(),
         #centrality_alpha= centrality_alpha(),
         centrality_authority = centrality_authority(),
         centrality_betweenness = centrality_betweenness(),
         #centrality_power=centrality_power(),
         centrality_closeness = centrality_closeness(),
         centrality_eigen = centrality_eigen(),
         centrality_hub= centrality_hub(),
         centrality_subgraph=centrality_subgraph(),
         centrality_degree=centrality_degree(),
         centrality_closeness_harmonic=centrality_closeness_harmonic(),
         centrality_closeness_residual=centrality_closeness_residual(),
         centrality_betweenness_network=centrality_betweenness_network(),
         #centrality_information=centrality_information()
  ) %>%
  as_tibble() 
KR_1st$Country <- country_select[i]

NetworkMetrics1st <- rbind(CN_1st,US_1st,JP_1st,KR_1st)
NetworkMetrics1st$Period <- "1st"

#For saving the pictures:
jpeg("Figures_IPC/IPC_all_CN_persp_Period1.jpg", width = 14, height = 10, units = 'in', res = 200)
IPC1
dev.off()

jpeg("Figures_IPC/IPC_all_CN_persp_Period1_optionB.jpg", width = 14, height = 10, units = 'in', res = 200)
IPC1b
dev.off()

jpeg("Figures_IPC/IPC_all_US_persp_Period1.jpg", width = 14, height = 10, units = 'in', res = 200)
IPC2
dev.off()

jpeg("Figures_IPC/IPC_all_JP_persp_Period1.jpg", width = 14, height = 10, units = 'in', res = 200)
IPC3
dev.off()

jpeg("Figures_IPC/IPC_all_KR_persp_Period1.jpg", width = 14, height = 10, units = 'in', res = 200)
IPC4
dev.off()

#Per Country 2nd period
i = 1
IPC1 <- g_tech_AI %N>%
  left_join(reg_RCA2 %>% filter(ctry_code == country_select[i]) %>% select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "grey") + 
  geom_node_point(aes(colour = RCA, size = dgr)) + 
  geom_node_text(aes(label = field_name), size = 5, repel = TRUE) +
  scale_color_gradient(low = "skyblue", high = "red") +
  theme_graph() +
  ggtitle("IPC Technology Space: China (1989-2003)")

IPC1b <- g_tech_AI %N>%
  left_join(reg_RCA2 %>% filter(ctry_code == country_select[i]) %>% select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "grey") + 
  geom_node_point(aes(colour = RCA, size = dgr)) + 
  geom_node_text(aes(filter=RCA > .9, label = field_name), size = 5, repel = TRUE) +
  scale_color_gradient(low = "skyblue", high = "red") +
  theme_graph() +
  ggtitle("IPC Technology Space: China (1989-2003)")

CN_2nd <- g_tech_AI %N>%
  left_join(reg_RCA2 %>% filter(ctry_code == country_select[i]) %>% select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  filter(RCA == 1) %>%
  mutate(n_neighbors = local_size(mindist = 1),
         weighted_degree = centrality_degree() / local_ave_degree(),
         triangles = local_triangles(),
         #centrality_alpha= centrality_alpha(),
         centrality_authority = centrality_authority(),
         centrality_betweenness = centrality_betweenness(),
         #centrality_power=centrality_power(),
         centrality_closeness = centrality_closeness(),
         centrality_eigen = centrality_eigen(),
         centrality_hub= centrality_hub(),
         centrality_subgraph=centrality_subgraph(),
         centrality_degree=centrality_degree(),
         centrality_closeness_harmonic=centrality_closeness_harmonic(),
         centrality_closeness_residual=centrality_closeness_residual(),
         centrality_betweenness_network=centrality_betweenness_network(),
         #centrality_information=centrality_information()
  ) %>%
  as_tibble() 
CN_2nd$Country <- country_select[i]

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

US_2nd <- g_tech_AI %N>%
  left_join(reg_RCA2 %>% filter(ctry_code == country_select[i]) %>% select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  filter(RCA == 1) %>%
  mutate(n_neighbors = local_size(mindist = 1),
         weighted_degree = centrality_degree() / local_ave_degree(),
         triangles = local_triangles(),
         #centrality_alpha= centrality_alpha(),
         centrality_authority = centrality_authority(),
         centrality_betweenness = centrality_betweenness(),
         #centrality_power=centrality_power(),
         centrality_closeness = centrality_closeness(),
         centrality_eigen = centrality_eigen(),
         centrality_hub= centrality_hub(),
         centrality_subgraph=centrality_subgraph(),
         centrality_degree=centrality_degree(),
         centrality_closeness_harmonic=centrality_closeness_harmonic(),
         centrality_closeness_residual=centrality_closeness_residual(),
         centrality_betweenness_network=centrality_betweenness_network(),
         #centrality_information=centrality_information()
  ) %>%
  as_tibble() 
US_2nd$Country <- country_select[i]

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

JP_2nd <- g_tech_AI %N>%
  left_join(reg_RCA2 %>% filter(ctry_code == country_select[i]) %>% select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  filter(RCA == 1) %>%
  mutate(n_neighbors = local_size(mindist = 1),
         weighted_degree = centrality_degree() / local_ave_degree(),
         triangles = local_triangles(),
         #centrality_alpha= centrality_alpha(),
         centrality_authority = centrality_authority(),
         centrality_betweenness = centrality_betweenness(),
         #centrality_power=centrality_power(),
         centrality_closeness = centrality_closeness(),
         centrality_eigen = centrality_eigen(),
         centrality_hub= centrality_hub(),
         centrality_subgraph=centrality_subgraph(),
         centrality_degree=centrality_degree(),
         centrality_closeness_harmonic=centrality_closeness_harmonic(),
         centrality_closeness_residual=centrality_closeness_residual(),
         centrality_betweenness_network=centrality_betweenness_network(),
         #centrality_information=centrality_information()
  ) %>%
  as_tibble() 
JP_2nd$Country <- country_select[i]

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

KR_2nd <- g_tech_AI %N>%
  left_join(reg_RCA2 %>% filter(ctry_code == country_select[i]) %>% select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  filter(RCA == 1) %>%
  mutate(n_neighbors = local_size(mindist = 1),
         weighted_degree = centrality_degree() / local_ave_degree(),
         triangles = local_triangles(),
         #centrality_alpha= centrality_alpha(),
         centrality_authority = centrality_authority(),
         centrality_betweenness = centrality_betweenness(),
         #centrality_power=centrality_power(),
         centrality_closeness = centrality_closeness(),
         centrality_eigen = centrality_eigen(),
         centrality_hub= centrality_hub(),
         centrality_subgraph=centrality_subgraph(),
         centrality_degree=centrality_degree(),
         centrality_closeness_harmonic=centrality_closeness_harmonic(),
         centrality_closeness_residual=centrality_closeness_residual(),
         centrality_betweenness_network=centrality_betweenness_network(),
         #centrality_information=centrality_information()
  ) %>%
  as_tibble() 
KR_2nd$Country <- country_select[i]

NetworkMetrics2nd <- rbind(CN_2nd,US_2nd,JP_2nd,KR_2nd)
NetworkMetrics2nd$Period <- "2nd"

#For saving the pictures:
jpeg("Figures_IPC/IPC_all_CN_persp_Period2.jpg", width = 14, height = 10, units = 'in', res = 200)
IPC1
dev.off()

jpeg("Figures_IPC/IPC_all_CN_persp_Period2_optionB.jpg", width = 14, height = 10, units = 'in', res = 200)
IPC1b
dev.off()

jpeg("Figures_IPC/IPC_all_US_persp_Period2.jpg", width = 14, height = 10, units = 'in', res = 200)
IPC2
dev.off()

jpeg("Figures_IPC/IPC_all_JP_persp_Period2.jpg", width = 14, height = 10, units = 'in', res = 200)
IPC3
dev.off()

jpeg("Figures_IPC/IPC_all_KR_persp_Period2.jpg", width = 14, height = 10, units = 'in', res = 200)
IPC4
dev.off()

#Per Country 3rd period
i = 1
IPC1 <- g_tech_AI %N>%
  left_join(reg_RCA3 %>% filter(ctry_code == country_select[i]) %>% select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "grey") + 
  geom_node_point(aes(colour = RCA, size = dgr)) + 
  geom_node_text(aes(label = field_name), size = 5, repel = TRUE) +
  scale_color_gradient(low = "skyblue", high = "red") +
  theme_graph() +
  ggtitle("IPC Technology Space: China (2004-2018)")

IPC1b <- g_tech_AI %N>%
  left_join(reg_RCA3 %>% filter(ctry_code == country_select[i]) %>% select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "grey") + 
  geom_node_point(aes(colour = RCA, size = dgr)) + 
  geom_node_text(aes(filter=RCA > .9, label = field_name), size = 5, repel = TRUE) +
  scale_color_gradient(low = "skyblue", high = "red") +
  theme_graph() +
  ggtitle("IPC Technology Space: China (2004-2018)")

CN_3rd <- g_tech_AI %N>%
  left_join(reg_RCA3 %>% filter(ctry_code == country_select[i]) %>% select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  filter(RCA == 1) %>%
  mutate(n_neighbors = local_size(mindist = 1),
         weighted_degree = centrality_degree() / local_ave_degree(),
         triangles = local_triangles(),
         #centrality_alpha= centrality_alpha(),
         centrality_authority = centrality_authority(),
         centrality_betweenness = centrality_betweenness(),
         #centrality_power=centrality_power(),
         centrality_closeness = centrality_closeness(),
         centrality_eigen = centrality_eigen(),
         centrality_hub= centrality_hub(),
         centrality_subgraph=centrality_subgraph(),
         centrality_degree=centrality_degree(),
         centrality_closeness_harmonic=centrality_closeness_harmonic(),
         centrality_closeness_residual=centrality_closeness_residual(),
         centrality_betweenness_network=centrality_betweenness_network(),
         #centrality_information=centrality_information()
  ) %>%
  as_tibble() 
CN_3rd$Country <- country_select[i]

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

US_3rd <- g_tech_AI %N>%
  left_join(reg_RCA3 %>% filter(ctry_code == country_select[i]) %>% select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  filter(RCA == 1) %>%
  mutate(n_neighbors = local_size(mindist = 1),
         weighted_degree = centrality_degree() / local_ave_degree(),
         triangles = local_triangles(),
         #centrality_alpha= centrality_alpha(),
         centrality_authority = centrality_authority(),
         centrality_betweenness = centrality_betweenness(),
         #centrality_power=centrality_power(),
         centrality_closeness = centrality_closeness(),
         centrality_eigen = centrality_eigen(),
         centrality_hub= centrality_hub(),
         centrality_subgraph=centrality_subgraph(),
         centrality_degree=centrality_degree(),
         centrality_closeness_harmonic=centrality_closeness_harmonic(),
         centrality_closeness_residual=centrality_closeness_residual(),
         centrality_betweenness_network=centrality_betweenness_network(),
         #centrality_information=centrality_information()
  ) %>%
  as_tibble() 
US_3rd$Country <- country_select[i]

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

JP_3rd <- g_tech_AI %N>%
  left_join(reg_RCA3 %>% filter(ctry_code == country_select[i]) %>% select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  filter(RCA == 1) %>%
  mutate(n_neighbors = local_size(mindist = 1),
         weighted_degree = centrality_degree() / local_ave_degree(),
         triangles = local_triangles(),
         #centrality_alpha= centrality_alpha(),
         centrality_authority = centrality_authority(),
         centrality_betweenness = centrality_betweenness(),
         #centrality_power=centrality_power(),
         centrality_closeness = centrality_closeness(),
         centrality_eigen = centrality_eigen(),
         centrality_hub= centrality_hub(),
         centrality_subgraph=centrality_subgraph(),
         centrality_degree=centrality_degree(),
         centrality_closeness_harmonic=centrality_closeness_harmonic(),
         centrality_closeness_residual=centrality_closeness_residual(),
         centrality_betweenness_network=centrality_betweenness_network(),
         #centrality_information=centrality_information()
  ) %>%
  as_tibble() 
JP_3rd$Country <- country_select[i]

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

KR_3rd <- g_tech_AI %N>%
  left_join(reg_RCA3 %>% filter(ctry_code == country_select[i]) %>% select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  filter(RCA == 1) %>%
  mutate(n_neighbors = local_size(mindist = 1),
         weighted_degree = centrality_degree() / local_ave_degree(),
         triangles = local_triangles(),
         #centrality_alpha= centrality_alpha(),
         centrality_authority = centrality_authority(),
         centrality_betweenness = centrality_betweenness(),
         #centrality_power=centrality_power(),
         centrality_closeness = centrality_closeness(),
         centrality_eigen = centrality_eigen(),
         centrality_hub= centrality_hub(),
         centrality_subgraph=centrality_subgraph(),
         centrality_degree=centrality_degree(),
         centrality_closeness_harmonic=centrality_closeness_harmonic(),
         centrality_closeness_residual=centrality_closeness_residual(),
         centrality_betweenness_network=centrality_betweenness_network(),
         #centrality_information=centrality_information()
  ) %>%
  as_tibble() 
KR_3rd$Country <- country_select[i]

NetworkMetrics3rd <- rbind(CN_3rd,US_3rd,JP_3rd,KR_3rd)
NetworkMetrics3rd$Period <- "3rd"
NetworkMetrics <- rbind(NetworkMetrics1st,NetworkMetrics2nd,NetworkMetrics3rd)
write.csv2(NetworkMetrics, file = "Data_Final_code/NetworkMetrics.csv", row.names = F)

Summary_NetworkMetrics_avr <- aggregate(NetworkMetrics[,c(4:18)], list(NetworkMetrics$Country, NetworkMetrics$Period), mean)
Summary_NetworkMetrics_sum <- aggregate(NetworkMetrics[,c(4:18)], list(NetworkMetrics$Country, NetworkMetrics$Period), sum)
Summary_NetworkMetrics <- rbind(Summary_NetworkMetrics_avr, Summary_NetworkMetrics_sum)
write.csv2(Summary_NetworkMetrics, file = "Data_Final_code/Summary_NetworkMetrics.csv", row.names = F)

#For saving the pictures:
jpeg("Figures_IPC/IPC_all_CN_persp_Period3.jpg", width = 14, height = 10, units = 'in', res = 200)
IPC1
dev.off()

jpeg("Figures_IPC/IPC_all_CN_persp_Period3_optionB.jpg", width = 14, height = 10, units = 'in', res = 200)
IPC1b
dev.off()

jpeg("Figures_IPC/IPC_all_US_persp_Period3.jpg", width = 14, height = 10, units = 'in', res = 200)
IPC2
dev.off()

jpeg("Figures_IPC/IPC_all_JP_persp_Period3.jpg", width = 14, height = 10, units = 'in', res = 200)
IPC3
dev.off()

jpeg("Figures_IPC/IPC_all_KR_persp_Period3.jpg", width = 14, height = 10, units = 'in', res = 200)
IPC4
dev.off()

#1.5.AI perspective-----
#For the AI perspective we use the same code as before. The only difference is that we replace the countries names
#with the name "AI_pat" in every AI-related patent. In this way, we can use the "AI_pat" to measure the 
#specialization of AI patents as if they were countries.

#Let's start with what we had on section 1.2.1.:

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

IPC_all_patents_Part1 <- IPC_all_patents_Part1[, c((-4), (-5))]
IPC_all_patents_Part2 <- IPC_all_patents_Part2[, c((-4), (-5))]
IPC_all_patents_Part3 <- IPC_all_patents_Part3[, c((-4), (-5))]

#we combine the 3 files:
IPC_all_patents_FirstPeriod <- rbind(IPC_all_patents_Part1, IPC_all_patents_Part2, IPC_all_patents_Part3)
#and exclude the 3 big ones we just used, so we have back our memory:
rm(IPC_all_patents_Part1, IPC_all_patents_Part2, IPC_all_patents_Part3)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

####   ### #######   ### ###
#Now we insert our AI data. This is the only part that changes from the previous code.
####   ### #######   ### ###

patents_AI_specific <- read.csv("Data_Final_code/IPCs_AI.csv", sep = ";", header = TRUE, dec=",")
patents_AI_specific$ctry_code <- as.vector(patents_AI_specific$ctry_code)
patents_AI_specific$ctry_code <- "AI_pat"

#now we replace the AI data on the IPC dataset;
setDT(patents_AI_specific)
setDT(IPC_all_patents_FirstPeriod)
IPC_all_patents_FirstPeriod[patents_AI_specific, on = c("appln_id"), ctry_code := i.ctry_code]

####   ### #######   ### ###
#now we go back to our old code and apply the 2 functions we had already created:
####   ### #######   ### ###
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

reg_tech1 <- group_by_applnID(IPC_all_patents_FirstPeriod)
rm(IPC_all_patents_FirstPeriod)
reg_tech1 <- group_by_ctry_and_IPC(reg_tech1)

#and save the final file, so we can use it again in section 1.3. (around the line 330)
write.csv2(reg_tech1, file = "Data_Final_code/reg_techAI_FirstPeriod.csv", row.names = F)

###First Period:
mat_reg_tech1 <- reg_tech1 %>%
  arrange(techn_field_nr, ctry_code) %>%
  pivot_wider(names_from = techn_field_nr, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

rownames(mat_reg_tech1) <- mat_reg_tech1 %>% pull(ctry_code)

mat_reg_tech1 %<>% select(-ctry_code) %>%
  as.matrix() %>%
  round()

reg_RCA1 <- mat_reg_tech1 %>% location.quotient(binary = TRUE) %>% 
  as.data.frame() %>% 
  rownames_to_column("ctry_code") %>% 
  as_tibble() %>% 
  gather(key = "techn_field_nr", value = "RCA", -ctry_code) %>%
  arrange(ctry_code, techn_field_nr)

country_select <- c("AI_pat")
i = 1
IPC_AI <- g_tech_AI %N>%
  left_join(reg_RCA1 %>% filter(ctry_code == country_select[i]) %>% select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "grey") + 
  geom_node_point(aes(colour = RCA, size = dgr)) + 
  geom_node_text(aes(filter=RCA > .9, label = field_name), size = 5, repel = TRUE) +
  scale_color_gradient(low = "skyblue", high = "red") +
  theme_graph() +
  ggtitle("Technology Space: AI patents (1974-1988)")

jpeg("Figures_IPC/IPC_all_AIpatents_specific_Period1.jpg", width = 14, height = 10, units = 'in', res = 200)
IPC_AI
dev.off()

#For the second period we repeat again the code from the related section (1.2.3.):
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

IPC_all_patents_Part1 <- IPC_all_patents_Part1[, c((-4), (-5))]
IPC_all_patents_Part2 <- IPC_all_patents_Part2[, c((-4), (-5))]
IPC_all_patents_Part3 <- IPC_all_patents_Part3[, c((-4), (-5))]

#we combine the 3 files:
IPC_all_patents_SecondPeriod <- rbind(IPC_all_patents_Part1, IPC_all_patents_Part2, IPC_all_patents_Part3)
#and exclude the 3 big ones we just used, so we have back our memory:
rm(IPC_all_patents_Part1, IPC_all_patents_Part2, IPC_all_patents_Part3)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

####   ### #######   ### ###
#Now we insert our AI data. This is the only part that changes from the previous code.
####   ### #######   ### ###

patents_AI_specific <- read.csv("Data_Final_code/IPCs_AI.csv", sep = ";", header = TRUE, dec=",")
patents_AI_specific$ctry_code <- as.vector(patents_AI_specific$ctry_code)
patents_AI_specific$ctry_code <- "AI_pat"
#I want to select some patents on these IPC_all_patents dataset and change the patent_office to, let's say, AI;
setDT(patents_AI_specific)
setDT(IPC_all_patents_SecondPeriod)
IPC_all_patents_SecondPeriod[patents_AI_specific, on = c("appln_id"), ctry_code := i.ctry_code]

####   ### #######   ### ###
#now we go back to our old code and apply the 2 functions we had already created:
####   ### #######   ### ###

reg_tech2 <- group_by_applnID(IPC_all_patents_SecondPeriod)
rm(IPC_all_patents_SecondPeriod)
reg_tech2 <- group_by_ctry_and_IPC(reg_tech2)

write.csv2(reg_tech2, file = "Data_Final_code/reg_techAI_SecondPeriod.csv", row.names = F)

###Second Period:
mat_reg_tech2 <- reg_tech2 %>%
  arrange(techn_field_nr, ctry_code) %>%
  pivot_wider(names_from = techn_field_nr, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

rownames(mat_reg_tech2) <- mat_reg_tech2 %>% pull(ctry_code)

mat_reg_tech2 %<>% select(-ctry_code) %>%
  as.matrix() %>%
  round()

reg_RCA2 <- mat_reg_tech2 %>% location.quotient(binary = TRUE) %>% 
  as.data.frame() %>% 
  rownames_to_column("ctry_code") %>% 
  as_tibble() %>% 
  gather(key = "techn_field_nr", value = "RCA", -ctry_code) %>%
  arrange(ctry_code, techn_field_nr)

country_select <- c("AI_pat")
i = 1
IPC_AI <- g_tech_AI %N>%
  left_join(reg_RCA2 %>% filter(ctry_code == country_select[i]) %>% select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "grey") + 
  geom_node_point(aes(colour = RCA, size = dgr)) + 
  geom_node_text(aes(filter=RCA > .9, label = field_name), size = 5, repel = TRUE) +
  scale_color_gradient(low = "skyblue", high = "red") +
  theme_graph() +
  ggtitle("Technology Space: AI patents (1989-2003)")

jpeg("Figures_IPC/IPC_all_AIpatents_specific_Period2.jpg", width = 14, height = 10, units = 'in', res = 200)
IPC_AI
dev.off()

#And finnaly For the third period (section 1.2.3.), which goes from 2004 to 2018:
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

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

####   ### #######   ### ###
#Now we insert our AI data. This is the only part that changes from the previous code.
####   ### #######   ### ###

patents_AI_specific <- read.csv("Data_Final_code/IPCs_AI.csv", sep = ";", header = TRUE, dec=",")
patents_AI_specific$ctry_code <- as.vector(patents_AI_specific$ctry_code)
patents_AI_specific$ctry_code <- "AI_pat"

setDT(patents_AI_specific)
setDT(IPC_all_patents_Part1)
setDT(IPC_all_patents_Part2)
setDT(IPC_all_patents_Part3)
IPC_all_patents_Part1[patents_AI_specific, on = c("appln_id"), ctry_code := i.ctry_code]
IPC_all_patents_Part2[patents_AI_specific, on = c("appln_id"), ctry_code := i.ctry_code]
IPC_all_patents_Part3[patents_AI_specific, on = c("appln_id"), ctry_code := i.ctry_code]

####   ### #######   ### ###
#now we go back to our old code and apply the 2 functions we had already created:
####   ### #######   ### ###

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
write.csv2(tabledata2, file = "Data_Final_code/reg_techAI_ThirdPeriod.csv", row.names = F)

###Third Period:
reg_tech3 <- tabledata2

mat_reg_tech3 <- reg_tech3 %>%
  arrange(techn_field_nr, ctry_code) %>%
  pivot_wider(names_from = techn_field_nr, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

rownames(mat_reg_tech3) <- mat_reg_tech3 %>% pull(ctry_code)

mat_reg_tech3 %<>% select(-ctry_code) %>%
  as.matrix() %>%
  round()

reg_RCA3 <- mat_reg_tech3 %>% location.quotient(binary = TRUE) %>% 
  as.data.frame() %>% 
  rownames_to_column("ctry_code") %>% 
  as_tibble() %>% 
  gather(key = "techn_field_nr", value = "RCA", -ctry_code) %>%
  arrange(ctry_code, techn_field_nr)

country_select <- c("AI_pat")
i = 1
IPC_AI <- g_tech_AI %N>%
  left_join(reg_RCA3 %>% filter(ctry_code == country_select[i]) %>% select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "grey") + 
  geom_node_point(aes(colour = RCA, size = dgr)) + 
  geom_node_text(aes(filter=RCA > .9, label = field_name), size = 5, repel = TRUE) +
  scale_color_gradient(low = "skyblue", high = "red") +
  theme_graph() +
  ggtitle("Technology Space: AI patents (2004-2018)")

jpeg("Figures_IPC/IPC_all_AIpatents_specific_Period3.jpg", width = 14, height = 10, units = 'in', res = 200)
IPC_AI
dev.off()
