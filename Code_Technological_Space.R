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
#1.1. First part -----
rm(list=ls())
#setwd("C:/Users/mathe/OneDrive/?rea de Trabalho")
setwd("C:/Users/Matheus/Desktop") #for loading the big file

#Now we will load a first big file containing all priorities and their related Nace codes published in or after 2004. This file has
#58,935,336 lines;
c <- 58935336-40000000

Nace_all_patents_Part1 <- fread("All_patents_and_Naces_Part1.csv", header = F, nrow = 20000000)
Nace_all_patents_Part2 <- fread("All_patents_and_Naces_Part1.csv", header = F, nrow = 20000000, skip = 20000000)
Nace_all_patents_Part3 <- fread("All_patents_and_Naces_Part1.csv", header = F, nrow = c, skip = 40000000)

names(Nace_all_patents_Part1) <- c("appln_id", "ctry_code", "nace2_code", "weight", "priority_year")
names(Nace_all_patents_Part2) <- c("appln_id", "ctry_code", "nace2_code", "weight", "priority_year")
names(Nace_all_patents_Part3) <- c("appln_id", "ctry_code", "nace2_code", "weight", "priority_year")

#I'll copy the country codes to another columns, so I don't loose this information when I replace the country codes by the code "AI_pat" (which
#I'm doing to identify the AI domain)
Nace_all_patents_Part1$ctry_code2 <- Nace_all_patents_Part1$ctry_code
Nace_all_patents_Part2$ctry_code2 <- Nace_all_patents_Part2$ctry_code
Nace_all_patents_Part3$ctry_code2 <- Nace_all_patents_Part3$ctry_code

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

add_matrices_3 <- function(matrix1, matrix2, matrix3) {
  a <- list(matrix1, matrix2, matrix3)
  cols <- sort(unique(unlist(lapply(a, colnames))))
  rows <- sort(unique(unlist(lapply(a, rownames))))
  out <- array(0, dim = c(length(rows), length(cols)), dimnames = list(rows,cols))
  for (m in a) out[rownames(m), colnames(m)] <- out[rownames(m), colnames(m)] + m
  out
}

mat_tech_AI_Final1 <- add_matrices_3(mat_tech_AI1, mat_tech_AI2, mat_tech_AI3)

#now, we drop the big files and load the remaining ones;
rm(Nace_all_patents_Part1)
rm(Nace_all_patents_Part2)
rm(Nace_all_patents_Part3)

#Now we will load the second big file containing all priorities and their related Nace codes published in or before 2003. This file has
#45,233,329 lines;
setwd("C:/Users/Matheus/Desktop")
c <- 45233329 -40000000
Nace_all_patents_Part1 <- fread("All_patents_and_Naces_Part2.csv", header = F, nrow = 20000000)
Nace_all_patents_Part2 <- fread("All_patents_and_Naces_Part2.csv", header = F, nrow = 20000000, skip = 20000000)
Nace_all_patents_Part3 <- fread("All_patents_and_Naces_Part2.csv", header = F, nrow = c, skip = 40000000)

names(Nace_all_patents_Part1) <- c("appln_id", "ctry_code", "nace2_code", "weight", "priority_year")
names(Nace_all_patents_Part2) <- c("appln_id", "ctry_code", "nace2_code", "weight", "priority_year")
names(Nace_all_patents_Part3) <- c("appln_id", "ctry_code", "nace2_code", "weight", "priority_year")

#I'll copy the country codes to another columns, so I don't loose this information when I replace the country codes by the code "AI_pat" (which
#I'm doing to identify the AI domain)
Nace_all_patents_Part1$ctry_code2 <- Nace_all_patents_Part1$ctry_code
Nace_all_patents_Part2$ctry_code2 <- Nace_all_patents_Part2$ctry_code
Nace_all_patents_Part3$ctry_code2 <- Nace_all_patents_Part3$ctry_code

#set the working directory to the folder where we opened this code:
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

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

add_matrices_2 <- function(matrix1, matrix2) {
  a <- list(matrix1, matrix2)
  cols <- sort(unique(unlist(lapply(a, colnames))))
  rows <- sort(unique(unlist(lapply(a, rownames))))
  out <- array(0, dim = c(length(rows), length(cols)), dimnames = list(rows,cols))
  for (m in a) out[rownames(m), colnames(m)] <- out[rownames(m), colnames(m)] + m
  out
}

mat_tech_AI_Final <- add_matrices_2(mat_tech_AI_Final1, mat_tech_AI_Final2)

write.csv2(mat_tech_AI_Final, file = "Data/Matrix_Nace.csv", row.names = TRUE)

#drop again the big files we don't need;
rm(Nace_all_patents_Part1)
rm(Nace_all_patents_Part2)
rm(Nace_all_patents_Part3)

library(janitor)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
matrix2 <- read.csv("Data/Matrix_Nace.csv", sep = ";", header = F)
matrix2 <- matrix2 %>%
  row_to_names(row_number = 1)
matrix <- matrix2[,-1]
rownames(matrix) <- matrix2[,1]
matrix <- as.matrix(matrix)
mat_tech_AI_Final <- matrix

#ALL RIGHT UNTIL HERE
mat_tech_rel_AI <- mat_tech_AI_Final %>% 
  relatedness(method = "cosine")

nace2_names <- read.csv("Data/tls902_ipc_nace2.csv", sep = ";", header = TRUE)%>%
  select(nace2_code, nace2_descr) %>%
  distinct(nace2_code, .keep_all = TRUE) %>%
  mutate(nace2_code = nace2_code) %>%
  arrange(nace2_code)

g_tech_AI <- mat_tech_rel_AI %>% as_tbl_graph(directed = FALSE) %N>%
  left_join(nace2_names %>% mutate(nace2_code = nace2_code %>% as.character()), by = c("name" = "nace2_code")) %>%
  mutate(dgr = centrality_eigen(weights = weight)) %E>%
  filter(weight >= mean(weight))

coords_tech_AI <- g_tech_AI %>% igraph::layout.fruchterman.reingold() %>% as_tibble()
colnames(coords_tech_AI) <- c("x", "y")

#let's take a look at the most and less complex Nace fields:
g_tech_AI %N>% 
  arrange(desc(dgr)) %>%
  as_tibble() %>%
  slice(1:10)

g_tech_AI %N>% 
  arrange(dgr) %>%
  as_tibble() %>%
  slice(1:10)

#1.2.Nace By Country AI----
setwd("C:/Users/Matheus/Desktop") #for loading the big file
c <- 58935336-40000000
Nace_all_patents_Part1 <- fread("All_patents_and_Naces_Part1.csv", header = F, nrow = 20000000)
names(Nace_all_patents_Part1) <- c("appln_id", "ctry_code", "nace2_code", "weight", "priority_year")
Nace_all_patents_Part1 <- Nace_all_patents_Part1[, c((-4), (-5))]

reg_tech1 <- Nace_all_patents_Part1 %>%
  group_by(appln_id) %>%
  mutate(field_weight = 1 / n()) %>%
  ungroup()

rm(Nace_all_patents_Part1)

Nace_all_patents_Part2 <- fread("All_patents_and_Naces_Part1.csv", header = F, nrow = 20000000, skip = 20000000)
names(Nace_all_patents_Part2) <- c("appln_id", "ctry_code", "nace2_code", "weight", "priority_year")
Nace_all_patents_Part2 <- Nace_all_patents_Part2[, c((-4), (-5))]

reg_tech2 <- Nace_all_patents_Part2 %>%
  group_by(appln_id) %>%
  mutate(field_weight = 1 / n()) %>%
  ungroup()

rm(Nace_all_patents_Part2)

Nace_all_patents_Part3 <- fread("All_patents_and_Naces_Part1.csv", header = F, nrow = c, skip = 40000000)
names(Nace_all_patents_Part3) <- c("appln_id", "ctry_code", "nace2_code", "weight", "priority_year")
Nace_all_patents_Part3 <- Nace_all_patents_Part3[, c((-4), (-5))]

reg_tech3 <- Nace_all_patents_Part3 %>%
  group_by(appln_id) %>%
  mutate(field_weight = 1 / n()) %>%
  ungroup()

rm(Nace_all_patents_Part3)
reg_tech_temp <- rbind(c(reg_tech1, reg_tech2, reg_tech3))
rm(reg_tech1)
rm(reg_tech2)
rm(reg_tech3)




#Second Part:
c <- 45233329 -40000000
Nace_all_patents_Part4 <- fread("All_patents_and_Naces_Part2.csv", header = F, nrow = 20000000)
names(Nace_all_patents_Part4) <- c("appln_id", "ctry_code", "nace2_code", "weight", "priority_year")
Nace_all_patents_Part4 <- Nace_all_patents_Part4[, c((-4), (-5))]

reg_tech4 <- Nace_all_patents_Part4 %>%
  group_by(appln_id) %>%
  mutate(field_weight = 1 / n()) %>%
  ungroup()

rm(Nace_all_patents_Part4)


Nace_all_patents_Part5 <- fread("All_patents_and_Naces_Part2.csv", header = F, nrow = 20000000, skip = 20000000)
names(Nace_all_patents_Part5) <- c("appln_id", "ctry_code", "nace2_code", "weight", "priority_year")
Nace_all_patents_Part5 <- Nace_all_patents_Part5[, c((-4), (-5))]

reg_tech5 <- Nace_all_patents_Part5 %>%
  group_by(appln_id) %>%
  mutate(field_weight = 1 / n()) %>%
  ungroup()

rm(Nace_all_patents_Part5)

Nace_all_patents_Part6 <- fread("All_patents_and_Naces_Part2.csv", header = F, nrow = c, skip = 40000000)
names(Nace_all_patents_Part6) <- c("appln_id", "ctry_code", "nace2_code", "weight", "priority_year")
Nace_all_patents_Part6 <- Nace_all_patents_Part6[, c((-4), (-5))]

reg_tech6 <- Nace_all_patents_Part6 %>%
  group_by(appln_id) %>%
  mutate(field_weight = 1 / n()) %>%
  ungroup()

rm(Nace_all_patents_Part6)

#Nace_all_patents_Part2 <- fread("All_patents_and_Naces_Part1.csv", header = F, nrow = 20000000, skip = 20000000)
#Nace_all_patents_Part3 <- fread("All_patents_and_Naces_Part1.csv", header = F, nrow = c, skip = 40000000)

#names(Nace_all_patents_Part1) <- c("appln_id", "ctry_code", "nace2_code", "weight", "priority_year")
#Nace_all_patents_Part1 <- Nace_all_patents_Part1[, c((-4), (-5))]

#Nace_all_patents_Part2 <- fread("All_patents_and_Naces_Part2.csv", header = F)
#names(Nace_all_patents_Part2) <- c("appln_id", "ctry_code", "nace2_code", "weight", "priority_year")
#Nace_all_patents_Part2 <- Nace_all_patents_Part2[, c((-4), (-5))]

#Nace_all_patents <- rbind(Nace_all_patents_Part1, Nace_all_patents_Part2)

#rm(Nace_all_patents_Part1)
#rm(Nace_all_patents_Part2)

#Nace_all_patents_applnId <- Nace_all_patents[, (1)]


#here commes the first big problem: I don't have the Nace_all_patents file anymore, which I need for calculating the weights;
#maybe I can load both files and merge by rows (rbind)

#I'll do the one below at the end:
countries_geo <- Nace_all_patents %>%
  distinct(ctry_code, .keep_all = TRUE) %>%
  select(ctry_code)


reg_tech %<>%
  group_by(ctry_code, nace2_code) %>%
  summarise(n_tech_reg = sum(field_weight)) %>%
  ungroup() %>%
  drop_na() 

mat_reg_tech <- reg_tech %>%
  arrange(nace2_code, ctry_code) %>%
  pivot_wider(names_from = nace2_code, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

rownames(mat_reg_tech) <- mat_reg_tech %>% pull(ctry_code)

mat_reg_tech %<>% select(-ctry_code) %>%
  as.matrix() %>%
  round()
mat_reg_tech[1:10, 1:10]

reg_RCA <- mat_reg_tech %>% location.quotient(binary = TRUE) %>% 
  as.data.frame() %>% 
  rownames_to_column("ctry_code") %>% 
  as_tibble() %>% 
  gather(key = "nace2_code", value = "RCA", -ctry_code) %>%
  arrange(ctry_code, nace2_code)
reg_RCA %>% head()

mat_reg_tech %>% 
  Herfindahl() %>% 
  as.data.frame() %>% 
  rownames_to_column("ctry_code") %>% 
  rename(HH = ".") %>% 
  arrange(desc(HH)) %>% 
  head(10)

mat_reg_tech %>% 
  entropy() %>% 
  as.data.frame() %>% 
  rownames_to_column("ctry_code") %>% 
  rename(SE = ".") %>% 
  arrange(desc(SE)) %>%
  head(10)

#1.4.Nace By Country countries---- Skip this
countries_geo <- Nace_all_patents %>%
  distinct(ctry_code2, .keep_all = TRUE) %>%
  select(ctry_code2)

reg_tech <- Nace_all_patents %>%
  group_by(appln_id) %>%
  mutate(field_weight = 1 / n()) %>%
  ungroup()

reg_tech %<>%
  group_by(ctry_code2, nace2_code) %>%
  summarise(n_tech_reg = sum(field_weight)) %>%
  ungroup() %>%
  drop_na() 

#library(tidyr)
mat_reg_tech <- reg_tech %>%
  arrange(nace2_code, ctry_code2) %>%
  pivot_wider(names_from = nace2_code, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

rownames(mat_reg_tech) <- mat_reg_tech %>% pull(ctry_code2)

mat_reg_tech %<>% select(-ctry_code2) %>%
  as.matrix() %>%
  round()
mat_reg_tech[1:10, 1:10]

reg_RCA <- mat_reg_tech %>% location.quotient(binary = TRUE) %>% 
  as.data.frame() %>% 
  rownames_to_column("ctry_code2") %>% 
  as_tibble() %>% 
  gather(key = "nace2_code", value = "RCA", -ctry_code2) %>%
  arrange(ctry_code2, nace2_code)
reg_RCA %>% head()

mat_reg_tech %>% 
  Herfindahl() %>% 
  as.data.frame() %>% 
  rownames_to_column("ctry_code2") %>% 
  rename(HH = ".") %>% 
  arrange(desc(HH)) %>% 
  head(10)

mat_reg_tech %>% 
  entropy() %>% 
  as.data.frame() %>% 
  rownames_to_column("ctry_code2") %>% 
  rename(SE = ".") %>% 
  arrange(desc(SE)) %>%
  head(10)

#1.5.Nace Visualization -----
library(ggplot2)
country_select <- c("CN", "US", "JP", "KR")
i = 1
Nace1 <- g_tech_AI %N>%
  left_join(reg_RCA %>% filter(ctry_code2 == country_select[i]) %>% select(-ctry_code2), by = c("name" = "nace2_code")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "grey") + 
  geom_node_point(aes(colour = RCA, size = dgr)) + 
  geom_node_text(aes(label = name), size = 5, repel = TRUE) +
  scale_color_gradient(low = "skyblue", high = "red") +
  theme_graph() +
  ggtitle("Technology Space: RCA China (Nace codes)")

i = 2
Nace2 <- g_tech_AI %N>%
  left_join(reg_RCA %>% filter(ctry_code2 == country_select[i]) %>% select(-ctry_code2), by = c("name" = "nace2_code")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "grey") + 
  geom_node_point(aes(colour = RCA, size = dgr)) + 
  geom_node_text(aes(label = name), size = 5, repel = TRUE) +
  scale_color_gradient(low = "skyblue", high = "red") +
  theme_graph() +
  ggtitle("Technology Space: RCA USA (Nace codes)")

i = 3
Nace3 <- g_tech_AI %N>%
  left_join(reg_RCA %>% filter(ctry_code2 == country_select[i]) %>% select(-ctry_code2), by = c("name" = "nace2_code")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "grey") + 
  geom_node_point(aes(colour = RCA, size = dgr)) + 
  geom_node_text(aes(label = name), size = 5, repel = TRUE) +
  scale_color_gradient(low = "skyblue", high = "red") +
  theme_graph() +
  ggtitle("Technology Space: RCA South Korea (Nace codes)")

i = 4
Nace4 <- g_tech_AI %N>%
  left_join(reg_RCA %>% filter(ctry_code2 == country_select[i]) %>% select(-ctry_code2), by = c("name" = "nace2_code")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "grey") + 
  geom_node_point(aes(colour = RCA, size = dgr)) + 
  geom_node_text(aes(label = name), size = 5, repel = TRUE) +
  scale_color_gradient(low = "skyblue", high = "red") +
  theme_graph() +
  ggtitle("Technology Space: RCA Japan (Nace codes)")

setwd("C:/Users/mathe/Google Drive/PhD/1.Paper 1 - Patent Analysis/0.Tudo escrito mais EMAEE e Workshop/GPT Nova vers?o P?s EMAEE/0.Novo draft/New Analysis Inventors Data/nace2 analysis")

jpeg("Nace_all_CN_persp_Period3.jpg", width = 14, height = 10, units = 'in', res = 200)
Nace1
dev.off()

jpeg("Nace_all_US_persp_Period3.jpg", width = 14, height = 10, units = 'in', res = 200)
Nace2
dev.off()

jpeg("Nace_all_JP_persp_Period3.jpg", width = 14, height = 10, units = 'in', res = 200)
Nace3
dev.off()

jpeg("Nace_all_KR_persp_Period3.jpg", width = 14, height = 10, units = 'in', res = 200)
Nace4
dev.off()


#1.3.Nace Visualization -----
#I have to figure it out where to put the 20 lines below:
#Now we load the data containing all nace codes used in our dataset of AI patents:
patents_AI_specific <- read.csv("Data/Nace2_AI csv.csv", sep = ";", header = TRUE, dec=",")

#Now, I'll divide this data in 3 periods, so we can see the evolution of AI over time.
#Starting by the 3rd period, from 2003 to 2018:
patents_AI_specific <- patents_AI_specific[patents_AI_specific$priority_year > 2003,]
patents_AI_specific$ctry_code <- as.vector(patents_AI_specific$ctry_code)
patents_AI_specific$ctry_code <- "AI_pat"
#I want to select some patents on these Nace_all_patents dataset and change the patent_office to, let's say, AI;
setDT(patents_AI_specific)
setDT(Nace_all_patents_Part1)
setDT(Nace_all_patents_Part2)
Nace_all_patents_Part1[patents_AI_specific, on = c("appln_id"), ctry_code := i.ctry_code]
Nace_all_patents_Part2[patents_AI_specific, on = c("appln_id"), ctry_code := i.ctry_code]

library(ggplot2)
country_select <- c("AI_pat")
i = 1
Nace_AI <- g_tech_AI %N>%
  left_join(reg_RCA %>% filter(ctry_code == country_select[i]) %>% select(-ctry_code), by = c("name" = "nace2_code")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "grey") + 
  geom_node_point(aes(colour = RCA, size = dgr)) + 
  geom_node_text(aes(label = name), size = 5, repel = TRUE) +
  scale_color_gradient(low = "skyblue", high = "red") +
  theme_graph() +
  ggtitle("Technology Space: RCA AI patents (Nace codes)")

Nace_AI1 <-g_tech_AI %>%
  ggraph(layout =  coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "grey") + 
  geom_node_point(aes(colour = dgr, size = dgr)) + 
  geom_node_text(aes(label = name), size = 4, repel = TRUE) +
  theme_graph()+
  ggtitle("Technology Space: AI Nace codes")

Nace_AI2 <-g_tech_AI %>%
  ggraph(layout =  coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "grey") + 
  geom_node_point(aes(colour = dgr, size = dgr^10)) + 
  geom_node_text(aes(label = name), size = 4, repel = TRUE) +
  theme_graph()+
  ggtitle("Technology Space: AI Nace codes")

setwd("C:/Users/mathe/Google Drive/PhD/1.Paper 1 - Patent Analysis/0.Tudo escrito mais EMAEE e Workshop/GPT Nova vers?o P?s EMAEE/0.Novo draft/New Analysis Inventors Data/nace2 analysis")

jpeg("Nace_all_AI_persp1_Period3.jpg", width = 14, height = 10, units = 'in', res = 200)
Nace_AI1
dev.off()

jpeg("Nace_all_AI_persp2_Period3.jpg", width = 14, height = 10, units = 'in', res = 200)
Nace_AI2
dev.off()

jpeg("Nace_all_AIpatents_specific_Period3.jpg", width = 14, height = 10, units = 'in', res = 200)
Nace_AI
dev.off()
