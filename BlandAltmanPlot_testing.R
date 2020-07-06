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

library(janitor) #used here for converting the first column of data to row names.

#1.1.g_tech_AI 4-digits ----
#Now we load all the files we already saved. We start by loading the janitor library, which is 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list=ls())
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

reg_RCA1 <- mat_reg_tech1 %>% location.quotient(binary = T) %>% 
  as.data.frame() %>% 
  rownames_to_column("ctry_code") %>% 
  as_tibble() %>% 
  gather(key = "Subclass", value = "RCA", -ctry_code) %>%
  arrange(ctry_code, Subclass)

Labels$Subclass <- as.vector(Labels$Subclass)
reg_RCA1$name <- Labels$Number[match(reg_RCA1$Subclass, Labels$Subclass)]
reg_RCA1$name <- as.character(reg_RCA1$name)

#1.3.Calculate the g_tech_AI IPC fields----
rm(list=ls())
#now we load the similarity matrix:
matrix2 <- read.csv("Data_IPC/Matrix_IPC.csv", sep = ";", header = F)
matrix2 <- matrix2 %>%
  row_to_names(row_number = 1)
matrix <- matrix2[,-1]
rownames(matrix) <- matrix2[,1]
matrix <- as.matrix(matrix)
mat_tech_AI_Final <- matrix

mat_tech_rel_AI <- mat_tech_AI_Final %>% 
  relatedness(method = "cosine")

IPC_names <- read.csv("Data_IPC/ipc_technology.csv", sep = ";", header = TRUE)%>%
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
reg_tech1 <- read.csv("Data_IPC/reg_tech_FirstPeriod.csv", sep = ";", header = TRUE, dec=",")

###First Period:
mat_reg_tech1 <- reg_tech1 %>%
  arrange(techn_field_nr, ctry_code) %>%
  pivot_wider(names_from = techn_field_nr, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

rownames(mat_reg_tech1) <- mat_reg_tech1 %>% pull(ctry_code)

mat_reg_tech1 %<>% select(-ctry_code) %>%
  as.matrix() %>%
  round()

reg_RCA1 <- mat_reg_tech1 %>% location.quotient(binary = F) %>% 
  as.data.frame() %>% 
  rownames_to_column("ctry_code") %>% 
  as_tibble() %>% 
  gather(key = "techn_field_nr", value = "RCA", -ctry_code) %>%
  arrange(ctry_code, techn_field_nr)



#heatmap(mat_tech_rel_AI)
library(BlandAltmanLeh)
bland.altman.plot(mat_tech_rel_AI, (matrix/2), main="This is a Bland Altman Plot", xlab="Means", ylab="Differences")
bland.altman.plot(mat_tech_rel_AI, (matrix), main="This is a Bland Altman Plot2", xlab="Means", ylab="Differences")


testCN <- reg_RCA1[reg_RCA1$ctry_code == "CN",]
testCN<- as.matrix(testCN[,3])

#mat_tech_rel_AI[,3]
#test2[,3]

testUS <- reg_RCA1[reg_RCA1$ctry_code == "US",]
testUS<- as.matrix(testUS[,3])

#testCN <- reg_RCA1[reg_RCA1$ctry_code == "CN",]
#test2<- as.matrix(testCN[,3])
#bland.altman.plot(mat_tech_rel_AI[,3], test2, graph.sys = "ggplot2")

bland.altman.plot(testUS, testCN, graph.sys = "ggplot2")

g_tech_AI[,11]


g_tech_AI2 <- mat_tech_rel_AI %>% as_tbl_graph(directed = FALSE) %N>%
  left_join(IPC_names %>% mutate(field_nr = field_nr %>% as.character()), by = c("name" = "field_nr")) %>%
  mutate(dgr = centrality_eigen(weights = weight)) %E>%
  filter(weight >= mean(weight))

mat_tech_rel_AI %>% as_tbl_graph(directed = FALSE) %N>%
  left_join(IPC_names %>% mutate(field_nr = field_nr %>% as.character()), by = c("name" = "field_nr")) %>%
  mutate(dgr = centrality_eigen(weights = weight)) %E>%
  filter(weight >= mean(weight))

mat_tech_rel_AI2 <- mat_tech_rel_AI
mean(mat_tech_rel_AI2)

median(mat_tech_rel_AI)
test22 <- unlist(g_tech_AI2)
mean(unlist(g_tech_AI2[6]))
sum(unlist(g_tech_AI2[6]))
sum(unlist(g_tech_AI2[1]))


#Now we read the data per period and calculate the RCAs:
reg_tech1_countries <- read.csv("Data_IPC/reg_tech_FirstPeriod.csv", sep = ";", header = TRUE, dec=",")

###First Period Countries-----
mat_reg_tech1_countries <- reg_tech1_countries %>%
  arrange(techn_field_nr, ctry_code) %>%
  pivot_wider(names_from = techn_field_nr, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

rownames(mat_reg_tech1_countries) <- mat_reg_tech1_countries %>% pull(ctry_code)

mat_reg_tech1_countries %<>% select(-ctry_code) %>%
  as.matrix() %>%
  round()

reg_RCA1_countries <- mat_reg_tech1_countries %>% location.quotient(binary = F) %>% 
  as.data.frame() %>% 
  rownames_to_column("ctry_code") %>% 
  as_tibble() %>% 
  gather(key = "techn_field_nr", value = "RCA", -ctry_code) %>%
  arrange(ctry_code, techn_field_nr)

###First Period AI-----
reg_tech1_AI <- read.csv("Data_IPC/reg_techAI_FirstPeriod.csv", sep = ";", header = TRUE, dec=",")

mat_reg_tech1_AI <- reg_tech1_AI %>%
  arrange(techn_field_nr, ctry_code) %>%
  pivot_wider(names_from = techn_field_nr, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

rownames(mat_reg_tech1_AI) <- mat_reg_tech1_AI %>% pull(ctry_code)

mat_reg_tech1_AI %<>% select(-ctry_code) %>%
  as.matrix() %>%
  round()

reg_RCA1_AI <- mat_reg_tech1_AI %>% location.quotient(binary = F) %>% 
  as.data.frame() %>% 
  rownames_to_column("ctry_code") %>% 
  as_tibble() %>% 
  gather(key = "techn_field_nr", value = "RCA", -ctry_code) %>%
  arrange(ctry_code, techn_field_nr)

US_first_period <- reg_RCA1_countries[reg_RCA1_countries$ctry_code == "US",]
US_first_period<- as.matrix(US_first_period[,3])

AI_first_period <- reg_RCA1_AI[reg_RCA1_AI$ctry_code == "AI_pat",]
AI_first_period<- as.matrix(AI_first_period[,3])


bland.altman.plot(AI_first_period, US_first_period, graph.sys = "ggplot2")
library(BlandAltmanLeh)
bland.altman.plot(AI_first_period, US_first_period, main="This is a Bland Altman Plot", xlab="Means", ylab="Differences")

US_first_period <- reg_RCA1_countries[,2:3][reg_RCA1_countries$ctry_code == "US",]
#US_first_period<- as.matrix(US_first_period[,2:3])

AI_first_period <- reg_RCA1_AI[,2:3][reg_RCA1_AI$ctry_code == "AI_pat",]
#AI_first_period<- as.matrix(AI_first_period[,2:3])

First_period <- merge(US_first_period, AI_first_period, by = "techn_field_nr")
names(First_period) <- c("techn_field_nr", "RCA_US", "RCA_AI")
log
ggplot(First_period, aes(x=log10(RCA_US), y=log10(RCA_AI))) + geom_point()

ggplot(First_period, aes(log10(RCA_US), y=log10(RCA_AI))) +
  geom_point() + 
  geom_text(label=First_period$techn_field_nr)
