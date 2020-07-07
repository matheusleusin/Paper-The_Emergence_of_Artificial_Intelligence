library(tidyverse) # Collection of all the good stuff like dplyr, ggplot2 ect.
library(magrittr) # For extra-piping operators (eg. %<>%)

# Network specific
library(tidygraph) # For tidy-style graph manipulation
library(ggraph) # For ggplot2 style graph plotting

library(EconGeo) # Economic Geography functions

library(data.table) #for reading the big files using fread and for replacing countries names (by AI_pat for example)

library(stringr) #for separating the IPC codes in subclasses

library(janitor) #used here for converting the first column of data to row names.

#1.Discussions presented in: Balland, P. A. (2017). Economic Geography in R: Introduction to the EconGeo package.----

#1.1. Number patents in each techn_field per country and RCA:----
mat = matrix (
  c (100, 0, 0, 0, 0,
     0, 15, 5, 70, 10,
     0, 20, 10, 20, 50,
     0, 25, 30, 5, 40,
     0, 40, 55, 5, 0), ncol = 5, byrow = T)
rownames(mat) <- c ("R1", "R2", "R3", "R4", "R5")
colnames(mat) <- c ("I1", "I2", "I3", "I4", "I5")

#i can get easily the number of patents in each techn_field per country (and thus, also total) by doing:
setwd("C:/Users/Matheus/Desktop")
IPC_all_patents_Part1 <- fread("All_patents_and_IPCs_Part2.csv", header = F, nrow = 200000)
names(IPC_all_patents_Part1) <- c("appln_id", "ctry_code", "techn_field_nr", "weight", "priority_year")
IPC_all_patents_example <- IPC_all_patents_Part1[,c((-1), (-4), (-5))]
IPC_all_patents_example$count <- 1
mat2<- get.matrix(IPC_all_patents_example)

#calculate RCAs:
location.quotient(mat2, binary = FALSE)

#1.2. Possible indicators:-----
#1.2.1.Herfindahl-Hirschman index (Herfindahl, 1959; Hirschman, 1945)
#[1] Herfindahl, O.C. (1959) Copper Costs and Prices: 1870-1957. Baltimore: The Johns Hopkins Press.
#[2] Hirschman, A.O. (1945) National Power and the Structure of Foreign Trade, Berkeley and Los Angeles:
#University of California Press.
Herfindahl(mat2)

#1.2.2. Shannon entropy index (Shannon and Weaver, 1949; Frenken et al., 2007)
#[3] Shannon, C.E., Weaver, W. (1949) The Mathematical Theory of Communication. Univ of Illinois Press.
#[4] Frenken, K., Van Oort, F. and Verburg, T. (2007) Related variety, unrelated variety and regional economic
#growth, Regional studies 41 (5): 685-697.
entropy(mat2)

#1.3.Hoover(1936) curve ----
#This function plots a Hoover curve from regions - industries matrices
ind <- c(0, 10, 10, 30, 50)
pop <- c(10, 15, 20, 25, 30)
Hoover.curve(ind, pop)
#this means that 30% of the population produces 50% of the industrial output (we see
#this by looking at the right side of the black curve)

#compute the corresponding Hoover Gini
Hoover.Gini(ind, pop)

#maybe I can use the Hoover to show how concentrated AI is in a few codes, and how few countries also 
#concentrate most of AI patents or most of AI related codes;

#1.4.Relatedness (Hidalgo et al., 2007; Boschma et al., 2015; Balland, 2016) between entities ----
#(industries, technologies, . . . ) from their co-occurrence (adjacency) matrix.
#Different normalization procedures are proposed following van Eck and Waltman (2009): association 
#strength, cosine, Jaccard, and an adapted version of the association strength that we refer to as 
#probability index.
#[5] Hidalgo, C.A., Klinger, B., Barabasi, A. and Hausmann, R. (2007) The product space conditions the
#development of nations, Science 317: 482-487.
#[6] Boschma, R., Balland, P.A. and Kogler, D. (2015) Relatedness and Technological Change in Cities: The rise
#and fall of technological knowledge in U.S. metropolitan areas from 1981 to 2010, Industrial and Corporate
#Change 24 (1): 223-250.
#[7] Balland, P.A. (2016) Relatedness and the Geography of Innovation, in: R. Shearmur, C. Carrincazeaux and
#D. Doloreux (eds) Handbook on the Geographies of Innovation. Northampton, MA: Edward Elgar

#for this, we need a symmetric matrix (that we calculate as mat_tech_AI1 previously)
set.seed(31)
mat <- matrix(sample(0:10,36,replace=T), ncol = 6)
mat[lower.tri(mat, diag = TRUE)] <- t(mat)[lower.tri(t(mat), diag = TRUE)]
rownames(mat) <- c ("I1", "I2", "I3", "I4", "I5", "I6")
colnames(mat) <- c ("I1", "I2", "I3", "I4", "I5", "I6")
mat
relatedness(mat)
#thus, it would be easy to calculate the relatedness of AI patent codes, for example;
#using other methods:
relatedness(mat, method = "association")
relatedness(mat, method = "cosine") #the one we are currently using
relatedness(mat, method = "Jaccard")

#Generate a matrix of entry events (Hidalgo et al., 2007; Neffke et al., 2011; Boschma et al., 2014) from two
#regions - industries matrices (same matrix composition from two different periods).
#not so important now, I'll jump it (page 10)

#1.5.Compute an index of knowledge complexity of industries using the method of reflection from ------
#regions - industries (incidence) matrices. The index has been developed by Hidalgo and Hausmann (2009) for
#country - product matrices and adapted by Balland and Rigby (2017) for city - technology matrices.
#[8] Hidalgo, C. and Hausmann, R. (2009) The building blocks of economic complexity, Proceedings of the
#National Academy of Sciences 106: 10570 - 10575.
#[9] Balland, P.A. and Rigby, D. (2017) The Geography of Complex Knowledge, Economic Geography, 93 (1): 1-23.

#from what I've understood, this is mainly to look at each industry, and need some interpretation to get to
#something about the country; maybe it would be a good choice for the scatter plot;
#1.5.1. Calculate it from a region - industry matrix with full count
set.seed(31)
mat <- matrix(sample(0:10,20,replace=T), ncol = 4)
rownames(mat) <- c ("R1", "R2", "R3", "R4", "R5")
colnames(mat) <- c ("I1", "I2", "I3", "I4")
mat
MORt(mat, RCA = TRUE)
MORt(mat, RCA = TRUE, steps = 1)
MORt(mat, RCA = TRUE, steps = 2)

##1.5.2. Calculate it from a region - industry matrix in which cells represent the presence/absence of a RCA (I have these
#binary matrices ready, but I can also make them continuous)
#this one may be better for the scatter: multiply countries binary RCA by the complexity of the technology
set.seed(32)
mat <- matrix(sample(0:1,20,replace=T), ncol = 4)
rownames(mat) <- c ("R1", "R2", "R3", "R4", "R5")
colnames(mat) <- c ("I1", "I2", "I3", "I4")
mat
MORt(mat)
MORt(mat, steps = 0)
MORt(mat, steps = 1)
MORt(mat, steps = 2)

## 1.5.3. Calculate it from the exact simple network of Hidalgo and Hausmann (2009) presented p.11 (Fig. S4)
countries <- c("C1", "C1", "C1", "C1", "C2", "C3", "C3", "C4")
products <- c("P1","P2", "P3", "P4", "P2", "P3", "P4", "P4")
data <- data.frame(countries, products)
data$freq <- 1
mat <- get.matrix (data)
mat
MORt(mat)
MORt(mat, steps = 0)
MORt(mat, steps = 1)
MORt(mat, steps = 2)

#my calculations:
mat*MORt(mat)
mat*MORt(mat, steps = 0) #this one makes no sense: it places c4 in front of c2
mat*MORt(mat, steps = 1) #this already puts countries in the right order
mat*MORt(mat, steps = 2) #and this messes up the order again
#thus, I don't get it clearly why step 2 is better than 1;

#interpretation given by Hidalgo for this data:
#As we iterate the method we find that there is important information encoded in the
#relative position of countries and products relative to one another. For example, when we look
#at the values characterizing countries after the second reflection (kc,2) we can see that country
#c1 comes up ahead, followed by country c3, c2 and c4. The method places country c2 ahead of
#c4 because by the second reflection it is already considering that country c2 produces a non
#ubiquitous product that is found only in diversified countries, probably signaling that country c2
#has a relatively good endowment of capabilities and produces a small number of products
#because of other reason, such as being of relatively small size. On the contrary, c4 produces a
#product that is ubiquitous and it is found in diversified and non diversified countries, probably
#indicating that is a simple product which is accessible to countries with relatively simple
#productive structures. Hence while both, c2 and c4 produce the same number of products, the
#method can differentiate between them and considers c2 to have a more complex productive
#structure than c4.
#While small in size this example illustrates how the method of reflections can be used to
#characterize the structure of a bipartite network and how this can be applied to help the
#understanding of the productive structure of countries and the sophistication of products.

#2. My calculations
#2.1.Calculate the g_tech_AI IPC fields----
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#Load the similarity matrix:
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
bland.altman.plot(mat_tech_rel_AI, (matrix), main="Bland Altman Plot", xlab="Means", ylab="Differences")
bland.altman.plot(mat_tech_rel_AI, (matrix), main="Bland Altman Plot2", xlab="mat_tech_rel_AI", ylab="matrix")

testCN <- reg_RCA1[reg_RCA1$ctry_code == "CN",]
testCN<- as.matrix(testCN[,3])

#mat_tech_rel_AI[,3]
#test2[,3]

testUS <- reg_RCA1[reg_RCA1$ctry_code == "US",]
testUS<- as.matrix(testUS[,3])

#testCN <- reg_RCA1[reg_RCA1$ctry_code == "CN",]
#test2<- as.matrix(testCN[,3])
#bland.altman.plot(mat_tech_rel_AI[,3], test2, graph.sys = "ggplot2")

#Compare two countries:
bland.altman.plot(testUS, testCN, graph.sys = "ggplot2")

g_tech_AI2 <- mat_tech_rel_AI %>% as_tbl_graph(directed = FALSE) %N>%
  left_join(IPC_names %>% mutate(field_nr = field_nr %>% as.character()), by = c("name" = "field_nr")) %>%
  mutate(dgr = centrality_eigen(weights = weight)) %E>%
  filter(weight >= mean(weight))

mat_tech_rel_AI %>% as_tbl_graph(directed = FALSE) %N>%
  left_join(IPC_names %>% mutate(field_nr = field_nr %>% as.character()), by = c("name" = "field_nr")) %>%
  mutate(dgr = centrality_eigen(weights = weight)) %E>%
  filter(weight >= mean(weight))

mat_tech_rel_AI2 <- mat_tech_rel_AI

median(mat_tech_rel_AI)
#test <- unlist(g_tech_AI2)
mean(unlist(g_tech_AI2[6]))
sum(unlist(g_tech_AI2[6]))


#Now we read the data per period and calculate the RCAs:
reg_tech1_countries <- read.csv("Data_IPC/reg_tech_FirstPeriod.csv", sep = ";", header = TRUE, dec=",")

###2.2. Test -----
#2.2.1. First Period Countries-----
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

###2.2.2. First Period AI-----
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
#the visualization is too bad (due to a bery high specialization of some fields, like the field in 33 with 50+ spec.);

US_first_period <- reg_RCA1_countries[,2:3][reg_RCA1_countries$ctry_code == "US",]
#US_first_period<- as.matrix(US_first_period[,2:3])

AI_first_period <- reg_RCA1_AI[,2:3][reg_RCA1_AI$ctry_code == "AI_pat",]
#AI_first_period<- as.matrix(AI_first_period[,2:3])

First_period <- merge(US_first_period, AI_first_period, by = "techn_field_nr")
names(First_period) <- c("techn_field_nr", "RCA_US", "RCA_AI")
ggplot(First_period, aes(x=log10(RCA_US), y=log10(RCA_AI))) + geom_point()

#already better; now I need to:
#1. create two lines on each log10 0.0. Then, for areas above the horizontal line we have a positive AI specialization;
  #for lines above it on the right side, we have a positive specialization of the country;
#2. make the right top quadrant (that is, AI specialization and country specialization) detailed, with a different
  #per field, and a different size according to some other variable (which should translate the importance of
  #that field for AI, or for the country). The index of knowledge complexity seems interesting for showing which
  #areas are more relevant for AI

#Besides, I also need a quantitative indicador (from section 1.2.) that allows identifying which countries
#perform better in AI (If I can show that visually it would be event better, e.g. puting together the indicators
#for each country for the 3 periods in one scatter plot). I think to use both Herfindahl-Hirschman index and 
#Shannon entropy index for the 3 periods for each country in a line graph (thus, 2 lines for each country (1 
#per index, with 3 points of data (1 per period) might be a good alternative))

#3.Indicators
rm(list=ls())
setwd("C:/Users/Matheus/Desktop") #for loading the big file

#Now we will load a first big file containing all priorities and their related IPC codes published in or after 2004. This file has
#58,841,893 lines. I will read it in 3 parts:
c <- 58841893-40000000
IPC_all_patents_Part1 <- fread("All_patents_and_IPCs_Part1.csv", header = F, nrow = 20000000)[ ,c((2),(3))]
IPC_all_patents_Part2 <- fread("All_patents_and_IPCs_Part1.csv", header = F, nrow = 20000000, skip = 20000000)[ ,c((2),(3))]
IPC_all_patents_Part3 <- fread("All_patents_and_IPCs_Part1.csv", header = F, nrow = c, skip = 40000000)[ ,c((2),(3))]

IPC_all_patents_3rd <- rbind(IPC_all_patents_Part1, IPC_all_patents_Part2, IPC_all_patents_Part3)
rm(IPC_all_patents_Part1, IPC_all_patents_Part2, IPC_all_patents_Part3)
names(IPC_all_patents_3rd) <- c("ctry_code", "techn_field_nr")
IPC_all_patents_3rd$count <- 1
mat_3rd <- get.matrix(IPC_all_patents_3rd)
Indicators <- Herfindahl(mat_3rd)
names(Indicators) <- c("ctry_code", "techn_field_nr", "Herfindahl")
entropy(mat2)
