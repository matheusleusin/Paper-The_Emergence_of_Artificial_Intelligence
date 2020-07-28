library(tidyverse) # Collection of all the good stuff like dplyr, ggplot2 ect.
library(magrittr) # For extra-piping operators (eg. %<>%)

# Network specific
#library(tidygraph) # For tidy-style graph manipulation
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

#2. Testing some calculations ----
#Calculate the g_tech_AI IPC fields (the useful calculations start on session 3 though)
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

###2.2. Test 
#2.2.1. First Period Countries
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

###2.2.2. First Period AI
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

library(BlandAltmanLeh)
bland.altman.plot(AI_first_period, US_first_period, graph.sys = "ggplot2")
bland.altman.plot(AI_first_period, US_first_period, main="This is a Bland Altman Plot", xlab="Means", ylab="Differences")
#the visualization is too bad (due to a bery high specialization of some fields, like the field in 33 with 50+ spec.);

US_first_period <- reg_RCA1_countries[,2:3][reg_RCA1_countries$ctry_code == "US",]
#US_first_period<- as.matrix(US_first_period[,2:3])

AI_first_period <- reg_RCA1_AI[,2:3][reg_RCA1_AI$ctry_code == "AI_pat",]
#AI_first_period<- as.matrix(AI_first_period[,2:3])

First_period <- merge(US_first_period, AI_first_period, by = "techn_field_nr")
names(First_period) <- c("techn_field_nr", "RCA_US", "RCA_AI")
ggplot(First_period, aes(x=log10(RCA_US), y=log10(RCA_AI), label = techn_field_nr)) + geom_point() + geom_text()

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

#3.Indicators ----
rm(list=ls())
setwd("C:/Users/Matheus/Desktop") #for loading the big file

#3.1. First period ----
#3.1.1.Load the data we need and filter it -----
#The file for the first period is composed of 45,182,803 lines which we will read in 3 parts:
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
patents_AI_specific <- read.csv("Data_IPC/IPCs_AI.csv", sep = ";", header = TRUE, dec=",")
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

#3.1.2. Calculate the Indicators -----
IPC_all_patents_1st_In <- IPC_all_patents_1st[,c((-1), (-4), (-5))]
mat_1st <- as.data.frame(table(IPC_all_patents_1st_In$ctry_code, IPC_all_patents_1st_In$techn_field_nr))
mat_1st <- get.matrix(mat_1st)

Indicators <- as.data.frame(Herfindahl(mat_1st))
names(Indicators) <- "Herfindahl"
mat_1st_RCAs <- location.quotient(mat_1st, binary = T)
Indicators$Herfindahl_RCA <- Herfindahl(mat_1st_RCAs)
Indicators$Entropy <- entropy(mat_1st)
Indicators$Entropy_RCA <- entropy(mat_1st_RCAs)
Indicators$Period <- "1st"

write.csv2(Indicators, file = "Data_calculations_IPC/Indicators_1st_period_IPC.csv", row.names = TRUE)
#To do: index of knowledge complexity per area
#here it would be possible to calculate the knowledge complexity of AI per technological field (using the last
#line of MergeAlldata_1st[5,] which is related to AI;) in relation to the 4 leaders or to the world; As I know the 4 
#leaders represent most of the patents, it might be interesting trying to look just at them. This will show how 
#they differ. If I'd go with all countries, this would explain which are the most developed regardless of AI, which
#is interesting too; I could also not consider AI at all;

KnowledgeComp_1st <- as.data.frame(MORt(mat_1st))
KnowledgeComp_1st$Step0 <- MORt(mat_1st, steps = 0)
KnowledgeComp_1st$Step1 <- MORt(mat_1st, steps = 1)
KnowledgeComp_1st$Step2 <- MORt(mat_1st, steps = 2)

KnowledgeComp_1st$RCA <- MORt(mat_1st_RCAs)
KnowledgeComp_1st$RCA_Step0 <- MORt(mat_1st_RCAs, steps = 0)
KnowledgeComp_1st$RCA_Step1 <- MORt(mat_1st_RCAs, steps = 1)
KnowledgeComp_1st$RCA_Step2 <- MORt(mat_1st_RCAs, steps = 2)

#my calculations:
KnowledgeComp_PerCountry_1st <- as.data.frame(mat_1st*MORt(mat_1st))
KnowledgeComp_PerCountry_1st$Step <- "NoStep"

KnowledgeComp_PerCountry_1st_Step0 <- as.data.frame(mat_1st*MORt(mat_1st, steps = 0))
KnowledgeComp_PerCountry_1st_Step0$Step <- "Step0"

KnowledgeComp_PerCountry_1st_Step1 <- as.data.frame(mat_1st*MORt(mat_1st, steps = 1))
KnowledgeComp_PerCountry_1st_Step1$Step <- "Step1"

KnowledgeComp_PerCountry_1st_Step2 <- as.data.frame(mat_1st*MORt(mat_1st, steps = 2))
KnowledgeComp_PerCountry_1st_Step2$Step <- "Step2"

#Considering RCAs:
KnowledgeComp_PerCountry_1st_RCA <- as.data.frame(mat_1st_RCAs*MORt(mat_1st_RCAs))
KnowledgeComp_PerCountry_1st_RCA$Step <- "NoStep"

KnowledgeComp_PerCountry_1st_Step0_RCA <- as.data.frame(mat_1st_RCAs*MORt(mat_1st_RCAs, steps = 0))
KnowledgeComp_PerCountry_1st_Step0_RCA$Step <- "Step0"

KnowledgeComp_PerCountry_1st_Step1_RCA <- as.data.frame(mat_1st_RCAs*MORt(mat_1st_RCAs, steps = 1))
KnowledgeComp_PerCountry_1st_Step1_RCA$Step <- "Step1"

KnowledgeComp_PerCountry_1st_Step2_RCA <- as.data.frame(mat_1st_RCAs*MORt(mat_1st_RCAs, steps = 2))
KnowledgeComp_PerCountry_1st_Step2_RCA$Step <- "Step2"

KnowledgeComp_PerCountry_1st_All <- rbind(KnowledgeComp_PerCountry_1st, KnowledgeComp_PerCountry_1st_Step0,
                                          KnowledgeComp_PerCountry_1st_Step1, KnowledgeComp_PerCountry_1st_Step2)

KnowledgeComp_PerCountry_1st_All_RCAs <- rbind(KnowledgeComp_PerCountry_1st_RCA, KnowledgeComp_PerCountry_1st_Step0_RCA,
                                                KnowledgeComp_PerCountry_1st_Step1_RCA, KnowledgeComp_PerCountry_1st_Step2_RCA)

write.csv2(KnowledgeComp_1st, file = "Data_calculations_IPC/KnowledgeComp_1st.csv", row.names = TRUE)
write.csv2(KnowledgeComp_PerCountry_1st_All, file = "Data_calculations_IPC/KnowledgeComp_PerCountry_1st_All.csv", row.names = TRUE)
write.csv2(KnowledgeComp_PerCountry_1st_All_RCAs, file = "Data_calculations_IPC/KnowledgeComp_PerCountry_1st_All_RCAs.csv", row.names = TRUE)

#For AI complexity and Indicators:
IPC_all_patents_1st_In <- IPC_all_patents_1st[,c((-1), (-4), (-2))]
mat_1st <- as.data.frame(table(IPC_all_patents_1st_In$ctry_code2, IPC_all_patents_1st_In$techn_field_nr))
mat_1st <- get.matrix(mat_1st)

Indicators <- as.data.frame(Herfindahl(mat_1st))
names(Indicators) <- "Herfindahl"
mat_1st_RCAs <- location.quotient(mat_1st, binary = T)
Indicators$Herfindahl_RCA <- Herfindahl(mat_1st_RCAs)
Indicators$Entropy <- entropy(mat_1st)
Indicators$Entropy_RCA <- entropy(mat_1st_RCAs)
Indicators$Period <- "1st"

write.csv2(Indicators, file = "Data_calculations_IPC/Indicators_1st_period_IPC_AI.csv", row.names = TRUE)
KnowledgeComp_1st <- as.data.frame(MORt(mat_1st))
KnowledgeComp_1st$Step0 <- MORt(mat_1st, steps = 0)
KnowledgeComp_1st$Step1 <- MORt(mat_1st, steps = 1)
KnowledgeComp_1st$Step2 <- MORt(mat_1st, steps = 2)

KnowledgeComp_1st$RCA <- MORt(mat_1st_RCAs)
KnowledgeComp_1st$RCA_Step0 <- MORt(mat_1st_RCAs, steps = 0)
KnowledgeComp_1st$RCA_Step1 <- MORt(mat_1st_RCAs, steps = 1)
KnowledgeComp_1st$RCA_Step2 <- MORt(mat_1st_RCAs, steps = 2)

KnowledgeComp_PerCountry_1st <- as.data.frame(mat_1st*MORt(mat_1st))
KnowledgeComp_PerCountry_1st$Step <- "NoStep"

KnowledgeComp_PerCountry_1st_Step0 <- as.data.frame(mat_1st*MORt(mat_1st, steps = 0))
KnowledgeComp_PerCountry_1st_Step0$Step <- "Step0"

KnowledgeComp_PerCountry_1st_Step1 <- as.data.frame(mat_1st*MORt(mat_1st, steps = 1))
KnowledgeComp_PerCountry_1st_Step1$Step <- "Step1"

KnowledgeComp_PerCountry_1st_Step2 <- as.data.frame(mat_1st*MORt(mat_1st, steps = 2))
KnowledgeComp_PerCountry_1st_Step2$Step <- "Step2"

#Considering RCAs:
KnowledgeComp_PerCountry_1st_RCA <- as.data.frame(mat_1st_RCAs*MORt(mat_1st_RCAs))
KnowledgeComp_PerCountry_1st_RCA$Step <- "NoStep"

KnowledgeComp_PerCountry_1st_Step0_RCA <- as.data.frame(mat_1st_RCAs*MORt(mat_1st_RCAs, steps = 0))
KnowledgeComp_PerCountry_1st_Step0_RCA$Step <- "Step0"

KnowledgeComp_PerCountry_1st_Step1_RCA <- as.data.frame(mat_1st_RCAs*MORt(mat_1st_RCAs, steps = 1))
KnowledgeComp_PerCountry_1st_Step1_RCA$Step <- "Step1"

KnowledgeComp_PerCountry_1st_Step2_RCA <- as.data.frame(mat_1st_RCAs*MORt(mat_1st_RCAs, steps = 2))
KnowledgeComp_PerCountry_1st_Step2_RCA$Step <- "Step2"

KnowledgeComp_PerCountry_1st_All <- rbind(KnowledgeComp_PerCountry_1st, KnowledgeComp_PerCountry_1st_Step0,
                                          KnowledgeComp_PerCountry_1st_Step1, KnowledgeComp_PerCountry_1st_Step2)

KnowledgeComp_PerCountry_1st_All_RCAs <- rbind(KnowledgeComp_PerCountry_1st_RCA, KnowledgeComp_PerCountry_1st_Step0_RCA,
                                               KnowledgeComp_PerCountry_1st_Step1_RCA, KnowledgeComp_PerCountry_1st_Step2_RCA)

write.csv2(KnowledgeComp_PerCountry_1st_All, file = "Data_calculations_IPC/KnowledgeComp_PerCountry_1st_All_AI.csv", row.names = TRUE)
write.csv2(KnowledgeComp_PerCountry_1st_All_RCAs, file = "Data_calculations_IPC/KnowledgeComp_PerCountry_1st_All_RCAs_AI.csv", row.names = TRUE)

#3.1.3. Calculate the relatedness -----
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

mat_tech_1st_US_rel_jacc <- relatedness(mat_tech_1st_US, method = "Jaccard")
mat_tech_1st_US_rel_asso <- relatedness(mat_tech_1st_US, method = "association")
mat_tech_1st_US_rel_cosi <- relatedness(mat_tech_1st_US, method = "cosine")

Relatedness <- as.data.frame(mean(mat_tech_1st_US_rel_jacc))
Relatedness$mat_tech_1st_US_rel_asso <- mean(mat_tech_1st_US_rel_asso)
Relatedness$mat_tech_1st_US_rel_cosi<- mean(mat_tech_1st_US_rel_cosi)
rownames(Relatedness) <- c("US")
names(Relatedness) <- c("Jaccard", "Association", "Cosine")
Relatedness$Period <- "1st"

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

mat_tech_1st_US_Top4_rel_jacc <- relatedness(mat_tech_1st_US_Top4, method = "Jaccard")
mat_tech_1st_US_Top4_rel_asso <- relatedness(mat_tech_1st_US_Top4, method = "association")
mat_tech_1st_US_Top4_rel_cosi <- relatedness(mat_tech_1st_US_Top4, method = "cosine")

Relatedness$Jaccard_top4 <- mean(mat_tech_1st_US_Top4_rel_jacc)
Relatedness$Association_top4 <- mean(mat_tech_1st_US_Top4_rel_asso)
Relatedness$Cosine_top4<- mean(mat_tech_1st_US_Top4_rel_cosi)

#then select only the top 3 areas:
IPC_all_patents_1st_US_Top3 <- IPC_all_patents_1st_US[IPC_all_patents_1st_US$techn_field_nr == "4" | 
                                                         IPC_all_patents_1st_US$techn_field_nr == "5"| 
                                                         IPC_all_patents_1st_US$techn_field_nr == "11", ]

mat_tech_1st_US_Top3 <- create_sparse_matrix(i = IPC_all_patents_1st_US_Top3 %>% pull(appln_id),
                                              j = IPC_all_patents_1st_US_Top3 %>% pull(techn_field_nr))

mat_tech_1st_US_Top3 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_1st_US_Top3_rel_jacc <- relatedness(mat_tech_1st_US_Top3, method = "Jaccard")
mat_tech_1st_US_Top3_rel_asso <- relatedness(mat_tech_1st_US_Top3, method = "association")
mat_tech_1st_US_Top3_rel_cosi <- relatedness(mat_tech_1st_US_Top3, method = "cosine")

Relatedness$Jaccard_Top3 <- mean(mat_tech_1st_US_Top3_rel_jacc)
Relatedness$Association_Top3 <- mean(mat_tech_1st_US_Top3_rel_asso)
Relatedness$Cosine_Top3<- mean(mat_tech_1st_US_Top3_rel_cosi)

#and finally the top 7 non-AI areas:
IPC_all_patents_1st_US_Top7 <- IPC_all_patents_1st_US[IPC_all_patents_1st_US$techn_field_nr == "1"| 
                                                        IPC_all_patents_1st_US$techn_field_nr == "2"| 
                                                        IPC_all_patents_1st_US$techn_field_nr == "3"|
                                                        #IPC_all_patents_1st_US$techn_field_nr == "4"| 
                                                        IPC_all_patents_1st_US$techn_field_nr == "13"| 
                                                        IPC_all_patents_1st_US$techn_field_nr == "25"| 
                                                        IPC_all_patents_1st_US$techn_field_nr == "34", ]

mat_tech_1st_US_Top7 <- create_sparse_matrix(i = IPC_all_patents_1st_US_Top7 %>% pull(appln_id),
                                             j = IPC_all_patents_1st_US_Top7 %>% pull(techn_field_nr))

mat_tech_1st_US_Top7 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_1st_US_Top7_rel_jacc <- relatedness(mat_tech_1st_US_Top7, method = "Jaccard")
mat_tech_1st_US_Top7_rel_asso <- relatedness(mat_tech_1st_US_Top7, method = "association")
mat_tech_1st_US_Top7_rel_cosi <- relatedness(mat_tech_1st_US_Top7, method = "cosine")

Relatedness$Jaccard_Top7 <- mean(mat_tech_1st_US_Top7_rel_jacc)
Relatedness$Association_Top7 <- mean(mat_tech_1st_US_Top7_rel_asso)
Relatedness$Cosine_Top7<- mean(mat_tech_1st_US_Top7_rel_cosi)

#China:
mat_tech_1st_CN <- create_sparse_matrix(i = IPC_all_patents_1st_CN %>% pull(appln_id),
                                          j = IPC_all_patents_1st_CN %>% pull(techn_field_nr))

mat_tech_1st_CN %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_1st_CN_rel_jacc <- relatedness(mat_tech_1st_CN, method = "Jaccard")
mat_tech_1st_CN_rel_asso <- relatedness(mat_tech_1st_CN, method = "association")
mat_tech_1st_CN_rel_cosi <- relatedness(mat_tech_1st_CN, method = "cosine")

Relatedness_CN <- as.data.frame(mean(mat_tech_1st_CN_rel_jacc))
Relatedness_CN$mat_tech_1st_CN_rel_asso <- mean(mat_tech_1st_CN_rel_asso)
Relatedness_CN$mat_tech_1st_CN_rel_cosi<- mean(mat_tech_1st_CN_rel_cosi)
rownames(Relatedness_CN) <- c("CN")
names(Relatedness_CN) <- c("Jaccard", "Association", "Cosine")
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

mat_tech_1st_CN_Top4_rel_jacc <- relatedness(mat_tech_1st_CN_Top4, method = "Jaccard")
mat_tech_1st_CN_Top4_rel_asso <- relatedness(mat_tech_1st_CN_Top4, method = "association")
mat_tech_1st_CN_Top4_rel_cosi <- relatedness(mat_tech_1st_CN_Top4, method = "cosine")

Relatedness_CN$Jaccard_top4 <- mean(mat_tech_1st_CN_Top4_rel_jacc)
Relatedness_CN$Association_top4 <- mean(mat_tech_1st_CN_Top4_rel_asso)
Relatedness_CN$Cosine_top4<- mean(mat_tech_1st_CN_Top4_rel_cosi)

#then select only the top 3 areas:
IPC_all_patents_1st_CN_Top3 <- IPC_all_patents_1st_CN[IPC_all_patents_1st_CN$techn_field_nr == "4" | 
                                                         IPC_all_patents_1st_CN$techn_field_nr == "5"| 
                                                         IPC_all_patents_1st_CN$techn_field_nr == "11", ]


mat_tech_1st_CN_Top3 <- create_sparse_matrix(i = IPC_all_patents_1st_CN_Top3 %>% pull(appln_id),
                                              j = IPC_all_patents_1st_CN_Top3 %>% pull(techn_field_nr))

mat_tech_1st_CN_Top3 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_1st_CN_Top3_rel_jacc <- relatedness(mat_tech_1st_CN_Top3, method = "Jaccard")
mat_tech_1st_CN_Top3_rel_asso <- relatedness(mat_tech_1st_CN_Top3, method = "association")
mat_tech_1st_CN_Top3_rel_cosi <- relatedness(mat_tech_1st_CN_Top3, method = "cosine")

Relatedness_CN$Jaccard_Top3 <- mean(mat_tech_1st_CN_Top3_rel_jacc)
Relatedness_CN$Association_Top3 <- mean(mat_tech_1st_CN_Top3_rel_asso)
Relatedness_CN$Cosine_Top3<- mean(mat_tech_1st_CN_Top3_rel_cosi)

#and finally the top 7 non-AI areas:
IPC_all_patents_1st_CN_Top7 <- IPC_all_patents_1st_CN[IPC_all_patents_1st_CN$techn_field_nr == "1"| 
                                                        IPC_all_patents_1st_CN$techn_field_nr == "2"| 
                                                        IPC_all_patents_1st_CN$techn_field_nr == "3"|
                                                        #IPC_all_patents_1st_CN$techn_field_nr == "4"| 
                                                        IPC_all_patents_1st_CN$techn_field_nr == "13"| 
                                                        IPC_all_patents_1st_CN$techn_field_nr == "25"| 
                                                        IPC_all_patents_1st_CN$techn_field_nr == "34", ]

mat_tech_1st_CN_Top7 <- create_sparse_matrix(i = IPC_all_patents_1st_CN_Top7 %>% pull(appln_id),
                                             j = IPC_all_patents_1st_CN_Top7 %>% pull(techn_field_nr))

mat_tech_1st_CN_Top7 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_1st_CN_Top7_rel_jacc <- relatedness(mat_tech_1st_CN_Top7, method = "Jaccard")
mat_tech_1st_CN_Top7_rel_asso <- relatedness(mat_tech_1st_CN_Top7, method = "association")
mat_tech_1st_CN_Top7_rel_cosi <- relatedness(mat_tech_1st_CN_Top7, method = "cosine")

Relatedness_CN$Jaccard_Top7 <- mean(mat_tech_1st_CN_Top7_rel_jacc)
Relatedness_CN$Association_Top7 <- mean(mat_tech_1st_CN_Top7_rel_asso)
Relatedness_CN$Cosine_Top7<- mean(mat_tech_1st_CN_Top7_rel_cosi)


#KR
mat_tech_1st_KR <- create_sparse_matrix(i = IPC_all_patents_1st_KR %>% pull(appln_id),
                                        j = IPC_all_patents_1st_KR %>% pull(techn_field_nr))

mat_tech_1st_KR %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_1st_KR_rel_jacc <- relatedness(mat_tech_1st_KR, method = "Jaccard")
mat_tech_1st_KR_rel_asso <- relatedness(mat_tech_1st_KR, method = "association")
mat_tech_1st_KR_rel_cosi <- relatedness(mat_tech_1st_KR, method = "cosine")

Relatedness_KR <- as.data.frame(mean(mat_tech_1st_KR_rel_jacc))
Relatedness_KR$mat_tech_1st_KR_rel_asso <- mean(mat_tech_1st_KR_rel_asso)
Relatedness_KR$mat_tech_1st_KR_rel_cosi<- mean(mat_tech_1st_KR_rel_cosi)
rownames(Relatedness_KR) <- c("KR")
names(Relatedness_KR) <- c("Jaccard", "Association", "Cosine")
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

mat_tech_1st_KR_Top4_rel_jacc <- relatedness(mat_tech_1st_KR_Top4, method = "Jaccard")
mat_tech_1st_KR_Top4_rel_asso <- relatedness(mat_tech_1st_KR_Top4, method = "association")
mat_tech_1st_KR_Top4_rel_cosi <- relatedness(mat_tech_1st_KR_Top4, method = "cosine")

Relatedness_KR$Jaccard_top4 <- mean(mat_tech_1st_KR_Top4_rel_jacc)
Relatedness_KR$Association_top4 <- mean(mat_tech_1st_KR_Top4_rel_asso)
Relatedness_KR$Cosine_top4<- mean(mat_tech_1st_KR_Top4_rel_cosi)

#then select only the top 3 areas:
IPC_all_patents_1st_KR_Top3 <- IPC_all_patents_1st_KR[IPC_all_patents_1st_KR$techn_field_nr == "4" | 
                                                         IPC_all_patents_1st_KR$techn_field_nr == "5"| 
                                                         IPC_all_patents_1st_KR$techn_field_nr == "11", ]


mat_tech_1st_KR_Top3 <- create_sparse_matrix(i = IPC_all_patents_1st_KR_Top3 %>% pull(appln_id),
                                              j = IPC_all_patents_1st_KR_Top3 %>% pull(techn_field_nr))

mat_tech_1st_KR_Top3 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_1st_KR_Top3_rel_jacc <- relatedness(mat_tech_1st_KR_Top3, method = "Jaccard")
mat_tech_1st_KR_Top3_rel_asso <- relatedness(mat_tech_1st_KR_Top3, method = "association")
mat_tech_1st_KR_Top3_rel_cosi <- relatedness(mat_tech_1st_KR_Top3, method = "cosine")

Relatedness_KR$Jaccard_Top3 <- mean(mat_tech_1st_KR_Top3_rel_jacc)
Relatedness_KR$Association_Top3 <- mean(mat_tech_1st_KR_Top3_rel_asso)
Relatedness_KR$Cosine_Top3<- mean(mat_tech_1st_KR_Top3_rel_cosi)

#and finally the top 7 non-AI areas:
IPC_all_patents_1st_KR_Top7 <- IPC_all_patents_1st_KR[IPC_all_patents_1st_KR$techn_field_nr == "1"| 
                                                        IPC_all_patents_1st_KR$techn_field_nr == "2"| 
                                                        IPC_all_patents_1st_KR$techn_field_nr == "3"|
                                                        #IPC_all_patents_1st_KR$techn_field_nr == "4"| 
                                                        IPC_all_patents_1st_KR$techn_field_nr == "13"| 
                                                        IPC_all_patents_1st_KR$techn_field_nr == "25"| 
                                                        IPC_all_patents_1st_KR$techn_field_nr == "34", ]

mat_tech_1st_KR_Top7 <- create_sparse_matrix(i = IPC_all_patents_1st_KR_Top7 %>% pull(appln_id),
                                             j = IPC_all_patents_1st_KR_Top7 %>% pull(techn_field_nr))

mat_tech_1st_KR_Top7 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_1st_KR_Top7_rel_jacc <- relatedness(mat_tech_1st_KR_Top7, method = "Jaccard")
mat_tech_1st_KR_Top7_rel_asso <- relatedness(mat_tech_1st_KR_Top7, method = "association")
mat_tech_1st_KR_Top7_rel_cosi <- relatedness(mat_tech_1st_KR_Top7, method = "cosine")

Relatedness_KR$Jaccard_Top7 <- mean(mat_tech_1st_KR_Top7_rel_jacc)
Relatedness_KR$Association_Top7 <- mean(mat_tech_1st_KR_Top7_rel_asso)
Relatedness_KR$Cosine_Top7<- mean(mat_tech_1st_KR_Top7_rel_cosi)

#Japan
mat_tech_1st_JP <- create_sparse_matrix(i = IPC_all_patents_1st_JP %>% pull(appln_id),
                                        j = IPC_all_patents_1st_JP %>% pull(techn_field_nr))

mat_tech_1st_JP %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_1st_JP_rel_jacc <- relatedness(mat_tech_1st_JP, method = "Jaccard")
mat_tech_1st_JP_rel_asso <- relatedness(mat_tech_1st_JP, method = "association")
mat_tech_1st_JP_rel_cosi <- relatedness(mat_tech_1st_JP, method = "cosine")

Relatedness_JP <- as.data.frame(mean(mat_tech_1st_JP_rel_jacc))
Relatedness_JP$mat_tech_1st_JP_rel_asso <- mean(mat_tech_1st_JP_rel_asso)
Relatedness_JP$mat_tech_1st_JP_rel_cosi<- mean(mat_tech_1st_JP_rel_cosi)
rownames(Relatedness_JP) <- c("JP")
names(Relatedness_JP) <- c("Jaccard", "Association", "Cosine")
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

mat_tech_1st_JP_Top4_rel_jacc <- relatedness(mat_tech_1st_JP_Top4, method = "Jaccard")
mat_tech_1st_JP_Top4_rel_asso <- relatedness(mat_tech_1st_JP_Top4, method = "association")
mat_tech_1st_JP_Top4_rel_cosi <- relatedness(mat_tech_1st_JP_Top4, method = "cosine")

Relatedness_JP$Jaccard_top4 <- mean(mat_tech_1st_JP_Top4_rel_jacc)
Relatedness_JP$Association_top4 <- mean(mat_tech_1st_JP_Top4_rel_asso)
Relatedness_JP$Cosine_top4<- mean(mat_tech_1st_JP_Top4_rel_cosi)

#then select only the top 3 areas:
IPC_all_patents_1st_JP_Top3 <- IPC_all_patents_1st_JP[IPC_all_patents_1st_JP$techn_field_nr == "4" | 
                                                         IPC_all_patents_1st_JP$techn_field_nr == "5"| 
                                                         IPC_all_patents_1st_JP$techn_field_nr == "11", ]

mat_tech_1st_JP_Top3 <- create_sparse_matrix(i = IPC_all_patents_1st_JP_Top3 %>% pull(appln_id),
                                              j = IPC_all_patents_1st_JP_Top3 %>% pull(techn_field_nr))

mat_tech_1st_JP_Top3 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_1st_JP_Top3_rel_jacc <- relatedness(mat_tech_1st_JP_Top3, method = "Jaccard")
mat_tech_1st_JP_Top3_rel_asso <- relatedness(mat_tech_1st_JP_Top3, method = "association")
mat_tech_1st_JP_Top3_rel_cosi <- relatedness(mat_tech_1st_JP_Top3, method = "cosine")

Relatedness_JP$Jaccard_Top3 <- mean(mat_tech_1st_JP_Top3_rel_jacc)
Relatedness_JP$Association_Top3 <- mean(mat_tech_1st_JP_Top3_rel_asso)
Relatedness_JP$Cosine_Top3<- mean(mat_tech_1st_JP_Top3_rel_cosi)

#and finally the top 7 non-AI areas:
IPC_all_patents_1st_JP_Top7 <- IPC_all_patents_1st_JP[IPC_all_patents_1st_JP$techn_field_nr == "1"| 
                                                        IPC_all_patents_1st_JP$techn_field_nr == "2"| 
                                                        IPC_all_patents_1st_JP$techn_field_nr == "3"|
                                                       #IPC_all_patents_1st_JP$techn_field_nr == "4"| 
                                                        IPC_all_patents_1st_JP$techn_field_nr == "13"| 
                                                        IPC_all_patents_1st_JP$techn_field_nr == "25"| 
                                                        IPC_all_patents_1st_JP$techn_field_nr == "34", ]

mat_tech_1st_JP_Top7 <- create_sparse_matrix(i = IPC_all_patents_1st_JP_Top7 %>% pull(appln_id),
                                             j = IPC_all_patents_1st_JP_Top7 %>% pull(techn_field_nr))

mat_tech_1st_JP_Top7 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_1st_JP_Top7_rel_jacc <- relatedness(mat_tech_1st_JP_Top7, method = "Jaccard")
mat_tech_1st_JP_Top7_rel_asso <- relatedness(mat_tech_1st_JP_Top7, method = "association")
mat_tech_1st_JP_Top7_rel_cosi <- relatedness(mat_tech_1st_JP_Top7, method = "cosine")

Relatedness_JP$Jaccard_Top7 <- mean(mat_tech_1st_JP_Top7_rel_jacc)
Relatedness_JP$Association_Top7 <- mean(mat_tech_1st_JP_Top7_rel_asso)
Relatedness_JP$Cosine_Top7<- mean(mat_tech_1st_JP_Top7_rel_cosi)

#AI
mat_tech_1st_AI <- create_sparse_matrix(i = IPC_all_patents_1st_AI %>% pull(appln_id),
                                        j = IPC_all_patents_1st_AI %>% pull(techn_field_nr))

mat_tech_1st_AI %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_1st_AI_rel_jacc <- relatedness(mat_tech_1st_AI, method = "Jaccard")
mat_tech_1st_AI_rel_asso <- relatedness(mat_tech_1st_AI, method = "association")
mat_tech_1st_AI_rel_cosi <- relatedness(mat_tech_1st_AI, method = "cosine")

Relatedness_AI <- as.data.frame(mean(mat_tech_1st_AI_rel_jacc))
Relatedness_AI$mat_tech_1st_AI_rel_asso <- mean(mat_tech_1st_AI_rel_asso)
Relatedness_AI$mat_tech_1st_AI_rel_cosi<- mean(mat_tech_1st_AI_rel_cosi)
rownames(Relatedness_AI) <- c("AI")
names(Relatedness_AI) <- c("Jaccard", "Association", "Cosine")
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

mat_tech_1st_AI_Top4_rel_jacc <- relatedness(mat_tech_1st_AI_Top4, method = "Jaccard")
mat_tech_1st_AI_Top4_rel_asso <- relatedness(mat_tech_1st_AI_Top4, method = "association")
mat_tech_1st_AI_Top4_rel_cosi <- relatedness(mat_tech_1st_AI_Top4, method = "cosine")

Relatedness_AI$Jaccard_top4 <- mean(mat_tech_1st_AI_Top4_rel_jacc)
Relatedness_AI$Association_top4 <- mean(mat_tech_1st_AI_Top4_rel_asso)
Relatedness_AI$Cosine_top4<- mean(mat_tech_1st_AI_Top4_rel_cosi)

#then select only the top 3 areas:
IPC_all_patents_1st_AI_Top3 <- IPC_all_patents_1st_AI[IPC_all_patents_1st_AI$techn_field_nr == "4" | 
                                                         IPC_all_patents_1st_AI$techn_field_nr == "5"| 
                                                         IPC_all_patents_1st_AI$techn_field_nr == "11", ]

mat_tech_1st_AI_Top3 <- create_sparse_matrix(i = IPC_all_patents_1st_AI_Top3 %>% pull(appln_id),
                                             j = IPC_all_patents_1st_AI_Top3 %>% pull(techn_field_nr))

mat_tech_1st_AI_Top3 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_1st_AI_Top3_rel_jacc <- relatedness(mat_tech_1st_AI_Top3, method = "Jaccard")
mat_tech_1st_AI_Top3_rel_asso <- relatedness(mat_tech_1st_AI_Top3, method = "association")
mat_tech_1st_AI_Top3_rel_cosi <- relatedness(mat_tech_1st_AI_Top3, method = "cosine")

Relatedness_AI$Jaccard_Top3 <- mean(mat_tech_1st_AI_Top3_rel_jacc)
Relatedness_AI$Association_Top3 <- mean(mat_tech_1st_AI_Top3_rel_asso)
Relatedness_AI$Cosine_Top3<- mean(mat_tech_1st_AI_Top3_rel_cosi)

#and finally the top 7 non-AI areas:
IPC_all_patents_1st_AI_Top7 <- IPC_all_patents_1st_AI[IPC_all_patents_1st_AI$techn_field_nr == "1"| 
                                                        IPC_all_patents_1st_AI$techn_field_nr == "2"| 
                                                        IPC_all_patents_1st_AI$techn_field_nr == "3"|
                                                        #IPC_all_patents_1st_AI$techn_field_nr == "4"| 
                                                        IPC_all_patents_1st_AI$techn_field_nr == "13"| 
                                                        IPC_all_patents_1st_AI$techn_field_nr == "25"| 
                                                        IPC_all_patents_1st_AI$techn_field_nr == "34", ]

mat_tech_1st_AI_Top7 <- create_sparse_matrix(i = IPC_all_patents_1st_AI_Top7 %>% pull(appln_id),
                                              j = IPC_all_patents_1st_AI_Top7 %>% pull(techn_field_nr))

mat_tech_1st_AI_Top7 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_1st_AI_Top7_rel_jacc <- relatedness(mat_tech_1st_AI_Top7, method = "Jaccard")
mat_tech_1st_AI_Top7_rel_asso <- relatedness(mat_tech_1st_AI_Top7, method = "association")
mat_tech_1st_AI_Top7_rel_cosi <- relatedness(mat_tech_1st_AI_Top7, method = "cosine")

Relatedness_AI$Jaccard_Top7 <- mean(mat_tech_1st_AI_Top7_rel_jacc)
Relatedness_AI$Association_Top7 <- mean(mat_tech_1st_AI_Top7_rel_asso)
Relatedness_AI$Cosine_Top7<- mean(mat_tech_1st_AI_Top7_rel_cosi)

#and we merge it all together:
Relatedness_FirstPeriod <- rbind(Relatedness, Relatedness_CN, Relatedness_KR, Relatedness_JP, Relatedness_AI)
Relatedness_FirstPeriod <- Relatedness_FirstPeriod[,c((1:3), (5:13), (4))]

write.csv2(Relatedness_FirstPeriod, file = "Data_calculations_IPC/Relatedness_1st_period_IPC.csv", row.names = TRUE)

#3.2. Second period ----
#Starting with an empty global environment:
rm(list=ls())
setwd("C:/Users/Matheus/Desktop")
#3.2.1.Load the data we need and filter it -----
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
patents_AI_specific <- read.csv("Data_IPC/IPCs_AI.csv", sep = ";", header = TRUE, dec=",")
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

#3.2.2. Calculate the Indicators -----
IPC_all_patents_2nd_In <- IPC_all_patents_2nd[,c((-1), (-4), (-5))]
mat_2nd <- as.data.frame(table(IPC_all_patents_2nd_In$ctry_code, IPC_all_patents_2nd_In$techn_field_nr))
mat_2nd <- get.matrix(mat_2nd)

Indicators <- as.data.frame(Herfindahl(mat_2nd))
names(Indicators) <- "Herfindahl"
mat_2nd_RCAs <- location.quotient(mat_2nd, binary = T)
Indicators$Herfindahl_RCA <- Herfindahl(mat_2nd_RCAs)
Indicators$Entropy <- entropy(mat_2nd)
Indicators$Entropy_RCA <- entropy(mat_2nd_RCAs)
Indicators$Period <- "2nd"

write.csv2(Indicators, file = "Data_calculations_IPC/Indicators_2nd_period_IPC.csv", row.names = TRUE)

#Knowledge complexity of fields and countries:
KnowledgeComp_2nd <- as.data.frame(MORt(mat_2nd))
KnowledgeComp_2nd$Step0 <- MORt(mat_2nd, steps = 0)
KnowledgeComp_2nd$Step1 <- MORt(mat_2nd, steps = 1)
KnowledgeComp_2nd$Step2 <- MORt(mat_2nd, steps = 2)

KnowledgeComp_2nd$RCA <- MORt(mat_2nd_RCAs)
KnowledgeComp_2nd$RCA_Step0 <- MORt(mat_2nd_RCAs, steps = 0)
KnowledgeComp_2nd$RCA_Step1 <- MORt(mat_2nd_RCAs, steps = 1)
KnowledgeComp_2nd$RCA_Step2 <- MORt(mat_2nd_RCAs, steps = 2)

#my calculations:
KnowledgeComp_PerCountry_2nd <- as.data.frame(mat_2nd*MORt(mat_2nd))
KnowledgeComp_PerCountry_2nd$Step <- "NoStep"

KnowledgeComp_PerCountry_2nd_Step0 <- as.data.frame(mat_2nd*MORt(mat_2nd, steps = 0))
KnowledgeComp_PerCountry_2nd_Step0$Step <- "Step0"

KnowledgeComp_PerCountry_2nd_Step1 <- as.data.frame(mat_2nd*MORt(mat_2nd, steps = 1))
KnowledgeComp_PerCountry_2nd_Step1$Step <- "Step1"

KnowledgeComp_PerCountry_2nd_Step2 <- as.data.frame(mat_2nd*MORt(mat_2nd, steps = 2))
KnowledgeComp_PerCountry_2nd_Step2$Step <- "Step2"

#Considering RCAs:
KnowledgeComp_PerCountry_2nd_RCA <- as.data.frame(mat_2nd_RCAs*MORt(mat_2nd_RCAs))
KnowledgeComp_PerCountry_2nd_RCA$Step <- "NoStep"

KnowledgeComp_PerCountry_2nd_Step0_RCA <- as.data.frame(mat_2nd_RCAs*MORt(mat_2nd_RCAs, steps = 0))
KnowledgeComp_PerCountry_2nd_Step0_RCA$Step <- "Step0"

KnowledgeComp_PerCountry_2nd_Step1_RCA <- as.data.frame(mat_2nd_RCAs*MORt(mat_2nd_RCAs, steps = 1))
KnowledgeComp_PerCountry_2nd_Step1_RCA$Step <- "Step1"

KnowledgeComp_PerCountry_2nd_Step2_RCA <- as.data.frame(mat_2nd_RCAs*MORt(mat_2nd_RCAs, steps = 2))
KnowledgeComp_PerCountry_2nd_Step2_RCA$Step <- "Step2"

KnowledgeComp_PerCountry_2nd_All <- rbind(KnowledgeComp_PerCountry_2nd, KnowledgeComp_PerCountry_2nd_Step0,
                                          KnowledgeComp_PerCountry_2nd_Step1, KnowledgeComp_PerCountry_2nd_Step2)

KnowledgeComp_PerCountry_2nd_All_RCAs <- rbind(KnowledgeComp_PerCountry_2nd_RCA, KnowledgeComp_PerCountry_2nd_Step0_RCA,
                                               KnowledgeComp_PerCountry_2nd_Step1_RCA, KnowledgeComp_PerCountry_2nd_Step2_RCA)

write.csv2(KnowledgeComp_2nd, file = "Data_calculations_IPC/KnowledgeComp_2nd.csv", row.names = TRUE)
write.csv2(KnowledgeComp_PerCountry_2nd_All, file = "Data_calculations_IPC/KnowledgeComp_PerCountry_2nd_All.csv", row.names = TRUE)
write.csv2(KnowledgeComp_PerCountry_2nd_All_RCAs, file = "Data_calculations_IPC/KnowledgeComp_PerCountry_2nd_All_RCAs.csv", row.names = TRUE)

#For AI complexity and Indicators:
IPC_all_patents_2nd_In <- IPC_all_patents_2nd[,c((-1), (-4), (-2))]
mat_2nd <- as.data.frame(table(IPC_all_patents_2nd_In$ctry_code2, IPC_all_patents_2nd_In$techn_field_nr))
mat_2nd <- get.matrix(mat_2nd)

Indicators <- as.data.frame(Herfindahl(mat_2nd))
names(Indicators) <- "Herfindahl"
mat_2nd_RCAs <- location.quotient(mat_2nd, binary = T)
Indicators$Herfindahl_RCA <- Herfindahl(mat_2nd_RCAs)
Indicators$Entropy <- entropy(mat_2nd)
Indicators$Entropy_RCA <- entropy(mat_2nd_RCAs)
Indicators$Period <- "2nd"

write.csv2(Indicators, file = "Data_calculations_IPC/Indicators_2nd_period_IPC_AI.csv", row.names = TRUE)
KnowledgeComp_2nd <- as.data.frame(MORt(mat_2nd))
KnowledgeComp_2nd$Step0 <- MORt(mat_2nd, steps = 0)
KnowledgeComp_2nd$Step1 <- MORt(mat_2nd, steps = 1)
KnowledgeComp_2nd$Step2 <- MORt(mat_2nd, steps = 2)

KnowledgeComp_2nd$RCA <- MORt(mat_2nd_RCAs)
KnowledgeComp_2nd$RCA_Step0 <- MORt(mat_2nd_RCAs, steps = 0)
KnowledgeComp_2nd$RCA_Step1 <- MORt(mat_2nd_RCAs, steps = 1)
KnowledgeComp_2nd$RCA_Step2 <- MORt(mat_2nd_RCAs, steps = 2)

KnowledgeComp_PerCountry_2nd <- as.data.frame(mat_2nd*MORt(mat_2nd))
KnowledgeComp_PerCountry_2nd$Step <- "NoStep"

KnowledgeComp_PerCountry_2nd_Step0 <- as.data.frame(mat_2nd*MORt(mat_2nd, steps = 0))
KnowledgeComp_PerCountry_2nd_Step0$Step <- "Step0"

KnowledgeComp_PerCountry_2nd_Step1 <- as.data.frame(mat_2nd*MORt(mat_2nd, steps = 1))
KnowledgeComp_PerCountry_2nd_Step1$Step <- "Step1"

KnowledgeComp_PerCountry_2nd_Step2 <- as.data.frame(mat_2nd*MORt(mat_2nd, steps = 2))
KnowledgeComp_PerCountry_2nd_Step2$Step <- "Step2"

#Considering RCAs:
KnowledgeComp_PerCountry_2nd_RCA <- as.data.frame(mat_2nd_RCAs*MORt(mat_2nd_RCAs))
KnowledgeComp_PerCountry_2nd_RCA$Step <- "NoStep"

KnowledgeComp_PerCountry_2nd_Step0_RCA <- as.data.frame(mat_2nd_RCAs*MORt(mat_2nd_RCAs, steps = 0))
KnowledgeComp_PerCountry_2nd_Step0_RCA$Step <- "Step0"

KnowledgeComp_PerCountry_2nd_Step1_RCA <- as.data.frame(mat_2nd_RCAs*MORt(mat_2nd_RCAs, steps = 1))
KnowledgeComp_PerCountry_2nd_Step1_RCA$Step <- "Step1"

KnowledgeComp_PerCountry_2nd_Step2_RCA <- as.data.frame(mat_2nd_RCAs*MORt(mat_2nd_RCAs, steps = 2))
KnowledgeComp_PerCountry_2nd_Step2_RCA$Step <- "Step2"

KnowledgeComp_PerCountry_2nd_All <- rbind(KnowledgeComp_PerCountry_2nd, KnowledgeComp_PerCountry_2nd_Step0,
                                          KnowledgeComp_PerCountry_2nd_Step1, KnowledgeComp_PerCountry_2nd_Step2)

KnowledgeComp_PerCountry_2nd_All_RCAs <- rbind(KnowledgeComp_PerCountry_2nd_RCA, KnowledgeComp_PerCountry_2nd_Step0_RCA,
                                               KnowledgeComp_PerCountry_2nd_Step1_RCA, KnowledgeComp_PerCountry_2nd_Step2_RCA)

write.csv2(KnowledgeComp_PerCountry_2nd_All, file = "Data_calculations_IPC/KnowledgeComp_PerCountry_2nd_All_AI.csv", row.names = TRUE)
write.csv2(KnowledgeComp_PerCountry_2nd_All_RCAs, file = "Data_calculations_IPC/KnowledgeComp_PerCountry_2nd_All_RCAs_AI.csv", row.names = TRUE)


#3.2.3. Calculate the relatedness -----
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

mat_tech_2nd_US_rel_jacc <- relatedness(mat_tech_2nd_US, method = "Jaccard")
mat_tech_2nd_US_rel_asso <- relatedness(mat_tech_2nd_US, method = "association")
mat_tech_2nd_US_rel_cosi <- relatedness(mat_tech_2nd_US, method = "cosine")

Relatedness <- as.data.frame(mean(mat_tech_2nd_US_rel_jacc))
Relatedness$mat_tech_2nd_US_rel_asso <- mean(mat_tech_2nd_US_rel_asso)
Relatedness$mat_tech_2nd_US_rel_cosi<- mean(mat_tech_2nd_US_rel_cosi)
rownames(Relatedness) <- c("US")
names(Relatedness) <- c("Jaccard", "Association", "Cosine")
Relatedness$Period <- "2nd"

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

mat_tech_2nd_US_Top4_rel_jacc <- relatedness(mat_tech_2nd_US_Top4, method = "Jaccard")
mat_tech_2nd_US_Top4_rel_asso <- relatedness(mat_tech_2nd_US_Top4, method = "association")
mat_tech_2nd_US_Top4_rel_cosi <- relatedness(mat_tech_2nd_US_Top4, method = "cosine")

Relatedness$Jaccard_top4 <- mean(mat_tech_2nd_US_Top4_rel_jacc)
Relatedness$Association_top4 <- mean(mat_tech_2nd_US_Top4_rel_asso)
Relatedness$Cosine_top4<- mean(mat_tech_2nd_US_Top4_rel_cosi)

#then select only the top 3 areas:
IPC_all_patents_2nd_US_Top3 <- IPC_all_patents_2nd_US[IPC_all_patents_2nd_US$techn_field_nr == "4" | 
                                                         IPC_all_patents_2nd_US$techn_field_nr == "5"| 
                                                         IPC_all_patents_2nd_US$techn_field_nr == "11", ]

mat_tech_2nd_US_Top3 <- create_sparse_matrix(i = IPC_all_patents_2nd_US_Top3 %>% pull(appln_id),
                                              j = IPC_all_patents_2nd_US_Top3 %>% pull(techn_field_nr))

mat_tech_2nd_US_Top3 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_2nd_US_Top3_rel_jacc <- relatedness(mat_tech_2nd_US_Top3, method = "Jaccard")
mat_tech_2nd_US_Top3_rel_asso <- relatedness(mat_tech_2nd_US_Top3, method = "association")
mat_tech_2nd_US_Top3_rel_cosi <- relatedness(mat_tech_2nd_US_Top3, method = "cosine")

Relatedness$Jaccard_Top3 <- mean(mat_tech_2nd_US_Top3_rel_jacc)
Relatedness$Association_Top3 <- mean(mat_tech_2nd_US_Top3_rel_asso)
Relatedness$Cosine_Top3<- mean(mat_tech_2nd_US_Top3_rel_cosi)

#and finally the top 7 non-AI areas:
IPC_all_patents_2nd_US_Top7 <- IPC_all_patents_2nd_US[IPC_all_patents_2nd_US$techn_field_nr == "1"| 
                                                        IPC_all_patents_2nd_US$techn_field_nr == "2"| 
                                                        IPC_all_patents_2nd_US$techn_field_nr == "3"|
                                                        #IPC_all_patents_2nd_US$techn_field_nr == "4"| 
                                                        IPC_all_patents_2nd_US$techn_field_nr == "13"| 
                                                        IPC_all_patents_2nd_US$techn_field_nr == "25"| 
                                                        IPC_all_patents_2nd_US$techn_field_nr == "34", ]

mat_tech_2nd_US_Top7 <- create_sparse_matrix(i = IPC_all_patents_2nd_US_Top7 %>% pull(appln_id),
                                             j = IPC_all_patents_2nd_US_Top7 %>% pull(techn_field_nr))

mat_tech_2nd_US_Top7 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_2nd_US_Top7_rel_jacc <- relatedness(mat_tech_2nd_US_Top7, method = "Jaccard")
mat_tech_2nd_US_Top7_rel_asso <- relatedness(mat_tech_2nd_US_Top7, method = "association")
mat_tech_2nd_US_Top7_rel_cosi <- relatedness(mat_tech_2nd_US_Top7, method = "cosine")

Relatedness$Jaccard_Top7 <- mean(mat_tech_2nd_US_Top7_rel_jacc)
Relatedness$Association_Top7 <- mean(mat_tech_2nd_US_Top7_rel_asso)
Relatedness$Cosine_Top7<- mean(mat_tech_2nd_US_Top7_rel_cosi)

#China:
mat_tech_2nd_CN <- create_sparse_matrix(i = IPC_all_patents_2nd_CN %>% pull(appln_id),
                                        j = IPC_all_patents_2nd_CN %>% pull(techn_field_nr))

mat_tech_2nd_CN %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_2nd_CN_rel_jacc <- relatedness(mat_tech_2nd_CN, method = "Jaccard")
mat_tech_2nd_CN_rel_asso <- relatedness(mat_tech_2nd_CN, method = "association")
mat_tech_2nd_CN_rel_cosi <- relatedness(mat_tech_2nd_CN, method = "cosine")

Relatedness_CN <- as.data.frame(mean(mat_tech_2nd_CN_rel_jacc))
Relatedness_CN$mat_tech_2nd_CN_rel_asso <- mean(mat_tech_2nd_CN_rel_asso)
Relatedness_CN$mat_tech_2nd_CN_rel_cosi<- mean(mat_tech_2nd_CN_rel_cosi)
rownames(Relatedness_CN) <- c("CN")
names(Relatedness_CN) <- c("Jaccard", "Association", "Cosine")
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

mat_tech_2nd_CN_Top4_rel_jacc <- relatedness(mat_tech_2nd_CN_Top4, method = "Jaccard")
mat_tech_2nd_CN_Top4_rel_asso <- relatedness(mat_tech_2nd_CN_Top4, method = "association")
mat_tech_2nd_CN_Top4_rel_cosi <- relatedness(mat_tech_2nd_CN_Top4, method = "cosine")

Relatedness_CN$Jaccard_top4 <- mean(mat_tech_2nd_CN_Top4_rel_jacc)
Relatedness_CN$Association_top4 <- mean(mat_tech_2nd_CN_Top4_rel_asso)
Relatedness_CN$Cosine_top4<- mean(mat_tech_2nd_CN_Top4_rel_cosi)

#then select only the top 3 areas:
IPC_all_patents_2nd_CN_Top3 <- IPC_all_patents_2nd_CN[IPC_all_patents_2nd_CN$techn_field_nr == "4" | 
                                                         IPC_all_patents_2nd_CN$techn_field_nr == "5"| 
                                                         IPC_all_patents_2nd_CN$techn_field_nr == "11", ]

mat_tech_2nd_CN_Top3 <- create_sparse_matrix(i = IPC_all_patents_2nd_CN_Top3 %>% pull(appln_id),
                                              j = IPC_all_patents_2nd_CN_Top3 %>% pull(techn_field_nr))

mat_tech_2nd_CN_Top3 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_2nd_CN_Top3_rel_jacc <- relatedness(mat_tech_2nd_CN_Top3, method = "Jaccard")
mat_tech_2nd_CN_Top3_rel_asso <- relatedness(mat_tech_2nd_CN_Top3, method = "association")
mat_tech_2nd_CN_Top3_rel_cosi <- relatedness(mat_tech_2nd_CN_Top3, method = "cosine")

Relatedness_CN$Jaccard_Top3 <- mean(mat_tech_2nd_CN_Top3_rel_jacc)
Relatedness_CN$Association_Top3 <- mean(mat_tech_2nd_CN_Top3_rel_asso)
Relatedness_CN$Cosine_Top3<- mean(mat_tech_2nd_CN_Top3_rel_cosi)

#and finally the top 7 non-AI areas:
IPC_all_patents_2nd_CN_Top7 <- IPC_all_patents_2nd_CN[IPC_all_patents_2nd_CN$techn_field_nr == "1"| 
                                                        IPC_all_patents_2nd_CN$techn_field_nr == "2"| 
                                                        IPC_all_patents_2nd_CN$techn_field_nr == "3"|
                                                        #IPC_all_patents_2nd_CN$techn_field_nr == "4"| 
                                                        IPC_all_patents_2nd_CN$techn_field_nr == "13"| 
                                                        IPC_all_patents_2nd_CN$techn_field_nr == "25"| 
                                                        IPC_all_patents_2nd_CN$techn_field_nr == "34", ]

mat_tech_2nd_CN_Top7 <- create_sparse_matrix(i = IPC_all_patents_2nd_CN_Top7 %>% pull(appln_id),
                                             j = IPC_all_patents_2nd_CN_Top7 %>% pull(techn_field_nr))

mat_tech_2nd_CN_Top7 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_2nd_CN_Top7_rel_jacc <- relatedness(mat_tech_2nd_CN_Top7, method = "Jaccard")
mat_tech_2nd_CN_Top7_rel_asso <- relatedness(mat_tech_2nd_CN_Top7, method = "association")
mat_tech_2nd_CN_Top7_rel_cosi <- relatedness(mat_tech_2nd_CN_Top7, method = "cosine")

Relatedness_CN$Jaccard_Top7 <- mean(mat_tech_2nd_CN_Top7_rel_jacc)
Relatedness_CN$Association_Top7 <- mean(mat_tech_2nd_CN_Top7_rel_asso)
Relatedness_CN$Cosine_Top7<- mean(mat_tech_2nd_CN_Top7_rel_cosi)


#KR
mat_tech_2nd_KR <- create_sparse_matrix(i = IPC_all_patents_2nd_KR %>% pull(appln_id),
                                        j = IPC_all_patents_2nd_KR %>% pull(techn_field_nr))

mat_tech_2nd_KR %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_2nd_KR_rel_jacc <- relatedness(mat_tech_2nd_KR, method = "Jaccard")
mat_tech_2nd_KR_rel_asso <- relatedness(mat_tech_2nd_KR, method = "association")
mat_tech_2nd_KR_rel_cosi <- relatedness(mat_tech_2nd_KR, method = "cosine")

Relatedness_KR <- as.data.frame(mean(mat_tech_2nd_KR_rel_jacc))
Relatedness_KR$mat_tech_2nd_KR_rel_asso <- mean(mat_tech_2nd_KR_rel_asso)
Relatedness_KR$mat_tech_2nd_KR_rel_cosi<- mean(mat_tech_2nd_KR_rel_cosi)
rownames(Relatedness_KR) <- c("KR")
names(Relatedness_KR) <- c("Jaccard", "Association", "Cosine")
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

mat_tech_2nd_KR_Top4_rel_jacc <- relatedness(mat_tech_2nd_KR_Top4, method = "Jaccard")
mat_tech_2nd_KR_Top4_rel_asso <- relatedness(mat_tech_2nd_KR_Top4, method = "association")
mat_tech_2nd_KR_Top4_rel_cosi <- relatedness(mat_tech_2nd_KR_Top4, method = "cosine")

Relatedness_KR$Jaccard_top4 <- mean(mat_tech_2nd_KR_Top4_rel_jacc)
Relatedness_KR$Association_top4 <- mean(mat_tech_2nd_KR_Top4_rel_asso)
Relatedness_KR$Cosine_top4<- mean(mat_tech_2nd_KR_Top4_rel_cosi)

#then select only the top 3 areas:
IPC_all_patents_2nd_KR_Top3 <- IPC_all_patents_2nd_KR[IPC_all_patents_2nd_KR$techn_field_nr == "4" | 
                                                         IPC_all_patents_2nd_KR$techn_field_nr == "5"| 
                                                         IPC_all_patents_2nd_KR$techn_field_nr == "11", ]

mat_tech_2nd_KR_Top3 <- create_sparse_matrix(i = IPC_all_patents_2nd_KR_Top3 %>% pull(appln_id),
                                              j = IPC_all_patents_2nd_KR_Top3 %>% pull(techn_field_nr))

mat_tech_2nd_KR_Top3 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_2nd_KR_Top3_rel_jacc <- relatedness(mat_tech_2nd_KR_Top3, method = "Jaccard")
mat_tech_2nd_KR_Top3_rel_asso <- relatedness(mat_tech_2nd_KR_Top3, method = "association")
mat_tech_2nd_KR_Top3_rel_cosi <- relatedness(mat_tech_2nd_KR_Top3, method = "cosine")

Relatedness_KR$Jaccard_Top3 <- mean(mat_tech_2nd_KR_Top3_rel_jacc)
Relatedness_KR$Association_Top3 <- mean(mat_tech_2nd_KR_Top3_rel_asso)
Relatedness_KR$Cosine_Top3<- mean(mat_tech_2nd_KR_Top3_rel_cosi)

#and finally the top 7 non-AI areas:
IPC_all_patents_2nd_KR_Top7 <- IPC_all_patents_2nd_KR[IPC_all_patents_2nd_KR$techn_field_nr == "1"| 
                                                        IPC_all_patents_2nd_KR$techn_field_nr == "2"| 
                                                        IPC_all_patents_2nd_KR$techn_field_nr == "3"|
                                                        #IPC_all_patents_2nd_KR$techn_field_nr == "4"| 
                                                        IPC_all_patents_2nd_KR$techn_field_nr == "13"| 
                                                        IPC_all_patents_2nd_KR$techn_field_nr == "25"| 
                                                        IPC_all_patents_2nd_KR$techn_field_nr == "34", ]

mat_tech_2nd_KR_Top7 <- create_sparse_matrix(i = IPC_all_patents_2nd_KR_Top7 %>% pull(appln_id),
                                             j = IPC_all_patents_2nd_KR_Top7 %>% pull(techn_field_nr))

mat_tech_2nd_KR_Top7 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_2nd_KR_Top7_rel_jacc <- relatedness(mat_tech_2nd_KR_Top7, method = "Jaccard")
mat_tech_2nd_KR_Top7_rel_asso <- relatedness(mat_tech_2nd_KR_Top7, method = "association")
mat_tech_2nd_KR_Top7_rel_cosi <- relatedness(mat_tech_2nd_KR_Top7, method = "cosine")

Relatedness_KR$Jaccard_Top7 <- mean(mat_tech_2nd_KR_Top7_rel_jacc)
Relatedness_KR$Association_Top7 <- mean(mat_tech_2nd_KR_Top7_rel_asso)
Relatedness_KR$Cosine_Top7<- mean(mat_tech_2nd_KR_Top7_rel_cosi)

#Japan
mat_tech_2nd_JP <- create_sparse_matrix(i = IPC_all_patents_2nd_JP %>% pull(appln_id),
                                        j = IPC_all_patents_2nd_JP %>% pull(techn_field_nr))

mat_tech_2nd_JP %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_2nd_JP_rel_jacc <- relatedness(mat_tech_2nd_JP, method = "Jaccard")
mat_tech_2nd_JP_rel_asso <- relatedness(mat_tech_2nd_JP, method = "association")
mat_tech_2nd_JP_rel_cosi <- relatedness(mat_tech_2nd_JP, method = "cosine")

Relatedness_JP <- as.data.frame(mean(mat_tech_2nd_JP_rel_jacc))
Relatedness_JP$mat_tech_2nd_JP_rel_asso <- mean(mat_tech_2nd_JP_rel_asso)
Relatedness_JP$mat_tech_2nd_JP_rel_cosi<- mean(mat_tech_2nd_JP_rel_cosi)
rownames(Relatedness_JP) <- c("JP")
names(Relatedness_JP) <- c("Jaccard", "Association", "Cosine")
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

mat_tech_2nd_JP_Top4_rel_jacc <- relatedness(mat_tech_2nd_JP_Top4, method = "Jaccard")
mat_tech_2nd_JP_Top4_rel_asso <- relatedness(mat_tech_2nd_JP_Top4, method = "association")
mat_tech_2nd_JP_Top4_rel_cosi <- relatedness(mat_tech_2nd_JP_Top4, method = "cosine")

Relatedness_JP$Jaccard_top4 <- mean(mat_tech_2nd_JP_Top4_rel_jacc)
Relatedness_JP$Association_top4 <- mean(mat_tech_2nd_JP_Top4_rel_asso)
Relatedness_JP$Cosine_top4<- mean(mat_tech_2nd_JP_Top4_rel_cosi)

#then select only the top 3 areas:
IPC_all_patents_2nd_JP_Top3 <- IPC_all_patents_2nd_JP[IPC_all_patents_2nd_JP$techn_field_nr == "4" | 
                                                         IPC_all_patents_2nd_JP$techn_field_nr == "5"| 
                                                         IPC_all_patents_2nd_JP$techn_field_nr == "11", ]

mat_tech_2nd_JP_Top3 <- create_sparse_matrix(i = IPC_all_patents_2nd_JP_Top3 %>% pull(appln_id),
                                              j = IPC_all_patents_2nd_JP_Top3 %>% pull(techn_field_nr))

mat_tech_2nd_JP_Top3 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_2nd_JP_Top3_rel_jacc <- relatedness(mat_tech_2nd_JP_Top3, method = "Jaccard")
mat_tech_2nd_JP_Top3_rel_asso <- relatedness(mat_tech_2nd_JP_Top3, method = "association")
mat_tech_2nd_JP_Top3_rel_cosi <- relatedness(mat_tech_2nd_JP_Top3, method = "cosine")

Relatedness_JP$Jaccard_Top3 <- mean(mat_tech_2nd_JP_Top3_rel_jacc)
Relatedness_JP$Association_Top3 <- mean(mat_tech_2nd_JP_Top3_rel_asso)
Relatedness_JP$Cosine_Top3<- mean(mat_tech_2nd_JP_Top3_rel_cosi)

#and finally the top 7 non-AI areas:
IPC_all_patents_2nd_JP_Top7 <- IPC_all_patents_2nd_JP[IPC_all_patents_2nd_JP$techn_field_nr == "1"| 
                                                        IPC_all_patents_2nd_JP$techn_field_nr == "2"| 
                                                        IPC_all_patents_2nd_JP$techn_field_nr == "3"|
                                                        #IPC_all_patents_2nd_JP$techn_field_nr == "4"| 
                                                        IPC_all_patents_2nd_JP$techn_field_nr == "13"| 
                                                        IPC_all_patents_2nd_JP$techn_field_nr == "25"| 
                                                        IPC_all_patents_2nd_JP$techn_field_nr == "34", ]

mat_tech_2nd_JP_Top7 <- create_sparse_matrix(i = IPC_all_patents_2nd_JP_Top7 %>% pull(appln_id),
                                             j = IPC_all_patents_2nd_JP_Top7 %>% pull(techn_field_nr))

mat_tech_2nd_JP_Top7 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_2nd_JP_Top7_rel_jacc <- relatedness(mat_tech_2nd_JP_Top7, method = "Jaccard")
mat_tech_2nd_JP_Top7_rel_asso <- relatedness(mat_tech_2nd_JP_Top7, method = "association")
mat_tech_2nd_JP_Top7_rel_cosi <- relatedness(mat_tech_2nd_JP_Top7, method = "cosine")

Relatedness_JP$Jaccard_Top7 <- mean(mat_tech_2nd_JP_Top7_rel_jacc)
Relatedness_JP$Association_Top7 <- mean(mat_tech_2nd_JP_Top7_rel_asso)
Relatedness_JP$Cosine_Top7<- mean(mat_tech_2nd_JP_Top7_rel_cosi)

#AI
mat_tech_2nd_AI <- create_sparse_matrix(i = IPC_all_patents_2nd_AI %>% pull(appln_id),
                                        j = IPC_all_patents_2nd_AI %>% pull(techn_field_nr))

mat_tech_2nd_AI %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_2nd_AI_rel_jacc <- relatedness(mat_tech_2nd_AI, method = "Jaccard")
mat_tech_2nd_AI_rel_asso <- relatedness(mat_tech_2nd_AI, method = "association")
mat_tech_2nd_AI_rel_cosi <- relatedness(mat_tech_2nd_AI, method = "cosine")

Relatedness_AI <- as.data.frame(mean(mat_tech_2nd_AI_rel_jacc))
Relatedness_AI$mat_tech_2nd_AI_rel_asso <- mean(mat_tech_2nd_AI_rel_asso)
Relatedness_AI$mat_tech_2nd_AI_rel_cosi<- mean(mat_tech_2nd_AI_rel_cosi)
rownames(Relatedness_AI) <- c("AI")
names(Relatedness_AI) <- c("Jaccard", "Association", "Cosine")
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

mat_tech_2nd_AI_Top4_rel_jacc <- relatedness(mat_tech_2nd_AI_Top4, method = "Jaccard")
mat_tech_2nd_AI_Top4_rel_asso <- relatedness(mat_tech_2nd_AI_Top4, method = "association")
mat_tech_2nd_AI_Top4_rel_cosi <- relatedness(mat_tech_2nd_AI_Top4, method = "cosine")

Relatedness_AI$Jaccard_top4 <- mean(mat_tech_2nd_AI_Top4_rel_jacc)
Relatedness_AI$Association_top4 <- mean(mat_tech_2nd_AI_Top4_rel_asso)
Relatedness_AI$Cosine_top4<- mean(mat_tech_2nd_AI_Top4_rel_cosi)

#then select only the top 3 areas:
IPC_all_patents_2nd_AI_Top3 <- IPC_all_patents_2nd_AI[IPC_all_patents_2nd_AI$techn_field_nr == "4" | 
                                                         IPC_all_patents_2nd_AI$techn_field_nr == "5"| 
                                                         IPC_all_patents_2nd_AI$techn_field_nr == "11", ]

mat_tech_2nd_AI_Top3 <- create_sparse_matrix(i = IPC_all_patents_2nd_AI_Top3 %>% pull(appln_id),
                                              j = IPC_all_patents_2nd_AI_Top3 %>% pull(techn_field_nr))

mat_tech_2nd_AI_Top3 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_2nd_AI_Top3_rel_jacc <- relatedness(mat_tech_2nd_AI_Top3, method = "Jaccard")
mat_tech_2nd_AI_Top3_rel_asso <- relatedness(mat_tech_2nd_AI_Top3, method = "association")
mat_tech_2nd_AI_Top3_rel_cosi <- relatedness(mat_tech_2nd_AI_Top3, method = "cosine")

Relatedness_AI$Jaccard_Top3 <- mean(mat_tech_2nd_AI_Top3_rel_jacc)
Relatedness_AI$Association_Top3 <- mean(mat_tech_2nd_AI_Top3_rel_asso)
Relatedness_AI$Cosine_Top3<- mean(mat_tech_2nd_AI_Top3_rel_cosi)

#and finally the top 7 non-AI areas:
IPC_all_patents_2nd_AI_Top7 <- IPC_all_patents_2nd_AI[IPC_all_patents_2nd_AI$techn_field_nr == "1"| 
                                                        IPC_all_patents_2nd_AI$techn_field_nr == "2"| 
                                                        IPC_all_patents_2nd_AI$techn_field_nr == "3"|
                                                        #IPC_all_patents_2nd_AI$techn_field_nr == "4"| 
                                                        IPC_all_patents_2nd_AI$techn_field_nr == "13"| 
                                                        IPC_all_patents_2nd_AI$techn_field_nr == "25"| 
                                                        IPC_all_patents_2nd_AI$techn_field_nr == "34", ]

mat_tech_2nd_AI_Top7 <- create_sparse_matrix(i = IPC_all_patents_2nd_AI_Top7 %>% pull(appln_id),
                                             j = IPC_all_patents_2nd_AI_Top7 %>% pull(techn_field_nr))

mat_tech_2nd_AI_Top7 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_2nd_AI_Top7_rel_jacc <- relatedness(mat_tech_2nd_AI_Top7, method = "Jaccard")
mat_tech_2nd_AI_Top7_rel_asso <- relatedness(mat_tech_2nd_AI_Top7, method = "association")
mat_tech_2nd_AI_Top7_rel_cosi <- relatedness(mat_tech_2nd_AI_Top7, method = "cosine")

Relatedness_AI$Jaccard_Top7 <- mean(mat_tech_2nd_AI_Top7_rel_jacc)
Relatedness_AI$Association_Top7 <- mean(mat_tech_2nd_AI_Top7_rel_asso)
Relatedness_AI$Cosine_Top7<- mean(mat_tech_2nd_AI_Top7_rel_cosi)

#and we merge it all together:
Relatedness_SecondPeriod <- rbind(Relatedness, Relatedness_CN, Relatedness_KR, Relatedness_JP, Relatedness_AI)
Relatedness_SecondPeriod <- Relatedness_SecondPeriod[,c((1:3), (5:13), (4))]

write.csv2(Relatedness_SecondPeriod, file = "Data_calculations_IPC/Relatedness_2nd_period_IPC.csv", row.names = TRUE)

#3.3. Third period ----
#Starting with an empty global environment:
rm(list=ls())
setwd("C:/Users/Matheus/Desktop")
#3.3.1.Load the data we need and filter it -----
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
patents_AI_specific <- read.csv("Data_IPC/IPCs_AI.csv", sep = ";", header = TRUE, dec=",")
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

#3.2.2. Calculate the Indicators -----
IPC_all_patents_3rd_In <- IPC_all_patents_3rd[,c((-1), (-4), (-5))]
mat_3rd <- as.data.frame(table(IPC_all_patents_3rd_In$ctry_code, IPC_all_patents_3rd_In$techn_field_nr))
mat_3rd <- get.matrix(mat_3rd)

Indicators <- as.data.frame(Herfindahl(mat_3rd))
names(Indicators) <- "Herfindahl"
mat_3rd_RCAs <- location.quotient(mat_3rd, binary = T)
Indicators$Herfindahl_RCA <- Herfindahl(mat_3rd_RCAs)
Indicators$Entropy <- entropy(mat_3rd)
Indicators$Entropy_RCA <- entropy(mat_3rd_RCAs)
Indicators$Period <- "3rd"

write.csv2(Indicators, file = "Data_calculations_IPC/Indicators_3rd_period_IPC.csv", row.names = TRUE)

#Knowledge complexity of fields and countries:
KnowledgeComp_3rd <- as.data.frame(MORt(mat_3rd))
KnowledgeComp_3rd$Step0 <- MORt(mat_3rd, steps = 0)
KnowledgeComp_3rd$Step1 <- MORt(mat_3rd, steps = 1)
KnowledgeComp_3rd$Step2 <- MORt(mat_3rd, steps = 2)

KnowledgeComp_3rd$RCA <- MORt(mat_3rd_RCAs)
KnowledgeComp_3rd$RCA_Step0 <- MORt(mat_3rd_RCAs, steps = 0)
KnowledgeComp_3rd$RCA_Step1 <- MORt(mat_3rd_RCAs, steps = 1)
KnowledgeComp_3rd$RCA_Step2 <- MORt(mat_3rd_RCAs, steps = 2)

#my calculations:
KnowledgeComp_PerCountry_3rd <- as.data.frame(mat_3rd*MORt(mat_3rd))
KnowledgeComp_PerCountry_3rd$Step <- "NoStep"

KnowledgeComp_PerCountry_3rd_Step0 <- as.data.frame(mat_3rd*MORt(mat_3rd, steps = 0))
KnowledgeComp_PerCountry_3rd_Step0$Step <- "Step0"

KnowledgeComp_PerCountry_3rd_Step1 <- as.data.frame(mat_3rd*MORt(mat_3rd, steps = 1))
KnowledgeComp_PerCountry_3rd_Step1$Step <- "Step1"

KnowledgeComp_PerCountry_3rd_Step2 <- as.data.frame(mat_3rd*MORt(mat_3rd, steps = 2))
KnowledgeComp_PerCountry_3rd_Step2$Step <- "Step2"

#Considering RCAs:
KnowledgeComp_PerCountry_3rd_RCA <- as.data.frame(mat_3rd_RCAs*MORt(mat_3rd_RCAs))
KnowledgeComp_PerCountry_3rd_RCA$Step <- "NoStep"

KnowledgeComp_PerCountry_3rd_Step0_RCA <- as.data.frame(mat_3rd_RCAs*MORt(mat_3rd_RCAs, steps = 0))
KnowledgeComp_PerCountry_3rd_Step0_RCA$Step <- "Step0"

KnowledgeComp_PerCountry_3rd_Step1_RCA <- as.data.frame(mat_3rd_RCAs*MORt(mat_3rd_RCAs, steps = 1))
KnowledgeComp_PerCountry_3rd_Step1_RCA$Step <- "Step1"

KnowledgeComp_PerCountry_3rd_Step2_RCA <- as.data.frame(mat_3rd_RCAs*MORt(mat_3rd_RCAs, steps = 2))
KnowledgeComp_PerCountry_3rd_Step2_RCA$Step <- "Step2"

KnowledgeComp_PerCountry_3rd_All <- rbind(KnowledgeComp_PerCountry_3rd, KnowledgeComp_PerCountry_3rd_Step0,
                                          KnowledgeComp_PerCountry_3rd_Step1, KnowledgeComp_PerCountry_3rd_Step2)

KnowledgeComp_PerCountry_3rd_All_RCAs <- rbind(KnowledgeComp_PerCountry_3rd_RCA, KnowledgeComp_PerCountry_3rd_Step0_RCA,
                                               KnowledgeComp_PerCountry_3rd_Step1_RCA, KnowledgeComp_PerCountry_3rd_Step2_RCA)

write.csv2(KnowledgeComp_3rd, file = "Data_calculations_IPC/KnowledgeComp_3rd.csv", row.names = TRUE)
write.csv2(KnowledgeComp_PerCountry_3rd_All, file = "Data_calculations_IPC/KnowledgeComp_PerCountry_3rd_All.csv", row.names = TRUE)
write.csv2(KnowledgeComp_PerCountry_3rd_All_RCAs, file = "Data_calculations_IPC/KnowledgeComp_PerCountry_3rd_All_RCAs.csv", row.names = TRUE)

#For AI complexity and Indicators:
IPC_all_patents_3rd_In <- IPC_all_patents_3rd[,c((-1), (-4), (-2))]
mat_3rd <- as.data.frame(table(IPC_all_patents_3rd_In$ctry_code2, IPC_all_patents_3rd_In$techn_field_nr))
mat_3rd <- get.matrix(mat_3rd)

Indicators <- as.data.frame(Herfindahl(mat_3rd))
names(Indicators) <- "Herfindahl"
mat_3rd_RCAs <- location.quotient(mat_3rd, binary = T)
Indicators$Herfindahl_RCA <- Herfindahl(mat_3rd_RCAs)
Indicators$Entropy <- entropy(mat_3rd)
Indicators$Entropy_RCA <- entropy(mat_3rd_RCAs)
Indicators$Period <- "3rd"

write.csv2(Indicators, file = "Data_calculations_IPC/Indicators_3rd_period_IPC_AI.csv", row.names = TRUE)
KnowledgeComp_3rd <- as.data.frame(MORt(mat_3rd))
KnowledgeComp_3rd$Step0 <- MORt(mat_3rd, steps = 0)
KnowledgeComp_3rd$Step1 <- MORt(mat_3rd, steps = 1)
KnowledgeComp_3rd$Step2 <- MORt(mat_3rd, steps = 2)

KnowledgeComp_3rd$RCA <- MORt(mat_3rd_RCAs)
KnowledgeComp_3rd$RCA_Step0 <- MORt(mat_3rd_RCAs, steps = 0)
KnowledgeComp_3rd$RCA_Step1 <- MORt(mat_3rd_RCAs, steps = 1)
KnowledgeComp_3rd$RCA_Step2 <- MORt(mat_3rd_RCAs, steps = 2)

KnowledgeComp_PerCountry_3rd <- as.data.frame(mat_3rd*MORt(mat_3rd))
KnowledgeComp_PerCountry_3rd$Step <- "NoStep"

KnowledgeComp_PerCountry_3rd_Step0 <- as.data.frame(mat_3rd*MORt(mat_3rd, steps = 0))
KnowledgeComp_PerCountry_3rd_Step0$Step <- "Step0"

KnowledgeComp_PerCountry_3rd_Step1 <- as.data.frame(mat_3rd*MORt(mat_3rd, steps = 1))
KnowledgeComp_PerCountry_3rd_Step1$Step <- "Step1"

KnowledgeComp_PerCountry_3rd_Step2 <- as.data.frame(mat_3rd*MORt(mat_3rd, steps = 2))
KnowledgeComp_PerCountry_3rd_Step2$Step <- "Step2"

#Considering RCAs:
KnowledgeComp_PerCountry_3rd_RCA <- as.data.frame(mat_3rd_RCAs*MORt(mat_3rd_RCAs))
KnowledgeComp_PerCountry_3rd_RCA$Step <- "NoStep"

KnowledgeComp_PerCountry_3rd_Step0_RCA <- as.data.frame(mat_3rd_RCAs*MORt(mat_3rd_RCAs, steps = 0))
KnowledgeComp_PerCountry_3rd_Step0_RCA$Step <- "Step0"

KnowledgeComp_PerCountry_3rd_Step1_RCA <- as.data.frame(mat_3rd_RCAs*MORt(mat_3rd_RCAs, steps = 1))
KnowledgeComp_PerCountry_3rd_Step1_RCA$Step <- "Step1"

KnowledgeComp_PerCountry_3rd_Step2_RCA <- as.data.frame(mat_3rd_RCAs*MORt(mat_3rd_RCAs, steps = 2))
KnowledgeComp_PerCountry_3rd_Step2_RCA$Step <- "Step2"

KnowledgeComp_PerCountry_3rd_All <- rbind(KnowledgeComp_PerCountry_3rd, KnowledgeComp_PerCountry_3rd_Step0,
                                          KnowledgeComp_PerCountry_3rd_Step1, KnowledgeComp_PerCountry_3rd_Step2)

KnowledgeComp_PerCountry_3rd_All_RCAs <- rbind(KnowledgeComp_PerCountry_3rd_RCA, KnowledgeComp_PerCountry_3rd_Step0_RCA,
                                               KnowledgeComp_PerCountry_3rd_Step1_RCA, KnowledgeComp_PerCountry_3rd_Step2_RCA)

write.csv2(KnowledgeComp_PerCountry_3rd_All, file = "Data_calculations_IPC/KnowledgeComp_PerCountry_3rd_All_AI.csv", row.names = TRUE)
write.csv2(KnowledgeComp_PerCountry_3rd_All_RCAs, file = "Data_calculations_IPC/KnowledgeComp_PerCountry_3rd_All_RCAs_AI.csv", row.names = TRUE)


#3.2.3. Calculate the relatedness -----
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

mat_tech_3rd_US_rel_jacc <- relatedness(mat_tech_3rd_US, method = "Jaccard")
mat_tech_3rd_US_rel_asso <- relatedness(mat_tech_3rd_US, method = "association")
mat_tech_3rd_US_rel_cosi <- relatedness(mat_tech_3rd_US, method = "cosine")

Relatedness <- as.data.frame(mean(mat_tech_3rd_US_rel_jacc))
Relatedness$mat_tech_3rd_US_rel_asso <- mean(mat_tech_3rd_US_rel_asso)
Relatedness$mat_tech_3rd_US_rel_cosi<- mean(mat_tech_3rd_US_rel_cosi)
rownames(Relatedness) <- c("US")
names(Relatedness) <- c("Jaccard", "Association", "Cosine")
Relatedness$Period <- "3rd"

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

mat_tech_3rd_US_Top4_rel_jacc <- relatedness(mat_tech_3rd_US_Top4, method = "Jaccard")
mat_tech_3rd_US_Top4_rel_asso <- relatedness(mat_tech_3rd_US_Top4, method = "association")
mat_tech_3rd_US_Top4_rel_cosi <- relatedness(mat_tech_3rd_US_Top4, method = "cosine")

Relatedness$Jaccard_top4 <- mean(mat_tech_3rd_US_Top4_rel_jacc)
Relatedness$Association_top4 <- mean(mat_tech_3rd_US_Top4_rel_asso)
Relatedness$Cosine_top4<- mean(mat_tech_3rd_US_Top4_rel_cosi)

#then select only the top 3 areas:
IPC_all_patents_3rd_US_Top3 <- IPC_all_patents_3rd_US[IPC_all_patents_3rd_US$techn_field_nr == "4" | 
                                                         IPC_all_patents_3rd_US$techn_field_nr == "5"| 
                                                         IPC_all_patents_3rd_US$techn_field_nr == "11", ]

mat_tech_3rd_US_Top3 <- create_sparse_matrix(i = IPC_all_patents_3rd_US_Top3 %>% pull(appln_id),
                                              j = IPC_all_patents_3rd_US_Top3 %>% pull(techn_field_nr))

mat_tech_3rd_US_Top3 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_3rd_US_Top3_rel_jacc <- relatedness(mat_tech_3rd_US_Top3, method = "Jaccard")
mat_tech_3rd_US_Top3_rel_asso <- relatedness(mat_tech_3rd_US_Top3, method = "association")
mat_tech_3rd_US_Top3_rel_cosi <- relatedness(mat_tech_3rd_US_Top3, method = "cosine")

Relatedness$Jaccard_Top3 <- mean(mat_tech_3rd_US_Top3_rel_jacc)
Relatedness$Association_Top3 <- mean(mat_tech_3rd_US_Top3_rel_asso)
Relatedness$Cosine_Top3<- mean(mat_tech_3rd_US_Top3_rel_cosi)

#and finally the top 7 non-AI areas:
IPC_all_patents_3rd_US_Top7 <- IPC_all_patents_3rd_US[IPC_all_patents_3rd_US$techn_field_nr == "1"| 
                                                        IPC_all_patents_3rd_US$techn_field_nr == "2"| 
                                                        IPC_all_patents_3rd_US$techn_field_nr == "3"|
                                                        #IPC_all_patents_3rd_US$techn_field_nr == "4"| 
                                                        IPC_all_patents_3rd_US$techn_field_nr == "13"| 
                                                        IPC_all_patents_3rd_US$techn_field_nr == "25"| 
                                                        IPC_all_patents_3rd_US$techn_field_nr == "34", ]

mat_tech_3rd_US_Top7 <- create_sparse_matrix(i = IPC_all_patents_3rd_US_Top7 %>% pull(appln_id),
                                             j = IPC_all_patents_3rd_US_Top7 %>% pull(techn_field_nr))

mat_tech_3rd_US_Top7 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_3rd_US_Top7_rel_jacc <- relatedness(mat_tech_3rd_US_Top7, method = "Jaccard")
mat_tech_3rd_US_Top7_rel_asso <- relatedness(mat_tech_3rd_US_Top7, method = "association")
mat_tech_3rd_US_Top7_rel_cosi <- relatedness(mat_tech_3rd_US_Top7, method = "cosine")

Relatedness$Jaccard_Top7 <- mean(mat_tech_3rd_US_Top7_rel_jacc)
Relatedness$Association_Top7 <- mean(mat_tech_3rd_US_Top7_rel_asso)
Relatedness$Cosine_Top7<- mean(mat_tech_3rd_US_Top7_rel_cosi)

#China:
mat_tech_3rd_CN <- create_sparse_matrix(i = IPC_all_patents_3rd_CN %>% pull(appln_id),
                                        j = IPC_all_patents_3rd_CN %>% pull(techn_field_nr))

mat_tech_3rd_CN %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_3rd_CN_rel_jacc <- relatedness(mat_tech_3rd_CN, method = "Jaccard")
mat_tech_3rd_CN_rel_asso <- relatedness(mat_tech_3rd_CN, method = "association")
mat_tech_3rd_CN_rel_cosi <- relatedness(mat_tech_3rd_CN, method = "cosine")

Relatedness_CN <- as.data.frame(mean(mat_tech_3rd_CN_rel_jacc))
Relatedness_CN$mat_tech_3rd_CN_rel_asso <- mean(mat_tech_3rd_CN_rel_asso)
Relatedness_CN$mat_tech_3rd_CN_rel_cosi<- mean(mat_tech_3rd_CN_rel_cosi)
rownames(Relatedness_CN) <- c("CN")
names(Relatedness_CN) <- c("Jaccard", "Association", "Cosine")
Relatedness_CN$Period <- "3rd"

#then select only the top 4 areas. But first, the global environment is already too full for the calculations.
#Let's clean it:
rm(IPC_all_patents_3rd, IPC_all_patents_3rd_US, IPC_all_patents_3rd_In)
IPC_all_patents_3rd_CN_Top4 <- IPC_all_patents_3rd_CN[IPC_all_patents_3rd_CN$techn_field_nr == "6" | 
                                                        IPC_all_patents_3rd_CN$techn_field_nr == "7"| 
                                                        IPC_all_patents_3rd_CN$techn_field_nr == "10"| 
                                                        IPC_all_patents_3rd_CN$techn_field_nr == "12", ]

mat_tech_3rd_CN_Top4 <- create_sparse_matrix(i = IPC_all_patents_3rd_CN_Top4 %>% pull(appln_id),
                                             j = IPC_all_patents_3rd_CN_Top4 %>% pull(techn_field_nr))

mat_tech_3rd_CN_Top4 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_3rd_CN_Top4_rel_jacc <- relatedness(mat_tech_3rd_CN_Top4, method = "Jaccard")
mat_tech_3rd_CN_Top4_rel_asso <- relatedness(mat_tech_3rd_CN_Top4, method = "association")
mat_tech_3rd_CN_Top4_rel_cosi <- relatedness(mat_tech_3rd_CN_Top4, method = "cosine")

Relatedness_CN$Jaccard_top4 <- mean(mat_tech_3rd_CN_Top4_rel_jacc)
Relatedness_CN$Association_top4 <- mean(mat_tech_3rd_CN_Top4_rel_asso)
Relatedness_CN$Cosine_top4<- mean(mat_tech_3rd_CN_Top4_rel_cosi)

#then select only the top 3 areas:
IPC_all_patents_3rd_CN_Top3 <- IPC_all_patents_3rd_CN[IPC_all_patents_3rd_CN$techn_field_nr == "4" | 
                                                         IPC_all_patents_3rd_CN$techn_field_nr == "5"| 
                                                         IPC_all_patents_3rd_CN$techn_field_nr == "11", ]

mat_tech_3rd_CN_Top3 <- create_sparse_matrix(i = IPC_all_patents_3rd_CN_Top3 %>% pull(appln_id),
                                              j = IPC_all_patents_3rd_CN_Top3 %>% pull(techn_field_nr))

mat_tech_3rd_CN_Top3 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_3rd_CN_Top3_rel_jacc <- relatedness(mat_tech_3rd_CN_Top3, method = "Jaccard")
mat_tech_3rd_CN_Top3_rel_asso <- relatedness(mat_tech_3rd_CN_Top3, method = "association")
mat_tech_3rd_CN_Top3_rel_cosi <- relatedness(mat_tech_3rd_CN_Top3, method = "cosine")

Relatedness_CN$Jaccard_Top3 <- mean(mat_tech_3rd_CN_Top3_rel_jacc)
Relatedness_CN$Association_Top3 <- mean(mat_tech_3rd_CN_Top3_rel_asso)
Relatedness_CN$Cosine_Top3<- mean(mat_tech_3rd_CN_Top3_rel_cosi)

#and finally the top 7 non-AI areas:
rm(IPC_all_patents_3rd_CN_Top4, IPC_all_patents_3rd_CN_Top3, IPC_all_patents_3rd_US_Top3, 
   IPC_all_patents_3rd_US_Top7, IPC_all_patents_3rd_US_Top4)

IPC_all_patents_3rd_CN_Top7 <- IPC_all_patents_3rd_CN[IPC_all_patents_3rd_CN$techn_field_nr == "1"| 
                                                        IPC_all_patents_3rd_CN$techn_field_nr == "2"| 
                                                        IPC_all_patents_3rd_CN$techn_field_nr == "3"|
                                                        #IPC_all_patents_3rd_CN$techn_field_nr == "4"| 
                                                        IPC_all_patents_3rd_CN$techn_field_nr == "13"| 
                                                        IPC_all_patents_3rd_CN$techn_field_nr == "25"| 
                                                        IPC_all_patents_3rd_CN$techn_field_nr == "34", ]

mat_tech_3rd_CN_Top7 <- create_sparse_matrix(i = IPC_all_patents_3rd_CN_Top7 %>% pull(appln_id),
                                             j = IPC_all_patents_3rd_CN_Top7 %>% pull(techn_field_nr))

mat_tech_3rd_CN_Top7 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_3rd_CN_Top7_rel_jacc <- relatedness(mat_tech_3rd_CN_Top7, method = "Jaccard")
mat_tech_3rd_CN_Top7_rel_asso <- relatedness(mat_tech_3rd_CN_Top7, method = "association")
mat_tech_3rd_CN_Top7_rel_cosi <- relatedness(mat_tech_3rd_CN_Top7, method = "cosine")

Relatedness_CN$Jaccard_Top7 <- mean(mat_tech_3rd_CN_Top7_rel_jacc)
Relatedness_CN$Association_Top7 <- mean(mat_tech_3rd_CN_Top7_rel_asso)
Relatedness_CN$Cosine_Top7<- mean(mat_tech_3rd_CN_Top7_rel_cosi)

#KR
rm(IPC_all_patents_3rd_CN)
mat_tech_3rd_KR <- create_sparse_matrix(i = IPC_all_patents_3rd_KR %>% pull(appln_id),
                                        j = IPC_all_patents_3rd_KR %>% pull(techn_field_nr))

mat_tech_3rd_KR %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_3rd_KR_rel_jacc <- relatedness(mat_tech_3rd_KR, method = "Jaccard")
mat_tech_3rd_KR_rel_asso <- relatedness(mat_tech_3rd_KR, method = "association")
mat_tech_3rd_KR_rel_cosi <- relatedness(mat_tech_3rd_KR, method = "cosine")

Relatedness_KR <- as.data.frame(mean(mat_tech_3rd_KR_rel_jacc))
Relatedness_KR$mat_tech_3rd_KR_rel_asso <- mean(mat_tech_3rd_KR_rel_asso)
Relatedness_KR$mat_tech_3rd_KR_rel_cosi<- mean(mat_tech_3rd_KR_rel_cosi)
rownames(Relatedness_KR) <- c("KR")
names(Relatedness_KR) <- c("Jaccard", "Association", "Cosine")
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

mat_tech_3rd_KR_Top4_rel_jacc <- relatedness(mat_tech_3rd_KR_Top4, method = "Jaccard")
mat_tech_3rd_KR_Top4_rel_asso <- relatedness(mat_tech_3rd_KR_Top4, method = "association")
mat_tech_3rd_KR_Top4_rel_cosi <- relatedness(mat_tech_3rd_KR_Top4, method = "cosine")

Relatedness_KR$Jaccard_top4 <- mean(mat_tech_3rd_KR_Top4_rel_jacc)
Relatedness_KR$Association_top4 <- mean(mat_tech_3rd_KR_Top4_rel_asso)
Relatedness_KR$Cosine_top4<- mean(mat_tech_3rd_KR_Top4_rel_cosi)

#then select only the top 3 areas:
IPC_all_patents_3rd_KR_Top3 <- IPC_all_patents_3rd_KR[IPC_all_patents_3rd_KR$techn_field_nr == "4" | 
                                                         IPC_all_patents_3rd_KR$techn_field_nr == "5"| 
                                                         IPC_all_patents_3rd_KR$techn_field_nr == "11", ]

mat_tech_3rd_KR_Top3 <- create_sparse_matrix(i = IPC_all_patents_3rd_KR_Top3 %>% pull(appln_id),
                                              j = IPC_all_patents_3rd_KR_Top3 %>% pull(techn_field_nr))

mat_tech_3rd_KR_Top3 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_3rd_KR_Top3_rel_jacc <- relatedness(mat_tech_3rd_KR_Top3, method = "Jaccard")
mat_tech_3rd_KR_Top3_rel_asso <- relatedness(mat_tech_3rd_KR_Top3, method = "association")
mat_tech_3rd_KR_Top3_rel_cosi <- relatedness(mat_tech_3rd_KR_Top3, method = "cosine")

Relatedness_KR$Jaccard_Top3 <- mean(mat_tech_3rd_KR_Top3_rel_jacc)
Relatedness_KR$Association_Top3 <- mean(mat_tech_3rd_KR_Top3_rel_asso)
Relatedness_KR$Cosine_Top3<- mean(mat_tech_3rd_KR_Top3_rel_cosi)

#and finally the top 7 non-AI areas:
IPC_all_patents_3rd_KR_Top7 <- IPC_all_patents_3rd_KR[IPC_all_patents_3rd_KR$techn_field_nr == "1"| 
                                                        IPC_all_patents_3rd_KR$techn_field_nr == "2"| 
                                                        IPC_all_patents_3rd_KR$techn_field_nr == "3"|
                                                        #IPC_all_patents_3rd_KR$techn_field_nr == "4"| 
                                                        IPC_all_patents_3rd_KR$techn_field_nr == "13"| 
                                                        IPC_all_patents_3rd_KR$techn_field_nr == "25"| 
                                                        IPC_all_patents_3rd_KR$techn_field_nr == "34", ]

mat_tech_3rd_KR_Top7 <- create_sparse_matrix(i = IPC_all_patents_3rd_KR_Top7 %>% pull(appln_id),
                                             j = IPC_all_patents_3rd_KR_Top7 %>% pull(techn_field_nr))

mat_tech_3rd_KR_Top7 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_3rd_KR_Top7_rel_jacc <- relatedness(mat_tech_3rd_KR_Top7, method = "Jaccard")
mat_tech_3rd_KR_Top7_rel_asso <- relatedness(mat_tech_3rd_KR_Top7, method = "association")
mat_tech_3rd_KR_Top7_rel_cosi <- relatedness(mat_tech_3rd_KR_Top7, method = "cosine")

Relatedness_KR$Jaccard_Top7 <- mean(mat_tech_3rd_KR_Top7_rel_jacc)
Relatedness_KR$Association_Top7 <- mean(mat_tech_3rd_KR_Top7_rel_asso)
Relatedness_KR$Cosine_Top7<- mean(mat_tech_3rd_KR_Top7_rel_cosi)

#Japan
mat_tech_3rd_JP <- create_sparse_matrix(i = IPC_all_patents_3rd_JP %>% pull(appln_id),
                                        j = IPC_all_patents_3rd_JP %>% pull(techn_field_nr))

mat_tech_3rd_JP %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_3rd_JP_rel_jacc <- relatedness(mat_tech_3rd_JP, method = "Jaccard")
mat_tech_3rd_JP_rel_asso <- relatedness(mat_tech_3rd_JP, method = "association")
mat_tech_3rd_JP_rel_cosi <- relatedness(mat_tech_3rd_JP, method = "cosine")

Relatedness_JP <- as.data.frame(mean(mat_tech_3rd_JP_rel_jacc))
Relatedness_JP$mat_tech_3rd_JP_rel_asso <- mean(mat_tech_3rd_JP_rel_asso)
Relatedness_JP$mat_tech_3rd_JP_rel_cosi<- mean(mat_tech_3rd_JP_rel_cosi)
rownames(Relatedness_JP) <- c("JP")
names(Relatedness_JP) <- c("Jaccard", "Association", "Cosine")
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

mat_tech_3rd_JP_Top4_rel_jacc <- relatedness(mat_tech_3rd_JP_Top4, method = "Jaccard")
mat_tech_3rd_JP_Top4_rel_asso <- relatedness(mat_tech_3rd_JP_Top4, method = "association")
mat_tech_3rd_JP_Top4_rel_cosi <- relatedness(mat_tech_3rd_JP_Top4, method = "cosine")

Relatedness_JP$Jaccard_top4 <- mean(mat_tech_3rd_JP_Top4_rel_jacc)
Relatedness_JP$Association_top4 <- mean(mat_tech_3rd_JP_Top4_rel_asso)
Relatedness_JP$Cosine_top4<- mean(mat_tech_3rd_JP_Top4_rel_cosi)

#then select only the top 3 areas:
IPC_all_patents_3rd_JP_Top3 <- IPC_all_patents_3rd_JP[IPC_all_patents_3rd_JP$techn_field_nr == "4" | 
                                                         IPC_all_patents_3rd_JP$techn_field_nr == "5"| 
                                                         IPC_all_patents_3rd_JP$techn_field_nr == "11", ]

mat_tech_3rd_JP_Top3 <- create_sparse_matrix(i = IPC_all_patents_3rd_JP_Top3 %>% pull(appln_id),
                                              j = IPC_all_patents_3rd_JP_Top3 %>% pull(techn_field_nr))

mat_tech_3rd_JP_Top3 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_3rd_JP_Top3_rel_jacc <- relatedness(mat_tech_3rd_JP_Top3, method = "Jaccard")
mat_tech_3rd_JP_Top3_rel_asso <- relatedness(mat_tech_3rd_JP_Top3, method = "association")
mat_tech_3rd_JP_Top3_rel_cosi <- relatedness(mat_tech_3rd_JP_Top3, method = "cosine")

Relatedness_JP$Jaccard_Top3 <- mean(mat_tech_3rd_JP_Top3_rel_jacc)
Relatedness_JP$Association_Top3 <- mean(mat_tech_3rd_JP_Top3_rel_asso)
Relatedness_JP$Cosine_Top3<- mean(mat_tech_3rd_JP_Top3_rel_cosi)

#and finally the top 7 non-AI areas:
IPC_all_patents_3rd_JP_Top7 <- IPC_all_patents_3rd_JP[IPC_all_patents_3rd_JP$techn_field_nr == "1"| 
                                                        IPC_all_patents_3rd_JP$techn_field_nr == "2"| 
                                                        IPC_all_patents_3rd_JP$techn_field_nr == "3"|
                                                        #IPC_all_patents_3rd_JP$techn_field_nr == "4"| 
                                                        IPC_all_patents_3rd_JP$techn_field_nr == "13"| 
                                                        IPC_all_patents_3rd_JP$techn_field_nr == "25"| 
                                                        IPC_all_patents_3rd_JP$techn_field_nr == "34", ]

mat_tech_3rd_JP_Top7 <- create_sparse_matrix(i = IPC_all_patents_3rd_JP_Top7 %>% pull(appln_id),
                                             j = IPC_all_patents_3rd_JP_Top7 %>% pull(techn_field_nr))

mat_tech_3rd_JP_Top7 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_3rd_JP_Top7_rel_jacc <- relatedness(mat_tech_3rd_JP_Top7, method = "Jaccard")
mat_tech_3rd_JP_Top7_rel_asso <- relatedness(mat_tech_3rd_JP_Top7, method = "association")
mat_tech_3rd_JP_Top7_rel_cosi <- relatedness(mat_tech_3rd_JP_Top7, method = "cosine")

Relatedness_JP$Jaccard_Top7 <- mean(mat_tech_3rd_JP_Top7_rel_jacc)
Relatedness_JP$Association_Top7 <- mean(mat_tech_3rd_JP_Top7_rel_asso)
Relatedness_JP$Cosine_Top7<- mean(mat_tech_3rd_JP_Top7_rel_cosi)

#AI
mat_tech_3rd_AI <- create_sparse_matrix(i = IPC_all_patents_3rd_AI %>% pull(appln_id),
                                        j = IPC_all_patents_3rd_AI %>% pull(techn_field_nr))

mat_tech_3rd_AI %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_3rd_AI_rel_jacc <- relatedness(mat_tech_3rd_AI, method = "Jaccard")
mat_tech_3rd_AI_rel_asso <- relatedness(mat_tech_3rd_AI, method = "association")
mat_tech_3rd_AI_rel_cosi <- relatedness(mat_tech_3rd_AI, method = "cosine")

Relatedness_AI <- as.data.frame(mean(mat_tech_3rd_AI_rel_jacc))
Relatedness_AI$mat_tech_3rd_AI_rel_asso <- mean(mat_tech_3rd_AI_rel_asso)
Relatedness_AI$mat_tech_3rd_AI_rel_cosi<- mean(mat_tech_3rd_AI_rel_cosi)
rownames(Relatedness_AI) <- c("AI")
names(Relatedness_AI) <- c("Jaccard", "Association", "Cosine")
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

mat_tech_3rd_AI_Top4_rel_jacc <- relatedness(mat_tech_3rd_AI_Top4, method = "Jaccard")
mat_tech_3rd_AI_Top4_rel_asso <- relatedness(mat_tech_3rd_AI_Top4, method = "association")
mat_tech_3rd_AI_Top4_rel_cosi <- relatedness(mat_tech_3rd_AI_Top4, method = "cosine")

Relatedness_AI$Jaccard_top4 <- mean(mat_tech_3rd_AI_Top4_rel_jacc)
Relatedness_AI$Association_top4 <- mean(mat_tech_3rd_AI_Top4_rel_asso)
Relatedness_AI$Cosine_top4<- mean(mat_tech_3rd_AI_Top4_rel_cosi)

#then select only the top 3 areas:
IPC_all_patents_3rd_AI_Top3 <- IPC_all_patents_3rd_AI[IPC_all_patents_3rd_AI$techn_field_nr == "4" | 
                                                         IPC_all_patents_3rd_AI$techn_field_nr == "5"| 
                                                         IPC_all_patents_3rd_AI$techn_field_nr == "11", ]

mat_tech_3rd_AI_Top3 <- create_sparse_matrix(i = IPC_all_patents_3rd_AI_Top3 %>% pull(appln_id),
                                              j = IPC_all_patents_3rd_AI_Top3 %>% pull(techn_field_nr))

mat_tech_3rd_AI_Top3 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_3rd_AI_Top3_rel_jacc <- relatedness(mat_tech_3rd_AI_Top3, method = "Jaccard")
mat_tech_3rd_AI_Top3_rel_asso <- relatedness(mat_tech_3rd_AI_Top3, method = "association")
mat_tech_3rd_AI_Top3_rel_cosi <- relatedness(mat_tech_3rd_AI_Top3, method = "cosine")

Relatedness_AI$Jaccard_Top3 <- mean(mat_tech_3rd_AI_Top3_rel_jacc)
Relatedness_AI$Association_Top3 <- mean(mat_tech_3rd_AI_Top3_rel_asso)
Relatedness_AI$Cosine_Top3<- mean(mat_tech_3rd_AI_Top3_rel_cosi)

#and finally the top 7 non-AI areas:
IPC_all_patents_3rd_AI_Top7 <- IPC_all_patents_3rd_AI[IPC_all_patents_3rd_AI$techn_field_nr == "1"| 
                                                        IPC_all_patents_3rd_AI$techn_field_nr == "2"| 
                                                        IPC_all_patents_3rd_AI$techn_field_nr == "3"|
                                                        #IPC_all_patents_3rd_AI$techn_field_nr == "4"| 
                                                        IPC_all_patents_3rd_AI$techn_field_nr == "13"| 
                                                        IPC_all_patents_3rd_AI$techn_field_nr == "25"| 
                                                        IPC_all_patents_3rd_AI$techn_field_nr == "34", ]

mat_tech_3rd_AI_Top7 <- create_sparse_matrix(i = IPC_all_patents_3rd_AI_Top7 %>% pull(appln_id),
                                             j = IPC_all_patents_3rd_AI_Top7 %>% pull(techn_field_nr))

mat_tech_3rd_AI_Top7 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_3rd_AI_Top7_rel_jacc <- relatedness(mat_tech_3rd_AI_Top7, method = "Jaccard")
mat_tech_3rd_AI_Top7_rel_asso <- relatedness(mat_tech_3rd_AI_Top7, method = "association")
mat_tech_3rd_AI_Top7_rel_cosi <- relatedness(mat_tech_3rd_AI_Top7, method = "cosine")

Relatedness_AI$Jaccard_Top7 <- mean(mat_tech_3rd_AI_Top7_rel_jacc)
Relatedness_AI$Association_Top7 <- mean(mat_tech_3rd_AI_Top7_rel_asso)
Relatedness_AI$Cosine_Top7<- mean(mat_tech_3rd_AI_Top7_rel_cosi)

#and we merge it all together:
Relatedness_ThirdPeriod <- rbind(Relatedness, Relatedness_CN, Relatedness_KR, Relatedness_JP, Relatedness_AI)
Relatedness_ThirdPeriod <- Relatedness_ThirdPeriod[,c((1:3), (5:13), (4))]

write.csv2(Relatedness_ThirdPeriod, file = "Data_calculations_IPC/Relatedness_3rd_period_IPC.csv", row.names = TRUE)

#4. Visualization RCAs -----
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

#4.1.First period ----
reg_tech1_countries <- read.csv("Data_IPC/reg_tech_FirstPeriod.csv", sep = ";", header = TRUE, dec=",")
#4.1.1. First Period Countries ----
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

###4.1.2. First Period AI----
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

IPC_names <- read.csv("Data_IPC/ipc_technology.csv", sep = ";", header = TRUE)%>%
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
KnwComp_Country_1st_RCA <- read.csv("Data_calculations_IPC/KnowledgeComp_PerCountry_1st_All_RCAs.csv", sep = ";", header = T, dec=",")
KnwComp_Country_1st_RCA <- as.data.frame(t(KnwComp_Country_1st_RCA))
KnwComp_Country_1st_RCA <- KnwComp_Country_1st_RCA %>%
    row_to_names(row_number = 1)
KnwComp_Country_1st_RCA<- KnwComp_Country_1st_RCA[(-36),]
KnwComp_Country_1st_RCA$techn_field_nr <- First_period$techn_field_nr
First_period$US_Com <- KnwComp_Country_1st_RCA$US3
First_period$CN_Com <- KnwComp_Country_1st_RCA$CN3
First_period$KR_Com <- KnwComp_Country_1st_RCA$KR3
First_period$JP_Com <- KnwComp_Country_1st_RCA$JP3
i <- c(9:12)
First_period[ , i] <- apply(First_period[ , i], 2,            # Specify own function within apply
                    function(x) as.integer(as.character(x)))

#remove files we don't need:
rm(AI_first_period, CN_first_period, JP_first_period, KR_first_period, KnwComp_Country_1st_RCA,
   mat_reg_tech1_AI, mat_reg_tech1_countries, reg_RCA1_AI, reg_RCA1_countries, reg_tech1_AI, reg_tech1_countries,
   US_first_period, i)
#Now we plot:
FigUS_1st <- ggplot(First_period, aes(x=log10(RCA_US), y=log10(RCA_AI), label = field_name)) + 
  geom_point(aes(colour = sector, size = US_Com),show.legend = F, stroke = 2) +  
  geom_text(aes(label=ifelse(RCA_US>1 & RCA_AI>1,as.character(field_name),''), size = 2), show.legend = F) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2)+
  geom_vline(xintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2) +
  #scale_color_brewer(palette="Paired") + theme_classic() + 
  scale_color_brewer(palette="Dark2") + theme_minimal() +
  #scale_color_brewer(palette="Accent") + theme_minimal() +
  geom_rect(aes(NULL, NULL, xmin = Inf, xmax = 0), ymin = Inf, ymax = 0,alpha=0.005,fill="royalblue2") +
  scale_size_continuous(range = c(1, 10)) +
  ggtitle("United States") +
  xlab(NULL) +
  ylab(NULL) +
  ylim(-0.5, 2)+
  xlim(-0.1, 0.3)

FigUS_1stb <- ggplot(First_period, aes(x=log10(RCA_US), y=log10(RCA_AI), label = field_name)) + 
  geom_point(aes(colour = sector, size = US_Com),show.legend = F, stroke = 2) +  
  geom_text(aes(label=ifelse(RCA_US>1 & RCA_AI>1,as.character(field_name),''), size = 2), show.legend = F) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2)+
  geom_vline(xintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2) +
  #scale_color_brewer(palette="Paired") + theme_classic() + 
  scale_color_brewer(palette="Dark2") + theme_minimal() +
  #scale_color_brewer(palette="Accent") + theme_minimal() +
  geom_rect(aes(NULL, NULL, xmin = Inf, xmax = 0), ymin = Inf, ymax = 0,alpha=0.005,fill="royalblue2") +
  scale_size_continuous(range = c(1, 10)) +
  ggtitle("United States") +
  xlab(NULL) +
  ylab(NULL) +
  ylim(-2, 2)+
  xlim(-0.6, 0.6)

FigUS_1stc <- ggplot(First_period, aes(x=log10(RCA_US), y=log10(RCA_AI), label = '')) + 
  geom_point(aes(colour = sector, size = US_Com),show.legend = F, stroke = 2) +  
  geom_text() +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2)+
  geom_vline(xintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2) +
  scale_color_brewer(palette="Dark2") + theme_minimal() +
  geom_label_repel(data = First_period, aes(label = ifelse(RCA_US>1 & RCA_AI>1,as.character(field_name),''))) +
  geom_rect(aes(NULL, NULL, xmin = Inf, xmax = 0), ymin = Inf, ymax = 0,alpha=0.005,fill="royalblue2") +
  scale_size_continuous(range = c(1, 10)) +
  ggtitle("United States") +
  xlab(NULL) +
  ylab(NULL) +
  ylim(-2, 2)+
  xlim(-0.6, 0.6)

FigCN_1st <-ggplot(First_period, aes(x=log10(RCA_CN), y=log10(RCA_AI), label = field_name)) + 
  geom_point(aes(colour = sector, size = CN_Com),show.legend = F, stroke = 2) +  
  geom_text(aes(label=ifelse(RCA_CN>1 & RCA_AI>1,as.character(field_name),''), size = 2), show.legend = F) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2)+
  geom_vline(xintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2) +
  scale_color_brewer(palette="Dark2") + theme_minimal() +
  geom_rect(aes(NULL, NULL, xmin = Inf, xmax = 0), ymin = Inf, ymax = 0,alpha=0.005,fill="royalblue2") +
  scale_size_continuous(range = c(1, 10)) +
  ggtitle("China") +
  xlab(NULL) +
  ylab(NULL)+
  ylim(-0.5, 2)+
  xlim(-0.1, 0.3)

FigCN_1stb <-ggplot(First_period, aes(x=log10(RCA_CN), y=log10(RCA_AI), label = field_name)) + 
  geom_point(aes(colour = sector, size = CN_Com),show.legend = F, stroke = 2) +  
  geom_text(aes(label=ifelse(RCA_CN>1 & RCA_AI>1,as.character(field_name),''), size = 2), show.legend = F) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2)+
  geom_vline(xintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2) +
  scale_color_brewer(palette="Dark2") + theme_minimal() +
  geom_rect(aes(NULL, NULL, xmin = Inf, xmax = 0), ymin = Inf, ymax = 0,alpha=0.005,fill="royalblue2") +
  scale_size_continuous(range = c(1, 10)) +
  ggtitle("China") +
  xlab(NULL) +
  ylab(NULL)+
  ylim(-2, 2)+
  xlim(-0.6, 0.6)

FigCN_1stc <-ggplot(First_period, aes(x=log10(RCA_CN), y=log10(RCA_AI), label = '')) + 
  geom_point(aes(colour = sector, size = CN_Com),show.legend = F, stroke = 2) +  
  geom_text() +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2)+
  geom_vline(xintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2) +
  scale_color_brewer(palette="Dark2") + theme_minimal() +
  geom_label_repel(data = First_period, aes(label = ifelse(RCA_CN>1 & RCA_AI>1,as.character(field_name),''))) +
  geom_rect(aes(NULL, NULL, xmin = Inf, xmax = 0), ymin = Inf, ymax = 0,alpha=0.005,fill="royalblue2") +
  scale_size_continuous(range = c(1, 10)) +
  ggtitle("China") +
  xlab(NULL) +
  ylab(NULL)+
  ylim(-2, 2)+
  xlim(-0.6, 0.6)

FigKR_1st <-ggplot(First_period, aes(x=log10(RCA_KR), y=log10(RCA_AI), label = field_name)) + 
  geom_point(aes(colour = sector, size = KR_Com),show.legend = F, stroke = 2) +  
  geom_text(aes(label=ifelse(RCA_KR>1 & RCA_AI>1,as.character(field_name),''), size = 2), show.legend = F) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2)+
  geom_vline(xintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2) +
  scale_color_brewer(palette="Dark2") + theme_minimal() +
  geom_rect(aes(NULL, NULL, xmin = Inf, xmax = 0), ymin = Inf, ymax = 0,alpha=0.005,fill="royalblue2") +
  scale_size_continuous(range = c(1, 10)) +
  ggtitle("South Korea") +
  xlab(NULL) +
  ylab(NULL)+
  ylim(-0.5, 2)+
  xlim(-0.1, 0.3)

FigKR_1stb <-ggplot(First_period, aes(x=log10(RCA_KR), y=log10(RCA_AI), label = field_name)) + 
  geom_point(aes(colour = sector, size = KR_Com),show.legend = F, stroke = 2) +  
  geom_text(aes(label=ifelse(RCA_KR>1 & RCA_AI>1,as.character(field_name),''), size = 2), show.legend = F) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2)+
  geom_vline(xintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2) +
  scale_color_brewer(palette="Dark2") + theme_minimal() +
  geom_rect(aes(NULL, NULL, xmin = Inf, xmax = 0), ymin = Inf, ymax = 0,alpha=0.005,fill="royalblue2") +
  scale_size_continuous(range = c(1, 10)) +
  ggtitle("South Korea") +
  xlab(NULL) +
  ylab(NULL)+
  ylim(-2, 2)+
  xlim(-0.6, 0.6)

FigKR_1stc <-ggplot(First_period, aes(x=log10(RCA_KR), y=log10(RCA_AI), label = '')) + 
  geom_point(aes(colour = sector, size = KR_Com),show.legend = F, stroke = 2) +  
  geom_text() +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2)+
  geom_vline(xintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2) +
  scale_color_brewer(palette="Dark2") + theme_minimal() +
  geom_label_repel(data = First_period, aes(label = ifelse(RCA_KR>1 & RCA_AI>1,as.character(field_name),''))) +
  geom_rect(aes(NULL, NULL, xmin = Inf, xmax = 0), ymin = Inf, ymax = 0,alpha=0.005,fill="royalblue2") +
  scale_size_continuous(range = c(1, 10)) +
  ggtitle("South Korea") +
  xlab(NULL) +
  ylab(NULL)+
  ylim(-2, 2)+
  xlim(-0.6, 0.6)

FigJP_1st <-ggplot(First_period, aes(x=log10(RCA_JP), y=log10(RCA_AI), label = field_name)) + 
  geom_point(aes(colour = sector, size = JP_Com),show.legend = F, stroke = 2) +  
  geom_text(aes(label=ifelse(RCA_JP>1 & RCA_AI>1,as.character(field_name),''), size = 2), show.legend = F) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2)+
  geom_vline(xintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2) +
  scale_color_brewer(palette="Dark2") + theme_minimal() +
  geom_rect(aes(NULL, NULL, xmin = Inf, xmax = 0), ymin = Inf, ymax = 0,alpha=0.005,fill="royalblue2") +
  scale_size_continuous(range = c(1, 10)) +
  ggtitle("Japan") +
  xlab(NULL) +
  ylab(NULL)+
  ylim(-0.5, 2)+
  xlim(-0.1, 0.3)

FigJP_1stb <-ggplot(First_period, aes(x=log10(RCA_JP), y=log10(RCA_AI), label = field_name)) + 
  geom_point(aes(colour = sector, size = JP_Com),show.legend = F, stroke = 2) +  
  geom_text(aes(label=ifelse(RCA_JP>1 & RCA_AI>1,as.character(field_name),''), size = 2), show.legend = F) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2)+
  geom_vline(xintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2) +
  scale_color_brewer(palette="Dark2") + theme_minimal() +
  geom_rect(aes(NULL, NULL, xmin = Inf, xmax = 0), ymin = Inf, ymax = 0,alpha=0.005,fill="royalblue2") +
  scale_size_continuous(range = c(1, 10)) +
  ggtitle("Japan") +
  xlab(NULL) +
  ylab(NULL)+
  ylim(-2, 2)+
  xlim(-0.6, 0.6)

FigJP_1stc <-ggplot(First_period, aes(x=log10(RCA_JP), y=log10(RCA_AI), label = '')) + 
  geom_point(aes(colour = sector, size = JP_Com),show.legend = F, stroke = 2) +  
  geom_text() +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2)+
  geom_vline(xintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2) +
  scale_color_brewer(palette="Dark2") + theme_minimal() +
  geom_label_repel(data = First_period, aes(label = ifelse(RCA_JP>1 & RCA_AI>1,as.character(field_name),'')),nudge_y = -0.6) +
  geom_rect(aes(NULL, NULL, xmin = Inf, xmax = 0), ymin = Inf, ymax = 0,alpha=0.005,fill="royalblue2") +
  scale_size_continuous(range = c(1, 10)) +
  ggtitle("Japan") +
  xlab(NULL) +
  ylab(NULL)+
  ylim(-2, 2)+
  xlim(-0.6, 0.6)

tiff("Figures_IPC/RCA_Comparison_IPC_1st.jpg", width = 12, height = 7, units = 'in', res = 200)
multiplot(FigUS_1st, FigCN_1st, FigKR_1st, FigJP_1st, cols=2)
dev.off()

tiff("Figures_IPC/RCA_Comparison_IPC_1st_optb.jpg", width = 12, height = 7, units = 'in', res = 200)
multiplot(FigUS_1stb, FigCN_1stb, FigKR_1stb, FigJP_1stb, cols=2)
dev.off()

tiff("Figures_IPC/RCA_Comparison_IPC_1st_optc.jpg", width = 12, height = 7, units = 'in', res = 200)
multiplot(FigUS_1stc, FigCN_1stc, FigKR_1stc, FigJP_1stc, cols=2)
dev.off()

#4.2.Second period ----
reg_tech2_countries <- read.csv("Data_IPC/reg_tech_SecondPeriod.csv", sep = ";", header = TRUE, dec=",")
#4.2.1. Second Period Countries ----
mat_reg_tech2_countries <- reg_tech2_countries %>%
  arrange(techn_field_nr, ctry_code) %>%
  pivot_wider(names_from = techn_field_nr, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

rownames(mat_reg_tech2_countries) <- mat_reg_tech2_countries %>% pull(ctry_code)

mat_reg_tech2_countries %<>% select(-ctry_code) %>%
  as.matrix() %>%
  round()

reg_RCA2_countries <- mat_reg_tech2_countries %>% location.quotient(binary = F) %>% 
  as.data.frame() %>% 
  rownames_to_column("ctry_code") %>% 
  as_tibble() %>% 
  gather(key = "techn_field_nr", value = "RCA", -ctry_code) %>%
  arrange(ctry_code, techn_field_nr)

###4.2.2. Second Period AI----
reg_tech2_AI <- read.csv("Data_IPC/reg_techAI_SecondPeriod.csv", sep = ";", header = TRUE, dec=",")

mat_reg_tech2_AI <- reg_tech2_AI %>%
  arrange(techn_field_nr, ctry_code) %>%
  pivot_wider(names_from = techn_field_nr, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

rownames(mat_reg_tech2_AI) <- mat_reg_tech2_AI %>% pull(ctry_code)

mat_reg_tech2_AI %<>% select(-ctry_code) %>%
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
KnwComp_Country_2nd_RCA <- read.csv("Data_calculations_IPC/KnowledgeComp_PerCountry_2nd_All_RCAs.csv", sep = ";", header = T, dec=",")
KnwComp_Country_2nd_RCA <- as.data.frame(t(KnwComp_Country_2nd_RCA))
KnwComp_Country_2nd_RCA <- KnwComp_Country_2nd_RCA %>%
  row_to_names(row_number = 1)
KnwComp_Country_2nd_RCA<- KnwComp_Country_2nd_RCA[(-36),]
KnwComp_Country_2nd_RCA$techn_field_nr <- Second_period$techn_field_nr
Second_period$US_Com <- KnwComp_Country_2nd_RCA$US3
Second_period$CN_Com <- KnwComp_Country_2nd_RCA$CN3
Second_period$KR_Com <- KnwComp_Country_2nd_RCA$KR3
Second_period$JP_Com <- KnwComp_Country_2nd_RCA$JP3
i <- c(9:12)
Second_period[ , i] <- apply(Second_period[ , i], 2,            # Specify own function within apply
                            function(x) as.integer(as.character(x)))

#remove files we don't need:
rm(AI_Second_period, CN_Second_period, JP_Second_period, KR_Second_period, KnwComp_Country_2nd_RCA,
   mat_reg_tech2_AI, mat_reg_tech2_countries, reg_RCA2_AI, reg_RCA2_countries, reg_tech2_AI, reg_tech2_countries,
   US_Second_period, i)

#Now we plot:
FigUS_2nd <- ggplot(Second_period, aes(x=log10(RCA_US), y=log10(RCA_AI), label = field_name)) + 
  geom_point(aes(colour = sector, size = US_Com),show.legend = F, stroke = 2) +  
  geom_text(aes(label=ifelse(RCA_US>1 & RCA_AI>1,as.character(field_name),''), size = 2), show.legend = F) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2)+
  geom_vline(xintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2) +
  scale_color_brewer(palette="Dark2") + theme_minimal() +
  geom_rect(aes(NULL, NULL, xmin = Inf, xmax = 0), ymin = Inf, ymax = 0,alpha=0.005,fill="royalblue2") +
  scale_size_continuous(range = c(1, 10)) +
  ggtitle("United States") +
  xlab(NULL) +
  ylab(NULL) +
  ylim(-0.5, 1.5)+
  xlim(-0.1, 0.36)

FigUS_2ndb <- ggplot(Second_period, aes(x=log10(RCA_US), y=log10(RCA_AI), label = field_name)) + 
  geom_point(aes(colour = sector, size = US_Com),show.legend = F, stroke = 2) +  
  geom_text(aes(label=ifelse(RCA_US>1 & RCA_AI>1,as.character(field_name),''), size = 2), show.legend = F) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2)+
  geom_vline(xintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2) +
  scale_color_brewer(palette="Dark2") + theme_minimal() +
  geom_rect(aes(NULL, NULL, xmin = Inf, xmax = 0), ymin = Inf, ymax = 0,alpha=0.005,fill="royalblue2") +
  scale_size_continuous(range = c(1, 10)) +
  ggtitle("United States") +
  xlab(NULL) +
  ylab(NULL) +
  ylim(-2, 2)+
  xlim(-0.6, 0.6)

FigUS_2ndc <- ggplot(Second_period, aes(x=log10(RCA_US), y=log10(RCA_AI), label = '')) + 
  geom_point(aes(colour = sector, size = US_Com),show.legend = F, stroke = 2) +  
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2)+
  geom_vline(xintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2) +
  scale_color_brewer(palette="Dark2") + theme_minimal() +
  geom_label_repel(data = Second_period, aes(label = ifelse(RCA_US>1 & RCA_AI>1,as.character(field_name),''))) +
  geom_rect(aes(NULL, NULL, xmin = Inf, xmax = 0), ymin = Inf, ymax = 0,alpha=0.005,fill="royalblue2") +
  scale_size_continuous(range = c(1, 10)) +
  ggtitle("United States") +
  xlab(NULL) +
  ylab(NULL) +
  ylim(-2, 2)+
  xlim(-0.6, 0.6)

FigCN_2nd <-ggplot(Second_period, aes(x=log10(RCA_CN), y=log10(RCA_AI), label = field_name)) + 
  geom_point(aes(colour = sector, size = CN_Com),show.legend = F, stroke = 2) +  
  geom_text(aes(label=ifelse(RCA_CN>1 & RCA_AI>1,as.character(field_name),''), size = 2), show.legend = F) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2)+
  geom_vline(xintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2) +
  scale_color_brewer(palette="Dark2") + theme_minimal() +
  geom_rect(aes(NULL, NULL, xmin = Inf, xmax = 0), ymin = Inf, ymax = 0,alpha=0.005,fill="royalblue2") +
  scale_size_continuous(range = c(1, 10)) +
  ggtitle("China") +
  xlab(NULL) +
  ylab(NULL)+
  ylim(-0.5, 1.5)+
  xlim(-0.1, 0.36)

FigCN_2ndb <-ggplot(Second_period, aes(x=log10(RCA_CN), y=log10(RCA_AI), label = field_name)) + 
  geom_point(aes(colour = sector, size = CN_Com),show.legend = F, stroke = 2) +  
  geom_text(aes(label=ifelse(RCA_CN>1 & RCA_AI>1,as.character(field_name),''), size = 2), show.legend = F) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2)+
  geom_vline(xintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2) +
  scale_color_brewer(palette="Dark2") + theme_minimal() +
  geom_rect(aes(NULL, NULL, xmin = Inf, xmax = 0), ymin = Inf, ymax = 0,alpha=0.005,fill="royalblue2") +
  scale_size_continuous(range = c(1, 10)) +
  ggtitle("China") +
  xlab(NULL) +
  ylab(NULL)+
  ylim(-2, 2)+
  xlim(-0.6, 0.6)

FigCN_2ndc <-ggplot(Second_period, aes(x=log10(RCA_CN), y=log10(RCA_AI), label = '')) + 
  geom_point(aes(colour = sector, size = CN_Com),show.legend = F, stroke = 2) +  
  geom_label_repel(data = Second_period, aes(label = ifelse(RCA_CN>1 & RCA_AI>1,as.character(field_name),''))) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2)+
  geom_vline(xintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2) +
  scale_color_brewer(palette="Dark2") + theme_minimal() +
  geom_rect(aes(NULL, NULL, xmin = Inf, xmax = 0), ymin = Inf, ymax = 0,alpha=0.005,fill="royalblue2") +
  scale_size_continuous(range = c(1, 10)) +
  ggtitle("China") +
  xlab(NULL) +
  ylab(NULL)+
  ylim(-2, 2)+
  xlim(-0.6, 0.6)

FigKR_2nd <-ggplot(Second_period, aes(x=log10(RCA_KR), y=log10(RCA_AI), label = field_name)) + 
  geom_point(aes(colour = sector, size = KR_Com),show.legend = F, stroke = 2) +  
  geom_text(aes(label=ifelse(RCA_KR>1 & RCA_AI>1,as.character(field_name),''), size = 2), show.legend = F) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2)+
  geom_vline(xintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2) +
  scale_color_brewer(palette="Dark2") + theme_minimal() +
  geom_rect(aes(NULL, NULL, xmin = Inf, xmax = 0), ymin = Inf, ymax = 0,alpha=0.005,fill="royalblue2") +
  scale_size_continuous(range = c(1, 10)) +
  ggtitle("South Korea") +
  xlab(NULL) +
  ylab(NULL)+
  ylim(-0.5, 1.5)+
  xlim(-0.1, 0.36)

FigKR_2ndb <-ggplot(Second_period, aes(x=log10(RCA_KR), y=log10(RCA_AI), label = field_name)) + 
  geom_point(aes(colour = sector, size = KR_Com),show.legend = F, stroke = 2) +  
  geom_text(aes(label=ifelse(RCA_KR>1 & RCA_AI>1,as.character(field_name),''), size = 2), show.legend = F) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2)+
  geom_vline(xintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2) +
  scale_color_brewer(palette="Dark2") + theme_minimal() +
  geom_rect(aes(NULL, NULL, xmin = Inf, xmax = 0), ymin = Inf, ymax = 0,alpha=0.005,fill="royalblue2") +
  scale_size_continuous(range = c(1, 10)) +
  ggtitle("South Korea") +
  xlab(NULL) +
  ylab(NULL)+
  ylim(-2, 2)+
  xlim(-0.6, 0.6)

FigKR_2ndc <-ggplot(Second_period, aes(x=log10(RCA_KR), y=log10(RCA_AI), label = '')) + 
  geom_point(aes(colour = sector, size = KR_Com),show.legend = F, stroke = 2) +  
  geom_label_repel(data = Second_period, aes(label = ifelse(RCA_KR>1 & RCA_AI>1,as.character(field_name),''))) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2)+
  geom_vline(xintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2) +
  scale_color_brewer(palette="Dark2") + theme_minimal() +
  geom_rect(aes(NULL, NULL, xmin = Inf, xmax = 0), ymin = Inf, ymax = 0,alpha=0.005,fill="royalblue2") +
  scale_size_continuous(range = c(1, 10)) +
  ggtitle("South Korea") +
  xlab(NULL) +
  ylab(NULL)+
  ylim(-2, 2)+
  xlim(-0.6, 0.6)

 FigJP_2nd <-ggplot(Second_period, aes(x=log10(RCA_JP), y=log10(RCA_AI), label = field_name)) + 
  geom_point(aes(colour = sector, size = JP_Com),show.legend = F, stroke = 2) +  
  geom_text(aes(label=ifelse(RCA_JP>1 & RCA_AI>1,as.character(field_name),''), size = 2), show.legend = F) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2)+
  geom_vline(xintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2) +
  scale_color_brewer(palette="Dark2") + theme_minimal() +
  geom_rect(aes(NULL, NULL, xmin = Inf, xmax = 0), ymin = Inf, ymax = 0,alpha=0.005,fill="royalblue2") +
  scale_size_continuous(range = c(1, 10)) +
  ggtitle("Japan") +
  xlab(NULL) +
  ylab(NULL)+
  ylim(-0.5, 1.5)+
  xlim(-0.1, 0.36)

FigJP_2ndb <-ggplot(Second_period, aes(x=log10(RCA_JP), y=log10(RCA_AI), label = field_name)) + 
  geom_point(aes(colour = sector, size = JP_Com),show.legend = F, stroke = 2) +  
  geom_text(aes(label=ifelse(RCA_JP>1 & RCA_AI>1,as.character(field_name),''), size = 2), show.legend = F) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2)+
  geom_vline(xintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2) +
  scale_color_brewer(palette="Dark2") + theme_minimal() +
  geom_rect(aes(NULL, NULL, xmin = Inf, xmax = 0), ymin = Inf, ymax = 0,alpha=0.005,fill="royalblue2") +
  scale_size_continuous(range = c(1, 10)) +
  ggtitle("Japan") +
  xlab(NULL) +
  ylab(NULL)+
  ylim(-2, 2)+
  xlim(-0.6, 0.6)

FigJP_2ndc <-ggplot(Second_period, aes(x=log10(RCA_JP), y=log10(RCA_AI), label = '')) + 
  geom_point(aes(colour = sector, size = JP_Com),show.legend = F, stroke = 2) +  
  geom_label_repel(data = Second_period, aes(label = ifelse(RCA_JP>1 & RCA_AI>1,as.character(field_name),''))) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2)+
  geom_vline(xintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2) +
  scale_color_brewer(palette="Dark2") + theme_minimal() +
  geom_rect(aes(NULL, NULL, xmin = Inf, xmax = 0), ymin = Inf, ymax = 0,alpha=0.005,fill="royalblue2") +
  scale_size_continuous(range = c(1, 10)) +
  ggtitle("Japan") +
  xlab(NULL) +
  ylab(NULL)+
  ylim(-2, 2)+
  xlim(-0.6, 0.6)

tiff("Figures_IPC/RCA_Comparison_IPC_2nd.jpg", width = 12, height = 7, units = 'in', res = 200)
multiplot(FigUS_2nd, FigCN_2nd, FigKR_2nd, FigJP_2nd, cols=2)
dev.off()

tiff("Figures_IPC/RCA_Comparison_IPC_2nd_optb.jpg", width = 12, height = 7, units = 'in', res = 200)
multiplot(FigUS_2ndb, FigCN_2ndb, FigKR_2ndb, FigJP_2ndb, cols=2)
dev.off()

tiff("Figures_IPC/RCA_Comparison_IPC_2nd_optc.jpg", width = 12, height = 7, units = 'in', res = 200)
multiplot(FigUS_2ndc, FigCN_2ndc, FigKR_2ndc, FigJP_2ndc, cols=2)
dev.off()

#4.3.Third period ----
reg_tech3_countries <- read.csv("Data_IPC/reg_tech_ThirdPeriod.csv", sep = ";", header = TRUE, dec=",")
#4.3.1. Third Period Countries ----
mat_reg_tech3_countries <- reg_tech3_countries %>%
  arrange(techn_field_nr, ctry_code) %>%
  pivot_wider(names_from = techn_field_nr, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

rownames(mat_reg_tech3_countries) <- mat_reg_tech3_countries %>% pull(ctry_code)

mat_reg_tech3_countries %<>% select(-ctry_code) %>%
  as.matrix() %>%
  round()

reg_RCA3_countries <- mat_reg_tech3_countries %>% location.quotient(binary = F) %>% 
  as.data.frame() %>% 
  rownames_to_column("ctry_code") %>% 
  as_tibble() %>% 
  gather(key = "techn_field_nr", value = "RCA", -ctry_code) %>%
  arrange(ctry_code, techn_field_nr)

###4.3.2. Third Period AI----
reg_tech3_AI <- read.csv("Data_IPC/reg_techAI_ThirdPeriod.csv", sep = ";", header = TRUE, dec=",")

mat_reg_tech3_AI <- reg_tech3_AI %>%
  arrange(techn_field_nr, ctry_code) %>%
  pivot_wider(names_from = techn_field_nr, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

rownames(mat_reg_tech3_AI) <- mat_reg_tech3_AI %>% pull(ctry_code)

mat_reg_tech3_AI %<>% select(-ctry_code) %>%
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
KnwComp_Country_3rd_RCA <- read.csv("Data_calculations_IPC/KnowledgeComp_PerCountry_3rd_All_RCAs.csv", sep = ";", header = T, dec=",")
KnwComp_Country_3rd_RCA <- as.data.frame(t(KnwComp_Country_3rd_RCA))
KnwComp_Country_3rd_RCA <- KnwComp_Country_3rd_RCA %>%
  row_to_names(row_number = 1)
KnwComp_Country_3rd_RCA<- KnwComp_Country_3rd_RCA[(-36),]
KnwComp_Country_3rd_RCA$techn_field_nr <- Third_period$techn_field_nr
Third_period$US_Com <- KnwComp_Country_3rd_RCA$US3
Third_period$CN_Com <- KnwComp_Country_3rd_RCA$CN3
Third_period$KR_Com <- KnwComp_Country_3rd_RCA$KR3
Third_period$JP_Com <- KnwComp_Country_3rd_RCA$JP3
i <- c(9:12)
Third_period[ , i] <- apply(Third_period[ , i], 2,            # Specify own function within apply
                             function(x) as.integer(as.character(x)))

#remove files we don't need:
rm(AI_Third_period, CN_Third_period, JP_Third_period, KR_Third_period, KnwComp_Country_3rd_RCA,
   mat_reg_tech3_AI, mat_reg_tech3_countries, reg_RCA3_AI, reg_RCA3_countries, reg_tech3_AI, reg_tech3_countries,
   US_Third_period, i)

#Now we plot:
FigUS_3rd <- ggplot(Third_period, aes(x=log10(RCA_US), y=log10(RCA_AI), label = field_name)) + 
  geom_point(aes(colour = sector, size = US_Com),show.legend = F, stroke = 2) +  
  geom_text(aes(label=ifelse(RCA_US>1 & RCA_AI>1,as.character(field_name),''), size = 2, vjust = factor(sector)), show.legend = F) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2)+
  geom_vline(xintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2) +
  scale_color_brewer(palette="Dark2") + theme_minimal() +
  geom_rect(aes(NULL, NULL, xmin = Inf, xmax = 0), ymin = Inf, ymax = 0,alpha=0.005,fill="royalblue2") +
  scale_size_continuous(range = c(1, 10)) +
  ggtitle("United States") +
  xlab(NULL) +
  ylab(NULL) +
  ylim(-0.5, 1)+
  xlim(-0.1, 0.36)

FigUS_3rdb <- ggplot(Third_period, aes(x=log10(RCA_US), y=log10(RCA_AI), label = field_name)) + 
  geom_point(aes(colour = sector, size = US_Com),show.legend = F, stroke = 2) +  
  geom_text(aes(label=ifelse(RCA_US>1 & RCA_AI>1,as.character(field_name),''), size = 2), show.legend = F) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2)+
  geom_vline(xintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2) +
  scale_color_brewer(palette="Dark2") + theme_minimal() +
  geom_rect(aes(NULL, NULL, xmin = Inf, xmax = 0), ymin = Inf, ymax = 0,alpha=0.005,fill="royalblue2") +
  scale_size_continuous(range = c(1, 10)) +
  ggtitle("United States") +
  xlab(NULL) +
  ylab(NULL) +
  ylim(-2, 2)+
  xlim(-0.6, 0.6)

FigUS_3rdc <- ggplot(Third_period, aes(x=log10(RCA_US), y=log10(RCA_AI), label = '')) + 
  geom_point(aes(colour = sector, size = US_Com),show.legend = F, stroke = 2) +  
  geom_text() +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2)+
  geom_vline(xintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2) +
  scale_color_brewer(palette="Dark2") + theme_minimal() +
  geom_label_repel(data = Third_period, aes(label = ifelse(RCA_US>1 & RCA_AI>1,as.character(field_name),'')),
                   nudge_y = -.2, nudge_x = -.2) +
  geom_rect(aes(NULL, NULL, xmin = Inf, xmax = 0), ymin = Inf, ymax = 0,alpha=0.005,fill="royalblue2") +
  scale_size_continuous(range = c(1, 10)) +
  ggtitle("United States") +
  xlab(NULL) +
  ylab(NULL) +
  ylim(-2, 2)+
  xlim(-0.6, 0.6)

FigCN_3rd <-ggplot(Third_period, aes(x=log10(RCA_CN), y=log10(RCA_AI), label = field_name)) + 
  geom_point(aes(colour = sector, size = CN_Com),show.legend = F, stroke = 2) +  
  geom_text(aes(label=ifelse(RCA_CN>1 & RCA_AI>1,as.character(field_name),''), size = 2), show.legend = F) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2)+
  geom_vline(xintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2) +
  scale_color_brewer(palette="Dark2") + theme_minimal() +
  geom_rect(aes(NULL, NULL, xmin = Inf, xmax = 0), ymin = Inf, ymax = 0,alpha=0.005,fill="royalblue2") +
  scale_size_continuous(range = c(1, 10)) +
  ggtitle("China") +
  xlab(NULL) +
  ylab(NULL)+
  ylim(-0.5, 1)+
  xlim(-0.1, 0.36)

FigCN_3rdb <-ggplot(Third_period, aes(x=log10(RCA_CN), y=log10(RCA_AI), label = field_name)) + 
  geom_point(aes(colour = sector, size = CN_Com),show.legend = F, stroke = 2) +  
  geom_text(aes(label=ifelse(RCA_CN>1 & RCA_AI>1,as.character(field_name),''), size = 2), show.legend = F) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2)+
  geom_vline(xintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2) +
  scale_color_brewer(palette="Dark2") + theme_minimal() +
  geom_rect(aes(NULL, NULL, xmin = Inf, xmax = 0), ymin = Inf, ymax = 0,alpha=0.005,fill="royalblue2") +
  scale_size_continuous(range = c(1, 10)) +
  ggtitle("China") +
  xlab(NULL) +
  ylab(NULL)+
  ylim(-2, 2)+
  xlim(-0.6, 0.6)

FigCN_3rdc <-ggplot(Third_period, aes(x=log10(RCA_CN), y=log10(RCA_AI), label = '')) + 
  geom_point(aes(colour = sector, size = CN_Com),show.legend = F, stroke = 2) +  
  geom_text() +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2)+
  geom_vline(xintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2) +
  scale_color_brewer(palette="Dark2") + theme_minimal() +
  geom_label_repel(data = Third_period, aes(label = ifelse(RCA_CN>1 & RCA_AI>1,as.character(field_name),''))) +
  geom_rect(aes(NULL, NULL, xmin = Inf, xmax = 0), ymin = Inf, ymax = 0,alpha=0.005,fill="royalblue2") +
  scale_size_continuous(range = c(1, 10)) +
  ggtitle("China") +
  xlab(NULL) +
  ylab(NULL)+
  ylim(-2, 2)+
  xlim(-0.6, 0.6)

FigKR_3rd <-ggplot(Third_period, aes(x=log10(RCA_KR), y=log10(RCA_AI), label = field_name)) + 
  geom_point(aes(colour = sector, size = KR_Com),show.legend = F, stroke = 2) +  
  geom_text(aes(label=ifelse(RCA_KR>1 & RCA_AI>1,as.character(field_name),''), size = 2), show.legend = F) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2)+
  geom_vline(xintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2) +
  scale_color_brewer(palette="Dark2") + theme_minimal() +
  geom_rect(aes(NULL, NULL, xmin = Inf, xmax = 0), ymin = Inf, ymax = 0,alpha=0.005,fill="royalblue2") +
  scale_size_continuous(range = c(1, 10)) +
  ggtitle("South Korea") +
  xlab(NULL) +
  ylab(NULL)+
  ylim(-0.5, 1)+
  xlim(-0.1, 0.36)

FigKR_3rdb <-ggplot(Third_period, aes(x=log10(RCA_KR), y=log10(RCA_AI), label = field_name)) + 
  geom_point(aes(colour = sector, size = KR_Com),show.legend = F, stroke = 2) +  
  geom_text(aes(label=ifelse(RCA_KR>1 & RCA_AI>1,as.character(field_name),''), size = 2), show.legend = F) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2)+
  geom_vline(xintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2) +
  scale_color_brewer(palette="Dark2") + theme_minimal() +
  geom_rect(aes(NULL, NULL, xmin = Inf, xmax = 0), ymin = Inf, ymax = 0,alpha=0.005,fill="royalblue2") +
  scale_size_continuous(range = c(1, 10)) +
  ggtitle("South Korea") +
  xlab(NULL) +
  ylab(NULL)+
  ylim(-2, 2)+
  xlim(-0.6, 0.6)

FigKR_3rdc <-ggplot(Third_period, aes(x=log10(RCA_KR), y=log10(RCA_AI), label = '')) + 
  geom_point(aes(colour = sector, size = KR_Com),show.legend = F, stroke = 2) +  
  geom_text() +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2)+
  geom_vline(xintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2) +
  scale_color_brewer(palette="Dark2") + theme_minimal() +
  geom_label_repel(data = Third_period, aes(label = ifelse(RCA_KR>1 & RCA_AI>1,as.character(field_name),''))) +
  geom_rect(aes(NULL, NULL, xmin = Inf, xmax = 0), ymin = Inf, ymax = 0,alpha=0.005,fill="royalblue2") +
  scale_size_continuous(range = c(1, 10)) +
  ggtitle("South Korea") +
  xlab(NULL) +
  ylab(NULL)+
  ylim(-2, 2)+
  xlim(-0.6, 0.6)

FigJP_3rd <-ggplot(Third_period, aes(x=log10(RCA_JP), y=log10(RCA_AI), label = field_name)) + 
  geom_point(aes(colour = sector, size = JP_Com),show.legend = F, stroke = 2) +  
  geom_text(aes(label=ifelse(RCA_JP>1 & RCA_AI>1,as.character(field_name),''), size = 2), show.legend = F) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2)+
  geom_vline(xintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2) +
  scale_color_brewer(palette="Dark2") + theme_minimal() +
  geom_rect(aes(NULL, NULL, xmin = Inf, xmax = 0), ymin = Inf, ymax = 0,alpha=0.005,fill="royalblue2") +
  scale_size_continuous(range = c(1, 10)) +
  ggtitle("Japan") +
  xlab(NULL) +
  ylab(NULL)+
  ylim(-0.5, 1)+
  xlim(-0.1, 0.36)

FigJP_3rdb <-ggplot(Third_period, aes(x=log10(RCA_JP), y=log10(RCA_AI), label = field_name)) + 
  geom_point(aes(colour = sector, size = JP_Com),show.legend = F, stroke = 2) +  
  geom_text(aes(label=ifelse(RCA_JP>1 & RCA_AI>1,as.character(field_name),''), size = 2), show.legend = F) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2)+
  geom_vline(xintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2) +
  scale_color_brewer(palette="Dark2") + theme_minimal() +
  geom_rect(aes(NULL, NULL, xmin = Inf, xmax = 0), ymin = Inf, ymax = 0,alpha=0.005,fill="royalblue2") +
  scale_size_continuous(range = c(1, 10)) +
  ggtitle("Japan") +
  xlab(NULL) +
  ylab(NULL)+
  ylim(-2, 2)+
  xlim(-0.6, 0.6)

FigJP_3rdc <-ggplot(Third_period, aes(x=log10(RCA_JP), y=log10(RCA_AI), label = '')) + 
  geom_point(aes(colour = sector, size = JP_Com),show.legend = F, stroke = 2) +  
  geom_text() +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2)+
  geom_vline(xintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2) +
  scale_color_brewer(palette="Dark2") + theme_minimal() +
  geom_label_repel(data = Third_period, aes(label = ifelse(RCA_JP>1 & RCA_AI>1,as.character(field_name),''))) +
  geom_rect(aes(NULL, NULL, xmin = Inf, xmax = 0), ymin = Inf, ymax = 0,alpha=0.005,fill="royalblue2") +
  scale_size_continuous(range = c(1, 10)) +
  ggtitle("Japan") +
  xlab(NULL) +
  ylab(NULL)+
  ylim(-2, 2)+
  xlim(-0.6, 0.6)

tiff("Figures_IPC/RCA_Comparison_IPC_3rd.jpg", width = 12, height = 7, units = 'in', res = 200)
multiplot(FigUS_3rd, FigCN_3rd, FigKR_3rd, FigJP_3rd, cols=2)
dev.off()

tiff("Figures_IPC/RCA_Comparison_IPC_3rd_optb.jpg", width = 12, height = 7, units = 'in', res = 200)
multiplot(FigUS_3rdb, FigCN_3rdb, FigKR_3rdb, FigJP_3rdb, cols=2)
dev.off()

tiff("Figures_IPC/RCA_Comparison_IPC_3rd_optc.jpg", width = 12, height = 7, units = 'in', res = 200)
multiplot(FigUS_3rdc, FigCN_3rdc, FigKR_3rdc, FigJP_3rdc, cols=2)
dev.off()

tiff("Figures_IPC/RCA_Comparison_IPC_3rd_optd.jpg", width = 6, height = 12, units = 'in', res = 200)
multiplot(FigUS_3rdb, FigCN_3rdb, FigKR_3rdb, FigJP_3rdb, cols=1)
dev.off()


#4.4. AI perspective----
KnwComp_Country_1st_RCA <- read.csv("Data_calculations_IPC/KnowledgeComp_PerCountry_1st_All_RCAs_AI.csv", sep = ";", header = T, dec=",")
KnwComp_Country_1st_RCA <- as.data.frame(t(KnwComp_Country_1st_RCA))
KnwComp_Country_1st_RCA <- KnwComp_Country_1st_RCA %>%
  row_to_names(row_number = 1)
KnwComp_Country_1st_RCA<- KnwComp_Country_1st_RCA[(-36),]
First_period$AI_Com <- KnwComp_Country_1st_RCA$AI_pat3
i <- c(12:13)
First_period[ , i] <- apply(First_period[ , i], 2,            # Specify own function within apply
                            function(x) as.integer(as.character(x)))
KnwComp_Country_2nd_RCA <- read.csv("Data_calculations_IPC/KnowledgeComp_PerCountry_2nd_All_RCAs_AI.csv", sep = ";", header = T, dec=",")
KnwComp_Country_2nd_RCA <- as.data.frame(t(KnwComp_Country_2nd_RCA))
KnwComp_Country_2nd_RCA <- KnwComp_Country_2nd_RCA %>%
  row_to_names(row_number = 1)
KnwComp_Country_2nd_RCA<- KnwComp_Country_2nd_RCA[(-36),]
Second_period$AI_Com <- KnwComp_Country_2nd_RCA$AI_pat3
i <- c(12:13)
Second_period[ , i] <- apply(Second_period[ , i], 2,            # Specify own function within apply
                            function(x) as.integer(as.character(x)))

KnwComp_Country_3rd_RCA <- read.csv("Data_calculations_IPC/KnowledgeComp_PerCountry_3rd_All_RCAs_AI.csv", sep = ";", header = T, dec=",")
KnwComp_Country_3rd_RCA <- as.data.frame(t(KnwComp_Country_3rd_RCA))
KnwComp_Country_3rd_RCA <- KnwComp_Country_3rd_RCA %>%
  row_to_names(row_number = 1)
KnwComp_Country_3rd_RCA<- KnwComp_Country_3rd_RCA[(-36),]
Third_period$AI_Com <- KnwComp_Country_3rd_RCA$AI_pat3
i <- c(12:13)
Third_period[ , i] <- apply(Third_period[ , i], 2,            # Specify own function within apply
                            function(x) as.integer(as.character(x)))
First_period$Period <- "Period 1 (1974-1988)"
Second_period$Period <- "Period 2 (1989-2003)"
Third_period$Period <- "Period 3 (2004-2018)"

AI_persp <- rbind(First_period, Second_period, Third_period)

tiff("Figures_IPC/AI_perspective.jpg", width = 10, height = 8, units = 'in', res = 200)
ggplot(AI_persp, aes(x=log10(AI_Com), y=log10(RCA_AI), label = '')) + 
  geom_point(aes(colour = sector, size = AI_Com), stroke = 2) +  
  geom_text() +
  facet_wrap(~Period, nrow=3) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2)+
  scale_color_brewer(palette="Dark2") + theme_classic() +
  geom_label_repel(data = AI_persp, aes(label = ifelse(AI_Com>1,as.character(field_name),''))) +
  scale_size_continuous(range = c(1, 10)) +
  ggtitle("AI specializations and complexity over time") +
  xlab(NULL) +
  ylab(NULL)+
  ylim(-.5, 1.8)
dev.off()

#5. Visualization Indicators -----
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
#5.1.Herfindahl and Entropy ----
Indicators1st <- read.csv("Data_calculations_IPC/Indicators_1st_period_IPC.csv", sep = ";", header = TRUE, dec=",")
Indicators2nd <- read.csv("Data_calculations_IPC/Indicators_2nd_period_IPC.csv", sep = ";", header = TRUE, dec=",")
Indicators3rd <- read.csv("Data_calculations_IPC/Indicators_3rd_period_IPC.csv", sep = ";", header = TRUE, dec=",")

Indicators <- rbind(Indicators1st, Indicators2nd, Indicators3rd)
Indicators$Period <- gsub("1st", "Period 1 (1974-1988)", str_trim(Indicators$Period))
Indicators$Period <- gsub("2nd", "Period 2 (1989-2003)", str_trim(Indicators$Period))
Indicators$Period <- gsub("3rd", "Period 3 (2004-2018)", str_trim(Indicators$Period))

Indicators_AI_1st <- read.csv("Data_calculations_IPC/Indicators_1st_period_IPC_AI.csv", sep = ";", header = TRUE, dec=",")
Indicators_AI_2nd <- read.csv("Data_calculations_IPC/Indicators_2nd_period_IPC_AI.csv", sep = ";", header = TRUE, dec=",")
Indicators_AI_3rd <- read.csv("Data_calculations_IPC/Indicators_3rd_period_IPC_AI.csv", sep = ";", header = TRUE, dec=",")
Indicators_AI <- rbind(Indicators_AI_1st, Indicators_AI_2nd, Indicators_AI_3rd)
Indicators_AI$Period <- gsub("1st", "Period 1 (1974-1988)", str_trim(Indicators_AI$Period))
Indicators_AI$Period <- gsub("2nd", "Period 2 (1989-2003)", str_trim(Indicators_AI$Period))
Indicators_AI$Period <- gsub("3rd", "Period 3 (2004-2018)", str_trim(Indicators_AI$Period))

Indicators_AI2 <- Indicators_AI[Indicators_AI$X == "AI_pat", ]

library(patchwork) #for cutting out the X labs while keeping the legend
Herfindahl_AI <- ggplot(Indicators_AI2, aes(x=Period, y=Herfindahl, fill=Period)) +
  geom_bar(stat="identity")+ theme_minimal() + labs(x = "") +
  scale_x_discrete(labels = NULL) + ylab(NULL) +
  ggtitle("AI Herfindahl Index")

Indicators_countries <- Indicators[Indicators$X == "US"|
                                           Indicators$X == "CN"|
                                           Indicators$X == "KR"|
                                           Indicators$X == "JP",]

Herfindahl_Countries <- ggplot(Indicators_countries, aes(x=X, y=Herfindahl, fill=Period)) +
  geom_bar(stat="identity", position=position_dodge())+theme_minimal()+ xlab(NULL) +
  ggtitle("Countries Herfindahl Index")

library(scales) #for scaling without cutting data out
Herfindahl_Countriesb <- ggplot(Indicators_countries, aes(x=X, y=Herfindahl, fill=Period)) +
  geom_bar(stat="identity", position=position_dodge(),show.legend = F)+theme_minimal()+ xlab(NULL) + ylab(NULL) +
  scale_y_continuous(limits=c(.035,0.055),oob = rescale_none) +
  ggtitle("Countries Herfindahl Index")

Entropy_RCA_AI <- ggplot(Indicators_AI2, aes(x=Period, y=Entropy_RCA, fill=Period)) +
  geom_bar(stat="identity")+theme_minimal() +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) + labs(x = "") +
  scale_x_discrete(labels = NULL) + ylab(NULL) +
  ggtitle("AI Shanon Index")

Entropy_RCA_Countries <- ggplot(Indicators_countries, aes(x=X, y=Entropy_RCA, fill=Period)) +
  geom_bar(stat="identity", position=position_dodge())+theme_minimal()+ xlab(NULL)+
  ggtitle("Countries Shanon Index")
Entropy_RCA_Countriesb <- ggplot(Indicators_countries, aes(x=X, y=Entropy_RCA, fill=Period)) +
  geom_bar(stat="identity", position=position_dodge(),show.legend = F)+theme_minimal()+ 
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) + xlab(NULL) + ylab(NULL) + 
  scale_y_continuous(limits=c(3.4,4.2),oob = rescale_none)+
  ggtitle("Countries Shanon Index")


tiff("Figures_IPC/Indicators_Herfindahl.jpg", width = 6, height = 4, units = 'in', res = 200)
multiplot(Herfindahl_Countries, Herfindahl_AI, cols=2)
dev.off()

tiff("Figures_IPC/Indicators_Herfindahl_optb.jpg", width = 6, height = 4, units = 'in', res = 200)
multiplot(Herfindahl_Countriesb, Herfindahl_AI, cols=2)
dev.off()

tiff("Figures_IPC/Indicators_Entropy.jpg", width = 6, height = 4, units = 'in', res = 200)
multiplot(Entropy_RCA_Countries, Entropy_RCA_AI, cols=2)
dev.off()

tiff("Figures_IPC/Indicators_Entropy_optb.jpg", width = 6, height = 4, units = 'in', res = 200)
multiplot(Entropy_RCA_Countriesb, Entropy_RCA_AI, cols=2)
dev.off()

#5.2.Relatedness ----
Relatedness_1st <- read.csv("Data_calculations_IPC/Relatedness_1st_period_IPC.csv", sep = ";", header = TRUE, dec=",")
Relatedness_2nd <- read.csv("Data_calculations_IPC/Relatedness_2nd_period_IPC.csv", sep = ";", header = TRUE, dec=",")
Relatedness_3rd <- read.csv("Data_calculations_IPC/Relatedness_3rd_period_IPC.csv", sep = ";", header = TRUE, dec=",")
Relatedness <- rbind(Relatedness_1st, Relatedness_2nd, Relatedness_3rd)

Relatedness$Period <- gsub("1st", "Period 1 (1974-1988)", str_trim(Relatedness$Period))
Relatedness$Period <- gsub("2nd", "Period 2 (1989-2003)", str_trim(Relatedness$Period))
Relatedness$Period <- gsub("3rd", "Period 3 (2004-2018)", str_trim(Relatedness$Period))

Relatedness1 <- Relatedness[,c(1,14,3)]
names(Relatedness1) <- c("Country", "Period", "Value")
Relatedness1$Indicator <- "Overall Relatedness"

Relatedness2 <- Relatedness[,c(1,14,6)]
names(Relatedness2) <- c("Country", "Period", "Value")
Relatedness2$Indicator <- "AI-core codes"

Relatedness3 <- Relatedness[,c(1,14,9)]
names(Relatedness3) <- c("Country", "Period", "Value")
Relatedness3$Indicator <- "AI-related codes"

Relatedness4 <- Relatedness[,c(1,14,12)]
names(Relatedness4) <- c("Country", "Period", "Value")
Relatedness4$Indicator <- "Surrounding codes"

Relatedness <- rbind(Relatedness1, Relatedness2, Relatedness3, Relatedness4)
rm(Relatedness1, Relatedness2, Relatedness3, Relatedness4)

Relatedness_AI <- Relatedness[Relatedness$Country == "AI", ]
Relatedness <- Relatedness[Relatedness$Country != "AI", ]

ggplot(Relatedness, aes(x=Country, y=Value, color=Indicator)) +
  geom_point()+theme_minimal()+ xlab(NULL) + ylab(NULL) +
  facet_wrap(~Period) + scale_y_log10()

ggplot(Relatedness, aes(x=Country, y=Value, color=Period)) +
  geom_point()+theme_minimal()+ xlab(NULL) + ylab(NULL) +
  facet_wrap(~Indicator)

#very nice one:
ggplot(Relatedness, aes(x=Country, y=Value, fill=Indicator)) +
  geom_bar(stat="identity", position=position_dodge())+theme_minimal()+ xlab(NULL) + ylab(NULL) +
  facet_wrap(~Period)

#nice one:
ggplot(Relatedness, aes(x=Country, y=Value, fill=Period)) +
  geom_bar(stat="identity", position=position_dodge())+theme_minimal()+ xlab(NULL) + ylab(NULL) +
  facet_wrap(~Indicator)

ggplot(Relatedness, aes(x=Period, y=Value, fill=Indicator)) +
  geom_bar(stat="identity", position=position_dodge())+theme_minimal()+ xlab(NULL) + ylab(NULL) +
  facet_wrap(~Country)

ggplot(Relatedness, aes(x=Indicator, y=Value, fill=Period)) +
  geom_bar(stat="identity", position=position_dodge())+theme_minimal()+ xlab(NULL) + ylab(NULL) +
  facet_wrap(~Country)
  #ggtitle("Countries Herfindahl Index") +

Rel_byP_a<- ggplot(Relatedness, aes(x=Country, y=Value, fill=Indicator)) +
  geom_bar(stat="identity", position=position_dodge(), show.legend = F)+theme_minimal()+ xlab(NULL) + ylab("Relatedness") +
  facet_wrap(~Period, ncol = 3)+
  #scale_fill_brewer(palette="Dark2")+theme_minimal() +
  scale_fill_brewer(palette="Paired") + theme_classic() + 
  #scale_fill_brewer(palette="Accent") + theme_minimal() +
  #scale_fill_brewer(palette="Reds") +
  #scale_fill_brewer(palette="Greens") +
  ggtitle("Countries Relatedness in the considered IPC fields") + 
  scale_y_continuous(limits=c(.45,1.5),oob = rescale_none)

Rel_byAI_a<- ggplot(Relatedness_AI, aes(x=Country, y=Value, fill=Indicator)) +
  geom_bar(stat="identity", position=position_dodge())+theme_minimal()+ labs(x = "") + 
  scale_x_discrete(labels = NULL) + ylab("Relatedness") +
  facet_wrap(~Period, ncol = 3)+
  scale_fill_brewer(palette="Paired") + theme_classic() + theme(legend.position="bottom") +
  ggtitle("AI Relatedness in the considered IPC fields") + 
  scale_y_continuous(limits=c(0.1,2.1),oob = rescale_none)

Rel_byP_b <- ggplot(Relatedness, aes(x=Country, y=Value, fill=Period)) +
  geom_bar(stat="identity", position=position_dodge(), show.legend = F)+theme_minimal()+ xlab(NULL) + ylab("Relatedness") +
  facet_wrap(~Indicator, ncol = 4) +
  scale_fill_brewer(palette="Paired") + theme_classic() +
  ggtitle("Countries Relatedness in the considered IPC fields") + 
  scale_y_continuous(limits=c(.45,1.5),oob = rescale_none)

Rel_byAI_b<- ggplot(Relatedness_AI, aes(x=Country, y=Value, fill=Period)) +
  geom_bar(stat="identity", position=position_dodge())+theme_minimal() + labs(x = "") +
  scale_x_discrete(labels = NULL) + ylab("Relatedness") +
  facet_wrap(~Indicator, ncol = 4) +
  scale_fill_brewer(palette="Paired") + theme_classic() + theme(legend.position="bottom") +
  ggtitle("AI Relatedness in the considered IPC fields")+
  scale_y_continuous(limits=c(.1,2.35),oob = rescale_none)

Relatedness2 <- Relatedness
Relatedness2$Indicator <- factor(Relatedness2$Indicator, levels = c("Overall Relatedness", "AI-core codes",
                                                                    "AI-related codes", "Surrounding codes"))

Relatedness_AI2 <- Relatedness_AI
Relatedness_AI2$Indicator <- factor(Relatedness_AI2$Indicator, levels = c("Overall Relatedness", "AI-core codes",
                                                                          "AI-related codes", "Surrounding codes"))
Rel_byP_c <- ggplot(Relatedness2, aes(x=Country, y=Value, fill=Period)) +
  geom_bar(stat="identity", position=position_dodge(), show.legend = F)+theme_minimal()+ xlab(NULL) + ylab("Relatedness") +
  facet_wrap(~Indicator, ncol = 4) +
  scale_fill_brewer(palette="Paired") + theme_classic() +
  ggtitle("Countries Relatedness in the considered IPC fields") + 
  scale_y_continuous(limits=c(.45,1.5),oob = rescale_none)

Rel_byAI_c<- ggplot(Relatedness_AI2, aes(x=Country, y=Value, fill=Period)) +
  geom_bar(stat="identity", position=position_dodge(), show.legend = F)+theme_minimal() + labs(x = "") +
  scale_x_discrete(labels = NULL) + ylab("Relatedness") +
  facet_wrap(~Indicator, ncol = 4) +
  scale_fill_brewer(palette="Paired") + theme_classic() +
  ggtitle("AI Relatedness in the considered IPC fields")+
  scale_y_continuous(limits=c(.1,2.35),oob = rescale_none)

tiff("Figures_IPC/Relatedness_opta.jpg", width = 8, height = 6, units = 'in', res = 200)
multiplot(Rel_byAI_a, Rel_byP_a, cols=1)
dev.off()

tiff("Figures_IPC/Relatedness_optb.jpg", width = 8, height = 6, units = 'in', res = 200)
multiplot(Rel_byAI_b, Rel_byP_b, cols=1)
dev.off()
#Change the order of items in the legend: + scale_x_discrete(limits=c("D2", "D0.5", "D1")) (being D2 the name of an
#indicator for example)

#5.3.Knowledge Complexity ----
KnowlComp_1st <- read.csv("Data_calculations_IPC/KnowledgeComp_PerCountry_1st_All_RCAs.csv", sep = ";", header = TRUE, dec=",")
KnowlComp_2nd <- read.csv("Data_calculations_IPC/KnowledgeComp_PerCountry_2nd_All_RCAs.csv", sep = ";", header = TRUE, dec=",")
KnowlComp_3rd <- read.csv("Data_calculations_IPC/KnowledgeComp_PerCountry_3rd_All_RCAs.csv", sep = ";", header = TRUE, dec=",")

KnowlComp_1st$Period <- "Period 1 (1974-1988)"
KnowlComp_2nd$Period <- "Period 2 (1989-2003)"
KnowlComp_3rd$Period <- "Period 3 (2004-2018)"
KnowledgeCompl <- rbind(KnowlComp_1st, KnowlComp_2nd, KnowlComp_3rd)

KnowledgeCompl <- KnowledgeCompl[KnowledgeCompl$X == "US3" | 
                                   KnowledgeCompl$X == "CN3"| 
                                   KnowledgeCompl$X == "KR3"| 
                                   KnowledgeCompl$X == "JP3", ]

KnowledgeCompl$"AI-core codes" = rowSums(KnowledgeCompl[,c("X6", "X7", "X10", "X12")])
KnowledgeCompl$"Overall Complexity" = rowSums(KnowledgeCompl[,c(2:36)])
KnowledgeCompl$"AI-related codes" = rowSums(KnowledgeCompl[,c("X11", "X4", "X5")])
KnowledgeCompl$"Surrounding codes" = rowSums(KnowledgeCompl[,c("X3", "X2", "X1", "X13", "X25", "X34")])

KnowledgeCompl2<- KnowledgeCompl[,c(1, 38, 39)]
names(KnowledgeCompl2) <- c("Country", "Period", "Value")
KnowledgeCompl2$Indicator <- "AI-core codes"

KnowledgeCompl3<- KnowledgeCompl[,c(1, 38, 40)]
names(KnowledgeCompl3) <- c("Country", "Period", "Value")
KnowledgeCompl3$Indicator <- "Overall Complexity"

KnowledgeCompl4<- KnowledgeCompl[,c(1, 38, 41)]
names(KnowledgeCompl4) <- c("Country", "Period", "Value")
KnowledgeCompl4$Indicator <- "AI-related codes"

KnowledgeCompl5<- KnowledgeCompl[,c(1, 38, 42)]
names(KnowledgeCompl5) <- c("Country", "Period", "Value")
KnowledgeCompl5$Indicator <- "Surrounding codes"

KnowledgeCompl_all <- rbind(KnowledgeCompl2, KnowledgeCompl3, KnowledgeCompl4, KnowledgeCompl5)
KnowledgeCompl_all$Country <- gsub("CN3", "CN", str_trim(KnowledgeCompl_all$Country))
KnowledgeCompl_all$Country <- gsub("JP3", "JP", str_trim(KnowledgeCompl_all$Country))
KnowledgeCompl_all$Country <- gsub("US3", "US", str_trim(KnowledgeCompl_all$Country))
KnowledgeCompl_all$Country <- gsub("KR3", "KR", str_trim(KnowledgeCompl_all$Country))

KnowlComp_1st_AI <- read.csv("Data_calculations_IPC/KnowledgeComp_PerCountry_1st_All_RCAs_AI.csv", sep = ";", header = TRUE, dec=",")
KnowlComp_2nd_AI <- read.csv("Data_calculations_IPC/KnowledgeComp_PerCountry_2nd_All_RCAs_AI.csv", sep = ";", header = TRUE, dec=",")
KnowlComp_3rd_AI <- read.csv("Data_calculations_IPC/KnowledgeComp_PerCountry_3rd_All_RCAs_AI.csv", sep = ";", header = TRUE, dec=",")

KnowlComp_1st_AI$Period <- "Period 1 (1974-1988)"
KnowlComp_2nd_AI$Period <- "Period 2 (1989-2003)"
KnowlComp_3rd_AI$Period <- "Period 3 (2004-2018)"
KnowledgeCompl_AI <- rbind(KnowlComp_1st_AI, KnowlComp_2nd_AI, KnowlComp_3rd_AI)

KnowledgeCompl_AI <- KnowledgeCompl_AI[KnowledgeCompl_AI$X == "AI_pat3", ]
KnowledgeCompl_AI$"AI-core codes" = rowSums(KnowledgeCompl_AI[,c("X6", "X7", "X10", "X12")])
KnowledgeCompl_AI$"Overall Complexity" = rowSums(KnowledgeCompl_AI[,c(2:36)])
KnowledgeCompl_AI$"AI-related codes" = rowSums(KnowledgeCompl_AI[,c("X11", "X4", "X5")])
KnowledgeCompl_AI$"Surrounding codes" = rowSums(KnowledgeCompl_AI[,c("X3", "X2", "X1", "X13", "X25", "X34")])

KnowledgeCompl_AI2<- KnowledgeCompl_AI[,c(1, 38, 39)]
names(KnowledgeCompl_AI2) <- c("Country", "Period", "Value")
KnowledgeCompl_AI2$Indicator <- "AI-core codes"

KnowledgeCompl_AI3<- KnowledgeCompl_AI[,c(1, 38, 40)]
names(KnowledgeCompl_AI3) <- c("Country", "Period", "Value")
KnowledgeCompl_AI3$Indicator <- "Overall Complexity"

KnowledgeCompl_AI4<- KnowledgeCompl_AI[,c(1, 38, 41)]
names(KnowledgeCompl_AI4) <- c("Country", "Period", "Value")
KnowledgeCompl_AI4$Indicator <- "AI-related codes"

KnowledgeCompl_AI5<- KnowledgeCompl_AI[,c(1, 38, 42)]
names(KnowledgeCompl_AI5) <- c("Country", "Period", "Value")
KnowledgeCompl_AI5$Indicator <- "Surrounding codes"

KnowledgeCompl_AI_all <- rbind(KnowledgeCompl_AI2, KnowledgeCompl_AI3, KnowledgeCompl_AI4, KnowledgeCompl_AI5)
KnowledgeCompl_AI_all$Country <- gsub("AI_pat3", "AI", str_trim(KnowledgeCompl_AI_all$Country))

Comp_byP_a <- 
ggplot(KnowledgeCompl_all, aes(x=Country, y=(Value), fill=Indicator)) +
  geom_bar(stat="identity", position=position_dodge(), show.legend = F)+theme_minimal()+ xlab(NULL) + ylab("Knowledge Complexity") +
  facet_wrap(~Period, ncol = 3)+
  scale_fill_brewer(palette="Paired") + theme_classic() + 
  ggtitle("Countries Knowledge Complexity in the considered IPC fields")

Comp_byAI_a<- 
ggplot(KnowledgeCompl_AI_all, aes(x=Country, y=Value, fill=Indicator)) +
  geom_bar(stat="identity", position=position_dodge())+theme_minimal()+ labs(x = "") + 
  scale_x_discrete(labels = NULL) + ylab("Knowledge Complexity") +
  facet_wrap(~Period, ncol = 3)+
  scale_fill_brewer(palette="Paired") + theme_classic() + theme(legend.position="bottom") +
  ggtitle("AI Knowledge Complexity in the considered IPC fields") 

Comp_byP_b <- 
ggplot(KnowledgeCompl_all, aes(x=Country, y=Value, fill=Period)) +
  geom_bar(stat="identity", position=position_dodge(), show.legend = F)+theme_minimal()+ xlab(NULL) + ylab("Knowledge Complexity") +
  facet_wrap(~Indicator, ncol = 4) +
  scale_fill_brewer(palette="Paired") + theme_classic() +
  ggtitle("Countries Knowledge Complexity in the considered IPC fields") 

Comp_byAI_b<- 
ggplot(KnowledgeCompl_AI_all, aes(x=Country, y=Value, fill=Period)) +
  geom_bar(stat="identity", position=position_dodge())+theme_minimal() + labs(x = "") +
  scale_x_discrete(labels = NULL) + ylab("Knowledge Complexity") +
  facet_wrap(~Indicator, ncol = 4) +
  scale_fill_brewer(palette="Paired") + theme_classic() + theme(legend.position="bottom") +
  ggtitle("AI Knowledge Complexity in the considered IPC fields")

KnowledgeCompl_all2 <- KnowledgeCompl_all
KnowledgeCompl_all2$Indicator <- factor(KnowledgeCompl_all2$Indicator, levels = c("Overall Complexity", "AI-core codes",
                                                                                  "AI-related codes", "Surrounding codes"))

KnowledgeCompl_AI_all2 <- KnowledgeCompl_AI_all
KnowledgeCompl_AI_all2$Indicator <- factor(KnowledgeCompl_AI_all2$Indicator, levels = c("Overall Complexity", "AI-core codes",
                                                                                        "AI-related codes", "Surrounding codes"))

Comp_byP_c <-
  ggplot(KnowledgeCompl_all2, aes(x=Country, y=Value, fill=Period)) +
  geom_bar(stat="identity", position=position_dodge())+theme_minimal()+ xlab(NULL) + ylab("Knowledge Complexity") +
  facet_wrap(~Indicator, ncol = 4) +
  scale_fill_brewer(palette="Paired") + theme_classic()  + theme(legend.position="bottom") +
  ggtitle("Countries Knowledge Complexity in the considered IPC fields") 

Comp_byAI_c<- 
  ggplot(KnowledgeCompl_AI_all2, aes(x=Country, y=Value, fill=Period)) +
  geom_bar(stat="identity", position=position_dodge())+theme_minimal() + labs(x = "") +
  scale_x_discrete(labels = NULL) + ylab("Knowledge Complexity") +
  facet_wrap(~Indicator, ncol = 4) +
  scale_fill_brewer(palette="Paired") + theme_classic() + theme(legend.position="bottom") +
  ggtitle("AI Knowledge Complexity in the considered IPC fields")


tiff("Figures_IPC/KnowledgeComplexity_opta.jpg", width = 8, height = 6, units = 'in', res = 200)
multiplot(Comp_byAI_a, Comp_byP_a, cols=1)
dev.off()

tiff("Figures_IPC/KnowledgeComplexity_optb.jpg", width = 8, height = 6, units = 'in', res = 200)
multiplot(Comp_byAI_b, Comp_byP_b, cols=1) 
dev.off()


tiff("Figures_IPC/Relatedness_and_Complex_countries.jpg", width = 8, height = 6, units = 'in', res = 200)
multiplot(Rel_byP_c, Comp_byP_c, cols=1) 
dev.off()

tiff("Figures_IPC/Relatedness_and_Complex_AI.jpg", width = 8, height = 6, units = 'in', res = 200)
multiplot(Rel_byAI_c, Comp_byAI_c, cols=1) 
dev.off()

#facet_wrap(~year, nrow=1)
#theme_ipsum() +
#theme(legend.position="none") +
#scale_y_log10()

#library(ggthemes)
#Entropy_RCA_Countries_b <- ggplot(Indicators_countries, aes(x=X, y=Entropy_RCA, fill=Period)) +
#  geom_bar(stat="identity", position=position_dodge())+theme_minimal()+ xlab(NULL)+
#  theme_economist()+scale_colour_economist()

#6.Network Metrics----
#This first part is not ready yet (nor necessary). The important part starts again at line 3573
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
coords_tech_AI <- read.csv("Data_IPC/coords_tech_AI.csv", sep = ";", header = TRUE, dec=",")
names(coords_tech_AI) <- c("techn_field_nr", "x", "y")
coords_tech_AI$techn_field_nr <- as.character(coords_tech_AI$techn_field_nr)

#Now we read the data per period and calculate the RCAs:
reg_tech1 <- read.csv("Data_IPC/reg_tech_FirstPeriod.csv", sep = ";", header = TRUE, dec=",")
reg_tech2 <- read.csv("Data_IPC/reg_tech_SecondPeriod.csv", sep = ";", header = TRUE, dec=",")
reg_tech3 <- read.csv("Data_IPC/reg_tech_ThirdPeriod.csv", sep = ";", header = TRUE, dec=",")

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

###second period:
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

###Third Period:
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

reg_RCA1_CN <- reg_RCA1[,(2:3)][reg_RCA1$ctry_code == "CN",]
coords_tech_AI$CN_1st <- reg_RCA1_CN$RCA[match(reg_RCA1_CN$techn_field_nr, coords_tech_AI$techn_field_nr)]

reg_RCA2_CN <- reg_RCA2[,(2:3)][reg_RCA2$ctry_code == "CN",]
coords_tech_AI$CN_2nd <- reg_RCA2_CN$RCA[match(reg_RCA2_CN$techn_field_nr, coords_tech_AI$techn_field_nr)]

reg_RCA3_CN <- reg_RCA3[,(2:3)][reg_RCA3$ctry_code == "CN",]
coords_tech_AI$CN_3rd <- reg_RCA3_CN$RCA[match(reg_RCA3_CN$techn_field_nr, coords_tech_AI$techn_field_nr)]

reg_RCA1_JP <- reg_RCA1[,(2:3)][reg_RCA1$ctry_code == "JP",]
coords_tech_AI$JP_1st <- reg_RCA1_JP$RCA[match(reg_RCA1_JP$techn_field_nr, coords_tech_AI$techn_field_nr)]

reg_RCA2_JP <- reg_RCA2[,(2:3)][reg_RCA2$ctry_code == "JP",]
coords_tech_AI$JP_2nd <- reg_RCA2_JP$RCA[match(reg_RCA2_JP$techn_field_nr, coords_tech_AI$techn_field_nr)]

reg_RCA3_JP <- reg_RCA3[,(2:3)][reg_RCA3$ctry_code == "JP",]
coords_tech_AI$JP_3rd <- reg_RCA3_JP$RCA[match(reg_RCA3_JP$techn_field_nr, coords_tech_AI$techn_field_nr)]

reg_RCA1_US <- reg_RCA1[,(2:3)][reg_RCA1$ctry_code == "US",]
coords_tech_AI$US_1st <- reg_RCA1_US$RCA[match(reg_RCA1_US$techn_field_nr, coords_tech_AI$techn_field_nr)]

reg_RCA2_US <- reg_RCA2[,(2:3)][reg_RCA2$ctry_code == "US",]
coords_tech_AI$US_2nd <- reg_RCA2_US$RCA[match(reg_RCA2_US$techn_field_nr, coords_tech_AI$techn_field_nr)]

reg_RCA3_US <- reg_RCA3[,(2:3)][reg_RCA3$ctry_code == "US",]
coords_tech_AI$US_3rd <- reg_RCA3_US$RCA[match(reg_RCA3_US$techn_field_nr, coords_tech_AI$techn_field_nr)]

reg_RCA1_KR <- reg_RCA1[,(2:3)][reg_RCA1$ctry_code == "KR",]
coords_tech_AI$KR_1st <- reg_RCA1_KR$RCA[match(reg_RCA1_KR$techn_field_nr, coords_tech_AI$techn_field_nr)]

reg_RCA2_KR <- reg_RCA2[,(2:3)][reg_RCA2$ctry_code == "KR",]
coords_tech_AI$KR_2nd <- reg_RCA2_KR$RCA[match(reg_RCA2_KR$techn_field_nr, coords_tech_AI$techn_field_nr)]

reg_RCA3_KR <- reg_RCA3[,(2:3)][reg_RCA3$ctry_code == "KR",]
coords_tech_AI$KR_3rd <- reg_RCA3_KR$RCA[match(reg_RCA3_KR$techn_field_nr, coords_tech_AI$techn_field_nr)]

x_6 <- coords_tech_AI[,(2:3)][coords_tech_AI$techn_field_nr == "6",]
x_cluster <- coords_tech_AI[,(2:3)][coords_tech_AI$techn_field_nr == "6"|
                                      coords_tech_AI$techn_field_nr == "7"|
                                      coords_tech_AI$techn_field_nr == "10"|
                                      coords_tech_AI$techn_field_nr == "11"|
                                      coords_tech_AI$techn_field_nr == "4"|
                                      coords_tech_AI$techn_field_nr == "5"|
                                      coords_tech_AI$techn_field_nr == "12",]
coords_tech_AI$distance_6 <- sqrt((coords_tech_AI$x-x_6[[1]])^2+(coords_tech_AI$y-x_6[[2]])^2)
coords_tech_AI$distance_cluster <- sqrt((coords_tech_AI$x-mean(x_cluster[[1]]))^2+(coords_tech_AI$y-mean(x_cluster[[2]]))^2)
coords_tech_AI$CN_d6_1st <- coords_tech_AI$distance_6*coords_tech_AI$CN_1st
coords_tech_AI$CN_d6_2nd <- coords_tech_AI$distance_6*coords_tech_AI$CN_2nd
coords_tech_AI$CN_d6_3rd <- coords_tech_AI$distance_6*coords_tech_AI$CN_3rd
coords_tech_AI$JP_d6_1st <- coords_tech_AI$distance_6*coords_tech_AI$JP_1st
coords_tech_AI$JP_d6_2nd <- coords_tech_AI$distance_6*coords_tech_AI$JP_2nd
coords_tech_AI$JP_d6_3rd <- coords_tech_AI$distance_6*coords_tech_AI$JP_3rd
coords_tech_AI$US_d6_1st <- coords_tech_AI$distance_6*coords_tech_AI$US_1st
coords_tech_AI$US_d6_2nd <- coords_tech_AI$distance_6*coords_tech_AI$US_2nd
coords_tech_AI$US_d6_3rd <- coords_tech_AI$distance_6*coords_tech_AI$US_3rd
coords_tech_AI$KR_d6_1st <- coords_tech_AI$distance_6*coords_tech_AI$KR_1st
coords_tech_AI$KR_d6_2nd <- coords_tech_AI$distance_6*coords_tech_AI$KR_2nd
coords_tech_AI$KR_d6_3rd <- coords_tech_AI$distance_6*coords_tech_AI$KR_3rd

coords_tech_AI$CN_dcl_1st <- coords_tech_AI$distance_cluster*coords_tech_AI$CN_1st
coords_tech_AI$CN_dcl_2nd <- coords_tech_AI$distance_cluster*coords_tech_AI$CN_2nd
coords_tech_AI$CN_dcl_3rd <- coords_tech_AI$distance_cluster*coords_tech_AI$CN_3rd
coords_tech_AI$JP_dcl_1st <- coords_tech_AI$distance_cluster*coords_tech_AI$JP_1st
coords_tech_AI$JP_dcl_2nd <- coords_tech_AI$distance_cluster*coords_tech_AI$JP_2nd
coords_tech_AI$JP_dcl_3rd <- coords_tech_AI$distance_cluster*coords_tech_AI$JP_3rd
coords_tech_AI$US_dcl_1st <- coords_tech_AI$distance_cluster*coords_tech_AI$US_1st
coords_tech_AI$US_dcl_2nd <- coords_tech_AI$distance_cluster*coords_tech_AI$US_2nd
coords_tech_AI$US_dcl_3rd <- coords_tech_AI$distance_cluster*coords_tech_AI$US_3rd
coords_tech_AI$KR_dcl_1st <- coords_tech_AI$distance_cluster*coords_tech_AI$KR_1st
coords_tech_AI$KR_dcl_2nd <- coords_tech_AI$distance_cluster*coords_tech_AI$KR_2nd
coords_tech_AI$KR_dcl_3rd <- coords_tech_AI$distance_cluster*coords_tech_AI$KR_3rd

write.csv2(coords_tech_AI, file = "Data_calculations_IPC/coords_tech_AI_metrics.csv", row.names = F)
SummaryData <- as.data.frame(colMeans(coords_tech_AI[,c(18:41)], na.rm=TRUE))
SummaryData

Summary_NetworkMetrics <- read.csv("Data_calculations_IPC/Summary_NetworkMetrics.csv", sep = ";", header = TRUE, dec=",")
names(Summary_NetworkMetrics)[names(Summary_NetworkMetrics) == 'Group.2'] <- 'Period'
Summary_NetworkMetricsAvr <- Summary_NetworkMetrics[c(1:12),]
Summary_NetworkMetricsSum <- Summary_NetworkMetrics[c(13:24),]

Indicators <- c("dgr",
                "n_neighbors",
                "weighted_degree",
                "triangles",
                "centrality_authority",
                "centrality_betweenness",
                "centrality_closeness",
                "centrality_eigen",
                "centrality_hub",
                "centrality_subgraph",
                "centrality_degree",
                "centrality_closeness_harmonic",
                "centrality_closeness_residual",
                "centrality_betweenness_network")

dgr1 <- ggplot(Summary_NetworkMetricsAvr, aes(x=Group.1, y=dgr, fill=Period)) +
  geom_bar(stat="identity", position=position_dodge(), show.legend = F)+theme_minimal() + labs(x = "") +
  scale_fill_brewer(palette="Paired") + theme_classic() + theme(legend.position="bottom") +
  ggtitle("Average - Dgr")

dgr2 <- ggplot(Summary_NetworkMetricsSum, aes(x=Group.1, y=dgr, fill=Period)) +
  geom_bar(stat="identity", position=position_dodge(), show.legend = F)+theme_minimal() + labs(x = "") +
  scale_fill_brewer(palette="Paired") + theme_classic() + theme(legend.position="bottom") +
  ggtitle("Sum - Dgr")

n_neighbors1<- ggplot(Summary_NetworkMetricsAvr, aes(x=Group.1, y=n_neighbors, fill=Period)) +
  geom_bar(stat="identity", position=position_dodge(), show.legend = F)+theme_minimal() + labs(x = "") +
  scale_fill_brewer(palette="Paired") + theme_classic() + theme(legend.position="bottom") +
  ggtitle("Average - n_neighbors")

n_neighbors2<- ggplot(Summary_NetworkMetricsSum, aes(x=Group.1, y=n_neighbors, fill=Period)) +
  geom_bar(stat="identity", position=position_dodge(), show.legend = F)+theme_minimal() + labs(x = "") +
  scale_fill_brewer(palette="Paired") + theme_classic() + theme(legend.position="bottom") +
  ggtitle("Sum - n_neighbors")

ggplot(Summary_NetworkMetricsAvr, aes(x=Group.1, y=weighted_degree, fill=Period)) +
  geom_bar(stat="identity", position=position_dodge(), show.legend = F)+theme_minimal() + labs(x = "") +
  scale_fill_brewer(palette="Paired") + theme_classic() + theme(legend.position="bottom") +
  ggtitle("Average - weighted_degree")

ggplot(Summary_NetworkMetricsSum, aes(x=Group.1, y=weighted_degree, fill=Period)) +
  geom_bar(stat="identity", position=position_dodge(), show.legend = F)+theme_minimal() + labs(x = "") +
  scale_fill_brewer(palette="Paired") + theme_classic() + theme(legend.position="bottom") +
  ggtitle("Sum - weighted_degree")

triangles1<- ggplot(Summary_NetworkMetricsAvr, aes(x=Group.1, y=triangles, fill=Period)) +
  geom_bar(stat="identity", position=position_dodge(), show.legend = F)+theme_minimal() + labs(x = "") +
  # scale_x_discrete(labels = NULL) + ylab("Knowledge Complexity") +
  #  facet_wrap(~Indicator, ncol = 4) +
  scale_fill_brewer(palette="Paired") + theme_classic() + theme(legend.position="bottom") +
  ggtitle("Average - triangles")

triangles2<- ggplot(Summary_NetworkMetricsSum, aes(x=Group.1, y=triangles, fill=Period)) +
  geom_bar(stat="identity", position=position_dodge(), show.legend = F)+theme_minimal() + labs(x = "") +
  scale_fill_brewer(palette="Paired") + theme_classic() + theme(legend.position="bottom") +
  ggtitle("Sum - triangles")

centrality_authority1<- ggplot(Summary_NetworkMetricsAvr, aes(x=Group.1, y=centrality_authority, fill=Period)) +
  geom_bar(stat="identity", position=position_dodge(), show.legend = F)+theme_minimal() + labs(x = "") +
  scale_fill_brewer(palette="Paired") + theme_classic() + theme(legend.position="bottom") +
  ggtitle("Average - centrality_authority")

centrality_authority2<- ggplot(Summary_NetworkMetricsSum, aes(x=Group.1, y=centrality_authority, fill=Period)) +
  geom_bar(stat="identity", position=position_dodge(), show.legend = F)+theme_minimal() + labs(x = "") +
  scale_fill_brewer(palette="Paired") + theme_classic() + theme(legend.position="bottom") +
  ggtitle("Sum - centrality_authority")

centrality_betweenness1<-ggplot(Summary_NetworkMetricsAvr, aes(x=Group.1, y=centrality_betweenness, fill=Period)) +
  geom_bar(stat="identity", position=position_dodge(), show.legend = F)+theme_minimal() + labs(x = "") +
  scale_fill_brewer(palette="Paired") + theme_classic() + theme(legend.position="bottom") +
  ggtitle("Average - centrality_betweenness")+ 
  scale_y_continuous(limits=c(2,7.2),oob = rescale_none)

centrality_betweenness2<-ggplot(Summary_NetworkMetricsSum, aes(x=Group.1, y=centrality_betweenness, fill=Period)) +
  geom_bar(stat="identity", position=position_dodge(), show.legend = F)+theme_minimal() + labs(x = "") +
  scale_fill_brewer(palette="Paired") + theme_classic() + theme(legend.position="bottom") +
  ggtitle("Sum - centrality_betweenness")

centrality_closeness1<-ggplot(Summary_NetworkMetricsAvr, aes(x=Group.1, y=centrality_closeness, fill=Period)) +
  geom_bar(stat="identity", position=position_dodge(), show.legend = F)+theme_minimal() + labs(x = "") +
  scale_fill_brewer(palette="Paired") + theme_classic() + theme(legend.position="bottom") +
  ggtitle("Average - centrality_closeness") + 
  scale_y_continuous(limits=c(.02,.055),oob = rescale_none)

centrality_closeness2<-ggplot(Summary_NetworkMetricsSum, aes(x=Group.1, y=centrality_closeness, fill=Period)) +
  geom_bar(stat="identity", position=position_dodge(), show.legend = F)+theme_minimal() + labs(x = "") +
  scale_fill_brewer(palette="Paired") + theme_classic() + theme(legend.position="bottom") +
  ggtitle("Sum - centrality_closeness")

centrality_eigen1<-ggplot(Summary_NetworkMetricsAvr, aes(x=Group.1, y=centrality_eigen, fill=Period)) +
  geom_bar(stat="identity", position=position_dodge())+theme_minimal() + labs(x = "") +
  scale_fill_brewer(palette="Paired") + theme_classic() + theme(legend.position="bottom") +
  ggtitle("Average - centrality_eigen")+ 
  scale_y_continuous(limits=c(.45,.67),oob = rescale_none)

centrality_eigen2<-ggplot(Summary_NetworkMetricsSum, aes(x=Group.1, y=centrality_eigen, fill=Period)) +
  geom_bar(stat="identity", position=position_dodge(), show.legend = F)+theme_minimal() + labs(x = "") +
  scale_fill_brewer(palette="Paired") + theme_classic() + theme(legend.position="bottom") +
  ggtitle("Sum - centrality_eigen")

centrality_hub1<-ggplot(Summary_NetworkMetricsAvr, aes(x=Group.1, y=centrality_hub, fill=Period)) +
  geom_bar(stat="identity", position=position_dodge(), show.legend = F)+theme_minimal() + labs(x = "") +
  scale_fill_brewer(palette="Paired") + theme_classic() + theme(legend.position="bottom") +
  ggtitle("Average - centrality_hub") + 
  scale_y_continuous(limits=c(.45,.7),oob = rescale_none)

centrality_hub2<-ggplot(Summary_NetworkMetricsSum, aes(x=Group.1, y=centrality_hub, fill=Period)) +
  geom_bar(stat="identity", position=position_dodge(), show.legend = F)+theme_minimal() + labs(x = "") +
  scale_fill_brewer(palette="Paired") + theme_classic() + theme(legend.position="bottom") +
  ggtitle("Sum - centrality_hub")

centrality_subgraph1<-ggplot(Summary_NetworkMetricsAvr, aes(x=Group.1, y=centrality_subgraph, fill=Period)) +
  geom_bar(stat="identity", position=position_dodge(), show.legend = F)+theme_minimal() + labs(x = "") +
  scale_fill_brewer(palette="Paired") + theme_classic() + theme(legend.position="bottom") +
  ggtitle("Average - centrality_subgraph")

centrality_subgraph2<-ggplot(Summary_NetworkMetricsSum, aes(x=Group.1, y=centrality_subgraph, fill=Period)) +
  geom_bar(stat="identity", position=position_dodge(), show.legend = F)+theme_minimal() + labs(x = "") +
  scale_fill_brewer(palette="Paired") + theme_classic() + theme(legend.position="bottom") +
  ggtitle("Sum - centrality_subgraph")

centrality_degree1<-ggplot(Summary_NetworkMetricsAvr, aes(x=Group.1, y=centrality_degree, fill=Period)) +
  geom_bar(stat="identity", position=position_dodge(), show.legend = F)+theme_minimal() + labs(x = "") +
  scale_fill_brewer(palette="Paired") + theme_classic() + theme(legend.position="bottom") +
  ggtitle("Average - centrality_degree")

centrality_degree2<-ggplot(Summary_NetworkMetricsSum, aes(x=Group.1, y=centrality_degree, fill=Period)) +
  geom_bar(stat="identity", position=position_dodge(), show.legend = F)+theme_minimal() + labs(x = "") +
  scale_fill_brewer(palette="Paired") + theme_classic() + theme(legend.position="bottom") +
  ggtitle("Sum - centrality_degree")

centrality_closeness_harmonic1<-ggplot(Summary_NetworkMetricsAvr, aes(x=Group.1, y=centrality_closeness_harmonic, fill=Period)) +
  geom_bar(stat="identity", position=position_dodge(), show.legend = F)+theme_minimal() + labs(x = "") +
  scale_fill_brewer(palette="Paired") + theme_classic() + theme(legend.position="bottom") +
  ggtitle("Average - centrality_closeness_harmonic")

centrality_closeness_harmonic2<-ggplot(Summary_NetworkMetricsSum, aes(x=Group.1, y=centrality_closeness_harmonic, fill=Period)) +
  geom_bar(stat="identity", position=position_dodge(), show.legend = F)+theme_minimal() + labs(x = "") +
  scale_fill_brewer(palette="Paired") + theme_classic() + theme(legend.position="bottom") +
  ggtitle("Sum - centrality_closeness_harmonic")

centrality_closeness_residual1<-ggplot(Summary_NetworkMetricsAvr, aes(x=Group.1, y=centrality_closeness_residual, fill=Period)) +
  geom_bar(stat="identity", position=position_dodge(), show.legend = F)+theme_minimal() + labs(x = "") +
  scale_fill_brewer(palette="Paired") + theme_classic() + theme(legend.position="bottom") +
  ggtitle("Average - centrality_closeness_residual")

centrality_closeness_residual2<-ggplot(Summary_NetworkMetricsSum, aes(x=Group.1, y=centrality_closeness_residual, fill=Period)) +
  geom_bar(stat="identity", position=position_dodge(), show.legend = F)+theme_minimal() + labs(x = "") +
  scale_fill_brewer(palette="Paired") + theme_classic() + theme(legend.position="bottom") +
  ggtitle("Sum - centrality_closeness_residual")

centrality_betweenness_network1<- ggplot(Summary_NetworkMetricsAvr, aes(x=Group.1, y=centrality_betweenness_network, fill=Period)) +
  geom_bar(stat="identity", position=position_dodge(), show.legend = F)+theme_minimal() + labs(x = "") +
  scale_fill_brewer(palette="Paired") + theme_classic() + theme(legend.position="bottom") +
  ggtitle("Average - centrality_betweenness_network")

centrality_betweenness_network2<- ggplot(Summary_NetworkMetricsSum, aes(x=Group.1, y=centrality_betweenness_network, fill=Period)) +
  geom_bar(stat="identity", position=position_dodge(), show.legend = F)+theme_minimal() + labs(x = "") +
  scale_fill_brewer(palette="Paired") + theme_classic() + theme(legend.position="bottom") +
  ggtitle("Sum - centrality_betweenness_network")

Indicators

tiff("Figures_IPC/NetworkMetrics_1_to_5.jpg", width = 8, height = 10, units = 'in', res = 200)
multiplot(dgr1, n_neighbors1, triangles1, centrality_authority1, centrality_closeness1,
          dgr2, n_neighbors2, triangles2, centrality_authority2, centrality_closeness2, cols=2)
dev.off()

tiff("Figures_IPC/NetworkMetrics_6_to_9.jpg", width = 8, height = 10, units = 'in', res = 200)
multiplot(centrality_betweenness1, centrality_subgraph1, centrality_degree1, centrality_eigen1,
          centrality_betweenness2, centrality_subgraph2, centrality_degree2, centrality_eigen2, cols=2)
dev.off()

tiff("Figures_IPC/NetworkMetrics_10_to_13.jpg", width = 8, height = 10, units = 'in', res = 200)
multiplot(centrality_closeness_harmonic1, centrality_closeness_residual1, centrality_betweenness_network1, centrality_hub1,
          centrality_closeness_harmonic2, centrality_closeness_residual2, centrality_betweenness_network2, centrality_hub2, cols=2)
dev.off()

tiff("Figures_IPC/NetworkMetrics_Best_ones.jpg", width = 8, height = 10, units = 'in', res = 200)
multiplot(centrality_closeness1, centrality_betweenness1, centrality_eigen1, cols=1)
dev.off()

#Best ones: centrality_closeness, centrality_eigen, centrality_hub, centrality_betweenness_network