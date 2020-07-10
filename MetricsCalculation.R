library(tidyverse) # Collection of all the good stuff like dplyr, ggplot2 ect.
library(magrittr) # For extra-piping operators (eg. %<>%)

# Network specific
#library(tidygraph) # For tidy-style graph manipulation
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

write.csv2(Indicators, file = "Data_calculations/Indicators_1st_period_IPC.csv", row.names = TRUE)
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

write.csv2(KnowledgeComp_1st, file = "Data_calculations/KnowledgeComp_1st.csv", row.names = TRUE)
write.csv2(KnowledgeComp_PerCountry_1st_All, file = "Data_calculations/KnowledgeComp_PerCountry_1st_All.csv", row.names = TRUE)
write.csv2(KnowledgeComp_PerCountry_1st_All_RCAs, file = "Data_calculations/KnowledgeComp_PerCountry_1st_All_RCAs.csv", row.names = TRUE)

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

#then select only the top 10 areas:
IPC_all_patents_1st_US_Top10 <- IPC_all_patents_1st_US[IPC_all_patents_1st_US$techn_field_nr == "6" | 
                                                         IPC_all_patents_1st_US$techn_field_nr == "7"| 
                                                         IPC_all_patents_1st_US$techn_field_nr == "10"| 
                                                         IPC_all_patents_1st_US$techn_field_nr == "12"|
                                                         IPC_all_patents_1st_US$techn_field_nr == "4"| 
                                                         IPC_all_patents_1st_US$techn_field_nr == "5"| 
                                                         IPC_all_patents_1st_US$techn_field_nr == "11"|
                                                         IPC_all_patents_1st_US$techn_field_nr == "3"| 
                                                         IPC_all_patents_1st_US$techn_field_nr == "2"| 
                                                         IPC_all_patents_1st_US$techn_field_nr == "9", ]

mat_tech_1st_US_Top10 <- create_sparse_matrix(i = IPC_all_patents_1st_US_Top10 %>% pull(appln_id),
                                              j = IPC_all_patents_1st_US_Top10 %>% pull(techn_field_nr))

mat_tech_1st_US_Top10 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_1st_US_Top10_rel_jacc <- relatedness(mat_tech_1st_US_Top10, method = "Jaccard")
mat_tech_1st_US_Top10_rel_asso <- relatedness(mat_tech_1st_US_Top10, method = "association")
mat_tech_1st_US_Top10_rel_cosi <- relatedness(mat_tech_1st_US_Top10, method = "cosine")

Relatedness$Jaccard_Top10 <- mean(mat_tech_1st_US_Top10_rel_jacc)
Relatedness$Association_Top10 <- mean(mat_tech_1st_US_Top10_rel_asso)
Relatedness$Cosine_Top10<- mean(mat_tech_1st_US_Top10_rel_cosi)

#and finally the top 6 non-AI areas:
IPC_all_patents_1st_US_Top6 <- IPC_all_patents_1st_US[IPC_all_patents_1st_US$techn_field_nr == "4"| 
                                                        IPC_all_patents_1st_US$techn_field_nr == "5"| 
                                                        IPC_all_patents_1st_US$techn_field_nr == "11"|
                                                        IPC_all_patents_1st_US$techn_field_nr == "3"| 
                                                        IPC_all_patents_1st_US$techn_field_nr == "2"| 
                                                        IPC_all_patents_1st_US$techn_field_nr == "9", ]

mat_tech_1st_US_Top6 <- create_sparse_matrix(i = IPC_all_patents_1st_US_Top6 %>% pull(appln_id),
                                             j = IPC_all_patents_1st_US_Top6 %>% pull(techn_field_nr))

mat_tech_1st_US_Top6 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_1st_US_Top6_rel_jacc <- relatedness(mat_tech_1st_US_Top6, method = "Jaccard")
mat_tech_1st_US_Top6_rel_asso <- relatedness(mat_tech_1st_US_Top6, method = "association")
mat_tech_1st_US_Top6_rel_cosi <- relatedness(mat_tech_1st_US_Top6, method = "cosine")

Relatedness$Jaccard_Top6 <- mean(mat_tech_1st_US_Top6_rel_jacc)
Relatedness$Association_Top6 <- mean(mat_tech_1st_US_Top6_rel_asso)
Relatedness$Cosine_Top6<- mean(mat_tech_1st_US_Top6_rel_cosi)

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

#then select only the top 10 areas:
IPC_all_patents_1st_CN_Top10 <- IPC_all_patents_1st_CN[IPC_all_patents_1st_CN$techn_field_nr == "6" | 
                                                         IPC_all_patents_1st_CN$techn_field_nr == "7"| 
                                                         IPC_all_patents_1st_CN$techn_field_nr == "10"| 
                                                         IPC_all_patents_1st_CN$techn_field_nr == "12"|
                                                         IPC_all_patents_1st_CN$techn_field_nr == "4"| 
                                                         IPC_all_patents_1st_CN$techn_field_nr == "5"| 
                                                         IPC_all_patents_1st_CN$techn_field_nr == "11"|
                                                         IPC_all_patents_1st_CN$techn_field_nr == "3"| 
                                                         IPC_all_patents_1st_CN$techn_field_nr == "2"| 
                                                         IPC_all_patents_1st_CN$techn_field_nr == "9", ]

mat_tech_1st_CN_Top10 <- create_sparse_matrix(i = IPC_all_patents_1st_CN_Top10 %>% pull(appln_id),
                                              j = IPC_all_patents_1st_CN_Top10 %>% pull(techn_field_nr))

mat_tech_1st_CN_Top10 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_1st_CN_Top10_rel_jacc <- relatedness(mat_tech_1st_CN_Top10, method = "Jaccard")
mat_tech_1st_CN_Top10_rel_asso <- relatedness(mat_tech_1st_CN_Top10, method = "association")
mat_tech_1st_CN_Top10_rel_cosi <- relatedness(mat_tech_1st_CN_Top10, method = "cosine")

Relatedness_CN$Jaccard_Top10 <- mean(mat_tech_1st_CN_Top10_rel_jacc)
Relatedness_CN$Association_Top10 <- mean(mat_tech_1st_CN_Top10_rel_asso)
Relatedness_CN$Cosine_Top10<- mean(mat_tech_1st_CN_Top10_rel_cosi)

#and finally the top 6 non-AI areas:
IPC_all_patents_1st_CN_Top6 <- IPC_all_patents_1st_CN[IPC_all_patents_1st_CN$techn_field_nr == "4"| 
                                                        IPC_all_patents_1st_CN$techn_field_nr == "5"| 
                                                        IPC_all_patents_1st_CN$techn_field_nr == "11"|
                                                        IPC_all_patents_1st_CN$techn_field_nr == "3"| 
                                                        IPC_all_patents_1st_CN$techn_field_nr == "2"| 
                                                        IPC_all_patents_1st_CN$techn_field_nr == "9", ]

mat_tech_1st_CN_Top6 <- create_sparse_matrix(i = IPC_all_patents_1st_CN_Top6 %>% pull(appln_id),
                                             j = IPC_all_patents_1st_CN_Top6 %>% pull(techn_field_nr))

mat_tech_1st_CN_Top6 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_1st_CN_Top6_rel_jacc <- relatedness(mat_tech_1st_CN_Top6, method = "Jaccard")
mat_tech_1st_CN_Top6_rel_asso <- relatedness(mat_tech_1st_CN_Top6, method = "association")
mat_tech_1st_CN_Top6_rel_cosi <- relatedness(mat_tech_1st_CN_Top6, method = "cosine")

Relatedness_CN$Jaccard_Top6 <- mean(mat_tech_1st_CN_Top6_rel_jacc)
Relatedness_CN$Association_Top6 <- mean(mat_tech_1st_CN_Top6_rel_asso)
Relatedness_CN$Cosine_Top6<- mean(mat_tech_1st_CN_Top6_rel_cosi)


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

#then select only the top 10 areas:
IPC_all_patents_1st_KR_Top10 <- IPC_all_patents_1st_KR[IPC_all_patents_1st_KR$techn_field_nr == "6" | 
                                                         IPC_all_patents_1st_KR$techn_field_nr == "7"| 
                                                         IPC_all_patents_1st_KR$techn_field_nr == "10"| 
                                                         IPC_all_patents_1st_KR$techn_field_nr == "12"|
                                                         IPC_all_patents_1st_KR$techn_field_nr == "4"| 
                                                         IPC_all_patents_1st_KR$techn_field_nr == "5"| 
                                                         IPC_all_patents_1st_KR$techn_field_nr == "11"|
                                                         IPC_all_patents_1st_KR$techn_field_nr == "3"| 
                                                         IPC_all_patents_1st_KR$techn_field_nr == "2"| 
                                                         IPC_all_patents_1st_KR$techn_field_nr == "9", ]

mat_tech_1st_KR_Top10 <- create_sparse_matrix(i = IPC_all_patents_1st_KR_Top10 %>% pull(appln_id),
                                              j = IPC_all_patents_1st_KR_Top10 %>% pull(techn_field_nr))

mat_tech_1st_KR_Top10 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_1st_KR_Top10_rel_jacc <- relatedness(mat_tech_1st_KR_Top10, method = "Jaccard")
mat_tech_1st_KR_Top10_rel_asso <- relatedness(mat_tech_1st_KR_Top10, method = "association")
mat_tech_1st_KR_Top10_rel_cosi <- relatedness(mat_tech_1st_KR_Top10, method = "cosine")

Relatedness_KR$Jaccard_Top10 <- mean(mat_tech_1st_KR_Top10_rel_jacc)
Relatedness_KR$Association_Top10 <- mean(mat_tech_1st_KR_Top10_rel_asso)
Relatedness_KR$Cosine_Top10<- mean(mat_tech_1st_KR_Top10_rel_cosi)

#and finally the top 6 non-AI areas:
IPC_all_patents_1st_KR_Top6 <- IPC_all_patents_1st_KR[IPC_all_patents_1st_KR$techn_field_nr == "4"| 
                                                        IPC_all_patents_1st_KR$techn_field_nr == "5"| 
                                                        IPC_all_patents_1st_KR$techn_field_nr == "11"|
                                                        IPC_all_patents_1st_KR$techn_field_nr == "3"| 
                                                        IPC_all_patents_1st_KR$techn_field_nr == "2"| 
                                                        IPC_all_patents_1st_KR$techn_field_nr == "9", ]

mat_tech_1st_KR_Top6 <- create_sparse_matrix(i = IPC_all_patents_1st_KR_Top6 %>% pull(appln_id),
                                             j = IPC_all_patents_1st_KR_Top6 %>% pull(techn_field_nr))

mat_tech_1st_KR_Top6 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_1st_KR_Top6_rel_jacc <- relatedness(mat_tech_1st_KR_Top6, method = "Jaccard")
mat_tech_1st_KR_Top6_rel_asso <- relatedness(mat_tech_1st_KR_Top6, method = "association")
mat_tech_1st_KR_Top6_rel_cosi <- relatedness(mat_tech_1st_KR_Top6, method = "cosine")

Relatedness_KR$Jaccard_Top6 <- mean(mat_tech_1st_KR_Top6_rel_jacc)
Relatedness_KR$Association_Top6 <- mean(mat_tech_1st_KR_Top6_rel_asso)
Relatedness_KR$Cosine_Top6<- mean(mat_tech_1st_KR_Top6_rel_cosi)

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

#then select only the top 10 areas:
IPC_all_patents_1st_JP_Top10 <- IPC_all_patents_1st_JP[IPC_all_patents_1st_JP$techn_field_nr == "6" | 
                                                         IPC_all_patents_1st_JP$techn_field_nr == "7"| 
                                                         IPC_all_patents_1st_JP$techn_field_nr == "10"| 
                                                         IPC_all_patents_1st_JP$techn_field_nr == "12"|
                                                         IPC_all_patents_1st_JP$techn_field_nr == "4"| 
                                                         IPC_all_patents_1st_JP$techn_field_nr == "5"| 
                                                         IPC_all_patents_1st_JP$techn_field_nr == "11"|
                                                         IPC_all_patents_1st_JP$techn_field_nr == "3"| 
                                                         IPC_all_patents_1st_JP$techn_field_nr == "2"| 
                                                         IPC_all_patents_1st_JP$techn_field_nr == "9", ]

mat_tech_1st_JP_Top10 <- create_sparse_matrix(i = IPC_all_patents_1st_JP_Top10 %>% pull(appln_id),
                                              j = IPC_all_patents_1st_JP_Top10 %>% pull(techn_field_nr))

mat_tech_1st_JP_Top10 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_1st_JP_Top10_rel_jacc <- relatedness(mat_tech_1st_JP_Top10, method = "Jaccard")
mat_tech_1st_JP_Top10_rel_asso <- relatedness(mat_tech_1st_JP_Top10, method = "association")
mat_tech_1st_JP_Top10_rel_cosi <- relatedness(mat_tech_1st_JP_Top10, method = "cosine")

Relatedness_JP$Jaccard_Top10 <- mean(mat_tech_1st_JP_Top10_rel_jacc)
Relatedness_JP$Association_Top10 <- mean(mat_tech_1st_JP_Top10_rel_asso)
Relatedness_JP$Cosine_Top10<- mean(mat_tech_1st_JP_Top10_rel_cosi)

#and finally the top 6 non-AI areas:
IPC_all_patents_1st_JP_Top6 <- IPC_all_patents_1st_JP[IPC_all_patents_1st_JP$techn_field_nr == "4"| 
                                                        IPC_all_patents_1st_JP$techn_field_nr == "5"| 
                                                        IPC_all_patents_1st_JP$techn_field_nr == "11"|
                                                        IPC_all_patents_1st_JP$techn_field_nr == "3"| 
                                                        IPC_all_patents_1st_JP$techn_field_nr == "2"| 
                                                        IPC_all_patents_1st_JP$techn_field_nr == "9", ]

mat_tech_1st_JP_Top6 <- create_sparse_matrix(i = IPC_all_patents_1st_JP_Top6 %>% pull(appln_id),
                                             j = IPC_all_patents_1st_JP_Top6 %>% pull(techn_field_nr))

mat_tech_1st_JP_Top6 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_1st_JP_Top6_rel_jacc <- relatedness(mat_tech_1st_JP_Top6, method = "Jaccard")
mat_tech_1st_JP_Top6_rel_asso <- relatedness(mat_tech_1st_JP_Top6, method = "association")
mat_tech_1st_JP_Top6_rel_cosi <- relatedness(mat_tech_1st_JP_Top6, method = "cosine")

Relatedness_JP$Jaccard_Top6 <- mean(mat_tech_1st_JP_Top6_rel_jacc)
Relatedness_JP$Association_Top6 <- mean(mat_tech_1st_JP_Top6_rel_asso)
Relatedness_JP$Cosine_Top6<- mean(mat_tech_1st_JP_Top6_rel_cosi)

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

#then select only the top 10 areas:
IPC_all_patents_1st_AI_Top10 <- IPC_all_patents_1st_AI[IPC_all_patents_1st_AI$techn_field_nr == "6" | 
                                                        IPC_all_patents_1st_AI$techn_field_nr == "7"| 
                                                        IPC_all_patents_1st_AI$techn_field_nr == "10"| 
                                                        IPC_all_patents_1st_AI$techn_field_nr == "12"|
                                                        IPC_all_patents_1st_AI$techn_field_nr == "4"| 
                                                        IPC_all_patents_1st_AI$techn_field_nr == "5"| 
                                                        IPC_all_patents_1st_AI$techn_field_nr == "11"|
                                                        IPC_all_patents_1st_AI$techn_field_nr == "3"| 
                                                        IPC_all_patents_1st_AI$techn_field_nr == "2"| 
                                                        IPC_all_patents_1st_AI$techn_field_nr == "9", ]

mat_tech_1st_AI_Top10 <- create_sparse_matrix(i = IPC_all_patents_1st_AI_Top10 %>% pull(appln_id),
                                             j = IPC_all_patents_1st_AI_Top10 %>% pull(techn_field_nr))

mat_tech_1st_AI_Top10 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_1st_AI_Top10_rel_jacc <- relatedness(mat_tech_1st_AI_Top10, method = "Jaccard")
mat_tech_1st_AI_Top10_rel_asso <- relatedness(mat_tech_1st_AI_Top10, method = "association")
mat_tech_1st_AI_Top10_rel_cosi <- relatedness(mat_tech_1st_AI_Top10, method = "cosine")

Relatedness_AI$Jaccard_Top10 <- mean(mat_tech_1st_AI_Top10_rel_jacc)
Relatedness_AI$Association_Top10 <- mean(mat_tech_1st_AI_Top10_rel_asso)
Relatedness_AI$Cosine_Top10<- mean(mat_tech_1st_AI_Top10_rel_cosi)

#and finally the top 6 non-AI areas:
IPC_all_patents_1st_AI_Top6 <- IPC_all_patents_1st_AI[IPC_all_patents_1st_AI$techn_field_nr == "4"| 
                                                         IPC_all_patents_1st_AI$techn_field_nr == "5"| 
                                                         IPC_all_patents_1st_AI$techn_field_nr == "11"|
                                                         IPC_all_patents_1st_AI$techn_field_nr == "3"| 
                                                         IPC_all_patents_1st_AI$techn_field_nr == "2"| 
                                                         IPC_all_patents_1st_AI$techn_field_nr == "9", ]

mat_tech_1st_AI_Top6 <- create_sparse_matrix(i = IPC_all_patents_1st_AI_Top6 %>% pull(appln_id),
                                              j = IPC_all_patents_1st_AI_Top6 %>% pull(techn_field_nr))

mat_tech_1st_AI_Top6 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_1st_AI_Top6_rel_jacc <- relatedness(mat_tech_1st_AI_Top6, method = "Jaccard")
mat_tech_1st_AI_Top6_rel_asso <- relatedness(mat_tech_1st_AI_Top6, method = "association")
mat_tech_1st_AI_Top6_rel_cosi <- relatedness(mat_tech_1st_AI_Top6, method = "cosine")

Relatedness_AI$Jaccard_Top6 <- mean(mat_tech_1st_AI_Top6_rel_jacc)
Relatedness_AI$Association_Top6 <- mean(mat_tech_1st_AI_Top6_rel_asso)
Relatedness_AI$Cosine_Top6<- mean(mat_tech_1st_AI_Top6_rel_cosi)

#and we merge it all together:
Relatedness_FirstPeriod <- rbind(Relatedness, Relatedness_CN, Relatedness_KR, Relatedness_JP, Relatedness_AI)
Relatedness_FirstPeriod <- Relatedness_FirstPeriod[,c((1:3), (5:13), (4))]

write.csv2(Relatedness_FirstPeriod, file = "Data_calculations/Relatedness_1st_period_IPC.csv", row.names = TRUE)

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

write.csv2(Indicators, file = "Data_calculations/Indicators_2nd_period_IPC.csv", row.names = TRUE)

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

write.csv2(KnowledgeComp_2nd, file = "Data_calculations/KnowledgeComp_2nd.csv", row.names = TRUE)
write.csv2(KnowledgeComp_PerCountry_2nd_All, file = "Data_calculations/KnowledgeComp_PerCountry_2nd_All.csv", row.names = TRUE)
write.csv2(KnowledgeComp_PerCountry_2nd_All_RCAs, file = "Data_calculations/KnowledgeComp_PerCountry_2nd_All_RCAs.csv", row.names = TRUE)

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

#then select only the top 10 areas:
IPC_all_patents_2nd_US_Top10 <- IPC_all_patents_2nd_US[IPC_all_patents_2nd_US$techn_field_nr == "6" | 
                                                         IPC_all_patents_2nd_US$techn_field_nr == "7"| 
                                                         IPC_all_patents_2nd_US$techn_field_nr == "10"| 
                                                         IPC_all_patents_2nd_US$techn_field_nr == "12"|
                                                         IPC_all_patents_2nd_US$techn_field_nr == "4"| 
                                                         IPC_all_patents_2nd_US$techn_field_nr == "5"| 
                                                         IPC_all_patents_2nd_US$techn_field_nr == "11"|
                                                         IPC_all_patents_2nd_US$techn_field_nr == "3"| 
                                                         IPC_all_patents_2nd_US$techn_field_nr == "2"| 
                                                         IPC_all_patents_2nd_US$techn_field_nr == "9", ]

mat_tech_2nd_US_Top10 <- create_sparse_matrix(i = IPC_all_patents_2nd_US_Top10 %>% pull(appln_id),
                                              j = IPC_all_patents_2nd_US_Top10 %>% pull(techn_field_nr))

mat_tech_2nd_US_Top10 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_2nd_US_Top10_rel_jacc <- relatedness(mat_tech_2nd_US_Top10, method = "Jaccard")
mat_tech_2nd_US_Top10_rel_asso <- relatedness(mat_tech_2nd_US_Top10, method = "association")
mat_tech_2nd_US_Top10_rel_cosi <- relatedness(mat_tech_2nd_US_Top10, method = "cosine")

Relatedness$Jaccard_Top10 <- mean(mat_tech_2nd_US_Top10_rel_jacc)
Relatedness$Association_Top10 <- mean(mat_tech_2nd_US_Top10_rel_asso)
Relatedness$Cosine_Top10<- mean(mat_tech_2nd_US_Top10_rel_cosi)

#and finally the top 6 non-AI areas:
IPC_all_patents_2nd_US_Top6 <- IPC_all_patents_2nd_US[IPC_all_patents_2nd_US$techn_field_nr == "4"| 
                                                        IPC_all_patents_2nd_US$techn_field_nr == "5"| 
                                                        IPC_all_patents_2nd_US$techn_field_nr == "11"|
                                                        IPC_all_patents_2nd_US$techn_field_nr == "3"| 
                                                        IPC_all_patents_2nd_US$techn_field_nr == "2"| 
                                                        IPC_all_patents_2nd_US$techn_field_nr == "9", ]

mat_tech_2nd_US_Top6 <- create_sparse_matrix(i = IPC_all_patents_2nd_US_Top6 %>% pull(appln_id),
                                             j = IPC_all_patents_2nd_US_Top6 %>% pull(techn_field_nr))

mat_tech_2nd_US_Top6 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_2nd_US_Top6_rel_jacc <- relatedness(mat_tech_2nd_US_Top6, method = "Jaccard")
mat_tech_2nd_US_Top6_rel_asso <- relatedness(mat_tech_2nd_US_Top6, method = "association")
mat_tech_2nd_US_Top6_rel_cosi <- relatedness(mat_tech_2nd_US_Top6, method = "cosine")

Relatedness$Jaccard_Top6 <- mean(mat_tech_2nd_US_Top6_rel_jacc)
Relatedness$Association_Top6 <- mean(mat_tech_2nd_US_Top6_rel_asso)
Relatedness$Cosine_Top6<- mean(mat_tech_2nd_US_Top6_rel_cosi)

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

#then select only the top 10 areas:
IPC_all_patents_2nd_CN_Top10 <- IPC_all_patents_2nd_CN[IPC_all_patents_2nd_CN$techn_field_nr == "6" | 
                                                         IPC_all_patents_2nd_CN$techn_field_nr == "7"| 
                                                         IPC_all_patents_2nd_CN$techn_field_nr == "10"| 
                                                         IPC_all_patents_2nd_CN$techn_field_nr == "12"|
                                                         IPC_all_patents_2nd_CN$techn_field_nr == "4"| 
                                                         IPC_all_patents_2nd_CN$techn_field_nr == "5"| 
                                                         IPC_all_patents_2nd_CN$techn_field_nr == "11"|
                                                         IPC_all_patents_2nd_CN$techn_field_nr == "3"| 
                                                         IPC_all_patents_2nd_CN$techn_field_nr == "2"| 
                                                         IPC_all_patents_2nd_CN$techn_field_nr == "9", ]

mat_tech_2nd_CN_Top10 <- create_sparse_matrix(i = IPC_all_patents_2nd_CN_Top10 %>% pull(appln_id),
                                              j = IPC_all_patents_2nd_CN_Top10 %>% pull(techn_field_nr))

mat_tech_2nd_CN_Top10 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_2nd_CN_Top10_rel_jacc <- relatedness(mat_tech_2nd_CN_Top10, method = "Jaccard")
mat_tech_2nd_CN_Top10_rel_asso <- relatedness(mat_tech_2nd_CN_Top10, method = "association")
mat_tech_2nd_CN_Top10_rel_cosi <- relatedness(mat_tech_2nd_CN_Top10, method = "cosine")

Relatedness_CN$Jaccard_Top10 <- mean(mat_tech_2nd_CN_Top10_rel_jacc)
Relatedness_CN$Association_Top10 <- mean(mat_tech_2nd_CN_Top10_rel_asso)
Relatedness_CN$Cosine_Top10<- mean(mat_tech_2nd_CN_Top10_rel_cosi)

#and finally the top 6 non-AI areas:
IPC_all_patents_2nd_CN_Top6 <- IPC_all_patents_2nd_CN[IPC_all_patents_2nd_CN$techn_field_nr == "4"| 
                                                        IPC_all_patents_2nd_CN$techn_field_nr == "5"| 
                                                        IPC_all_patents_2nd_CN$techn_field_nr == "11"|
                                                        IPC_all_patents_2nd_CN$techn_field_nr == "3"| 
                                                        IPC_all_patents_2nd_CN$techn_field_nr == "2"| 
                                                        IPC_all_patents_2nd_CN$techn_field_nr == "9", ]

mat_tech_2nd_CN_Top6 <- create_sparse_matrix(i = IPC_all_patents_2nd_CN_Top6 %>% pull(appln_id),
                                             j = IPC_all_patents_2nd_CN_Top6 %>% pull(techn_field_nr))

mat_tech_2nd_CN_Top6 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_2nd_CN_Top6_rel_jacc <- relatedness(mat_tech_2nd_CN_Top6, method = "Jaccard")
mat_tech_2nd_CN_Top6_rel_asso <- relatedness(mat_tech_2nd_CN_Top6, method = "association")
mat_tech_2nd_CN_Top6_rel_cosi <- relatedness(mat_tech_2nd_CN_Top6, method = "cosine")

Relatedness_CN$Jaccard_Top6 <- mean(mat_tech_2nd_CN_Top6_rel_jacc)
Relatedness_CN$Association_Top6 <- mean(mat_tech_2nd_CN_Top6_rel_asso)
Relatedness_CN$Cosine_Top6<- mean(mat_tech_2nd_CN_Top6_rel_cosi)


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

#then select only the top 10 areas:
IPC_all_patents_2nd_KR_Top10 <- IPC_all_patents_2nd_KR[IPC_all_patents_2nd_KR$techn_field_nr == "6" | 
                                                         IPC_all_patents_2nd_KR$techn_field_nr == "7"| 
                                                         IPC_all_patents_2nd_KR$techn_field_nr == "10"| 
                                                         IPC_all_patents_2nd_KR$techn_field_nr == "12"|
                                                         IPC_all_patents_2nd_KR$techn_field_nr == "4"| 
                                                         IPC_all_patents_2nd_KR$techn_field_nr == "5"| 
                                                         IPC_all_patents_2nd_KR$techn_field_nr == "11"|
                                                         IPC_all_patents_2nd_KR$techn_field_nr == "3"| 
                                                         IPC_all_patents_2nd_KR$techn_field_nr == "2"| 
                                                         IPC_all_patents_2nd_KR$techn_field_nr == "9", ]

mat_tech_2nd_KR_Top10 <- create_sparse_matrix(i = IPC_all_patents_2nd_KR_Top10 %>% pull(appln_id),
                                              j = IPC_all_patents_2nd_KR_Top10 %>% pull(techn_field_nr))

mat_tech_2nd_KR_Top10 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_2nd_KR_Top10_rel_jacc <- relatedness(mat_tech_2nd_KR_Top10, method = "Jaccard")
mat_tech_2nd_KR_Top10_rel_asso <- relatedness(mat_tech_2nd_KR_Top10, method = "association")
mat_tech_2nd_KR_Top10_rel_cosi <- relatedness(mat_tech_2nd_KR_Top10, method = "cosine")

Relatedness_KR$Jaccard_Top10 <- mean(mat_tech_2nd_KR_Top10_rel_jacc)
Relatedness_KR$Association_Top10 <- mean(mat_tech_2nd_KR_Top10_rel_asso)
Relatedness_KR$Cosine_Top10<- mean(mat_tech_2nd_KR_Top10_rel_cosi)

#and finally the top 6 non-AI areas:
IPC_all_patents_2nd_KR_Top6 <- IPC_all_patents_2nd_KR[IPC_all_patents_2nd_KR$techn_field_nr == "4"| 
                                                        IPC_all_patents_2nd_KR$techn_field_nr == "5"| 
                                                        IPC_all_patents_2nd_KR$techn_field_nr == "11"|
                                                        IPC_all_patents_2nd_KR$techn_field_nr == "3"| 
                                                        IPC_all_patents_2nd_KR$techn_field_nr == "2"| 
                                                        IPC_all_patents_2nd_KR$techn_field_nr == "9", ]

mat_tech_2nd_KR_Top6 <- create_sparse_matrix(i = IPC_all_patents_2nd_KR_Top6 %>% pull(appln_id),
                                             j = IPC_all_patents_2nd_KR_Top6 %>% pull(techn_field_nr))

mat_tech_2nd_KR_Top6 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_2nd_KR_Top6_rel_jacc <- relatedness(mat_tech_2nd_KR_Top6, method = "Jaccard")
mat_tech_2nd_KR_Top6_rel_asso <- relatedness(mat_tech_2nd_KR_Top6, method = "association")
mat_tech_2nd_KR_Top6_rel_cosi <- relatedness(mat_tech_2nd_KR_Top6, method = "cosine")

Relatedness_KR$Jaccard_Top6 <- mean(mat_tech_2nd_KR_Top6_rel_jacc)
Relatedness_KR$Association_Top6 <- mean(mat_tech_2nd_KR_Top6_rel_asso)
Relatedness_KR$Cosine_Top6<- mean(mat_tech_2nd_KR_Top6_rel_cosi)

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

#then select only the top 10 areas:
IPC_all_patents_2nd_JP_Top10 <- IPC_all_patents_2nd_JP[IPC_all_patents_2nd_JP$techn_field_nr == "6" | 
                                                         IPC_all_patents_2nd_JP$techn_field_nr == "7"| 
                                                         IPC_all_patents_2nd_JP$techn_field_nr == "10"| 
                                                         IPC_all_patents_2nd_JP$techn_field_nr == "12"|
                                                         IPC_all_patents_2nd_JP$techn_field_nr == "4"| 
                                                         IPC_all_patents_2nd_JP$techn_field_nr == "5"| 
                                                         IPC_all_patents_2nd_JP$techn_field_nr == "11"|
                                                         IPC_all_patents_2nd_JP$techn_field_nr == "3"| 
                                                         IPC_all_patents_2nd_JP$techn_field_nr == "2"| 
                                                         IPC_all_patents_2nd_JP$techn_field_nr == "9", ]

mat_tech_2nd_JP_Top10 <- create_sparse_matrix(i = IPC_all_patents_2nd_JP_Top10 %>% pull(appln_id),
                                              j = IPC_all_patents_2nd_JP_Top10 %>% pull(techn_field_nr))

mat_tech_2nd_JP_Top10 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_2nd_JP_Top10_rel_jacc <- relatedness(mat_tech_2nd_JP_Top10, method = "Jaccard")
mat_tech_2nd_JP_Top10_rel_asso <- relatedness(mat_tech_2nd_JP_Top10, method = "association")
mat_tech_2nd_JP_Top10_rel_cosi <- relatedness(mat_tech_2nd_JP_Top10, method = "cosine")

Relatedness_JP$Jaccard_Top10 <- mean(mat_tech_2nd_JP_Top10_rel_jacc)
Relatedness_JP$Association_Top10 <- mean(mat_tech_2nd_JP_Top10_rel_asso)
Relatedness_JP$Cosine_Top10<- mean(mat_tech_2nd_JP_Top10_rel_cosi)

#and finally the top 6 non-AI areas:
IPC_all_patents_2nd_JP_Top6 <- IPC_all_patents_2nd_JP[IPC_all_patents_2nd_JP$techn_field_nr == "4"| 
                                                        IPC_all_patents_2nd_JP$techn_field_nr == "5"| 
                                                        IPC_all_patents_2nd_JP$techn_field_nr == "11"|
                                                        IPC_all_patents_2nd_JP$techn_field_nr == "3"| 
                                                        IPC_all_patents_2nd_JP$techn_field_nr == "2"| 
                                                        IPC_all_patents_2nd_JP$techn_field_nr == "9", ]

mat_tech_2nd_JP_Top6 <- create_sparse_matrix(i = IPC_all_patents_2nd_JP_Top6 %>% pull(appln_id),
                                             j = IPC_all_patents_2nd_JP_Top6 %>% pull(techn_field_nr))

mat_tech_2nd_JP_Top6 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_2nd_JP_Top6_rel_jacc <- relatedness(mat_tech_2nd_JP_Top6, method = "Jaccard")
mat_tech_2nd_JP_Top6_rel_asso <- relatedness(mat_tech_2nd_JP_Top6, method = "association")
mat_tech_2nd_JP_Top6_rel_cosi <- relatedness(mat_tech_2nd_JP_Top6, method = "cosine")

Relatedness_JP$Jaccard_Top6 <- mean(mat_tech_2nd_JP_Top6_rel_jacc)
Relatedness_JP$Association_Top6 <- mean(mat_tech_2nd_JP_Top6_rel_asso)
Relatedness_JP$Cosine_Top6<- mean(mat_tech_2nd_JP_Top6_rel_cosi)

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

#then select only the top 10 areas:
IPC_all_patents_2nd_AI_Top10 <- IPC_all_patents_2nd_AI[IPC_all_patents_2nd_AI$techn_field_nr == "6" | 
                                                         IPC_all_patents_2nd_AI$techn_field_nr == "7"| 
                                                         IPC_all_patents_2nd_AI$techn_field_nr == "10"| 
                                                         IPC_all_patents_2nd_AI$techn_field_nr == "12"|
                                                         IPC_all_patents_2nd_AI$techn_field_nr == "4"| 
                                                         IPC_all_patents_2nd_AI$techn_field_nr == "5"| 
                                                         IPC_all_patents_2nd_AI$techn_field_nr == "11"|
                                                         IPC_all_patents_2nd_AI$techn_field_nr == "3"| 
                                                         IPC_all_patents_2nd_AI$techn_field_nr == "2"| 
                                                         IPC_all_patents_2nd_AI$techn_field_nr == "9", ]

mat_tech_2nd_AI_Top10 <- create_sparse_matrix(i = IPC_all_patents_2nd_AI_Top10 %>% pull(appln_id),
                                              j = IPC_all_patents_2nd_AI_Top10 %>% pull(techn_field_nr))

mat_tech_2nd_AI_Top10 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_2nd_AI_Top10_rel_jacc <- relatedness(mat_tech_2nd_AI_Top10, method = "Jaccard")
mat_tech_2nd_AI_Top10_rel_asso <- relatedness(mat_tech_2nd_AI_Top10, method = "association")
mat_tech_2nd_AI_Top10_rel_cosi <- relatedness(mat_tech_2nd_AI_Top10, method = "cosine")

Relatedness_AI$Jaccard_Top10 <- mean(mat_tech_2nd_AI_Top10_rel_jacc)
Relatedness_AI$Association_Top10 <- mean(mat_tech_2nd_AI_Top10_rel_asso)
Relatedness_AI$Cosine_Top10<- mean(mat_tech_2nd_AI_Top10_rel_cosi)

#and finally the top 6 non-AI areas:
IPC_all_patents_2nd_AI_Top6 <- IPC_all_patents_2nd_AI[IPC_all_patents_2nd_AI$techn_field_nr == "4"| 
                                                        IPC_all_patents_2nd_AI$techn_field_nr == "5"| 
                                                        IPC_all_patents_2nd_AI$techn_field_nr == "11"|
                                                        IPC_all_patents_2nd_AI$techn_field_nr == "3"| 
                                                        IPC_all_patents_2nd_AI$techn_field_nr == "2"| 
                                                        IPC_all_patents_2nd_AI$techn_field_nr == "9", ]

mat_tech_2nd_AI_Top6 <- create_sparse_matrix(i = IPC_all_patents_2nd_AI_Top6 %>% pull(appln_id),
                                             j = IPC_all_patents_2nd_AI_Top6 %>% pull(techn_field_nr))

mat_tech_2nd_AI_Top6 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_2nd_AI_Top6_rel_jacc <- relatedness(mat_tech_2nd_AI_Top6, method = "Jaccard")
mat_tech_2nd_AI_Top6_rel_asso <- relatedness(mat_tech_2nd_AI_Top6, method = "association")
mat_tech_2nd_AI_Top6_rel_cosi <- relatedness(mat_tech_2nd_AI_Top6, method = "cosine")

Relatedness_AI$Jaccard_Top6 <- mean(mat_tech_2nd_AI_Top6_rel_jacc)
Relatedness_AI$Association_Top6 <- mean(mat_tech_2nd_AI_Top6_rel_asso)
Relatedness_AI$Cosine_Top6<- mean(mat_tech_2nd_AI_Top6_rel_cosi)

#and we merge it all together:
Relatedness_FirstPeriod <- rbind(Relatedness, Relatedness_CN, Relatedness_KR, Relatedness_JP, Relatedness_AI)
Relatedness_FirstPeriod <- Relatedness_FirstPeriod[,c((1:3), (5:13), (4))]

write.csv2(Relatedness_FirstPeriod, file = "Data_calculations/Relatedness_2nd_period_IPC.csv", row.names = TRUE)

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

write.csv2(Indicators, file = "Data_calculations/Indicators_3rd_period_IPC.csv", row.names = TRUE)

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

write.csv2(KnowledgeComp_3rd, file = "Data_calculations/KnowledgeComp_3rd.csv", row.names = TRUE)
write.csv2(KnowledgeComp_PerCountry_3rd_All, file = "Data_calculations/KnowledgeComp_PerCountry_3rd_All.csv", row.names = TRUE)
write.csv2(KnowledgeComp_PerCountry_3rd_All_RCAs, file = "Data_calculations/KnowledgeComp_PerCountry_3rd_All_RCAs.csv", row.names = TRUE)

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

#then select only the top 10 areas:
IPC_all_patents_3rd_US_Top10 <- IPC_all_patents_3rd_US[IPC_all_patents_3rd_US$techn_field_nr == "6" | 
                                                         IPC_all_patents_3rd_US$techn_field_nr == "7"| 
                                                         IPC_all_patents_3rd_US$techn_field_nr == "10"| 
                                                         IPC_all_patents_3rd_US$techn_field_nr == "12"|
                                                         IPC_all_patents_3rd_US$techn_field_nr == "4"| 
                                                         IPC_all_patents_3rd_US$techn_field_nr == "5"| 
                                                         IPC_all_patents_3rd_US$techn_field_nr == "11"|
                                                         IPC_all_patents_3rd_US$techn_field_nr == "3"| 
                                                         IPC_all_patents_3rd_US$techn_field_nr == "2"| 
                                                         IPC_all_patents_3rd_US$techn_field_nr == "9", ]

mat_tech_3rd_US_Top10 <- create_sparse_matrix(i = IPC_all_patents_3rd_US_Top10 %>% pull(appln_id),
                                              j = IPC_all_patents_3rd_US_Top10 %>% pull(techn_field_nr))

mat_tech_3rd_US_Top10 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_3rd_US_Top10_rel_jacc <- relatedness(mat_tech_3rd_US_Top10, method = "Jaccard")
mat_tech_3rd_US_Top10_rel_asso <- relatedness(mat_tech_3rd_US_Top10, method = "association")
mat_tech_3rd_US_Top10_rel_cosi <- relatedness(mat_tech_3rd_US_Top10, method = "cosine")

Relatedness$Jaccard_Top10 <- mean(mat_tech_3rd_US_Top10_rel_jacc)
Relatedness$Association_Top10 <- mean(mat_tech_3rd_US_Top10_rel_asso)
Relatedness$Cosine_Top10<- mean(mat_tech_3rd_US_Top10_rel_cosi)

#and finally the top 6 non-AI areas:
IPC_all_patents_3rd_US_Top6 <- IPC_all_patents_3rd_US[IPC_all_patents_3rd_US$techn_field_nr == "4"| 
                                                        IPC_all_patents_3rd_US$techn_field_nr == "5"| 
                                                        IPC_all_patents_3rd_US$techn_field_nr == "11"|
                                                        IPC_all_patents_3rd_US$techn_field_nr == "3"| 
                                                        IPC_all_patents_3rd_US$techn_field_nr == "2"| 
                                                        IPC_all_patents_3rd_US$techn_field_nr == "9", ]

mat_tech_3rd_US_Top6 <- create_sparse_matrix(i = IPC_all_patents_3rd_US_Top6 %>% pull(appln_id),
                                             j = IPC_all_patents_3rd_US_Top6 %>% pull(techn_field_nr))

mat_tech_3rd_US_Top6 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_3rd_US_Top6_rel_jacc <- relatedness(mat_tech_3rd_US_Top6, method = "Jaccard")
mat_tech_3rd_US_Top6_rel_asso <- relatedness(mat_tech_3rd_US_Top6, method = "association")
mat_tech_3rd_US_Top6_rel_cosi <- relatedness(mat_tech_3rd_US_Top6, method = "cosine")

Relatedness$Jaccard_Top6 <- mean(mat_tech_3rd_US_Top6_rel_jacc)
Relatedness$Association_Top6 <- mean(mat_tech_3rd_US_Top6_rel_asso)
Relatedness$Cosine_Top6<- mean(mat_tech_3rd_US_Top6_rel_cosi)

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
rm(IPC_all_patents_3rd, IPC_all_patents_3rd_US, IPC_all_patents_3rd_In_US)
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

#then select only the top 10 areas:
IPC_all_patents_3rd_CN_Top10 <- IPC_all_patents_3rd_CN[IPC_all_patents_3rd_CN$techn_field_nr == "6" | 
                                                         IPC_all_patents_3rd_CN$techn_field_nr == "7"| 
                                                         IPC_all_patents_3rd_CN$techn_field_nr == "10"| 
                                                         IPC_all_patents_3rd_CN$techn_field_nr == "12"|
                                                         IPC_all_patents_3rd_CN$techn_field_nr == "4"| 
                                                         IPC_all_patents_3rd_CN$techn_field_nr == "5"| 
                                                         IPC_all_patents_3rd_CN$techn_field_nr == "11"|
                                                         IPC_all_patents_3rd_CN$techn_field_nr == "3"| 
                                                         IPC_all_patents_3rd_CN$techn_field_nr == "2"| 
                                                         IPC_all_patents_3rd_CN$techn_field_nr == "9", ]

mat_tech_3rd_CN_Top10 <- create_sparse_matrix(i = IPC_all_patents_3rd_CN_Top10 %>% pull(appln_id),
                                              j = IPC_all_patents_3rd_CN_Top10 %>% pull(techn_field_nr))

mat_tech_3rd_CN_Top10 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_3rd_CN_Top10_rel_jacc <- relatedness(mat_tech_3rd_CN_Top10, method = "Jaccard")
mat_tech_3rd_CN_Top10_rel_asso <- relatedness(mat_tech_3rd_CN_Top10, method = "association")
mat_tech_3rd_CN_Top10_rel_cosi <- relatedness(mat_tech_3rd_CN_Top10, method = "cosine")

Relatedness_CN$Jaccard_Top10 <- mean(mat_tech_3rd_CN_Top10_rel_jacc)
Relatedness_CN$Association_Top10 <- mean(mat_tech_3rd_CN_Top10_rel_asso)
Relatedness_CN$Cosine_Top10<- mean(mat_tech_3rd_CN_Top10_rel_cosi)

#and finally the top 6 non-AI areas:
rm(IPC_all_patents_3rd_CN_Top4, IPC_all_patents_3rd_CN_Top10, IPC_all_patents_3rd_US_Top10, 
   IPC_all_patents_3rd_US_Top6, IPC_all_patents_3rd_US_Top4)

IPC_all_patents_3rd_CN_Top6 <- IPC_all_patents_3rd_CN[IPC_all_patents_3rd_CN$techn_field_nr == "4"| 
                                                        IPC_all_patents_3rd_CN$techn_field_nr == "5"| 
                                                        IPC_all_patents_3rd_CN$techn_field_nr == "11"|
                                                        IPC_all_patents_3rd_CN$techn_field_nr == "3"| 
                                                        IPC_all_patents_3rd_CN$techn_field_nr == "2"| 
                                                        IPC_all_patents_3rd_CN$techn_field_nr == "9", ]

mat_tech_3rd_CN_Top6 <- create_sparse_matrix(i = IPC_all_patents_3rd_CN_Top6 %>% pull(appln_id),
                                             j = IPC_all_patents_3rd_CN_Top6 %>% pull(techn_field_nr))

mat_tech_3rd_CN_Top6 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_3rd_CN_Top6_rel_jacc <- relatedness(mat_tech_3rd_CN_Top6, method = "Jaccard")
mat_tech_3rd_CN_Top6_rel_asso <- relatedness(mat_tech_3rd_CN_Top6, method = "association")
mat_tech_3rd_CN_Top6_rel_cosi <- relatedness(mat_tech_3rd_CN_Top6, method = "cosine")

Relatedness_CN$Jaccard_Top6 <- mean(mat_tech_3rd_CN_Top6_rel_jacc)
Relatedness_CN$Association_Top6 <- mean(mat_tech_3rd_CN_Top6_rel_asso)
Relatedness_CN$Cosine_Top6<- mean(mat_tech_3rd_CN_Top6_rel_cosi)

#KR
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

#then select only the top 10 areas:
IPC_all_patents_3rd_KR_Top10 <- IPC_all_patents_3rd_KR[IPC_all_patents_3rd_KR$techn_field_nr == "6" | 
                                                         IPC_all_patents_3rd_KR$techn_field_nr == "7"| 
                                                         IPC_all_patents_3rd_KR$techn_field_nr == "10"| 
                                                         IPC_all_patents_3rd_KR$techn_field_nr == "12"|
                                                         IPC_all_patents_3rd_KR$techn_field_nr == "4"| 
                                                         IPC_all_patents_3rd_KR$techn_field_nr == "5"| 
                                                         IPC_all_patents_3rd_KR$techn_field_nr == "11"|
                                                         IPC_all_patents_3rd_KR$techn_field_nr == "3"| 
                                                         IPC_all_patents_3rd_KR$techn_field_nr == "2"| 
                                                         IPC_all_patents_3rd_KR$techn_field_nr == "9", ]

mat_tech_3rd_KR_Top10 <- create_sparse_matrix(i = IPC_all_patents_3rd_KR_Top10 %>% pull(appln_id),
                                              j = IPC_all_patents_3rd_KR_Top10 %>% pull(techn_field_nr))

mat_tech_3rd_KR_Top10 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_3rd_KR_Top10_rel_jacc <- relatedness(mat_tech_3rd_KR_Top10, method = "Jaccard")
mat_tech_3rd_KR_Top10_rel_asso <- relatedness(mat_tech_3rd_KR_Top10, method = "association")
mat_tech_3rd_KR_Top10_rel_cosi <- relatedness(mat_tech_3rd_KR_Top10, method = "cosine")

Relatedness_KR$Jaccard_Top10 <- mean(mat_tech_3rd_KR_Top10_rel_jacc)
Relatedness_KR$Association_Top10 <- mean(mat_tech_3rd_KR_Top10_rel_asso)
Relatedness_KR$Cosine_Top10<- mean(mat_tech_3rd_KR_Top10_rel_cosi)

#and finally the top 6 non-AI areas:
IPC_all_patents_3rd_KR_Top6 <- IPC_all_patents_3rd_KR[IPC_all_patents_3rd_KR$techn_field_nr == "4"| 
                                                        IPC_all_patents_3rd_KR$techn_field_nr == "5"| 
                                                        IPC_all_patents_3rd_KR$techn_field_nr == "11"|
                                                        IPC_all_patents_3rd_KR$techn_field_nr == "3"| 
                                                        IPC_all_patents_3rd_KR$techn_field_nr == "2"| 
                                                        IPC_all_patents_3rd_KR$techn_field_nr == "9", ]

mat_tech_3rd_KR_Top6 <- create_sparse_matrix(i = IPC_all_patents_3rd_KR_Top6 %>% pull(appln_id),
                                             j = IPC_all_patents_3rd_KR_Top6 %>% pull(techn_field_nr))

mat_tech_3rd_KR_Top6 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_3rd_KR_Top6_rel_jacc <- relatedness(mat_tech_3rd_KR_Top6, method = "Jaccard")
mat_tech_3rd_KR_Top6_rel_asso <- relatedness(mat_tech_3rd_KR_Top6, method = "association")
mat_tech_3rd_KR_Top6_rel_cosi <- relatedness(mat_tech_3rd_KR_Top6, method = "cosine")

Relatedness_KR$Jaccard_Top6 <- mean(mat_tech_3rd_KR_Top6_rel_jacc)
Relatedness_KR$Association_Top6 <- mean(mat_tech_3rd_KR_Top6_rel_asso)
Relatedness_KR$Cosine_Top6<- mean(mat_tech_3rd_KR_Top6_rel_cosi)

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

#then select only the top 10 areas:
IPC_all_patents_3rd_JP_Top10 <- IPC_all_patents_3rd_JP[IPC_all_patents_3rd_JP$techn_field_nr == "6" | 
                                                         IPC_all_patents_3rd_JP$techn_field_nr == "7"| 
                                                         IPC_all_patents_3rd_JP$techn_field_nr == "10"| 
                                                         IPC_all_patents_3rd_JP$techn_field_nr == "12"|
                                                         IPC_all_patents_3rd_JP$techn_field_nr == "4"| 
                                                         IPC_all_patents_3rd_JP$techn_field_nr == "5"| 
                                                         IPC_all_patents_3rd_JP$techn_field_nr == "11"|
                                                         IPC_all_patents_3rd_JP$techn_field_nr == "3"| 
                                                         IPC_all_patents_3rd_JP$techn_field_nr == "2"| 
                                                         IPC_all_patents_3rd_JP$techn_field_nr == "9", ]

mat_tech_3rd_JP_Top10 <- create_sparse_matrix(i = IPC_all_patents_3rd_JP_Top10 %>% pull(appln_id),
                                              j = IPC_all_patents_3rd_JP_Top10 %>% pull(techn_field_nr))

mat_tech_3rd_JP_Top10 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_3rd_JP_Top10_rel_jacc <- relatedness(mat_tech_3rd_JP_Top10, method = "Jaccard")
mat_tech_3rd_JP_Top10_rel_asso <- relatedness(mat_tech_3rd_JP_Top10, method = "association")
mat_tech_3rd_JP_Top10_rel_cosi <- relatedness(mat_tech_3rd_JP_Top10, method = "cosine")

Relatedness_JP$Jaccard_Top10 <- mean(mat_tech_3rd_JP_Top10_rel_jacc)
Relatedness_JP$Association_Top10 <- mean(mat_tech_3rd_JP_Top10_rel_asso)
Relatedness_JP$Cosine_Top10<- mean(mat_tech_3rd_JP_Top10_rel_cosi)

#and finally the top 6 non-AI areas:
IPC_all_patents_3rd_JP_Top6 <- IPC_all_patents_3rd_JP[IPC_all_patents_3rd_JP$techn_field_nr == "4"| 
                                                        IPC_all_patents_3rd_JP$techn_field_nr == "5"| 
                                                        IPC_all_patents_3rd_JP$techn_field_nr == "11"|
                                                        IPC_all_patents_3rd_JP$techn_field_nr == "3"| 
                                                        IPC_all_patents_3rd_JP$techn_field_nr == "2"| 
                                                        IPC_all_patents_3rd_JP$techn_field_nr == "9", ]

mat_tech_3rd_JP_Top6 <- create_sparse_matrix(i = IPC_all_patents_3rd_JP_Top6 %>% pull(appln_id),
                                             j = IPC_all_patents_3rd_JP_Top6 %>% pull(techn_field_nr))

mat_tech_3rd_JP_Top6 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_3rd_JP_Top6_rel_jacc <- relatedness(mat_tech_3rd_JP_Top6, method = "Jaccard")
mat_tech_3rd_JP_Top6_rel_asso <- relatedness(mat_tech_3rd_JP_Top6, method = "association")
mat_tech_3rd_JP_Top6_rel_cosi <- relatedness(mat_tech_3rd_JP_Top6, method = "cosine")

Relatedness_JP$Jaccard_Top6 <- mean(mat_tech_3rd_JP_Top6_rel_jacc)
Relatedness_JP$Association_Top6 <- mean(mat_tech_3rd_JP_Top6_rel_asso)
Relatedness_JP$Cosine_Top6<- mean(mat_tech_3rd_JP_Top6_rel_cosi)

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

#then select only the top 10 areas:
IPC_all_patents_3rd_AI_Top10 <- IPC_all_patents_3rd_AI[IPC_all_patents_3rd_AI$techn_field_nr == "6" | 
                                                         IPC_all_patents_3rd_AI$techn_field_nr == "7"| 
                                                         IPC_all_patents_3rd_AI$techn_field_nr == "10"| 
                                                         IPC_all_patents_3rd_AI$techn_field_nr == "12"|
                                                         IPC_all_patents_3rd_AI$techn_field_nr == "4"| 
                                                         IPC_all_patents_3rd_AI$techn_field_nr == "5"| 
                                                         IPC_all_patents_3rd_AI$techn_field_nr == "11"|
                                                         IPC_all_patents_3rd_AI$techn_field_nr == "3"| 
                                                         IPC_all_patents_3rd_AI$techn_field_nr == "2"| 
                                                         IPC_all_patents_3rd_AI$techn_field_nr == "9", ]

mat_tech_3rd_AI_Top10 <- create_sparse_matrix(i = IPC_all_patents_3rd_AI_Top10 %>% pull(appln_id),
                                              j = IPC_all_patents_3rd_AI_Top10 %>% pull(techn_field_nr))

mat_tech_3rd_AI_Top10 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_3rd_AI_Top10_rel_jacc <- relatedness(mat_tech_3rd_AI_Top10, method = "Jaccard")
mat_tech_3rd_AI_Top10_rel_asso <- relatedness(mat_tech_3rd_AI_Top10, method = "association")
mat_tech_3rd_AI_Top10_rel_cosi <- relatedness(mat_tech_3rd_AI_Top10, method = "cosine")

Relatedness_AI$Jaccard_Top10 <- mean(mat_tech_3rd_AI_Top10_rel_jacc)
Relatedness_AI$Association_Top10 <- mean(mat_tech_3rd_AI_Top10_rel_asso)
Relatedness_AI$Cosine_Top10<- mean(mat_tech_3rd_AI_Top10_rel_cosi)

#and finally the top 6 non-AI areas:
IPC_all_patents_3rd_AI_Top6 <- IPC_all_patents_3rd_AI[IPC_all_patents_3rd_AI$techn_field_nr == "4"| 
                                                        IPC_all_patents_3rd_AI$techn_field_nr == "5"| 
                                                        IPC_all_patents_3rd_AI$techn_field_nr == "11"|
                                                        IPC_all_patents_3rd_AI$techn_field_nr == "3"| 
                                                        IPC_all_patents_3rd_AI$techn_field_nr == "2"| 
                                                        IPC_all_patents_3rd_AI$techn_field_nr == "9", ]

mat_tech_3rd_AI_Top6 <- create_sparse_matrix(i = IPC_all_patents_3rd_AI_Top6 %>% pull(appln_id),
                                             j = IPC_all_patents_3rd_AI_Top6 %>% pull(techn_field_nr))

mat_tech_3rd_AI_Top6 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_3rd_AI_Top6_rel_jacc <- relatedness(mat_tech_3rd_AI_Top6, method = "Jaccard")
mat_tech_3rd_AI_Top6_rel_asso <- relatedness(mat_tech_3rd_AI_Top6, method = "association")
mat_tech_3rd_AI_Top6_rel_cosi <- relatedness(mat_tech_3rd_AI_Top6, method = "cosine")

Relatedness_AI$Jaccard_Top6 <- mean(mat_tech_3rd_AI_Top6_rel_jacc)
Relatedness_AI$Association_Top6 <- mean(mat_tech_3rd_AI_Top6_rel_asso)
Relatedness_AI$Cosine_Top6<- mean(mat_tech_3rd_AI_Top6_rel_cosi)

#and we merge it all together:
Relatedness_FirstPeriod <- rbind(Relatedness, Relatedness_CN, Relatedness_KR, Relatedness_JP, Relatedness_AI)
Relatedness_FirstPeriod <- Relatedness_FirstPeriod[,c((1:3), (5:13), (4))]

write.csv2(Relatedness_FirstPeriod, file = "Data_calculations/Relatedness_3rd_period_IPC.csv", row.names = TRUE)
