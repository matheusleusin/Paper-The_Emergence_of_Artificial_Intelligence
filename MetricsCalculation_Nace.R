library(tidyverse) # Collection of all the good stuff like dplyr, ggplot2 ect.
library(magrittr) # For extra-piping operators (eg. %<>%)
library(tidygraph) # For tidy-style graph manipulation
library(ggraph) # For ggplot2 style graph plotting

library(EconGeo) # Economic Geography functions

library(data.table) #for reading the big files using fread and for replacing countries names (by AI_pat for example)

library(stringr) #for separating the Nace codes in subclasses

library(janitor) #used here for converting the first column of data to row names.

#visualization:
library(ggrepel)
library(scales) #for scaling without cutting data out
library(patchwork) #for cutting out the X labs while keeping the legend

#1.Indicators ----
rm(list=ls())
setwd("C:/Users/Matheus/Desktop") #for loading the big file

#1.1. First period ----
#1.1.1.Load the data we need and filter it -----
#The file for the first period is composed of 45,182,803 lines which we will read in 3 parts:
c <- 45233329 -40000000
Nace_all_patents_Part1 <- fread("All_patents_and_Naces_Part2.csv", header = F, nrow = 20000000)[ ,c(-4)]
Nace_all_patents_Part2 <- fread("All_patents_and_Naces_Part2.csv", header = F, nrow = 20000000, skip = 20000000)[ ,c(-4)]
Nace_all_patents_Part3 <- fread("All_patents_and_Naces_Part2.csv", header = F, nrow = c, skip = 40000000)[ ,c(-4)]

a = 1973
b = 1989

Nace_all_patents_Part1 <- Nace_all_patents_Part1[Nace_all_patents_Part1$V5 < b,]
Nace_all_patents_Part1 <- Nace_all_patents_Part1[Nace_all_patents_Part1$V5 > a,]

Nace_all_patents_Part2 <- Nace_all_patents_Part2[Nace_all_patents_Part2$V5 < b,]
Nace_all_patents_Part2 <- Nace_all_patents_Part2[Nace_all_patents_Part2$V5 > a,]

Nace_all_patents_Part3 <- Nace_all_patents_Part3[Nace_all_patents_Part3$V5 < b,]
Nace_all_patents_Part3 <- Nace_all_patents_Part3[Nace_all_patents_Part3$V5 > a,]

Nace_all_patents_1st <- rbind(Nace_all_patents_Part1, Nace_all_patents_Part2, Nace_all_patents_Part3)
rm(Nace_all_patents_Part1, Nace_all_patents_Part2, Nace_all_patents_Part3)

names(Nace_all_patents_1st) <- c("appln_id", "ctry_code", "nace2_code", "priority_year")
Nace_all_patents_1st$ctry_code2 <- Nace_all_patents_1st$ctry_code

#read AI patents
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
patents_AI_specific <- read.csv("Data_Nace/Nace2_AI csv.csv", sep = ";", header = TRUE, dec=",")
patents_AI_specific$ctry_code <- as.vector(patents_AI_specific$ctry_code)
patents_AI_specific$ctry_code <- "AI_pat"
setDT(patents_AI_specific)
setDT(Nace_all_patents_1st)
Nace_all_patents_1st[patents_AI_specific, on = c("appln_id"), ctry_code2 := i.ctry_code]

Nace_all_patents_1st_US <- Nace_all_patents_1st[Nace_all_patents_1st$ctry_code == "US", ]
Nace_all_patents_1st_CN <- Nace_all_patents_1st[Nace_all_patents_1st$ctry_code == "CN", ]
Nace_all_patents_1st_KR <- Nace_all_patents_1st[Nace_all_patents_1st$ctry_code == "KR", ]
Nace_all_patents_1st_JP <- Nace_all_patents_1st[Nace_all_patents_1st$ctry_code == "JP", ]
Nace_all_patents_1st_AI <- Nace_all_patents_1st[Nace_all_patents_1st$ctry_code2 == "AI_pat", ]

#1.1.2. Calculate the Indicators -----
Nace_all_patents_1st_In <- Nace_all_patents_1st[,c((-1), (-4), (-5))]
mat_1st <- as.data.frame(table(Nace_all_patents_1st_In$ctry_code, Nace_all_patents_1st_In$nace2_code))
mat_1st <- get.matrix(mat_1st)

Indicators <- as.data.frame(Herfindahl(mat_1st))
names(Indicators) <- "Herfindahl"
mat_1st_RCAs <- location.quotient(mat_1st, binary = T)
Indicators$Herfindahl_RCA <- Herfindahl(mat_1st_RCAs)
Indicators$Entropy <- entropy(mat_1st)
Indicators$Entropy_RCA <- entropy(mat_1st_RCAs)
Indicators$Period <- "1st"

write.csv2(Indicators, file = "Data_calculations_Nace/Indicators_1st_period_Nace.csv", row.names = TRUE)

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

write.csv2(KnowledgeComp_1st, file = "Data_calculations_Nace/KnowledgeComp_1st.csv", row.names = TRUE)
write.csv2(KnowledgeComp_PerCountry_1st_All, file = "Data_calculations_Nace/KnowledgeComp_PerCountry_1st_All.csv", row.names = TRUE)
write.csv2(KnowledgeComp_PerCountry_1st_All_RCAs, file = "Data_calculations_Nace/KnowledgeComp_PerCountry_1st_All_RCAs.csv", row.names = TRUE)

#For AI complexity and Indicators:
Nace_all_patents_1st_In <- Nace_all_patents_1st[,c((-1), (-4), (-2))]
mat_1st <- as.data.frame(table(Nace_all_patents_1st_In$ctry_code2, Nace_all_patents_1st_In$nace2_code))
mat_1st <- get.matrix(mat_1st)

Indicators <- as.data.frame(Herfindahl(mat_1st))
names(Indicators) <- "Herfindahl"
mat_1st_RCAs <- location.quotient(mat_1st, binary = T)
Indicators$Herfindahl_RCA <- Herfindahl(mat_1st_RCAs)
Indicators$Entropy <- entropy(mat_1st)
Indicators$Entropy_RCA <- entropy(mat_1st_RCAs)
Indicators$Period <- "1st"

write.csv2(Indicators, file = "Data_calculations_Nace/Indicators_1st_period_Nace_AI.csv", row.names = TRUE)
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

write.csv2(KnowledgeComp_PerCountry_1st_All, file = "Data_calculations_Nace/KnowledgeComp_PerCountry_1st_All_AI.csv", row.names = TRUE)
write.csv2(KnowledgeComp_PerCountry_1st_All_RCAs, file = "Data_calculations_Nace/KnowledgeComp_PerCountry_1st_All_RCAs_AI.csv", row.names = TRUE)

#1.1.3. Calculate the relatedness -----
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
mat_tech_1st_US <- create_sparse_matrix(i = Nace_all_patents_1st_US %>% pull(appln_id),
                                        j = Nace_all_patents_1st_US %>% pull(nace2_code))

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
Nace_all_patents_1st_US_Top4 <- Nace_all_patents_1st_US[Nace_all_patents_1st_US$nace2_code == "26.2" | 
                                                     Nace_all_patents_1st_US$nace2_code == "26.5"| 
                                                     Nace_all_patents_1st_US$nace2_code == "26.51"| 
                                                     Nace_all_patents_1st_US$nace2_code == "62", ]

mat_tech_1st_US_Top4 <- create_sparse_matrix(i = Nace_all_patents_1st_US_Top4 %>% pull(appln_id),
                                        j = Nace_all_patents_1st_US_Top4 %>% pull(nace2_code))

mat_tech_1st_US_Top4 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_1st_US_Top4_rel_jacc <- relatedness(mat_tech_1st_US_Top4, method = "Jaccard")
mat_tech_1st_US_Top4_rel_asso <- relatedness(mat_tech_1st_US_Top4, method = "association")
mat_tech_1st_US_Top4_rel_cosi <- relatedness(mat_tech_1st_US_Top4, method = "cosine")

Relatedness$Jaccard_top4 <- mean(mat_tech_1st_US_Top4_rel_jacc)
Relatedness$Association_top4 <- mean(mat_tech_1st_US_Top4_rel_asso)
Relatedness$Cosine_top4<- mean(mat_tech_1st_US_Top4_rel_cosi)

#then select only the top 7 areas:
Nace_all_patents_1st_US_Top7 <- Nace_all_patents_1st_US[Nace_all_patents_1st_US$nace2_code == "25.3"| 
                                                         Nace_all_patents_1st_US$nace2_code == "28.23"| 
                                                         Nace_all_patents_1st_US$nace2_code == "29.3"|
                                                         Nace_all_patents_1st_US$nace2_code == "26.4"| 
                                                         Nace_all_patents_1st_US$nace2_code == "27.12"| 
                                                         Nace_all_patents_1st_US$nace2_code == "27.9"| 
                                                         Nace_all_patents_1st_US$nace2_code == "28.25", ]

mat_tech_1st_US_Top7 <- create_sparse_matrix(i = Nace_all_patents_1st_US_Top7 %>% pull(appln_id),
                                              j = Nace_all_patents_1st_US_Top7 %>% pull(nace2_code))

mat_tech_1st_US_Top7 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_1st_US_Top7_rel_jacc <- relatedness(mat_tech_1st_US_Top7, method = "Jaccard")
mat_tech_1st_US_Top7_rel_asso <- relatedness(mat_tech_1st_US_Top7, method = "association")
mat_tech_1st_US_Top7_rel_cosi <- relatedness(mat_tech_1st_US_Top7, method = "cosine")

Relatedness$Jaccard_Top7 <- mean(mat_tech_1st_US_Top7_rel_jacc)
Relatedness$Association_Top7 <- mean(mat_tech_1st_US_Top7_rel_asso)
Relatedness$Cosine_Top7<- mean(mat_tech_1st_US_Top7_rel_cosi)

#then select only the top 8 areas:
Nace_all_patents_1st_US_Top8 <- Nace_all_patents_1st_US[Nace_all_patents_1st_US$nace2_code == "28.23"| 
                                                          Nace_all_patents_1st_US$nace2_code == "26.7"| 
                                                          Nace_all_patents_1st_US$nace2_code == "32.9"|
                                                          Nace_all_patents_1st_US$nace2_code == "26.1"| 
                                                          Nace_all_patents_1st_US$nace2_code == "27.5"| 
                                                          Nace_all_patents_1st_US$nace2_code == "27.33"| 
                                                          Nace_all_patents_1st_US$nace2_code == "27.4"| 
                                                          Nace_all_patents_1st_US$nace2_code == "26.3", ]

mat_tech_1st_US_Top8 <- create_sparse_matrix(i = Nace_all_patents_1st_US_Top8 %>% pull(appln_id),
                                             j = Nace_all_patents_1st_US_Top8 %>% pull(nace2_code))

mat_tech_1st_US_Top8 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_1st_US_Top8_rel_jacc <- relatedness(mat_tech_1st_US_Top8, method = "Jaccard")
mat_tech_1st_US_Top8_rel_asso <- relatedness(mat_tech_1st_US_Top8, method = "association")
mat_tech_1st_US_Top8_rel_cosi <- relatedness(mat_tech_1st_US_Top8, method = "cosine")

Relatedness$Jaccard_Top8 <- mean(mat_tech_1st_US_Top8_rel_jacc)
Relatedness$Association_Top8 <- mean(mat_tech_1st_US_Top8_rel_asso)
Relatedness$Cosine_Top8<- mean(mat_tech_1st_US_Top8_rel_cosi)

#and finally the top 3 close to AI areas:
Nace_all_patents_1st_US_Top3 <- Nace_all_patents_1st_US[Nace_all_patents_1st_US$nace2_code == "25.3"| 
                                                        Nace_all_patents_1st_US$nace2_code == "28.23"| 
                                                        Nace_all_patents_1st_US$nace2_code == "29.3", ]

mat_tech_1st_US_Top3 <- create_sparse_matrix(i = Nace_all_patents_1st_US_Top3 %>% pull(appln_id),
                                             j = Nace_all_patents_1st_US_Top3 %>% pull(nace2_code))

mat_tech_1st_US_Top3 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_1st_US_Top3_rel_jacc <- relatedness(mat_tech_1st_US_Top3, method = "Jaccard")
mat_tech_1st_US_Top3_rel_asso <- relatedness(mat_tech_1st_US_Top3, method = "association")
mat_tech_1st_US_Top3_rel_cosi <- relatedness(mat_tech_1st_US_Top3, method = "cosine")

Relatedness$Jaccard_Top3 <- mean(mat_tech_1st_US_Top3_rel_jacc)
Relatedness$Association_Top3 <- mean(mat_tech_1st_US_Top3_rel_asso)
Relatedness$Cosine_Top3<- mean(mat_tech_1st_US_Top3_rel_cosi)

#China:
mat_tech_1st_CN <- create_sparse_matrix(i = Nace_all_patents_1st_CN %>% pull(appln_id),
                                          j = Nace_all_patents_1st_CN %>% pull(nace2_code))

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
Nace_all_patents_1st_CN_Top4 <- Nace_all_patents_1st_CN[Nace_all_patents_1st_CN$nace2_code == "26.2" | 
                                                        Nace_all_patents_1st_CN$nace2_code == "26.5"| 
                                                        Nace_all_patents_1st_CN$nace2_code == "26.51"| 
                                                        Nace_all_patents_1st_CN$nace2_code == "62", ]

mat_tech_1st_CN_Top4 <- create_sparse_matrix(i = Nace_all_patents_1st_CN_Top4 %>% pull(appln_id),
                                             j = Nace_all_patents_1st_CN_Top4 %>% pull(nace2_code))

mat_tech_1st_CN_Top4 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_1st_CN_Top4_rel_jacc <- relatedness(mat_tech_1st_CN_Top4, method = "Jaccard")
mat_tech_1st_CN_Top4_rel_asso <- relatedness(mat_tech_1st_CN_Top4, method = "association")
mat_tech_1st_CN_Top4_rel_cosi <- relatedness(mat_tech_1st_CN_Top4, method = "cosine")

Relatedness_CN$Jaccard_top4 <- mean(mat_tech_1st_CN_Top4_rel_jacc)
Relatedness_CN$Association_top4 <- mean(mat_tech_1st_CN_Top4_rel_asso)
Relatedness_CN$Cosine_top4<- mean(mat_tech_1st_CN_Top4_rel_cosi)

#then select only the top 7 areas:
Nace_all_patents_1st_CN_Top7 <- Nace_all_patents_1st_CN[Nace_all_patents_1st_CN$nace2_code == "25.3"| 
                                                         Nace_all_patents_1st_CN$nace2_code == "28.23"| 
                                                         Nace_all_patents_1st_CN$nace2_code == "29.3"|
                                                         Nace_all_patents_1st_CN$nace2_code == "26.4"| 
                                                         Nace_all_patents_1st_CN$nace2_code == "27.12"| 
                                                         Nace_all_patents_1st_CN$nace2_code == "27.9"| 
                                                         Nace_all_patents_1st_CN$nace2_code == "28.25", ]

mat_tech_1st_CN_Top7 <- create_sparse_matrix(i = Nace_all_patents_1st_CN_Top7 %>% pull(appln_id),
                                              j = Nace_all_patents_1st_CN_Top7 %>% pull(nace2_code))

mat_tech_1st_CN_Top7 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_1st_CN_Top7_rel_jacc <- relatedness(mat_tech_1st_CN_Top7, method = "Jaccard")
mat_tech_1st_CN_Top7_rel_asso <- relatedness(mat_tech_1st_CN_Top7, method = "association")
mat_tech_1st_CN_Top7_rel_cosi <- relatedness(mat_tech_1st_CN_Top7, method = "cosine")

Relatedness_CN$Jaccard_Top7 <- mean(mat_tech_1st_CN_Top7_rel_jacc)
Relatedness_CN$Association_Top7 <- mean(mat_tech_1st_CN_Top7_rel_asso)
Relatedness_CN$Cosine_Top7<- mean(mat_tech_1st_CN_Top7_rel_cosi)

#then select only the top 8 areas:
Nace_all_patents_1st_CN_Top8 <- Nace_all_patents_1st_CN[Nace_all_patents_1st_CN$nace2_code == "28.23"| 
                                                          Nace_all_patents_1st_CN$nace2_code == "26.7"| 
                                                          Nace_all_patents_1st_CN$nace2_code == "32.9"|
                                                          Nace_all_patents_1st_CN$nace2_code == "26.1"| 
                                                          Nace_all_patents_1st_CN$nace2_code == "27.5"| 
                                                          Nace_all_patents_1st_CN$nace2_code == "27.33"| 
                                                          Nace_all_patents_1st_CN$nace2_code == "27.4"| 
                                                          Nace_all_patents_1st_CN$nace2_code == "26.3", ]

mat_tech_1st_CN_Top8 <- create_sparse_matrix(i = Nace_all_patents_1st_CN_Top8 %>% pull(appln_id),
                                             j = Nace_all_patents_1st_CN_Top8 %>% pull(nace2_code))

mat_tech_1st_CN_Top8 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_1st_CN_Top8_rel_jacc <- relatedness(mat_tech_1st_CN_Top8, method = "Jaccard")
mat_tech_1st_CN_Top8_rel_asso <- relatedness(mat_tech_1st_CN_Top8, method = "association")
mat_tech_1st_CN_Top8_rel_cosi <- relatedness(mat_tech_1st_CN_Top8, method = "cosine")

Relatedness_CN$Jaccard_Top8 <- mean(mat_tech_1st_CN_Top8_rel_jacc)
Relatedness_CN$Association_Top8 <- mean(mat_tech_1st_CN_Top8_rel_asso)
Relatedness_CN$Cosine_Top8<- mean(mat_tech_1st_CN_Top8_rel_cosi)

#and finally the top 3 close to AI areas:
Nace_all_patents_1st_CN_Top3 <- Nace_all_patents_1st_CN[Nace_all_patents_1st_CN$nace2_code == "25.3"| 
                                                        Nace_all_patents_1st_CN$nace2_code == "28.23"| 
                                                        Nace_all_patents_1st_CN$nace2_code == "29.3", ]

mat_tech_1st_CN_Top3 <- create_sparse_matrix(i = Nace_all_patents_1st_CN_Top3 %>% pull(appln_id),
                                             j = Nace_all_patents_1st_CN_Top3 %>% pull(nace2_code))

mat_tech_1st_CN_Top3 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_1st_CN_Top3_rel_jacc <- relatedness(mat_tech_1st_CN_Top3, method = "Jaccard")
mat_tech_1st_CN_Top3_rel_asso <- relatedness(mat_tech_1st_CN_Top3, method = "association")
mat_tech_1st_CN_Top3_rel_cosi <- relatedness(mat_tech_1st_CN_Top3, method = "cosine")

Relatedness_CN$Jaccard_Top3 <- mean(mat_tech_1st_CN_Top3_rel_jacc)
Relatedness_CN$Association_Top3 <- mean(mat_tech_1st_CN_Top3_rel_asso)
Relatedness_CN$Cosine_Top3<- mean(mat_tech_1st_CN_Top3_rel_cosi)


#KR
mat_tech_1st_KR <- create_sparse_matrix(i = Nace_all_patents_1st_KR %>% pull(appln_id),
                                        j = Nace_all_patents_1st_KR %>% pull(nace2_code))

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
Nace_all_patents_1st_KR_Top4 <- Nace_all_patents_1st_KR[Nace_all_patents_1st_KR$nace2_code == "26.2" | 
                                                        Nace_all_patents_1st_KR$nace2_code == "26.5"| 
                                                        Nace_all_patents_1st_KR$nace2_code == "26.51"| 
                                                        Nace_all_patents_1st_KR$nace2_code == "62", ]

mat_tech_1st_KR_Top4 <- create_sparse_matrix(i = Nace_all_patents_1st_KR_Top4 %>% pull(appln_id),
                                             j = Nace_all_patents_1st_KR_Top4 %>% pull(nace2_code))

mat_tech_1st_KR_Top4 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_1st_KR_Top4_rel_jacc <- relatedness(mat_tech_1st_KR_Top4, method = "Jaccard")
mat_tech_1st_KR_Top4_rel_asso <- relatedness(mat_tech_1st_KR_Top4, method = "association")
mat_tech_1st_KR_Top4_rel_cosi <- relatedness(mat_tech_1st_KR_Top4, method = "cosine")

Relatedness_KR$Jaccard_top4 <- mean(mat_tech_1st_KR_Top4_rel_jacc)
Relatedness_KR$Association_top4 <- mean(mat_tech_1st_KR_Top4_rel_asso)
Relatedness_KR$Cosine_top4<- mean(mat_tech_1st_KR_Top4_rel_cosi)

#then select only the top 7 areas:
Nace_all_patents_1st_KR_Top7 <- Nace_all_patents_1st_KR[Nace_all_patents_1st_KR$nace2_code == "25.3"| 
                                                         Nace_all_patents_1st_KR$nace2_code == "28.23"| 
                                                         Nace_all_patents_1st_KR$nace2_code == "29.3"|
                                                         Nace_all_patents_1st_KR$nace2_code == "26.4"| 
                                                         Nace_all_patents_1st_KR$nace2_code == "27.12"| 
                                                         Nace_all_patents_1st_KR$nace2_code == "27.9"| 
                                                         Nace_all_patents_1st_KR$nace2_code == "28.25", ]

mat_tech_1st_KR_Top7 <- create_sparse_matrix(i = Nace_all_patents_1st_KR_Top7 %>% pull(appln_id),
                                              j = Nace_all_patents_1st_KR_Top7 %>% pull(nace2_code))

mat_tech_1st_KR_Top7 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_1st_KR_Top7_rel_jacc <- relatedness(mat_tech_1st_KR_Top7, method = "Jaccard")
mat_tech_1st_KR_Top7_rel_asso <- relatedness(mat_tech_1st_KR_Top7, method = "association")
mat_tech_1st_KR_Top7_rel_cosi <- relatedness(mat_tech_1st_KR_Top7, method = "cosine")

Relatedness_KR$Jaccard_Top7 <- mean(mat_tech_1st_KR_Top7_rel_jacc)
Relatedness_KR$Association_Top7 <- mean(mat_tech_1st_KR_Top7_rel_asso)
Relatedness_KR$Cosine_Top7<- mean(mat_tech_1st_KR_Top7_rel_cosi)

#then select only the top 8 areas:
Nace_all_patents_1st_KR_Top8 <- Nace_all_patents_1st_KR[Nace_all_patents_1st_KR$nace2_code == "28.23"| 
                                                          Nace_all_patents_1st_KR$nace2_code == "26.7"| 
                                                          Nace_all_patents_1st_KR$nace2_code == "32.9"|
                                                          Nace_all_patents_1st_KR$nace2_code == "26.1"| 
                                                          Nace_all_patents_1st_KR$nace2_code == "27.5"| 
                                                          Nace_all_patents_1st_KR$nace2_code == "27.33"| 
                                                          Nace_all_patents_1st_KR$nace2_code == "27.4"| 
                                                          Nace_all_patents_1st_KR$nace2_code == "26.3", ]

mat_tech_1st_KR_Top8 <- create_sparse_matrix(i = Nace_all_patents_1st_KR_Top8 %>% pull(appln_id),
                                             j = Nace_all_patents_1st_KR_Top8 %>% pull(nace2_code))

mat_tech_1st_KR_Top8 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_1st_KR_Top8_rel_jacc <- relatedness(mat_tech_1st_KR_Top8, method = "Jaccard")
mat_tech_1st_KR_Top8_rel_asso <- relatedness(mat_tech_1st_KR_Top8, method = "association")
mat_tech_1st_KR_Top8_rel_cosi <- relatedness(mat_tech_1st_KR_Top8, method = "cosine")

Relatedness_KR$Jaccard_Top8 <- mean(mat_tech_1st_KR_Top8_rel_jacc)
Relatedness_KR$Association_Top8 <- mean(mat_tech_1st_KR_Top8_rel_asso)
Relatedness_KR$Cosine_Top8<- mean(mat_tech_1st_KR_Top8_rel_cosi)

#and finally the top 3 close to AI areas:
Nace_all_patents_1st_KR_Top3 <- Nace_all_patents_1st_KR[Nace_all_patents_1st_KR$nace2_code == "25.3"| 
                                                        Nace_all_patents_1st_KR$nace2_code == "28.23"| 
                                                        Nace_all_patents_1st_KR$nace2_code == "29.3", ]

mat_tech_1st_KR_Top3 <- create_sparse_matrix(i = Nace_all_patents_1st_KR_Top3 %>% pull(appln_id),
                                             j = Nace_all_patents_1st_KR_Top3 %>% pull(nace2_code))

mat_tech_1st_KR_Top3 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_1st_KR_Top3_rel_jacc <- relatedness(mat_tech_1st_KR_Top3, method = "Jaccard")
mat_tech_1st_KR_Top3_rel_asso <- relatedness(mat_tech_1st_KR_Top3, method = "association")
mat_tech_1st_KR_Top3_rel_cosi <- relatedness(mat_tech_1st_KR_Top3, method = "cosine")

Relatedness_KR$Jaccard_Top3 <- mean(mat_tech_1st_KR_Top3_rel_jacc)
Relatedness_KR$Association_Top3 <- mean(mat_tech_1st_KR_Top3_rel_asso)
Relatedness_KR$Cosine_Top3<- mean(mat_tech_1st_KR_Top3_rel_cosi)

#Japan
mat_tech_1st_JP <- create_sparse_matrix(i = Nace_all_patents_1st_JP %>% pull(appln_id),
                                        j = Nace_all_patents_1st_JP %>% pull(nace2_code))

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
Nace_all_patents_1st_JP_Top4 <- Nace_all_patents_1st_JP[Nace_all_patents_1st_JP$nace2_code == "26.2" | 
                                                        Nace_all_patents_1st_JP$nace2_code == "26.5"| 
                                                        Nace_all_patents_1st_JP$nace2_code == "26.51"| 
                                                        Nace_all_patents_1st_JP$nace2_code == "62", ]

mat_tech_1st_JP_Top4 <- create_sparse_matrix(i = Nace_all_patents_1st_JP_Top4 %>% pull(appln_id),
                                             j = Nace_all_patents_1st_JP_Top4 %>% pull(nace2_code))

mat_tech_1st_JP_Top4 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_1st_JP_Top4_rel_jacc <- relatedness(mat_tech_1st_JP_Top4, method = "Jaccard")
mat_tech_1st_JP_Top4_rel_asso <- relatedness(mat_tech_1st_JP_Top4, method = "association")
mat_tech_1st_JP_Top4_rel_cosi <- relatedness(mat_tech_1st_JP_Top4, method = "cosine")

Relatedness_JP$Jaccard_top4 <- mean(mat_tech_1st_JP_Top4_rel_jacc)
Relatedness_JP$Association_top4 <- mean(mat_tech_1st_JP_Top4_rel_asso)
Relatedness_JP$Cosine_top4<- mean(mat_tech_1st_JP_Top4_rel_cosi)

#then select only the top 7 areas:
Nace_all_patents_1st_JP_Top7 <- Nace_all_patents_1st_JP[Nace_all_patents_1st_JP$nace2_code == "25.3"| 
                                                         Nace_all_patents_1st_JP$nace2_code == "28.23"| 
                                                         Nace_all_patents_1st_JP$nace2_code == "29.3"|
                                                         Nace_all_patents_1st_JP$nace2_code == "26.4"| 
                                                         Nace_all_patents_1st_JP$nace2_code == "27.12"| 
                                                         Nace_all_patents_1st_JP$nace2_code == "27.9"| 
                                                         Nace_all_patents_1st_JP$nace2_code == "28.25", ]

mat_tech_1st_JP_Top7 <- create_sparse_matrix(i = Nace_all_patents_1st_JP_Top7 %>% pull(appln_id),
                                              j = Nace_all_patents_1st_JP_Top7 %>% pull(nace2_code))

mat_tech_1st_JP_Top7 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_1st_JP_Top7_rel_jacc <- relatedness(mat_tech_1st_JP_Top7, method = "Jaccard")
mat_tech_1st_JP_Top7_rel_asso <- relatedness(mat_tech_1st_JP_Top7, method = "association")
mat_tech_1st_JP_Top7_rel_cosi <- relatedness(mat_tech_1st_JP_Top7, method = "cosine")

Relatedness_JP$Jaccard_Top7 <- mean(mat_tech_1st_JP_Top7_rel_jacc)
Relatedness_JP$Association_Top7 <- mean(mat_tech_1st_JP_Top7_rel_asso)
Relatedness_JP$Cosine_Top7<- mean(mat_tech_1st_JP_Top7_rel_cosi)

#then select only the top 8 areas:
Nace_all_patents_1st_JP_Top8 <- Nace_all_patents_1st_JP[Nace_all_patents_1st_JP$nace2_code == "28.23"| 
                                                          Nace_all_patents_1st_JP$nace2_code == "26.7"| 
                                                          Nace_all_patents_1st_JP$nace2_code == "32.9"|
                                                          Nace_all_patents_1st_JP$nace2_code == "26.1"| 
                                                          Nace_all_patents_1st_JP$nace2_code == "27.5"| 
                                                          Nace_all_patents_1st_JP$nace2_code == "27.33"| 
                                                          Nace_all_patents_1st_JP$nace2_code == "27.4"| 
                                                          Nace_all_patents_1st_JP$nace2_code == "26.3", ]

mat_tech_1st_JP_Top8 <- create_sparse_matrix(i = Nace_all_patents_1st_JP_Top8 %>% pull(appln_id),
                                             j = Nace_all_patents_1st_JP_Top8 %>% pull(nace2_code))

mat_tech_1st_JP_Top8 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_1st_JP_Top8_rel_jacc <- relatedness(mat_tech_1st_JP_Top8, method = "Jaccard")
mat_tech_1st_JP_Top8_rel_asso <- relatedness(mat_tech_1st_JP_Top8, method = "association")
mat_tech_1st_JP_Top8_rel_cosi <- relatedness(mat_tech_1st_JP_Top8, method = "cosine")

Relatedness_JP$Jaccard_Top8 <- mean(mat_tech_1st_JP_Top8_rel_jacc)
Relatedness_JP$Association_Top8 <- mean(mat_tech_1st_JP_Top8_rel_asso)
Relatedness_JP$Cosine_Top8<- mean(mat_tech_1st_JP_Top8_rel_cosi)

#and finally the top 3 close to AI areas:
Nace_all_patents_1st_JP_Top3 <- Nace_all_patents_1st_JP[Nace_all_patents_1st_JP$nace2_code == "25.3"| 
                                                        Nace_all_patents_1st_JP$nace2_code == "28.23"| 
                                                        Nace_all_patents_1st_JP$nace2_code == "29.3", ]

mat_tech_1st_JP_Top3 <- create_sparse_matrix(i = Nace_all_patents_1st_JP_Top3 %>% pull(appln_id),
                                             j = Nace_all_patents_1st_JP_Top3 %>% pull(nace2_code))

mat_tech_1st_JP_Top3 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_1st_JP_Top3_rel_jacc <- relatedness(mat_tech_1st_JP_Top3, method = "Jaccard")
mat_tech_1st_JP_Top3_rel_asso <- relatedness(mat_tech_1st_JP_Top3, method = "association")
mat_tech_1st_JP_Top3_rel_cosi <- relatedness(mat_tech_1st_JP_Top3, method = "cosine")

Relatedness_JP$Jaccard_Top3 <- mean(mat_tech_1st_JP_Top3_rel_jacc)
Relatedness_JP$Association_Top3 <- mean(mat_tech_1st_JP_Top3_rel_asso)
Relatedness_JP$Cosine_Top3<- mean(mat_tech_1st_JP_Top3_rel_cosi)

#AI
mat_tech_1st_AI <- create_sparse_matrix(i = Nace_all_patents_1st_AI %>% pull(appln_id),
                                        j = Nace_all_patents_1st_AI %>% pull(nace2_code))

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
Nace_all_patents_1st_AI_Top4 <- Nace_all_patents_1st_AI[Nace_all_patents_1st_AI$nace2_code == "26.2" | 
                                                        Nace_all_patents_1st_AI$nace2_code == "26.5"| 
                                                        Nace_all_patents_1st_AI$nace2_code == "26.51"| 
                                                        Nace_all_patents_1st_AI$nace2_code == "62", ]

mat_tech_1st_AI_Top4 <- create_sparse_matrix(i = Nace_all_patents_1st_AI_Top4 %>% pull(appln_id),
                                             j = Nace_all_patents_1st_AI_Top4 %>% pull(nace2_code))

mat_tech_1st_AI_Top4 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_1st_AI_Top4_rel_jacc <- relatedness(mat_tech_1st_AI_Top4, method = "Jaccard")
mat_tech_1st_AI_Top4_rel_asso <- relatedness(mat_tech_1st_AI_Top4, method = "association")
mat_tech_1st_AI_Top4_rel_cosi <- relatedness(mat_tech_1st_AI_Top4, method = "cosine")

Relatedness_AI$Jaccard_top4 <- mean(mat_tech_1st_AI_Top4_rel_jacc)
Relatedness_AI$Association_top4 <- mean(mat_tech_1st_AI_Top4_rel_asso)
Relatedness_AI$Cosine_top4<- mean(mat_tech_1st_AI_Top4_rel_cosi)

#then select only the top 7 areas:
Nace_all_patents_1st_AI_Top7 <- Nace_all_patents_1st_AI[Nace_all_patents_1st_AI$nace2_code == "25.3"| 
                                                        Nace_all_patents_1st_AI$nace2_code == "28.23"| 
                                                        Nace_all_patents_1st_AI$nace2_code == "29.3"|
                                                        Nace_all_patents_1st_AI$nace2_code == "26.4"| 
                                                        Nace_all_patents_1st_AI$nace2_code == "27.12"| 
                                                        Nace_all_patents_1st_AI$nace2_code == "27.9"| 
                                                        Nace_all_patents_1st_AI$nace2_code == "28.25", ]

mat_tech_1st_AI_Top7 <- create_sparse_matrix(i = Nace_all_patents_1st_AI_Top7 %>% pull(appln_id),
                                             j = Nace_all_patents_1st_AI_Top7 %>% pull(nace2_code))

mat_tech_1st_AI_Top7 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_1st_AI_Top7_rel_jacc <- relatedness(mat_tech_1st_AI_Top7, method = "Jaccard")
mat_tech_1st_AI_Top7_rel_asso <- relatedness(mat_tech_1st_AI_Top7, method = "association")
mat_tech_1st_AI_Top7_rel_cosi <- relatedness(mat_tech_1st_AI_Top7, method = "cosine")

Relatedness_AI$Jaccard_Top7 <- mean(mat_tech_1st_AI_Top7_rel_jacc)
Relatedness_AI$Association_Top7 <- mean(mat_tech_1st_AI_Top7_rel_asso)
Relatedness_AI$Cosine_Top7<- mean(mat_tech_1st_AI_Top7_rel_cosi)

#then select only the top 8 areas:
Nace_all_patents_1st_AI_Top8 <- Nace_all_patents_1st_AI[Nace_all_patents_1st_AI$nace2_code == "28.23"| 
                                                          Nace_all_patents_1st_AI$nace2_code == "26.7"| 
                                                          Nace_all_patents_1st_AI$nace2_code == "32.9"|
                                                          Nace_all_patents_1st_AI$nace2_code == "26.1"| 
                                                          Nace_all_patents_1st_AI$nace2_code == "27.5"| 
                                                          Nace_all_patents_1st_AI$nace2_code == "27.33"| 
                                                          Nace_all_patents_1st_AI$nace2_code == "27.4"| 
                                                          Nace_all_patents_1st_AI$nace2_code == "26.3", ]

mat_tech_1st_AI_Top8 <- create_sparse_matrix(i = Nace_all_patents_1st_AI_Top8 %>% pull(appln_id),
                                             j = Nace_all_patents_1st_AI_Top8 %>% pull(nace2_code))

mat_tech_1st_AI_Top8 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_1st_AI_Top8_rel_jacc <- relatedness(mat_tech_1st_AI_Top8, method = "Jaccard")
mat_tech_1st_AI_Top8_rel_asso <- relatedness(mat_tech_1st_AI_Top8, method = "association")
mat_tech_1st_AI_Top8_rel_cosi <- relatedness(mat_tech_1st_AI_Top8, method = "cosine")

Relatedness_AI$Jaccard_Top8 <- mean(mat_tech_1st_AI_Top8_rel_jacc)
Relatedness_AI$Association_Top8 <- mean(mat_tech_1st_AI_Top8_rel_asso)
Relatedness_AI$Cosine_Top8<- mean(mat_tech_1st_AI_Top8_rel_cosi)

#and finally the top 3 close to AI areas:
Nace_all_patents_1st_AI_Top3 <- Nace_all_patents_1st_AI[Nace_all_patents_1st_AI$nace2_code == "25.3"| 
                                                         Nace_all_patents_1st_AI$nace2_code == "28.23"| 
                                                         Nace_all_patents_1st_AI$nace2_code == "29.3", ]

mat_tech_1st_AI_Top3 <- create_sparse_matrix(i = Nace_all_patents_1st_AI_Top3 %>% pull(appln_id),
                                              j = Nace_all_patents_1st_AI_Top3 %>% pull(nace2_code))

mat_tech_1st_AI_Top3 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_1st_AI_Top3_rel_jacc <- relatedness(mat_tech_1st_AI_Top3, method = "Jaccard")
mat_tech_1st_AI_Top3_rel_asso <- relatedness(mat_tech_1st_AI_Top3, method = "association")
mat_tech_1st_AI_Top3_rel_cosi <- relatedness(mat_tech_1st_AI_Top3, method = "cosine")

Relatedness_AI$Jaccard_Top3 <- mean(mat_tech_1st_AI_Top3_rel_jacc)
Relatedness_AI$Association_Top3 <- mean(mat_tech_1st_AI_Top3_rel_asso)
Relatedness_AI$Cosine_Top3<- mean(mat_tech_1st_AI_Top3_rel_cosi)

#and we merge it all together:
Relatedness_FirstPeriod <- rbind(Relatedness, Relatedness_CN, Relatedness_KR, Relatedness_JP, Relatedness_AI)
Relatedness_FirstPeriod <- Relatedness_FirstPeriod[,c((1:3), (5:16), (4))]

write.csv2(Relatedness_FirstPeriod, file = "Data_calculations_Nace/Relatedness_1st_period_Nace.csv", row.names = TRUE)

#1.2. Second period ----
#Starting with an empty global environment:
rm(list=ls())
setwd("C:/Users/Matheus/Desktop")
#1.2.1.Load the data we need and filter it -----
#The file for the first period is composed of 45,182,803 lines which we will read in 3 parts:
c <- 45233329 -40000000
Nace_all_patents_Part1 <- fread("All_patents_and_Naces_Part2.csv", header = F, nrow = 20000000)[ ,c(-4)]
Nace_all_patents_Part2 <- fread("All_patents_and_Naces_Part2.csv", header = F, nrow = 20000000, skip = 20000000)[ ,c(-4)]
Nace_all_patents_Part3 <- fread("All_patents_and_Naces_Part2.csv", header = F, nrow = c, skip = 40000000)[ ,c(-4)]

a = 1988
b = 2004

Nace_all_patents_Part1 <- Nace_all_patents_Part1[Nace_all_patents_Part1$V5 < b,]
Nace_all_patents_Part1 <- Nace_all_patents_Part1[Nace_all_patents_Part1$V5 > a,]

Nace_all_patents_Part2 <- Nace_all_patents_Part2[Nace_all_patents_Part2$V5 < b,]
Nace_all_patents_Part2 <- Nace_all_patents_Part2[Nace_all_patents_Part2$V5 > a,]

Nace_all_patents_Part3 <- Nace_all_patents_Part3[Nace_all_patents_Part3$V5 < b,]
Nace_all_patents_Part3 <- Nace_all_patents_Part3[Nace_all_patents_Part3$V5 > a,]

Nace_all_patents_2nd <- rbind(Nace_all_patents_Part1, Nace_all_patents_Part2, Nace_all_patents_Part3)
rm(Nace_all_patents_Part1, Nace_all_patents_Part2, Nace_all_patents_Part3)

names(Nace_all_patents_2nd) <- c("appln_id", "ctry_code", "nace2_code", "priority_year")
Nace_all_patents_2nd$ctry_code2 <- Nace_all_patents_2nd$ctry_code

#read AI patents
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
patents_AI_specific <- read.csv("Data_Nace/Nace2_AI csv.csv", sep = ";", header = TRUE, dec=",")
patents_AI_specific$ctry_code <- as.vector(patents_AI_specific$ctry_code)
patents_AI_specific$ctry_code <- "AI_pat"
setDT(patents_AI_specific)
setDT(Nace_all_patents_2nd)
Nace_all_patents_2nd[patents_AI_specific, on = c("appln_id"), ctry_code2 := i.ctry_code]

Nace_all_patents_2nd_US <- Nace_all_patents_2nd[Nace_all_patents_2nd$ctry_code == "US", ]
Nace_all_patents_2nd_CN <- Nace_all_patents_2nd[Nace_all_patents_2nd$ctry_code == "CN", ]
Nace_all_patents_2nd_KR <- Nace_all_patents_2nd[Nace_all_patents_2nd$ctry_code == "KR", ]
Nace_all_patents_2nd_JP <- Nace_all_patents_2nd[Nace_all_patents_2nd$ctry_code == "JP", ]
Nace_all_patents_2nd_AI <- Nace_all_patents_2nd[Nace_all_patents_2nd$ctry_code2 == "AI_pat", ]

#1.2.2. Calculate the Indicators -----
Nace_all_patents_2nd_In <- Nace_all_patents_2nd[,c((-1), (-4), (-5))]
mat_2nd <- as.data.frame(table(Nace_all_patents_2nd_In$ctry_code, Nace_all_patents_2nd_In$nace2_code))
mat_2nd <- get.matrix(mat_2nd)

Indicators <- as.data.frame(Herfindahl(mat_2nd))
names(Indicators) <- "Herfindahl"
mat_2nd_RCAs <- location.quotient(mat_2nd, binary = T)
Indicators$Herfindahl_RCA <- Herfindahl(mat_2nd_RCAs)
Indicators$Entropy <- entropy(mat_2nd)
Indicators$Entropy_RCA <- entropy(mat_2nd_RCAs)
Indicators$Period <- "2nd"

write.csv2(Indicators, file = "Data_calculations_Nace/Indicators_2nd_period_Nace.csv", row.names = TRUE)

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

write.csv2(KnowledgeComp_2nd, file = "Data_calculations_Nace/KnowledgeComp_2nd.csv", row.names = TRUE)
write.csv2(KnowledgeComp_PerCountry_2nd_All, file = "Data_calculations_Nace/KnowledgeComp_PerCountry_2nd_All.csv", row.names = TRUE)
write.csv2(KnowledgeComp_PerCountry_2nd_All_RCAs, file = "Data_calculations_Nace/KnowledgeComp_PerCountry_2nd_All_RCAs.csv", row.names = TRUE)

#For AI complexity and Indicators:
Nace_all_patents_2nd_In <- Nace_all_patents_2nd[,c((-1), (-4), (-2))]
mat_2nd <- as.data.frame(table(Nace_all_patents_2nd_In$ctry_code2, Nace_all_patents_2nd_In$nace2_code))
mat_2nd <- get.matrix(mat_2nd)

Indicators <- as.data.frame(Herfindahl(mat_2nd))
names(Indicators) <- "Herfindahl"
mat_2nd_RCAs <- location.quotient(mat_2nd, binary = T)
Indicators$Herfindahl_RCA <- Herfindahl(mat_2nd_RCAs)
Indicators$Entropy <- entropy(mat_2nd)
Indicators$Entropy_RCA <- entropy(mat_2nd_RCAs)
Indicators$Period <- "2nd"

write.csv2(Indicators, file = "Data_calculations_Nace/Indicators_2nd_period_Nace_AI.csv", row.names = TRUE)
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

write.csv2(KnowledgeComp_PerCountry_2nd_All, file = "Data_calculations_Nace/KnowledgeComp_PerCountry_2nd_All_AI.csv", row.names = TRUE)
write.csv2(KnowledgeComp_PerCountry_2nd_All_RCAs, file = "Data_calculations_Nace/KnowledgeComp_PerCountry_2nd_All_RCAs_AI.csv", row.names = TRUE)

#1.2.3. Calculate the relatedness -----
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
mat_tech_2nd_US <- create_sparse_matrix(i = Nace_all_patents_2nd_US %>% pull(appln_id),
                                        j = Nace_all_patents_2nd_US %>% pull(nace2_code))

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
Nace_all_patents_2nd_US_Top4 <- Nace_all_patents_2nd_US[Nace_all_patents_2nd_US$nace2_code == "26.2" | 
                                                        Nace_all_patents_2nd_US$nace2_code == "26.5"| 
                                                        Nace_all_patents_2nd_US$nace2_code == "26.51"| 
                                                        Nace_all_patents_2nd_US$nace2_code == "62", ]

mat_tech_2nd_US_Top4 <- create_sparse_matrix(i = Nace_all_patents_2nd_US_Top4 %>% pull(appln_id),
                                             j = Nace_all_patents_2nd_US_Top4 %>% pull(nace2_code))

mat_tech_2nd_US_Top4 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_2nd_US_Top4_rel_jacc <- relatedness(mat_tech_2nd_US_Top4, method = "Jaccard")
mat_tech_2nd_US_Top4_rel_asso <- relatedness(mat_tech_2nd_US_Top4, method = "association")
mat_tech_2nd_US_Top4_rel_cosi <- relatedness(mat_tech_2nd_US_Top4, method = "cosine")

Relatedness$Jaccard_top4 <- mean(mat_tech_2nd_US_Top4_rel_jacc)
Relatedness$Association_top4 <- mean(mat_tech_2nd_US_Top4_rel_asso)
Relatedness$Cosine_top4<- mean(mat_tech_2nd_US_Top4_rel_cosi)

#then select only the top 7 areas:
Nace_all_patents_2nd_US_Top7 <- Nace_all_patents_2nd_US[Nace_all_patents_2nd_US$nace2_code == "25.3"| 
                                                          Nace_all_patents_2nd_US$nace2_code == "28.23"| 
                                                          Nace_all_patents_2nd_US$nace2_code == "29.3"|
                                                          Nace_all_patents_2nd_US$nace2_code == "26.4"| 
                                                          Nace_all_patents_2nd_US$nace2_code == "27.12"| 
                                                          Nace_all_patents_2nd_US$nace2_code == "27.9"| 
                                                          Nace_all_patents_2nd_US$nace2_code == "28.25", ]

mat_tech_2nd_US_Top7 <- create_sparse_matrix(i = Nace_all_patents_2nd_US_Top7 %>% pull(appln_id),
                                             j = Nace_all_patents_2nd_US_Top7 %>% pull(nace2_code))

mat_tech_2nd_US_Top7 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_2nd_US_Top7_rel_jacc <- relatedness(mat_tech_2nd_US_Top7, method = "Jaccard")
mat_tech_2nd_US_Top7_rel_asso <- relatedness(mat_tech_2nd_US_Top7, method = "association")
mat_tech_2nd_US_Top7_rel_cosi <- relatedness(mat_tech_2nd_US_Top7, method = "cosine")

Relatedness$Jaccard_Top7 <- mean(mat_tech_2nd_US_Top7_rel_jacc)
Relatedness$Association_Top7 <- mean(mat_tech_2nd_US_Top7_rel_asso)
Relatedness$Cosine_Top7<- mean(mat_tech_2nd_US_Top7_rel_cosi)

#then select only the top 8 areas:
Nace_all_patents_2nd_US_Top8 <- Nace_all_patents_2nd_US[Nace_all_patents_2nd_US$nace2_code == "28.23"| 
                                                          Nace_all_patents_2nd_US$nace2_code == "26.7"| 
                                                          Nace_all_patents_2nd_US$nace2_code == "32.9"|
                                                          Nace_all_patents_2nd_US$nace2_code == "26.1"| 
                                                          Nace_all_patents_2nd_US$nace2_code == "27.5"| 
                                                          Nace_all_patents_2nd_US$nace2_code == "27.33"| 
                                                          Nace_all_patents_2nd_US$nace2_code == "27.4"| 
                                                          Nace_all_patents_2nd_US$nace2_code == "26.3", ]

mat_tech_2nd_US_Top8 <- create_sparse_matrix(i = Nace_all_patents_2nd_US_Top8 %>% pull(appln_id),
                                             j = Nace_all_patents_2nd_US_Top8 %>% pull(nace2_code))

mat_tech_2nd_US_Top8 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_2nd_US_Top8_rel_jacc <- relatedness(mat_tech_2nd_US_Top8, method = "Jaccard")
mat_tech_2nd_US_Top8_rel_asso <- relatedness(mat_tech_2nd_US_Top8, method = "association")
mat_tech_2nd_US_Top8_rel_cosi <- relatedness(mat_tech_2nd_US_Top8, method = "cosine")

Relatedness$Jaccard_Top8 <- mean(mat_tech_2nd_US_Top8_rel_jacc)
Relatedness$Association_Top8 <- mean(mat_tech_2nd_US_Top8_rel_asso)
Relatedness$Cosine_Top8<- mean(mat_tech_2nd_US_Top8_rel_cosi)

#and finally the top 3 close to AI areas:
Nace_all_patents_2nd_US_Top3 <- Nace_all_patents_2nd_US[Nace_all_patents_2nd_US$nace2_code == "25.3"| 
                                                          Nace_all_patents_2nd_US$nace2_code == "28.23"| 
                                                          Nace_all_patents_2nd_US$nace2_code == "29.3", ]

mat_tech_2nd_US_Top3 <- create_sparse_matrix(i = Nace_all_patents_2nd_US_Top3 %>% pull(appln_id),
                                             j = Nace_all_patents_2nd_US_Top3 %>% pull(nace2_code))

mat_tech_2nd_US_Top3 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_2nd_US_Top3_rel_jacc <- relatedness(mat_tech_2nd_US_Top3, method = "Jaccard")
mat_tech_2nd_US_Top3_rel_asso <- relatedness(mat_tech_2nd_US_Top3, method = "association")
mat_tech_2nd_US_Top3_rel_cosi <- relatedness(mat_tech_2nd_US_Top3, method = "cosine")

Relatedness$Jaccard_Top3 <- mean(mat_tech_2nd_US_Top3_rel_jacc)
Relatedness$Association_Top3 <- mean(mat_tech_2nd_US_Top3_rel_asso)
Relatedness$Cosine_Top3<- mean(mat_tech_2nd_US_Top3_rel_cosi)

#China:
mat_tech_2nd_CN <- create_sparse_matrix(i = Nace_all_patents_2nd_CN %>% pull(appln_id),
                                        j = Nace_all_patents_2nd_CN %>% pull(nace2_code))

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
Nace_all_patents_2nd_CN_Top4 <- Nace_all_patents_2nd_CN[Nace_all_patents_2nd_CN$nace2_code == "26.2" | 
                                                          Nace_all_patents_2nd_CN$nace2_code == "26.5"| 
                                                          Nace_all_patents_2nd_CN$nace2_code == "26.51"| 
                                                          Nace_all_patents_2nd_CN$nace2_code == "62", ]

mat_tech_2nd_CN_Top4 <- create_sparse_matrix(i = Nace_all_patents_2nd_CN_Top4 %>% pull(appln_id),
                                             j = Nace_all_patents_2nd_CN_Top4 %>% pull(nace2_code))

mat_tech_2nd_CN_Top4 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_2nd_CN_Top4_rel_jacc <- relatedness(mat_tech_2nd_CN_Top4, method = "Jaccard")
mat_tech_2nd_CN_Top4_rel_asso <- relatedness(mat_tech_2nd_CN_Top4, method = "association")
mat_tech_2nd_CN_Top4_rel_cosi <- relatedness(mat_tech_2nd_CN_Top4, method = "cosine")

Relatedness_CN$Jaccard_top4 <- mean(mat_tech_2nd_CN_Top4_rel_jacc)
Relatedness_CN$Association_top4 <- mean(mat_tech_2nd_CN_Top4_rel_asso)
Relatedness_CN$Cosine_top4<- mean(mat_tech_2nd_CN_Top4_rel_cosi)

#then select only the top 7 areas:
Nace_all_patents_2nd_CN_Top7 <- Nace_all_patents_2nd_CN[Nace_all_patents_2nd_CN$nace2_code == "25.3"| 
                                                          Nace_all_patents_2nd_CN$nace2_code == "28.23"| 
                                                          Nace_all_patents_2nd_CN$nace2_code == "29.3"|
                                                          Nace_all_patents_2nd_CN$nace2_code == "26.4"| 
                                                          Nace_all_patents_2nd_CN$nace2_code == "27.12"| 
                                                          Nace_all_patents_2nd_CN$nace2_code == "27.9"| 
                                                          Nace_all_patents_2nd_CN$nace2_code == "28.25", ]

mat_tech_2nd_CN_Top7 <- create_sparse_matrix(i = Nace_all_patents_2nd_CN_Top7 %>% pull(appln_id),
                                             j = Nace_all_patents_2nd_CN_Top7 %>% pull(nace2_code))

mat_tech_2nd_CN_Top7 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_2nd_CN_Top7_rel_jacc <- relatedness(mat_tech_2nd_CN_Top7, method = "Jaccard")
mat_tech_2nd_CN_Top7_rel_asso <- relatedness(mat_tech_2nd_CN_Top7, method = "association")
mat_tech_2nd_CN_Top7_rel_cosi <- relatedness(mat_tech_2nd_CN_Top7, method = "cosine")

Relatedness_CN$Jaccard_Top7 <- mean(mat_tech_2nd_CN_Top7_rel_jacc)
Relatedness_CN$Association_Top7 <- mean(mat_tech_2nd_CN_Top7_rel_asso)
Relatedness_CN$Cosine_Top7<- mean(mat_tech_2nd_CN_Top7_rel_cosi)

#then select only the top 8 areas:
Nace_all_patents_2nd_CN_Top8 <- Nace_all_patents_2nd_CN[Nace_all_patents_2nd_CN$nace2_code == "28.23"| 
                                                          Nace_all_patents_2nd_CN$nace2_code == "26.7"| 
                                                          Nace_all_patents_2nd_CN$nace2_code == "32.9"|
                                                          Nace_all_patents_2nd_CN$nace2_code == "26.1"| 
                                                          Nace_all_patents_2nd_CN$nace2_code == "27.5"| 
                                                          Nace_all_patents_2nd_CN$nace2_code == "27.33"| 
                                                          Nace_all_patents_2nd_CN$nace2_code == "27.4"| 
                                                          Nace_all_patents_2nd_CN$nace2_code == "26.3", ]

mat_tech_2nd_CN_Top8 <- create_sparse_matrix(i = Nace_all_patents_2nd_CN_Top8 %>% pull(appln_id),
                                             j = Nace_all_patents_2nd_CN_Top8 %>% pull(nace2_code))

mat_tech_2nd_CN_Top8 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_2nd_CN_Top8_rel_jacc <- relatedness(mat_tech_2nd_CN_Top8, method = "Jaccard")
mat_tech_2nd_CN_Top8_rel_asso <- relatedness(mat_tech_2nd_CN_Top8, method = "association")
mat_tech_2nd_CN_Top8_rel_cosi <- relatedness(mat_tech_2nd_CN_Top8, method = "cosine")

Relatedness_CN$Jaccard_Top8 <- mean(mat_tech_2nd_CN_Top8_rel_jacc)
Relatedness_CN$Association_Top8 <- mean(mat_tech_2nd_CN_Top8_rel_asso)
Relatedness_CN$Cosine_Top8<- mean(mat_tech_2nd_CN_Top8_rel_cosi)

#and finally the top 3 close to AI areas:
Nace_all_patents_2nd_CN_Top3 <- Nace_all_patents_2nd_CN[Nace_all_patents_2nd_CN$nace2_code == "25.3"| 
                                                          Nace_all_patents_2nd_CN$nace2_code == "28.23"| 
                                                          Nace_all_patents_2nd_CN$nace2_code == "29.3", ]

mat_tech_2nd_CN_Top3 <- create_sparse_matrix(i = Nace_all_patents_2nd_CN_Top3 %>% pull(appln_id),
                                             j = Nace_all_patents_2nd_CN_Top3 %>% pull(nace2_code))

mat_tech_2nd_CN_Top3 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_2nd_CN_Top3_rel_jacc <- relatedness(mat_tech_2nd_CN_Top3, method = "Jaccard")
mat_tech_2nd_CN_Top3_rel_asso <- relatedness(mat_tech_2nd_CN_Top3, method = "association")
mat_tech_2nd_CN_Top3_rel_cosi <- relatedness(mat_tech_2nd_CN_Top3, method = "cosine")

Relatedness_CN$Jaccard_Top3 <- mean(mat_tech_2nd_CN_Top3_rel_jacc)
Relatedness_CN$Association_Top3 <- mean(mat_tech_2nd_CN_Top3_rel_asso)
Relatedness_CN$Cosine_Top3<- mean(mat_tech_2nd_CN_Top3_rel_cosi)


#KR
mat_tech_2nd_KR <- create_sparse_matrix(i = Nace_all_patents_2nd_KR %>% pull(appln_id),
                                        j = Nace_all_patents_2nd_KR %>% pull(nace2_code))

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
Nace_all_patents_2nd_KR_Top4 <- Nace_all_patents_2nd_KR[Nace_all_patents_2nd_KR$nace2_code == "26.2" | 
                                                          Nace_all_patents_2nd_KR$nace2_code == "26.5"| 
                                                          Nace_all_patents_2nd_KR$nace2_code == "26.51"| 
                                                          Nace_all_patents_2nd_KR$nace2_code == "62", ]

mat_tech_2nd_KR_Top4 <- create_sparse_matrix(i = Nace_all_patents_2nd_KR_Top4 %>% pull(appln_id),
                                             j = Nace_all_patents_2nd_KR_Top4 %>% pull(nace2_code))

mat_tech_2nd_KR_Top4 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_2nd_KR_Top4_rel_jacc <- relatedness(mat_tech_2nd_KR_Top4, method = "Jaccard")
mat_tech_2nd_KR_Top4_rel_asso <- relatedness(mat_tech_2nd_KR_Top4, method = "association")
mat_tech_2nd_KR_Top4_rel_cosi <- relatedness(mat_tech_2nd_KR_Top4, method = "cosine")

Relatedness_KR$Jaccard_top4 <- mean(mat_tech_2nd_KR_Top4_rel_jacc)
Relatedness_KR$Association_top4 <- mean(mat_tech_2nd_KR_Top4_rel_asso)
Relatedness_KR$Cosine_top4<- mean(mat_tech_2nd_KR_Top4_rel_cosi)

#then select only the top 7 areas:
Nace_all_patents_2nd_KR_Top7 <- Nace_all_patents_2nd_KR[Nace_all_patents_2nd_KR$nace2_code == "25.3"| 
                                                          Nace_all_patents_2nd_KR$nace2_code == "28.23"| 
                                                          Nace_all_patents_2nd_KR$nace2_code == "29.3"|
                                                          Nace_all_patents_2nd_KR$nace2_code == "26.4"| 
                                                          Nace_all_patents_2nd_KR$nace2_code == "27.12"| 
                                                          Nace_all_patents_2nd_KR$nace2_code == "27.9"| 
                                                          Nace_all_patents_2nd_KR$nace2_code == "28.25", ]

mat_tech_2nd_KR_Top7 <- create_sparse_matrix(i = Nace_all_patents_2nd_KR_Top7 %>% pull(appln_id),
                                             j = Nace_all_patents_2nd_KR_Top7 %>% pull(nace2_code))

mat_tech_2nd_KR_Top7 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_2nd_KR_Top7_rel_jacc <- relatedness(mat_tech_2nd_KR_Top7, method = "Jaccard")
mat_tech_2nd_KR_Top7_rel_asso <- relatedness(mat_tech_2nd_KR_Top7, method = "association")
mat_tech_2nd_KR_Top7_rel_cosi <- relatedness(mat_tech_2nd_KR_Top7, method = "cosine")

Relatedness_KR$Jaccard_Top7 <- mean(mat_tech_2nd_KR_Top7_rel_jacc)
Relatedness_KR$Association_Top7 <- mean(mat_tech_2nd_KR_Top7_rel_asso)
Relatedness_KR$Cosine_Top7<- mean(mat_tech_2nd_KR_Top7_rel_cosi)

#then select only the top 8 areas:
Nace_all_patents_2nd_KR_Top8 <- Nace_all_patents_2nd_KR[Nace_all_patents_2nd_KR$nace2_code == "28.23"| 
                                                          Nace_all_patents_2nd_KR$nace2_code == "26.7"| 
                                                          Nace_all_patents_2nd_KR$nace2_code == "32.9"|
                                                          Nace_all_patents_2nd_KR$nace2_code == "26.1"| 
                                                          Nace_all_patents_2nd_KR$nace2_code == "27.5"| 
                                                          Nace_all_patents_2nd_KR$nace2_code == "27.33"| 
                                                          Nace_all_patents_2nd_KR$nace2_code == "27.4"| 
                                                          Nace_all_patents_2nd_KR$nace2_code == "26.3", ]

mat_tech_2nd_KR_Top8 <- create_sparse_matrix(i = Nace_all_patents_2nd_KR_Top8 %>% pull(appln_id),
                                             j = Nace_all_patents_2nd_KR_Top8 %>% pull(nace2_code))

mat_tech_2nd_KR_Top8 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_2nd_KR_Top8_rel_jacc <- relatedness(mat_tech_2nd_KR_Top8, method = "Jaccard")
mat_tech_2nd_KR_Top8_rel_asso <- relatedness(mat_tech_2nd_KR_Top8, method = "association")
mat_tech_2nd_KR_Top8_rel_cosi <- relatedness(mat_tech_2nd_KR_Top8, method = "cosine")

Relatedness_KR$Jaccard_Top8 <- mean(mat_tech_2nd_KR_Top8_rel_jacc)
Relatedness_KR$Association_Top8 <- mean(mat_tech_2nd_KR_Top8_rel_asso)
Relatedness_KR$Cosine_Top8<- mean(mat_tech_2nd_KR_Top8_rel_cosi)

#and finally the top 3 close to AI areas:
Nace_all_patents_2nd_KR_Top3 <- Nace_all_patents_2nd_KR[Nace_all_patents_2nd_KR$nace2_code == "25.3"| 
                                                          Nace_all_patents_2nd_KR$nace2_code == "28.23"| 
                                                          Nace_all_patents_2nd_KR$nace2_code == "29.3", ]

mat_tech_2nd_KR_Top3 <- create_sparse_matrix(i = Nace_all_patents_2nd_KR_Top3 %>% pull(appln_id),
                                             j = Nace_all_patents_2nd_KR_Top3 %>% pull(nace2_code))

mat_tech_2nd_KR_Top3 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_2nd_KR_Top3_rel_jacc <- relatedness(mat_tech_2nd_KR_Top3, method = "Jaccard")
mat_tech_2nd_KR_Top3_rel_asso <- relatedness(mat_tech_2nd_KR_Top3, method = "association")
mat_tech_2nd_KR_Top3_rel_cosi <- relatedness(mat_tech_2nd_KR_Top3, method = "cosine")

Relatedness_KR$Jaccard_Top3 <- mean(mat_tech_2nd_KR_Top3_rel_jacc)
Relatedness_KR$Association_Top3 <- mean(mat_tech_2nd_KR_Top3_rel_asso)
Relatedness_KR$Cosine_Top3<- mean(mat_tech_2nd_KR_Top3_rel_cosi)

#Japan
mat_tech_2nd_JP <- create_sparse_matrix(i = Nace_all_patents_2nd_JP %>% pull(appln_id),
                                        j = Nace_all_patents_2nd_JP %>% pull(nace2_code))

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
Nace_all_patents_2nd_JP_Top4 <- Nace_all_patents_2nd_JP[Nace_all_patents_2nd_JP$nace2_code == "26.2" | 
                                                          Nace_all_patents_2nd_JP$nace2_code == "26.5"| 
                                                          Nace_all_patents_2nd_JP$nace2_code == "26.51"| 
                                                          Nace_all_patents_2nd_JP$nace2_code == "62", ]

mat_tech_2nd_JP_Top4 <- create_sparse_matrix(i = Nace_all_patents_2nd_JP_Top4 %>% pull(appln_id),
                                             j = Nace_all_patents_2nd_JP_Top4 %>% pull(nace2_code))

mat_tech_2nd_JP_Top4 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_2nd_JP_Top4_rel_jacc <- relatedness(mat_tech_2nd_JP_Top4, method = "Jaccard")
mat_tech_2nd_JP_Top4_rel_asso <- relatedness(mat_tech_2nd_JP_Top4, method = "association")
mat_tech_2nd_JP_Top4_rel_cosi <- relatedness(mat_tech_2nd_JP_Top4, method = "cosine")

Relatedness_JP$Jaccard_top4 <- mean(mat_tech_2nd_JP_Top4_rel_jacc)
Relatedness_JP$Association_top4 <- mean(mat_tech_2nd_JP_Top4_rel_asso)
Relatedness_JP$Cosine_top4<- mean(mat_tech_2nd_JP_Top4_rel_cosi)

#then select only the top 7 areas:
Nace_all_patents_2nd_JP_Top7 <- Nace_all_patents_2nd_JP[Nace_all_patents_2nd_JP$nace2_code == "25.3"| 
                                                          Nace_all_patents_2nd_JP$nace2_code == "28.23"| 
                                                          Nace_all_patents_2nd_JP$nace2_code == "29.3"|
                                                          Nace_all_patents_2nd_JP$nace2_code == "26.4"| 
                                                          Nace_all_patents_2nd_JP$nace2_code == "27.12"| 
                                                          Nace_all_patents_2nd_JP$nace2_code == "27.9"| 
                                                          Nace_all_patents_2nd_JP$nace2_code == "28.25", ]

mat_tech_2nd_JP_Top7 <- create_sparse_matrix(i = Nace_all_patents_2nd_JP_Top7 %>% pull(appln_id),
                                             j = Nace_all_patents_2nd_JP_Top7 %>% pull(nace2_code))

mat_tech_2nd_JP_Top7 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_2nd_JP_Top7_rel_jacc <- relatedness(mat_tech_2nd_JP_Top7, method = "Jaccard")
mat_tech_2nd_JP_Top7_rel_asso <- relatedness(mat_tech_2nd_JP_Top7, method = "association")
mat_tech_2nd_JP_Top7_rel_cosi <- relatedness(mat_tech_2nd_JP_Top7, method = "cosine")

Relatedness_JP$Jaccard_Top7 <- mean(mat_tech_2nd_JP_Top7_rel_jacc)
Relatedness_JP$Association_Top7 <- mean(mat_tech_2nd_JP_Top7_rel_asso)
Relatedness_JP$Cosine_Top7<- mean(mat_tech_2nd_JP_Top7_rel_cosi)

#then select only the top 8 areas:
Nace_all_patents_2nd_JP_Top8 <- Nace_all_patents_2nd_JP[Nace_all_patents_2nd_JP$nace2_code == "28.23"| 
                                                          Nace_all_patents_2nd_JP$nace2_code == "26.7"| 
                                                          Nace_all_patents_2nd_JP$nace2_code == "32.9"|
                                                          Nace_all_patents_2nd_JP$nace2_code == "26.1"| 
                                                          Nace_all_patents_2nd_JP$nace2_code == "27.5"| 
                                                          Nace_all_patents_2nd_JP$nace2_code == "27.33"| 
                                                          Nace_all_patents_2nd_JP$nace2_code == "27.4"| 
                                                          Nace_all_patents_2nd_JP$nace2_code == "26.3", ]

mat_tech_2nd_JP_Top8 <- create_sparse_matrix(i = Nace_all_patents_2nd_JP_Top8 %>% pull(appln_id),
                                             j = Nace_all_patents_2nd_JP_Top8 %>% pull(nace2_code))

mat_tech_2nd_JP_Top8 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_2nd_JP_Top8_rel_jacc <- relatedness(mat_tech_2nd_JP_Top8, method = "Jaccard")
mat_tech_2nd_JP_Top8_rel_asso <- relatedness(mat_tech_2nd_JP_Top8, method = "association")
mat_tech_2nd_JP_Top8_rel_cosi <- relatedness(mat_tech_2nd_JP_Top8, method = "cosine")

Relatedness_JP$Jaccard_Top8 <- mean(mat_tech_2nd_JP_Top8_rel_jacc)
Relatedness_JP$Association_Top8 <- mean(mat_tech_2nd_JP_Top8_rel_asso)
Relatedness_JP$Cosine_Top8<- mean(mat_tech_2nd_JP_Top8_rel_cosi)

#and finally the top 3 close to AI areas:
Nace_all_patents_2nd_JP_Top3 <- Nace_all_patents_2nd_JP[Nace_all_patents_2nd_JP$nace2_code == "25.3"| 
                                                          Nace_all_patents_2nd_JP$nace2_code == "28.23"| 
                                                          Nace_all_patents_2nd_JP$nace2_code == "29.3", ]

mat_tech_2nd_JP_Top3 <- create_sparse_matrix(i = Nace_all_patents_2nd_JP_Top3 %>% pull(appln_id),
                                             j = Nace_all_patents_2nd_JP_Top3 %>% pull(nace2_code))

mat_tech_2nd_JP_Top3 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_2nd_JP_Top3_rel_jacc <- relatedness(mat_tech_2nd_JP_Top3, method = "Jaccard")
mat_tech_2nd_JP_Top3_rel_asso <- relatedness(mat_tech_2nd_JP_Top3, method = "association")
mat_tech_2nd_JP_Top3_rel_cosi <- relatedness(mat_tech_2nd_JP_Top3, method = "cosine")

Relatedness_JP$Jaccard_Top3 <- mean(mat_tech_2nd_JP_Top3_rel_jacc)
Relatedness_JP$Association_Top3 <- mean(mat_tech_2nd_JP_Top3_rel_asso)
Relatedness_JP$Cosine_Top3<- mean(mat_tech_2nd_JP_Top3_rel_cosi)

#AI
mat_tech_2nd_AI <- create_sparse_matrix(i = Nace_all_patents_2nd_AI %>% pull(appln_id),
                                        j = Nace_all_patents_2nd_AI %>% pull(nace2_code))

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
Nace_all_patents_2nd_AI_Top4 <- Nace_all_patents_2nd_AI[Nace_all_patents_2nd_AI$nace2_code == "26.2" | 
                                                          Nace_all_patents_2nd_AI$nace2_code == "26.5"| 
                                                          Nace_all_patents_2nd_AI$nace2_code == "26.51"| 
                                                          Nace_all_patents_2nd_AI$nace2_code == "62", ]

mat_tech_2nd_AI_Top4 <- create_sparse_matrix(i = Nace_all_patents_2nd_AI_Top4 %>% pull(appln_id),
                                             j = Nace_all_patents_2nd_AI_Top4 %>% pull(nace2_code))

mat_tech_2nd_AI_Top4 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_2nd_AI_Top4_rel_jacc <- relatedness(mat_tech_2nd_AI_Top4, method = "Jaccard")
mat_tech_2nd_AI_Top4_rel_asso <- relatedness(mat_tech_2nd_AI_Top4, method = "association")
mat_tech_2nd_AI_Top4_rel_cosi <- relatedness(mat_tech_2nd_AI_Top4, method = "cosine")

Relatedness_AI$Jaccard_top4 <- mean(mat_tech_2nd_AI_Top4_rel_jacc)
Relatedness_AI$Association_top4 <- mean(mat_tech_2nd_AI_Top4_rel_asso)
Relatedness_AI$Cosine_top4<- mean(mat_tech_2nd_AI_Top4_rel_cosi)

#then select only the top 7 areas:
Nace_all_patents_2nd_AI_Top7 <- Nace_all_patents_2nd_AI[Nace_all_patents_2nd_AI$nace2_code == "25.3"| 
                                                          Nace_all_patents_2nd_AI$nace2_code == "28.23"| 
                                                          Nace_all_patents_2nd_AI$nace2_code == "29.3"|
                                                          Nace_all_patents_2nd_AI$nace2_code == "26.4"| 
                                                          Nace_all_patents_2nd_AI$nace2_code == "27.12"| 
                                                          Nace_all_patents_2nd_AI$nace2_code == "27.9"| 
                                                          Nace_all_patents_2nd_AI$nace2_code == "28.25", ]

mat_tech_2nd_AI_Top7 <- create_sparse_matrix(i = Nace_all_patents_2nd_AI_Top7 %>% pull(appln_id),
                                             j = Nace_all_patents_2nd_AI_Top7 %>% pull(nace2_code))

mat_tech_2nd_AI_Top7 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_2nd_AI_Top7_rel_jacc <- relatedness(mat_tech_2nd_AI_Top7, method = "Jaccard")
mat_tech_2nd_AI_Top7_rel_asso <- relatedness(mat_tech_2nd_AI_Top7, method = "association")
mat_tech_2nd_AI_Top7_rel_cosi <- relatedness(mat_tech_2nd_AI_Top7, method = "cosine")

Relatedness_AI$Jaccard_Top7 <- mean(mat_tech_2nd_AI_Top7_rel_jacc)
Relatedness_AI$Association_Top7 <- mean(mat_tech_2nd_AI_Top7_rel_asso)
Relatedness_AI$Cosine_Top7<- mean(mat_tech_2nd_AI_Top7_rel_cosi)

#then select only the top 8 areas:
Nace_all_patents_2nd_AI_Top8 <- Nace_all_patents_2nd_AI[Nace_all_patents_2nd_AI$nace2_code == "28.23"| 
                                                          Nace_all_patents_2nd_AI$nace2_code == "26.7"| 
                                                          Nace_all_patents_2nd_AI$nace2_code == "32.9"|
                                                          Nace_all_patents_2nd_AI$nace2_code == "26.1"| 
                                                          Nace_all_patents_2nd_AI$nace2_code == "27.5"| 
                                                          Nace_all_patents_2nd_AI$nace2_code == "27.33"| 
                                                          Nace_all_patents_2nd_AI$nace2_code == "27.4"| 
                                                          Nace_all_patents_2nd_AI$nace2_code == "26.3", ]

mat_tech_2nd_AI_Top8 <- create_sparse_matrix(i = Nace_all_patents_2nd_AI_Top8 %>% pull(appln_id),
                                             j = Nace_all_patents_2nd_AI_Top8 %>% pull(nace2_code))

mat_tech_2nd_AI_Top8 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_2nd_AI_Top8_rel_jacc <- relatedness(mat_tech_2nd_AI_Top8, method = "Jaccard")
mat_tech_2nd_AI_Top8_rel_asso <- relatedness(mat_tech_2nd_AI_Top8, method = "association")
mat_tech_2nd_AI_Top8_rel_cosi <- relatedness(mat_tech_2nd_AI_Top8, method = "cosine")

Relatedness_AI$Jaccard_Top8 <- mean(mat_tech_2nd_AI_Top8_rel_jacc)
Relatedness_AI$Association_Top8 <- mean(mat_tech_2nd_AI_Top8_rel_asso)
Relatedness_AI$Cosine_Top8<- mean(mat_tech_2nd_AI_Top8_rel_cosi)

#and finally the top 3 close to AI areas:
Nace_all_patents_2nd_AI_Top3 <- Nace_all_patents_2nd_AI[Nace_all_patents_2nd_AI$nace2_code == "25.3"| 
                                                          Nace_all_patents_2nd_AI$nace2_code == "28.23"| 
                                                          Nace_all_patents_2nd_AI$nace2_code == "29.3", ]

mat_tech_2nd_AI_Top3 <- create_sparse_matrix(i = Nace_all_patents_2nd_AI_Top3 %>% pull(appln_id),
                                             j = Nace_all_patents_2nd_AI_Top3 %>% pull(nace2_code))

mat_tech_2nd_AI_Top3 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_2nd_AI_Top3_rel_jacc <- relatedness(mat_tech_2nd_AI_Top3, method = "Jaccard")
mat_tech_2nd_AI_Top3_rel_asso <- relatedness(mat_tech_2nd_AI_Top3, method = "association")
mat_tech_2nd_AI_Top3_rel_cosi <- relatedness(mat_tech_2nd_AI_Top3, method = "cosine")

Relatedness_AI$Jaccard_Top3 <- mean(mat_tech_2nd_AI_Top3_rel_jacc)
Relatedness_AI$Association_Top3 <- mean(mat_tech_2nd_AI_Top3_rel_asso)
Relatedness_AI$Cosine_Top3<- mean(mat_tech_2nd_AI_Top3_rel_cosi)

#and we merge it all together:
Relatedness_SecondPeriod <- rbind(Relatedness, Relatedness_CN, Relatedness_KR, Relatedness_JP, Relatedness_AI)
Relatedness_SecondPeriod <- Relatedness_SecondPeriod[,c((1:3), (5:16), (4))]

write.csv2(Relatedness_SecondPeriod, file = "Data_calculations_Nace/Relatedness_2nd_period_Nace.csv", row.names = TRUE)

#1.3. Third period ----
#Starting with an empty global environment:
rm(list=ls())
setwd("C:/Users/Matheus/Desktop")
#1.3.1.Load the data we need and filter it -----
#The file for the first period is composed of 58,841,893 lines which we will read in 3 parts:
c <- 58935336 -40000000
Nace_all_patents_Part1 <- fread("All_patents_and_Naces_Part1.csv", header = F, nrow = 20000000)[ ,c(-4)]
Nace_all_patents_Part2 <- fread("All_patents_and_Naces_Part1.csv", header = F, nrow = 20000000, skip = 20000000)[ ,c(-4)]
Nace_all_patents_Part3 <- fread("All_patents_and_Naces_Part1.csv", header = F, nrow = c, skip = 40000000)[ ,c(-4)]

Nace_all_patents_3rd <- rbind(Nace_all_patents_Part1, Nace_all_patents_Part2, Nace_all_patents_Part3)
rm(Nace_all_patents_Part1, Nace_all_patents_Part2, Nace_all_patents_Part3)
names(Nace_all_patents_3rd) <- c("appln_id", "ctry_code", "nace2_code", "priority_year")
Nace_all_patents_3rd$ctry_code2 <- Nace_all_patents_3rd$ctry_code

#read AI patents
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
patents_AI_specific <- read.csv("Data_Nace/Nace2_AI csv.csv", sep = ";", header = TRUE, dec=",")
patents_AI_specific$ctry_code <- as.vector(patents_AI_specific$ctry_code)
patents_AI_specific$ctry_code <- "AI_pat"
setDT(patents_AI_specific)
setDT(Nace_all_patents_3rd)
Nace_all_patents_3rd[patents_AI_specific, on = c("appln_id"), ctry_code2 := i.ctry_code]

Nace_all_patents_3rd_US <- Nace_all_patents_3rd[Nace_all_patents_3rd$ctry_code == "US", ]
Nace_all_patents_3rd_CN <- Nace_all_patents_3rd[Nace_all_patents_3rd$ctry_code == "CN", ]
Nace_all_patents_3rd_KR <- Nace_all_patents_3rd[Nace_all_patents_3rd$ctry_code == "KR", ]
Nace_all_patents_3rd_JP <- Nace_all_patents_3rd[Nace_all_patents_3rd$ctry_code == "JP", ]
Nace_all_patents_3rd_AI <- Nace_all_patents_3rd[Nace_all_patents_3rd$ctry_code2 == "AI_pat", ]

#1.2.2. Calculate the Indicators -----
Nace_all_patents_3rd_In <- Nace_all_patents_3rd[,c((-1), (-4), (-5))]
mat_3rd <- as.data.frame(table(Nace_all_patents_3rd_In$ctry_code, Nace_all_patents_3rd_In$nace2_code))
mat_3rd <- get.matrix(mat_3rd)

Indicators <- as.data.frame(Herfindahl(mat_3rd))
names(Indicators) <- "Herfindahl"
mat_3rd_RCAs <- location.quotient(mat_3rd, binary = T)
Indicators$Herfindahl_RCA <- Herfindahl(mat_3rd_RCAs)
Indicators$Entropy <- entropy(mat_3rd)
Indicators$Entropy_RCA <- entropy(mat_3rd_RCAs)
Indicators$Period <- "3rd"

write.csv2(Indicators, file = "Data_calculations_Nace/Indicators_3rd_period_Nace.csv", row.names = TRUE)

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

write.csv2(KnowledgeComp_3rd, file = "Data_calculations_Nace/KnowledgeComp_3rd.csv", row.names = TRUE)
write.csv2(KnowledgeComp_PerCountry_3rd_All, file = "Data_calculations_Nace/KnowledgeComp_PerCountry_3rd_All.csv", row.names = TRUE)
write.csv2(KnowledgeComp_PerCountry_3rd_All_RCAs, file = "Data_calculations_Nace/KnowledgeComp_PerCountry_3rd_All_RCAs.csv", row.names = TRUE)

#For AI complexity and Indicators:
Nace_all_patents_3rd_In <- Nace_all_patents_3rd[,c((-1), (-4), (-2))]
mat_3rd <- as.data.frame(table(Nace_all_patents_3rd_In$ctry_code2, Nace_all_patents_3rd_In$nace2_code))
mat_3rd <- get.matrix(mat_3rd)

Indicators <- as.data.frame(Herfindahl(mat_3rd))
names(Indicators) <- "Herfindahl"
mat_3rd_RCAs <- location.quotient(mat_3rd, binary = T)
Indicators$Herfindahl_RCA <- Herfindahl(mat_3rd_RCAs)
Indicators$Entropy <- entropy(mat_3rd)
Indicators$Entropy_RCA <- entropy(mat_3rd_RCAs)
Indicators$Period <- "3rd"

write.csv2(Indicators, file = "Data_calculations_Nace/Indicators_3rd_period_Nace_AI.csv", row.names = TRUE)
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

write.csv2(KnowledgeComp_PerCountry_3rd_All, file = "Data_calculations_Nace/KnowledgeComp_PerCountry_3rd_All_AI.csv", row.names = TRUE)
write.csv2(KnowledgeComp_PerCountry_3rd_All_RCAs, file = "Data_calculations_Nace/KnowledgeComp_PerCountry_3rd_All_RCAs_AI.csv", row.names = TRUE)

#1.2.3. Calculate the relatedness -----
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
mat_tech_3rd_US <- create_sparse_matrix(i = Nace_all_patents_3rd_US %>% pull(appln_id),
                                        j = Nace_all_patents_3rd_US %>% pull(nace2_code))

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
Nace_all_patents_3rd_US_Top4 <- Nace_all_patents_3rd_US[Nace_all_patents_3rd_US$nace2_code == "26.2" | 
                                                        Nace_all_patents_3rd_US$nace2_code == "26.5"| 
                                                        Nace_all_patents_3rd_US$nace2_code == "26.51"| 
                                                        Nace_all_patents_3rd_US$nace2_code == "62", ]

mat_tech_3rd_US_Top4 <- create_sparse_matrix(i = Nace_all_patents_3rd_US_Top4 %>% pull(appln_id),
                                             j = Nace_all_patents_3rd_US_Top4 %>% pull(nace2_code))

mat_tech_3rd_US_Top4 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_3rd_US_Top4_rel_jacc <- relatedness(mat_tech_3rd_US_Top4, method = "Jaccard")
mat_tech_3rd_US_Top4_rel_asso <- relatedness(mat_tech_3rd_US_Top4, method = "association")
mat_tech_3rd_US_Top4_rel_cosi <- relatedness(mat_tech_3rd_US_Top4, method = "cosine")

Relatedness$Jaccard_top4 <- mean(mat_tech_3rd_US_Top4_rel_jacc)
Relatedness$Association_top4 <- mean(mat_tech_3rd_US_Top4_rel_asso)
Relatedness$Cosine_top4<- mean(mat_tech_3rd_US_Top4_rel_cosi)

#then select only the top 7 areas:
Nace_all_patents_3rd_US_Top7 <- Nace_all_patents_3rd_US[Nace_all_patents_3rd_US$nace2_code == "25.3"| 
                                                          Nace_all_patents_3rd_US$nace2_code == "28.23"| 
                                                          Nace_all_patents_3rd_US$nace2_code == "29.3"|
                                                          Nace_all_patents_3rd_US$nace2_code == "26.4"| 
                                                          Nace_all_patents_3rd_US$nace2_code == "27.12"| 
                                                          Nace_all_patents_3rd_US$nace2_code == "27.9"| 
                                                          Nace_all_patents_3rd_US$nace2_code == "28.25", ]

mat_tech_3rd_US_Top7 <- create_sparse_matrix(i = Nace_all_patents_3rd_US_Top7 %>% pull(appln_id),
                                             j = Nace_all_patents_3rd_US_Top7 %>% pull(nace2_code))

mat_tech_3rd_US_Top7 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_3rd_US_Top7_rel_jacc <- relatedness(mat_tech_3rd_US_Top7, method = "Jaccard")
mat_tech_3rd_US_Top7_rel_asso <- relatedness(mat_tech_3rd_US_Top7, method = "association")
mat_tech_3rd_US_Top7_rel_cosi <- relatedness(mat_tech_3rd_US_Top7, method = "cosine")

Relatedness$Jaccard_Top7 <- mean(mat_tech_3rd_US_Top7_rel_jacc)
Relatedness$Association_Top7 <- mean(mat_tech_3rd_US_Top7_rel_asso)
Relatedness$Cosine_Top7<- mean(mat_tech_3rd_US_Top7_rel_cosi)

#then select only the top 8 areas:
Nace_all_patents_3rd_US_Top8 <- Nace_all_patents_3rd_US[Nace_all_patents_3rd_US$nace2_code == "28.23"| 
                                                          Nace_all_patents_3rd_US$nace2_code == "26.7"| 
                                                          Nace_all_patents_3rd_US$nace2_code == "32.9"|
                                                          Nace_all_patents_3rd_US$nace2_code == "26.1"| 
                                                          Nace_all_patents_3rd_US$nace2_code == "27.5"| 
                                                          Nace_all_patents_3rd_US$nace2_code == "27.33"| 
                                                          Nace_all_patents_3rd_US$nace2_code == "27.4"| 
                                                          Nace_all_patents_3rd_US$nace2_code == "26.3", ]

mat_tech_3rd_US_Top8 <- create_sparse_matrix(i = Nace_all_patents_3rd_US_Top8 %>% pull(appln_id),
                                             j = Nace_all_patents_3rd_US_Top8 %>% pull(nace2_code))

mat_tech_3rd_US_Top8 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_3rd_US_Top8_rel_jacc <- relatedness(mat_tech_3rd_US_Top8, method = "Jaccard")
mat_tech_3rd_US_Top8_rel_asso <- relatedness(mat_tech_3rd_US_Top8, method = "association")
mat_tech_3rd_US_Top8_rel_cosi <- relatedness(mat_tech_3rd_US_Top8, method = "cosine")

Relatedness$Jaccard_Top8 <- mean(mat_tech_3rd_US_Top8_rel_jacc)
Relatedness$Association_Top8 <- mean(mat_tech_3rd_US_Top8_rel_asso)
Relatedness$Cosine_Top8<- mean(mat_tech_3rd_US_Top8_rel_cosi)

#and finally the top 3 close to AI areas:
Nace_all_patents_3rd_US_Top3 <- Nace_all_patents_3rd_US[Nace_all_patents_3rd_US$nace2_code == "25.3"| 
                                                          Nace_all_patents_3rd_US$nace2_code == "28.23"| 
                                                          Nace_all_patents_3rd_US$nace2_code == "29.3", ]

mat_tech_3rd_US_Top3 <- create_sparse_matrix(i = Nace_all_patents_3rd_US_Top3 %>% pull(appln_id),
                                             j = Nace_all_patents_3rd_US_Top3 %>% pull(nace2_code))

mat_tech_3rd_US_Top3 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_3rd_US_Top3_rel_jacc <- relatedness(mat_tech_3rd_US_Top3, method = "Jaccard")
mat_tech_3rd_US_Top3_rel_asso <- relatedness(mat_tech_3rd_US_Top3, method = "association")
mat_tech_3rd_US_Top3_rel_cosi <- relatedness(mat_tech_3rd_US_Top3, method = "cosine")

Relatedness$Jaccard_Top3 <- mean(mat_tech_3rd_US_Top3_rel_jacc)
Relatedness$Association_Top3 <- mean(mat_tech_3rd_US_Top3_rel_asso)
Relatedness$Cosine_Top3<- mean(mat_tech_3rd_US_Top3_rel_cosi)

#China:
#the global environment is already too full for the calculations, let's clean it:
rm(Nace_all_patents_3rd, Nace_all_patents_3rd_US, Nace_all_patents_3rd_In)
mat_tech_3rd_CN <- create_sparse_matrix(i = Nace_all_patents_3rd_CN %>% pull(appln_id),
                                        j = Nace_all_patents_3rd_CN %>% pull(nace2_code))

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

#then select only the top 4 areas:
Nace_all_patents_3rd_CN_Top4 <- Nace_all_patents_3rd_CN[Nace_all_patents_3rd_CN$nace2_code == "26.2" | 
                                                          Nace_all_patents_3rd_CN$nace2_code == "26.5"| 
                                                          Nace_all_patents_3rd_CN$nace2_code == "26.51"| 
                                                          Nace_all_patents_3rd_CN$nace2_code == "62", ]

mat_tech_3rd_CN_Top4 <- create_sparse_matrix(i = Nace_all_patents_3rd_CN_Top4 %>% pull(appln_id),
                                             j = Nace_all_patents_3rd_CN_Top4 %>% pull(nace2_code))

mat_tech_3rd_CN_Top4 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_3rd_CN_Top4_rel_jacc <- relatedness(mat_tech_3rd_CN_Top4, method = "Jaccard")
mat_tech_3rd_CN_Top4_rel_asso <- relatedness(mat_tech_3rd_CN_Top4, method = "association")
mat_tech_3rd_CN_Top4_rel_cosi <- relatedness(mat_tech_3rd_CN_Top4, method = "cosine")

Relatedness_CN$Jaccard_top4 <- mean(mat_tech_3rd_CN_Top4_rel_jacc)
Relatedness_CN$Association_top4 <- mean(mat_tech_3rd_CN_Top4_rel_asso)
Relatedness_CN$Cosine_top4<- mean(mat_tech_3rd_CN_Top4_rel_cosi)

#then select only the top 7 areas:
#but first remove big files:
rm(Nace_all_patents_3rd_CN_Top4, Nace_all_patents_3rd_US_Top4, Nace_all_patents_3rd_US_Top8)
Nace_all_patents_3rd_CN_Top7 <- Nace_all_patents_3rd_CN[Nace_all_patents_3rd_CN$nace2_code == "25.3"| 
                                                          Nace_all_patents_3rd_CN$nace2_code == "28.23"| 
                                                          Nace_all_patents_3rd_CN$nace2_code == "29.3"|
                                                          Nace_all_patents_3rd_CN$nace2_code == "26.4"| 
                                                          Nace_all_patents_3rd_CN$nace2_code == "27.12"| 
                                                          Nace_all_patents_3rd_CN$nace2_code == "27.9"| 
                                                          Nace_all_patents_3rd_CN$nace2_code == "28.25", ]

mat_tech_3rd_CN_Top7 <- create_sparse_matrix(i = Nace_all_patents_3rd_CN_Top7 %>% pull(appln_id),
                                             j = Nace_all_patents_3rd_CN_Top7 %>% pull(nace2_code))
rm(Nace_all_patents_3rd_CN_Top7)
mat_tech_3rd_CN_Top7 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_3rd_CN_Top7_rel_jacc <- relatedness(mat_tech_3rd_CN_Top7, method = "Jaccard")
mat_tech_3rd_CN_Top7_rel_asso <- relatedness(mat_tech_3rd_CN_Top7, method = "association")
mat_tech_3rd_CN_Top7_rel_cosi <- relatedness(mat_tech_3rd_CN_Top7, method = "cosine")

Relatedness_CN$Jaccard_Top7 <- mean(mat_tech_3rd_CN_Top7_rel_jacc)
Relatedness_CN$Association_Top7 <- mean(mat_tech_3rd_CN_Top7_rel_asso)
Relatedness_CN$Cosine_Top7<- mean(mat_tech_3rd_CN_Top7_rel_cosi)

#then select only the top 8 areas:
Nace_all_patents_3rd_CN_Top8 <- Nace_all_patents_3rd_CN[Nace_all_patents_3rd_CN$nace2_code == "28.23"| 
                                                          Nace_all_patents_3rd_CN$nace2_code == "26.7"| 
                                                          Nace_all_patents_3rd_CN$nace2_code == "32.9"|
                                                          Nace_all_patents_3rd_CN$nace2_code == "26.1"| 
                                                          Nace_all_patents_3rd_CN$nace2_code == "27.5"| 
                                                          Nace_all_patents_3rd_CN$nace2_code == "27.33"| 
                                                          Nace_all_patents_3rd_CN$nace2_code == "27.4"| 
                                                          Nace_all_patents_3rd_CN$nace2_code == "26.3", ]

mat_tech_3rd_CN_Top8 <- create_sparse_matrix(i = Nace_all_patents_3rd_CN_Top8 %>% pull(appln_id),
                                             j = Nace_all_patents_3rd_CN_Top8 %>% pull(nace2_code))
rm(Nace_all_patents_3rd_CN_Top8)
mat_tech_3rd_CN_Top8 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_3rd_CN_Top8_rel_jacc <- relatedness(mat_tech_3rd_CN_Top8, method = "Jaccard")
mat_tech_3rd_CN_Top8_rel_asso <- relatedness(mat_tech_3rd_CN_Top8, method = "association")
mat_tech_3rd_CN_Top8_rel_cosi <- relatedness(mat_tech_3rd_CN_Top8, method = "cosine")

Relatedness_CN$Jaccard_Top8 <- mean(mat_tech_3rd_CN_Top8_rel_jacc)
Relatedness_CN$Association_Top8 <- mean(mat_tech_3rd_CN_Top8_rel_asso)
Relatedness_CN$Cosine_Top8<- mean(mat_tech_3rd_CN_Top8_rel_cosi)

#and finally the top 3 close to AI areas:
Nace_all_patents_3rd_CN_Top3 <- Nace_all_patents_3rd_CN[Nace_all_patents_3rd_CN$nace2_code == "25.3"| 
                                                          Nace_all_patents_3rd_CN$nace2_code == "28.23"| 
                                                          Nace_all_patents_3rd_CN$nace2_code == "29.3", ]

mat_tech_3rd_CN_Top3 <- create_sparse_matrix(i = Nace_all_patents_3rd_CN_Top3 %>% pull(appln_id),
                                             j = Nace_all_patents_3rd_CN_Top3 %>% pull(nace2_code))

mat_tech_3rd_CN_Top3 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_3rd_CN_Top3_rel_jacc <- relatedness(mat_tech_3rd_CN_Top3, method = "Jaccard")
mat_tech_3rd_CN_Top3_rel_asso <- relatedness(mat_tech_3rd_CN_Top3, method = "association")
mat_tech_3rd_CN_Top3_rel_cosi <- relatedness(mat_tech_3rd_CN_Top3, method = "cosine")

Relatedness_CN$Jaccard_Top3 <- mean(mat_tech_3rd_CN_Top3_rel_jacc)
Relatedness_CN$Association_Top3 <- mean(mat_tech_3rd_CN_Top3_rel_asso)
Relatedness_CN$Cosine_Top3<- mean(mat_tech_3rd_CN_Top3_rel_cosi)
rm(Nace_all_patents_3rd_CN)

#KR
mat_tech_3rd_KR <- create_sparse_matrix(i = Nace_all_patents_3rd_KR %>% pull(appln_id),
                                        j = Nace_all_patents_3rd_KR %>% pull(nace2_code))

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
Nace_all_patents_3rd_KR_Top4 <- Nace_all_patents_3rd_KR[Nace_all_patents_3rd_KR$nace2_code == "26.2" | 
                                                          Nace_all_patents_3rd_KR$nace2_code == "26.5"| 
                                                          Nace_all_patents_3rd_KR$nace2_code == "26.51"| 
                                                          Nace_all_patents_3rd_KR$nace2_code == "62", ]

mat_tech_3rd_KR_Top4 <- create_sparse_matrix(i = Nace_all_patents_3rd_KR_Top4 %>% pull(appln_id),
                                             j = Nace_all_patents_3rd_KR_Top4 %>% pull(nace2_code))

mat_tech_3rd_KR_Top4 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_3rd_KR_Top4_rel_jacc <- relatedness(mat_tech_3rd_KR_Top4, method = "Jaccard")
mat_tech_3rd_KR_Top4_rel_asso <- relatedness(mat_tech_3rd_KR_Top4, method = "association")
mat_tech_3rd_KR_Top4_rel_cosi <- relatedness(mat_tech_3rd_KR_Top4, method = "cosine")

Relatedness_KR$Jaccard_top4 <- mean(mat_tech_3rd_KR_Top4_rel_jacc)
Relatedness_KR$Association_top4 <- mean(mat_tech_3rd_KR_Top4_rel_asso)
Relatedness_KR$Cosine_top4<- mean(mat_tech_3rd_KR_Top4_rel_cosi)

#then select only the top 7 areas:
Nace_all_patents_3rd_KR_Top7 <- Nace_all_patents_3rd_KR[Nace_all_patents_3rd_KR$nace2_code == "25.3"| 
                                                          Nace_all_patents_3rd_KR$nace2_code == "28.23"| 
                                                          Nace_all_patents_3rd_KR$nace2_code == "29.3"|
                                                          Nace_all_patents_3rd_KR$nace2_code == "26.4"| 
                                                          Nace_all_patents_3rd_KR$nace2_code == "27.12"| 
                                                          Nace_all_patents_3rd_KR$nace2_code == "27.9"| 
                                                          Nace_all_patents_3rd_KR$nace2_code == "28.25", ]

mat_tech_3rd_KR_Top7 <- create_sparse_matrix(i = Nace_all_patents_3rd_KR_Top7 %>% pull(appln_id),
                                             j = Nace_all_patents_3rd_KR_Top7 %>% pull(nace2_code))

mat_tech_3rd_KR_Top7 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_3rd_KR_Top7_rel_jacc <- relatedness(mat_tech_3rd_KR_Top7, method = "Jaccard")
mat_tech_3rd_KR_Top7_rel_asso <- relatedness(mat_tech_3rd_KR_Top7, method = "association")
mat_tech_3rd_KR_Top7_rel_cosi <- relatedness(mat_tech_3rd_KR_Top7, method = "cosine")

Relatedness_KR$Jaccard_Top7 <- mean(mat_tech_3rd_KR_Top7_rel_jacc)
Relatedness_KR$Association_Top7 <- mean(mat_tech_3rd_KR_Top7_rel_asso)
Relatedness_KR$Cosine_Top7<- mean(mat_tech_3rd_KR_Top7_rel_cosi)

#then select only the top 8 areas:
Nace_all_patents_3rd_KR_Top8 <- Nace_all_patents_3rd_KR[Nace_all_patents_3rd_KR$nace2_code == "28.23"| 
                                                          Nace_all_patents_3rd_KR$nace2_code == "26.7"| 
                                                          Nace_all_patents_3rd_KR$nace2_code == "32.9"|
                                                          Nace_all_patents_3rd_KR$nace2_code == "26.1"| 
                                                          Nace_all_patents_3rd_KR$nace2_code == "27.5"| 
                                                          Nace_all_patents_3rd_KR$nace2_code == "27.33"| 
                                                          Nace_all_patents_3rd_KR$nace2_code == "27.4"| 
                                                          Nace_all_patents_3rd_KR$nace2_code == "26.3", ]

mat_tech_3rd_KR_Top8 <- create_sparse_matrix(i = Nace_all_patents_3rd_KR_Top8 %>% pull(appln_id),
                                             j = Nace_all_patents_3rd_KR_Top8 %>% pull(nace2_code))

mat_tech_3rd_KR_Top8 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_3rd_KR_Top8_rel_jacc <- relatedness(mat_tech_3rd_KR_Top8, method = "Jaccard")
mat_tech_3rd_KR_Top8_rel_asso <- relatedness(mat_tech_3rd_KR_Top8, method = "association")
mat_tech_3rd_KR_Top8_rel_cosi <- relatedness(mat_tech_3rd_KR_Top8, method = "cosine")

Relatedness_KR$Jaccard_Top8 <- mean(mat_tech_3rd_KR_Top8_rel_jacc)
Relatedness_KR$Association_Top8 <- mean(mat_tech_3rd_KR_Top8_rel_asso)
Relatedness_KR$Cosine_Top8<- mean(mat_tech_3rd_KR_Top8_rel_cosi)

#and finally the top 3 close to AI areas:
Nace_all_patents_3rd_KR_Top3 <- Nace_all_patents_3rd_KR[Nace_all_patents_3rd_KR$nace2_code == "25.3"| 
                                                          Nace_all_patents_3rd_KR$nace2_code == "28.23"| 
                                                          Nace_all_patents_3rd_KR$nace2_code == "29.3", ]

mat_tech_3rd_KR_Top3 <- create_sparse_matrix(i = Nace_all_patents_3rd_KR_Top3 %>% pull(appln_id),
                                             j = Nace_all_patents_3rd_KR_Top3 %>% pull(nace2_code))

mat_tech_3rd_KR_Top3 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_3rd_KR_Top3_rel_jacc <- relatedness(mat_tech_3rd_KR_Top3, method = "Jaccard")
mat_tech_3rd_KR_Top3_rel_asso <- relatedness(mat_tech_3rd_KR_Top3, method = "association")
mat_tech_3rd_KR_Top3_rel_cosi <- relatedness(mat_tech_3rd_KR_Top3, method = "cosine")

Relatedness_KR$Jaccard_Top3 <- mean(mat_tech_3rd_KR_Top3_rel_jacc)
Relatedness_KR$Association_Top3 <- mean(mat_tech_3rd_KR_Top3_rel_asso)
Relatedness_KR$Cosine_Top3<- mean(mat_tech_3rd_KR_Top3_rel_cosi)
rm(Nace_all_patents_3rd_KR)
#Japan
mat_tech_3rd_JP <- create_sparse_matrix(i = Nace_all_patents_3rd_JP %>% pull(appln_id),
                                        j = Nace_all_patents_3rd_JP %>% pull(nace2_code))

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
Nace_all_patents_3rd_JP_Top4 <- Nace_all_patents_3rd_JP[Nace_all_patents_3rd_JP$nace2_code == "26.2" | 
                                                          Nace_all_patents_3rd_JP$nace2_code == "26.5"| 
                                                          Nace_all_patents_3rd_JP$nace2_code == "26.51"| 
                                                          Nace_all_patents_3rd_JP$nace2_code == "62", ]

mat_tech_3rd_JP_Top4 <- create_sparse_matrix(i = Nace_all_patents_3rd_JP_Top4 %>% pull(appln_id),
                                             j = Nace_all_patents_3rd_JP_Top4 %>% pull(nace2_code))

mat_tech_3rd_JP_Top4 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_3rd_JP_Top4_rel_jacc <- relatedness(mat_tech_3rd_JP_Top4, method = "Jaccard")
mat_tech_3rd_JP_Top4_rel_asso <- relatedness(mat_tech_3rd_JP_Top4, method = "association")
mat_tech_3rd_JP_Top4_rel_cosi <- relatedness(mat_tech_3rd_JP_Top4, method = "cosine")

Relatedness_JP$Jaccard_top4 <- mean(mat_tech_3rd_JP_Top4_rel_jacc)
Relatedness_JP$Association_top4 <- mean(mat_tech_3rd_JP_Top4_rel_asso)
Relatedness_JP$Cosine_top4<- mean(mat_tech_3rd_JP_Top4_rel_cosi)

#then select only the top 7 areas:
Nace_all_patents_3rd_JP_Top7 <- Nace_all_patents_3rd_JP[Nace_all_patents_3rd_JP$nace2_code == "25.3"| 
                                                          Nace_all_patents_3rd_JP$nace2_code == "28.23"| 
                                                          Nace_all_patents_3rd_JP$nace2_code == "29.3"|
                                                          Nace_all_patents_3rd_JP$nace2_code == "26.4"| 
                                                          Nace_all_patents_3rd_JP$nace2_code == "27.12"| 
                                                          Nace_all_patents_3rd_JP$nace2_code == "27.9"| 
                                                          Nace_all_patents_3rd_JP$nace2_code == "28.25", ]

mat_tech_3rd_JP_Top7 <- create_sparse_matrix(i = Nace_all_patents_3rd_JP_Top7 %>% pull(appln_id),
                                             j = Nace_all_patents_3rd_JP_Top7 %>% pull(nace2_code))

mat_tech_3rd_JP_Top7 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_3rd_JP_Top7_rel_jacc <- relatedness(mat_tech_3rd_JP_Top7, method = "Jaccard")
mat_tech_3rd_JP_Top7_rel_asso <- relatedness(mat_tech_3rd_JP_Top7, method = "association")
mat_tech_3rd_JP_Top7_rel_cosi <- relatedness(mat_tech_3rd_JP_Top7, method = "cosine")

Relatedness_JP$Jaccard_Top7 <- mean(mat_tech_3rd_JP_Top7_rel_jacc)
Relatedness_JP$Association_Top7 <- mean(mat_tech_3rd_JP_Top7_rel_asso)
Relatedness_JP$Cosine_Top7<- mean(mat_tech_3rd_JP_Top7_rel_cosi)

#then select only the top 8 areas:
Nace_all_patents_3rd_JP_Top8 <- Nace_all_patents_3rd_JP[Nace_all_patents_3rd_JP$nace2_code == "28.23"| 
                                                          Nace_all_patents_3rd_JP$nace2_code == "26.7"| 
                                                          Nace_all_patents_3rd_JP$nace2_code == "32.9"|
                                                          Nace_all_patents_3rd_JP$nace2_code == "26.1"| 
                                                          Nace_all_patents_3rd_JP$nace2_code == "27.5"| 
                                                          Nace_all_patents_3rd_JP$nace2_code == "27.33"| 
                                                          Nace_all_patents_3rd_JP$nace2_code == "27.4"| 
                                                          Nace_all_patents_3rd_JP$nace2_code == "26.3", ]

mat_tech_3rd_JP_Top8 <- create_sparse_matrix(i = Nace_all_patents_3rd_JP_Top8 %>% pull(appln_id),
                                             j = Nace_all_patents_3rd_JP_Top8 %>% pull(nace2_code))

mat_tech_3rd_JP_Top8 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_3rd_JP_Top8_rel_jacc <- relatedness(mat_tech_3rd_JP_Top8, method = "Jaccard")
mat_tech_3rd_JP_Top8_rel_asso <- relatedness(mat_tech_3rd_JP_Top8, method = "association")
mat_tech_3rd_JP_Top8_rel_cosi <- relatedness(mat_tech_3rd_JP_Top8, method = "cosine")

Relatedness_JP$Jaccard_Top8 <- mean(mat_tech_3rd_JP_Top8_rel_jacc)
Relatedness_JP$Association_Top8 <- mean(mat_tech_3rd_JP_Top8_rel_asso)
Relatedness_JP$Cosine_Top8<- mean(mat_tech_3rd_JP_Top8_rel_cosi)

#and finally the top 3 close to AI areas:
Nace_all_patents_3rd_JP_Top3 <- Nace_all_patents_3rd_JP[Nace_all_patents_3rd_JP$nace2_code == "25.3"| 
                                                          Nace_all_patents_3rd_JP$nace2_code == "28.23"| 
                                                          Nace_all_patents_3rd_JP$nace2_code == "29.3", ]

mat_tech_3rd_JP_Top3 <- create_sparse_matrix(i = Nace_all_patents_3rd_JP_Top3 %>% pull(appln_id),
                                             j = Nace_all_patents_3rd_JP_Top3 %>% pull(nace2_code))

mat_tech_3rd_JP_Top3 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_3rd_JP_Top3_rel_jacc <- relatedness(mat_tech_3rd_JP_Top3, method = "Jaccard")
mat_tech_3rd_JP_Top3_rel_asso <- relatedness(mat_tech_3rd_JP_Top3, method = "association")
mat_tech_3rd_JP_Top3_rel_cosi <- relatedness(mat_tech_3rd_JP_Top3, method = "cosine")

Relatedness_JP$Jaccard_Top3 <- mean(mat_tech_3rd_JP_Top3_rel_jacc)
Relatedness_JP$Association_Top3 <- mean(mat_tech_3rd_JP_Top3_rel_asso)
Relatedness_JP$Cosine_Top3<- mean(mat_tech_3rd_JP_Top3_rel_cosi)

#AI
mat_tech_3rd_AI <- create_sparse_matrix(i = Nace_all_patents_3rd_AI %>% pull(appln_id),
                                        j = Nace_all_patents_3rd_AI %>% pull(nace2_code))

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
Nace_all_patents_3rd_AI_Top4 <- Nace_all_patents_3rd_AI[Nace_all_patents_3rd_AI$nace2_code == "26.2" | 
                                                          Nace_all_patents_3rd_AI$nace2_code == "26.5"| 
                                                          Nace_all_patents_3rd_AI$nace2_code == "26.51"| 
                                                          Nace_all_patents_3rd_AI$nace2_code == "62", ]

mat_tech_3rd_AI_Top4 <- create_sparse_matrix(i = Nace_all_patents_3rd_AI_Top4 %>% pull(appln_id),
                                             j = Nace_all_patents_3rd_AI_Top4 %>% pull(nace2_code))

mat_tech_3rd_AI_Top4 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_3rd_AI_Top4_rel_jacc <- relatedness(mat_tech_3rd_AI_Top4, method = "Jaccard")
mat_tech_3rd_AI_Top4_rel_asso <- relatedness(mat_tech_3rd_AI_Top4, method = "association")
mat_tech_3rd_AI_Top4_rel_cosi <- relatedness(mat_tech_3rd_AI_Top4, method = "cosine")

Relatedness_AI$Jaccard_top4 <- mean(mat_tech_3rd_AI_Top4_rel_jacc)
Relatedness_AI$Association_top4 <- mean(mat_tech_3rd_AI_Top4_rel_asso)
Relatedness_AI$Cosine_top4<- mean(mat_tech_3rd_AI_Top4_rel_cosi)

#then select only the top 7 areas:
Nace_all_patents_3rd_AI_Top7 <- Nace_all_patents_3rd_AI[Nace_all_patents_3rd_AI$nace2_code == "25.3"| 
                                                          Nace_all_patents_3rd_AI$nace2_code == "28.23"| 
                                                          Nace_all_patents_3rd_AI$nace2_code == "29.3"|
                                                          Nace_all_patents_3rd_AI$nace2_code == "26.4"| 
                                                          Nace_all_patents_3rd_AI$nace2_code == "27.12"| 
                                                          Nace_all_patents_3rd_AI$nace2_code == "27.9"| 
                                                          Nace_all_patents_3rd_AI$nace2_code == "28.25", ]

mat_tech_3rd_AI_Top7 <- create_sparse_matrix(i = Nace_all_patents_3rd_AI_Top7 %>% pull(appln_id),
                                             j = Nace_all_patents_3rd_AI_Top7 %>% pull(nace2_code))

mat_tech_3rd_AI_Top7 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_3rd_AI_Top7_rel_jacc <- relatedness(mat_tech_3rd_AI_Top7, method = "Jaccard")
mat_tech_3rd_AI_Top7_rel_asso <- relatedness(mat_tech_3rd_AI_Top7, method = "association")
mat_tech_3rd_AI_Top7_rel_cosi <- relatedness(mat_tech_3rd_AI_Top7, method = "cosine")

Relatedness_AI$Jaccard_Top7 <- mean(mat_tech_3rd_AI_Top7_rel_jacc)
Relatedness_AI$Association_Top7 <- mean(mat_tech_3rd_AI_Top7_rel_asso)
Relatedness_AI$Cosine_Top7<- mean(mat_tech_3rd_AI_Top7_rel_cosi)

#then select only the top 8 areas:
Nace_all_patents_3rd_AI_Top8 <- Nace_all_patents_3rd_AI[Nace_all_patents_3rd_AI$nace2_code == "28.23"| 
                                                          Nace_all_patents_3rd_AI$nace2_code == "26.7"| 
                                                          Nace_all_patents_3rd_AI$nace2_code == "32.9"|
                                                          Nace_all_patents_3rd_AI$nace2_code == "26.1"| 
                                                          Nace_all_patents_3rd_AI$nace2_code == "27.5"| 
                                                          Nace_all_patents_3rd_AI$nace2_code == "27.33"| 
                                                          Nace_all_patents_3rd_AI$nace2_code == "27.4"| 
                                                          Nace_all_patents_3rd_AI$nace2_code == "26.3", ]

mat_tech_3rd_AI_Top8 <- create_sparse_matrix(i = Nace_all_patents_3rd_AI_Top8 %>% pull(appln_id),
                                             j = Nace_all_patents_3rd_AI_Top8 %>% pull(nace2_code))

mat_tech_3rd_AI_Top8 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_3rd_AI_Top8_rel_jacc <- relatedness(mat_tech_3rd_AI_Top8, method = "Jaccard")
mat_tech_3rd_AI_Top8_rel_asso <- relatedness(mat_tech_3rd_AI_Top8, method = "association")
mat_tech_3rd_AI_Top8_rel_cosi <- relatedness(mat_tech_3rd_AI_Top8, method = "cosine")

Relatedness_AI$Jaccard_Top8 <- mean(mat_tech_3rd_AI_Top8_rel_jacc)
Relatedness_AI$Association_Top8 <- mean(mat_tech_3rd_AI_Top8_rel_asso)
Relatedness_AI$Cosine_Top8<- mean(mat_tech_3rd_AI_Top8_rel_cosi)

#and finally the top 3 close to AI areas:
Nace_all_patents_3rd_AI_Top3 <- Nace_all_patents_3rd_AI[Nace_all_patents_3rd_AI$nace2_code == "25.3"| 
                                                          Nace_all_patents_3rd_AI$nace2_code == "28.23"| 
                                                          Nace_all_patents_3rd_AI$nace2_code == "29.3", ]

mat_tech_3rd_AI_Top3 <- create_sparse_matrix(i = Nace_all_patents_3rd_AI_Top3 %>% pull(appln_id),
                                             j = Nace_all_patents_3rd_AI_Top3 %>% pull(nace2_code))

mat_tech_3rd_AI_Top3 %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_3rd_AI_Top3_rel_jacc <- relatedness(mat_tech_3rd_AI_Top3, method = "Jaccard")
mat_tech_3rd_AI_Top3_rel_asso <- relatedness(mat_tech_3rd_AI_Top3, method = "association")
mat_tech_3rd_AI_Top3_rel_cosi <- relatedness(mat_tech_3rd_AI_Top3, method = "cosine")

Relatedness_AI$Jaccard_Top3 <- mean(mat_tech_3rd_AI_Top3_rel_jacc)
Relatedness_AI$Association_Top3 <- mean(mat_tech_3rd_AI_Top3_rel_asso)
Relatedness_AI$Cosine_Top3<- mean(mat_tech_3rd_AI_Top3_rel_cosi)

#and we merge it all together:
Relatedness_ThirdPeriod <- rbind(Relatedness, Relatedness_CN, Relatedness_KR, Relatedness_JP, Relatedness_AI)
Relatedness_ThirdPeriod <- Relatedness_ThirdPeriod[,c((1:3), (5:16), (4))]

write.csv2(Relatedness_ThirdPeriod, file = "Data_calculations_Nace/Relatedness_3rd_period_Nace.csv", row.names = TRUE)

#2. Visualization RCAs -----
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

#2.1.First period ----
reg_tech1_countries <- read.csv("Data_Nace/reg_tech_FirstPeriod.csv", sep = ";", header = TRUE, dec=",")
#2.1.1. First Period Countries ----
mat_reg_tech1_countries <- reg_tech1_countries %>%
  arrange(nace2_code, ctry_code) %>%
  pivot_wider(names_from = nace2_code, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

rownames(mat_reg_tech1_countries) <- mat_reg_tech1_countries %>% pull(ctry_code)

mat_reg_tech1_countries %<>% select(-ctry_code) %>%
  as.matrix() %>%
  round()

reg_RCA1_countries <- mat_reg_tech1_countries %>% location.quotient(binary = F) %>% 
  as.data.frame() %>% 
  rownames_to_column("ctry_code") %>% 
  as_tibble() %>% 
  gather(key = "nace2_code", value = "RCA", -ctry_code) %>%
  arrange(ctry_code, nace2_code)

###2.1.2. First Period AI----
reg_tech1_AI <- read.csv("Data_Nace/reg_techAI_FirstPeriod.csv", sep = ";", header = TRUE, dec=",")

mat_reg_tech1_AI <- reg_tech1_AI %>%
  arrange(nace2_code, ctry_code) %>%
  pivot_wider(names_from = nace2_code, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

rownames(mat_reg_tech1_AI) <- mat_reg_tech1_AI %>% pull(ctry_code)

mat_reg_tech1_AI %<>% select(-ctry_code) %>%
  as.matrix() %>%
  round()

reg_RCA1_AI <- mat_reg_tech1_AI %>% location.quotient(binary = F) %>% 
  as.data.frame() %>% 
  rownames_to_column("ctry_code") %>% 
  as_tibble() %>% 
  gather(key = "nace2_code", value = "RCA", -ctry_code) %>%
  arrange(ctry_code, nace2_code)

nace2_names <- read.csv("Data_Nace/tls902_ipc_nace2.csv", sep = ";", header = TRUE)%>%
  select(nace2_code, nace2_descr) %>%
  distinct(nace2_code, .keep_all = TRUE) %>%
  mutate(nace2_code = nace2_code) %>%
  arrange(nace2_code)

#select countries
US_first_period <- reg_RCA1_countries[,2:3][reg_RCA1_countries$ctry_code == "US",]
CN_first_period <- reg_RCA1_countries[,2:3][reg_RCA1_countries$ctry_code == "CN",]
KR_first_period <- reg_RCA1_countries[,2:3][reg_RCA1_countries$ctry_code == "KR",]
JP_first_period <- reg_RCA1_countries[,2:3][reg_RCA1_countries$ctry_code == "JP",]
AI_first_period <- reg_RCA1_AI[,2:3][reg_RCA1_AI$ctry_code == "AI_pat",]

First_period <- merge(merge(merge(merge(merge(
  nace2_names,US_first_period), CN_first_period, by = "nace2_code"), KR_first_period, by = "nace2_code"), 
  JP_first_period, by = "nace2_code"), AI_first_period, by = "nace2_code")
names(First_period) = c("nace2_code", "nace2_descr", "RCA_US", "RCA_CN","RCA_KR","RCA_JP", "RCA_AI")
KnwComp_Country_1st_RCA <- read.csv("Data_calculations_Nace/KnowledgeComp_PerCountry_1st_All_RCAs.csv", sep = ";", header = T, dec=",")
KnwComp_Country_1st_RCA <- as.data.frame(t(KnwComp_Country_1st_RCA))
KnwComp_Country_1st_RCA <- KnwComp_Country_1st_RCA %>%
  row_to_names(row_number = 1)
KnwComp_Country_1st_RCA<- KnwComp_Country_1st_RCA[(-85),]
KnwComp_Country_1st_RCA$nace2_code <- First_period$nace2_code
First_period$US_Com <- KnwComp_Country_1st_RCA$US3
First_period$CN_Com <- KnwComp_Country_1st_RCA$CN3
First_period$KR_Com <- KnwComp_Country_1st_RCA$KR3
First_period$JP_Com <- KnwComp_Country_1st_RCA$JP3
i <- c(8:11)
First_period[ , i] <- apply(First_period[ , i], 2,            # Specify own function within apply
                            function(x) as.integer(as.character(x)))

First_period$nace2_code <- as.factor(First_period$nace2_code)

#remove files we don't need:
rm(AI_first_period, CN_first_period, JP_first_period, KR_first_period, KnwComp_Country_1st_RCA,
   mat_reg_tech1_AI, mat_reg_tech1_countries, reg_RCA1_AI, reg_RCA1_countries, reg_tech1_AI, reg_tech1_countries,
   US_first_period, i)
#Now we plot:
FigUS_1st <- 
  ggplot(First_period, aes(x=log10(RCA_US), y=log10(RCA_AI), label = '')) + 
  geom_point(aes(colour = nace2_code, size = US_Com),show.legend = F, stroke = 2) +  
  geom_text() +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2)+
  geom_vline(xintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2) +
  geom_label_repel(data = First_period, aes(label = ifelse(RCA_US>1 & RCA_AI>1,as.character(nace2_descr),''), size = 2), show.legend = F) +
  geom_rect(aes(NULL, NULL, xmin = Inf, xmax = 0), ymin = Inf, ymax = 0,alpha=0.005,fill="royalblue2") +
  scale_size_continuous(range = c(1, 10)) +
  ggtitle("United States") +
  xlab(NULL) +
  ylab(NULL)+  
  ylim(-1,2) +
  xlim(-0.6, 0.6)

FigUS_1stb <- 
ggplot(First_period, aes(x=log10(RCA_US), y=log10(RCA_AI), label = '')) + 
  geom_point(aes(colour = nace2_code, size = US_Com),show.legend = F, stroke = 2) +  
  geom_text() +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2)+
  geom_vline(xintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2) +
  geom_label_repel(data = First_period, aes(label = ifelse(RCA_US>1 & RCA_AI>1,as.character(nace2_descr),''), size = 3), show.legend = F) +
  geom_rect(aes(NULL, NULL, xmin = Inf, xmax = 0), ymin = Inf, ymax = 0,alpha=0.005,fill="royalblue2") +
  scale_size_continuous(range = c(1, 10)) +
  ggtitle("United States") +
  xlab(NULL) +
  ylab(NULL)

FigCN_1st <-
  ggplot(First_period, aes(x=log10(RCA_CN), y=log10(RCA_AI), label = '')) + 
  geom_point(aes(colour = nace2_code, size = CN_Com),show.legend = F, stroke = 2) +  
  geom_text() +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2)+
  geom_vline(xintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2) +
  geom_label_repel(data = First_period, aes(label = ifelse(RCA_CN>1 & RCA_AI>1,as.character(nace2_descr),''), size = 2), show.legend = F) +
  geom_rect(aes(NULL, NULL, xmin = Inf, xmax = 0), ymin = Inf, ymax = 0,alpha=0.005,fill="royalblue2") +
  scale_size_continuous(range = c(1, 10)) +
  ggtitle("China") +
  xlab(NULL) +
  ylab(NULL) +
  ylim(-1,2) +
  xlim(-0.6, 0.6)

FigCN_1stb <-ggplot(First_period, aes(x=log10(RCA_CN), y=log10(RCA_AI), label = '')) + 
  geom_point(aes(colour = nace2_code, size = CN_Com),show.legend = F, stroke = 2) +  
  geom_text() +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2)+
  geom_vline(xintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2) +
  geom_label_repel(data = First_period, aes(label = ifelse(RCA_CN>1 & RCA_AI>1,as.character(nace2_descr),''), size = 3), show.legend = F) +
  geom_rect(aes(NULL, NULL, xmin = Inf, xmax = 0), ymin = Inf, ymax = 0,alpha=0.005,fill="royalblue2") +
  scale_size_continuous(range = c(1, 10)) +
  ggtitle("China") +
  xlab(NULL) +
  ylab(NULL)

FigKR_1st <-ggplot(First_period, aes(x=log10(RCA_KR), y=log10(RCA_AI), label = '')) + 
  geom_point(aes(colour = nace2_code, size = KR_Com),show.legend = F, stroke = 2) +  
  geom_text() +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2)+
  geom_vline(xintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2) +
  geom_label_repel(data = First_period, aes(label = ifelse(RCA_KR>1 & RCA_AI>1,as.character(nace2_descr),''), size = 2), show.legend = F) +
  geom_rect(aes(NULL, NULL, xmin = Inf, xmax = 0), ymin = Inf, ymax = 0,alpha=0.005,fill="royalblue2") +
  scale_size_continuous(range = c(1, 10)) +
  ggtitle("South Korea") +
  xlab(NULL) +
  ylab(NULL)+
  ylim(-1,2) +
  xlim(-0.6, 0.6)

FigKR_1stb <-ggplot(First_period, aes(x=log10(RCA_KR), y=log10(RCA_AI), label = '')) + 
  geom_point(aes(colour = nace2_code, size = KR_Com),show.legend = F, stroke = 2) +  
  geom_text() +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2)+
  geom_vline(xintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2) +
  geom_label_repel(data = First_period, aes(label = ifelse(RCA_KR>1 & RCA_AI>1,as.character(nace2_descr),''), size = 3), show.legend = F) +
  geom_rect(aes(NULL, NULL, xmin = Inf, xmax = 0), ymin = Inf, ymax = 0,alpha=0.005,fill="royalblue2") +
  scale_size_continuous(range = c(1, 10)) +
  ggtitle("South Korea") +
  xlab(NULL) +
  ylab(NULL)

FigJP_1st <-ggplot(First_period, aes(x=log10(RCA_JP), y=log10(RCA_AI), label = '')) + 
  geom_point(aes(colour = nace2_code, size = JP_Com),show.legend = F, stroke = 2) +  
  geom_text() +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2)+
  geom_vline(xintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2) +
  geom_label_repel(data = First_period, aes(label = ifelse(RCA_JP>1 & RCA_AI>1,as.character(nace2_descr),''), size = 2), show.legend = F) +
  geom_rect(aes(NULL, NULL, xmin = Inf, xmax = 0), ymin = Inf, ymax = 0,alpha=0.005,fill="royalblue2") +
  scale_size_continuous(range = c(1, 10)) +
  ggtitle("Japan") +
  xlab(NULL) +
  ylab(NULL)+
  ylim(-1,2) +
  xlim(-0.6, 0.6)

FigJP_1stb <-ggplot(First_period, aes(x=log10(RCA_JP), y=log10(RCA_AI), label = '')) + 
  geom_point(aes(colour = nace2_code, size = JP_Com),show.legend = F, stroke = 2) +  
  geom_text() +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2)+
  geom_vline(xintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2) +
  geom_label_repel(data = First_period, aes(label = ifelse(RCA_JP>1 & RCA_AI>1,as.character(nace2_descr),''), size = 3), show.legend = F) +
  geom_rect(aes(NULL, NULL, xmin = Inf, xmax = 0), ymin = Inf, ymax = 0,alpha=0.005,fill="royalblue2") +
  scale_size_continuous(range = c(1, 10)) +
  ggtitle("Japan") +
  xlab(NULL) +
  ylab(NULL)

tiff("Figures_Nace/RCA_Comparison_Nace_1st.jpg", width = 12, height = 7, units = 'in', res = 200)
multiplot(FigUS_1st, FigCN_1st, FigKR_1st, FigJP_1st, cols=2)
dev.off()

tiff("Figures_Nace/RCA_Comparison_Nace_1st_optb.jpg", width = 12, height = 7, units = 'in', res = 200)
multiplot(FigUS_1stb, FigCN_1stb, FigKR_1stb, FigJP_1stb, cols=2)
dev.off()

#2.2.Second period ----
reg_tech2_countries <- read.csv("Data_Nace/reg_tech_SecondPeriod.csv", sep = ";", header = TRUE, dec=",")
#2.2.1. Second Period Countries ----
mat_reg_tech2_countries <- reg_tech2_countries %>%
  arrange(nace2_code, ctry_code) %>%
  pivot_wider(names_from = nace2_code, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

rownames(mat_reg_tech2_countries) <- mat_reg_tech2_countries %>% pull(ctry_code)

mat_reg_tech2_countries %<>% select(-ctry_code) %>%
  as.matrix() %>%
  round()

reg_RCA2_countries <- mat_reg_tech2_countries %>% location.quotient(binary = F) %>% 
  as.data.frame() %>% 
  rownames_to_column("ctry_code") %>% 
  as_tibble() %>% 
  gather(key = "nace2_code", value = "RCA", -ctry_code) %>%
  arrange(ctry_code, nace2_code)

###2.2.2. Second Period AI----
reg_tech2_AI <- read.csv("Data_Nace/reg_techAI_SecondPeriod.csv", sep = ";", header = TRUE, dec=",")

mat_reg_tech2_AI <- reg_tech2_AI %>%
  arrange(nace2_code, ctry_code) %>%
  pivot_wider(names_from = nace2_code, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

rownames(mat_reg_tech2_AI) <- mat_reg_tech2_AI %>% pull(ctry_code)

mat_reg_tech2_AI %<>% select(-ctry_code) %>%
  as.matrix() %>%
  round()

reg_RCA2_AI <- mat_reg_tech2_AI %>% location.quotient(binary = F) %>% 
  as.data.frame() %>% 
  rownames_to_column("ctry_code") %>% 
  as_tibble() %>% 
  gather(key = "nace2_code", value = "RCA", -ctry_code) %>%
  arrange(ctry_code, nace2_code)

#select countries
US_Second_period <- reg_RCA2_countries[,2:3][reg_RCA2_countries$ctry_code == "US",]
CN_Second_period <- reg_RCA2_countries[,2:3][reg_RCA2_countries$ctry_code == "CN",]
KR_Second_period <- reg_RCA2_countries[,2:3][reg_RCA2_countries$ctry_code == "KR",]
JP_Second_period <- reg_RCA2_countries[,2:3][reg_RCA2_countries$ctry_code == "JP",]
AI_Second_period <- reg_RCA2_AI[,2:3][reg_RCA2_AI$ctry_code == "AI_pat",]

Second_period <- merge(merge(merge(merge(merge(
  nace2_names,US_Second_period), CN_Second_period, by = "nace2_code"), KR_Second_period, by = "nace2_code"), 
  JP_Second_period, by = "nace2_code"), AI_Second_period, by = "nace2_code")
names(Second_period) = c("nace2_code", "nace2_descr", "RCA_US", "RCA_CN","RCA_KR","RCA_JP", "RCA_AI")
KnwComp_Country_2nd_RCA <- read.csv("Data_calculations_Nace/KnowledgeComp_PerCountry_2nd_All_RCAs.csv", sep = ";", header = T, dec=",")
KnwComp_Country_2nd_RCA <- as.data.frame(t(KnwComp_Country_2nd_RCA))
KnwComp_Country_2nd_RCA <- KnwComp_Country_2nd_RCA %>%
  row_to_names(row_number = 1)
KnwComp_Country_2nd_RCA<- KnwComp_Country_2nd_RCA[(-85),]
KnwComp_Country_2nd_RCA$nace2_code <- Second_period$nace2_code
Second_period$US_Com <- KnwComp_Country_2nd_RCA$US3
Second_period$CN_Com <- KnwComp_Country_2nd_RCA$CN3
Second_period$KR_Com <- KnwComp_Country_2nd_RCA$KR3
Second_period$JP_Com <- KnwComp_Country_2nd_RCA$JP3
i <- c(8:11)
Second_period[ , i] <- apply(Second_period[ , i], 2,            # Specify own function within apply
                             function(x) as.integer(as.character(x)))
Second_period$nace2_code <-as.factor(Second_period$nace2_code)
#remove files we don't need:
rm(AI_Second_period, CN_Second_period, JP_Second_period, KR_Second_period, KnwComp_Country_2nd_RCA,
   mat_reg_tech2_AI, mat_reg_tech2_countries, reg_RCA2_AI, reg_RCA2_countries, reg_tech2_AI, reg_tech2_countries,
   US_Second_period, i)

#Now we plot:
FigUS_2nd <- ggplot(Second_period, aes(x=log10(RCA_US), y=log10(RCA_AI), label = '')) + 
  geom_point(aes(colour = nace2_code, size = US_Com),show.legend = F, stroke = 2) +  
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2)+
  geom_vline(xintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2) +
  geom_label_repel(data = Second_period, aes(label = ifelse(RCA_US>1 & RCA_AI>1,as.character(nace2_descr),''), size = 2), show.legend = F) +
  geom_rect(aes(NULL, NULL, xmin = Inf, xmax = 0), ymin = Inf, ymax = 0,alpha=0.005,fill="royalblue2") +
  scale_size_continuous(range = c(1, 10)) +
  ggtitle("United States") +
  xlab(NULL) +
  ylab(NULL) +
  ylim(-1,1) +
  xlim(-0.6, 0.6)

FigUS_2ndb <- ggplot(Second_period, aes(x=log10(RCA_US), y=log10(RCA_AI), label = '')) + 
  geom_point(aes(colour = nace2_code, size = US_Com),show.legend = F, stroke = 2) +  
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2)+
  geom_vline(xintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2) +
  geom_label_repel(data = Second_period, aes(label = ifelse(RCA_US>1 & RCA_AI>1,as.character(nace2_descr),''), size = 2), show.legend = F) +
  geom_rect(aes(NULL, NULL, xmin = Inf, xmax = 0), ymin = Inf, ymax = 0,alpha=0.005,fill="royalblue2") +
  scale_size_continuous(range = c(1, 10)) +
  ggtitle("United States") +
  xlab(NULL) +
  ylab(NULL)

FigCN_2nd <-ggplot(Second_period, aes(x=log10(RCA_CN), y=log10(RCA_AI), label = '')) + 
  geom_point(aes(colour = nace2_code, size = CN_Com),show.legend = F, stroke = 2) +  
  geom_label_repel(data = Second_period, aes(label = ifelse(RCA_CN>1 & RCA_AI>1,as.character(nace2_descr),''), size = 2), show.legend = F) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2)+
  geom_vline(xintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2) +
  geom_rect(aes(NULL, NULL, xmin = Inf, xmax = 0), ymin = Inf, ymax = 0,alpha=0.005,fill="royalblue2") +
  scale_size_continuous(range = c(1, 10)) +
  ggtitle("China") +
  xlab(NULL) +
  ylab(NULL)+
  ylim(-1,1) +
  xlim(-0.6, 0.6)

FigCN_2ndb <-ggplot(Second_period, aes(x=log10(RCA_CN), y=log10(RCA_AI), label = '')) + 
  geom_point(aes(colour = nace2_code, size = CN_Com),show.legend = F, stroke = 2) +  
  geom_label_repel(data = Second_period, aes(label = ifelse(RCA_CN>1 & RCA_AI>1,as.character(nace2_descr),''), size = 2), show.legend = F) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2)+
  geom_vline(xintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2) +
  geom_rect(aes(NULL, NULL, xmin = Inf, xmax = 0), ymin = Inf, ymax = 0,alpha=0.005,fill="royalblue2") +
  scale_size_continuous(range = c(1, 10)) +
  ggtitle("China") +
  xlab(NULL) +
  ylab(NULL)

FigKR_2nd <-ggplot(Second_period, aes(x=log10(RCA_KR), y=log10(RCA_AI), label = '')) + 
  geom_point(aes(colour = nace2_code, size = KR_Com),show.legend = F, stroke = 2) +  
  geom_label_repel(data = Second_period, aes(label = ifelse(RCA_KR>1 & RCA_AI>1,as.character(nace2_descr),''), size = 2), show.legend = F) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2)+
  geom_vline(xintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2) +
  geom_rect(aes(NULL, NULL, xmin = Inf, xmax = 0), ymin = Inf, ymax = 0,alpha=0.005,fill="royalblue2") +
  scale_size_continuous(range = c(1, 10)) +
  ggtitle("South Korea") +
  xlab(NULL) +
  ylab(NULL)+
  ylim(-1,1) +
  xlim(-0.6, 0.6)

FigKR_2ndb <-ggplot(Second_period, aes(x=log10(RCA_KR), y=log10(RCA_AI), label = '')) + 
  geom_point(aes(colour = nace2_code, size = KR_Com),show.legend = F, stroke = 2) +  
  geom_label_repel(data = Second_period, aes(label = ifelse(RCA_KR>1 & RCA_AI>1,as.character(nace2_descr),''), size = 2), show.legend = F) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2)+
  geom_vline(xintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2) +
  geom_rect(aes(NULL, NULL, xmin = Inf, xmax = 0), ymin = Inf, ymax = 0,alpha=0.005,fill="royalblue2") +
  scale_size_continuous(range = c(1, 10)) +
  ggtitle("South Korea") +
  xlab(NULL) +
  ylab(NULL)

FigJP_2nd <-ggplot(Second_period, aes(x=log10(RCA_JP), y=log10(RCA_AI), label = '')) + 
  geom_point(aes(colour = nace2_code, size = JP_Com),show.legend = F, stroke = 2) +  
  geom_label_repel(data = Second_period, aes(label = ifelse(RCA_JP>1 & RCA_AI>1,as.character(nace2_descr),''), size = 2), 
                  show.legend = F, nudge_y = -0.2, nudge_x = 0.1) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2)+
  geom_vline(xintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2) +
  geom_rect(aes(NULL, NULL, xmin = Inf, xmax = 0), ymin = Inf, ymax = 0,alpha=0.005,fill="royalblue2") +
  scale_size_continuous(range = c(1, 10)) +
  ggtitle("Japan") +
  xlab(NULL) +
  ylab(NULL)+
  ylim(-1,1) +
  xlim(-0.6, 0.6)

FigJP_2ndb <-ggplot(Second_period, aes(x=log10(RCA_JP), y=log10(RCA_AI), label = '')) + 
  geom_point(aes(colour = nace2_code, size = JP_Com),show.legend = F, stroke = 2) +  
  geom_label_repel(data = Second_period, aes(label = ifelse(RCA_JP>1 & RCA_AI>1,as.character(nace2_descr),''), size = 2), show.legend = F) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2)+
  geom_vline(xintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2) +
  geom_rect(aes(NULL, NULL, xmin = Inf, xmax = 0), ymin = Inf, ymax = 0,alpha=0.005,fill="royalblue2") +
  scale_size_continuous(range = c(1, 10)) +
  ggtitle("Japan") +
  xlab(NULL) +
  ylab(NULL)

tiff("Figures_Nace/RCA_Comparison_Nace_2nd.jpg", width = 12, height = 7, units = 'in', res = 200)
multiplot(FigUS_2nd, FigCN_2nd, FigKR_2nd, FigJP_2nd, cols=2)
dev.off()

tiff("Figures_Nace/RCA_Comparison_Nace_2nd_optb.jpg", width = 12, height = 7, units = 'in', res = 200)
multiplot(FigUS_2ndb, FigCN_2ndb, FigKR_2ndb, FigJP_2ndb, cols=2)
dev.off()

#2.3.Third period ----
reg_tech3_countries <- read.csv("Data_Nace/reg_tech_ThirdPeriod.csv", sep = ";", header = TRUE, dec=",")
#2.3.1. Third Period Countries ----
mat_reg_tech3_countries <- reg_tech3_countries %>%
  arrange(nace2_code, ctry_code) %>%
  pivot_wider(names_from = nace2_code, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

rownames(mat_reg_tech3_countries) <- mat_reg_tech3_countries %>% pull(ctry_code)

mat_reg_tech3_countries %<>% select(-ctry_code) %>%
  as.matrix() %>%
  round()

reg_RCA3_countries <- mat_reg_tech3_countries %>% location.quotient(binary = F) %>% 
  as.data.frame() %>% 
  rownames_to_column("ctry_code") %>% 
  as_tibble() %>% 
  gather(key = "nace2_code", value = "RCA", -ctry_code) %>%
  arrange(ctry_code, nace2_code)

###2.3.2. Third Period AI----
reg_tech3_AI <- read.csv("Data_Nace/reg_techAI_ThirdPeriod.csv", sep = ";", header = TRUE, dec=",")

mat_reg_tech3_AI <- reg_tech3_AI %>%
  arrange(nace2_code, ctry_code) %>%
  pivot_wider(names_from = nace2_code, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

rownames(mat_reg_tech3_AI) <- mat_reg_tech3_AI %>% pull(ctry_code)

mat_reg_tech3_AI %<>% select(-ctry_code) %>%
  as.matrix() %>%
  round()

reg_RCA3_AI <- mat_reg_tech3_AI %>% location.quotient(binary = F) %>% 
  as.data.frame() %>% 
  rownames_to_column("ctry_code") %>% 
  as_tibble() %>% 
  gather(key = "nace2_code", value = "RCA", -ctry_code) %>%
  arrange(ctry_code, nace2_code)

#select countries
US_Third_period <- reg_RCA3_countries[,2:3][reg_RCA3_countries$ctry_code == "US",]
CN_Third_period <- reg_RCA3_countries[,2:3][reg_RCA3_countries$ctry_code == "CN",]
KR_Third_period <- reg_RCA3_countries[,2:3][reg_RCA3_countries$ctry_code == "KR",]
JP_Third_period <- reg_RCA3_countries[,2:3][reg_RCA3_countries$ctry_code == "JP",]
AI_Third_period <- reg_RCA3_AI[,2:3][reg_RCA3_AI$ctry_code == "AI_pat",]

Third_period <- merge(merge(merge(merge(merge(
  nace2_names,US_Third_period), CN_Third_period, by = "nace2_code"), KR_Third_period, by = "nace2_code"), 
  JP_Third_period, by = "nace2_code"), AI_Third_period, by = "nace2_code")
names(Third_period) = c("nace2_code", "nace2_descr", "RCA_US", "RCA_CN","RCA_KR","RCA_JP", "RCA_AI")
KnwComp_Country_3rd_RCA <- read.csv("Data_calculations_Nace/KnowledgeComp_PerCountry_3rd_All_RCAs.csv", sep = ";", header = T, dec=",")
KnwComp_Country_3rd_RCA <- as.data.frame(t(KnwComp_Country_3rd_RCA))
KnwComp_Country_3rd_RCA <- KnwComp_Country_3rd_RCA %>%
  row_to_names(row_number = 1)
KnwComp_Country_3rd_RCA<- KnwComp_Country_3rd_RCA[(-85),]
KnwComp_Country_3rd_RCA$nace2_code <- Third_period$nace2_code
Third_period$US_Com <- KnwComp_Country_3rd_RCA$US3
Third_period$CN_Com <- KnwComp_Country_3rd_RCA$CN3
Third_period$KR_Com <- KnwComp_Country_3rd_RCA$KR3
Third_period$JP_Com <- KnwComp_Country_3rd_RCA$JP3
i <- c(8:11)
Third_period[ , i] <- apply(Third_period[ , i], 2,            # Specify own function within apply
                            function(x) as.integer(as.character(x)))

Third_period$nace2_code <- as.factor(Third_period$nace2_code)
#remove files we don't need:
rm(AI_Third_period, CN_Third_period, JP_Third_period, KR_Third_period, KnwComp_Country_3rd_RCA,
   mat_reg_tech3_AI, mat_reg_tech3_countries, reg_RCA3_AI, reg_RCA3_countries, reg_tech3_AI, reg_tech3_countries,
   US_Third_period, i)

#Now we plot:
FigUS_3rd <- ggplot(Third_period, aes(x=log10(RCA_US), y=log10(RCA_AI), label = '')) + 
  geom_point(aes(colour = nace2_code, size = US_Com),show.legend = F, stroke = 2) +  
  geom_text() +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2)+
  geom_vline(xintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2) +
  geom_label_repel(data = Third_period, aes(label = ifelse(RCA_US>1 & RCA_AI>1,as.character(nace2_descr),''), size=2), 
                   show.legend = F, nudge_y = -0.6, nudge_x = -0.1) +
  geom_rect(aes(NULL, NULL, xmin = Inf, xmax = 0), ymin = Inf, ymax = 0,alpha=0.005,fill="royalblue2") +
  scale_size_continuous(range = c(1, 10)) +
  ggtitle("United States") +
  xlab(NULL) +
  ylab(NULL) +
  ylim(-1,1) +
  xlim(-0.6, 0.6)

FigUS_3rdb <- ggplot(Third_period, aes(x=log10(RCA_US), y=log10(RCA_AI), label = '')) + 
  geom_point(aes(colour = nace2_code, size = US_Com),show.legend = F, stroke = 2) +  
  geom_text() +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2)+
  geom_vline(xintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2) +
  geom_label_repel(data = Third_period, aes(label = ifelse(RCA_US>1 & RCA_AI>1,as.character(nace2_descr),''), size=2), show.legend = F) +
  geom_rect(aes(NULL, NULL, xmin = Inf, xmax = 0), ymin = Inf, ymax = 0,alpha=0.005,fill="royalblue2") +
  scale_size_continuous(range = c(1, 10)) +
  ggtitle("United States") +
  xlab(NULL) +
  ylab(NULL)

FigCN_3rd <-ggplot(Third_period, aes(x=log10(RCA_CN), y=log10(RCA_AI), label = '')) + 
  geom_point(aes(colour = nace2_code, size = CN_Com),show.legend = F, stroke = 2) +  
  geom_text() +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2)+
  geom_vline(xintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2)  +
  geom_label_repel(data = Third_period, aes(label = ifelse(RCA_CN>1 & RCA_AI>1,as.character(nace2_descr),''), size = 2 ), show.legend = F) +
  geom_rect(aes(NULL, NULL, xmin = Inf, xmax = 0), ymin = Inf, ymax = 0,alpha=0.005,fill="royalblue2") +
  scale_size_continuous(range = c(1, 10)) +
  ggtitle("China") +
  xlab(NULL) +
  ylab(NULL)+
  ylim(-1,1) +
  xlim(-0.6, 0.6)

FigCN_3rdb <-ggplot(Third_period, aes(x=log10(RCA_CN), y=log10(RCA_AI), label = '')) + 
  geom_point(aes(colour = nace2_code, size = CN_Com),show.legend = F, stroke = 2) +  
  geom_text() +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2)+
  geom_vline(xintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2)  +
  geom_label_repel(data = Third_period, aes(label = ifelse(RCA_CN>1 & RCA_AI>1,as.character(nace2_descr),''), size = 2 ), show.legend = F) +
  geom_rect(aes(NULL, NULL, xmin = Inf, xmax = 0), ymin = Inf, ymax = 0,alpha=0.005,fill="royalblue2") +
  scale_size_continuous(range = c(1, 10)) +
  ggtitle("China") +
  xlab(NULL) +
  ylab(NULL)

FigKR_3rd <-ggplot(Third_period, aes(x=log10(RCA_KR), y=log10(RCA_AI), label = '')) + 
  geom_point(aes(colour = nace2_code, size = KR_Com),show.legend = F, stroke = 2) +  
  geom_text() +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2)+
  geom_vline(xintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2) +
  geom_label_repel(data = Third_period, aes(label = ifelse(RCA_KR>1 & RCA_AI>1,as.character(nace2_descr),''), size = 2 ), show.legend = F) +
  geom_rect(aes(NULL, NULL, xmin = Inf, xmax = 0), ymin = Inf, ymax = 0,alpha=0.005,fill="royalblue2") +
  scale_size_continuous(range = c(1, 10)) +
  ggtitle("South Korea") +
  xlab(NULL) +
  ylab(NULL)+
  ylim(-1,1) +
  xlim(-0.6, 0.6)

FigKR_3rdb <-ggplot(Third_period, aes(x=log10(RCA_KR), y=log10(RCA_AI), label = '')) + 
  geom_point(aes(colour = nace2_code, size = KR_Com),show.legend = F, stroke = 2) +  
  geom_text() +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2)+
  geom_vline(xintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2) +
  geom_label_repel(data = Third_period, aes(label = ifelse(RCA_KR>1 & RCA_AI>1,as.character(nace2_descr),''), size = 2 ), show.legend = F) +
  geom_rect(aes(NULL, NULL, xmin = Inf, xmax = 0), ymin = Inf, ymax = 0,alpha=0.005,fill="royalblue2") +
  scale_size_continuous(range = c(1, 10)) +
  ggtitle("South Korea") +
  xlab(NULL) +
  ylab(NULL)

FigJP_3rd <-ggplot(Third_period, aes(x=log10(RCA_JP), y=log10(RCA_AI), label = '')) + 
  geom_point(aes(colour = nace2_code, size = JP_Com),show.legend = F, stroke = 2) +  
  geom_text() +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2)+
  geom_vline(xintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2) +
  geom_label_repel(data = Third_period, aes(label = ifelse(RCA_JP>1 & RCA_AI>1,as.character(nace2_descr),''), size = 2), show.legend = F) +
  geom_rect(aes(NULL, NULL, xmin = Inf, xmax = 0), ymin = Inf, ymax = 0,alpha=0.005,fill="royalblue2") +
  scale_size_continuous(range = c(1, 10)) +
  ggtitle("Japan") +
  xlab(NULL) +
  ylab(NULL)+
  ylim(-1,1) +
  xlim(-0.6, 0.6)

FigJP_3rdb <-ggplot(Third_period, aes(x=log10(RCA_JP), y=log10(RCA_AI), label = '')) + 
  geom_point(aes(colour = nace2_code, size = JP_Com),show.legend = F, stroke = 2) +  
  geom_text() +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2)+
  geom_vline(xintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2) +
  geom_label_repel(data = Third_period, aes(label = ifelse(RCA_JP>1 & RCA_AI>1,as.character(nace2_descr),''), size = 2), show.legend = F) +
  geom_rect(aes(NULL, NULL, xmin = Inf, xmax = 0), ymin = Inf, ymax = 0,alpha=0.005,fill="royalblue2") +
  scale_size_continuous(range = c(1, 10)) +
  ggtitle("Japan") +
  xlab(NULL) +
  ylab(NULL)

tiff("Figures_Nace/RCA_Comparison_Nace_3rd.jpg", width = 12, height = 7, units = 'in', res = 200)
multiplot(FigUS_3rd, FigCN_3rd, FigKR_3rd, FigJP_3rd, cols=2)
dev.off()

tiff("Figures_Nace/RCA_Comparison_Nace_3rd_optb.jpg", width = 12, height = 7, units = 'in', res = 200)
multiplot(FigUS_3rdb, FigCN_3rdb, FigKR_3rdb, FigJP_3rdb, cols=2)
dev.off()

#2.4. AI perspective----
KnwComp_Country_1st_RCA <- read.csv("Data_calculations_Nace/KnowledgeComp_PerCountry_1st_All_RCAs_AI.csv", sep = ";", header = T, dec=",")
KnwComp_Country_1st_RCA <- as.data.frame(t(KnwComp_Country_1st_RCA))
KnwComp_Country_1st_RCA <- KnwComp_Country_1st_RCA %>%
  row_to_names(row_number = 1)
KnwComp_Country_1st_RCA<- KnwComp_Country_1st_RCA[(-85),]
First_period$AI_Com <- KnwComp_Country_1st_RCA$AI_pat3
i <- c(11:12)
First_period[ , i] <- apply(First_period[ , i], 2,            # Specify own function within apply
                            function(x) as.integer(as.character(x)))
KnwComp_Country_2nd_RCA <- read.csv("Data_calculations_Nace/KnowledgeComp_PerCountry_2nd_All_RCAs_AI.csv", sep = ";", header = T, dec=",")
KnwComp_Country_2nd_RCA <- as.data.frame(t(KnwComp_Country_2nd_RCA))
KnwComp_Country_2nd_RCA <- KnwComp_Country_2nd_RCA %>%
  row_to_names(row_number = 1)
KnwComp_Country_2nd_RCA<- KnwComp_Country_2nd_RCA[(-85),]
Second_period$AI_Com <- KnwComp_Country_2nd_RCA$AI_pat3
i <- c(11:12)
Second_period[ , i] <- apply(Second_period[ , i], 2,            # Specify own function within apply
                             function(x) as.integer(as.character(x)))

KnwComp_Country_3rd_RCA <- read.csv("Data_calculations_Nace/KnowledgeComp_PerCountry_3rd_All_RCAs_AI.csv", sep = ";", header = T, dec=",")
KnwComp_Country_3rd_RCA <- as.data.frame(t(KnwComp_Country_3rd_RCA))
KnwComp_Country_3rd_RCA <- KnwComp_Country_3rd_RCA %>%
  row_to_names(row_number = 1)
KnwComp_Country_3rd_RCA<- KnwComp_Country_3rd_RCA[(-85),]
Third_period$AI_Com <- KnwComp_Country_3rd_RCA$AI_pat3
i <- c(11:12)
Third_period[ , i] <- apply(Third_period[ , i], 2,            # Specify own function within apply
                            function(x) as.integer(as.character(x)))
First_period$Period <- "Period 1 (1974-1988)"
Second_period$Period <- "Period 2 (1989-2003)"
Third_period$Period <- "Period 3 (2004-2018)"

AI_persp <- rbind(First_period, Second_period, Third_period)

tiff("Figures_Nace/AI_perspective.jpg", width = 10, height = 8, units = 'in', res = 200)
ggplot(AI_persp, aes(x=log10(AI_Com), y=log10(RCA_AI), label = '')) + 
  geom_point(aes(colour = nace2_code, size = AI_Com),show.legend = F, stroke = 2) +  
  geom_text() +
  facet_wrap(~Period, nrow=3) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5, alpha = 1/2)+
  geom_label_repel(data = AI_persp, aes(label = ifelse(AI_Com>1,as.character(nace2_descr),''), size = 3), 
                   show.legend = F, nudge_x = -.2, nudge_y = -.4) +
  ggtitle("AI specializations and complexity over time") +
  scale_size_continuous(range = c(1, 10)) +
  xlab(NULL) +
  ylab(NULL)
dev.off()


#3. Visualization Indicators -----
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
#3.1.Herfindahl and Entropy ----
Indicators1st <- read.csv("Data_calculations_Nace/Indicators_1st_period_Nace.csv", sep = ";", header = TRUE, dec=",")
Indicators2nd <- read.csv("Data_calculations_Nace/Indicators_2nd_period_Nace.csv", sep = ";", header = TRUE, dec=",")
Indicators3rd <- read.csv("Data_calculations_Nace/Indicators_3rd_period_Nace.csv", sep = ";", header = TRUE, dec=",")

Indicators <- rbind(Indicators1st, Indicators2nd, Indicators3rd)
Indicators$Period <- gsub("1st", "Period 1 (1974-1988)", str_trim(Indicators$Period))
Indicators$Period <- gsub("2nd", "Period 2 (1989-2003)", str_trim(Indicators$Period))
Indicators$Period <- gsub("3rd", "Period 3 (2004-2018)", str_trim(Indicators$Period))

Indicators_AI_1st <- read.csv("Data_calculations_Nace/Indicators_1st_period_Nace_AI.csv", sep = ";", header = TRUE, dec=",")
Indicators_AI_2nd <- read.csv("Data_calculations_Nace/Indicators_2nd_period_Nace_AI.csv", sep = ";", header = TRUE, dec=",")
Indicators_AI_3rd <- read.csv("Data_calculations_Nace/Indicators_3rd_period_Nace_AI.csv", sep = ";", header = TRUE, dec=",")
Indicators_AI <- rbind(Indicators_AI_1st, Indicators_AI_2nd, Indicators_AI_3rd)
Indicators_AI$Period <- gsub("1st", "Period 1 (1974-1988)", str_trim(Indicators_AI$Period))
Indicators_AI$Period <- gsub("2nd", "Period 2 (1989-2003)", str_trim(Indicators_AI$Period))
Indicators_AI$Period <- gsub("3rd", "Period 3 (2004-2018)", str_trim(Indicators_AI$Period))

Indicators_AI2 <- Indicators_AI[Indicators_AI$X == "AI_pat", ]

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

Herfindahl_Countriesb <- ggplot(Indicators_countries, aes(x=X, y=Herfindahl, fill=Period)) +
  geom_bar(stat="identity", position=position_dodge(),show.legend = F)+theme_minimal()+ xlab(NULL) + ylab(NULL) +
  scale_y_continuous(limits=c(.035,0.065),oob = rescale_none) +
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
  scale_y_continuous(limits=c(4.4,5.6),oob = rescale_none)+
  ggtitle("Countries Shanon Index")


tiff("Figures_Nace/Indicators_Herfindahl.jpg", width = 6, height = 4, units = 'in', res = 200)
multiplot(Herfindahl_Countries, Herfindahl_AI, cols=2)
dev.off()

tiff("Figures_Nace/Indicators_Herfindahl_optb.jpg", width = 6, height = 4, units = 'in', res = 200)
multiplot(Herfindahl_Countriesb, Herfindahl_AI, cols=2)
dev.off()

tiff("Figures_Nace/Indicators_Entropy.jpg", width = 6, height = 4, units = 'in', res = 200)
multiplot(Entropy_RCA_Countries, Entropy_RCA_AI, cols=2)
dev.off()

tiff("Figures_Nace/Indicators_Entropy_optb.jpg", width = 6, height = 4, units = 'in', res = 200)
multiplot(Entropy_RCA_Countriesb, Entropy_RCA_AI, cols=2)
dev.off()

#3.2.Relatedness ----
Relatedness_1st <- read.csv("Data_calculations_Nace/Relatedness_1st_period_Nace.csv", sep = ";", header = TRUE, dec=",")
Relatedness_2nd <- read.csv("Data_calculations_Nace/Relatedness_2nd_period_Nace.csv", sep = ";", header = TRUE, dec=",")
Relatedness_3rd <- read.csv("Data_calculations_Nace/Relatedness_3rd_period_Nace.csv", sep = ";", header = TRUE, dec=",")
Relatedness <- rbind(Relatedness_1st, Relatedness_2nd, Relatedness_3rd)

Relatedness$Period <- gsub("1st", "Period 1 (1974-1988)", str_trim(Relatedness$Period))
Relatedness$Period <- gsub("2nd", "Period 2 (1989-2003)", str_trim(Relatedness$Period))
Relatedness$Period <- gsub("3rd", "Period 3 (2004-2018)", str_trim(Relatedness$Period))

Relatedness1 <- Relatedness[,c(1,17,3)]
names(Relatedness1) <- c("Country", "Period", "Value")
Relatedness1$Indicator <- "Overall Relatedness"

Relatedness2 <- Relatedness[,c(1,17,6)]
names(Relatedness2) <- c("Country", "Period", "Value")
Relatedness2$Indicator <- "AI-core codes"

Relatedness3 <- Relatedness[,c(1,17,9)]
names(Relatedness3) <- c("Country", "Period", "Value")
Relatedness3$Indicator <- "AI-related codes"

Relatedness4 <- Relatedness[,c(1,17,12)]
names(Relatedness4) <- c("Country", "Period", "Value")
Relatedness4$Indicator <- "Surrounding codes"

Relatedness <- rbind(Relatedness1, Relatedness2, Relatedness3, Relatedness4)
rm(Relatedness1, Relatedness2, Relatedness3, Relatedness4)

Relatedness_AI <- Relatedness[Relatedness$Country == "AI", ]
Relatedness <- Relatedness[Relatedness$Country != "AI", ]

Rel_byP_a<- ggplot(Relatedness, aes(x=Country, y=Value, fill=Indicator)) +
  geom_bar(stat="identity", position=position_dodge(), show.legend = F)+theme_minimal()+ xlab(NULL) + ylab("Relatedness") +
  facet_wrap(~Period, ncol = 3)+
  scale_fill_brewer(palette="Paired") + theme_classic() + 
  ggtitle("Countries Relatedness in the considered Nace fields") + 
  scale_y_continuous(limits=c(.35,3.8),oob = rescale_none)

Rel_byAI_a<- ggplot(Relatedness_AI, aes(x=Country, y=Value, fill=Indicator)) +
  geom_bar(stat="identity", position=position_dodge())+theme_minimal()+ labs(x = "") + 
  scale_x_discrete(labels = NULL) + ylab("Relatedness") +
  facet_wrap(~Period, ncol = 3)+
  scale_fill_brewer(palette="Paired") + theme_classic() + theme(legend.position="bottom") +
  ggtitle("AI Relatedness in the considered Nace fields") + 
  scale_y_continuous(limits=c(0.1,4),oob = rescale_none)

Rel_byP_b <- ggplot(Relatedness, aes(x=Country, y=Value, fill=Period)) +
  geom_bar(stat="identity", position=position_dodge(), show.legend = F)+theme_minimal()+ xlab(NULL) + ylab("Relatedness") +
  facet_wrap(~Indicator, ncol = 4) +
  scale_fill_brewer(palette="Paired") + theme_classic() +
  ggtitle("Countries Relatedness in the considered Nace fields") + 
  scale_y_continuous(limits=c(.35,3.8),oob = rescale_none)

Rel_byAI_b<- ggplot(Relatedness_AI, aes(x=Country, y=Value, fill=Period)) +
  geom_bar(stat="identity", position=position_dodge())+theme_minimal() + labs(x = "") +
  scale_x_discrete(labels = NULL) + ylab("Relatedness") +
  facet_wrap(~Indicator, ncol = 4) +
  scale_fill_brewer(palette="Paired") + theme_classic() + theme(legend.position="bottom") +
  ggtitle("AI Relatedness in the considered Nace fields")+
  scale_y_continuous(limits=c(0.1,4),oob = rescale_none)

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
  ggtitle("Countries Relatedness in the considered Nace fields") + 
  scale_y_continuous(limits=c(.35,3.8),oob = rescale_none)

Rel_byAI_c<- ggplot(Relatedness_AI2, aes(x=Country, y=Value, fill=Period)) +
  geom_bar(stat="identity", position=position_dodge(), show.legend = F)+theme_minimal() + labs(x = "") +
  scale_x_discrete(labels = NULL) + ylab("Relatedness") +
  facet_wrap(~Indicator, ncol = 4) +
  scale_fill_brewer(palette="Paired") + theme_classic() + 
  ggtitle("AI Relatedness in the considered Nace fields")+
  scale_y_continuous(limits=c(0.1,4),oob = rescale_none)

tiff("Figures_Nace/Relatedness_opta.jpg", width = 8, height = 6, units = 'in', res = 200)
multiplot(Rel_byAI_a, Rel_byP_a, cols=1)
dev.off()

tiff("Figures_Nace/Relatedness_optb.jpg", width = 8, height = 6, units = 'in', res = 200)
multiplot(Rel_byAI_b, Rel_byP_b, cols=1)
dev.off()

#3.3.Knowledge Complexity ----
KnowlComp_1st <- read.csv("Data_calculations_Nace/KnowledgeComp_PerCountry_1st_All_RCAs.csv", sep = ";", header = TRUE, dec=",")
KnowlComp_2nd <- read.csv("Data_calculations_Nace/KnowledgeComp_PerCountry_2nd_All_RCAs.csv", sep = ";", header = TRUE, dec=",")
KnowlComp_3rd <- read.csv("Data_calculations_Nace/KnowledgeComp_PerCountry_3rd_All_RCAs.csv", sep = ";", header = TRUE, dec=",")

KnowlComp_1st$Period <- "Period 1 (1974-1988)"
KnowlComp_2nd$Period <- "Period 2 (1989-2003)"
KnowlComp_3rd$Period <- "Period 3 (2004-2018)"
KnowledgeCompl <- rbind(KnowlComp_1st, KnowlComp_2nd, KnowlComp_3rd)

KnowledgeCompl <- KnowledgeCompl[KnowledgeCompl$X == "US3" | 
                                   KnowledgeCompl$X == "CN3"| 
                                   KnowledgeCompl$X == "KR3"| 
                                   KnowledgeCompl$X == "JP3", ]

KnowledgeCompl$"AI-core codes" = rowSums(KnowledgeCompl[,c("X26.2", "X26.5", "X26.51", "X62")])
KnowledgeCompl$"Overall Complexity" = rowSums(KnowledgeCompl[,c(2:85)])
KnowledgeCompl$"AI-related codes" = rowSums(KnowledgeCompl[,c("X26.3", "X26.1", "X26.7", "X27.33", 
                                                              "X27.4", "X27.5", "X28.23", "X32.9")])
KnowledgeCompl$"Surrounding codes" = rowSums(KnowledgeCompl[,c("X25.3", "X28.23", "X29.3", "X26.4", "X27.12", 
                                                               "X27.9", "X28.25")])

KnowledgeCompl2<- KnowledgeCompl[,c(1, 87, 88)]
names(KnowledgeCompl2) <- c("Country", "Period", "Value")
KnowledgeCompl2$Indicator <- "AI-core codes"

KnowledgeCompl3<- KnowledgeCompl[,c(1, 87, 89)]
names(KnowledgeCompl3) <- c("Country", "Period", "Value")
KnowledgeCompl3$Indicator <- "Overall Complexity"

KnowledgeCompl4<- KnowledgeCompl[,c(1, 87, 90)]
names(KnowledgeCompl4) <- c("Country", "Period", "Value")
KnowledgeCompl4$Indicator <- "AI-related codes"

KnowledgeCompl5<- KnowledgeCompl[,c(1, 87, 91)]
names(KnowledgeCompl5) <- c("Country", "Period", "Value")
KnowledgeCompl5$Indicator <- "Surrounding codes"

KnowledgeCompl_all <- rbind(KnowledgeCompl2, KnowledgeCompl3, KnowledgeCompl4, KnowledgeCompl5)
KnowledgeCompl_all$Country <- gsub("CN3", "CN", str_trim(KnowledgeCompl_all$Country))
KnowledgeCompl_all$Country <- gsub("JP3", "JP", str_trim(KnowledgeCompl_all$Country))
KnowledgeCompl_all$Country <- gsub("US3", "US", str_trim(KnowledgeCompl_all$Country))
KnowledgeCompl_all$Country <- gsub("KR3", "KR", str_trim(KnowledgeCompl_all$Country))

KnowlComp_1st_AI <- read.csv("Data_calculations_Nace/KnowledgeComp_PerCountry_1st_All_RCAs_AI.csv", sep = ";", header = TRUE, dec=",")
KnowlComp_2nd_AI <- read.csv("Data_calculations_Nace/KnowledgeComp_PerCountry_2nd_All_RCAs_AI.csv", sep = ";", header = TRUE, dec=",")
KnowlComp_3rd_AI <- read.csv("Data_calculations_Nace/KnowledgeComp_PerCountry_3rd_All_RCAs_AI.csv", sep = ";", header = TRUE, dec=",")

KnowlComp_1st_AI$Period <- "Period 1 (1974-1988)"
KnowlComp_2nd_AI$Period <- "Period 2 (1989-2003)"
KnowlComp_3rd_AI$Period <- "Period 3 (2004-2018)"
KnowledgeCompl_AI <- rbind(KnowlComp_1st_AI, KnowlComp_2nd_AI, KnowlComp_3rd_AI)

KnowledgeCompl_AI <- KnowledgeCompl_AI[KnowledgeCompl_AI$X == "AI_pat3", ]

KnowledgeCompl_AI$"AI-core codes" = rowSums(KnowledgeCompl_AI[,c("X26.2", "X26.5", "X26.51", "X62")])
KnowledgeCompl_AI$"Overall Complexity" = rowSums(KnowledgeCompl_AI[,c(2:85)])
KnowledgeCompl_AI$"AI-related codes" = rowSums(KnowledgeCompl_AI[,c("X26.3", "X26.1", "X26.7", "X27.33", 
                                                                    "X27.4", "X27.5", "X28.23", "X32.9")])
KnowledgeCompl_AI$"Surrounding codes" = rowSums(KnowledgeCompl_AI[,c("X25.3", "X28.23", "X29.3", "X26.4", "X27.12", 
                                                                     "X27.9", "X28.25")])

KnowledgeCompl_AI2<- KnowledgeCompl_AI[,c(1, 87, 88)]
names(KnowledgeCompl_AI2) <- c("Country", "Period", "Value")
KnowledgeCompl_AI2$Indicator <- "AI-core codes"

KnowledgeCompl_AI3<- KnowledgeCompl_AI[,c(1, 87, 89)]
names(KnowledgeCompl_AI3) <- c("Country", "Period", "Value")
KnowledgeCompl_AI3$Indicator <- "Overall Complexity"

KnowledgeCompl_AI4<- KnowledgeCompl_AI[,c(1, 87, 90)]
names(KnowledgeCompl_AI4) <- c("Country", "Period", "Value")
KnowledgeCompl_AI4$Indicator <- "AI-related codes"

KnowledgeCompl_AI5<- KnowledgeCompl_AI[,c(1, 87, 91)]
names(KnowledgeCompl_AI5) <- c("Country", "Period", "Value")
KnowledgeCompl_AI5$Indicator <- "Surrounding codes"

KnowledgeCompl_AI_all <- rbind(KnowledgeCompl_AI2, KnowledgeCompl_AI3, KnowledgeCompl_AI4, KnowledgeCompl_AI5)
KnowledgeCompl_AI_all$Country <- gsub("AI_pat3", "AI", str_trim(KnowledgeCompl_AI_all$Country))

Comp_byP_a <- 
  ggplot(KnowledgeCompl_all, aes(x=Country, y=(Value), fill=Indicator)) +
  geom_bar(stat="identity", position=position_dodge(), show.legend = F)+theme_minimal()+ xlab(NULL) + ylab("Knowledge Complexity") +
  facet_wrap(~Period, ncol = 3)+
  scale_fill_brewer(palette="Paired") + theme_classic() + 
  ggtitle("Countries Knowledge Complexity in the considered Nace fields")

Comp_byAI_a<- 
  ggplot(KnowledgeCompl_AI_all, aes(x=Country, y=Value, fill=Indicator)) +
  geom_bar(stat="identity", position=position_dodge())+theme_minimal()+ labs(x = "") + 
  scale_x_discrete(labels = NULL) + ylab("Knowledge Complexity") +
  facet_wrap(~Period, ncol = 3)+
  scale_fill_brewer(palette="Paired") + theme_classic() + theme(legend.position="bottom") +
  ggtitle("AI Knowledge Complexity in the considered Nace fields") 

Comp_byP_b <- 
  ggplot(KnowledgeCompl_all, aes(x=Country, y=Value, fill=Period)) +
  geom_bar(stat="identity", position=position_dodge(), show.legend = F)+theme_minimal()+ xlab(NULL) + ylab("Knowledge Complexity") +
  facet_wrap(~Indicator, ncol = 4) +
  scale_fill_brewer(palette="Paired") + theme_classic() +
  ggtitle("Countries Knowledge Complexity in the considered Nace fields") 

Comp_byAI_b<- 
  ggplot(KnowledgeCompl_AI_all, aes(x=Country, y=Value, fill=Period)) +
  geom_bar(stat="identity", position=position_dodge())+theme_minimal() + labs(x = "") +
  scale_x_discrete(labels = NULL) + ylab("Knowledge Complexity") +
  facet_wrap(~Indicator, ncol = 4) +
  scale_fill_brewer(palette="Paired") + theme_classic() + theme(legend.position="bottom") +
  ggtitle("AI Knowledge Complexity in the considered Nace fields")

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
  scale_fill_brewer(palette="Paired") + theme_classic() + theme(legend.position="bottom") +
  ggtitle("Countries Knowledge Complexity in the considered Nace fields")

Comp_byAI_c<- 
  ggplot(KnowledgeCompl_AI_all2, aes(x=Country, y=Value, fill=Period)) +
  geom_bar(stat="identity", position=position_dodge())+theme_minimal() + labs(x = "") +
  scale_x_discrete(labels = NULL) + ylab("Knowledge Complexity") +
  facet_wrap(~Indicator, ncol = 4) +
  scale_fill_brewer(palette="Paired") + theme_classic() + theme(legend.position="bottom") +
  ggtitle("AI Knowledge Complexity in the considered Nace fields")

tiff("Figures_Nace/KnowledgeComplexity_opta.jpg", width = 8, height = 6, units = 'in', res = 200)
multiplot(Comp_byAI_a, Comp_byP_a, cols=1)
dev.off()

tiff("Figures_Nace/KnowledgeComplexity_optb.jpg", width = 8, height = 6, units = 'in', res = 200)
multiplot(Comp_byAI_b, Comp_byP_b, cols=1)
dev.off()

tiff("Figures_Nace/Relatedness_and_Complex_countries.jpg", width = 8, height = 6, units = 'in', res = 200)
multiplot(Rel_byP_c, Comp_byP_c, cols=1) 
dev.off()

tiff("Figures_Nace/Relatedness_and_Complex_AI.jpg", width = 8, height = 6, units = 'in', res = 200)
multiplot(Rel_byAI_c, Comp_byAI_c, cols=1) 
dev.off()

#4.Network Metrics----
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
Summary_NetworkMetrics <- read.csv("Data_calculations_Nace/Summary_NetworkMetrics.csv", sep = ";", header = TRUE, dec=",")
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

centrality_betweenness1<-
ggplot(Summary_NetworkMetricsAvr, aes(x=Group.1, y=centrality_betweenness, fill=Period)) +
  geom_bar(stat="identity", position=position_dodge(), show.legend = F)+theme_minimal() + labs(x = "") +
  scale_fill_brewer(palette="Paired") + theme_classic() + theme(legend.position="bottom") +
  ggtitle("Average - Centrality_betweenness (Nace)")+ 
  scale_y_continuous(limits=c(9,19),oob = rescale_none)

centrality_closeness1<-
  ggplot(Summary_NetworkMetricsAvr, aes(x=Group.1, y=centrality_closeness, fill=Period)) +
  geom_bar(stat="identity", position=position_dodge(), show.legend = F)+theme_minimal() + labs(x = "") +
  scale_fill_brewer(palette="Paired") + theme_classic() + theme(legend.position="bottom") +
  ggtitle("Average - Centrality_closeness (Nace)") + 
  scale_y_continuous(limits=c(.005,.02),oob = rescale_none)
  
centrality_eigen1<-
  ggplot(Summary_NetworkMetricsAvr, aes(x=Group.1, y=centrality_eigen, fill=Period)) +
    geom_bar(stat="identity", position=position_dodge())+theme_minimal() + labs(x = "") +
    scale_fill_brewer(palette="Paired") + theme_classic() + theme(legend.position="bottom") +
    ggtitle("Average - Centrality_eigen (Nace)")+ 
    scale_y_continuous(limits=c(.3,.52),oob = rescale_none)  
  
centrality_hub1<-
  ggplot(Summary_NetworkMetricsAvr, aes(x=Group.1, y=centrality_hub, fill=Period)) +
    geom_bar(stat="identity", position=position_dodge(), show.legend = F)+theme_minimal() + labs(x = "") +
    scale_fill_brewer(palette="Paired") + theme_classic() + theme(legend.position="bottom") +
    ggtitle("Average - Centrality_hub (Nace)") + 
    scale_y_continuous(limits=c(.3,.53),oob = rescale_none)

tiff("Figures_Nace/NetworkMetrics_Nace_Best_ones.jpg", width = 8, height = 10, units = 'in', res = 200)
multiplot(centrality_closeness1, centrality_betweenness1, centrality_eigen1, cols=1)
dev.off()

