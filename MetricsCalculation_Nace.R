library(tidyverse) # Collection of all the good stuff like dplyr, ggplot2 ect.
library(magrittr) # For extra-piping operators (eg. %<>%)

library(ggraph) # For ggplot2 style graph plotting

library(EconGeo) # Economic Geography functions

library(data.table) #for reading the big files using fread and for replacing countries names (by AI_pat for example)

library(stringr) #for separating the Nace codes in subclasses

library(janitor) #used here for converting the first column of data to row names.

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
