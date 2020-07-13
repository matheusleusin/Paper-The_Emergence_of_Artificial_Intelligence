library(tidyverse) # Collection of all the good stuff like dplyr, ggplot2 ect.
library(magrittr) # For extra-piping operators (eg. %<>%)

# Network specific
library(tidygraph) # For tidy-style graph manipulation
library(ggraph) # For ggplot2 style graph plotting

library(EconGeo) # Economic Geography functions

rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#1.IPCs: -----
#read the data per period and calculate the RCAs:
reg_tech1 <- read.csv("Data_IPC/reg_techAI_FirstPeriod.csv", sep = ";", header = TRUE, dec=",")
reg_tech2 <- read.csv("Data_IPC/reg_techAI_SecondPeriod.csv", sep = ";", header = TRUE, dec=",")
reg_tech3 <- read.csv("Data_IPC/reg_techAI_ThirdPeriod.csv", sep = ";", header = TRUE, dec=",")

###First Period:
mat_reg_tech1 <- reg_tech1 %>%
  arrange(techn_field_nr, ctry_code) %>%
  pivot_wider(names_from = techn_field_nr, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

rownames(mat_reg_tech1) <- mat_reg_tech1 %>% pull(ctry_code)

mat_reg_tech1 %<>% select(-ctry_code) %>%
  as.matrix() %>%
  round()

reg_RCA1 <- mat_reg_tech1 %>% location.quotient(binary = T) %>% 
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

reg_RCA2 <- mat_reg_tech2 %>% location.quotient(binary = T) %>% 
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

reg_RCA3 <- mat_reg_tech3 %>% location.quotient(binary = T) %>% 
  as.data.frame() %>% 
  rownames_to_column("ctry_code") %>% 
  as_tibble() %>% 
  gather(key = "techn_field_nr", value = "RCA", -ctry_code) %>%
  arrange(ctry_code, techn_field_nr)

AI_reg_RCA1 <- reg_RCA1[reg_RCA1$ctry_code == "AI_pat", ]
AI_reg_RCA2 <- reg_RCA2[reg_RCA2$ctry_code == "AI_pat", ]
AI_reg_RCA3 <- reg_RCA3[reg_RCA3$ctry_code == "AI_pat", ]
AI_reg_RCA1 <- AI_reg_RCA1[,(-1)]
AI_reg_RCA2 <- AI_reg_RCA2[,(-1)]
AI_reg_RCA3 <- AI_reg_RCA3[,(-1)]

AI_RCAs <- merge(AI_reg_RCA1, AI_reg_RCA2, all=TRUE, by="techn_field_nr")
AI_RCAs <- merge(AI_RCAs, AI_reg_RCA3, all=TRUE, by="techn_field_nr")
names(AI_RCAs) <- c("techn_field_nr", "RCA_1st_period", "RCA_2nd_period", "RCA_3rd_period")
AI_RCAs$sum <- rowSums(AI_RCAs[,c(2:4)])
write.csv2(AI_RCAs, file = "Data_IPC/Most_Important_IPCs.csv", row.names = F)

#2.Naces ----
rm(list=ls())
#read the data per period and calculate the RCAs:
reg_tech1 <- read.csv("Data_Nace/reg_techAI_FirstPeriod.csv", sep = ";", header = TRUE, dec=",")
reg_tech2 <- read.csv("Data_Nace/reg_techAI_SecondPeriod.csv", sep = ";", header = TRUE, dec=",")
reg_tech3 <- read.csv("Data_Nace/reg_techAI_ThirdPeriod.csv", sep = ";", header = TRUE, dec=",")

###First Period:
mat_reg_tech1 <- reg_tech1 %>%
  arrange(nace2_code, ctry_code) %>%
  pivot_wider(names_from = nace2_code, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

rownames(mat_reg_tech1) <- mat_reg_tech1 %>% pull(ctry_code)

mat_reg_tech1 %<>% select(-ctry_code) %>%
  as.matrix() %>%
  round()

reg_RCA1 <- mat_reg_tech1 %>% location.quotient(binary = T) %>% 
  as.data.frame() %>% 
  rownames_to_column("ctry_code") %>% 
  as_tibble() %>% 
  gather(key = "nace2_code", value = "RCA", -ctry_code) %>%
  arrange(ctry_code, nace2_code)

###second period:
mat_reg_tech2 <- reg_tech2 %>%
  arrange(nace2_code, ctry_code) %>%
  pivot_wider(names_from = nace2_code, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

rownames(mat_reg_tech2) <- mat_reg_tech2 %>% pull(ctry_code)

mat_reg_tech2 %<>% select(-ctry_code) %>%
  as.matrix() %>%
  round()

reg_RCA2 <- mat_reg_tech2 %>% location.quotient(binary = T) %>% 
  as.data.frame() %>% 
  rownames_to_column("ctry_code") %>% 
  as_tibble() %>% 
  gather(key = "nace2_code", value = "RCA", -ctry_code) %>%
  arrange(ctry_code, nace2_code)

###Third Period:
mat_reg_tech3 <- reg_tech3 %>%
  arrange(nace2_code, ctry_code) %>%
  pivot_wider(names_from = nace2_code, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

rownames(mat_reg_tech3) <- mat_reg_tech3 %>% pull(ctry_code)

mat_reg_tech3 %<>% select(-ctry_code) %>%
  as.matrix() %>%
  round()

reg_RCA3 <- mat_reg_tech3 %>% location.quotient(binary = T) %>% 
  as.data.frame() %>% 
  rownames_to_column("ctry_code") %>% 
  as_tibble() %>% 
  gather(key = "nace2_code", value = "RCA", -ctry_code) %>%
  arrange(ctry_code, nace2_code)

AI_reg_RCA1 <- reg_RCA1[reg_RCA1$ctry_code == "AI_pat", ]
AI_reg_RCA2 <- reg_RCA2[reg_RCA2$ctry_code == "AI_pat", ]
AI_reg_RCA3 <- reg_RCA3[reg_RCA3$ctry_code == "AI_pat", ]
AI_reg_RCA1 <- AI_reg_RCA1[,(-1)]
AI_reg_RCA2 <- AI_reg_RCA2[,(-1)]
AI_reg_RCA3 <- AI_reg_RCA3[,(-1)]

AI_RCAs <- merge(AI_reg_RCA1, AI_reg_RCA2, all=TRUE, by="nace2_code")
AI_RCAs <- merge(AI_RCAs, AI_reg_RCA3, all=TRUE, by="nace2_code")
names(AI_RCAs) <- c("nace2_code", "RCA_1st_period", "RCA_2nd_period", "RCA_3rd_period")
AI_RCAs$sum <- rowSums(AI_RCAs[,c(2:4)])
write.csv2(AI_RCAs, file = "Data_Nace/Most_Important_Naces.csv", row.names = F)
