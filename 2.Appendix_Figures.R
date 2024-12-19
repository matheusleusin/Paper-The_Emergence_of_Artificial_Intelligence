#Figure appendix A (AI technological space based on subclasses)

library(tidyverse) # Collection of all the good stuff like dplyr, ggplot2 ect.
library(magrittr) # For extra-piping operators (eg. %<>%)

library(tidygraph) # For tidy-style graph manipulation
library(ggraph) # For ggplot2 style graph plotting

library(EconGeo) # Economic Geography functions
library(data.table) #for reading the big files using fread and for replacing countries names (by AI_pat for example)

library(netrankr) #library for calculating pagerank related indicators (i.e. centrality_closeness_harmonic and centrality_closeness_residual)

library(dplyr)
library(tidyr)
library(ggrepel)
library(scales) #for scaling without cutting data out
library(patchwork) #for cutting out the X labs while keeping the legend
library(RColorBrewer)

library(ggforce) #for using geom_mark_hull

library(stringr) #for separating the IPC codes in subclasses
library(magrittr) # For extra-piping operators (eg. %<>%)

#1.Calculate Relatedness ----
##1.1.First period -----
rm(list=ls())
#set the working directory to where you saved the R code:
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

group_by_applnID <- function (data){
  data %>%
    group_by(appln_id) %>%
    mutate(field_weight = 1 / n())%>%
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
IPC_all_patents_Part1 <- fread("large_files/All_patents_and_IPC_codes_Part2.csv", header = F, nrow = 20000000)
IPC_all_patents_Part2 <- fread("large_files/All_patents_and_IPC_codes_Part2.csv", header = F, nrow = 20000000, skip = 20000000)
IPC_all_patents_Part3 <- fread("large_files/All_patents_and_IPC_codes_Part2.csv", header = F, nrow = 20000000, skip = 40000000)
IPC_all_patents_Part4 <- fread("large_files/All_patents_and_IPC_codes_Part2.csv", header = F, nrow = 20000000, skip = 60000000)
IPC_all_patents_Part5 <- fread("large_files/All_patents_and_IPC_codes_Part2.csv", header = F, nrow = c, skip = 80000000)

names(IPC_all_patents_Part1) <- c("appln_id", "ctry_code", "ipc_class_symbol", "priority_year")
names(IPC_all_patents_Part2) <- c("appln_id", "ctry_code", "ipc_class_symbol", "priority_year")
names(IPC_all_patents_Part3) <- c("appln_id", "ctry_code", "ipc_class_symbol", "priority_year")
names(IPC_all_patents_Part4) <- c("appln_id", "ctry_code", "ipc_class_symbol", "priority_year")
names(IPC_all_patents_Part5) <- c("appln_id", "ctry_code", "ipc_class_symbol", "priority_year")

#we want to pick only the registers from the first interval first (from 1974 to 1988, including both cited years)
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

#let's drop the columns we won't use (priority_year):
IPC_all_patents_Part1 <- IPC_all_patents_Part1[, c((-4))]
IPC_all_patents_Part2 <- IPC_all_patents_Part2[, c((-4))]
IPC_all_patents_Part3 <- IPC_all_patents_Part3[, c((-4))]
IPC_all_patents_Part4 <- IPC_all_patents_Part4[, c((-4))]
IPC_all_patents_Part5 <- IPC_all_patents_Part5[, c((-4))]

#we combine the 3 files:
IPC_all_patents_FirstPeriod <- rbind(IPC_all_patents_Part1, IPC_all_patents_Part2, IPC_all_patents_Part3, IPC_all_patents_Part4, IPC_all_patents_Part5)
#and exclude the 3 big ones we just used, so we have back our memory:
rm(IPC_all_patents_Part1, IPC_all_patents_Part2, IPC_all_patents_Part3, IPC_all_patents_Part4, IPC_all_patents_Part5)

length(unique(IPC_all_patents_FirstPeriod$appln_id))#4,820,523 unique publication numbers (i.e., unique priorities) out of
length(IPC_all_patents_FirstPeriod$appln_id) #28,705,762 lines of data

#we pick just the subclass for analysis:
IPC_all_patents_FirstPeriod$Subclass <- substr(IPC_all_patents_FirstPeriod$ipc_class_symbol,1,4)

#drop lines that show repeated subclasses for the same patent
IPC_all_patents_FirstPeriod %<>% group_by(appln_id) %>% distinct(Subclass, .keep_all = TRUE) #from 28,705,762 lines to 7,648,401

#Now we read the AI data
patents_AI_specific_1st <- read.csv("other_files/IPCs_AI.csv", sep = ";", header = TRUE, dec=",")

a = 1973
b = 1989

patents_AI_specific_1st <- patents_AI_specific_1st[patents_AI_specific_1st$priority_year < b,]
patents_AI_specific_1st <- patents_AI_specific_1st[patents_AI_specific_1st$priority_year > a,]

patents_AI_specific_1st %<>% distinct(appln_id, .keep_all = TRUE) #435
patents_AI_specific_1st$ctry_code <- "AI_pat"

IPC_all_patents_FirstPeriod$ctry_code <- ifelse(IPC_all_patents_FirstPeriod$appln_id %in% patents_AI_specific_1st$appln_id, 
                                                patents_AI_specific_1st$ctry_code[match(IPC_all_patents_FirstPeriod$appln_id, patents_AI_specific_1st$appln_id)],
                                                IPC_all_patents_FirstPeriod$ctry_code)

table(IPC_all_patents_FirstPeriod$ctry_code)

reg_tech1 <- group_by_applnID(IPC_all_patents_FirstPeriod)
reg_tech1 <- group_by_ctry_and_IPC(reg_tech1)

mat_reg_tech1 <- reg_tech1 %>%
  arrange(Subclass, ctry_code) %>%
  pivot_wider(names_from = Subclass, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

mat_reg_tech1 %<>% remove_rownames %>% column_to_rownames(var="ctry_code") %>%
  as.matrix() %>%
  round()

reg_RCA1st <- mat_reg_tech1 %>% location_quotient(binary = F) %>% 
  as.data.frame() %>% 
  rownames_to_column("ctry_code") %>% 
  as_tibble() %>% 
  gather(key = "Subclass", value = "RCA", -ctry_code) %>%
  arrange(ctry_code, Subclass)

reg_RCA1st_AI <- reg_RCA1st[reg_RCA1st$ctry_code == "AI_pat",]
write.csv2(reg_RCA1st_AI, file = "Files_created_with_the_code/data/files_code_4-digits_analysis/RCA_1st_AI.csv", row.names = F)
rm(reg_RCA1st_AI, reg_RCA1st,mat_reg_tech1, reg_tech1, IPC_all_patents_FirstPeriod, patents_AI_specific_1st)

##1.2.Second period -----
c <- 97664418 -80000000
IPC_all_patents_Part1 <- fread("large_files/All_patents_and_IPC_codes_Part2.csv", header = F, nrow = 20000000)
IPC_all_patents_Part2 <- fread("large_files/All_patents_and_IPC_codes_Part2.csv", header = F, nrow = 20000000, skip = 20000000)
IPC_all_patents_Part3 <- fread("large_files/All_patents_and_IPC_codes_Part2.csv", header = F, nrow = 20000000, skip = 40000000)
IPC_all_patents_Part4 <- fread("large_files/All_patents_and_IPC_codes_Part2.csv", header = F, nrow = 20000000, skip = 60000000)
IPC_all_patents_Part5 <- fread("large_files/All_patents_and_IPC_codes_Part2.csv", header = F, nrow = c, skip = 80000000)

names(IPC_all_patents_Part1) <- c("appln_id", "ctry_code", "ipc_class_symbol", "priority_year")
names(IPC_all_patents_Part2) <- c("appln_id", "ctry_code", "ipc_class_symbol", "priority_year")
names(IPC_all_patents_Part3) <- c("appln_id", "ctry_code", "ipc_class_symbol", "priority_year")
names(IPC_all_patents_Part4) <- c("appln_id", "ctry_code", "ipc_class_symbol", "priority_year")
names(IPC_all_patents_Part5) <- c("appln_id", "ctry_code", "ipc_class_symbol", "priority_year")

#we want to pick only the registers from the period we want (from 1974 to 1988, including both cited years)
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

#let's drop the columns we won't use (priority_year):
IPC_all_patents_Part1 <- IPC_all_patents_Part1[, c((-4))]
IPC_all_patents_Part2 <- IPC_all_patents_Part2[, c((-4))]
IPC_all_patents_Part3 <- IPC_all_patents_Part3[, c((-4))]
IPC_all_patents_Part4 <- IPC_all_patents_Part4[, c((-4))]
IPC_all_patents_Part5 <- IPC_all_patents_Part5[, c((-4))]

#we combine the 3 files:
IPC_all_patents_SecondPeriod <- rbind(IPC_all_patents_Part1, IPC_all_patents_Part2, IPC_all_patents_Part3, IPC_all_patents_Part4, IPC_all_patents_Part5)
#and exclude the 3 big ones we just used, so we have back our memory:
rm(IPC_all_patents_Part1, IPC_all_patents_Part2, IPC_all_patents_Part3, IPC_all_patents_Part4, IPC_all_patents_Part5)

length(unique(IPC_all_patents_SecondPeriod$appln_id))#8,852,648 unique publication numbers (i.e., unique priorities) out of
length(IPC_all_patents_SecondPeriod$appln_id) #63,620,929 lines of data

#we pick just the subclass for analysis:
IPC_all_patents_SecondPeriod$Subclass <- substr(IPC_all_patents_SecondPeriod$ipc_class_symbol,1,4)

#drop lines that show repeated subclasses for the same patent
IPC_all_patents_SecondPeriod %<>% group_by(appln_id) %>% distinct(Subclass, .keep_all = TRUE) #from 63,620,929 lines to 15,057,543 lines

###Now we read the AI data
patents_AI_specific_2nd <- read.csv("other_files/IPCs_AI.csv", sep = ";", header = TRUE, dec=",")

a = 1988
b = 2004

patents_AI_specific_2nd <- patents_AI_specific_2nd[patents_AI_specific_2nd$priority_year < b,]
patents_AI_specific_2nd <- patents_AI_specific_2nd[patents_AI_specific_2nd$priority_year > a,]

patents_AI_specific_2nd %<>% distinct(appln_id, .keep_all = TRUE) #7887
patents_AI_specific_2nd$ctry_code <- "AI_pat"

IPC_all_patents_SecondPeriod$ctry_code <- ifelse(IPC_all_patents_SecondPeriod$appln_id %in% patents_AI_specific_2nd$appln_id, 
                                                 patents_AI_specific_2nd$ctry_code[match(IPC_all_patents_SecondPeriod$appln_id, patents_AI_specific_2nd$appln_id)],
                                                 IPC_all_patents_SecondPeriod$ctry_code)

table(IPC_all_patents_SecondPeriod$ctry_code) #17,340 codes related to AI_pat
reg_tech2 <- group_by_applnID(IPC_all_patents_SecondPeriod)
reg_tech2 <- group_by_ctry_and_IPC(reg_tech2)

mat_reg_tech2 <- reg_tech2 %>%
  arrange(Subclass, ctry_code) %>%
  pivot_wider(names_from = Subclass, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

mat_reg_tech2 %<>% remove_rownames %>% column_to_rownames(var="ctry_code") %>%
  as.matrix() %>%
  round()

reg_RCA2nd <- mat_reg_tech2 %>% location_quotient(binary = F) %>% 
  as.data.frame() %>% 
  rownames_to_column("ctry_code") %>% 
  as_tibble() %>% 
  gather(key = "Subclass", value = "RCA", -ctry_code) %>%
  arrange(ctry_code, Subclass)

reg_RCA2nd_AI <- reg_RCA2nd[reg_RCA2nd$ctry_code == "AI_pat",]
write.csv2(reg_RCA2nd_AI, file = "Files_created_with_the_code/data/files_code_4-digits_analysis/RCA_2nd_AI.csv", row.names = F)

rm(reg_RCA2nd_AI, reg_RCA2nd,mat_reg_tech2, reg_tech2, IPC_all_patents_SecondPeriod, patents_AI_specific_2nd)

##1.3.Third period -----
IPC_all_patents_Part1 <- fread("large_files/All_patents_and_IPC_codes_Part1.csv", header = F)
names(IPC_all_patents_Part1) <- c("appln_id", "ctry_code", "ipc_class_symbol", "priority_year")
IPC_all_patents_Part1 <- IPC_all_patents_Part1[, c((-4))]

length(unique(IPC_all_patents_Part1$appln_id))#16,271,712 unique publication numbers (i.e., unique priorities) out of
length(IPC_all_patents_Part1$appln_id) #120,419,184 lines of data

#we pick just the subclass for analysis:
IPC_all_patents_Part1$Subclass <- substr(IPC_all_patents_Part1$ipc_class_symbol,1,4)

#drop lines that show repeated subclasses for the same patent
IPC_all_patents_Part1 %<>% group_by(appln_id) %>% distinct(Subclass, .keep_all = TRUE) #from 120,419,184 lines to 25,538,071
#important: I'm explicitely dropping geography here by not considering country information in the groupping (i.e., instead of group_by(appln_id, ctry_code); 
#this is on purpose due to the aim of this visualization on the paper but, just for the sake of information: if I'd use the country code in this third period (which is by
#far the one with more patents), there would be 26,103,624 lines of code instead of 25,538,071, which means an increase of 2,17%

###Now we read the AI data
patents_AI_specific_1st <- read.csv("other_files/IPCs_AI.csv", sep = ";", header = TRUE, dec=",")

a = 2003
b = 2019

patents_AI_specific_1st <- patents_AI_specific_1st[patents_AI_specific_1st$priority_year < b,]
patents_AI_specific_1st <- patents_AI_specific_1st[patents_AI_specific_1st$priority_year > a,]

patents_AI_specific_1st %<>% distinct(appln_id, .keep_all = TRUE) #34576
patents_AI_specific_1st$ctry_code <- "AI_pat"

IPC_all_patents_Part1$ctry_code <- ifelse(IPC_all_patents_Part1$appln_id %in% patents_AI_specific_1st$appln_id, 
                                          patents_AI_specific_1st$ctry_code[match(IPC_all_patents_Part1$appln_id, patents_AI_specific_1st$appln_id)],
                                          IPC_all_patents_Part1$ctry_code)
table(IPC_all_patents_Part1$ctry_code) #56,675 codes related to AI_pat

reg_tech1 <- group_by_applnID(IPC_all_patents_Part1)
reg_tech1 <- group_by_ctry_and_IPC(reg_tech1)

mat_reg_tech3 <- reg_tech1 %>%
  arrange(Subclass, ctry_code) %>%
  pivot_wider(names_from = Subclass, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

mat_reg_tech3 %<>% remove_rownames %>% column_to_rownames(var="ctry_code") %>%
  as.matrix() %>%
  round()

reg_RCA3rd <- mat_reg_tech3 %>% location_quotient(binary = F) %>% 
  as.data.frame() %>% 
  rownames_to_column("ctry_code") %>% 
  as_tibble() %>% 
  gather(key = "Subclass", value = "RCA", -ctry_code) %>%
  arrange(ctry_code, Subclass)

reg_RCA3rd_AI <- reg_RCA3rd[reg_RCA3rd$ctry_code == "AI_pat",]
write.csv2(reg_RCA3rd_AI, file = "Files_created_with_the_code/data/files_code_4-digits_analysis/RCA_3rd_AI.csv", row.names = F)
rm(reg_RCA3rd_AI, reg_RCA3rd,mat_reg_tech3, reg_tech1, IPC_all_patents_Part1, patents_AI_specific_1st)

#2.Calculate AI Matrix and generate figure----
##2.1.First period----
rm(list=ls())
patents_AI_specific_1st <- read.csv("other_files/IPCs_AI.csv", sep = ";", header = TRUE, dec=",")

a = 1973
b = 1989

patents_AI_specific_1st <- patents_AI_specific_1st[patents_AI_specific_1st$priority_year < b,]
patents_AI_specific_1st <- patents_AI_specific_1st[patents_AI_specific_1st$priority_year > a,]

#pick just the subclass for analysis:
patents_AI_specific_1st$Subclass <- substr(patents_AI_specific_1st$ipc_class_symbol,1,4)

length(unique(patents_AI_specific_1st$appln_id)) #435
length(unique(patents_AI_specific_1st$Subclass)) #87
patents_AI_specific_1st <- patents_AI_specific_1st[is.na(patents_AI_specific_1st$appln_id)==F,]

#drop lines that show repeated subclasses for the same patent
patents_AI_specific_1st %<>% group_by(appln_id) %>% distinct(Subclass, .keep_all = TRUE)
length(unique(patents_AI_specific_1st$appln_id)) #435
length(unique(patents_AI_specific_1st$Subclass)) #87

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

mat_tech_AI <- create_sparse_matrix(i = patents_AI_specific_1st %>% pull(appln_id),
                                    j = patents_AI_specific_1st %>% pull(Subclass))

mat_tech_AI %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_rel_AI <- mat_tech_AI %>% 
  relatedness(method = "cosine")

IPC_names <- read.csv("other_files/ipc_technology.csv", sep = ";", header = TRUE)%>%
  mutate(Subclass = ipc_maingroup_symbol) %>%
  arrange(Subclass)

IPC_names$Subclass <- substr(IPC_names$Subclass,1,4)
IPC_names<-distinct(IPC_names, Subclass, .keep_all = TRUE)
AI_RCA_1st <- read.csv("Files_created_with_the_code/data/files_code_4-digits_analysis/RCA_1st_AI.csv", sep = ";", header = TRUE, dec=",")#[,c(1,2,3)]
IPC_names <- left_join(IPC_names, AI_RCA_1st, by="Subclass")

g_tech_AI <- mat_tech_rel_AI %>% as_tbl_graph(directed = FALSE) %N>%
  left_join(IPC_names %>% mutate(Subclass = Subclass %>% as.character()), by = c("name" = "Subclass")) %>%
  mutate(dgr = centrality_eigen(weights = weight)) %E>%
  filter(weight >= mean(weight))

#Create the Coordinates
coords_tech_AI <- g_tech_AI %>% igraph::layout.fruchterman.reingold() %>% as_tibble()
colnames(coords_tech_AI) <- c("x", "y")

#let's take a look at the most and less complex subclasses; this is also the only break check needed; sometimes three codes
#from chemistry get a 1 of dgr, and the rest is all 0s for some reason (it seems to be due to the random initialization of the network)
#if this happens, just run the code again from line 292 until here;
g_tech_AI %N>% 
  arrange(desc(dgr)) %>%
  as_tibble() %>%
  slice(1:10)

#and less complex ones:
g_tech_AI %N>% 
  arrange(dgr) %>%
  as_tibble() %>%
  slice(1:10)

AI_RCA <- read.csv("Files_created_with_the_code/data/files_code_4-digits_analysis/RCA_1st_AI.csv", sep = ";", header = TRUE, dec=",")
AI_RCA <- AI_RCA[,c(2,3)]
AI_RCA$Period <- "1st"
names(AI_RCA) <- c("Subclass", "RCA_AI_Period", "Period_sim")
AI_RCA$Binary <- ifelse(AI_RCA$RCA_AI_Period < 1, 0,1)

AI_RCA1 <- AI_RCA[is.na(AI_RCA$Binary)==F,]
AI_RCA1<-distinct(AI_RCA1, Subclass, .keep_all = TRUE)

AI_RCA_1st <-   g_tech_AI %N>%
  left_join(AI_RCA1, filter(Period_sim ==p), by = c("name" = "Subclass")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "#CCCCCC") +
  geom_node_point(aes(fill = sector, size = 1000^dgr, shape= sector)) +
  scale_shape_manual(values=c(21, 22, 23, 24, 25)) + labs(color   = "Degree")+ scale_size("Degree", range = c(2, 12)) +
  geom_node_text(aes(filter=Binary > .99, label = name), size = 5, repel = TRUE) + 
  theme_graph() +  ggtitle("AI-specific technological space: 4-digits IPC level (1974-1988)") #
  
jpeg("Files_created_with_the_code/figures/Extra/Appendix_A_AI_relatedness_and_special_subclasses_1stInterv.jpg", width = 14, height = 10, units = 'in', res = 300)
AI_RCA_1st 
dev.off()

rm(AI_RCA, AI_RCA1, coords_tech_AI, g_tech_AI, mat_tech_AI, mat_tech_rel_AI, patents_AI_specific_1st)

##2.2.Second period----
patents_AI_specific_2nd <- read.csv("other_files/IPCs_AI.csv", sep = ";", header = TRUE, dec=",")

a = 1988
b = 2004

patents_AI_specific_2nd <- patents_AI_specific_2nd[patents_AI_specific_2nd$priority_year < b,]
patents_AI_specific_2nd <- patents_AI_specific_2nd[patents_AI_specific_2nd$priority_year > a,]

#pick just the subclass for analysis:
patents_AI_specific_2nd$Subclass <- substr(patents_AI_specific_2nd$ipc_class_symbol,1,4)

length(unique(patents_AI_specific_2nd$appln_id)) #7887
length(unique(patents_AI_specific_2nd$Subclass)) #335
patents_AI_specific_2nd <- patents_AI_specific_2nd[is.na(patents_AI_specific_2nd$appln_id)==F,]

#drop lines that show repeated subclasses for the same patent
patents_AI_specific_2nd %<>% group_by(appln_id) %>% distinct(Subclass, .keep_all = TRUE)
length(unique(patents_AI_specific_2nd$appln_id)) #7887
length(unique(patents_AI_specific_2nd$Subclass)) #335

mat_tech_AI <- create_sparse_matrix(i = patents_AI_specific_2nd %>% pull(appln_id),
                                    j = patents_AI_specific_2nd %>% pull(Subclass))

mat_tech_AI %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_rel_AI <- mat_tech_AI %>% 
  relatedness(method = "cosine")

IPC_names <- IPC_names[,c((-6),(-7))]
AI_RCA_2nd <- read.csv("Files_created_with_the_code/data/files_code_4-digits_analysis/RCA_2nd_AI.csv", sep = ";", header = TRUE, dec=",")#[,c(1,2,3)]
IPC_names <- left_join(IPC_names, AI_RCA_2nd, by="Subclass")

g_tech_AI <- mat_tech_rel_AI %>% as_tbl_graph(directed = FALSE) %N>%
  left_join(IPC_names %>% mutate(Subclass = Subclass %>% as.character()), by = c("name" = "Subclass")) %>%
  mutate(dgr = centrality_eigen(weights = weight)) %E>%
  filter(weight >= mean(weight))

#Create the Coordinates
coords_tech_AI <- g_tech_AI %>% igraph::layout.fruchterman.reingold() %>% as_tibble()
colnames(coords_tech_AI) <- c("x", "y")

AI_RCA <- read.csv("Files_created_with_the_code/data/files_code_4-digits_analysis/RCA_2nd_AI.csv", sep = ";", header = TRUE, dec=",")
AI_RCA <- AI_RCA[,c(2,3)]
AI_RCA$Period <- "2nd"
names(AI_RCA) <- c("Subclass", "RCA_AI_Period", "Period_sim")
AI_RCA$Binary <- ifelse(AI_RCA$RCA_AI_Period < 1, 0,1)

AI_RCA1 <- AI_RCA[is.na(AI_RCA$Binary)==F,]
AI_RCA1<-distinct(AI_RCA1, Subclass, .keep_all = TRUE)

AI_RCA_2nd <- g_tech_AI %N>%
  left_join(AI_RCA1, filter(Period_sim ==p), by = c("name" = "Subclass")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "#CCCCCC") +
  geom_node_point(aes(fill = sector, size = 1000^dgr, shape= sector)) +
  scale_shape_manual(values=c(21, 22, 23, 24, 25)) + labs(color   = "Degree")+ scale_size("Degree", range = c(2, 12)) +
  geom_node_text(aes(filter=Binary > .99, label = name), size = 5, repel = TRUE) + #filter=Binary > .99, 
  theme_graph() +  ggtitle("AI-specific technological space: 4-digits IPC level (1989-2003)") #


jpeg("Files_created_with_the_code/figures/Extra/Appendix_A_AI_relatedness_and_special_subclasses_2ndInterv.jpg", width = 14, height = 10, units = 'in', res = 300)
AI_RCA_2nd 
dev.off()

rm(AI_RCA, AI_RCA1, coords_tech_AI, g_tech_AI, mat_tech_AI, mat_tech_rel_AI, patents_AI_specific_2nd)

##2.3.Third period----
patents_AI_specific_3rd <- read.csv("other_files/IPCs_AI.csv", sep = ";", header = TRUE, dec=",")

a = 2003
b = 2019

patents_AI_specific_3rd <- patents_AI_specific_3rd[patents_AI_specific_3rd$priority_year < b,]
patents_AI_specific_3rd <- patents_AI_specific_3rd[patents_AI_specific_3rd$priority_year > a,]

patents_AI_specific_3rd$Subclass <- substr(patents_AI_specific_3rd$ipc_class_symbol,1,4)

length(unique(patents_AI_specific_3rd$appln_id)) #34576
length(unique(patents_AI_specific_3rd$Subclass)) #417
patents_AI_specific_3rd <- patents_AI_specific_3rd[is.na(patents_AI_specific_3rd$appln_id)==F,]

#drop lines that show repeated subclasses for the same patent
patents_AI_specific_3rd %<>% group_by(appln_id) %>% distinct(Subclass, .keep_all = TRUE)
length(unique(patents_AI_specific_3rd$appln_id)) #34576
length(unique(patents_AI_specific_3rd$Subclass)) #417

mat_tech_AI <- create_sparse_matrix(i = patents_AI_specific_3rd %>% pull(appln_id),
                                    j = patents_AI_specific_3rd %>% pull(Subclass))

mat_tech_AI %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_rel_AI <- mat_tech_AI %>% 
  relatedness(method = "cosine")

IPC_names <- IPC_names[,c((-6),(-7))]
AI_RCA_3rd <- read.csv("Files_created_with_the_code/data/files_code_4-digits_analysis/RCA_3rd_AI.csv", sep = ";", header = TRUE, dec=",")#[,c(1,2,3)]
IPC_names <- left_join(IPC_names, AI_RCA_3rd, by="Subclass")

g_tech_AI <- mat_tech_rel_AI %>% as_tbl_graph(directed = FALSE) %N>%
  left_join(IPC_names %>% mutate(Subclass = Subclass %>% as.character()), by = c("name" = "Subclass"), keep=T) %>%
  mutate(dgr = centrality_eigen(weights = weight)) %E>%
  filter(weight >= mean(weight)) %E>%
  na.omit()

#Create the coordinates
coords_tech_AI <- g_tech_AI %>% igraph::layout.fruchterman.reingold() %>% as_tibble()
colnames(coords_tech_AI) <- c("x", "y")

AI_RCA <- read.csv("Files_created_with_the_code/data/files_code_4-digits_analysis/RCA_3rd_AI.csv", sep = ";", header = TRUE, dec=",")
AI_RCA <- AI_RCA[,c(2,3)]
AI_RCA$Period <- "3rd"
names(AI_RCA) <- c("Subclass", "RCA_AI_Period", "Period_sim")
AI_RCA$Binary <- ifelse(AI_RCA$RCA_AI_Period < 1, 0,1)

AI_RCA1 <- AI_RCA[is.na(AI_RCA$Binary)==F,]
AI_RCA1<-distinct(AI_RCA1, Subclass, .keep_all = TRUE)

AI_RCA_3rd <- 
  g_tech_AI %N>%
  left_join(AI_RCA1, filter(Period_sim ==p), by = c("name" = "Subclass")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "#CCCCCC") +
  geom_node_point(aes(fill = sector, size = 1000^dgr, shape= sector)) +
  scale_shape_manual(values=c(21, 22, 23, 24, 25)) + labs(color   = "Degree")+ scale_size("Degree", range = c(2, 12)) +
  geom_node_text(aes(filter=Binary > .99, label = name), size = 5, repel = TRUE) + #filter=Binary > .99, 
  theme_graph() +
  ggtitle("AI-specific technological space: 4-digits IPC level (2004-2018)") #

jpeg("Files_created_with_the_code/figures/Extra/Appendix_A_AI_relatedness_and_special_subclasses_3rdInterv.jpg", width = 14, height = 10, units = 'in', res = 300)
AI_RCA_3rd 
dev.off()

#create one figure for the three AI subclass-based technological spaces:
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

jpeg("Files_created_with_the_code/figures/Appendix_A_AI_relatedness_and_special_subclasses_All_Interv.jpg", width = 16, height = 38, units = 'in', res = 300)
multiplot(AI_RCA_1st, AI_RCA_2nd, AI_RCA_3rd, cols=1) 
dev.off()
