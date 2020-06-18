library(tidyverse) # Collection of all the good stuff like dplyr, ggplot2 ect.
library(magrittr) # For extra-piping operators (eg. %<>%)

# Network specific
library(tidygraph) # For tidy-style graph manipulation
library(ggraph) # For ggplot2 style graph plotting

library(devtools)
library(EconGeo) # Economic Geography functions

library(ggplot2)
library("data.table") #for reading the big files using fread

#1.NACE data -----
#1.1. First part -----
rm(list=ls())
#setwd("C:/Users/mathe/OneDrive/?rea de Trabalho")
setwd("C:/Users/Matheus/Desktop") #for loading the big file

#Now we will load a big file containing all priorities and their related Nace codes published in or after 2004.
Nace_all_patents <- fread("all data nace post 2004.csv", header = F)
names(Nace_all_patents) <- c("appln_id", "ctry_code", "nace2_code", "weight")

#I'll copy the country codes to another columns, so I don't loose this information when I replace the country codes by the code "AI_pat" (which
#I'm doing to identify the AI domain)
Nace_all_patents$ctry_code2 <- Nace_all_patents$ctry_code
#set the working directory to the folder where we opened this code:
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Now we load the data containing all nace codes used in our dataset of AI patents:
patents_AI_specific <- read.csv("Data/Nace2_AI csv.csv", sep = ";", header = TRUE, dec=",")

#Now, I'll divide this data in 3 periods, so we can see the evolution of AI over time.
#Starting by the 3rd period, from 2003 to 2018:
patents_AI_specific <- patents_AI_specific[patents_AI_specific$priority_year > 2003,]
patents_AI_specific$ctry_code <- as.vector(patents_AI_specific$ctry_code)
#library(stringr)
patents_AI_specific$ctry_code <- "AI_pat"
#I want to select some patents on these Nace_all_patents dataset and change the patent_office to, let's say, AI;
library(data.table)
setDT(patents_AI_specific)
setDT(Nace_all_patents)
Nace_all_patents[patents_AI_specific, on = c("appln_id"), ctry_code := i.ctry_code]

setwd("C:/Users/mathe/Google Drive/PhD/1.Paper 1 - Patent Analysis/0.Tudo escrito mais EMAEE e Workshop/GPT Nova vers?o P?s EMAEE/0.Novo draft/New Analysis Inventors Data/nace2 analysis")
nace2_names <- read.csv("Data/tls902_ipc_nace2.csv", sep = ";", header = TRUE)%>%
  select(nace2_code, nace2_descr) %>%
  distinct(nace2_code, .keep_all = TRUE) %>%
  mutate(nace2_code = nace2_code) %>%
  arrange(nace2_code)

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


mat_tech_AI <- create_sparse_matrix(i = Nace_all_patents %>% pull(appln_id),
                                    j = Nace_all_patents %>% pull(nace2_code))

mat_tech_AI %<>% 
  crossprod() %>% 
  as.matrix() 

mat_tech_rel_AI <- mat_tech_AI %>% 
  relatedness(method = "cosine")


library(tidygraph)

g_tech_AI <- mat_tech_rel_AI %>% as_tbl_graph(directed = FALSE) %N>%
  left_join(nace2_names %>% mutate(nace2_code = nace2_code %>% as.character()), by = c("name" = "nace2_code")) %>%
  mutate(dgr = centrality_eigen(weights = weight)) %E>%
  filter(weight >= mean(weight))

coords_tech_AI <- g_tech_AI %>% igraph::layout.fruchterman.reingold() %>% as_tibble()
colnames(coords_tech_AI) <- c("x", "y")

g_tech_AI %N>% 
  arrange(desc(dgr)) %>%
  as_tibble() %>%
  slice(1:10)

g_tech_AI %N>% 
  arrange(dgr) %>%
  as_tibble() %>%
  slice(1:10)

#1.2.Nace By Country AI----
countries_geo <- Nace_all_patents %>%
  distinct(ctry_code, .keep_all = TRUE) %>%
  select(ctry_code)

reg_tech <- Nace_all_patents %>%
  group_by(appln_id) %>%
  mutate(field_weight = 1 / n()) %>%
  ungroup()

reg_tech %<>%
  group_by(ctry_code, nace2_code) %>%
  summarise(n_tech_reg = sum(field_weight)) %>%
  ungroup() %>%
  drop_na() 

#library(tidyr)
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

#1.3.Nace Visualization -----
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

#1.4.Nace By Country countries----
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