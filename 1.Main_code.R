#### Main Code
#This is the first code to be executed. It generates the main data and figures used in the analysis.
# Load required libraries
library(tidyverse)
library(magrittr)
library(tidygraph)
library(ggraph)
library(EconGeo)
library(data.table)
library(netrankr)
library(dplyr)
library(tidyr)
library(ggrepel)
library(scales)
library(patchwork)
library(RColorBrewer)
library(janitor)
library(ggforce)
library(stringr)
library(openxlsx)
library(stargazer) #for generating nice econometric tables

rm(list=ls())
#set the working directory to where you saved the R code:
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#create important folders
# Helper function to create the folders only if they don't exist yet
safe_dir_create <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
}

# Create all needed folders
safe_dir_create("Files_created_with_the_code/data")
safe_dir_create("Files_created_with_the_code/data/files_code_4-digits_analysis")
safe_dir_create("Files_created_with_the_code/data/files_code_4-digits_analysis/robustness")
safe_dir_create("Files_created_with_the_code/data/files_code_Fields_analysis")
safe_dir_create("Files_created_with_the_code/data/files_code_Fields_analysis/robustness")

safe_dir_create("Files_created_with_the_code/figures")
safe_dir_create("Files_created_with_the_code/figures/robustness")
safe_dir_create("Files_created_with_the_code/figures/extra")

#1.FIRST PART: Technological Spaces based on Technological field -----
#Create important functions 
# This function groups data by application ID, and within each group, it calculates
# a field-specific weight equal to 1 divided by the number of records in that group.
group_by_applnID <- function(data){
  data %>%
    group_by(appln_id) %>%
    mutate(field_weight = 1 / n()) %>%
    ungroup()
}

# This function aggregates the weighted fields at the country-technology field level.
group_by_ctry_and_techn_field <- function(data){
  data %<>%
    group_by(ctry_code, techn_field_nr) %>%
    summarise(n_tech_reg = sum(field_weight)) %>%
    ungroup() %>%
    drop_na() 
}

# This function aggregates the weighted fields at the country-subclass level.
group_by_ctry_and_subclass <- function(data){
  data %<>%
    group_by(ctry_code, Subclass) %>%
    summarise(n_tech_reg = sum(field_weight)) %>%
    ungroup() %>%
    drop_na() 
}

# This function arranges multiple ggplots into one figure.
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Combine all plots into a single list
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  
  # If no layout is provided, define one based on the specified number of columns
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  # If only one plot, just print it
  if (numPlots==1) {
    print(plots[[1]])
  } else {
    # Set up a new page for the layout
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Print each plot in the correct layout position
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# This function creates a sparse matrix from the given inputs.
# i.input and j.input represent row and column indices, respectively.
create_sparse_matrix <- function(i.input, j.input){
  require(Matrix)
  mat <- spMatrix(
    nrow = i.input %>% n_distinct(),
    ncol = j.input %>% n_distinct(),
    i    = i.input %>% factor() %>% as.numeric(),
    j    = j.input %>% factor() %>% as.numeric(),
    x    = rep(1, i.input %>% length())
  )
  
  row.names(mat) <- i.input %>% factor() %>% levels()
  colnames(mat)  <- j.input %>% factor() %>% levels()
  return(mat)
}

##1.1. Calculate Specializations for Different Time intervals -----
###1.1.1 Read and prepare big files
# Reading large files in parts to avoid memory issues
rows_part2 <- 45182803 - 40000000
ipc_all_patents_part2_chunk1_df <- fread("large_files/All_patents_and_IPCs_Part2.csv", 
                                         header = FALSE, nrow = 20000000)
ipc_all_patents_part2_chunk2_df <- fread("large_files/All_patents_and_IPCs_Part2.csv", 
                                         header = FALSE, nrow = 20000000, skip = 20000000)
ipc_all_patents_part2_chunk3_df <- fread("large_files/All_patents_and_IPCs_Part2.csv", 
                                         header = FALSE, nrow = rows_part2, skip = 40000000)

ipc_all_patents_part2_df <- rbind(ipc_all_patents_part2_chunk1_df, 
                                  ipc_all_patents_part2_chunk2_df, ipc_all_patents_part2_chunk3_df)

rm(ipc_all_patents_part2_chunk1_df, ipc_all_patents_part2_chunk2_df, ipc_all_patents_part2_chunk3_df)

setnames(ipc_all_patents_part2_df, c("appln_id", "ctry_code", "techn_field_nr", "weight", "priority_year"))

# Load AI-specific data
ai_patents_df <- fread("other_files/IPCs_AI.csv", sep=";", dec=",", header=TRUE)
ai_patents_df$ctry_code <- "AI_pat"

# Load IPC technology names for labeling
ipc_names_df <- read.csv("other_files/ipc_technology.csv", sep = ";", header = TRUE) %>%
  select(field_nr, sector, field_name) %>% distinct(field_nr, .keep_all = TRUE) %>%
  mutate(techn_field_nr = field_nr) %>% arrange(techn_field_nr)

###1.1.2.Calculate per interval
####1.1.2.1. Interval 1 (1974 - 1988)
# Define Time Interval
start_year <- 1973
end_year   <- 1989

###Filter Data for Interval 1 
ipc_all_patents_first_period_df <- ipc_all_patents_part2_df[priority_year > start_year & priority_year < end_year]
ipc_all_patents_first_period_df <- ipc_all_patents_first_period_df[, .(appln_id, ctry_code, techn_field_nr)]

# Calculate General RCA for Interval 1
# Compute weighted values at country-technology field level
region_tech_fields_1_df <- group_by_applnID(ipc_all_patents_first_period_df)
region_tech_fields_1_df <- group_by_ctry_and_techn_field(region_tech_fields_1_df)
write.csv2(region_tech_fields_1_df, 
           file = "Files_created_with_the_code/data/files_code_Fields_analysis/reg_tech_FirstPeriod.csv", 
           row.names = FALSE)

# Create a matrix from the aggregated data
mat_reg_tech1 <- region_tech_fields_1_df %>% arrange(techn_field_nr, ctry_code) %>%
  pivot_wider(names_from = techn_field_nr, values_from = n_tech_reg, values_fill = 0)
mat_reg_tech1 %<>% remove_rownames %>% column_to_rownames(var="ctry_code") %>%
  as.matrix() %>% round()

# Calculate RCA (relative comparative advantage) for general technologies
reg_RCA1_df <- mat_reg_tech1 %>% location_quotient(binary = FALSE) %>% as.data.frame() %>% 
  rownames_to_column("ctry_code") %>% as_tibble() %>% gather("techn_field_nr", "RCA", -ctry_code)

# Calculate AI-Specific RCA for Interval 1
ai_patents_period_1_df <- ai_patents_df[ai_patents_df$priority_year > start_year & ai_patents_df$priority_year < end_year,]

# Join technological fields info to AI patents
ai_patents_period_1_df <- distinct(ai_patents_period_1_df, appln_id, .keep_all = TRUE)[, c(1,3)]
ai_patents_period_1_df <- left_join(ai_patents_period_1_df, ipc_all_patents_first_period_df, by = "appln_id")

# Compute weighted fields for AI patents
region_tech_fields_1_ai_df <- group_by_applnID(ai_patents_period_1_df)
rm(ai_patents_period_1_df)
region_tech_fields_1_ai_df <- group_by_ctry_and_techn_field(region_tech_fields_1_ai_df)

mat_reg_tech1_AI <- region_tech_fields_1_ai_df %>%
  arrange(techn_field_nr, ctry_code) %>%
  pivot_wider(names_from = techn_field_nr, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

mat_reg_tech1_AI %<>% remove_rownames %>% column_to_rownames(var="ctry_code") %>% as.matrix() %>%  round()

reg_RCA1_AI_df <- mat_reg_tech1_AI %>% location_quotient(binary = F) %>% 
  as.data.frame() %>% rownames_to_column("ctry_code") %>%   
  as_tibble() %>%   gather(key = "techn_field_nr", value = "RCA", -ctry_code) %>%  arrange(ctry_code, techn_field_nr)

#mat_reg_tech1_AI has the numbers I need for AI; mat_reg_tech1 has the numbers for general;
#test1 <- mat_reg_tech1[,c("1", "2", "3")]

#library(tibble)
General_raw <- rownames_to_column(as.data.frame(mat_reg_tech1), var = "country_code")
General_raw$info <- "general"
AI_raw <- rownames_to_column(as.data.frame(mat_reg_tech1_AI), var = "country_code")
AI_raw$info <- "AI"
Raw_Info <- full_join(General_raw, AI_raw)
write.csv2(Raw_Info, 
           file = "Files_created_with_the_code/data/files_code_Fields_analysis/Data1period_Raw_Info.csv", 
           row.names = FALSE)
rm(General_raw, AI_raw)

# Merge general and AI-specific RCA data for Interval 1
rca_data_period_1_df <- merge(reg_RCA1_df, reg_RCA1_AI_df, all = TRUE, by = c("ctry_code", "techn_field_nr"))
rca_data_period_1_df$Period <- "1974-1988"
names(rca_data_period_1_df) <- c("ctry_code", "techn_field_nr", "RCA_Gen", "RCA_AI", "Period")

#test <- rca_data_period_1_df[rca_data_period_1_df$ctry_code == "JP",]

write.csv2(rca_data_period_1_df, 
           file = "Files_created_with_the_code/data/files_code_Fields_analysis/Data1period_RCA_techn_field.csv", 
           row.names = FALSE)

# Regional Tech for AI - Interval 1
setDT(ai_patents_df)
setDT(ipc_all_patents_first_period_df)
ipc_all_patents_first_period_df[ai_patents_df, on = "appln_id", ctry_code := i.ctry_code]
region_tech_ai_1_df <- group_by_applnID(ipc_all_patents_first_period_df)
region_tech_ai_1_df <- group_by_ctry_and_techn_field(region_tech_ai_1_df) #this part has the country information on number of
#patents per field per country (first term); the previous region_tech_fields_1_ai_df has the number of AI patents per field,
#whereas mat_reg_tech1 refers to the general

write.csv2(region_tech_ai_1_df, 
           file = "Files_created_with_the_code/data/files_code_Fields_analysis/reg_techAI_FirstPeriod.csv", 
           row.names = FALSE)
rm(ipc_all_patents_first_period_df, mat_reg_tech1, reg_RCA1_df, reg_RCA1_AI_df)

####1.1.2.2. Interval 2 (1989 - 2003)
# Define Time Interval 
start_year <- 1988
end_year   <- 2004

# Filter Data for Interval 2 
ipc_all_patents_second_period_df <- ipc_all_patents_part2_df[priority_year > start_year & priority_year < end_year]
ipc_all_patents_second_period_df <- ipc_all_patents_second_period_df[, .(appln_id, ctry_code, techn_field_nr)]

# Calculate General RCA for Interval 2
region_tech_fields_2_df <- group_by_applnID(ipc_all_patents_second_period_df)
region_tech_fields_2_df <- group_by_ctry_and_techn_field(region_tech_fields_2_df)

write.csv2(region_tech_fields_2_df, 
           file = "Files_created_with_the_code/data/files_code_Fields_analysis/reg_tech_SecondPeriod.csv", 
           row.names = FALSE)

mat_reg_tech2 <- region_tech_fields_2_df %>% arrange(techn_field_nr, ctry_code) %>%
  pivot_wider(names_from = techn_field_nr, values_from = n_tech_reg, values_fill = 0)

mat_reg_tech2 %<>% remove_rownames %>% column_to_rownames(var="ctry_code") %>%
  as.matrix() %>% round()

reg_RCA2_df <- mat_reg_tech2 %>% location_quotient(binary = FALSE) %>% 
  as.data.frame() %>% rownames_to_column("ctry_code") %>% as_tibble() %>%
  gather("techn_field_nr","RCA",-ctry_code)

# Calculate AI-Specific RCA for Interval 2
ai_patents_period_2_df <- ai_patents_df[ai_patents_df$priority_year > start_year & ai_patents_df$priority_year < end_year,]
ai_patents_period_2_df <- distinct(ai_patents_period_2_df, appln_id, .keep_all = TRUE)[, c(1,3)]
ai_patents_period_2_df <- left_join(ai_patents_period_2_df, ipc_all_patents_second_period_df, by = "appln_id")

region_tech_fields_2_ai_df <- group_by_applnID(ai_patents_period_2_df)
rm(ai_patents_period_2_df)
region_tech_fields_2_ai_df <- group_by_ctry_and_techn_field(region_tech_fields_2_ai_df)

mat_reg_tech2_AI <- region_tech_fields_2_ai_df %>% arrange(techn_field_nr, ctry_code) %>%
  pivot_wider(names_from = techn_field_nr, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

mat_reg_tech2_AI %<>% remove_rownames %>% column_to_rownames(var="ctry_code") %>%
  as.matrix() %>% round()

reg_RCA2_AI_df <- mat_reg_tech2_AI %>% location_quotient(binary = FALSE) %>% 
  as.data.frame() %>% rownames_to_column("ctry_code") %>% as_tibble() %>%   
  gather(key = "techn_field_nr", value = "RCA", -ctry_code) %>%   arrange(ctry_code, techn_field_nr)

General_raw <- rownames_to_column(as.data.frame(mat_reg_tech2), var = "country_code")
General_raw$info <- "general"
AI_raw <- rownames_to_column(as.data.frame(mat_reg_tech2_AI), var = "country_code")
AI_raw$info <- "AI"
Raw_Info <- full_join(General_raw, AI_raw)
write.csv2(Raw_Info, 
           file = "Files_created_with_the_code/data/files_code_Fields_analysis/Data2period_Raw_Info.csv", 
           row.names = FALSE)
rm(General_raw, AI_raw)

# Merge general and AI-specific RCA data for Interval 2
rca_data_period_2_df <- merge(reg_RCA2_df, reg_RCA2_AI_df, all = TRUE, by = c("ctry_code","techn_field_nr"))
rca_data_period_2_df$Period <- "1989-2003"
names(rca_data_period_2_df) <- c("ctry_code","techn_field_nr","RCA_Gen","RCA_AI","Period")

write.csv2(rca_data_period_2_df, 
           file = "Files_created_with_the_code/data/files_code_Fields_analysis/Data2period_RCA_techn_field.csv", 
           row.names = FALSE)

# Regional Tech for AI - Interval 2 
ipc_all_patents_second_period_df[ai_patents_df, on = "appln_id", ctry_code := i.ctry_code]
region_tech_ai_2_df <- group_by_applnID(ipc_all_patents_second_period_df)
region_tech_ai_2_df <- group_by_ctry_and_techn_field(region_tech_ai_2_df)

write.csv2(region_tech_ai_2_df, 
           file = "Files_created_with_the_code/data/files_code_Fields_analysis/reg_techAI_SecondPeriod.csv", 
           row.names = FALSE)
rm(ipc_all_patents_second_period_df, mat_reg_tech2, reg_RCA2_df, reg_RCA2_AI_df)

####1.1.2.3. Interval 3 (2004 - 2018)
# Load Additional Data for Interval 3
rows_part1 <- 58841893 - 40000000
ipc_all_patents_part1_chunk1_df <- fread("large_files/All_patents_and_IPCs_Part1.csv", 
                                         header = FALSE, nrow = 20000000)
ipc_all_patents_part1_chunk2_df <- fread("large_files/All_patents_and_IPCs_Part1.csv", 
                                         header = FALSE, nrow = 20000000, skip = 20000000)
ipc_all_patents_part1_chunk3_df <- fread("large_files/All_patents_and_IPCs_Part1.csv", 
                                         header = FALSE, nrow = rows_part1, skip = 40000000)

ipc_all_patents_part1_df <- rbind(ipc_all_patents_part1_chunk1_df, ipc_all_patents_part1_chunk2_df, 
                                  ipc_all_patents_part1_chunk3_df)

rm(ipc_all_patents_part1_chunk1_df, ipc_all_patents_part1_chunk2_df, ipc_all_patents_part1_chunk3_df)
setnames(ipc_all_patents_part1_df, c("appln_id", "ctry_code", "techn_field_nr", "weight", "priority_year"))

# Filter Data for Interval 3 (2004-2018) 
# This dataset only includes patents from 2004 to 2018, so no filter needed
ipc_all_patents_third_period_df <- ipc_all_patents_part1_df[, .(appln_id, ctry_code, techn_field_nr)]

# Calculate General RCA for Interval 3 
# Split into chunks for memory efficiency
part1_chunk_df <- ipc_all_patents_third_period_df[1:20000000]
part2_chunk_df <- ipc_all_patents_third_period_df[20000001:40000000]
part3_chunk_df <- ipc_all_patents_third_period_df[40000001:.N]

reg_tech_3_part1_df <- group_by_ctry_and_techn_field(group_by_applnID(part1_chunk_df))
reg_tech_3_part2_df <- group_by_ctry_and_techn_field(group_by_applnID(part2_chunk_df))
reg_tech_3_part3_df <- group_by_ctry_and_techn_field(group_by_applnID(part3_chunk_df))

# Merge the three parts for the full Interval 3 dataset
reg_tech_3_merged_df <- merge(reg_tech_3_part1_df, reg_tech_3_part2_df, all=TRUE, by=c("ctry_code","techn_field_nr"))
reg_tech_3_merged_df <- merge(reg_tech_3_merged_df, reg_tech_3_part3_df, all=TRUE, by=c("ctry_code","techn_field_nr"))

# Replace NA with 0 and sum the three parts
reg_tech_3_merged_df[is.na(reg_tech_3_merged_df)] <- 0
reg_tech_3_merged_df$sum <- rowSums(reg_tech_3_merged_df[, c(3:5)])

reg_tech_3_df <- reg_tech_3_merged_df[, c("ctry_code","techn_field_nr","sum")]
names(reg_tech_3_df) <- c("ctry_code","techn_field_nr","n_tech_reg")

write.csv2(reg_tech_3_df, 
           file = "Files_created_with_the_code/data/files_code_Fields_analysis/reg_tech_ThirdPeriod.csv", 
           row.names = FALSE)

# Create matrix and compute RCA
mat_reg_tech3 <- reg_tech_3_df %>% arrange(techn_field_nr, ctry_code) %>%
  pivot_wider(names_from=techn_field_nr, values_from=n_tech_reg, values_fill=0)

mat_reg_tech3 %<>% remove_rownames %>% column_to_rownames(var="ctry_code") %>%
  as.matrix() %>% round()

reg_RCA3_df <- mat_reg_tech3 %>% location_quotient(binary = FALSE) %>% as.data.frame() %>% 
  rownames_to_column("ctry_code") %>% as_tibble() %>% gather("techn_field_nr","RCA",-ctry_code)

# Calculate AI-Specific RCA for Interval 3 
start_year <- 2003
end_year   <- 2019
ai_patents_period_3_df <- ai_patents_df[ai_patents_df$priority_year > start_year & ai_patents_df$priority_year < end_year,]

ai_patents_period_3_df <- distinct(ai_patents_period_3_df, appln_id, .keep_all = TRUE)[, c(1,3)]
ai_patents_period_3_df <- left_join(ai_patents_period_3_df, ipc_all_patents_third_period_df, by = "appln_id")

region_tech_fields_3_ai_df <- group_by_applnID(ai_patents_period_3_df)
rm(ai_patents_period_3_df)
region_tech_fields_3_ai_df <- group_by_ctry_and_techn_field(region_tech_fields_3_ai_df)

mat_reg_tech3_AI <- region_tech_fields_3_ai_df %>%arrange(techn_field_nr, ctry_code) %>%
  pivot_wider(names_from = techn_field_nr, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

mat_reg_tech3_AI %<>% remove_rownames %>% column_to_rownames(var="ctry_code") %>%
  as.matrix() %>%  round()

reg_RCA3_AI_df <- mat_reg_tech3_AI %>% location_quotient(binary = FALSE) %>% 
  as.data.frame() %>%   rownames_to_column("ctry_code") %>%   
  as_tibble() %>%   gather(key = "techn_field_nr", value = "RCA", -ctry_code) %>%  arrange(ctry_code, techn_field_nr)

General_raw <- rownames_to_column(as.data.frame(mat_reg_tech3), var = "country_code")
General_raw$info <- "general"
AI_raw <- rownames_to_column(as.data.frame(mat_reg_tech3_AI), var = "country_code")
AI_raw$info <- "AI"
Raw_Info <- full_join(General_raw, AI_raw)
write.csv2(Raw_Info, 
           file = "Files_created_with_the_code/data/files_code_Fields_analysis/Data3period_Raw_Info.csv", 
           row.names = FALSE)
rm(General_raw, AI_raw)

# Merge general and AI-specific RCA data for Interval 3
rca_data_period_3_df <- merge(reg_RCA3_df, reg_RCA3_AI_df, all=TRUE, by=c("ctry_code","techn_field_nr"))
rca_data_period_3_df$Period <- "2004-2018"
names(rca_data_period_3_df) <- c("ctry_code","techn_field_nr","RCA_Gen","RCA_AI","Period")

write.csv2(rca_data_period_3_df, 
           file = "Files_created_with_the_code/data/files_code_Fields_analysis/Data3period_RCA_techn_field.csv", 
           row.names = FALSE)

# Regional Tech for AI - Interval 3 
ipc_all_patents_third_period_df[ai_patents_df, on = "appln_id", ctry_code := i.ctry_code]

AI_part1_chunk_df <- ipc_all_patents_third_period_df[1:20000000]
AI_part2_chunk_df <- ipc_all_patents_third_period_df[20000001:40000000]
AI_part3_chunk_df <- ipc_all_patents_third_period_df[40000001:.N]

reg_tech_AI_3_part1_df <- group_by_ctry_and_techn_field(group_by_applnID(AI_part1_chunk_df))
reg_tech_AI_3_part2_df <- group_by_ctry_and_techn_field(group_by_applnID(AI_part2_chunk_df))
reg_tech_AI_3_part3_df <- group_by_ctry_and_techn_field(group_by_applnID(AI_part3_chunk_df))

tabledata_AI2_df <- merge(reg_tech_AI_3_part1_df, reg_tech_AI_3_part2_df, all=TRUE, by=c("ctry_code","techn_field_nr"))
tabledata_AI2_df <- merge(tabledata_AI2_df, reg_tech_AI_3_part3_df, all=TRUE, by=c("ctry_code","techn_field_nr"))
tabledata_AI2_df[is.na(tabledata_AI2_df)] <- 0
tabledata_AI2_df$sum <- rowSums(tabledata_AI2_df[, c(3:5)])
tabledata_AI2_df <- tabledata_AI2_df[, c("ctry_code","techn_field_nr","sum")]
names(tabledata_AI2_df) <- c("ctry_code","techn_field_nr","n_tech_reg")

write.csv2(tabledata_AI2_df, 
           file = "Files_created_with_the_code/data/files_code_Fields_analysis/reg_techAI_ThirdPeriod.csv", 
           row.names = FALSE)

# Clean up large objects
rm(mat_reg_tech3_AI, reg_RCA3_df, reg_RCA3_AI_df, AI_part1_chunk_df, AI_part2_chunk_df, AI_part3_chunk_df, 
   ipc_all_patents_part1_df, ipc_all_patents_part2_df, ipc_all_patents_third_period_df, 
   part1_chunk_df, part2_chunk_df, part3_chunk_df, ai_patents_df)

# Combine Data for All Intervals
ipc_rcas_df <- rbind(rca_data_period_1_df, rca_data_period_2_df, rca_data_period_3_df)
write.csv2(ipc_rcas_df, 
           file = "Files_created_with_the_code/data/files_code_Fields_analysis/IPC_RCAs.csv", 
           row.names = FALSE)
# Remove all non-function objects in the global environment and clean memory
rm(list = ls()[!sapply(ls(), function(x) is.function(get(x)))])
gc()

###1.1.2.4.  Create a Specializations Summary
# First Interval Countries 
# Load first interval data for countries
reg_tech1_countries <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/reg_tech_FirstPeriod.csv", 
                                sep = ";", header = TRUE, dec=",")

# Create a wide matrix of technology fields for the first interval
mat_reg_tech1_countries <- reg_tech1_countries %>%  arrange(techn_field_nr, ctry_code) %>%
  pivot_wider(names_from = techn_field_nr, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

mat_reg_tech1_countries %<>%   remove_rownames %>% 
  column_to_rownames(var="ctry_code") %>%  as.matrix() %>%    round()

# Compute RCA for the first interval (general)
reg_RCA1_countries <- mat_reg_tech1_countries %>%   location_quotient(binary = FALSE) %>% 
  as.data.frame() %>% rownames_to_column("ctry_code") %>% 
  as_tibble() %>% gather(key = "techn_field_nr", value = "RCA", -ctry_code) %>% arrange(ctry_code, techn_field_nr)

# First Interval AI 
# Load first interval data for AI-specific patents
reg_tech1_AI <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/reg_techAI_FirstPeriod.csv", 
                         sep = ";", header = TRUE, dec=",")

# Create a wide matrix of technology fields for AI patents in the first interval
mat_reg_tech1_AI <- reg_tech1_AI %>%  arrange(techn_field_nr, ctry_code) %>%
  pivot_wider(names_from = techn_field_nr, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

mat_reg_tech1_AI %<>%   remove_rownames %>%   column_to_rownames(var="ctry_code") %>%
  as.matrix() %>%  round()

# Compute RCA for the first interval (AI-specific)
reg_RCA1_AI <- mat_reg_tech1_AI %>%   location_quotient(binary = FALSE) %>% 
  as.data.frame() %>%   rownames_to_column("ctry_code") %>% 
  as_tibble() %>%   gather(key = "techn_field_nr", value = "RCA", -ctry_code) %>%
  arrange(ctry_code, techn_field_nr)

# Load IPC names and metadata
IPC_names <- read.csv("other_files/ipc_technology.csv", sep = ";", header = TRUE)%>%
  select(field_nr, sector, field_name) %>%  distinct(field_nr, .keep_all = TRUE) %>%
  mutate(techn_field_nr = field_nr) %>%  arrange(techn_field_nr)
IPC_names <- IPC_names[, -1]

# Extract top countries and AI data for first interval
US_first_period <- reg_RCA1_countries[,2:3][reg_RCA1_countries$ctry_code == "US",]
CN_first_period <- reg_RCA1_countries[,2:3][reg_RCA1_countries$ctry_code == "CN",]
KR_first_period <- reg_RCA1_countries[,2:3][reg_RCA1_countries$ctry_code == "KR",]
JP_first_period <- reg_RCA1_countries[,2:3][reg_RCA1_countries$ctry_code == "JP",]
AI_first_period <- reg_RCA1_AI[,2:3][reg_RCA1_AI$ctry_code == "AI_pat",]

# Merge IPC names with the countries and AI RCAs for the first interval
First_period <- merge(merge(merge(merge(merge(IPC_names, US_first_period), CN_first_period, by = "techn_field_nr"), 
  KR_first_period, by = "techn_field_nr"), JP_first_period, by = "techn_field_nr"), 
  AI_first_period, by = "techn_field_nr")

names(First_period) <- c("techn_field_nr", "sector", "field_name", "RCA_US", "RCA_CN","RCA_KR","RCA_JP", "RCA_AI")

write.csv2(First_period, 
           file = "Files_created_with_the_code/data/files_code_Fields_analysis/Metrics_First_period.csv", 
           row.names = FALSE)

# Second Interval Countries 
# Load second interval data for countries
reg_tech2_countries <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/reg_tech_SecondPeriod.csv", 
                                sep = ";", header = TRUE, dec=",")

mat_reg_tech2_countries <- reg_tech2_countries %>%  arrange(techn_field_nr, ctry_code) %>%
  pivot_wider(names_from = techn_field_nr, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

mat_reg_tech2_countries %<>%   remove_rownames %>% 
  column_to_rownames(var="ctry_code") %>%  as.matrix() %>%    round()

# Compute RCA for the second interval (general)
reg_RCA2_countries <- mat_reg_tech2_countries %>%   location_quotient(binary = FALSE) %>% 
  as.data.frame() %>%   rownames_to_column("ctry_code") %>%   as_tibble() %>% 
  gather(key = "techn_field_nr", value = "RCA", -ctry_code) %>%  arrange(ctry_code, techn_field_nr)

# Second Interval AI 
# Load second interval data for AI-specific patents
reg_tech2_AI <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/reg_techAI_SecondPeriod.csv", 
                         sep = ";", header = TRUE, dec=",")

mat_reg_tech2_AI <- reg_tech2_AI %>%  arrange(techn_field_nr, ctry_code) %>%
  pivot_wider(names_from = techn_field_nr, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

mat_reg_tech2_AI %<>%   remove_rownames %>%   column_to_rownames(var="ctry_code") %>%
  as.matrix() %>%    round()

# Compute RCA for the second interval (AI-specific)
reg_RCA2_AI <- mat_reg_tech2_AI %>%   location_quotient(binary = FALSE) %>% 
  as.data.frame() %>%   rownames_to_column("ctry_code") %>%   as_tibble() %>% 
  gather(key = "techn_field_nr", value = "RCA", -ctry_code) %>%  arrange(ctry_code, techn_field_nr)

# Extract top countries and AI data for second interval
US_Second_period <- reg_RCA2_countries[,2:3][reg_RCA2_countries$ctry_code == "US",]
CN_Second_period <- reg_RCA2_countries[,2:3][reg_RCA2_countries$ctry_code == "CN",]
KR_Second_period <- reg_RCA2_countries[,2:3][reg_RCA2_countries$ctry_code == "KR",]
JP_Second_period <- reg_RCA2_countries[,2:3][reg_RCA2_countries$ctry_code == "JP",]
AI_Second_period <- reg_RCA2_AI[,2:3][reg_RCA2_AI$ctry_code == "AI_pat",]

# Merge IPC names with the countries and AI RCAs for the second interval
Second_period <- merge(merge(merge(merge(merge(IPC_names, US_Second_period), CN_Second_period, by = "techn_field_nr"), 
  KR_Second_period, by = "techn_field_nr"), JP_Second_period, by = "techn_field_nr"), 
  AI_Second_period, by = "techn_field_nr")

names(Second_period) <- c("techn_field_nr", "sector", "field_name", "RCA_US", "RCA_CN","RCA_KR","RCA_JP", "RCA_AI")

write.csv2(Second_period, 
           file = "Files_created_with_the_code/data/files_code_Fields_analysis/Metrics_Second_period.csv", 
           row.names = FALSE)

# Third Interval Countries  
reg_tech3_countries <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/reg_tech_ThirdPeriod.csv", 
                                sep = ";", header = TRUE, dec=",")

mat_reg_tech3_countries <- reg_tech3_countries %>%  arrange(techn_field_nr, ctry_code) %>%
  pivot_wider(names_from = techn_field_nr, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

mat_reg_tech3_countries %<>%   remove_rownames %>%   column_to_rownames(var="ctry_code") %>%
  as.matrix() %>%    round()

# Compute RCA for the third interval (general)
reg_RCA3_countries <- mat_reg_tech3_countries %>%   location_quotient(binary = FALSE) %>% 
  as.data.frame() %>%   rownames_to_column("ctry_code") %>%   as_tibble() %>% 
  gather(key = "techn_field_nr", value = "RCA", -ctry_code) %>%  arrange(ctry_code, techn_field_nr)

# Third Interval AI 
reg_tech3_AI <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/reg_techAI_ThirdPeriod.csv", 
                         sep = ";", header = TRUE, dec=",")

mat_reg_tech3_AI <- reg_tech3_AI %>%  arrange(techn_field_nr, ctry_code) %>%
  pivot_wider(names_from = techn_field_nr, values_from = n_tech_reg, 
              values_fill = list(n_tech_reg = 0))

mat_reg_tech3_AI %<>%   remove_rownames %>%   column_to_rownames(var="ctry_code") %>%
  as.matrix() %>%    round()

# Compute RCA for the third interval (AI-specific)
reg_RCA3_AI <- mat_reg_tech3_AI %>%   location_quotient(binary = FALSE) %>% 
  as.data.frame() %>%   rownames_to_column("ctry_code") %>%   as_tibble() %>% 
  gather(key = "techn_field_nr", value = "RCA", -ctry_code) %>%  arrange(ctry_code, techn_field_nr)

# Extract top countries and AI data for third interval
US_Third_period <- reg_RCA3_countries[,2:3][reg_RCA3_countries$ctry_code == "US",]
CN_Third_period <- reg_RCA3_countries[,2:3][reg_RCA3_countries$ctry_code == "CN",]
KR_Third_period <- reg_RCA3_countries[,2:3][reg_RCA3_countries$ctry_code == "KR",]
JP_Third_period <- reg_RCA3_countries[,2:3][reg_RCA3_countries$ctry_code == "JP",]
AI_Third_period <- reg_RCA3_AI[,2:3][reg_RCA3_AI$ctry_code == "AI_pat",]

# Merge IPC names with the countries and AI RCAs for the third interval
Third_period <- merge(merge(merge(merge(merge(IPC_names, US_Third_period), CN_Third_period, by = "techn_field_nr"), 
  KR_Third_period, by = "techn_field_nr"), JP_Third_period, by = "techn_field_nr"), 
  AI_Third_period, by = "techn_field_nr")

names(Third_period) <- c("techn_field_nr", "sector", "field_name", "RCA_US", "RCA_CN","RCA_KR","RCA_JP", "RCA_AI")

write.csv2(Third_period, 
           file = "Files_created_with_the_code/data/files_code_Fields_analysis/Metrics_Third_period.csv", 
           row.names = FALSE)

# Combine all intervals into a single dataset
First_period <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/Metrics_First_period.csv", 
                         sep = ";", header = TRUE, dec=",")
Second_period <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/Metrics_Second_period.csv", 
                          sep = ";", header = TRUE, dec=",")
Third_period <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/Metrics_Third_period.csv", 
                         sep = ";", header = TRUE, dec=",")

First_period$Period <- "Period 1 (1974-1988)"
Second_period$Period <- "Period 2 (1989-2003)"
Third_period$Period <- "Period 3 (2004-2018)"
All_periods <- rbind(First_period, Second_period, Third_period)

# Categorize technology fields
All_periods$Category <- "Other"
Surr <- c(1, 2, 3, 13, 25, 34)
All_periods$Category[(All_periods$techn_field_nr %in% Surr)] <- "Surrounding fields"
AIrel <- c(11, 5, 4)
All_periods$Category[(All_periods$techn_field_nr %in% AIrel)] <- "AI-related fields"
AIcor <- c(6,7,10,12)
All_periods$Category[(All_periods$techn_field_nr %in% AIcor)] <- "AI-core fields"

All_periods$Category <- factor(All_periods$Category, 
                               levels = c("AI-core fields", "AI-related fields", "Surrounding fields", "Other"))

All_periods$Category2 <- as.numeric(All_periods$Category)
All_periods$Category2 <- as.numeric(All_periods$Category2) # redundant line, but keeps original code logic

# Save final specialization data
write.csv2(All_periods, 
           file = "Files_created_with_the_code/data/files_code_Fields_analysis/Specializations_All_periods_IPC.csv", 
           row.names = TRUE)

# Clean environment
rm(list = ls()[!sapply(ls(), function(x) is.function(get(x)))])
gc()

# Summary RCAs for the four leading countries
IPC_RCAs <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/IPC_RCAs.csv", 
                     sep = ";", header = TRUE, dec=",")

# Filter top 4 countries
IPC_RCAs_Top4 <- IPC_RCAs[IPC_RCAs$ctry_code %in% c("CN","KR","US","JP"),]
IPC_RCAs_Top4$ctry_code <- as.vector(IPC_RCAs_Top4$ctry_code)

# Add descriptive labels for fields
IPC_names <- read.csv("other_files/ipc_technology.csv", sep = ";", header = TRUE)%>%
  select(field_nr, sector, field_name) %>%   distinct(field_nr, .keep_all = TRUE) %>%
  mutate(techn_field_nr = field_nr) %>%   arrange(techn_field_nr)

# Match field names to IPC_RCA_Top4
IPC_RCAs_Top4$Label <- IPC_names$field_name[match(IPC_RCAs_Top4$techn_field_nr, IPC_names$techn_field_nr)]

# Replace NA with 0
IPC_RCAs_Top4[is.na(IPC_RCAs_Top4)] <- 0

# Create binary RCA indicators
IPC_RCAs_Top4$Round_general <- ifelse(IPC_RCAs_Top4$RCA_Gen < 1, 0, 1)
IPC_RCAs_Top4$Round_AI <- ifelse(IPC_RCAs_Top4$RCA_AI < 1, 0, 1)

# Total RCA is the sum of binary indicators
IPC_RCAs_Top4$Total_RCA <- IPC_RCAs_Top4$Round_general + IPC_RCAs_Top4$Round_AI

write.csv2(IPC_RCAs_Top4, file = "Files_created_with_the_code/data/files_code_Fields_analysis/RCA_4countries_detailed.csv", 
           row.names = FALSE)

## 1.2 Create Sparse Matrix of relatedness between technological fields -----
# This section creates a large sparse matrix from patent-techn_field data
# and computes their co-occurrence (cross-product). Finally, it saves the resulting technology-relatedness matrix.

### 1.2.1. Calculate Matrix of co-occurrences -----
# Load large files (2004 onward)
c <- 58841893 - 40000000
IPC_all_patents_Part1 <- fread("large_files/All_patents_and_IPCs_Part1.csv", header = FALSE, nrow = 20000000)
IPC_all_patents_Part2 <- fread("large_files/All_patents_and_IPCs_Part1.csv", header = FALSE, nrow = 20000000, skip = 20000000)
IPC_all_patents_Part3 <- fread("large_files/All_patents_and_IPCs_Part1.csv", header = FALSE, nrow = c, skip = 40000000)

names(IPC_all_patents_Part1) <- c("appln_id", "ctry_code", "techn_field_nr", "weight", "priority_year")
names(IPC_all_patents_Part2) <- c("appln_id", "ctry_code", "techn_field_nr", "weight", "priority_year")
names(IPC_all_patents_Part3) <- c("appln_id", "ctry_code", "techn_field_nr", "weight", "priority_year")

# Create sparse matrices and compute cross-products for the first big file
mat_tech_AI1 <- create_sparse_matrix(i = IPC_all_patents_Part1 %>% pull(appln_id),
                                     j = IPC_all_patents_Part1 %>% pull(techn_field_nr)) #
mat_tech_AI1 %<>%   crossprod() %>%  as.matrix() 

#let's make it in just one go:
mat_tech_AI2 <- create_sparse_matrix(i = IPC_all_patents_Part2 %>% pull(appln_id),
                                     j = IPC_all_patents_Part2 %>% pull(techn_field_nr)) %>%
  crossprod() %>% as.matrix()

mat_tech_AI3 <- create_sparse_matrix(i = IPC_all_patents_Part3 %>% pull(appln_id),
                                     j = IPC_all_patents_Part3 %>% pull(techn_field_nr)) %>%
  crossprod() %>% as.matrix()

# Function to add (sum) three matrices, aligning by row and column names
add_matrices_3 <- function(matrix1, matrix2, matrix3) {
  a <- list(matrix1, matrix2, matrix3)
  cols <- sort(unique(unlist(lapply(a, colnames))))
  rows <- sort(unique(unlist(lapply(a, rownames))))
  out <- array(0, dim = c(length(rows), length(cols)), dimnames = list(rows,cols))
  for (m in a) out[rownames(m), colnames(m)] <- out[rownames(m), colnames(m)] + m
  out
}

# Combine the three matrices for the first big file
mat_tech_AI_Final1 <- add_matrices_3(mat_tech_AI1, mat_tech_AI2, mat_tech_AI3)

# Clean up
rm(IPC_all_patents_Part1,IPC_all_patents_Part2, IPC_all_patents_Part3, 
   mat_tech_AI1, mat_tech_AI2, mat_tech_AI3)

# Load second big file (published on or before 2003)
c <- 45182803 - 40000000
IPC_all_patents_Part1 <- fread("large_files/All_patents_and_IPCs_Part2.csv", header = FALSE, nrow = 20000000)
IPC_all_patents_Part2 <- fread("large_files/All_patents_and_IPCs_Part2.csv", header = FALSE, nrow = 20000000, skip = 20000000)
IPC_all_patents_Part3 <- fread("large_files/All_patents_and_IPCs_Part2.csv", header = FALSE, nrow = c, skip = 40000000)

names(IPC_all_patents_Part1) <- c("appln_id", "ctry_code", "techn_field_nr", "weight", "priority_year")
names(IPC_all_patents_Part2) <- c("appln_id", "ctry_code", "techn_field_nr", "weight", "priority_year")
names(IPC_all_patents_Part3) <- c("appln_id", "ctry_code", "techn_field_nr", "weight", "priority_year")

# Create sparse matrices and compute cross-products for the second big file
mat_tech_AI1 <- create_sparse_matrix(i = IPC_all_patents_Part1 %>% pull(appln_id),
                                     j = IPC_all_patents_Part1 %>% pull(techn_field_nr)) %>%
  crossprod() %>% as.matrix()

mat_tech_AI2 <- create_sparse_matrix(i = IPC_all_patents_Part2 %>% pull(appln_id),
                                     j = IPC_all_patents_Part2 %>% pull(techn_field_nr)) %>%
  crossprod() %>% as.matrix()

mat_tech_AI3 <- create_sparse_matrix(i = IPC_all_patents_Part3 %>% pull(appln_id),
                                     j = IPC_all_patents_Part3 %>% pull(techn_field_nr)) %>%
  crossprod() %>% as.matrix()

# Combine the three matrices for the second big file
mat_tech_AI_Final2 <- add_matrices_3(mat_tech_AI1, mat_tech_AI2, mat_tech_AI3)

# Function to sum two matrices
add_matrices_2 <- function(matrix1, matrix2) {
  a <- list(matrix1, matrix2)
  cols <- sort(unique(unlist(lapply(a, colnames))))
  rows <- sort(unique(unlist(lapply(a, rownames))))
  out <- array(0, dim = c(length(rows), length(cols)), dimnames = list(rows,cols))
  for (m in a) out[rownames(m), colnames(m)] <- out[rownames(m), colnames(m)] + m
  out
}

# Combine the first and second set of matrices
mat_tech_AI_Final <- add_matrices_2(mat_tech_AI_Final1, mat_tech_AI_Final2)

# Save the final combined matrix
write.csv2(mat_tech_AI_Final, file = "Files_created_with_the_code/data/files_code_Fields_analysis/Matrix_IPC.csv", row.names = TRUE)

# Clean up
rm(list = ls()[!sapply(ls(), function(x) is.function(get(x)))])
gc()

# Remove unused functions
rm(add_matrices_2, add_matrices_3)

### 1.2.2. Create New Technological Space -----
matrix2 <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/Matrix_IPC.csv", 
                    sep = ";", header = FALSE)

matrix2 <- matrix2 %>%  row_to_names(row_number = 1)
matrix <- matrix2[,-1]
rownames(matrix) <- matrix2[,1]
matrix <- as.matrix(matrix)
mat_tech_AI_Final <- matrix

# Calculate relatedness using cosine similarity
mat_tech_rel_AI <- mat_tech_AI_Final %>% relatedness(method = "cosine")

# Load IPC names and categories
IPC_names <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/Specializations_All_periods_IPC.csv", 
                      sep = ";", header = TRUE)%>%
  select(techn_field_nr, sector, field_name, Category) %>%  distinct(techn_field_nr, .keep_all = TRUE) %>%
  mutate(techn_field_nr = techn_field_nr) %>%  arrange(techn_field_nr)

# Build a graph from the relatedness matrix
g_tech_AI <- mat_tech_rel_AI %>%   as_tbl_graph(directed = FALSE) %N>%
  left_join(IPC_names %>% mutate(techn_field_nr = as.character(techn_field_nr)), 
            by = c("name" = "techn_field_nr")) %>%  mutate(dgr = centrality_eigen(weights = weight)) %E>%
  filter(weight >= mean(weight))

# Layout for visualization (Fruchterman-Reingold)
coords_tech_AI <- g_tech_AI %>%   igraph::layout_with_fr() %>% as_tibble()
colnames(coords_tech_AI) <- c("x", "y")
# Alternatively, load predefined coordinates
coords_tech_AI <- read.csv("other_files/coords_tech_AI_layout1.csv", sep = ";", header = TRUE, dec=",")

# Load top 4 countries RCA details
IPC_RCAs_Top4 <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/RCA_4countries_detailed.csv", 
                          sep = ";", header = TRUE, dec=",")
IPC_RCAs_Top4$Total_RCA <- as.factor(IPC_RCAs_Top4$Total_RCA)
IPC_RCAs_Top4$Period_sim <- as.numeric(factor(IPC_RCAs_Top4$Period, levels=unique(IPC_RCAs_Top4$Period)))
IPC_RCAs_Top4$techn_field_nr <- as.character(IPC_RCAs_Top4$techn_field_nr)

# Load AI RCA data and merge with IPC_RCAs_Top4
AI_RCA <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/Specializations_All_periods_IPC.csv", 
                   sep = ";", header = TRUE, dec=",")
AI_RCA$Period_sim <- as.numeric(factor(AI_RCA$Period, levels=unique(AI_RCA$Period)))
AI_RCA <- AI_RCA[, c(2,9,13)]
AI_RCA$techn_field_nr <- as.character(AI_RCA$techn_field_nr)
names(AI_RCA) <- c("techn_field_nr", "RCA_AI_Period", "Period_sim")

IPC_RCAs_Top4 <- left_join(IPC_RCAs_Top4, AI_RCA, by = c("techn_field_nr", "Period_sim"))

# Adjust Total_RCA_2 to differentiate between general and AI specialization
IPC_RCAs_Top4$Total_RCA_2 <- IPC_RCAs_Top4$Round_general + 2*IPC_RCAs_Top4$Round_AI

# Summarize data by category of specialization
Newtable <- as.data.frame(table(IPC_RCAs_Top4$Total_RCA_2, IPC_RCAs_Top4$ctry_code, IPC_RCAs_Top4$Period))
Newtable$Var1 <- gsub("0", "No specialization", str_trim(Newtable$Var1))
Newtable$Var1 <- gsub("1", "General specialization", str_trim(Newtable$Var1))
Newtable$Var1 <- gsub("2", "AI-specific specialization", str_trim(Newtable$Var1))
Newtable$Var1 <- gsub("3", "Coinciding specialization", str_trim(Newtable$Var1))

write.xlsx(Newtable, file = "Files_created_with_the_code/data/files_code_Fields_analysis/Table_appendix.xlsx", rowNames = FALSE)
rm(Newtable)
### 1.2.3. Plot technological spaces-----
#General (GTS, Figure 3)
General <- 
  g_tech_AI %>%  ggraph(layout =  coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.4, colour = "grey") + 
  geom_node_point(aes(fill = sector, size = 1000^dgr, shape= sector))+ # 
  scale_shape_manual(values=c(21, 22, 23, 24, 25)) + scale_size("Degree", range = c(2, 12)) + 
  geom_node_text(aes(label = paste0(field_name, "\n(", name, ")")), size = 4, repel = TRUE) +  #field_name or name
  theme_graph(base_family = "sans")+  ggtitle("Global technological space: IPC Technological fields") + 
  theme(legend.title = element_text(size = 14), legend.text = element_text(size = 10)) + 
  guides(colour = guide_legend(override.aes = list(size=10)))+
  geom_mark_hull(aes(x = x, y=y, colour = sector, fill= sector,
                     linetype = sector), alpha = 0.15, expand = unit(2.5, "mm"), size = 1) 

jpeg("Files_created_with_the_code/figures/Figure_3_GTS_for_IPC_fields.jpg", width = 14, height = 10, units = 'in', res = 300)
General 
dev.off()
rm(General)

#GTS with specialisations per country
country_select <- c("CN", "US", "JP", "KR")

#### 1.2.3.1. First Country------
i=1
IPC_RCAs_wide_simplified <- IPC_RCAs_Top4 %>% pivot_wider(id_cols = c(ctry_code, techn_field_nr, Label), 
    names_from = Period_sim,
    values_from = c(RCA_AI_Period, Total_RCA_2, RCA_Gen, RCA_AI, Round_general, Round_AI, Total_RCA), 
    names_glue = "{.value}_Period_{Period_sim}" )

China_1stFig <- 
  g_tech_AI %N>% left_join(IPC_RCAs_wide_simplified %>%
                             filter(ctry_code == country_select[i]) %>%
                             select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  mutate(Shape_Group_P1_Factor = factor(
    ifelse(is.na(Total_RCA_2_Period_1), "NA_Value", as.character(Total_RCA_2_Period_1)),
    levels = c("0", "1", "2", "3", "NA_Value"))) %>% ggraph(layout = coords_tech_AI) +
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "#CCCCCC", show.legend = FALSE) + 
  geom_node_point(aes(shape = Shape_Group_P1_Factor, 
                      size = 5, stroke = ifelse(Total_RCA_2_Period_1 == 3, 2.5, 1.3),
                      alpha = 1), color = "#FF3300", show.legend = c(shape=TRUE, size=FALSE, stroke=FALSE, alpha=FALSE, color=FALSE)) + 
  geom_node_point(aes(shape = factor(Total_RCA_2_Period_2),
                      size = 5.5, stroke = ifelse(Total_RCA_2_Period_2 == 3, 2.5, 1.3),
                      alpha = 1), color = "#3399FF", show.legend = FALSE) +
  geom_node_point(aes(shape = factor(Total_RCA_2_Period_3), 
                      size = 6.5,stroke = ifelse(Total_RCA_2_Period_3 == 3, 2.5, 1.3),
                      alpha = 1), color = "#009900", show.legend = FALSE) +
  scale_shape_manual(name = "Type of specialisation",
                     values = c("0" = 4, "1" = 1, "2" = 5, "3" = 2, "NA_Value" = 16), breaks = c("0", "1", "2", "3"),                                
                     labels = c("0" = "No specialisation", "1" = "General specialisation", 
                                "2" = "Break-through specialisation", "3" = "Break-in specialisation"), 
                     na.translate = FALSE, drop = FALSE) + scale_size("Degree", range = c(7, 18))+ 
  scale_alpha(guide = "none") + 
  #geom_node_label(aes(label = name), size = 2, repel = F) + 
  geom_mark_hull(aes(filter = Total_RCA_2_Period_1 > .99, x = x, y = y, fill = "Period 1", group = "Period 1"), 
                 concavity = .1, alpha = .11, linetype = "dotted",expand = unit(2, "mm"), size = .5, color = "#FF3300") + 
  geom_mark_hull(aes(filter = Total_RCA_2_Period_2 > .99, x = x, y = y, fill = "Period 2", group = "Period 2"),
                 concavity = .1, alpha = .11, linetype = "longdash",expand = unit(2, "mm"), size = .5, color = "#3399FF") +
  geom_mark_hull(aes(filter = Total_RCA_2_Period_3 > .99, x = x, y = y, fill = "Period 3", group = "Period 3"),
                 concavity = .1, alpha = .02, expand = unit(2, "mm"), size = 1, color = "#009900") +
  scale_fill_manual(name = "Interval colour (same for \nboth nodes and cluster)", # New legend for fill
                    values = c("Period 1" = "#FF3300", "Period 2" = "#3399FF", "Period 3" = "#009900"),
                    labels = c("Interval 1 (1974-1988)", "Interval 2 (1989-2003)", "Interval 3 (2004-2018)")) +
  theme_graph(base_family = "sans") +  theme(legend.position = "bottom", #right
                                             legend.box = "vertical", legend.title = element_text(size = 12, face = "bold"), 
                                             legend.text = element_text(size = 10), legend.key.size = unit(0.7, "cm") ) +
  ggtitle("d) Global technological space: China (1974-2018)") +
  geom_node_text(aes(label = name), size = 5, repel = TRUE) +  #field_name or name
  guides(shape = guide_legend(title.position = "top", 
                              override.aes = list(size = 5, stroke = 1.5, color = "black") ),
         colour = guide_legend(title.position = "top", 
                               override.aes = list(linetype = c("solid", "longdash", "dotted"), 
                                                   alpha = 1, size = 1, shape = NA) ))

#China_1stFig
#2nd figure
bar_plot_China <- bar_plot_China <- IPC_RCAs_Top4[IPC_RCAs_Top4$ctry_code == country_select[i],] %>%                                   
  arrange(Label, Period) %>%  group_by(Label) %>%                          
  mutate( general = Total_RCA_2 == 1,    
          break_in              = Total_RCA_2 == 2,            
          break_through         = Total_RCA_2 == 3,            
          sustained_general    = general  & lag(general, 1, default = FALSE),
          sustained_break_in    = break_in  & lag(break_in, 1, default = FALSE),
          sustained_break_through    = break_through & lag(break_through, 1, default = FALSE)) %>% 
  ungroup()

bar_plot_China <- bar_plot_China %>% 
  group_by(Period) %>% summarise(`General case`                 = sum(general,           na.rm = TRUE),
                                 `Break-through case`                 = sum(break_in,           na.rm = TRUE),
                                 `Break-in case`            = sum(break_through,      na.rm = TRUE),
                                 `Sustained General case`       = sum(sustained_general, na.rm = TRUE),
                                 `Sustained break-through case`       = sum(sustained_break_in, na.rm = TRUE),
                                 `Sustained break-in case`  = sum(sustained_break_through, na.rm = TRUE),
                                 .groups = "drop") %>% arrange(Period)

plot_long_China <- bar_plot_China |>  rename(Period = Period) |>
  pivot_longer(cols= -Period,names_to= "Indicator",values_to = "Count")

#order labels
plot_long_China$Indicator <- factor(plot_long_China$Indicator, levels = rev(c("General case", "Break-through case",  "Break-in case", 
                                                              "Sustained General case", "Sustained break-through case", "Sustained break-in case")))
plot_long_China$Period <- factor(plot_long_China$Period, levels = c("2004-2018", "1989-2003", "1974-1988"))

legend_order <- c(
  "General case", "Break-through case", "Break-in case",
  "Sustained General case", "Sustained break-through case", "Sustained break-in case"
)

China_2ndFig <- 
  ggplot(plot_long_China, aes(x = factor(Period),y = Count, fill = Indicator)) +
  geom_col(position = position_dodge(width = .8), width = .7) +
  scale_fill_manual(values = c("General case"  = "#FF3300",
    "Sustained General case"  = "#993333",
    "Break-in case"                 = "#009900", #3399FF
    "Sustained break-in case"       = "#006633", #3333CC
    "Break-through case"            = "#3399FF",  #009900
    "Sustained break-through case"  = "#3333CC"),
    breaks = legend_order) + #006633
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(x = "Interval",y = "Number of cases", fill = NULL, title = NULL)+
  ggtitle("Summary of specialisations China") +
  theme_classic(base_size = 11) + theme(legend.position = "bottom")+ coord_flip()
China_2ndFig

combined_plot_ggplot_only <- China_1stFig + China_2ndFig +
  plot_layout(widths = c(0.7, 0.3)) 

ggsave("Files_created_with_the_code/figures/Figure_5_Specialisations_techn_space_3_periods_4_countries_d_China.jpg",
       plot = combined_plot_ggplot_only,
       width = 20, height = 12, dpi = 300, units = "in", bg = "white") #changed height from 10 to 12, position bottom, \n from type 3

#### 1.2.3.2. Second Country------
i=2
country_select[i]
IPC_RCAs_wide_simplified <- IPC_RCAs_Top4 %>% pivot_wider(id_cols = c(ctry_code, techn_field_nr, Label), 
                                                          names_from = Period_sim,
                                                          values_from = c(RCA_AI_Period, Total_RCA_2, RCA_Gen, RCA_AI, Round_general, Round_AI, Total_RCA), 
                                                          names_glue = "{.value}_Period_{Period_sim}" )

USA_1stFig <- g_tech_AI %N>% left_join(IPC_RCAs_wide_simplified %>%
                                           filter(ctry_code == country_select[i]) %>%
                                           select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  mutate(Shape_Group_P1_Factor = factor(
    ifelse(is.na(Total_RCA_2_Period_1), "NA_Value", as.character(Total_RCA_2_Period_1)),
    levels = c("0", "1", "2", "3", "NA_Value"))) %>% ggraph(layout = coords_tech_AI) +
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "#CCCCCC", show.legend = FALSE) + 
  geom_node_point(aes(shape = Shape_Group_P1_Factor, 
                      size = 5, stroke = ifelse(Total_RCA_2_Period_1 == 3, 2.5, 1.3),
                      alpha = 1), color = "#FF3300", show.legend = c(shape=TRUE, size=FALSE, stroke=FALSE, alpha=FALSE, color=FALSE)) + 
  geom_node_point(aes(shape = factor(Total_RCA_2_Period_2),
                      size = 5.5, stroke = ifelse(Total_RCA_2_Period_2 == 3, 2.5, 1.3),
                      alpha = 1), color = "#3399FF", show.legend = FALSE) +
  geom_node_point(aes(shape = factor(Total_RCA_2_Period_3), 
                      size = 6.5,stroke = ifelse(Total_RCA_2_Period_3 == 3, 2.5, 1.3),
                      alpha = 1), color = "#009900", show.legend = FALSE) +
  scale_shape_manual(name = "Type of specialisation",
                     values = c("0" = 4, "1" = 1, "2" = 5, "3" = 2, "NA_Value" = 16), breaks = c("0", "1", "2", "3"),                                
                     labels = c("0" = "No specialisation", "1" = "General specialisation", 
                                "2" = "Break-through specialisation", "3" = "Break-in specialisation"), 
                     na.translate = FALSE, drop = FALSE) + scale_size("Degree", range = c(7, 18))+ 
  scale_alpha(guide = "none") + 
  geom_mark_hull(aes(filter = Total_RCA_2_Period_1 > .99, x = x, y = y, fill = "Period 1", group = "Period 1"), 
                 concavity = .1, alpha = .11, linetype = "dotted",expand = unit(2, "mm"), size = .5, color = "#FF3300") + 
  geom_mark_hull(aes(filter = Total_RCA_2_Period_2 > .99, x = x, y = y, fill = "Period 2", group = "Period 2"),
                 concavity = .1, alpha = .11, linetype = "longdash",expand = unit(2, "mm"), size = .5, color = "#3399FF") +
  geom_mark_hull(aes(filter = Total_RCA_2_Period_3 > .99, x = x, y = y, fill = "Period 3", group = "Period 3"),
                 concavity = .1, alpha = .02, expand = unit(2, "mm"), size = 1, color = "#009900") +
  scale_fill_manual(name = "Interval colour (same for \nboth nodes and cluster)", # New legend for fill
                    values = c("Period 1" = "#FF3300", "Period 2" = "#3399FF", "Period 3" = "#009900"),
                    labels = c("Interval 1 (1974-1988)", "Interval 2 (1989-2003)", "Interval 3 (2004-2018)")) +
  theme_graph(base_family = "sans") +  theme(legend.position = "bottom",
                                             legend.box = "vertical", legend.title = element_text(size = 12, face = "bold"), 
                                             legend.text = element_text(size = 10), legend.key.size = unit(0.7, "cm") ) +
  ggtitle("b) Global technological space: USA (1974-2018)") +
  geom_node_text(aes(label = name), size = 5, repel = TRUE) +  #field_name or name
  guides(shape = guide_legend(title.position = "top", 
                              override.aes = list(size = 5, stroke = 1.5, color = "black") ),
         colour = guide_legend(title.position = "top", 
                               override.aes = list(linetype = c("solid", "longdash", "dotted"), 
                                                   alpha = 1, size = 1, shape = NA) ))

#2nd figure
bar_plot_USA <- IPC_RCAs_Top4[IPC_RCAs_Top4$ctry_code == country_select[i],] %>%                                   
  arrange(Label, Period) %>%  group_by(Label) %>%                          
  mutate( general = Total_RCA_2 == 1,    
          break_in              = Total_RCA_2 == 2,            
          break_through         = Total_RCA_2 == 3,            
          sustained_general    = general  & lag(general, 1, default = FALSE),
          sustained_break_in    = break_in  & lag(break_in, 1, default = FALSE),
          sustained_break_through    = break_through & lag(break_through, 1, default = FALSE)) %>% 
  ungroup()

bar_plot_USA <- bar_plot_USA %>% 
  group_by(Period) %>% summarise(`General case`                 = sum(general,           na.rm = TRUE),
                                 `Break-through case`                 = sum(break_in,           na.rm = TRUE),
                                 `Break-in case`            = sum(break_through,      na.rm = TRUE),
                                 `Sustained General case`       = sum(sustained_general, na.rm = TRUE),
                                 `Sustained break-through case`       = sum(sustained_break_in, na.rm = TRUE),
                                 `Sustained break-in case`  = sum(sustained_break_through, na.rm = TRUE),
                                 .groups = "drop") %>% arrange(Period)

plot_long_USA <- bar_plot_USA |>  rename(Period = Period) |>
  pivot_longer(cols= -Period,names_to= "Indicator",values_to = "Count")

#order labels
plot_long_USA$Indicator <- factor(plot_long_USA$Indicator, levels = rev(c("General case", "Break-through case",  "Break-in case", 
                                                                          "Sustained General case", "Sustained break-through case", "Sustained break-in case")))
plot_long_USA$Period <- factor(plot_long_USA$Period, levels = c("2004-2018", "1989-2003", "1974-1988"))

USA_2ndFig <- 
  ggplot(plot_long_USA, aes(x = factor(Period),y = Count, fill = Indicator)) +
  geom_col(position = position_dodge(width = .8), width = .7) +
  scale_fill_manual(values = c("General case"  = "#FF3300",
                               "Sustained General case"  = "#993333",
                               "Break-in case"                 = "#009900", #3399FF
                               "Sustained break-in case"       = "#006633", #3333CC
                               "Break-through case"            = "#3399FF",  #009900
                               "Sustained break-through case"  = "#3333CC"),
                    breaks = legend_order) + #006633
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(x = "Interval",y = "Number of cases", fill = NULL, title = NULL)+
  ggtitle("Summary of specialisations USA") +
  theme_classic(base_size = 11) + theme(legend.position = "bottom")+ coord_flip()

combined_plot_ggplot_only <- USA_1stFig + USA_2ndFig +
  plot_layout(widths = c(0.7, 0.3)) 

ggsave("Files_created_with_the_code/figures/Figure_5_Specialisations_techn_space_3_periods_4_countries_b_USA.jpg",
       plot = combined_plot_ggplot_only,
       width = 20, height = 12, dpi = 300, units = "in", bg = "white") 

#### 1.2.3.3. Third Country------
i=3
country_select[i]
IPC_RCAs_wide_simplified <- IPC_RCAs_Top4 %>% pivot_wider(id_cols = c(ctry_code, techn_field_nr, Label), 
                                                          names_from = Period_sim,
                                                          values_from = c(RCA_AI_Period, Total_RCA_2, RCA_Gen, RCA_AI, Round_general, Round_AI, Total_RCA), 
                                                          names_glue = "{.value}_Period_{Period_sim}" )

Japan_1stFig <- g_tech_AI %N>% left_join(IPC_RCAs_wide_simplified %>%
                                           filter(ctry_code == country_select[i]) %>%
                                           select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  mutate(Shape_Group_P1_Factor = factor(
    ifelse(is.na(Total_RCA_2_Period_1), "NA_Value", as.character(Total_RCA_2_Period_1)),
    levels = c("0", "1", "2", "3", "NA_Value"))) %>% ggraph(layout = coords_tech_AI) +
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "#CCCCCC", show.legend = FALSE) + 
  geom_node_point(aes(shape = Shape_Group_P1_Factor, 
                      size = 5, stroke = ifelse(Total_RCA_2_Period_1 == 3, 2.5, 1.3),
                      alpha = 1), color = "#FF3300", show.legend = c(shape=TRUE, size=FALSE, stroke=FALSE, alpha=FALSE, color=FALSE)) + 
  geom_node_point(aes(shape = factor(Total_RCA_2_Period_2),
                      size = 5.5, stroke = ifelse(Total_RCA_2_Period_2 == 3, 2.5, 1.3),
                      alpha = 1), color = "#3399FF", show.legend = FALSE) +
  geom_node_point(aes(shape = factor(Total_RCA_2_Period_3), 
                      size = 6.5,stroke = ifelse(Total_RCA_2_Period_3 == 3, 2.5, 1.3),
                      alpha = 1), color = "#009900", show.legend = FALSE) +
  scale_shape_manual(name = "Type of specialisation",
                     values = c("0" = 4, "1" = 1, "2" = 5, "3" = 2, "NA_Value" = 16), breaks = c("0", "1", "2", "3"),                                
                     labels = c("0" = "No specialisation", "1" = "General specialisation", 
                                "2" = "Break-through specialisation", "3" = "Break-in specialisation"), 
                     na.translate = FALSE, drop = FALSE) + scale_size("Degree", range = c(7, 18))+ 
  scale_alpha(guide = "none") + 
  geom_mark_hull(aes(filter = Total_RCA_2_Period_1 > .99, x = x, y = y, fill = "Period 1", group = "Period 1"), 
                 concavity = .1, alpha = .11, linetype = "dotted",expand = unit(2, "mm"), size = .5, color = "#FF3300") + 
  geom_mark_hull(aes(filter = Total_RCA_2_Period_2 > .99, x = x, y = y, fill = "Period 2", group = "Period 2"),
                 concavity = .1, alpha = .11, linetype = "longdash",expand = unit(2, "mm"), size = .5, color = "#3399FF") +
  geom_mark_hull(aes(filter = Total_RCA_2_Period_3 > .99, x = x, y = y, fill = "Period 3", group = "Period 3"),
                 concavity = .1, alpha = .02, expand = unit(2, "mm"), size = 1, color = "#009900") +
  scale_fill_manual(name = "Interval colour (same for \nboth nodes and cluster)", # New legend for fill
                    values = c("Period 1" = "#FF3300", "Period 2" = "#3399FF", "Period 3" = "#009900"),
                    labels = c("Interval 1 (1974-1988)", "Interval 2 (1989-2003)", "Interval 3 (2004-2018)")) +
  theme_graph(base_family = "sans") +  theme(legend.position = "bottom",
                                             legend.box = "vertical", legend.title = element_text(size = 12, face = "bold"), 
                                             legend.text = element_text(size = 10), legend.key.size = unit(0.7, "cm") ) +
  ggtitle("a) Global technological space: Japan (1974-2018)") +
  geom_node_text(aes(label = name), size = 5, repel = TRUE) +  #field_name or name
  guides(shape = guide_legend(title.position = "top", 
                              override.aes = list(size = 5, stroke = 1.5, color = "black") ),
         colour = guide_legend(title.position = "top", 
                               override.aes = list(linetype = c("solid", "longdash", "dotted"), 
                                                   alpha = 1, size = 1, shape = NA) ))

#2nd figure
bar_plot_Japan <- IPC_RCAs_Top4[IPC_RCAs_Top4$ctry_code == country_select[i],] %>%                                   
  arrange(Label, Period) %>%  group_by(Label) %>%                          
  mutate( general = Total_RCA_2 == 1,    
          break_in              = Total_RCA_2 == 2,            
          break_through         = Total_RCA_2 == 3,            
          sustained_general    = general  & lag(general, 1, default = FALSE),
          sustained_break_in    = break_in  & lag(break_in, 1, default = FALSE),
          sustained_break_through    = break_through & lag(break_through, 1, default = FALSE)) %>% 
  ungroup()

bar_plot_Japan <- bar_plot_Japan %>% 
  group_by(Period) %>% summarise(`General case`                 = sum(general,           na.rm = TRUE),
                                 `Break-through case`                 = sum(break_in,           na.rm = TRUE),
                                 `Break-in case`            = sum(break_through,      na.rm = TRUE),
                                 `Sustained General case`       = sum(sustained_general, na.rm = TRUE),
                                 `Sustained break-through case`       = sum(sustained_break_in, na.rm = TRUE),
                                 `Sustained break-in case`  = sum(sustained_break_through, na.rm = TRUE),
                                 .groups = "drop") %>% arrange(Period)

plot_long_Japan <- bar_plot_Japan |>  rename(Period = Period) |>
  pivot_longer(cols= -Period,names_to= "Indicator",values_to = "Count")

#order labels
plot_long_Japan$Indicator <- factor(plot_long_Japan$Indicator, levels = rev(c("General case", "Break-through case",  "Break-in case", 
                                                                              "Sustained General case", "Sustained break-through case", "Sustained break-in case")))
plot_long_Japan$Period <- factor(plot_long_Japan$Period, levels = c("2004-2018", "1989-2003", "1974-1988"))

Japan_2ndFig <- 
  ggplot(plot_long_Japan, aes(x = factor(Period),y = Count, fill = Indicator)) +
  geom_col(position = position_dodge(width = .8), width = .7) +
  scale_fill_manual(values = c("General case"  = "#FF3300",
                               "Sustained General case"  = "#993333",
                               "Break-in case"                 = "#009900", #3399FF
                               "Sustained break-in case"       = "#006633", #3333CC
                               "Break-through case"            = "#3399FF",  #009900
                               "Sustained break-through case"  = "#3333CC"),
                    breaks = legend_order) + #006633
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(x = "Interval",y = "Number of cases", fill = NULL, title = NULL)+
  ggtitle("Summary of specialisations Japan") +
  scale_y_continuous(breaks = breaks_pretty(n = 5))+
  theme_classic(base_size = 11) + theme(legend.position = "bottom")+ coord_flip()
#Japan_2ndFig
combined_plot_ggplot_only <- Japan_1stFig + Japan_2ndFig +
  plot_layout(widths = c(0.7, 0.3)) 

ggsave("Files_created_with_the_code/figures/Figure_5_Specialisations_techn_space_3_periods_4_countries_a_Japan.jpg",
       plot = combined_plot_ggplot_only,
       width = 20, height = 12, dpi = 300, units = "in", bg = "white") 

#### 1.2.3.4. Fourth Country------
i=4
IPC_RCAs_wide_simplified <- IPC_RCAs_Top4 %>% pivot_wider(id_cols = c(ctry_code, techn_field_nr, Label), 
                                                          names_from = Period_sim,
                                                          values_from = c(RCA_AI_Period, Total_RCA_2, RCA_Gen, RCA_AI, Round_general, Round_AI, Total_RCA), 
                                                          names_glue = "{.value}_Period_{Period_sim}" )

SouthKorea_1stFig <- g_tech_AI %N>% left_join(IPC_RCAs_wide_simplified %>%
                                           filter(ctry_code == country_select[i]) %>%
                                           select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  mutate(Shape_Group_P1_Factor = factor(
    ifelse(is.na(Total_RCA_2_Period_1), "NA_Value", as.character(Total_RCA_2_Period_1)),
    levels = c("0", "1", "2", "3", "NA_Value"))) %>% ggraph(layout = coords_tech_AI) +
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "#CCCCCC", show.legend = FALSE) + 
  geom_node_point(aes(shape = Shape_Group_P1_Factor, 
                      size = 5, stroke = ifelse(Total_RCA_2_Period_1 == 3, 2.5, 1.3),
                      alpha = 1), color = "#FF3300", show.legend = c(shape=TRUE, size=FALSE, stroke=FALSE, alpha=FALSE, color=FALSE)) + 
  geom_node_point(aes(shape = factor(Total_RCA_2_Period_2),
                      size = 5.5, stroke = ifelse(Total_RCA_2_Period_2 == 3, 2.5, 1.3),
                      alpha = 1), color = "#3399FF", show.legend = FALSE) +
  geom_node_point(aes(shape = factor(Total_RCA_2_Period_3), 
                      size = 6.5,stroke = ifelse(Total_RCA_2_Period_3 == 3, 2.5, 1.3),
                      alpha = 1), color = "#009900", show.legend = FALSE) +
  scale_shape_manual(name = "Type of specialisation",
                     values = c("0" = 4, "1" = 1, "2" = 5, "3" = 2, "NA_Value" = 16), breaks = c("0", "1", "2", "3"),                                
                     labels = c("0" = "No specialisation", "1" = "General specialisation", 
                                "2" = "Break-through specialisation", "3" = "Break-in specialisation"), 
                     na.translate = FALSE, drop = FALSE) + scale_size("Degree", range = c(7, 18))+ 
  scale_alpha(guide = "none") + 
  geom_mark_hull(aes(filter = Total_RCA_2_Period_1 > .99, x = x, y = y, fill = "Period 1", group = "Period 1"), 
                 concavity = .1, alpha = .11, linetype = "dotted",expand = unit(2, "mm"), size = .5, color = "#FF3300") + 
  geom_mark_hull(aes(filter = Total_RCA_2_Period_2 > .99, x = x, y = y, fill = "Period 2", group = "Period 2"),
                 concavity = .1, alpha = .11, linetype = "longdash",expand = unit(2, "mm"), size = .5, color = "#3399FF") +
  geom_mark_hull(aes(filter = Total_RCA_2_Period_3 > .99, x = x, y = y, fill = "Period 3", group = "Period 3"),
                 concavity = .1, alpha = .02, expand = unit(2, "mm"), size = 1, color = "#009900") +
  scale_fill_manual(name = "Interval colour (same for \nboth nodes and cluster)", # New legend for fill
                    values = c("Period 1" = "#FF3300", "Period 2" = "#3399FF", "Period 3" = "#009900"),
                    labels = c("Interval 1 (1974-1988)", "Interval 2 (1989-2003)", "Interval 3 (2004-2018)")) +
  theme_graph(base_family = "sans") +  theme(legend.position = "bottom",
                                             legend.box = "vertical", legend.title = element_text(size = 12, face = "bold"), 
                                             legend.text = element_text(size = 10), legend.key.size = unit(0.7, "cm") ) +
  ggtitle("c) Global technological space: South Korea (1974-2018)") +
  geom_node_text(aes(label = name), size = 5, repel = TRUE) +  #field_name or name
  guides(shape = guide_legend(title.position = "top", 
                              override.aes = list(size = 5, stroke = 1.5, color = "black") ),
         colour = guide_legend(title.position = "top", 
                               override.aes = list(linetype = c("solid", "longdash", "dotted"), 
                                                   alpha = 1, size = 1, shape = NA) ))

#2nd figure
bar_plot_SouthKorea <- IPC_RCAs_Top4[IPC_RCAs_Top4$ctry_code == country_select[i],] %>%                                   
  arrange(Label, Period) %>%  group_by(Label) %>%                          
  mutate( general = Total_RCA_2 == 1,    
          break_in              = Total_RCA_2 == 2,            
          break_through         = Total_RCA_2 == 3,            
          sustained_general    = general  & lag(general, 1, default = FALSE),
          sustained_break_in    = break_in  & lag(break_in, 1, default = FALSE),
          sustained_break_through    = break_through & lag(break_through, 1, default = FALSE)) %>% 
  ungroup()

bar_plot_SouthKorea <- bar_plot_SouthKorea %>% 
  group_by(Period) %>% summarise(`General case`                 = sum(general,           na.rm = TRUE),
                                 `Break-through case`                 = sum(break_in,           na.rm = TRUE),
                                 `Break-in case`            = sum(break_through,      na.rm = TRUE),
                                 `Sustained General case`       = sum(sustained_general, na.rm = TRUE),
                                 `Sustained break-through case`       = sum(sustained_break_in, na.rm = TRUE),
                                 `Sustained break-in case`  = sum(sustained_break_through, na.rm = TRUE),
                                 .groups = "drop") %>% arrange(Period)

plot_long_SouthKorea <- bar_plot_SouthKorea |>  rename(Period = Period) |>
  pivot_longer(cols= -Period,names_to= "Indicator",values_to = "Count")

#order labels
plot_long_SouthKorea$Indicator <- factor(plot_long_SouthKorea$Indicator, levels = rev(c("General case", "Break-through case",  "Break-in case", 
                                                                                        "Sustained General case", "Sustained break-through case", "Sustained break-in case")))
plot_long_SouthKorea$Period <- factor(plot_long_SouthKorea$Period, levels = c("2004-2018", "1989-2003", "1974-1988"))

SouthKorea_2ndFig <- 
  ggplot(plot_long_SouthKorea, aes(x = factor(Period),y = Count, fill = Indicator)) +
  geom_col(position = position_dodge(width = .8), width = .7) +
  scale_fill_manual(values = c("General case"  = "#FF3300",
                               "Sustained General case"  = "#993333",
                               "Break-in case"                 = "#009900", #3399FF
                               "Sustained break-in case"       = "#006633", #3333CC
                               "Break-through case"            = "#3399FF",  #009900
                               "Sustained break-through case"  = "#3333CC"),
                    breaks = legend_order) + #006633
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(x = "Interval",y = "Number of cases", fill = NULL, title = NULL)+
  ggtitle("Summary of specialisations South Korea") +
  theme_classic(base_size = 11) + theme(legend.position = "bottom")+ coord_flip()
#SouthKorea_2ndFig
combined_plot_ggplot_only <- SouthKorea_1stFig + SouthKorea_2ndFig +
  plot_layout(widths = c(0.7, 0.3)) 

ggsave("Files_created_with_the_code/figures/Figure_5_Specialisations_techn_space_3_periods_4_countries_c_SouthKorea.jpg",
       plot = combined_plot_ggplot_only,
       width = 20, height = 12, dpi = 300, units = "in", bg = "white") 

####1.2.3.5.Extra figures Appendix C-----
p=1
China_1st<-
  g_tech_AI %N>%
  left_join(IPC_RCAs_Top4 %>% filter(ctry_code == country_select[i] & IPC_RCAs_Top4$Period_sim == p) %>% 
              select(-ctry_code), by = c("name" = "techn_field_nr")) %>%  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "#CCCCCC") +
  geom_node_point(aes(fill = factor(Total_RCA_2), size = 7, shape= factor(Total_RCA_2))) +
  scale_shape_manual(values=c(21, 22, 23, 24)) + labs(color   = "RCA") + scale_size("Degree", range = c(2, 12))+ 
  geom_node_text(aes(filter=RCA_AI_Period > .99, label = field_name), size = 4, repel = TRUE) +
  scale_fill_manual(values=c("#999999", "#FF3300", "#3399FF", "#009900"))+
  theme_graph(base_family = "sans") + theme(legend.position = "none")+
  ggtitle("Global technological space: China (1974-1988)")

p=2
China_2nd<-
  g_tech_AI %N>%
  left_join(IPC_RCAs_Top4 %>% filter(ctry_code == country_select[i] & IPC_RCAs_Top4$Period_sim == p) %>% 
              select(-ctry_code), by = c("name" = "techn_field_nr")) %>%  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "#CCCCCC") +
  geom_node_point(aes(fill = factor(Total_RCA_2), size = 7, shape= factor(Total_RCA_2))) +
  scale_shape_manual(values=c(21, 22, 23, 24)) + labs(color   = "RCA") + scale_size("Degree", range = c(2, 12))+ 
  geom_node_text(aes(filter=RCA_AI_Period > .99, label = field_name), size = 4, repel = TRUE) +
  scale_fill_manual(values=c("#999999", "#FF3300", "#3399FF", "#009900"))+
  theme_graph(base_family = "sans") +theme(legend.position = "none")+
  ggtitle("Global technological space: China (1989-2003)")

p=3
China_3rd<-
  g_tech_AI %N>%
  left_join(IPC_RCAs_Top4 %>% filter(ctry_code == country_select[i] & IPC_RCAs_Top4$Period_sim == p) %>% 
              select(-ctry_code), by = c("name" = "techn_field_nr")) %>%  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "#CCCCCC", show.legend = F) +
  geom_node_point(aes(fill = factor(Total_RCA_2), size = 7, shape= factor(Total_RCA_2))) +
  scale_shape_manual(values=c(21, 22, 23, 24)) + labs(color   = "RCA") + scale_size("Degree", range = c(2, 12))+ 
  geom_node_text(aes(filter=RCA_AI_Period > .99, label = field_name), size = 4, repel = TRUE) +
  scale_fill_manual(values=c("#999999", "#FF3300", "#3399FF", "#009900"))+
  theme_graph(base_family = "sans") +guides(size = "none",  fill = guide_legend(order = 1, override.aes = list(size = 5)), 
                                            shape = guide_legend(order = 1, override.aes = list(size = 10)) )+
  theme(legend.title = element_text(size = 14, face = "bold"), legend.text = element_text(size = 12),  
        legend.key.size = unit(1.2, "cm")) +  labs( fill = "Type of \nspecialisation", shape = "Type of \nspecialisation") +
  ggtitle("Global technological space: China (2004-2018)")

jpeg("Files_created_with_the_code/figures/extra/Example_Figure_5_Specialisations_techn_space_4_countries_d_China_1stInterval.jpg", width = 14, height = 10, units = 'in', res = 300)
China_1st 
dev.off()

### 1.2.3.2. Second Country
i=2
p=1
USA_1st<-
  g_tech_AI %N>%
  left_join(IPC_RCAs_Top4 %>% filter(ctry_code == country_select[i] & IPC_RCAs_Top4$Period_sim == p) %>% 
              select(-ctry_code), by = c("name" = "techn_field_nr")) %>%  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "#CCCCCC") +
  geom_node_point(aes(fill = factor(Total_RCA_2), size = 7, shape= factor(Total_RCA_2))) +
  scale_shape_manual(values=c(21, 22, 23, 24)) + labs(color   = "RCA") + scale_size("Degree", range = c(2, 12))+ 
  geom_node_text(aes(filter=RCA_AI_Period > .99, label = field_name), size = 5, repel = TRUE) +
  scale_fill_manual(values=c("#999999", "#FF3300", "#3399FF", "#009900"))+
  theme_graph(base_family = "sans") + theme(legend.position = "none")+
  ggtitle("Global technological space: USA (1974-1988)")

p=2
USA_2nd<-
  g_tech_AI %N>%
  left_join(IPC_RCAs_Top4 %>% filter(ctry_code == country_select[i] & IPC_RCAs_Top4$Period_sim == p) %>% 
              select(-ctry_code), by = c("name" = "techn_field_nr")) %>%  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "#CCCCCC") +
  geom_node_point(aes(fill = factor(Total_RCA_2), size = 7, shape= factor(Total_RCA_2))) +
  scale_shape_manual(values=c(21, 22, 23, 24)) + labs(color   = "RCA") + scale_size("Degree", range = c(2, 12))+ 
  geom_node_text(aes(filter=RCA_AI_Period > .99, label = field_name), size = 5, repel = TRUE) +
  scale_fill_manual(values=c("#999999", "#FF3300", "#3399FF", "#009900"))+
  theme_graph(base_family = "sans") + theme(legend.position = "none")+
  ggtitle("Global technological space: USA (1989-2003)")

p=3

USA_3rd<-
  g_tech_AI %N>%
  left_join(IPC_RCAs_Top4 %>% filter(ctry_code == country_select[i] & IPC_RCAs_Top4$Period_sim == p) %>% 
              select(-ctry_code), by = c("name" = "techn_field_nr")) %>%  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "#CCCCCC", show.legend = F) + 
  geom_node_point(aes(fill = factor(Total_RCA_2), size = 7, shape= factor(Total_RCA_2))) +
  scale_shape_manual(values=c(21, 22, 23, 24)) + labs(color   = "RCA") + scale_size("Degree", range = c(2, 12))+ 
  geom_node_text(aes(filter=RCA_AI_Period > .99, label = field_name), size = 5, repel = TRUE) +
  scale_fill_manual(values=c("#999999", "#FF3300", "#3399FF", "#009900"))+
  theme_graph(base_family = "sans") + guides(size = "none",  fill = guide_legend(order = 1, override.aes = list(size = 5)), 
                                             shape = guide_legend(order = 1, override.aes = list(size = 10)) )+
  theme(legend.title = element_text(size = 14, face = "bold"), legend.text = element_text(size = 12),  
        legend.key.size = unit(1.2, "cm")) +  labs( fill = "Type of \nspecialisation", shape = "Type of \nspecialisation") +
  ggtitle("Global technological space: USA (2004-2018)")

### 1.2.3.3. Third Country
i=3
p=1
Japan_1st<-
  g_tech_AI %N>%
  left_join(IPC_RCAs_Top4 %>% filter(ctry_code == country_select[i] & IPC_RCAs_Top4$Period_sim == p) %>% 
              select(-ctry_code), by = c("name" = "techn_field_nr")) %>%  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "#CCCCCC") +
  geom_node_point(aes(fill = factor(Total_RCA_2), size = 7, shape= factor(Total_RCA_2))) +
  scale_shape_manual(values=c(21, 22, 23, 24)) + labs(color   = "RCA") + scale_size("Degree", range = c(2, 12))+ 
  geom_node_text(aes(filter=RCA_AI_Period > .99, label = field_name), size = 5, repel = TRUE) +
  scale_fill_manual(values=c("#999999", "#FF3300", "#3399FF", "#009900"))+
  theme_graph(base_family = "sans") + theme(legend.position = "none")+
  ggtitle("Global technological space: Japan (1974-1988)")

p=2
Japan_2nd<-
  g_tech_AI %N>%
  left_join(IPC_RCAs_Top4 %>% filter(ctry_code == country_select[i] & IPC_RCAs_Top4$Period_sim == p) %>% 
              select(-ctry_code), by = c("name" = "techn_field_nr")) %>%  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "#CCCCCC") +
  geom_node_point(aes(fill = factor(Total_RCA_2), size = 7, shape= factor(Total_RCA_2))) +
  scale_shape_manual(values=c(21, 22, 23, 24)) + labs(color   = "RCA") + scale_size("Degree", range = c(2, 12))+ 
  geom_node_text(aes(filter=RCA_AI_Period > .99, label = field_name), size = 5, repel = TRUE) +
  scale_fill_manual(values=c("#999999", "#FF3300", "#3399FF", "#009900"))+
  theme_graph(base_family = "sans") + theme(legend.position = "none")+
  ggtitle("Global technological space: Japan (1989-2003)")

p=3
Japan_3rd<-
  g_tech_AI %N>%
  left_join(IPC_RCAs_Top4 %>% filter(ctry_code == country_select[i] & IPC_RCAs_Top4$Period_sim == p) %>% 
              select(-ctry_code), by = c("name" = "techn_field_nr")) %>%  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "#CCCCCC", show.legend = F) +
  geom_node_point(aes(fill = factor(Total_RCA_2), size = 7, shape= factor(Total_RCA_2))) +
  scale_shape_manual(values=c(21, 22, 23, 24)) + labs(color   = "RCA") + scale_size("Degree", range = c(2, 12))+ 
  geom_node_text(aes(filter=RCA_AI_Period > .99, label = field_name), size = 5, repel = TRUE) +
  scale_fill_manual(values=c("#999999", "#FF3300", "#3399FF", "#009900"))+
  theme_graph(base_family = "sans") + guides(size = "none",  fill = guide_legend(order = 1, override.aes = list(size = 5)), 
                                             shape = guide_legend(order = 1, override.aes = list(size = 10)) )+
  theme(legend.title = element_text(size = 14, face = "bold"), legend.text = element_text(size = 12),  
        legend.key.size = unit(1.2, "cm")) +  labs( fill = "Type of \nspecialisation", shape = "Type of \nspecialisation") +
  ggtitle("Global technological space: Japan (2004-2018)")

### 1.2.3.4. Fourth Country
i=4
p=1
SouthKorea_1st<-
  g_tech_AI %N>%
  left_join(IPC_RCAs_Top4 %>% filter(ctry_code == country_select[i] & IPC_RCAs_Top4$Period_sim == p) %>% 
              select(-ctry_code), by = c("name" = "techn_field_nr")) %>%  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "#CCCCCC") +
  geom_node_point(aes(fill = factor(Total_RCA_2), size = 7, shape= factor(Total_RCA_2))) +
  scale_shape_manual(values=c(21, 22, 23, 24)) + labs(color   = "RCA") + scale_size("Degree", range = c(2, 12))+ 
  geom_node_text(aes(filter=RCA_AI_Period > .99, label = field_name), size = 5, repel = TRUE) +
  scale_fill_manual(values=c("#999999", "#FF3300", "#3399FF", "#009900"))+
  theme_graph(base_family = "sans") +theme(legend.position = "none")+
  ggtitle("Global technological space: South Korea (1974-1988)")

p=2
SouthKorea_2nd<-
  g_tech_AI %N>%
  left_join(IPC_RCAs_Top4 %>% filter(ctry_code == country_select[i] & IPC_RCAs_Top4$Period_sim == p) %>% 
              select(-ctry_code), by = c("name" = "techn_field_nr")) %>%  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "#CCCCCC") +
  geom_node_point(aes(fill = factor(Total_RCA_2), size = 7, shape= factor(Total_RCA_2))) +
  scale_shape_manual(values=c(21, 22, 23, 24)) + labs(color   = "RCA") + scale_size("Degree", range = c(2, 12))+ 
  geom_node_text(aes(filter=RCA_AI_Period > .99, label = field_name), size = 5, repel = TRUE) +
  scale_fill_manual(values=c("#999999", "#FF3300", "#3399FF", "#009900"))+
  theme_graph(base_family = "sans") +theme(legend.position = "none")+
  ggtitle("Global technological space: South Korea (1989-2003)")

p=3
SouthKorea_3rd<-
  g_tech_AI %N>%
  left_join(IPC_RCAs_Top4 %>% filter(ctry_code == country_select[i] & IPC_RCAs_Top4$Period_sim == p) %>% 
              select(-ctry_code), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "#CCCCCC", show.legend = F) +
  geom_node_point(aes(fill = factor(Total_RCA_2), size = 7, shape= factor(Total_RCA_2))) +
  scale_shape_manual(values=c(21, 22, 23, 24)) + labs(color   = "RCA") + scale_size("Degree", range = c(2, 12))+ 
  geom_node_text(aes(filter=RCA_AI_Period > .99, label = field_name), size = 5, repel = TRUE) +
  scale_fill_manual(values=c("#999999", "#FF3300", "#3399FF", "#009900"))+
  theme_graph(base_family = "sans") +guides(size = "none",  fill = guide_legend(order = 1, override.aes = list(size = 5)), 
                                            shape = guide_legend(order = 1, override.aes = list(size = 10)) )+
  theme(legend.title = element_text(size = 14, face = "bold"), legend.text = element_text(size = 12),  
        legend.key.size = unit(1.2, "cm")) +  labs( fill = "Type of \nspecialisation", shape = "Type of \nspecialisation") +
  ggtitle("Global technological space: South Korea (2004-2018)")

#Now we'll plot the three intervals at once per country:
jpeg("Files_created_with_the_code/figures/extra/Appendix_C_d_China.jpg", width = 38, height = 10, units = 'in', res = 300)
multiplot(China_1st, China_2nd, China_3rd, cols=3) 
dev.off()

jpeg("Files_created_with_the_code/figures/extra/Appendix_C_b_USA.jpg", width = 38, height = 10, units = 'in', res = 300)
multiplot(USA_1st, USA_2nd, USA_3rd, cols=3) 
dev.off()

jpeg("Files_created_with_the_code/figures/extra/Appendix_C_a_Japan.jpg", width = 38, height = 10, units = 'in', res = 300)
multiplot(Japan_1st, Japan_2nd, Japan_3rd, cols=3) 
dev.off()

jpeg("Files_created_with_the_code/figures/extra/Appendix_C_c_SouthKorea.jpg", width = 38, height = 10, units = 'in', res = 300)
multiplot(SouthKorea_1st, SouthKorea_2nd, SouthKorea_3rd, cols=3) 
dev.off()

rm(China_1st, China_2nd, China_3rd, USA_1st, USA_2nd, USA_3rd, Japan_1st, Japan_2nd, Japan_3rd, SouthKorea_1st, SouthKorea_2nd, SouthKorea_3rd)

### 1.2.4. Technological space AI (ATS) ----
# Remove all non-function objects in the global environment and clean memory
rm(list = ls()[!sapply(ls(), function(x) is.function(get(x)))])
gc()

# Get the AI matrix, based on its technological fields
IPC_all_patents_Part1 <- fread("large_files/All_patents_and_IPCs_Part1.csv", header = F)
IPC_all_patents_Part2 <- fread("large_files/All_patents_and_IPCs_Part2.csv", header = F)

table(IPC_all_patents_Part1$V5)
table(IPC_all_patents_Part2$V5)

IPC_all_patents_complete <- rbind(IPC_all_patents_Part1,IPC_all_patents_Part2)
rm(IPC_all_patents_Part1,IPC_all_patents_Part2)
names(IPC_all_patents_complete) <- c("appln_id", "ctry_code", "techn_field_nr", "weight", "priority_year")

patents_AI <- read.csv("other_files/IPCs_AI.csv", sep = ";", header = TRUE, dec=",")

#add technological fields numbers to AI patents
patents_AI <- distinct(patents_AI, appln_id, .keep_all = TRUE)[,c(1,3)]
patents_AI <- left_join(patents_AI, IPC_all_patents_complete, by = "appln_id")
rm(IPC_all_patents_complete)
write.csv2(patents_AI, file = "Files_created_with_the_code/data/files_code_Fields_analysis/AI_ALL_patents.csv", row.names = F)
rm(patents_AI)

# ATS First interval
patents_AI_specific_1st <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/AI_ALL_patents.csv", sep = ";", header = TRUE, dec=",")

a = 1973
b = 1989

patents_AI_specific_1st <- patents_AI_specific_1st[patents_AI_specific_1st$priority_year < b,]
patents_AI_specific_1st <- patents_AI_specific_1st[patents_AI_specific_1st$priority_year > a,]

length(unique(patents_AI_specific_1st$appln_id)) #436
patents_AI_specific_1st <- patents_AI_specific_1st[is.na(patents_AI_specific_1st$appln_id)==F,]

mat_tech_AI <- create_sparse_matrix(i = patents_AI_specific_1st %>% pull(appln_id),
                                    j = patents_AI_specific_1st %>% pull(techn_field_nr))

mat_tech_AI %<>%   crossprod() %>%   as.matrix() 

mat_tech_rel_AI <- mat_tech_AI %>%   relatedness(method = "cosine")

IPC_names <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/Specializations_All_periods_IPC.csv", sep = ";", header = TRUE)%>%
  select(techn_field_nr, sector, field_name, Category) %>%  distinct(techn_field_nr, .keep_all = TRUE) %>%
  mutate(techn_field_nr = techn_field_nr) %>%  arrange(techn_field_nr)

g_tech_AI <- mat_tech_rel_AI %>% as_tbl_graph(directed = FALSE) %N>%
  left_join(IPC_names %>% mutate(techn_field_nr = techn_field_nr %>% as.character()), by = c("name" = "techn_field_nr")) %>%
  mutate(dgr = centrality_eigen(weights = weight)) %E>%  filter(weight >= mean(weight))

#Create the Coordinates
coords_tech_AI <- g_tech_AI %>% igraph::layout.fruchterman.reingold() %>% as_tibble()
colnames(coords_tech_AI) <- c("x", "y")
#let's take a look at the most and less complex IPC fields:
g_tech_AI %N>%   arrange(desc(dgr)) %>%  as_tibble() %>%  slice(1:10)

g_tech_AI %N>%   arrange(dgr) %>%  as_tibble() %>%  slice(1:10)

AI_RCA <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/Specializations_All_periods_IPC.csv", sep = ";", header = TRUE, dec=",")
AI_RCA$Period_sim <- as.numeric(factor(AI_RCA$Period,levels=unique(AI_RCA$Period)))
AI_RCA <- AI_RCA[,c(2,9,13)]
AI_RCA$techn_field_nr <- as.character(AI_RCA$techn_field_nr)
names(AI_RCA) <- c("techn_field_nr", "RCA_AI_Period", "Period_sim")
AI_RCA$Binary <- ifelse(AI_RCA$RCA_AI_Period < 1, 0,1)

g_tech_AI %>%  ggraph(layout =  coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.4, colour = "grey") + 
  geom_node_point(aes(fill = sector, size = dgr, shape= sector))+ 
  scale_shape_manual(values=c(21, 22, 23, 24, 25)) + scale_size(range = c(2, 10)) +
  geom_node_text(aes(label = field_name), size = 4, repel = TRUE) + 
  theme_graph(base_family = "sans")+  ggtitle("Technology Space: IPC codes") + 
  theme(legend.title = element_text(size = 14),legend.text = element_text(size = 10)) + 
  guides(colour = guide_legend(override.aes = list(size=10)))+
  geom_mark_hull(aes(x = x, y=y, colour = sector, fill= sector,
                     linetype = sector), alpha = 0.15, expand = unit(2.5, "mm"), size = 1) 

AI_RCA1 <- AI_RCA[AI_RCA$Period_sim == 1,]
#period = 1
p=1

AI_dgr_1st <- 
  g_tech_AI %N>%
  left_join(AI_RCA1 %>% filter(Period_sim == p), by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "#CCCCCC") +
  geom_node_point(aes(fill = sector, size = 1000^dgr, shape= sector)) +
  scale_shape_manual(values=c(21, 22, 23, 24, 25)) + labs(color   = "RCA")+ scale_size("Degree", range = c(2, 12)) +
  geom_node_text(aes(filter=Binary > .99, label = field_name), size = 5, repel = TRUE) + #filter=Binary > .99, 
  theme_graph(base_family = "sans") +
  ggtitle("AI-specific technological space (1974-1988)") #

jpeg("Files_created_with_the_code/figures/extra/Figure_2_Example_ATS_and_AI_core_technologies_1stInterval.jpg", width = 14, height = 10, units = 'in', res = 300)
AI_dgr_1st 
dev.off()

#save a new network just to check all the names later:
AI_RCA_1st_allnames <-
  g_tech_AI %N>%
  left_join(AI_RCA1, by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "#CCCCCC") +
  geom_node_point(aes(fill = sector, size = RCA_AI_Period, shape= sector)) +
  scale_shape_manual(values=c(21, 22, 23, 24, 25)) + labs(color   = "RCA")+ scale_size(range = c(1, 12)) +
  geom_node_text(aes(label = field_name), size = 5, repel = TRUE) + #filter=Binary > .99, 
  theme_graph(base_family = "sans") +
  ggtitle("AI-specific technological space (1974-1988)") #

jpeg("Files_created_with_the_code/figures/extra/AI_RCA_1st_allnames.jpg", width = 14, height = 10, units = 'in', res = 300)
AI_RCA_1st_allnames 
dev.off()
rm(AI_RCA_1st_allnames, patents_AI_specific_1st, IPC_all_patents_Part1, IPC_all_patents_complete, AI_RCA1, coords_tech_AI, mat_tech_AI, mat_tech_rel_AI, g_tech_AI)

# ATS Second interval
patents_AI_specific_2nd <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/AI_ALL_patents.csv", sep = ";", header = TRUE, dec=",")

a = 1988
b = 2004

patents_AI_specific_2nd <- patents_AI_specific_2nd[patents_AI_specific_2nd$priority_year < b,]
patents_AI_specific_2nd <- patents_AI_specific_2nd[patents_AI_specific_2nd$priority_year > a,]

length(unique(patents_AI_specific_2nd$appln_id)) #7888
patents_AI_specific_2nd <- patents_AI_specific_2nd[is.na(patents_AI_specific_2nd$appln_id)==F,]

mat_tech_AI <- create_sparse_matrix(i = patents_AI_specific_2nd %>% pull(appln_id),
                                    j = patents_AI_specific_2nd %>% pull(techn_field_nr))

mat_tech_AI %<>%   crossprod() %>%   as.matrix() 

mat_tech_rel_AI <- mat_tech_AI %>%   relatedness(method = "cosine")

g_tech_AI <- mat_tech_rel_AI %>% as_tbl_graph(directed = FALSE) %N>%
  left_join(IPC_names %>% mutate(techn_field_nr = techn_field_nr %>% as.character()), by = c("name" = "techn_field_nr")) %>%
  mutate(dgr = centrality_eigen(weights = weight)) %E>%  filter(weight >= mean(weight))

#Create the Coordinates
coords_tech_AI <- g_tech_AI %>% igraph::layout.fruchterman.reingold() %>% as_tibble()
colnames(coords_tech_AI) <- c("x", "y")

AI_RCA2 <- AI_RCA[AI_RCA$Period_sim == 2,]
#period = 2
p=2

AI_dgr_2nd <- 
  g_tech_AI %N>%
  left_join(AI_RCA2, by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "#CCCCCC") +
  geom_node_point(aes(fill = sector, size = 1000^dgr, shape= sector)) +
  scale_shape_manual(values=c(21, 22, 23, 24, 25)) + scale_size("Degree",range = c(2, 12)) +
  geom_node_text(aes(filter=Binary > .99, label = field_name), size = 5, repel = TRUE) + 
  theme_graph(base_family = "sans") +
  ggtitle("AI-specific technological space (1989-2003)") #

#save a new network just to check all the names later:
AI_RCA_2nd_allnames <-
  g_tech_AI %N>%
  left_join(AI_RCA2, by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "#CCCCCC") +
  geom_node_point(aes(fill = sector, size = RCA_AI_Period, shape= sector)) +
  scale_shape_manual(values=c(21, 22, 23, 24, 25)) + labs(color   = "RCA")+ scale_size(range = c(1, 12)) +
  geom_node_text(aes(label = field_name), size = 5, repel = TRUE) + #filter=Binary > .99, 
  theme_graph(base_family = "sans") +
  ggtitle("AI-specific technological space (1974-1988)") #
jpeg("Files_created_with_the_code/figures/extra/AI_RCA_2nd_allnames.jpg", width = 14, height = 10, units = 'in', res = 300)
AI_RCA_2nd_allnames 
dev.off()
rm(AI_RCA_2nd_allnames, patents_AI_specific_2nd, AI_RCA2, coords_tech_AI, mat_tech_AI, mat_tech_rel_AI, g_tech_AI)

# ATS Third interval
patents_AI_specific_3rd <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/AI_ALL_patents.csv", sep = ";", header = TRUE, dec=",")

a = 2003
b = 2019

patents_AI_specific_3rd <- patents_AI_specific_3rd[patents_AI_specific_3rd$priority_year < b,]
patents_AI_specific_3rd <- patents_AI_specific_3rd[patents_AI_specific_3rd$priority_year > a,]

length(unique(patents_AI_specific_3rd$appln_id)) #34576
patents_AI_specific_3rd <- patents_AI_specific_3rd[is.na(patents_AI_specific_3rd$appln_id)==F,]

mat_tech_AI <- create_sparse_matrix(i = patents_AI_specific_3rd %>% pull(appln_id),
                                    j = patents_AI_specific_3rd %>% pull(techn_field_nr))

mat_tech_AI %<>%   crossprod() %>%   as.matrix() 

mat_tech_rel_AI <- mat_tech_AI %>%  relatedness(method = "cosine")

g_tech_AI <- mat_tech_rel_AI %>% as_tbl_graph(directed = FALSE) %N>%
  left_join(IPC_names %>% mutate(techn_field_nr = techn_field_nr %>% as.character()), by = c("name" = "techn_field_nr")) %>%
  mutate(dgr = centrality_eigen(weights = weight)) %E>%  filter(weight >= mean(weight))

#Create the Coordinates
coords_tech_AI <- g_tech_AI %>% igraph::layout.fruchterman.reingold() %>% as_tibble()
colnames(coords_tech_AI) <- c("x", "y")

#period = 3
p=3
AI_RCA3 <- AI_RCA[AI_RCA$Period_sim == 3,]

AI_dgr_3rd <- 
  g_tech_AI %N>%
  left_join(AI_RCA3, by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "#CCCCCC") +
  geom_node_point(aes(fill = sector, size = 1000^dgr, shape= sector)) +
  scale_shape_manual(values=c(21, 22, 23, 24, 25)) + scale_size("Degree", range = c(2, 12)) +
  geom_node_text(aes(filter=Binary > .99, label = field_name), size = 5, repel = TRUE) + 
  theme_graph(base_family = "sans") + 
  ggtitle("AI-specific technological space (2004-2018)") #

#save a new network just to check all the names later:
AI_RCA_3rd_allnames <-
  g_tech_AI %N>%
  left_join(AI_RCA3, by = c("name" = "techn_field_nr")) %>%
  ggraph(layout = coords_tech_AI) + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "#CCCCCC") +
  geom_node_point(aes(fill = sector, size = RCA_AI_Period, shape= sector)) +
  scale_shape_manual(values=c(21, 22, 23, 24, 25)) + labs(color   = "RCA")+ scale_size(range = c(1, 12)) +
  geom_node_text(aes(label = field_name), size = 5, repel = TRUE) + #filter=Binary > .99, 
  theme_graph(base_family = "sans") +
  ggtitle("AI-specific technological space (1974-1988)") #
jpeg("Files_created_with_the_code/figures/extra/AI_RCA_3rd_allnames.jpg", width = 14, height = 10, units = 'in', res = 300)
AI_RCA_3rd_allnames 
dev.off()

rm(AI_RCA_3rd_allnames, patents_AI_specific_3rd, AI_RCA3, coords_tech_AI, mat_tech_AI, mat_tech_rel_AI, g_tech_AI, AI_dgr_3rd_1)

#create a figure with all networks together
jpeg("Files_created_with_the_code/figures/Figure_2_ATS_and_AI_core_technologies_3_intervals.jpg", width = 10, height = 20, units = 'in', res = 300)
multiplot(AI_dgr_1st, AI_dgr_2nd, AI_dgr_3rd, cols=1) 
dev.off()

#2.SECOND PART: Specialisations based on subclasses (4-digits) -----
##2.1. Calculate specialisations per interval for countries and AI ------
# Remove all non-function objects in the global environment and clean memory
rm(list = ls()[!sapply(ls(), function(x) is.function(get(x)))])
gc()

###2.1.1. Subclasses First interval (1974-1988) ----
# General Perspective First interval  
#For the first interval, which goes from 1974 to 1988, we need only the dataset from Part2:
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

#we want to pick only the registers from the interval we want (from 1974 to 1988, including both cited years)
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

#now we apply the 2 functions we created at the beginning of this section:
reg_tech1 <- group_by_applnID(IPC_all_patents_FirstPeriod)
rm(IPC_all_patents_FirstPeriod)
reg_tech1 <- group_by_ctry_and_subclass(reg_tech1)

mat_reg_tech1 <- reg_tech1 %>%  arrange(Subclass, ctry_code) %>%
  pivot_wider(names_from = Subclass, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

mat_reg_tech1 %<>% remove_rownames %>% column_to_rownames(var="ctry_code") %>%
  as.matrix() %>%  round()

reg_RCA1 <- mat_reg_tech1 %>% location_quotient(binary = F) %>%   as.data.frame() %>% 
  rownames_to_column("ctry_code") %>%   as_tibble() %>% 
  gather(key = "Subclass", value = "RCA", -ctry_code) %>%  arrange(ctry_code, Subclass)

# AI perspective First interval (1974-1988)
#For the first interval, which goes from 1974 to 1988, we need only the dataset from Part2:
patents_AI_specific_1st <- read.csv("other_files/IPCs_AI.csv", sep = ";", header = TRUE, dec=",")

a = 1973
b = 1989

patents_AI_specific_1st <- patents_AI_specific_1st[patents_AI_specific_1st$priority_year < b,]
patents_AI_specific_1st <- patents_AI_specific_1st[patents_AI_specific_1st$priority_year > a,]

#now we pick just the subclass for analysis:
patents_AI_specific_1st$Subclass <- substr(patents_AI_specific_1st$ipc_class_symbol,1,4)

#and apply the 2 functions we created at the beginning of this code:
reg_tech1 <- group_by_applnID(patents_AI_specific_1st)
rm(patents_AI_specific_1st)
reg_tech1 <- group_by_ctry_and_subclass(reg_tech1)

mat_reg_tech1 <- reg_tech1 %>%  arrange(Subclass, ctry_code) %>%
  pivot_wider(names_from = Subclass, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

mat_reg_tech1 %<>% remove_rownames %>% column_to_rownames(var="ctry_code") %>%
  as.matrix() %>%  round()

reg_RCA1_AIspecific <- mat_reg_tech1 %>% location_quotient(binary = F) %>%   as.data.frame() %>% 
  rownames_to_column("ctry_code") %>%   as_tibble() %>% 
  gather(key = "Subclass", value = "RCA", -ctry_code) %>%  arrange(ctry_code, Subclass)

Data1period <- merge(reg_RCA1, reg_RCA1_AIspecific, all=T, by=c("ctry_code", "Subclass"))
Data1period$Period <- "1974-1988"
names(Data1period) <- c("ctry_code", "Subclass", "RCA_Gen", "RCA_AI", "Period")
write.csv2(Data1period, file = "Files_created_with_the_code/data/files_code_4-digits_analysis/Data1period_RCA_subclass.csv", row.names = F)

rm(mat_reg_tech1)
###2.1.2. Subclasses Second interval (1989-2003) ----
# General Perspective Second interval 
#For the second interval, which goes from 1989 to 2003, we need only the dataset from Part2:
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

#we want to pick only the registers from the interval we want (from 1989 to 2003, including both cited years)
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

#let's drop the column we won't use (priority_year):
IPC_all_patents_Part1 <- IPC_all_patents_Part1[, c((-4))]
IPC_all_patents_Part2 <- IPC_all_patents_Part2[, c((-4))]
IPC_all_patents_Part3 <- IPC_all_patents_Part3[, c((-4))]
IPC_all_patents_Part4 <- IPC_all_patents_Part4[, c((-4))]
IPC_all_patents_Part5 <- IPC_all_patents_Part5[, c((-4))]

#we combine the 5 files:
IPC_all_patents_SecondPeriod <- rbind(IPC_all_patents_Part1, IPC_all_patents_Part2, IPC_all_patents_Part3, IPC_all_patents_Part4, IPC_all_patents_Part5)
#and exclude the 5 big ones we just used, so we have back our memory:
rm(IPC_all_patents_Part1, IPC_all_patents_Part2, IPC_all_patents_Part3, IPC_all_patents_Part4, IPC_all_patents_Part5)

length(unique(IPC_all_patents_SecondPeriod$appln_id))#8,852,648 unique publication numbers (i.e., unique priorities) out of
length(IPC_all_patents_SecondPeriod$appln_id) #63,620,929 lines of data

#we pick just the subclass for analysis:
IPC_all_patents_SecondPeriod$Subclass <- substr(IPC_all_patents_SecondPeriod$ipc_class_symbol,1,4)

#now we apply the 2 functions we created at the beginning of this section:
reg_tech2 <- group_by_applnID(IPC_all_patents_SecondPeriod)
rm(IPC_all_patents_SecondPeriod)
reg_tech2 <- group_by_ctry_and_subclass(reg_tech2)

mat_reg_tech2 <- reg_tech2 %>%  arrange(Subclass, ctry_code) %>%
  pivot_wider(names_from = Subclass, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

mat_reg_tech2 %<>% remove_rownames %>% column_to_rownames(var="ctry_code") %>%
  as.matrix() %>%  round()

reg_RCA2 <- mat_reg_tech2 %>% location_quotient(binary = F) %>%   as.data.frame() %>% 
  rownames_to_column("ctry_code") %>%   as_tibble() %>% 
  gather(key = "Subclass", value = "RCA", -ctry_code) %>%  arrange(ctry_code, Subclass)

# AI Perspective Second interval (1989-2003)
patents_AI_specific_2nd <- read.csv("other_files/IPCs_AI.csv", sep = ";", header = TRUE, dec=",")

a = 1988
b = 2004

patents_AI_specific_2nd <- patents_AI_specific_2nd[patents_AI_specific_2nd$priority_year < b,]
patents_AI_specific_2nd <- patents_AI_specific_2nd[patents_AI_specific_2nd$priority_year > a,]

#now we pick just the subclass for analysis:
patents_AI_specific_2nd$Subclass <- substr(patents_AI_specific_2nd$ipc_class_symbol,1,4)

#and apply the 2 functions we created at the beginning of this code:
reg_tech2 <- group_by_applnID(patents_AI_specific_2nd)
rm(patents_AI_specific_2nd)
reg_tech2 <- group_by_ctry_and_subclass(reg_tech2)

mat_reg_tech2 <- reg_tech2 %>%  arrange(Subclass, ctry_code) %>%
  pivot_wider(names_from = Subclass, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

mat_reg_tech2 %<>% remove_rownames %>% column_to_rownames(var="ctry_code") %>%
  as.matrix() %>%  round()

reg_RCA2_AIspecific <- mat_reg_tech2 %>% location_quotient(binary = F) %>% 
  as.data.frame() %>%   rownames_to_column("ctry_code") %>% 
  as_tibble() %>%   gather(key = "Subclass", value = "RCA", -ctry_code) %>%  arrange(ctry_code, Subclass)

Data2period <- merge(reg_RCA2, reg_RCA2_AIspecific, all=T, by=c("ctry_code", "Subclass"))
Data2period$Period <- "1989-2003"
names(Data2period) <- c("ctry_code", "Subclass", "RCA_Gen", "RCA_AI", "Period")
write.csv2(Data2period, file = "Files_created_with_the_code/data/files_code_4-digits_analysis/Data2period_RCA_subclass.csv", row.names = F)
rm(mat_reg_tech2)

###2.1.3. Subclasses Third interval (2004-2018) ----
# General Perspective Third interval 
#For the third interval, which goes from 2004 to 2018, we  need only the dataset from Part1. This
#specific dataset only has patents from 2004 to 2018, so we don't have to filter it. But calculating
#the reg_tech is very computationally expansive, so we divide that into 6 parts.
c <- 120419184-100000000
IPC_all_patents_Part1 <- fread("large_files/All_patents_and_IPC_codes_Part1.csv", header = F, nrow = 20000000)
IPC_all_patents_Part2 <- fread("large_files/All_patents_and_IPC_codes_Part1.csv", header = F, nrow = 20000000, skip = 20000000)
IPC_all_patents_Part3 <- fread("large_files/All_patents_and_IPC_codes_Part1.csv", header = F, nrow = 20000000, skip = 40000000)

names(IPC_all_patents_Part1) <- c("appln_id", "ctry_code", "ipc_class_symbol", "priority_year")
names(IPC_all_patents_Part2) <- c("appln_id", "ctry_code", "ipc_class_symbol", "priority_year")
names(IPC_all_patents_Part3) <- c("appln_id", "ctry_code", "ipc_class_symbol", "priority_year")

IPC_all_patents_Part1 <- IPC_all_patents_Part1[, c((-4))]
IPC_all_patents_Part2 <- IPC_all_patents_Part2[, c((-4))]
IPC_all_patents_Part3 <- IPC_all_patents_Part3[, c((-4))]

#just check how many patents we have on this first part of the third interval
IPC_all_patents_ThirdPeriodTemporaryFirstPart <- rbind(IPC_all_patents_Part1, IPC_all_patents_Part2, IPC_all_patents_Part3)
length(unique(IPC_all_patents_ThirdPeriodTemporaryFirstPart$appln_id))#8,131,440 unique publication numbers in the first part 
length(IPC_all_patents_ThirdPeriodTemporaryFirstPart$appln_id) #out of 60,000,000 lines of data
rm(IPC_all_patents_ThirdPeriodTemporaryFirstPart)

IPC_all_patents_Part1$Subclass <- substr(IPC_all_patents_Part1$ipc_class_symbol,1,4)
IPC_all_patents_Part2$Subclass <- substr(IPC_all_patents_Part2$ipc_class_symbol,1,4)
IPC_all_patents_Part3$Subclass <- substr(IPC_all_patents_Part3$ipc_class_symbol,1,4)

#here we divide our calculations of the reg_tech (which was not necessary on the 2 previous intervals)
reg_tech4 <- group_by_applnID(IPC_all_patents_Part1)
rm(IPC_all_patents_Part1)
reg_tech4 <- group_by_ctry_and_subclass(reg_tech4)

reg_tech5 <- group_by_applnID(IPC_all_patents_Part2)
rm(IPC_all_patents_Part2)
reg_tech5 <- group_by_ctry_and_subclass(reg_tech5)

reg_tech6 <- group_by_applnID(IPC_all_patents_Part3)
rm(IPC_all_patents_Part3)
reg_tech6 <- group_by_ctry_and_subclass(reg_tech6)

#now we merge them
reg_tech3 <- merge(reg_tech4, reg_tech5, all=T, by=c("ctry_code", "Subclass"))
reg_tech3 <- merge(reg_tech3, reg_tech6, all=T, by=c("ctry_code", "Subclass"))
rm(reg_tech4, reg_tech5, reg_tech6)
#second part:
IPC_all_patents_Part4 <- fread("large_files/All_patents_and_IPC_codes_Part1.csv", header = F, nrow = 20000000, skip = 60000000)
IPC_all_patents_Part5 <- fread("large_files/All_patents_and_IPC_codes_Part1.csv", header = F, nrow = 20000000, skip = 80000000)
IPC_all_patents_Part6 <- fread("large_files/All_patents_and_IPC_codes_Part1.csv", header = F, nrow = c, skip = 100000000)

names(IPC_all_patents_Part4) <- c("appln_id", "ctry_code", "ipc_class_symbol", "priority_year")
names(IPC_all_patents_Part5) <- c("appln_id", "ctry_code", "ipc_class_symbol", "priority_year")
names(IPC_all_patents_Part6) <- c("appln_id", "ctry_code", "ipc_class_symbol", "priority_year")

IPC_all_patents_Part4 <- IPC_all_patents_Part4[, c((-4))]
IPC_all_patents_Part5 <- IPC_all_patents_Part5[, c((-4))]
IPC_all_patents_Part6 <- IPC_all_patents_Part6[, c((-4))]

#just check how many patents we have on this second part of the third interval
IPC_all_patents_ThirdPeriodTemporarySecondPart <- rbind(IPC_all_patents_Part4, IPC_all_patents_Part5, IPC_all_patents_Part6)
length(unique(IPC_all_patents_ThirdPeriodTemporarySecondPart$appln_id))#8,195,913 unique publication numbers in the first part 
length(IPC_all_patents_ThirdPeriodTemporarySecondPart$appln_id) #out of 60,419,184 lines of data
rm(IPC_all_patents_ThirdPeriodTemporarySecondPart)

IPC_all_patents_Part4$Subclass <- substr(IPC_all_patents_Part4$ipc_class_symbol,1,4)
IPC_all_patents_Part5$Subclass <- substr(IPC_all_patents_Part5$ipc_class_symbol,1,4)
IPC_all_patents_Part6$Subclass <- substr(IPC_all_patents_Part6$ipc_class_symbol,1,4)

reg_tech7 <- group_by_applnID(IPC_all_patents_Part4)
rm(IPC_all_patents_Part4)
reg_tech7 <- group_by_ctry_and_subclass(reg_tech7)

reg_tech8 <- group_by_applnID(IPC_all_patents_Part5)
rm(IPC_all_patents_Part5)
reg_tech8 <- group_by_ctry_and_subclass(reg_tech8)

reg_tech9 <- group_by_applnID(IPC_all_patents_Part6)
rm(IPC_all_patents_Part6)
reg_tech9 <- group_by_ctry_and_subclass(reg_tech9)

reg_tech4 <- merge(reg_tech7, reg_tech8, all=T, by=c("ctry_code", "Subclass"))
reg_tech4 <- merge(reg_tech4, reg_tech9, all=T, by=c("ctry_code", "Subclass"))
rm(reg_tech7, reg_tech8, reg_tech9)

#replace NAs, so we don't have problems when summing:
reg_tech3[is.na(reg_tech3)] <- 0
reg_tech4[is.na(reg_tech4)] <- 0

#do the summ, exclude the tables used, and rename the dataset accordingly:
reg_tech3$sum <- rowSums(reg_tech3[,c(3:5)])
reg_tech3 <- reg_tech3[, c((-3), (-4), (-5))]
names(reg_tech3) <- c("ctry_code", "Subclass", "n_tech_reg")

reg_tech4$sum <- rowSums(reg_tech4[,c(3:5)])
reg_tech4 <- reg_tech4[, c((-3), (-4), (-5))]
names(reg_tech4) <- c("ctry_code", "Subclass", "n_tech_reg")

reg_tech5 <- merge(reg_tech3, reg_tech4, all=T, by=c("ctry_code", "Subclass"))
reg_tech5[is.na(reg_tech5)] <- 0
reg_tech5$sum <- rowSums(reg_tech5[,c(3:4)])
reg_tech5 <- reg_tech5[, c((-3), (-4))]
names(reg_tech5) <- c("ctry_code", "Subclass", "n_tech_reg")

mat_reg_tech3 <- reg_tech5 %>%   arrange(Subclass, ctry_code) %>%
  pivot_wider(names_from = Subclass, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

mat_reg_tech3 %<>% remove_rownames %>% column_to_rownames(var="ctry_code") %>%
  as.matrix() %>%  round()

reg_RCA3 <- mat_reg_tech3 %>% location_quotient(binary = F) %>%   as.data.frame() %>% 
  rownames_to_column("ctry_code") %>%   as_tibble() %>% 
  gather(key = "Subclass", value = "RCA", -ctry_code) %>%  arrange(ctry_code, Subclass)

# AI Perspective Third interval (2004-2018)
patents_AI_specific_3rd <- read.csv("other_files/IPCs_AI.csv", sep = ";", header = TRUE, dec=",")

a = 2003
b = 2019

patents_AI_specific_3rd <- patents_AI_specific_3rd[patents_AI_specific_3rd$priority_year < b,]
patents_AI_specific_3rd <- patents_AI_specific_3rd[patents_AI_specific_3rd$priority_year > a,]

#now we pick just the subclass for analysis:
patents_AI_specific_3rd$Subclass <- substr(patents_AI_specific_3rd$ipc_class_symbol,1,4)

#let's drop the columns we won't use:
patents_AI_specific_3rd <- patents_AI_specific_3rd[, c((-2), (-3), (-6), (-7), (-8), (-10))]

#now we apply the 2 functions we created at the beginning of this section:
reg_tech3 <- group_by_applnID(patents_AI_specific_3rd)
rm(patents_AI_specific_3rd)
reg_tech3 <- group_by_ctry_and_subclass(reg_tech3)

mat_reg_tech3 <- reg_tech3 %>%  arrange(Subclass, ctry_code) %>%
  pivot_wider(names_from = Subclass, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

mat_reg_tech3 %<>% remove_rownames %>% column_to_rownames(var="ctry_code") %>%
  as.matrix() %>%  round()

reg_RCA3_AIspecific <- mat_reg_tech3 %>% location_quotient(binary = F) %>% 
  as.data.frame() %>%   rownames_to_column("ctry_code") %>% 
  as_tibble() %>%   gather(key = "Subclass", value = "RCA", -ctry_code) %>%  arrange(ctry_code, Subclass)

Data3period <- merge(reg_RCA3, reg_RCA3_AIspecific, all=T, by=c("ctry_code", "Subclass"))
Data3period$Period <- "2004-2018"
names(Data3period) <- c("ctry_code", "Subclass", "RCA_Gen", "RCA_AI", "Period")
write.csv2(Data3period, file = "Files_created_with_the_code/data/files_code_4-digits_analysis/Data3period_RCA_subclass.csv", row.names = F)

IPC_RCAs <- rbind(Data1period, Data2period, Data3period)
write.csv2(IPC_RCAs, file = "Files_created_with_the_code/data/files_code_4-digits_analysis/IPC_RCAs_subclass.csv", row.names = F)

#3.THIRD PART: Remaining figures ------
##3.1. Overlapping specializations (Fig 6 and 7)----
##3.1.1. Technological fields 
# Remove all non-function objects in the global environment and clean memory
rm(list = ls()[!sapply(ls(), function(x) is.function(get(x)))])
gc()

IPC_RCAs_Top4 <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/RCA_4countries_detailed.csv", sep = ";", header = TRUE, dec=",")
IPC_RCAs_Top4$Total_RCA <- as.factor(IPC_RCAs_Top4$Total_RCA)
IPC_RCAs_Top4$Period_sim <- as.numeric(factor(IPC_RCAs_Top4$Period,levels=unique(IPC_RCAs_Top4$Period)))
IPC_RCAs_Top4$techn_field_nr <- as.character(IPC_RCAs_Top4$techn_field_nr)

#replace names:
IPC_RCAs_Top4$ctry_code <- gsub("US", "USA", str_trim(IPC_RCAs_Top4$ctry_code))
IPC_RCAs_Top4$ctry_code <- gsub("CN", "China", str_trim(IPC_RCAs_Top4$ctry_code))
IPC_RCAs_Top4$ctry_code <- gsub("JP", "Japan", str_trim(IPC_RCAs_Top4$ctry_code))
IPC_RCAs_Top4$ctry_code <- gsub("KR", "South Korea", str_trim(IPC_RCAs_Top4$ctry_code))

AI_RCA <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/Specializations_All_periods_IPC.csv", sep = ";", header = TRUE, dec=",")
AI_RCA$Period_sim <- as.numeric(factor(AI_RCA$Period,levels=unique(AI_RCA$Period)))
AI_RCA <- AI_RCA[,c(2,9,13)]
AI_RCA$techn_field_nr <- as.character(AI_RCA$techn_field_nr)
names(AI_RCA) <- c("techn_field_nr", "RCA_AI_Period", "Period_sim")
IPC_RCAs_Top4 <- left_join(IPC_RCAs_Top4, AI_RCA, by = c("techn_field_nr", "Period_sim"))

#fix Total_RCA:
IPC_RCAs_Top4$Total_RCA_2 <- IPC_RCAs_Top4$Round_general + 2*IPC_RCAs_Top4$Round_AI
rm(AI_RCA)

IPC_RCAs_Top4$Coiciding <- ifelse(IPC_RCAs_Top4$Total_RCA_2 ==3,1,0)
IPC_RCAs_Top4$justGeneral <- ifelse(IPC_RCAs_Top4$Total_RCA_2 ==1,1,0)
IPC_RCAs_Top4$OnlyAI <- ifelse(IPC_RCAs_Top4$Total_RCA_2 ==2,1,0)

#now, create a file per country per interval, where I sum over the 3 columns;
IPC_RCAs <- IPC_RCAs_Top4
IPC_RCAs %<>% 
  group_by(ctry_code,Period) %>%   mutate(Share_coinciding = sum(Coiciding)/(sum(Coiciding)+sum(justGeneral))) %>% 
  mutate(Share_OnlyAI = sum(OnlyAI)/(sum(OnlyAI)+sum(Coiciding))) %>% 
  mutate(sum_coinciding = sum(Coiciding)) %>%   mutate(sum_justGeneral = sum(justGeneral)) %>%
  mutate(sum_OnlyAI = sum(OnlyAI)) %>%  ungroup()

SummaryAllData<-distinct(IPC_RCAs, ctry_code, Period, .keep_all = TRUE) 
colnames(SummaryAllData)[1] <- "Country"

OverlapTechn<-
  ggplot(data=SummaryAllData, aes(x=Period, y=Share_coinciding, group=Country, shape = Country, color=Country)) +
  geom_point(aes(fill = Country), size=8) +   scale_shape_manual(values=c(21, 22, 24, 23)) +
  xlab("Interval") +  ylab("Share of break-in specialisations (%)") +
  theme_classic() +  geom_line(aes(color=Country), linetype = "dashed", size=1.5)+
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A")) +
  scale_color_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A"))  #"#99CC00", "#66CC33", "#336600", "#66FF66"


jpeg("Files_created_with_the_code/figures/Figure_6_Share_coinciding_specialisations_techn_field.jpg", width = 7, height = 4, units = 'in', res = 300)
OverlapTechn 
dev.off()

#Subclasses (4-digits)
IPC_RCAs <- read.csv("Files_created_with_the_code/data/files_code_4-digits_analysis/IPC_RCAs_subclass.csv", sep = ";", header = TRUE, dec=",")
#Select the 4 countries we want
IPC_RCAs_Top4 <- IPC_RCAs[IPC_RCAs$ctry_code == "CN" | IPC_RCAs$ctry_code == "KR"| 
                            IPC_RCAs$ctry_code == "US"|IPC_RCAs$ctry_code == "JP", ]
rm(IPC_RCAs)

#replace names:
IPC_RCAs_Top4$ctry_code <- gsub("US", "USA", str_trim(IPC_RCAs_Top4$ctry_code))
IPC_RCAs_Top4$ctry_code <- gsub("CN", "China", str_trim(IPC_RCAs_Top4$ctry_code))
IPC_RCAs_Top4$ctry_code <- gsub("JP", "Japan", str_trim(IPC_RCAs_Top4$ctry_code))
IPC_RCAs_Top4$ctry_code <- gsub("KR", "South Korea", str_trim(IPC_RCAs_Top4$ctry_code))

IPC_RCAs_Top4$Period_sim <- as.numeric(factor(IPC_RCAs_Top4$Period,levels=unique(IPC_RCAs_Top4$Period)))

#replace NAs by 0:
#replace NAs, so we don't have problems when summing:
IPC_RCAs_Top4[is.na(IPC_RCAs_Top4)] <- 0

#make the numbers binary
IPC_RCAs_Top4$RCA_Gen2 <- ifelse(IPC_RCAs_Top4$RCA_Gen >=1,1,0)
IPC_RCAs_Top4$RCA_AI2 <- ifelse(IPC_RCAs_Top4$RCA_AI >=1,1,0)

#fix Total_RCA:
IPC_RCAs_Top4$Total_RCA_2 <- IPC_RCAs_Top4$RCA_Gen2 + 2*IPC_RCAs_Top4$RCA_AI2

IPC_RCAs_Top4$Coiciding <- ifelse(IPC_RCAs_Top4$Total_RCA_2 ==3,1,0)
IPC_RCAs_Top4$justGeneral <- ifelse(IPC_RCAs_Top4$Total_RCA_2 ==1,1,0)
IPC_RCAs_Top4$OnlyAI <- ifelse(IPC_RCAs_Top4$Total_RCA_2 ==2,1,0)

#now, create a file per country per interval, where I sum over the 3 columns;
IPC_RCAs <- IPC_RCAs_Top4
IPC_RCAs %<>% 
  group_by(ctry_code,Period) %>% 
  mutate(Share_coinciding = sum(Coiciding)/(sum(Coiciding)+sum(justGeneral))) %>% 
  mutate(Share_OnlyAI = sum(OnlyAI)/(sum(OnlyAI)+sum(Coiciding))) %>% 
  mutate(sum_coinciding = sum(Coiciding)) %>% 
  mutate(sum_justGeneral = sum(justGeneral)) %>%
  mutate(sum_OnlyAI = sum(OnlyAI)) %>%
  ungroup()

SummaryAllData4dig<-distinct(IPC_RCAs, ctry_code, Period, .keep_all = TRUE) 
colnames(SummaryAllData4dig)[1] <- "Country"

OverlapTechn2<-
  ggplot(data=SummaryAllData4dig, aes(x=Period, y=Share_coinciding, group=Country, shape = Country, color=Country)) +
  geom_point(aes(fill = Country), size=8) + 
  scale_shape_manual(values=c(21, 22, 24, 23)) +
  xlab("Interval") +
  ylab("Share of break-in specialisations (%)") +
  theme_classic() +
  geom_line(aes(color=Country), linetype = "dashed", size=1.5)+
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A")) +
  scale_color_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A")) 

jpeg("Files_created_with_the_code/figures/Figure_7_Share_coinciding_specialisations_subclass.jpg", width = 8, height = 4, units = 'in', res = 300)
OverlapTechn2
dev.off()

##3.2. Figure number of AI patents per country (Figure 1) ------
patents_AI_specific <- read.csv("other_files/IPCs_AI.csv", sep = ";", header = TRUE, dec=",")
patents_AI_specific <- patents_AI_specific[,c((1), (3:4))]

patents_AI_specific %<>%  mutate(DistinctOwnerInf = !duplicated(appln_id)) %>%  ungroup()

patents_AI_specific %<>%   group_by(appln_id) %>% 
  mutate(DistinctpatentOffice = !duplicated(patent_office)) %>%  ungroup()

patents_AI_specific %<>%  group_by(appln_id) %>% 
  mutate(DistinctpatentOffice = n_distinct(patent_office, na.rm = T)) %>%  ungroup()

table(patents_AI_specific$DistinctpatentOffice)
test<- patents_AI_specific[1,]

test$patent_office <- gsub("CN", "US", str_trim(test$patent_office))

patents_AI_specific2 <- rbind(patents_AI_specific, test)

patents_AI_specific2 %<>% group_by(appln_id) %>% 
  mutate(DistinctpatentOffice = n_distinct(patent_office, na.rm = T)) %>% ungroup()

patents_AI_specific2[patents_AI_specific2$appln_id == "475222998",]
table(patents_AI_specific2$DistinctpatentOffice)
#thus, there is no patent with inventors from distinct patent offices in our dataset;

patents_AI_specific_simplified <- patents_AI_specific[patents_AI_specific$DistinctOwnerInf == T,]
patents_AI_specific_simplified2 <- patents_AI_specific2[patents_AI_specific2$DistinctOwnerInf == T,]

patents_AI_specific_simplified_4<- patents_AI_specific_simplified[patents_AI_specific_simplified$patent_office == "CN" |
                                                                    patents_AI_specific_simplified$patent_office == "US"|
                                                                    patents_AI_specific_simplified$patent_office == "KR"|
                                                                    patents_AI_specific_simplified$patent_office == "JP", ]

patents_AI_specific_simplified_4$patent_office <- gsub("US", "USA", str_trim(patents_AI_specific_simplified_4$patent_office))
patents_AI_specific_simplified_4$patent_office <- gsub("CN", "China", str_trim(patents_AI_specific_simplified_4$patent_office))
patents_AI_specific_simplified_4$patent_office <- gsub("JP", "Japan", str_trim(patents_AI_specific_simplified_4$patent_office))
patents_AI_specific_simplified_4$patent_office <- gsub("KR", "South Korea", str_trim(patents_AI_specific_simplified_4$patent_office))

table(patents_AI_specific_simplified_4$patent_office)
Data <- as.data.frame(table(patents_AI_specific_simplified_4$patent_office, patents_AI_specific_simplified_4$priority_year))
names(Data) <- c("Country", "Year", "Number_of_AI_patents")

Data$Year <- as.Date(paste(Data$Year, 1, 1, sep = "-")) # beginning of year
Data$Year <- as.Date(paste(Data$Year, 12, 31, sep = "-"))
Data$Year <- as.numeric(format(Data$Year, "%Y"))

Data$Period <- ifelse(Data$Year >= 1974 & Data$Year <= 1988, "First Period (1974-1988)",
                      ifelse(Data$Year > 1988 & Data$Year <= 2003, "Second Period (1989-2003)",
                             ifelse(Data$Year >= 2004 & Data$Year < 2019, "Third Period (2004-2018)", "No period")))
test <- Data[Data$Period != "No period",]  

Log_10_AI_patents_per_country <-
  ggplot(data=test, aes(x=Year, y=log10(Number_of_AI_patents), group=Country, colour=Country, shape=Country)) +
  geom_line(size=1.2, aes(linetype=Country)) +
  geom_point(size=8) +  xlab("Year") +  ylab("Number of new AI registers [Log10]") + theme_classic() +
  scale_linetype_manual(values=c("twodash", "longdash", "solid", "solid")) +
  scale_shape_manual(values=c(16, 15, 17, 18)) + theme(legend.position="bottom") +
  theme(text = element_text(size = 25)) +  scale_y_continuous(limits=c(0,4)) + 
  geom_vline(data=test, aes(xintercept=c(1988),  colour=Period), linetype="dashed", size=1, color = "grey") +  
  geom_vline(data=test, aes(xintercept=c(2003),  colour=Period), linetype="dashed", size=1, color = "grey") +  
  scale_x_continuous(breaks = c(1974, 1988, 2003, 2018), limits=c(1974, 2018)) + scale_color_brewer(palette="Dark2") + 
  annotate("rect", xmin = 1974, xmax=1988, ymin = 3.6, ymax = 4, alpha = .01, color = "black") +
  annotate("text", x = 1981, y = 3.8, label = c("First Period (1974-1988)"), size=7)+
  annotate("rect", xmin = 1988, xmax=2003, ymin = 3.6, ymax = 4, alpha = .01, color = "black") +
  annotate("text", x = 1996, y = 3.8, label = c("Second Period (1989-2003)"), size=7) +
  annotate("rect", xmin = 2003, xmax=2018, ymin = 3.6, ymax = 4, alpha = .01, color = "black") +
  annotate("text", x = 2011, y = 3.8, label = c("Third Period (2004-2018)"), size=7)

jpeg("Files_created_with_the_code/figures/Figure_1_Log_10_AI_patents_per_country.jpg", width = 14, height = 10, units = 'in', res = 800)
Log_10_AI_patents_per_country
dev.off()

##3.3.Figure 13------
rm(list=ls())

empty_theme <- theme(plot.background = element_blank(), panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(),
                     axis.line = element_blank(),  axis.ticks = element_blank(),  axis.text.y = element_text(angle = 90))

plot <- ggplot(NULL, aes()) +  coord_fixed() +  
  scale_x_continuous(expand = c(0, 0), limits = c(-1, 11), breaks = c(2,8), labels=c("2" = "", "8" = "")) +
  scale_y_continuous(expand = c(0, 0), limits = c(-1,11), breaks = c(2,8), labels=c("2" = "", "8" = "")) +
  empty_theme +  labs(title = NULL,x = "National Capability in AI-core Technologies", y = "Strength/Connectedness of Existing Industrial Sectors") +
  geom_segment(aes(x = 10, y = 0, xend = 10, yend = 10)) +  geom_segment(aes(x = 0, y = 0, xend = 0, yend = 10)) +
  geom_segment(aes(x = 0, y = 0, xend = 10, yend = 0)) +  geom_segment(aes(x = 0, y = 5, xend = 10, yend = 5)) +
  geom_segment(aes(x = 5, y = 0, xend = 5, yend = 10)) +  geom_segment(aes(x = 0, y = 10, xend = 10, yend = 10)) +
  annotate("text", x = 2.5, y = 4.5, alpha = 2, label = "Foundational Buildup", fontface = "bold") +
  annotate("text", x = 2.5, y = 9.5, alpha = 2, label = "Further Digitalisation", fontface = "bold") +
  annotate("text", x = 7.5, y = 4.5, alpha = 2, label = "Technology Push/Break-through Risk", fontface = "bold") +
  annotate("text", x = 7.5, y = 9.5, alpha = 2, label = "Synergistic Leadership", fontface = "bold") +
  
  annotate("text", x = 2.5, y = 3.5, alpha = 2, label = "\n \nCountries:\nDeveloping nations") +
  annotate("text", x = 7.5, y = 3.5, alpha = 2, label = "\n \nCountries:\nE.g., Strong in AI research,\nweaker industrial base") +
  annotate("text", x = 2.5, y = 8.5, alpha = 2, label = "\n \nCountries:\nE.g., European industrial leaders") +
  annotate("text", x = 7.5, y = 8.5, alpha = 2, label = "\n \nCountries:\nUSA, Japan, South Korea") +
  
  annotate("text", x = 2.5, y = 2.0, alpha = 2, label = "\n \nStrategy:\nFocus on foundational education, \ndigital infrastructure, and developing niche strengths.") +
  annotate("text", x = 7.5, y = 2.0, alpha = 2, label = "\n \nStrategy:\nBuild industrial absorptive capacity. Create \nnew markets to commercialise research.") +
  annotate("text", x = 2.5, y = 7.0, alpha = 2, label = "\n \nStrategy:\n1. Digitise hub industries.\n2. Foster targeted AI 'break-ins' to build capabilities.") +
  annotate("text", x = 7.5, y = 7.0, alpha = 2, label = "\n \nStrategy:\nStrategic Anchoring. Focus on 'break-in' \nopportunities in highly connected hub sectors.") +
  
  annotate("segment", x = 0, xend = 10, y = -.9, yend = -.9,colour = "black",
           size=2, alpha=1, arrow=arrow(type = "closed", angle = 15)) +
  annotate("segment", x = -.9, xend = -.9, y = 0, yend = 10, colour = "black",
           size=2, alpha=1, arrow=arrow(type = "closed", angle = 15))

plot <- 
  plot + 
  annotate("text", x = 7.5, y = -0.5, label = "High", color = "black") +
  annotate("text", x = 2.5, y = -0.5, label = "Low", color = "black") +
  annotate("text", x = -.5, y = 7.5, angle = 90, label = "High", color = "black") +
  annotate("text", x = -.5, y = 2.5, angle = 90, label = "Low", color = "black") +
  annotate("rect", xmin = 0, xmax = 5, ymin = 0, ymax = 5, fill= "#999999", alpha = .6)+
  annotate("rect", xmin = 5, xmax = 10, ymin = 0, ymax = 5, fill= "#3399FF", alpha = .6)+
  annotate("rect", xmin = 0, xmax = 5, ymin = 5, ymax = 10, fill= "#FF0000", alpha = .5)+
  annotate("rect", xmin = 5, xmax = 10, ymin = 5, ymax = 10, fill= "#009900", alpha = .6)
plot

jpeg("Files_created_with_the_code/figures/Figure_13.jpg", width = 9.5, height = 9.5, units = 'in', res = 500)
plot
dev.off()

#4. FOURTH PART: Econometrics-----
##4.1.Calculate density matrix for 5-years intervals and techn field level-----
rm(list=ls())
options(scipen = 999) #deactivate scientific notation
#Create Matrix
matrix2 <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/Matrix_IPC.csv", 
                    sep = ";", header = FALSE)

matrix2 <- matrix2 %>%  row_to_names(row_number = 1)
matrix <- matrix2[,-1]
rownames(matrix) <- matrix2[,1]
matrix <- as.matrix(matrix)
mat_tech_AI_Final <- matrix #this is a matrix of co-occurrences between techn fields, i.e., industry x
#industry matrix (for the 35 techn fields)

# Calculate relatedness using cosine similarity
mat_tech_rel_AI <- mat_tech_AI_Final %>% relatedness(method = "cosine") #this is literally the relatedness part
#of the relatedness_density_int_avg(mat, relatedness) function; now I need the mat (which is one per period) 
rm(mat_tech_AI_Final,matrix,matrix2)

Periods_5y <- c(
  "1974-1978",
  "1979-1983",
  "1984-1988",
  "1989-1993",
  "1994-1998",
  "1999-2003",
  "2004-2008",
  "2009-2013",
  "2014-2018"
)

Periods_5y[1]
#for the first period (1974-1978)
reg_tech1_countries <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/robustness_automated/reg_tech_5_years_1974-1978.csv", 
                                sep = ";", header = TRUE, dec=",")

# Create a wide matrix of technology fields for the first interval
mat_reg_tech1_countries <- reg_tech1_countries %>%  arrange(techn_field_nr, ctry_code) %>%
  pivot_wider(names_from = techn_field_nr, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

mat_reg_tech1_countries %<>%   remove_rownames %>% 
  column_to_rownames(var="ctry_code") %>%  as.matrix() %>%    round()

# Compute RCA for the first interval (general)

if (!is.matrix(mat_reg_tech1_countries)) {
  # Ensure the country code column is character for rownames
  mat_reg_tech1_countries$ctry_code <- as.character(mat_reg_tech1_countries$ctry_code)
  # Check for duplicate country codes before setting rownames
  if (any(duplicated(mat_reg_tech1_countries$ctry_code))) {
    stop("Duplicate country codes found. Rownames must be unique.")
  }
  temp_matrix <- mat_reg_tech1_countries %>%
    column_to_rownames(var = "ctry_code") %>% # Set first column as rownames
    as.matrix()
} else {
  temp_matrix <- mat_reg_tech1_countries # If it's already a matrix with rownames
}

# Now, calculate the binary RCA matrix (this will be the 'mat' for relatedness.density.int.avg)
binary_specialization_matrix <- location_quotient(temp_matrix, binary = TRUE)

Period1 <- as.data.frame(relatedness_density_int_avg(binary_specialization_matrix, mat_tech_rel_AI))
names(Period1) <- "Rel_density"
Period1$Period <- paste(Periods_5y[1])
Period1 %<>%
  rownames_to_column(var = "ctry_code")

Periods_5y[2]
#for the second period (1979-1983)
reg_tech2_countries <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/robustness_automated/reg_tech_5_years_1979-1983.csv", 
                                sep = ";", header = TRUE, dec=",")

# Create a wide matrix of technology fields for the first interval
mat_reg_tech2_countries <- reg_tech2_countries %>%  arrange(techn_field_nr, ctry_code) %>%
  pivot_wider(names_from = techn_field_nr, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

mat_reg_tech2_countries %<>%   remove_rownames %>% 
  column_to_rownames(var="ctry_code") %>%  as.matrix() %>%    round()

# Compute RCA for the first interval (general)

if (!is.matrix(mat_reg_tech2_countries)) {
  # Ensure the country code column is character for rownames
  mat_reg_tech2_countries$ctry_code <- as.character(mat_reg_tech2_countries$ctry_code)
  # Check for duplicate country codes before setting rownames
  if (any(duplicated(mat_reg_tech2_countries$ctry_code))) {
    stop("Duplicate country codes found. Rownames must be unique.")
  }
  temp_matrix <- mat_reg_tech2_countries %>%
    column_to_rownames(var = "ctry_code") %>% # Set first column as rownames
    as.matrix()
} else {
  temp_matrix <- mat_reg_tech2_countries # If it's already a matrix with rownames
}

# Now, calculate the binary RCA matrix (this will be the 'mat' for relatedness.density.int.avg)
binary_specialization_matrix <- location_quotient(temp_matrix, binary = TRUE)

Period2 <- as.data.frame(relatedness_density_int_avg(binary_specialization_matrix, mat_tech_rel_AI))
names(Period2) <- "Rel_density"
Period2$Period <- paste(Periods_5y[2])
Period2 %<>%
  rownames_to_column(var = "ctry_code")

Periods_5y[3]
#for the third period (1984-1988)
reg_tech3_countries <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/robustness_automated/reg_tech_5_years_1984-1988.csv", 
                                sep = ";", header = TRUE, dec=",")

# Create a wide matrix of technology fields for the first interval
mat_reg_tech3_countries <- reg_tech3_countries %>%  arrange(techn_field_nr, ctry_code) %>%
  pivot_wider(names_from = techn_field_nr, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

mat_reg_tech3_countries %<>%   remove_rownames %>% 
  column_to_rownames(var="ctry_code") %>%  as.matrix() %>%    round()

# Compute RCA for the first interval (general)

if (!is.matrix(mat_reg_tech3_countries)) {
  # Ensure the country code column is character for rownames
  mat_reg_tech3_countries$ctry_code <- as.character(mat_reg_tech3_countries$ctry_code)
  # Check for duplicate country codes before setting rownames
  if (any(duplicated(mat_reg_tech3_countries$ctry_code))) {
    stop("Duplicate country codes found. Rownames must be unique.")
  }
  temp_matrix <- mat_reg_tech3_countries %>%
    column_to_rownames(var = "ctry_code") %>% # Set first column as rownames
    as.matrix()
} else {
  temp_matrix <- mat_reg_tech3_countries # If it's already a matrix with rownames
}

# Now, calculate the binary RCA matrix (this will be the 'mat' for relatedness.density.int.avg)
binary_specialization_matrix <- location_quotient(temp_matrix, binary = TRUE)

Period3 <- as.data.frame(relatedness_density_int_avg(binary_specialization_matrix, mat_tech_rel_AI))
names(Period3) <- "Rel_density"
Period3$Period <- paste(Periods_5y[3])
Period3 %<>%
  rownames_to_column(var = "ctry_code")

Periods_5y[4]
#for the fourth period (1989-1993)
reg_tech4_countries <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/robustness_automated/reg_tech_5_years_1989-1993.csv", 
                                sep = ";", header = TRUE, dec=",")

# Create a wide matrix of technology fields for the first interval
mat_reg_tech4_countries <- reg_tech4_countries %>%  arrange(techn_field_nr, ctry_code) %>%
  pivot_wider(names_from = techn_field_nr, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

mat_reg_tech4_countries %<>%   remove_rownames %>% 
  column_to_rownames(var="ctry_code") %>%  as.matrix() %>%    round()

# Compute RCA for the first interval (general)

if (!is.matrix(mat_reg_tech4_countries)) {
  # Ensure the country code column is character for rownames
  mat_reg_tech4_countries$ctry_code <- as.character(mat_reg_tech4_countries$ctry_code)
  # Check for duplicate country codes before setting rownames
  if (any(duplicated(mat_reg_tech4_countries$ctry_code))) {
    stop("Duplicate country codes found. Rownames must be unique.")
  }
  temp_matrix <- mat_reg_tech4_countries %>%
    column_to_rownames(var = "ctry_code") %>% # Set first column as rownames
    as.matrix()
} else {
  temp_matrix <- mat_reg_tech4_countries # If it's already a matrix with rownames
}

# Now, calculate the binary RCA matrix (this will be the 'mat' for relatedness.density.int.avg)
binary_specialization_matrix <- location_quotient(temp_matrix, binary = TRUE)

Period4 <- as.data.frame(relatedness_density_int_avg(binary_specialization_matrix, mat_tech_rel_AI))
names(Period4) <- "Rel_density"
Period4$Period <- paste(Periods_5y[4])
Period4 %<>%
  rownames_to_column(var = "ctry_code")

Periods_5y[5]
#for the fifth period (1994-1998)
reg_tech5_countries <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/robustness_automated/reg_tech_5_years_1994-1998.csv", 
                                sep = ";", header = TRUE, dec=",")

# Create a wide matrix of technology fields for the first interval
mat_reg_tech5_countries <- reg_tech5_countries %>%  arrange(techn_field_nr, ctry_code) %>%
  pivot_wider(names_from = techn_field_nr, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

mat_reg_tech5_countries %<>%   remove_rownames %>% 
  column_to_rownames(var="ctry_code") %>%  as.matrix() %>%    round()

# Compute RCA for the first interval (general)

if (!is.matrix(mat_reg_tech5_countries)) {
  # Ensure the country code column is character for rownames
  mat_reg_tech5_countries$ctry_code <- as.character(mat_reg_tech5_countries$ctry_code)
  # Check for duplicate country codes before setting rownames
  if (any(duplicated(mat_reg_tech5_countries$ctry_code))) {
    stop("Duplicate country codes found. Rownames must be unique.")
  }
  temp_matrix <- mat_reg_tech5_countries %>%
    column_to_rownames(var = "ctry_code") %>% # Set first column as rownames
    as.matrix()
} else {
  temp_matrix <- mat_reg_tech5_countries # If it's already a matrix with rownames
}

# Now, calculate the binary RCA matrix (this will be the 'mat' for relatedness.density.int.avg)
binary_specialization_matrix <- location_quotient(temp_matrix, binary = TRUE)

Period5 <- as.data.frame(relatedness_density_int_avg(binary_specialization_matrix, mat_tech_rel_AI))
names(Period5) <- "Rel_density"
Period5$Period <- paste(Periods_5y[5])
Period5 %<>%
  rownames_to_column(var = "ctry_code")

Periods_5y[6]
#for the sixth period (1999-2003)
reg_tech6_countries <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/robustness_automated/reg_tech_5_years_1999-2003.csv", 
                                sep = ";", header = TRUE, dec=",")

# Create a wide matrix of technology fields for the first interval
mat_reg_tech6_countries <- reg_tech6_countries %>%  arrange(techn_field_nr, ctry_code) %>%
  pivot_wider(names_from = techn_field_nr, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

mat_reg_tech6_countries %<>%   remove_rownames %>% 
  column_to_rownames(var="ctry_code") %>%  as.matrix() %>%    round()

# Compute RCA for the first interval (general)

if (!is.matrix(mat_reg_tech6_countries)) {
  # Ensure the country code column is character for rownames
  mat_reg_tech6_countries$ctry_code <- as.character(mat_reg_tech6_countries$ctry_code)
  # Check for duplicate country codes before setting rownames
  if (any(duplicated(mat_reg_tech6_countries$ctry_code))) {
    stop("Duplicate country codes found. Rownames must be unique.")
  }
  temp_matrix <- mat_reg_tech6_countries %>%
    column_to_rownames(var = "ctry_code") %>% # Set first column as rownames
    as.matrix()
} else {
  temp_matrix <- mat_reg_tech6_countries # If it's already a matrix with rownames
}

# Now, calculate the binary RCA matrix (this will be the 'mat' for relatedness.density.int.avg)
binary_specialization_matrix <- location_quotient(temp_matrix, binary = TRUE)

Period6 <- as.data.frame(relatedness_density_int_avg(binary_specialization_matrix, mat_tech_rel_AI))
names(Period6) <- "Rel_density"
Period6$Period <- paste(Periods_5y[6])
Period6 %<>%
  rownames_to_column(var = "ctry_code")

Periods_5y[7]
#for the seventh period (2004-2008)
reg_tech7_countries <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/robustness_automated/reg_tech_5_years_2004-2008.csv", 
                                sep = ";", header = TRUE, dec=",")

# Create a wide matrix of technology fields for the first interval
mat_reg_tech7_countries <- reg_tech7_countries %>%  arrange(techn_field_nr, ctry_code) %>%
  pivot_wider(names_from = techn_field_nr, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

mat_reg_tech7_countries %<>%   remove_rownames %>% 
  column_to_rownames(var="ctry_code") %>%  as.matrix() %>%    round()

# Compute RCA for the first interval (general)

if (!is.matrix(mat_reg_tech7_countries)) {
  # Ensure the country code column is character for rownames
  mat_reg_tech7_countries$ctry_code <- as.character(mat_reg_tech7_countries$ctry_code)
  # Check for duplicate country codes before setting rownames
  if (any(duplicated(mat_reg_tech7_countries$ctry_code))) {
    stop("Duplicate country codes found. Rownames must be unique.")
  }
  temp_matrix <- mat_reg_tech7_countries %>%
    column_to_rownames(var = "ctry_code") %>% # Set first column as rownames
    as.matrix()
} else {
  temp_matrix <- mat_reg_tech7_countries # If it's already a matrix with rownames
}

# Now, calculate the binary RCA matrix (this will be the 'mat' for relatedness.density.int.avg)
binary_specialization_matrix <- location_quotient(temp_matrix, binary = TRUE)

Period7 <- as.data.frame(relatedness_density_int_avg(binary_specialization_matrix, mat_tech_rel_AI))
names(Period7) <- "Rel_density"
Period7$Period <- paste(Periods_5y[7])
Period7 %<>%
  rownames_to_column(var = "ctry_code")

Periods_5y[8]
#for the eighth period (2009-2013)
reg_tech8_countries <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/robustness_automated/reg_tech_5_years_2009-2013.csv", 
                                sep = ";", header = TRUE, dec=",")

# Create a wide matrix of technology fields for the first interval
mat_reg_tech8_countries <- reg_tech8_countries %>%  arrange(techn_field_nr, ctry_code) %>%
  pivot_wider(names_from = techn_field_nr, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

mat_reg_tech8_countries %<>%   remove_rownames %>% 
  column_to_rownames(var="ctry_code") %>%  as.matrix() %>%    round()

# Compute RCA for the first interval (general)
if (!is.matrix(mat_reg_tech8_countries)) {
  # Ensure the country code column is character for rownames
  mat_reg_tech8_countries$ctry_code <- as.character(mat_reg_tech8_countries$ctry_code)
  # Check for duplicate country codes before setting rownames
  if (any(duplicated(mat_reg_tech8_countries$ctry_code))) {
    stop("Duplicate country codes found. Rownames must be unique.")
  }
  temp_matrix <- mat_reg_tech8_countries %>%
    column_to_rownames(var = "ctry_code") %>% # Set first column as rownames
    as.matrix()
} else {
  temp_matrix <- mat_reg_tech8_countries # If it's already a matrix with rownames
}

# Now, calculate the binary RCA matrix (this will be the 'mat' for relatedness.density.int.avg)
binary_specialization_matrix <- location_quotient(temp_matrix, binary = TRUE)

Period8 <- as.data.frame(relatedness_density_int_avg(binary_specialization_matrix, mat_tech_rel_AI))
names(Period8) <- "Rel_density"
Period8$Period <- paste(Periods_5y[8])
Period8 %<>%
  rownames_to_column(var = "ctry_code")

Periods_5y[9]
#for the ninth period (2014-2018)
reg_tech9_countries <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/robustness_automated/reg_tech_5_years_2014-2018.csv", 
                                sep = ";", header = TRUE, dec=",")

# Create a wide matrix of technology fields for the first interval
mat_reg_tech9_countries <- reg_tech9_countries %>%  arrange(techn_field_nr, ctry_code) %>%
  pivot_wider(names_from = techn_field_nr, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

mat_reg_tech9_countries %<>%   remove_rownames %>% 
  column_to_rownames(var="ctry_code") %>%  as.matrix() %>%    round()

# Compute RCA for the first interval (general)

if (!is.matrix(mat_reg_tech9_countries)) {
  # Ensure the country code column is character for rownames
  mat_reg_tech9_countries$ctry_code <- as.character(mat_reg_tech9_countries$ctry_code)
  # Check for duplicate country codes before setting rownames
  if (any(duplicated(mat_reg_tech9_countries$ctry_code))) {
    stop("Duplicate country codes found. Rownames must be unique.")
  }
  temp_matrix <- mat_reg_tech9_countries %>%
    column_to_rownames(var = "ctry_code") %>% # Set first column as rownames
    as.matrix()
} else {
  temp_matrix <- mat_reg_tech9_countries # If it's already a matrix with rownames
}

# Now, calculate the binary RCA matrix (this will be the 'mat' for relatedness.density.int.avg)
binary_specialization_matrix <- location_quotient(temp_matrix, binary = TRUE)

Period9 <- as.data.frame(relatedness_density_int_avg(binary_specialization_matrix, mat_tech_rel_AI))
names(Period9) <- "Rel_density"
Period9$Period <- paste(Periods_5y[9])
Period9 %<>%
  rownames_to_column(var = "ctry_code")

Rel_density <- rbind(Period1, Period2, Period3, Period4, Period5, Period6, Period7, Period8, Period9 )
length(unique(Rel_density$Period)) #9, which is correct
unique(Rel_density$Period)
write.csv2(Rel_density, 
           file = "Files_created_with_the_code/data/files_code_Fields_analysis/robustness/Rel_density_5y.csv", 
           row.names = FALSE)

##4.2.Put it all together in the models----
options(scipen = 999) #deactivate scientific notation
rm(list=ls())
Rel_density <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/robustness/Rel_density_5y.csv", 
                        sep = ";", header = TRUE, dec=",")
target_countries <- c("CN", "JP", "US", "KR") 
Rel_density <-  Rel_density[Rel_density$ctry_code %in% target_countries,] 

Periods_5y <- c("1974-1978","1979-1983","1984-1988","1989-1993","1994-1998","1999-2003","2004-2008","2009-2013","2014-2018")

Per_1 <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/robustness_automated/Data_RCA_techn_field_5_years_1974-1978.csv", 
                  sep = ";", header = TRUE, dec=",")
Per_2 <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/robustness_automated/Data_RCA_techn_field_5_years_1979-1983.csv", 
                  sep = ";", header = TRUE, dec=",")
Per_3 <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/robustness_automated/Data_RCA_techn_field_5_years_1984-1988.csv", 
                  sep = ";", header = TRUE, dec=",")
Per_4 <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/robustness_automated/Data_RCA_techn_field_5_years_1989-1993.csv", 
                  sep = ";", header = TRUE, dec=",")
Per_5 <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/robustness_automated/Data_RCA_techn_field_5_years_1994-1998.csv", 
                  sep = ";", header = TRUE, dec=",")
Per_6 <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/robustness_automated/Data_RCA_techn_field_5_years_1999-2003.csv", 
                  sep = ";", header = TRUE, dec=",")
Per_7 <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/robustness_automated/Data_RCA_techn_field_5_years_2004-2008.csv", 
                  sep = ";", header = TRUE, dec=",")
Per_8 <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/robustness_automated/Data_RCA_techn_field_5_years_2009-2013.csv", 
                  sep = ";", header = TRUE, dec=",")
Per_9 <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/robustness_automated/Data_RCA_techn_field_5_years_2014-2018.csv", 
                  sep = ";", header = TRUE, dec=",")
Per_all <- rbind(Per_1, Per_2, Per_3, Per_4, Per_5, Per_6, Per_7, Per_8, Per_9)
rm(Per_1, Per_2, Per_3, Per_4, Per_5, Per_6, Per_7, Per_8, Per_9)
unique(Per_all$Period)

Per_all[is.na(Per_all)] <- 0
Per_all <- Per_all[Per_all$ctry_code %in% target_countries,] 
Per_all$Round_general <- ifelse(Per_all$RCA_Gen < 1, 0, 1)
Per_all$Round_AI <- ifelse(Per_all$RCA_AI < 1, 0, 1)
Per_all$Total_RCA_2 <- Per_all$Round_general + 2*Per_all$Round_AI

Specializations_data <- as.data.frame(table(Per_all$Total_RCA_2, Per_all$ctry_code, Per_all$Period))
Specializations_data$Var1 <- gsub("0", "No specialization", str_trim(Specializations_data$Var1))
Specializations_data$Var1 <- gsub("1", "General specialization", str_trim(Specializations_data$Var1))
Specializations_data$Var1 <- gsub("2", "AI-specific specialization", str_trim(Specializations_data$Var1))
Specializations_data$Var1 <- gsub("3", "Coinciding specialization", str_trim(Specializations_data$Var1))

names(Specializations_data) <- c("Var1","ctry_code", "Period", "N_spec")
# Pivot the data
Specializations_wide <- Specializations_data %>%
  pivot_wider(names_from = Var1,
              values_from = N_spec)

Rel_density <- left_join(Rel_density, Specializations_wide, by = c("ctry_code", "Period"))

actual_shares_5_years <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/robustness/actual_shares_5_years.csv", 
                                  sep = ";", header = TRUE, dec=",")
#variables from the actual_shares_5_years file:
#Actual_n_persistent_Round_AI = n_persistent_Round_AI, # Is it persistently Round_AI?
#Actual_n_AI_prev_coinciding = n_AI_prev_coinciding, # Was it coinciding and then justAI?
#Actual_n_coinciding_prev_AI = n_coinciding_prev_AI, # Was it justAI and then coinciding?
#Actual_n_AI_prev_gen = n_AI_prev_gen, #Was it justGeneral and then justAI?
#Actual_n_persistent_core_fields = n_persistent_core_fields, #sustains AI in core fields?
#Actual_n_persistent_NOT_core_fields = n_persistent_NOT_core_fields,#sustains AI in NOT core fields?
#Actual_n_persistent_coin_core_fields = n_persistent_coin_core_fields, # persistent coinciding in AI core fields? 
#Actual_n_persistent_coin_NOT_core_fields = n_persistent_coin_NOT_core_fields# persistent coinciding in NOT AI core fields? 

Rel_density <- left_join(Rel_density, actual_shares_5_years, by = c("ctry_code", "Period"))

Rel_density %<>%  clean_names()
Rel_density$total_general_specializations <- Rel_density$general_specialization + Rel_density$coinciding_specialization
Rel_density$double_check <- Rel_density$total_general_specializations + Rel_density$no_specialization + Rel_density$ai_specific_specialization
Rel_density$total_specializations <- Rel_density$total_general_specializations + Rel_density$ai_specific_specialization #which should be the same as
#total_general_specializations - no_specialization; pay attention that this includes AI
Rel_density$total_AI_specializations <- Rel_density$coinciding_specialization + Rel_density$ai_specific_specialization

regression_data <- Rel_density %>%
  mutate(Share_Coinciding =  coinciding_specialization/(total_general_specializations),
         # Ensure Period is a factor for the regression
         Period = factor(period, levels = c("1974-1978","1979-1983","1984-1988","1989-1993","1994-1998","1999-2003",
                                            "2004-2008","2009-2013","2014-2018"), ordered = FALSE), # Not ordered for lm factor
         ctry_code = factor(ctry_code))
rm(Specializations_wide, Specializations_data)

regression_data <- regression_data %>%
  mutate(ctry_code = case_when(
    ctry_code == "JP" ~ "Japan",
    ctry_code == "US" ~ "US",
    ctry_code == "KR" ~ "South Korea",
    ctry_code == "CN" ~ "China",
    TRUE ~ ctry_code # This keeps any other codes as they are
  ))

#redefine names:
regression_data_renamed <- regression_data %>%
  rename(
    "Country’s technological relatedness density" = rel_density,
    "Interval " = Period,
    "No. of ‘general’ spec." = total_general_specializations,
    "Share of ‘break-in’ spec." = Share_Coinciding,
    "No. of ‘AI-specific’ spec." = total_AI_specializations,
    "No. of sustained ‘break-in’ spec." = actual_persistent_coinciding,
    "No. of sustained ‘general’ spec." = actual_persistent_general_all,
    "No. of sustained ‘AI-specific’ spec." = actual_n_persistent_round_ai,
    "No. of ‘break-in’ spec." = coinciding_specialization,
    "Country "= ctry_code
  )

###4.2.1.Main Models----
# Model 1 - The simplest model
model1 <- lm(`Share of ‘break-in’ spec.` ~ `Country’s technological relatedness density` + `Interval `, 
             data = regression_data_renamed) 
summary(model1)

# Model 2 - adding number of general specialisations
model2 <- lm(`Share of ‘break-in’ spec.` ~ `Country’s technological relatedness density` + `No. of ‘general’ spec.` + `Interval ` + 
               `No. of ‘AI-specific’ spec.`, 
             data = regression_data_renamed) 
summary(model2)

# Model 3 - controlling for country
model3 <- lm(`Share of ‘break-in’ spec.` ~ `Country’s technological relatedness density` + `No. of ‘general’ spec.` + `Interval ` + `Country ` + 
               `No. of ‘AI-specific’ spec.`,
             data = regression_data_renamed)
summary(model3)

desired_order_model1_to_3 <- c("`Country’s technological relatedness density`", "`Interval `", "`Country `", "`No. of ‘general’ spec.`",
                   "`No. of ‘AI-specific’ spec.`")

stargazer(model1, model2, model3, type = "html", order = desired_order_model1_to_3, out = "Files_created_with_the_code/data/files_code_Fields_analysis/models_1_2_3.doc")

#Next models: looking at sustained specialisations
#Model 4 - persistent break-ins
newmodel4 <- lm(`No. of sustained ‘break-in’ spec.` ~ `Country’s technological relatedness density` + `Interval `+ `No. of sustained ‘general’ spec.` + 
                  `No. of sustained ‘AI-specific’ spec.`, data = regression_data_renamed)
summary(newmodel4)

#Model 5 - persistent break-ins
newmodel5 <- lm(`No. of sustained ‘break-in’ spec.` ~ `Country’s technological relatedness density` + `Interval `+ `No. of ‘general’ spec.` + `No. of ‘break-in’ spec.` +
                  `No. of ‘AI-specific’ spec.`, 
                data = regression_data_renamed)
summary(newmodel5)

#Model 6 persistent AI-specific
newmodel6 <- lm(`No. of sustained ‘AI-specific’ spec.` ~ `Country’s technological relatedness density` + `Interval `+ `No. of ‘general’ spec.` +
                  `No. of sustained ‘break-in’ spec.`+`No. of ‘break-in’ spec.`+`No. of ‘AI-specific’ spec.`, 
                data = regression_data_renamed)
summary(newmodel6)

desired_order_model4_to_6 <- c("`Country’s technological relatedness density`", "`Interval `", 
                               "`No. of ‘break-in’ spec.`", "`No. of ‘general’ spec.`", "`No. of ‘AI-specific’ spec.`",
                               "`No. of sustained ‘break-in’ spec.`", "`No. of sustained ‘general’ spec.`", 
                               "`No. of sustained ‘AI-specific’ spec.`")

stargazer(newmodel5, newmodel4,newmodel6, type = "html", order = desired_order_model4_to_6, out = "Files_created_with_the_code/data/files_code_Fields_analysis/modelsnew_4_to_6.doc")


###4.2.3. Additional models------
####4.2.3.1. Beta regression for Table 3-----
library(betareg)
library(MASS)
library(statmod)

#The dependent variable "Share of break-in specialisations" is a proportion (ranging from 0 to 1). For such data, 
#Beta regression is generally preferred over OLS, as it directly models the mean of a variable that follows a Beta 
#distribution. 
table(regression_data_renamed$`Share of ‘break-in’ spec.`)
#Ensure the "Share of break-in specialisations" variable is strictly between 0 and 1. If there are 0s or 1s, one needs to transform 
#them slightly (e.g., (y * (n - 1) + 0.5) / n where n is the sample size, or using a small epsilon).
regression_data_renamed$`Share of ‘break-in’ spec.` <- ifelse(regression_data_renamed$`Share of ‘break-in’ spec.` ==0,0.01, regression_data_renamed$`Share of ‘break-in’ spec.`)
table(regression_data_renamed$`Share of ‘break-in’ spec.`)

# Model 1 - The simplest model
model1_beta <- betareg(`Share of ‘break-in’ spec.` ~ `Country’s technological relatedness density` + `Interval `, 
                       data = regression_data_renamed) 
summary(model1_beta)

# Model 2 - adding number of general specialisations
model2_beta <- betareg(`Share of ‘break-in’ spec.` ~ `Country’s technological relatedness density` + `No. of ‘general’ spec.` + `Interval ` + 
                         `No. of ‘AI-specific’ spec.`, 
                       data = regression_data_renamed) 
summary(model2_beta)

# Model 3 - controlling for country
model3_beta <- betareg(`Share of ‘break-in’ spec.` ~ `Country’s technological relatedness density` + `No. of ‘general’ spec.` + `Interval ` + `Country ` + 
                         `No. of ‘AI-specific’ spec.`,
                       data = regression_data_renamed)
summary(model3_beta)

stargazer(model1_beta, model2_beta, model3_beta, type = "html",  order = desired_order_model1_to_3, out = "Files_created_with_the_code/data/files_code_Fields_analysis/models_1_2_3_Beta.doc")

####4.2.3.2. Poisson regression and Negative Binomial regression for Table 4-----
#The dependent variables "No. of sustained 'break-in' Spec." and "No. of sustained 'Al-specific' Spec." are counts. 
#For count data, Poisson regression or Negative Binomial regression are appropriate. Negative Binomial regression 
#is often preferred when there's evidence of overdispersion (variance > mean) in the count data, which is very 
#common.
#Let's start with poisson
#Model 4 - persistent break-ins
model4_poisson <- glm(`No. of sustained ‘break-in’ spec.` ~ `Country’s technological relatedness density` + `Interval `+ `No. of sustained ‘general’ spec.` + 
                        `No. of sustained ‘AI-specific’ spec.`,
                      family = poisson(link = "log"),
                      data = regression_data_renamed)
summary(model4_poisson)

#Model 5 - persistent break-ins
model5_poisson <- glm(`No. of sustained ‘break-in’ spec.` ~ `Country’s technological relatedness density` + `Interval `+ `No. of ‘general’ spec.` + `No. of ‘break-in’ spec.` +
                        `No. of ‘AI-specific’ spec.`, 
                      family = poisson(link = "log"),
                      data = regression_data_renamed)
summary(model5_poisson)

#Model 6 persistent AI-specific
model6_poisson <- glm(`No. of sustained ‘AI-specific’ spec.` ~ `Country’s technological relatedness density` + `Interval `+ `No. of ‘general’ spec.` +
                        `No. of sustained ‘break-in’ spec.`+`No. of ‘break-in’ spec.`+`No. of ‘AI-specific’ spec.`, 
                      family = poisson(link = "log"),
                      data = regression_data_renamed)
summary(model6_poisson)
stargazer(model5_poisson, model4_poisson,  model6_poisson, type = "html",  order = desired_order_model4_to_6, out = "Files_created_with_the_code/data/files_code_Fields_analysis/modelsnew_4_to_6_poisson.doc")

#now we do negative binomial
#Model 4 - persistent break-ins
model4_negbin <- glm.nb(`No. of sustained ‘break-in’ spec.` ~ `Country’s technological relatedness density` + `Interval `+ `No. of sustained ‘general’ spec.` + 
                          `No. of sustained ‘AI-specific’ spec.`,
                        data = regression_data_renamed)
summary(model4_negbin)

#Model 5 - persistent break-ins
model5_negbin <- glm.nb(`No. of sustained ‘break-in’ spec.` ~ `Country’s technological relatedness density` + `Interval `+ `No. of ‘general’ spec.` + `No. of ‘break-in’ spec.` +
                          `No. of ‘AI-specific’ spec.`, 
                        data = regression_data_renamed)
summary(model5_negbin)

#Model 6 persistent AI-specific
model6_negbin <- glm.nb(`No. of sustained ‘AI-specific’ spec.` ~ `Country’s technological relatedness density` + `Interval `+ `No. of ‘general’ spec.` +
                          `No. of sustained ‘break-in’ spec.`+`No. of ‘break-in’ spec.`+`No. of ‘AI-specific’ spec.`, 
                        data = regression_data_renamed)
summary(model6_negbin)
stargazer(model5_negbin, model4_negbin,  model6_negbin, type = "html",  order = desired_order_model4_to_6, out = "Files_created_with_the_code/data/files_code_Fields_analysis/modelsnew_4_to_6_negbin.doc")
