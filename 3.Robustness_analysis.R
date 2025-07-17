#### Robustness analysis
#This is the third code to be executed. It generates the data and figures used in the robustness analysis of the paper.

#Assuming that the previous 2 codes were run using the same R session, some of the libraries loaded in the previous 2 codes 
#conflict with the ones loaded here, so let's unload everything:
unload_all_packages <- function() {
  # Identify all loaded packages except base packages
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", 
                      "package:utils", "package:datasets", "package:methods", 
                      "package:base")
  
  loaded.packages <- search()[grepl("package:", search())]
  packages.to.unload <- setdiff(loaded.packages, basic.packages)
  
  # Detach each package
  for (pkg in packages.to.unload) {
    try(detach(pkg, character.only = TRUE, unload = TRUE), silent = TRUE)
  }
}
# Run the function
unload_all_packages()

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
library(janitor) #also used in the clean_names() function
library(ggforce)
library(stringr)
library(openxlsx)
library(gridExtra) #for grid.arrange
library(readxl) #for reading the xlsx files
library(lmtest) #for LM analysis and robustness econometric test
library(sandwich) 
library(stargazer) #for generating nice econometric tables

rm(list=ls())
#set the working directory to where you saved the R code:
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#FIRST PART: Share of break-ins at different intervals------
##1.1.Calculate specialisations based on Technological field -----
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

##1.1. Calculate Specializations for Different Time intervals
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
           file = "Files_created_with_the_code/data/files_code_Fields_analysis/robustness/reg_tech_FirstPeriod.csv", 
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

# Merge general and AI-specific RCA data for Interval 1
rca_data_period_1_df <- merge(reg_RCA1_df, reg_RCA1_AI_df, all = TRUE, by = c("ctry_code", "techn_field_nr"))
rca_data_period_1_df$Period <- "1974-1988"
names(rca_data_period_1_df) <- c("ctry_code", "techn_field_nr", "RCA_Gen", "RCA_AI", "Period")

#test <- rca_data_period_1_df[rca_data_period_1_df$ctry_code == "JP",]

write.csv2(rca_data_period_1_df, 
           file = "Files_created_with_the_code/data/files_code_Fields_analysis/robustness/Data1period_RCA_techn_field.csv", 
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
           file = "Files_created_with_the_code/data/files_code_Fields_analysis/robustness/reg_techAI_FirstPeriod.csv", 
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
           file = "Files_created_with_the_code/data/files_code_Fields_analysis/robustness/reg_tech_SecondPeriod.csv", 
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

# Merge general and AI-specific RCA data for Interval 2
rca_data_period_2_df <- merge(reg_RCA2_df, reg_RCA2_AI_df, all = TRUE, by = c("ctry_code","techn_field_nr"))
rca_data_period_2_df$Period <- "1989-2003"
names(rca_data_period_2_df) <- c("ctry_code","techn_field_nr","RCA_Gen","RCA_AI","Period")

write.csv2(rca_data_period_2_df, 
           file = "Files_created_with_the_code/data/files_code_Fields_analysis/robustness/Data2period_RCA_techn_field.csv", 
           row.names = FALSE)

# Regional Tech for AI - Interval 2 
ipc_all_patents_second_period_df[ai_patents_df, on = "appln_id", ctry_code := i.ctry_code]
region_tech_ai_2_df <- group_by_applnID(ipc_all_patents_second_period_df)
region_tech_ai_2_df <- group_by_ctry_and_techn_field(region_tech_ai_2_df)

write.csv2(region_tech_ai_2_df, 
           file = "Files_created_with_the_code/data/files_code_Fields_analysis/robustness/reg_techAI_SecondPeriod.csv", 
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
           file = "Files_created_with_the_code/data/files_code_Fields_analysis/robustness/reg_tech_ThirdPeriod.csv", 
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

# Merge general and AI-specific RCA data for Interval 3
rca_data_period_3_df <- merge(reg_RCA3_df, reg_RCA3_AI_df, all=TRUE, by=c("ctry_code","techn_field_nr"))
rca_data_period_3_df$Period <- "2004-2018"
names(rca_data_period_3_df) <- c("ctry_code","techn_field_nr","RCA_Gen","RCA_AI","Period")

write.csv2(rca_data_period_3_df, 
           file = "Files_created_with_the_code/data/files_code_Fields_analysis/robustness/Data3period_RCA_techn_field.csv", 
           row.names = FALSE)

# Combine Data for All Intervals
ipc_rcas_df <- rbind(rca_data_period_1_df, rca_data_period_2_df, rca_data_period_3_df)
write.csv2(ipc_rcas_df, 
           file = "Files_created_with_the_code/data/files_code_Fields_analysis/robustness/IPC_RCAs.csv", 
           row.names = FALSE)

# Remove all non-function objects in the global environment and clean memory
rm(list = ls()[!sapply(ls(), function(x) is.function(get(x)))])
gc()

# Summary RCAs for the four leading countries
IPC_RCAs <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/robustness/IPC_RCAs.csv", 
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

write.csv2(IPC_RCAs_Top4, file = "Files_created_with_the_code/data/files_code_Fields_analysis/robustness/RCA_4countries_detailed.csv", 
           row.names = FALSE)

IPC_RCAs_Top4_original <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/RCA_4countries_detailed.csv", 
                          sep = ";", header = TRUE, dec=",")

IPC_RCAs_Top4_new <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/robustness/RCA_4countries_detailed.csv", 
                          sep = ";", header = TRUE, dec=",")

##1.2.Calculate RTA of countries for AI technologies for distinct intervals--------
rm(list=ls())
library(data.table)
library(dplyr)
library(tidyr)
library(tibble)
library(EconGeo)

# --- Functions ---
group_by_applnID <- function(data){
  data %>%
    group_by(appln_id) %>%
    mutate(field_weight = 1 / n()) %>%
    ungroup()
}

group_by_ctry_and_techn_field <- function(data){
  # Ensure data is data.table for efficient summarization if it's large
  if (!is.data.table(data)) data <- as.data.table(data)
  result <- data[, .(n_tech_reg = sum(field_weight)), by = .(ctry_code, techn_field_nr)]
  result <- result[!is.na(ctry_code) & !is.na(techn_field_nr)] # More robust NA drop
  as_tibble(result) # Convert back to tibble if preferred downstream
}

# --- 1. Consolidate Data Loading and Initial Preparation ---

# Part 2 Data
rows_part2 <- 45182803 - 40000000
ipc_all_patents_part2_chunk1_df <- fread("large_files/All_patents_and_IPCs_Part2.csv", header = FALSE, nrow = 20000000)
ipc_all_patents_part2_chunk2_df <- fread("large_files/All_patents_and_IPCs_Part2.csv", header = FALSE, nrow = 20000000, skip = 20000000)
ipc_all_patents_part2_chunk3_df <- fread("large_files/All_patents_and_IPCs_Part2.csv", header = FALSE, nrow = rows_part2, skip = 40000000)
ipc_all_patents_part2_df <- rbindlist(list(ipc_all_patents_part2_chunk1_df, ipc_all_patents_part2_chunk2_df, ipc_all_patents_part2_chunk3_df))
rm(ipc_all_patents_part2_chunk1_df, ipc_all_patents_part2_chunk2_df, ipc_all_patents_part2_chunk3_df); gc()
setnames(ipc_all_patents_part2_df, c("appln_id", "ctry_code", "techn_field_nr", "weight", "priority_year"))

# Part 1 Data
rows_part1 <- 58841893 - 40000000
ipc_all_patents_part1_chunk1_df <- fread("large_files/All_patents_and_IPCs_Part1.csv", header = FALSE, nrow = 20000000)
ipc_all_patents_part1_chunk2_df <- fread("large_files/All_patents_and_IPCs_Part1.csv", header = FALSE, nrow = 20000000, skip = 20000000)
ipc_all_patents_part1_chunk3_df <- fread("large_files/All_patents_and_IPCs_Part1.csv", header = FALSE, nrow = rows_part1, skip = 40000000)
ipc_all_patents_part1_df <- rbindlist(list(ipc_all_patents_part1_chunk1_df, ipc_all_patents_part1_chunk2_df, ipc_all_patents_part1_chunk3_df))
rm(ipc_all_patents_part1_chunk1_df, ipc_all_patents_part1_chunk2_df, ipc_all_patents_part1_chunk3_df); gc()
setnames(ipc_all_patents_part1_df, c("appln_id", "ctry_code", "techn_field_nr", "weight", "priority_year"))

# Combine all patent data
all_patents_combined_df <- rbindlist(list(ipc_all_patents_part1_df, ipc_all_patents_part2_df))
rm(ipc_all_patents_part1_df, ipc_all_patents_part2_df); gc()
# Ensure priority_year is numeric for filtering
all_patents_combined_df[, priority_year := as.numeric(priority_year)]

# Load AI-specific data
ai_patents_df <- fread("other_files/IPCs_AI.csv", sep=";", dec=",", header=TRUE)
ai_patents_df[, ctry_code := "AI_pat"] # Using data.table assignment
ai_patents_df[, priority_year := as.numeric(priority_year)]

# Define base output path
output_base_dir <- "Files_created_with_the_code/data/files_code_Fields_analysis/robustness_automated"
dir.create(output_base_dir, recursive = TRUE, showWarnings = FALSE)

process_rca_for_period <- function(start_year_filter, end_year_filter,
                                   period_label_display, period_label_file,
                                   all_patents_data, ai_data,
                                   min_ai_obs_for_rca = 10) { # Added threshold parameter
  
  message(paste0("Processing RCA for period: ", period_label_display, " (File Suffix: ", period_label_file, ")"))
  
  # --- Filter Data for Current Period ---
  ipc_current_period_df <- all_patents_data[priority_year > start_year_filter & priority_year < end_year_filter]
  ipc_current_period_df <- ipc_current_period_df[, .(appln_id, ctry_code, techn_field_nr)]
  gc()
  
  if (nrow(ipc_current_period_df) == 0) {
    message(paste("No patent data found for period", period_label_display, "- skipping."))
    return(tibble(ctry_code=character(0), techn_field_nr=character(0), RCA_Gen=numeric(0), RCA_AI=numeric(0), Period=character(0)))
  }
  
  # --- Calculate General RCA ---
  message("Calculating General RCA...")
  region_tech_fields_df <- group_by_applnID(ipc_current_period_df) # Returns dplyr tibble
  region_tech_fields_df <- group_by_ctry_and_techn_field(region_tech_fields_df) # Returns tibble
  write.csv2(region_tech_fields_df,
             file = file.path(output_base_dir, paste0("reg_tech_", period_label_file, ".csv")),
             row.names = FALSE)
  
  reg_RCA_df <- tibble(ctry_code = character(0), techn_field_nr = character(0), RCA = numeric(0)) # Initialize
  if (nrow(region_tech_fields_df) > 0) {
    mat_reg_tech <- region_tech_fields_df %>%
      arrange(techn_field_nr, ctry_code) %>%
      pivot_wider(names_from = techn_field_nr, values_from = n_tech_reg, values_fill = 0) %>%
      remove_rownames() %>% column_to_rownames(var = "ctry_code") %>%
      as.matrix() %>% round()
    
    if (nrow(mat_reg_tech) >= 1 && ncol(mat_reg_tech) >= 1 ) {
      lq_result_gen <- tryCatch({ location_quotient(mat_reg_tech, binary = FALSE) },
                                error = function(e) {
                                  message(paste("Error in location_quotient for Gen RCA, period", period_label_display, ": ", e$message)); return(NULL)
                                })
      if(!is.null(lq_result_gen) && nrow(lq_result_gen) > 0) {
        reg_RCA_df <- lq_result_gen %>% as.data.frame() %>%
          rownames_to_column("ctry_code") %>% as_tibble() %>%
          gather("techn_field_nr", "RCA", -ctry_code)
      } else {message(paste("General RCA calculation yielded no results for period", period_label_display))}
    } else {message(paste("Matrix for general RCA is unsuitable for period", period_label_display))}
  } else {message(paste("No aggregated general tech data for period", period_label_display, "- General RCA will be empty."))}
  if(nrow(reg_RCA_df) > 0) reg_RCA_df$techn_field_nr <- as.character(reg_RCA_df$techn_field_nr)
  
  
  # --- Calculate AI-Specific RCA (FOR ACTUAL COUNTRIES) ---
  message("Calculating AI-Specific RCA for actual countries...")
  ai_appln_ids_in_period_dt <- ai_data[priority_year > start_year_filter & priority_year < end_year_filter, .(appln_id = unique(appln_id))]
  reg_RCA_AI_df <- tibble(ctry_code = character(0), techn_field_nr = character(0), RCA = numeric(0)) # Initialize
  
  if (nrow(ai_appln_ids_in_period_dt) == 0) {
    message(paste("No AI patent applications found in ai_data for period", period_label_display, "- AI RCA for actual countries will be empty."))
  } else {
    ai_patents_with_country_and_tech_df <- ipc_current_period_df[ai_appln_ids_in_period_dt, on = "appln_id", nomatch = 0]
    
    # THRESHOLD CHECK for "too few" AI patent observations
    if (nrow(ai_patents_with_country_and_tech_df) == 0) {
      message(paste("AI applications from ai_data found, but none match appln_id in general patents of period", period_label_display, "- AI RCA for actual countries will be empty."))
    } else if (nrow(ai_patents_with_country_and_tech_df) < min_ai_obs_for_rca) {
      message(paste("Fewer than", min_ai_obs_for_rca, "AI patent-techfield observations (found:", nrow(ai_patents_with_country_and_tech_df),
                    ") for period", period_label_display, ". AI RCA for actual countries will be treated as empty (NA)."))
    } else { # Sufficient AI patents, proceed with calculation
      region_tech_fields_ai_df <- group_by_applnID(ai_patents_with_country_and_tech_df) # Returns dplyr tibble
      region_tech_fields_ai_df <- group_by_ctry_and_techn_field(region_tech_fields_ai_df) # Returns tibble
      
      if (nrow(region_tech_fields_ai_df) > 0) {
        mat_reg_tech_AI <- region_tech_fields_ai_df %>%
          arrange(techn_field_nr, ctry_code) %>%
          pivot_wider(names_from = techn_field_nr, values_from = n_tech_reg, values_fill = 0) %>%
          remove_rownames() %>% column_to_rownames(var = "ctry_code") %>%
          as.matrix() %>% round()
        
        if (nrow(mat_reg_tech_AI) >= 1 && ncol(mat_reg_tech_AI) >= 1) {
          lq_result_ai <- tryCatch({ location_quotient(mat_reg_tech_AI, binary = FALSE) },
                                   error = function(e) {
                                     message(paste("Error in location_quotient for AI RCA (actual countries), period", period_label_display, ": ", e$message)); return(NULL)
                                   })
          if(!is.null(lq_result_ai) && nrow(lq_result_ai) > 0) {
            reg_RCA_AI_df <- lq_result_ai %>% as.data.frame() %>%
              rownames_to_column("ctry_code") %>% as_tibble() %>%
              gather(key = "techn_field_nr", value = "RCA", -ctry_code) %>%
              arrange(ctry_code, techn_field_nr)
          } else {message(paste("AI RCA calculation yielded no results for period", period_label_display))}
        } else {message(paste("Matrix for AI RCA (actual countries) is unsuitable for period", period_label_display))}
      } else {message(paste("No aggregated AI tech data (with actual countries) for period", period_label_display))}
    }
  }
  if(nrow(reg_RCA_AI_df) > 0) reg_RCA_AI_df$techn_field_nr <- as.character(reg_RCA_AI_df$techn_field_nr)
  
  
  # --- Merge general and AI-specific RCA data (using dplyr::full_join) ---
  message("Merging RCA data...")
  
  # Rename RCA columns before join for clarity if they exist and have the default "RCA" name
  if(nrow(reg_RCA_df) > 0 && "RCA" %in% names(reg_RCA_df)) {
    reg_RCA_df <- reg_RCA_df %>% rename(RCA_Gen = RCA)
  }
  if(nrow(reg_RCA_AI_df) > 0 && "RCA" %in% names(reg_RCA_AI_df)) {
    reg_RCA_AI_df <- reg_RCA_AI_df %>% rename(RCA_AI = RCA)
  }
  
  if (nrow(reg_RCA_df) > 0 && nrow(reg_RCA_AI_df) > 0) {
    rca_data_this_period_df <- full_join(reg_RCA_df, reg_RCA_AI_df, by = c("ctry_code", "techn_field_nr"))
  } else if (nrow(reg_RCA_df) > 0) {
    rca_data_this_period_df <- reg_RCA_df # Assumes RCA_Gen is already the column name or it was empty
    rca_data_this_period_df$RCA_AI <- NA_real_
  } else if (nrow(reg_RCA_AI_df) > 0) {
    rca_data_this_period_df <- reg_RCA_AI_df # Assumes RCA_AI is already the column name or it was empty
    rca_data_this_period_df$RCA_Gen <- NA_real_
  } else { # Both are effectively empty or resulted in no RCA values
    rca_data_this_period_df <- tibble(ctry_code = character(0), techn_field_nr = character(0), 
                                      RCA_Gen = numeric(0), RCA_AI = numeric(0))
  }
  rca_data_this_period_df$Period <- period_label_display
  
  # Ensure consistent column order and presence for rbindlist
  # This part should now work as rca_data_this_period_df will be a tibble
  expected_cols <- c("ctry_code", "techn_field_nr", "RCA_Gen", "RCA_AI", "Period")
  for(col_name in expected_cols){
    if(!col_name %in% names(rca_data_this_period_df)){
      if(col_name %in% c("RCA_Gen", "RCA_AI")) rca_data_this_period_df[[col_name]] <- NA_real_
      else rca_data_this_period_df[[col_name]] <- NA_character_
    }
  }
  rca_data_this_period_df <- rca_data_this_period_df[, expected_cols, drop = FALSE] # Standard subsetting
  
  
  write.csv2(rca_data_this_period_df,
             file = file.path(output_base_dir, paste0("Data_RCA_techn_field_", period_label_file, ".csv")),
             row.names = FALSE)
  
  # --- Regional Tech counts for AI (under 'AI_pat' country code FOR THIS SPECIFIC OUTPUT) ---
  # (Logic for reg_techAI file remains largely the same as before, ensure it handles empty inputs robustly)
  message("Calculating Regional Tech counts for AI (under 'AI_pat' country code)...")
  ipc_for_reg_tech_ai_count_df <- copy(ipc_current_period_df) 
  setDT(ipc_for_reg_tech_ai_count_df)
  ai_appln_ids_with_marker <- ai_data[priority_year > start_year_filter & priority_year < end_year_filter, .(appln_id, marker_ctry_code = ctry_code)]
  empty_reg_tech_ai_df <- tibble(ctry_code=character(0), techn_field_nr=character(0), n_tech_reg=numeric(0))
  
  if(nrow(ai_appln_ids_with_marker) > 0){
    ipc_for_reg_tech_ai_count_df[ai_appln_ids_with_marker, on = "appln_id", ctry_code_temp_marker := i.marker_ctry_code]
    patents_marked_as_ai_for_count <- ipc_for_reg_tech_ai_count_df[!is.na(ctry_code_temp_marker)]
    if(nrow(patents_marked_as_ai_for_count) > 0) {
      patents_marked_as_ai_for_count[, ctry_code := ctry_code_temp_marker]
      patents_marked_as_ai_for_count[, ctry_code_temp_marker := NULL]
      region_tech_ai_counts_df <- group_by_applnID(patents_marked_as_ai_for_count)
      region_tech_ai_counts_df <- group_by_ctry_and_techn_field(region_tech_ai_counts_df)
      write.csv2(region_tech_ai_counts_df, file = file.path(output_base_dir, paste0("reg_techAI_", period_label_file, ".csv")), row.names = FALSE)
    } else {
      write.csv2(empty_reg_tech_ai_df, file = file.path(output_base_dir, paste0("reg_techAI_", period_label_file, ".csv")), row.names = FALSE)
    }
  } else {
    write.csv2(empty_reg_tech_ai_df, file = file.path(output_base_dir, paste0("reg_techAI_", period_label_file, ".csv")), row.names = FALSE)
  }
  if(exists("ipc_for_reg_tech_ai_count_df")) rm(ipc_for_reg_tech_ai_count_df); if(exists("patents_marked_as_ai_for_count")) rm(patents_marked_as_ai_for_count); gc()
  
  return(rca_data_this_period_df)
}

# --- 3. Define Interval Configurations ---
#for regular 15 years:
periods_config <- list(
  # Original 15-year periods
  list(start_filter=1973, end_filter=1989, label_display="1974-1988", label_file="15_years_1974-1988"),
  list(start_filter=1988, end_filter=2004, label_display="1989-2003", label_file="15_years_1989-2003"),
  list(start_filter=2003, end_filter=2019, label_display="2004-2018", label_file="15_years_2004-2018")
)

#for 10 years:
ten_year_periods_def <- list(
  c(2009, 2018), c(1999, 2008), c(1989, 1998), c(1979, 1988)
)
for (p in ten_year_periods_def) {
  actual_start_year <- p[1]
  actual_end_year <- p[2]
  periods_config[[length(periods_config) + 1]] <- list(
    start_filter = actual_start_year - 1,
    end_filter = actual_end_year + 1,
    label_display = paste0(actual_start_year, "-", actual_end_year),
    label_file = paste0("10_years_", actual_start_year, "-", actual_end_year) 
  )
}

# for 5-years
five_year_periods_def <- list(
  c(2014, 2018), c(2009, 2013), c(2004, 2008), c(1999, 2003), c(1994, 1998),
  c(1989, 1993), c(1984, 1988), c(1979, 1983), c(1974, 1978)
)
for (p in five_year_periods_def) {
  actual_start_year <- p[1]
  actual_end_year <- p[2]
  periods_config[[length(periods_config) + 1]] <- list(
    start_filter = actual_start_year - 1,
    end_filter = actual_end_year + 1,
    label_display = paste0(actual_start_year, "-", actual_end_year),
    label_file = paste0("5_years_", actual_start_year, "-", actual_end_year)
  )
}

# for 1-year
all_single_years <- 1974:2018
for (year_val in all_single_years) {
  periods_config[[length(periods_config) + 1]] <- list(
    start_filter = year_val - 1,
    end_filter = year_val + 1,
    label_display = as.character(year_val),
    label_file = paste0("1_year_", year_val)
  )
}

# --- 4. Loop and Execute ---
all_rca_results <- list()

for (config in periods_config) {
  period_rca_data <- process_rca_for_period(
    start_year_filter = config$start_filter,
    end_year_filter = config$end_filter,
    period_label_display = config$label_display,
    period_label_file = config$label_file,
    all_patents_data = all_patents_combined_df,
    ai_data = ai_patents_df,
    min_ai_obs_for_rca = 10 # threshold of having at least 10 patents
  )
  if (!is.null(period_rca_data)) {
    all_rca_results[[config$label_file]] <- period_rca_data # Use label_file as unique key
  }
  gc() 
}

# --- 5. Combine All Results ---
message("Combining all RCA results...")
final_ipc_rcas_df <- rbindlist(all_rca_results, use.names = TRUE, fill = TRUE)

if (nrow(final_ipc_rcas_df) > 0) {
  write.csv2(final_ipc_rcas_df,
             file = file.path(output_base_dir, "IPC_RCAs_ALL_PERIODS_COMBINED.csv"),
             row.names = FALSE)
  message(paste0("Successfully combined all RCA results and saved to: ",
                 file.path(output_base_dir, "IPC_RCAs_ALL_PERIODS_COMBINED.csv")))
} else {
  message("No RCA results were generated to combine.")
}

message("Processing complete.")

write.csv2(final_ipc_rcas_df, 
           file = "Files_created_with_the_code/data/files_code_Fields_analysis/robustness_automated/final_ipc_rcas_df.csv", 
           row.names = FALSE)

rca_data_period_3_df_old <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/Data3period_RCA_techn_field.csv", 
                                     sep = ";", header = TRUE, dec=",")

rca_data_period_3_df_new <- final_ipc_rcas_df[final_ipc_rcas_df$Period =="2004-2018",]

test_old <- rca_data_period_3_df_old[rca_data_period_3_df_old$ctry_code == "CN",]
test_new <- rca_data_period_3_df_new[rca_data_period_3_df_new$ctry_code == "CN",] #just the RCA_AI is not working;

rca_data_period_3_df_new <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/robustness_automated/reg_techAI_15_years_2004-2018.csv", 
                                     sep = ";", header = TRUE, dec=",")

##1.3.Generate plots break-in shares for distinct intervals -----
library(stringr)
library(tidyverse)

rm(list=ls())
Robustness_data_RTA <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/robustness_automated/final_ipc_rcas_df.csv", 
                                     sep = ";", header = TRUE, dec=",")

# Replace NA with 0 and sum the three parts
Robustness_data_RTA[is.na(Robustness_data_RTA)] <- 0
Threshold <- 1
Robustness_data_RTA$Round_general <- ifelse(Robustness_data_RTA$RCA_Gen < Threshold, 0, 1)
Robustness_data_RTA$Round_AI <- ifelse(Robustness_data_RTA$RCA_AI < Threshold, 0, 1)
#fix Total_RCA:
Robustness_data_RTA$Total_RCA_2 <- Robustness_data_RTA$Round_general + 2*Robustness_data_RTA$Round_AI

#test1a <- Robustness_data_RTA[Robustness_data_RTA$ctry_code == "JP" & Robustness_data_RTA$Period == "1974-1988",]

#pick the 4 countries:
Robustness_data_RTA <- Robustness_data_RTA[Robustness_data_RTA$ctry_code == "JP"|Robustness_data_RTA$ctry_code == "US"|
                                             Robustness_data_RTA$ctry_code == "CN"|Robustness_data_RTA$ctry_code == "KR",]
Robustness_data_RTA$techn_field_nr <- as.character(Robustness_data_RTA$techn_field_nr)

#replace names:
Robustness_data_RTA$ctry_code <- gsub("US", "USA", str_trim(Robustness_data_RTA$ctry_code))
Robustness_data_RTA$ctry_code <- gsub("CN", "China", str_trim(Robustness_data_RTA$ctry_code))
Robustness_data_RTA$ctry_code <- gsub("JP", "Japan", str_trim(Robustness_data_RTA$ctry_code))
Robustness_data_RTA$ctry_code <- gsub("KR", "South Korea", str_trim(Robustness_data_RTA$ctry_code))

###1.3.1. Intervals 15-years -----
Robustness_data_RTA_15_years <- Robustness_data_RTA[Robustness_data_RTA$Period == "1974-1988"|Robustness_data_RTA$Period == "1989-2003"|
                                                      Robustness_data_RTA$Period == "2004-2018", ]
Robustness_data_RTA_15_years$Period_sim <- as.numeric(factor(Robustness_data_RTA_15_years$Period,levels=unique(Robustness_data_RTA_15_years$Period)))

Robustness_data_RTA_15_years$Coiciding <- ifelse(Robustness_data_RTA_15_years$Total_RCA_2 ==3,1,0)
Robustness_data_RTA_15_years$justGeneral <- ifelse(Robustness_data_RTA_15_years$Total_RCA_2 == 1,1,0)
Robustness_data_RTA_15_years$OnlyAI <- ifelse(Robustness_data_RTA_15_years$Total_RCA_2 ==2,1,0)

#now, create a file per country per interval, where I sum over the 3 columns;
IPC_RCAs <- Robustness_data_RTA_15_years
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
  theme_classic() +  geom_line(aes(color=Country, linetype = Country), size=.5)+
  scale_fill_manual(values =  c("#1B9E77", "#D95F02", "#7570B3", "#E7298A")) +
  scale_color_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A")) +
  scale_linetype_manual(values=c("twodash", "longdash", "solid", "solid")) +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, .9))  + 
  labs(title = "a) 15-year intervals")

###1.3.2. Intervals 10-years -----
Robustness_data_RTA_10_years <- Robustness_data_RTA[
  Robustness_data_RTA$Period %in% c("1979-1988",
    "1989-1998",
    "1999-2008",
    "2009-2018"), ]

Robustness_data_RTA_10_years$Period_sim <- as.numeric(factor(Robustness_data_RTA_10_years$Period,levels=unique(Robustness_data_RTA_10_years$Period)))

Robustness_data_RTA_10_years$Coiciding <- ifelse(Robustness_data_RTA_10_years$Total_RCA_2 ==3,1,0)
Robustness_data_RTA_10_years$justGeneral <- ifelse(Robustness_data_RTA_10_years$Total_RCA_2 == 1,1,0)
Robustness_data_RTA_10_years$OnlyAI <- ifelse(Robustness_data_RTA_10_years$Total_RCA_2 ==2,1,0)

#now, create a file per country per interval, where I sum over the 3 columns;
IPC_RCAs <- Robustness_data_RTA_10_years
IPC_RCAs %<>% 
  group_by(ctry_code,Period) %>%   mutate(Share_coinciding = sum(Coiciding)/(sum(Coiciding)+sum(justGeneral))) %>% 
  mutate(Share_OnlyAI = sum(OnlyAI)/(sum(OnlyAI)+sum(Coiciding))) %>% 
  mutate(sum_coinciding = sum(Coiciding)) %>%   mutate(sum_justGeneral = sum(justGeneral)) %>%
  mutate(sum_OnlyAI = sum(OnlyAI)) %>%  ungroup()

SummaryAllData<-distinct(IPC_RCAs, ctry_code, Period, .keep_all = TRUE)
colnames(SummaryAllData)[1] <- "Country"

OverlapTechn_10years<-
  ggplot(data=SummaryAllData, aes(x=Period, y=Share_coinciding, group=Country, shape = Country, color=Country)) +
  geom_point(aes(fill = Country), size=8) +   scale_shape_manual(values=c(21, 22,  24, 23)) +
  xlab("Interval") +  ylab("Share of break-in specialisations (%)") +
  theme_classic() +  geom_line(aes(color=Country, linetype = Country), size=.5)+
  scale_fill_manual(values =  c("#1B9E77", "#D95F02", "#7570B3", "#E7298A")) +
  scale_color_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A")) +
  scale_linetype_manual(values=c("twodash", "longdash", "solid", "solid")) +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, .9))  + 
  labs(title = "b) 10-year intervals")

###1.3.3. Intervals 5-years -----
Robustness_data_RTA_5_years <- Robustness_data_RTA[
  Robustness_data_RTA$Period %in% c(
    "1974-1978",
    "1979-1983",
    "1984-1988",
    "1989-1993",
    "1994-1998",
    "1999-2003",
    "2004-2008",
    "2009-2013",
    "2014-2018"
  ), ]

Robustness_data_RTA_5_years$Period_sim <- as.numeric(factor(Robustness_data_RTA_5_years$Period,levels=unique(Robustness_data_RTA_5_years$Period)))

Robustness_data_RTA_5_years$Coiciding <- ifelse(Robustness_data_RTA_5_years$Total_RCA_2 ==3,1,0)
Robustness_data_RTA_5_years$justGeneral <- ifelse(Robustness_data_RTA_5_years$Total_RCA_2 == 1,1,0)
Robustness_data_RTA_5_years$OnlyAI <- ifelse(Robustness_data_RTA_5_years$Total_RCA_2 ==2,1,0)

#now, create a file per country per interval, where I sum over the 3 columns;
IPC_RCAs <- Robustness_data_RTA_5_years
IPC_RCAs %<>% 
  group_by(ctry_code,Period) %>%   mutate(Share_coinciding = sum(Coiciding)/(sum(Coiciding)+sum(justGeneral))) %>% 
  mutate(Share_OnlyAI = sum(OnlyAI)/(sum(OnlyAI)+sum(Coiciding))) %>% 
  mutate(sum_coinciding = sum(Coiciding)) %>%   mutate(sum_justGeneral = sum(justGeneral)) %>%
  mutate(sum_OnlyAI = sum(OnlyAI)) %>%  ungroup()

SummaryAllData<-distinct(IPC_RCAs, ctry_code, Period, .keep_all = TRUE)
colnames(SummaryAllData)[1] <- "Country"

OverlapTechn_5years<-
  ggplot(data=SummaryAllData, aes(x=Period, y=Share_coinciding, group=Country, shape = Country, color=Country)) +
  geom_point(aes(fill = Country), size=4) +   scale_shape_manual(values=c(21, 22, 24, 23)) +
  xlab("Interval") +  ylab("Share of break-in specialisations (%)") +
  theme_classic() +  geom_line(aes(color=Country, linetype = Country), size=.5)+
  scale_fill_manual(values =  c("#1B9E77", "#D95F02", "#7570B3", "#E7298A")) +
  scale_color_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A")) +
  scale_linetype_manual(values=c("twodash", "longdash", "solid", "solid")) +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, .9))  + 
  labs(title = "c) 5-year intervals")

###1.3.3. Intervals 1-year -----
Robustness_data_RTA_1_years <- Robustness_data_RTA[
  nchar(Robustness_data_RTA$Period) == 4, ]
unique(Robustness_data_RTA_1_years$Period)

Robustness_data_RTA_1_years$Period_sim <- as.numeric(factor(Robustness_data_RTA_1_years$Period,levels=unique(Robustness_data_RTA_1_years$Period)))

Robustness_data_RTA_1_years$Coiciding <- ifelse(Robustness_data_RTA_1_years$Total_RCA_2 ==3,1,0)
Robustness_data_RTA_1_years$justGeneral <- ifelse(Robustness_data_RTA_1_years$Total_RCA_2 == 1,1,0)
Robustness_data_RTA_1_years$OnlyAI <- ifelse(Robustness_data_RTA_1_years$Total_RCA_2 ==2,1,0)

#now, create a file per country per interval, where I sum over the 3 columns;
IPC_RCAs <- Robustness_data_RTA_1_years
IPC_RCAs %<>% 
  group_by(ctry_code,Period) %>%   mutate(Share_coinciding = sum(Coiciding)/(sum(Coiciding)+sum(justGeneral))) %>% 
  mutate(Share_OnlyAI = sum(OnlyAI)/(sum(OnlyAI)+sum(Coiciding))) %>% 
  mutate(sum_coinciding = sum(Coiciding)) %>%   mutate(sum_justGeneral = sum(justGeneral)) %>%
  mutate(sum_OnlyAI = sum(OnlyAI)) %>%  ungroup()

SummaryAllData<-distinct(IPC_RCAs, ctry_code, Period, .keep_all = TRUE)
colnames(SummaryAllData)[1] <- "Country"
SummaryAllData$Period <- as.numeric(SummaryAllData$Period)

OverlapTechn_1years<-
  ggplot(data=SummaryAllData, aes(x=Period, y=Share_coinciding, group=Country, shape = Country, color=Country)) +
  geom_point(aes(fill = Country), size=2) +   scale_shape_manual(values=c(21, 22, 24, 23)) +
  xlab("Interval") +  ylab("Share of break-in specialisations (%)") +
  theme_classic() +  geom_line(aes(color=Country, linetype = Country), size=.5)+
  scale_fill_manual(values =  c("#1B9E77", "#D95F02", "#7570B3", "#E7298A")) +
  scale_color_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A")) +
  scale_linetype_manual(values=c("twodash", "longdash", "solid", "solid")) +
  scale_y_continuous(labels = scales::percent, limits = c(0, .8))   +
  scale_x_discrete(limits = c(1974,2018)) + 
  labs(title = "d) 1-year intervals")

jpeg("Files_created_with_the_code/figures/robustness/Figure_10_break_in_shares.jpg", width = 8, height = 12, units = 'in', res = 300)
grid.arrange(OverlapTechn, OverlapTechn_10years, OverlapTechn_5years, OverlapTechn_1years, ncol = 1)
dev.off()

#SECOND PART: Permutations-------
##2.1.Technological field level -----
###2.1.1. Intervals 15-years -----
rm(list=ls())
#set the working directory to where you saved the R code:
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

##1.1. Calculate Specializations for Different Time intervals
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

###Calculate per interval
#### Interval 1 (1974 - 1988
# Define Time Interval
start_year <- 1973
end_year   <- 1989

###Filter Data for Interval 1 
ipc_all_patents_first_period_df <- ipc_all_patents_part2_df[priority_year > start_year & priority_year < end_year]
ipc_all_patents_first_period_df <- ipc_all_patents_first_period_df[, .(appln_id, ctry_code, techn_field_nr)]

# Calculate AI-Specific RCA for Interval 1
# Load AI-specific data
ai_patents_df <- fread("other_files/IPCs_AI.csv", sep=";", dec=",", header=TRUE)
ai_patents_period_1_df <- ai_patents_df[ai_patents_df$priority_year > start_year & ai_patents_df$priority_year < end_year,]

# Define target countries
target_countries <- c("CN", "JP", "US", "KR") # 

#For MULTIPLE Permutations
num_permutations <- 1000 
list_of_permuted_dfs <- vector("list", length = num_permutations)

for (p in 1:num_permutations) {
  if (p %% 100 == 0) print(paste("Permutation number:", p)) # Progress indicator
  
  # This dataframe will hold the permuted AI patents for target countries ONLY for THIS iteration
  permuted_ai_for_target_countries_iter <- data.frame()
  
  for (country in target_countries) {
    # 1. Identify and Count ACTUAL AI patents for the current country from the original AI dataset
    actual_ai_appln_ids_country <- ai_patents_period_1_df %>%
      filter(ctry_code == country) %>%
      distinct(appln_id) %>%
      pull(appln_id)
    
    n_ai_country <- length(actual_ai_appln_ids_country)
    
    if (n_ai_country == 0) {
      # print(paste("No AI patents found for", country, "in original AI data. Skipping for perm", p))
      next # Skip to the next country if no AI patents to replace
    }
    
    # 2. Prepare the pool of ALL patents for the current country from the general dataset
    country_all_patents_pool <- ipc_all_patents_first_period_df %>%
      filter(ctry_code == country) %>%
      distinct(appln_id)
    
    if (nrow(country_all_patents_pool) == 0) {
      # print(paste("No patents in general pool for", country, ". Skipping for perm", p))
      next
    }
    
    # Handle cases where the pool is smaller than the number of AI patents to sample
    # This is unlikely if ipc_all_patents_first_period_df is complete, but good for robustness
    sample_size <- min(n_ai_country, nrow(country_all_patents_pool))
    replace_sampling <- FALSE
    if (n_ai_country > nrow(country_all_patents_pool)) {
      if(nrow(country_all_patents_pool) < n_ai_country && !replace_sampling){
                next # Skip this country for this permutation if not enough patents
      }
    }
    
    # 3. Randomly select an equivalent number of unique appln_ids from this country's general pool
    random_appln_ids_country <- sample(country_all_patents_pool$appln_id,
                                       size = sample_size, # Use adjusted sample_size
                                       replace = replace_sampling) # Use replace_sampling flag
    
    # 4. Get all rows for these randomly selected patents from the ipc_all_patents_first_period_df
    randomly_selected_patents_df_country <- ipc_all_patents_first_period_df %>%
      filter(appln_id %in% random_appln_ids_country & ctry_code == country)
    
    # 5. Add these randomly selected patents for the current country to the iteration's df
    if (nrow(randomly_selected_patents_df_country) > 0) {
      permuted_ai_for_target_countries_iter <- bind_rows(
        permuted_ai_for_target_countries_iter,
        randomly_selected_patents_df_country
      )
    }
  } # End of country loop
  
  # Add the permutation number to all rows of this iteration's dataframe
  if (nrow(permuted_ai_for_target_countries_iter) > 0) {
    permuted_ai_for_target_countries_iter$permutation_number <- p
  }
  
  # Store the dataframe for this iteration in the list
  list_of_permuted_dfs[[p]] <- permuted_ai_for_target_countries_iter
  
} # End of permutation loop

# Combine all permuted dataframes from the list into one large dataframe
final_permuted_dataset <- bind_rows(list_of_permuted_dfs)

#pick the original AI patents and save them as a final_permuted_dataset$permutation_number == 0 (so
#that I can double-check in the end that the calculation remains correct);
#then, separate unique appln_ids, as it was being done already, merge it back to the dataset
#and finally, rbind, for each permutation, the original patents that are NOT from the selected countries, 
'%notin%' <- Negate('%in%')
not_selected_AI <- ai_patents_period_1_df[ai_patents_period_1_df$ctry_code %notin% target_countries, c("appln_id", "ctry_code")]
table(not_selected_AI$ctry_code)
#repeat the original dataset of non-selected countries a num_permutations number of times;

# Create a list to hold the replicated dataframes
list_of_replicated_not_selected_ai <- vector("list", length = num_permutations + 1)

# Loop from 0 to num_permutations
for (i in 0:num_permutations) {
  # Create a copy of the not_selected_AI dataframe for this iteration
  temp_df <- not_selected_AI
  
  # Add the permutation_number column
  temp_df$permutation_number <- i
  
  # Store it in the list (adjust index because list is 1-based, i is 0-based)
  list_of_replicated_not_selected_ai[[i + 1]] <- temp_df
  
  if (i %% 100 == 0) print(paste("Replicating not_selected_AI for permutation_number:", i))
}

# Combine all replicated dataframes from the list into one large dataframe
replicated_not_selected_ai_final <- bind_rows(list_of_replicated_not_selected_ai)
table(replicated_not_selected_ai_final$permutation_number)

original_selected_AI <- ai_patents_period_1_df[ai_patents_period_1_df$ctry_code %in% target_countries, c("appln_id", "ctry_code")]
table(original_selected_AI$ctry_code)
original_selected_AI$permutation_number <- 0

#exclude the techn_field column (I don't need it for now)
final_permuted_dataset <- subset(final_permuted_dataset, select = -c(techn_field_nr) )
#merge both in the "reference" dataset, when permutation = 0
original_selected_AI <- rbind(original_selected_AI, replicated_not_selected_ai_final)
rm(replicated_not_selected_ai_final)
table(original_selected_AI$ctry_code) #the non selected countries appear 11 times more, because they appear already for all the permutations
table(ai_patents_period_1_df$ctry_code) 
table(original_selected_AI$permutation_number)
length(unique(original_selected_AI$appln_id)) #435
length(unique(ai_patents_period_1_df$appln_id)) #435

#rbind it with the original dataset:
final_permuted_dataset <- rbind(original_selected_AI,final_permuted_dataset) 
table(final_permuted_dataset$permutation_number)

# Join technological fields info to AI patents
final_permuted_dataset <- distinct_at(final_permuted_dataset, vars(appln_id,permutation_number), .keep_all = T)
final_permuted_dataset <- subset(final_permuted_dataset, select = -c(ctry_code) )

final_permuted_dataset <- left_join(final_permuted_dataset, ipc_all_patents_first_period_df, by = "appln_id")

#4. Change the RTA formula so that it calculates RTAs per permutation_number (I could possible just apply a group_by(permutation_number) or so
#5. then, I should check if the calculation is correct for the original dataset when permutation_number == 0, and if so,
#I plot these 1000 points  for each of the 4 countries, throw a regression line on the ones where 
#permutation_number above 0 (i.e., the artificial ones), and throw another line on permutation_number = 0 and use it as a reference;
group_by_applnID_perm <- function(data){
  data %>%
    group_by(appln_id, permutation_number) %>% # Add permutation_number if an appln_id might exist across permutations
    # If appln_id is unique within each permutation, then just appln_id is fine
    mutate(field_weight = 1 / n()) %>%
    ungroup()
}

group_by_ctry_techn_field_perm <- function(data){
  data %>% # Removed %<>% as it's better to assign explicitly in the main flow
    group_by(permutation_number, ctry_code, techn_field_nr) %>% # Add permutation_number
    summarise(n_tech_reg = sum(field_weight), .groups = 'drop') %>% # Use .groups = 'drop'
    # ungroup() %>% # .groups = 'drop' handles this
    drop_na()
}

#Main Workflow incorporating permutations

# 1. Compute weighted fields for permuted AI patents
print("Step 1: Calculating field weights...")
region_tech_fields_perm_df <- group_by_applnID_perm(final_permuted_dataset)

# 2. Aggregate at the permutation-country-technology field level
print("Step 2: Aggregating by country and technology field per permutation...")
region_tech_fields_perm_df <- group_by_ctry_techn_field_perm(region_tech_fields_perm_df)
print("Counts of records per permutation_number after aggregation:")
print(table(region_tech_fields_perm_df$permutation_number))

# 3. Process per permutation for matrix creation and RCA calculation
#    This is the trickiest part. We need to do the pivot_wider and location_quotient
#    for each permutation_number separately.
#    We can use group_by() %>% group_split() %>% map() or group_by() %>% do()

print("Step 3: Calculating RCA per permutation...")

# Using dplyr::group_by and then purrr::map or a loop
# The result will be a list of dataframes (one RCA df per permutation)
list_of_rca_dfs <- region_tech_fields_perm_df %>%
  group_by(permutation_number) %>%
  group_split() %>% # This splits the df into a list of dfs, one for each permutation
  purrr::map(~{
    current_permutation_number <- unique(.x$permutation_number)
    print(paste("Processing RCA for permutation_number:", current_permutation_number))
    
    # Matrix creation for the current permutation's data
    mat_reg_tech_perm_AI <- .x %>%
      select(-permutation_number) %>% # Temporarily remove for pivot if it causes issues
      arrange(techn_field_nr, ctry_code) %>%
      pivot_wider(names_from = techn_field_nr,
                  values_from = n_tech_reg,
                  values_fill = 0) # Changed from list(n_tech_reg = 0) for simplicity
    
    # Check if ctry_code column exists and is not empty
    if (!"ctry_code" %in% names(mat_reg_tech_perm_AI) || nrow(mat_reg_tech_perm_AI) == 0 || all(is.na(mat_reg_tech_perm_AI$ctry_code))) {
      print(paste("Skipping permutation", current_permutation_number, "due to missing ctry_code or empty data after pivot."))
      return(NULL) # Return NULL or an empty tibble
    }
    
    # Check for duplicate ctry_codes which would prevent rownames_to_column
    if (any(duplicated(mat_reg_tech_perm_AI$ctry_code))) {
      print(paste("Warning: Duplicate ctry_code found for permutation", current_permutation_number, ". Aggregating or handling needed."))
      return(tibble(permutation_number = current_permutation_number, error="duplicate ctry_code"))
    }
    
    
    mat_reg_tech_perm_AI <- mat_reg_tech_perm_AI %>%
      remove_rownames() %>%
      column_to_rownames(var = "ctry_code") %>%
      as.matrix() %>%  round()# No rounding here, location_quotient might prefer raw numbers
    
    # RTA calculation
    # Ensure matrix is suitable (e.g., no NA/NaN/Inf that location_quotient can't handle)
    if (nrow(mat_reg_tech_perm_AI) == 0 || ncol(mat_reg_tech_perm_AI) == 0) {
      print(paste("Skipping RCA for permutation", current_permutation_number, "due to empty matrix."))
      return(NULL)
    }
    
    # Ensure there are at least two columns for location_quotient (ctry_code was one)
    if (ncol(mat_reg_tech_perm_AI) < 1) { # If only ctry_code was present and now it's rownames
      print(paste("Skipping RCA for permutation", current_permutation_number, "due to insufficient columns in matrix."))
      return(NULL)
    }
    
    
    # Check for all zero rows/columns if location_quotient is sensitive
    rca_results_perm <- tryCatch({
      mat_reg_tech_perm_AI %>%
        location_quotient(binary = FALSE) %>% 
        as.data.frame() %>%
        rownames_to_column("ctry_code") %>%
        as_tibble() %>%
        gather(key = "techn_field_nr", value = "RCA", -ctry_code) %>%
        arrange(ctry_code, techn_field_nr) %>%
        mutate(permutation_number = current_permutation_number) # Add back permutation number
    }, error = function(e) {
      print(paste("Error in location_quotient for permutation", current_permutation_number, ":", e$message))
      return(tibble(permutation_number = current_permutation_number, ctry_code=NA, techn_field_nr=NA, RCA=NA, error_message = e$message)) # Return an empty or error-marked tibble
    })
    
    return(rca_results_perm)
  })

# Combine the list of RCA dataframes into one final dataframe
final_rca_all_permutations_df <- bind_rows(list_of_rca_dfs)

# Check for errors in the error column
if("error_message" %in% names(final_rca_all_permutations_df)){
  print("Permutations with errors during RCA calculation:")
  print(final_rca_all_permutations_df %>% filter(!is.na(error_message)))
}

write.csv2(final_rca_all_permutations_df, 
           file = "Files_created_with_the_code/data/files_code_Fields_analysis/robustness/final_rca_all_permutations_df_1st_Period.csv", 
           row.names = FALSE)

#so, final_rca_all_permutations_df is the result....
historical_ai <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/RCA_4countries_detailed.csv", 
                          sep = ";", header = TRUE, dec=",")
historical_ai <- historical_ai[historical_ai$Period == "1974-1988",]
historical_ai <- historical_ai[historical_ai$ctry_code == "JP"|historical_ai$ctry_code == "US",]

test <- final_rca_all_permutations_df[final_rca_all_permutations_df$permutation_number == 0,]
test <- test[test$ctry_code == "JP"|test$ctry_code == "US"|
               test$ctry_code == "CN"|test$ctry_code == "KR",]
sum(historical_ai$RCA_AI) #43.35699
sum(test$RCA) #43.35699

rm(final_rca_all_permutations_df, historical_ai, ipc_all_patents_first_period_df, ai_patents_period_1_df,
   country_all_patents_pool, final_permuted_dataset, test, list_of_permuted_dfs,list_of_rca_dfs, 
   list_of_replicated_not_selected_ai, not_selected_AI, original_selected_AI, permuted_ai_for_target_countries_iter, 
   randomly_selected_patents_df_country,region_tech_fields_perm_df, temp_df)

####Interval 2 (1989 - 2003
# Define Time Interval 
start_year <- 1988
end_year   <- 2004

###Filter Data for Interval 2 
ipc_all_patents_second_period_df <- ipc_all_patents_part2_df[priority_year > start_year & priority_year < end_year]
ipc_all_patents_second_period_df <- ipc_all_patents_second_period_df[, .(appln_id, ctry_code, techn_field_nr)]

# Calculate AI-Specific RCA for Interval 2
ai_patents_period_2_df <- ai_patents_df[ai_patents_df$priority_year > start_year & ai_patents_df$priority_year < end_year,]

list_of_permuted_dfs <- vector("list", length = num_permutations)

for (p in 1:num_permutations) {
  if (p %% 100 == 0) print(paste("Permutation number:", p)) # Progress indicator
  
  # This dataframe will hold the permuted AI patents for target countries ONLY for THIS iteration
  permuted_ai_for_target_countries_iter <- data.frame()
  
  for (country in target_countries) {
    # 1. Identify and Count ACTUAL AI patents for the current country from the original AI dataset
    actual_ai_appln_ids_country <- ai_patents_period_2_df %>%
      filter(ctry_code == country) %>%
      distinct(appln_id) %>%
      pull(appln_id)
    
    n_ai_country <- length(actual_ai_appln_ids_country)
    
    if (n_ai_country == 0) {
      # print(paste("No AI patents found for", country, "in original AI data. Skipping for perm", p))
      next # Skip to the next country if no AI patents to replace
    }
    
    # 2. Prepare the pool of ALL patents for the current country from the general dataset
    country_all_patents_pool <- ipc_all_patents_second_period_df %>%
      filter(ctry_code == country) %>%
      distinct(appln_id)
    
    if (nrow(country_all_patents_pool) == 0) {
      # print(paste("No patents in general pool for", country, ". Skipping for perm", p))
      next
    }
    
    # Handle cases where the pool is smaller than the number of AI patents to sample
    # This is unlikely if ipc_all_patents_second_period_df is complete, but good for robustness
    sample_size <- min(n_ai_country, nrow(country_all_patents_pool))
    replace_sampling <- FALSE
    if (n_ai_country > nrow(country_all_patents_pool)) {
      if(nrow(country_all_patents_pool) < n_ai_country && !replace_sampling){
        next # Skip this country for this permutation if not enough patents
      }
    }
    
    # 3. Randomly select an equivalent number of unique appln_ids from this country's general pool
    random_appln_ids_country <- sample(country_all_patents_pool$appln_id,
                                       size = sample_size, # Use adjusted sample_size
                                       replace = replace_sampling) # Use replace_sampling flag
    
    # 4. Get all rows for these randomly selected patents from the ipc_all_patents_second_period_df
    randomly_selected_patents_df_country <- ipc_all_patents_second_period_df %>%
      filter(appln_id %in% random_appln_ids_country & ctry_code == country)
    
    # 5. Add these randomly selected patents for the current country to the iteration's df
    if (nrow(randomly_selected_patents_df_country) > 0) {
      permuted_ai_for_target_countries_iter <- bind_rows(
        permuted_ai_for_target_countries_iter,
        randomly_selected_patents_df_country
      )
    }
  } # End of country loop
  
  # Add the permutation number to all rows of this iteration's dataframe
  if (nrow(permuted_ai_for_target_countries_iter) > 0) {
    permuted_ai_for_target_countries_iter$permutation_number <- p
  }
  
  # Store the dataframe for this iteration in the list
  list_of_permuted_dfs[[p]] <- permuted_ai_for_target_countries_iter
  
} # End of permutation loop

# Combine all permuted dataframes from the list into one large dataframe
final_permuted_dataset <- bind_rows(list_of_permuted_dfs)

not_selected_AI <- ai_patents_period_2_df[ai_patents_period_2_df$ctry_code %notin% target_countries, c("appln_id", "ctry_code")]

#repeat the original dataset of non-selected countries a num_permutations number of times;
# Create a list to hold the replicated dataframes
list_of_replicated_not_selected_ai <- vector("list", length = num_permutations + 1)

# Loop from 0 to num_permutations
for (i in 0:num_permutations) {
  # Create a copy of the not_selected_AI dataframe for this iteration
  temp_df <- not_selected_AI
  
  # Add the permutation_number column
  temp_df$permutation_number <- i
  
  # Store it in the list (adjust index because list is 1-based, i is 0-based)
  list_of_replicated_not_selected_ai[[i + 1]] <- temp_df
  
  if (i %% 100 == 0) print(paste("Replicating not_selected_AI for permutation_number:", i))
}

# Combine all replicated dataframes from the list into one large dataframe
replicated_not_selected_ai_final <- bind_rows(list_of_replicated_not_selected_ai)
table(replicated_not_selected_ai_final$ctry_code) #everything but selected countries, as expected
table(replicated_not_selected_ai_final$permutation_number)

original_selected_AI <- ai_patents_period_2_df[ai_patents_period_2_df$ctry_code %in% target_countries, c("appln_id", "ctry_code")]
table(original_selected_AI$ctry_code)
original_selected_AI$permutation_number <- 0

#exclude the techn_field column (I don't need it for now)
final_permuted_dataset <- subset(final_permuted_dataset, select = -c(techn_field_nr) )
#merge both in the "reference" dataset, when permutation = 0
original_selected_AI <- rbind(original_selected_AI, replicated_not_selected_ai_final)
rm(replicated_not_selected_ai_final)
table(original_selected_AI$ctry_code) #the non selected countries appear 11 times more, because they appear already for all the permutations
table(ai_patents_period_2_df$ctry_code) 
table(original_selected_AI$permutation_number)
length(unique(original_selected_AI$appln_id)) #7887
length(unique(ai_patents_period_2_df$appln_id)) #7887

#rbind it with the original dataset:
final_permuted_dataset <- rbind(original_selected_AI,final_permuted_dataset) 
table(final_permuted_dataset$permutation_number)

# Join technological fields info to AI patents
final_permuted_dataset <- distinct_at(final_permuted_dataset, vars(appln_id,permutation_number), .keep_all = T)
final_permuted_dataset <- subset(final_permuted_dataset, select = -c(ctry_code) )

final_permuted_dataset <- left_join(final_permuted_dataset, ipc_all_patents_second_period_df, by = "appln_id")

# 1. Compute weighted fields for permuted AI patents
print("Step 1: Calculating field weights...")
region_tech_fields_perm_df <- group_by_applnID_perm(final_permuted_dataset)

# 2. Aggregate at the permutation-country-technology field level
print("Step 2: Aggregating by country and technology field per permutation...")
region_tech_fields_perm_df <- group_by_ctry_techn_field_perm(region_tech_fields_perm_df)
print("Counts of records per permutation_number after aggregation:")
print(table(region_tech_fields_perm_df$permutation_number))

# 3. Process per permutation for matrix creation and RCA calculation
#    This is the trickiest part. We need to do the pivot_wider and location_quotient
#    for each permutation_number separately.
#    We can use group_by() %>% group_split() %>% map() or group_by() %>% do()

print("Step 3: Calculating RCA per permutation...")

# Using dplyr::group_by and then purrr::map or a loop
# The result will be a list of dataframes (one RCA df per permutation)
list_of_rca_dfs <- region_tech_fields_perm_df %>%
  group_by(permutation_number) %>%
  group_split() %>% # This splits the df into a list of dfs, one for each permutation
  purrr::map(~{
    current_permutation_number <- unique(.x$permutation_number)
    print(paste("Processing RCA for permutation_number:", current_permutation_number))
    
    # Matrix creation for the current permutation's data
    mat_reg_tech_perm_AI <- .x %>%
      select(-permutation_number) %>% # Temporarily remove for pivot if it causes issues
      arrange(techn_field_nr, ctry_code) %>%
      pivot_wider(names_from = techn_field_nr,
                  values_from = n_tech_reg,
                  values_fill = 0) # Changed from list(n_tech_reg = 0) for simplicity
    
    # Check if ctry_code column exists and is not empty
    if (!"ctry_code" %in% names(mat_reg_tech_perm_AI) || nrow(mat_reg_tech_perm_AI) == 0 || all(is.na(mat_reg_tech_perm_AI$ctry_code))) {
      print(paste("Skipping permutation", current_permutation_number, "due to missing ctry_code or empty data after pivot."))
      return(NULL) # Return NULL or an empty tibble
    }
    
    # Check for duplicate ctry_codes which would prevent rownames_to_column
    if (any(duplicated(mat_reg_tech_perm_AI$ctry_code))) {
      print(paste("Warning: Duplicate ctry_code found for permutation", current_permutation_number, ". Aggregating or handling needed."))
      return(tibble(permutation_number = current_permutation_number, error="duplicate ctry_code"))
    }
    
    
    mat_reg_tech_perm_AI <- mat_reg_tech_perm_AI %>%
      remove_rownames() %>%
      column_to_rownames(var = "ctry_code") %>%
      as.matrix() %>%  round()# No rounding here, location_quotient might prefer raw numbers
    
    # RTA calculation
    # Ensure matrix is suitable (e.g., no NA/NaN/Inf that location_quotient can't handle)
    if (nrow(mat_reg_tech_perm_AI) == 0 || ncol(mat_reg_tech_perm_AI) == 0) {
      print(paste("Skipping RCA for permutation", current_permutation_number, "due to empty matrix."))
      return(NULL)
    }
    
    # Ensure there are at least two columns for location_quotient (ctry_code was one)
    if (ncol(mat_reg_tech_perm_AI) < 1) { # If only ctry_code was present and now it's rownames
      print(paste("Skipping RCA for permutation", current_permutation_number, "due to insufficient columns in matrix."))
      return(NULL)
    }
    
    
    # Check for all zero rows/columns if location_quotient is sensitive
    rca_results_perm <- tryCatch({
      mat_reg_tech_perm_AI %>%
        location_quotient(binary = FALSE) %>% 
        as.data.frame() %>%
        rownames_to_column("ctry_code") %>%
        as_tibble() %>%
        gather(key = "techn_field_nr", value = "RCA", -ctry_code) %>%
        arrange(ctry_code, techn_field_nr) %>%
        mutate(permutation_number = current_permutation_number) # Add back permutation number
    }, error = function(e) {
      print(paste("Error in location_quotient for permutation", current_permutation_number, ":", e$message))
      return(tibble(permutation_number = current_permutation_number, ctry_code=NA, techn_field_nr=NA, RCA=NA, error_message = e$message)) # Return an empty or error-marked tibble
    })
    
    return(rca_results_perm)
  })

# Combine the list of RCA dataframes into one final dataframe
final_rca_all_permutations_df <- bind_rows(list_of_rca_dfs)

# Check for errors in the error column
if("error_message" %in% names(final_rca_all_permutations_df)){
  print("Permutations with errors during RCA calculation:")
  print(final_rca_all_permutations_df %>% filter(!is.na(error_message)))
}

write.csv2(final_rca_all_permutations_df, 
           file = "Files_created_with_the_code/data/files_code_Fields_analysis/robustness/final_rca_all_permutations_df_2nd_Period.csv", 
           row.names = FALSE)

historical_ai <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/RCA_4countries_detailed.csv", 
                          sep = ";", header = TRUE, dec=",")
historical_ai <- historical_ai[historical_ai$Period == "1989-2003",]
historical_ai <- historical_ai[historical_ai$ctry_code == "JP"|historical_ai$ctry_code == "US",]

test <- final_rca_all_permutations_df[final_rca_all_permutations_df$permutation_number == 0,]
test <- test[test$ctry_code == "JP"|test$ctry_code == "US",]
sum(historical_ai$RCA_AI) #64.86376
sum(test$RCA) #64.86376

rm(final_rca_all_permutations_df, historical_ai, ipc_all_patents_second_period_df, ai_patents_period_2_df,
   country_all_patents_pool, final_permuted_dataset, test, list_of_permuted_dfs,list_of_rca_dfs, 
   list_of_replicated_not_selected_ai, not_selected_AI, original_selected_AI, permuted_ai_for_target_countries_iter, 
   randomly_selected_patents_df_country,region_tech_fields_perm_df, temp_df)
rm(ipc_all_patents_part2_df)

#### Interval 3 (2004 - 2018
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

start_year <- 2003
end_year   <- 2019

# Calculate AI-Specific RCA for Interval 3
ai_patents_period_3_df <- ai_patents_df[ai_patents_df$priority_year > start_year & ai_patents_df$priority_year < end_year,]

list_of_permuted_dfs <- vector("list", length = num_permutations)

for (p in 1:num_permutations) {
  if (p %% 100 == 0) print(paste("Permutation number:", p)) # Progress indicator
  
  # This dataframe will hold the permuted AI patents for target countries ONLY for THIS iteration
  permuted_ai_for_target_countries_iter <- data.frame()
  
  for (country in target_countries) {
    # 1. Identify and Count ACTUAL AI patents for the current country from the original AI dataset
    actual_ai_appln_ids_country <- ai_patents_period_3_df %>%
      filter(ctry_code == country) %>%
      distinct(appln_id) %>%
      pull(appln_id)
    
    n_ai_country <- length(actual_ai_appln_ids_country)
    
    if (n_ai_country == 0) {
      # print(paste("No AI patents found for", country, "in original AI data. Skipping for perm", p))
      next # Skip to the next country if no AI patents to replace
    }
    
    # 2. Prepare the pool of ALL patents for the current country from the general dataset
    country_all_patents_pool <- ipc_all_patents_third_period_df %>%
      filter(ctry_code == country) %>%
      distinct(appln_id)
    
    if (nrow(country_all_patents_pool) == 0) {
      # print(paste("No patents in general pool for", country, ". Skipping for perm", p))
      next
    }
    
    # Handle cases where the pool is smaller than the number of AI patents to sample
    # This is unlikely if ipc_all_patents_third_period_df is complete, but good for robustness
    sample_size <- min(n_ai_country, nrow(country_all_patents_pool))
    replace_sampling <- FALSE
    if (n_ai_country > nrow(country_all_patents_pool)) {
      if(nrow(country_all_patents_pool) < n_ai_country && !replace_sampling){
        next # Skip this country for this permutation if not enough patents
      }
    }
    
    # 3. Randomly select an equivalent number of unique appln_ids from this country's general pool
    random_appln_ids_country <- sample(country_all_patents_pool$appln_id,
                                       size = sample_size, # Use adjusted sample_size
                                       replace = replace_sampling) # Use replace_sampling flag
    
    # 4. Get all rows for these randomly selected patents from the ipc_all_patents_third_period_df
    randomly_selected_patents_df_country <- ipc_all_patents_third_period_df %>%
      filter(appln_id %in% random_appln_ids_country & ctry_code == country)
    
    # 5. Add these randomly selected patents for the current country to the iteration's df
    if (nrow(randomly_selected_patents_df_country) > 0) {
      permuted_ai_for_target_countries_iter <- bind_rows(
        permuted_ai_for_target_countries_iter,
        randomly_selected_patents_df_country
      )
    }
  } # End of country loop
  
  # Add the permutation number to all rows of this iteration's dataframe
  if (nrow(permuted_ai_for_target_countries_iter) > 0) {
    permuted_ai_for_target_countries_iter$permutation_number <- p
  }
  
  # Store the dataframe for this iteration in the list
  list_of_permuted_dfs[[p]] <- permuted_ai_for_target_countries_iter
  
} # End of permutation loop

# Combine all permuted dataframes from the list into one large dataframe
final_permuted_dataset <- bind_rows(list_of_permuted_dfs)

not_selected_AI <- ai_patents_period_3_df[ai_patents_period_3_df$ctry_code %notin% target_countries, c("appln_id", "ctry_code")]

#repeat the original dataset of non-selected countries a num_permutations number of times;
# Create a list to hold the replicated dataframes
list_of_replicated_not_selected_ai <- vector("list", length = num_permutations + 1)

# Loop from 0 to num_permutations
for (i in 0:num_permutations) {
  # Create a copy of the not_selected_AI dataframe for this iteration
  temp_df <- not_selected_AI
  
  # Add the permutation_number column
  temp_df$permutation_number <- i
  
  # Store it in the list (adjust index because list is 1-based, i is 0-based)
  list_of_replicated_not_selected_ai[[i + 1]] <- temp_df
  
  if (i %% 100 == 0) print(paste("Replicating not_selected_AI for permutation_number:", i))
}

# Combine all replicated dataframes from the list into one large dataframe
replicated_not_selected_ai_final <- bind_rows(list_of_replicated_not_selected_ai)
table(replicated_not_selected_ai_final$meuctry_code) #everything but selected countries, as expected
table(replicated_not_selected_ai_final$permutation_number)

original_selected_AI <- ai_patents_period_3_df[ai_patents_period_3_df$ctry_code %in% target_countries, c("appln_id", "ctry_code")]
table(original_selected_AI$ctry_code)
original_selected_AI$permutation_number <- 0

#exclude the techn_field column (I don't need it for now)
final_permuted_dataset <- subset(final_permuted_dataset, select = -c(techn_field_nr) )
#merge both in the "reference" dataset, when permutation = 0
original_selected_AI <- rbind(original_selected_AI, replicated_not_selected_ai_final)
rm(replicated_not_selected_ai_final)
table(original_selected_AI$ctry_code) #the non selected countries appear 11 times more, because they appear already for all the permutations
table(ai_patents_period_3_df$ctry_code) 
table(original_selected_AI$permutation_number)
length(unique(original_selected_AI$appln_id)) #34576
length(unique(ai_patents_period_3_df$appln_id)) #34576

#rbind it with the original dataset:
final_permuted_dataset <- rbind(original_selected_AI,final_permuted_dataset) 
table(final_permuted_dataset$permutation_number)

# Join technological fields info to AI patents
final_permuted_dataset <- distinct_at(final_permuted_dataset, vars(appln_id,permutation_number), .keep_all = T)
final_permuted_dataset <- subset(final_permuted_dataset, select = -c(ctry_code) )

final_permuted_dataset <- left_join(final_permuted_dataset, ipc_all_patents_third_period_df, by = "appln_id")

# 1. Compute weighted fields for permuted AI patents
print("Step 1: Calculating field weights...")
region_tech_fields_perm_df <- group_by_applnID_perm(final_permuted_dataset)

# 2. Aggregate at the permutation-country-technology field level
print("Step 2: Aggregating by country and technology field per permutation...")
region_tech_fields_perm_df <- group_by_ctry_techn_field_perm(region_tech_fields_perm_df)
print("Counts of records per permutation_number after aggregation:")
print(table(region_tech_fields_perm_df$permutation_number))

# 3. Process per permutation for matrix creation and RCA calculation
#    This is the trickiest part. We need to do the pivot_wider and location_quotient
#    for each permutation_number separately.
#    We can use group_by() %>% group_split() %>% map() or group_by() %>% do()

print("Step 3: Calculating RCA per permutation...")

# Using dplyr::group_by and then purrr::map or a loop
# The result will be a list of dataframes (one RCA df per permutation)
list_of_rca_dfs <- region_tech_fields_perm_df %>%
  group_by(permutation_number) %>%
  group_split() %>% # This splits the df into a list of dfs, one for each permutation
  purrr::map(~{
    current_permutation_number <- unique(.x$permutation_number)
    print(paste("Processing RCA for permutation_number:", current_permutation_number))
    
    # Matrix creation for the current permutation's data
    mat_reg_tech_perm_AI <- .x %>%
      select(-permutation_number) %>% # Temporarily remove for pivot if it causes issues
      arrange(techn_field_nr, ctry_code) %>%
      pivot_wider(names_from = techn_field_nr,
                  values_from = n_tech_reg,
                  values_fill = 0) # Changed from list(n_tech_reg = 0) for simplicity
    
    # Check if ctry_code column exists and is not empty
    if (!"ctry_code" %in% names(mat_reg_tech_perm_AI) || nrow(mat_reg_tech_perm_AI) == 0 || all(is.na(mat_reg_tech_perm_AI$ctry_code))) {
      print(paste("Skipping permutation", current_permutation_number, "due to missing ctry_code or empty data after pivot."))
      return(NULL) # Return NULL or an empty tibble
    }
    
    # Check for duplicate ctry_codes which would prevent rownames_to_column
    if (any(duplicated(mat_reg_tech_perm_AI$ctry_code))) {
      print(paste("Warning: Duplicate ctry_code found for permutation", current_permutation_number, ". Aggregating or handling needed."))
      return(tibble(permutation_number = current_permutation_number, error="duplicate ctry_code"))
    }
    
    
    mat_reg_tech_perm_AI <- mat_reg_tech_perm_AI %>%
      remove_rownames() %>%
      column_to_rownames(var = "ctry_code") %>%
      as.matrix() %>%  round()# No rounding here, location_quotient might prefer raw numbers
    
    # RTA calculation
    # Ensure matrix is suitable (e.g., no NA/NaN/Inf that location_quotient can't handle)
    if (nrow(mat_reg_tech_perm_AI) == 0 || ncol(mat_reg_tech_perm_AI) == 0) {
      print(paste("Skipping RCA for permutation", current_permutation_number, "due to empty matrix."))
      return(NULL)
    }
    
    # Ensure there are at least two columns for location_quotient (ctry_code was one)
    if (ncol(mat_reg_tech_perm_AI) < 1) { # If only ctry_code was present and now it's rownames
      print(paste("Skipping RCA for permutation", current_permutation_number, "due to insufficient columns in matrix."))
      return(NULL)
    }
    
    
    # Check for all zero rows/columns if location_quotient is sensitive
    rca_results_perm <- tryCatch({
      mat_reg_tech_perm_AI %>%
        location_quotient(binary = FALSE) %>% 
        as.data.frame() %>%
        rownames_to_column("ctry_code") %>%
        as_tibble() %>%
        gather(key = "techn_field_nr", value = "RCA", -ctry_code) %>%
        arrange(ctry_code, techn_field_nr) %>%
        mutate(permutation_number = current_permutation_number) # Add back permutation number
    }, error = function(e) {
      print(paste("Error in location_quotient for permutation", current_permutation_number, ":", e$message))
      return(tibble(permutation_number = current_permutation_number, ctry_code=NA, techn_field_nr=NA, RCA=NA, error_message = e$message)) # Return an empty or error-marked tibble
    })
    
    return(rca_results_perm)
  })

# Combine the list of RCA dataframes into one final dataframe
final_rca_all_permutations_df <- bind_rows(list_of_rca_dfs)

# Check for errors in the error column
if("error_message" %in% names(final_rca_all_permutations_df)){
  print("Permutations with errors during RCA calculation:")
  print(final_rca_all_permutations_df %>% filter(!is.na(error_message)))
}

write.csv2(final_rca_all_permutations_df, 
           file = "Files_created_with_the_code/data/files_code_Fields_analysis/robustness/final_rca_all_permutations_df_3rd_Period.csv", 
           row.names = FALSE)

historical_ai <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/RCA_4countries_detailed.csv", 
                          sep = ";", header = TRUE, dec=",")
historical_ai <- historical_ai[historical_ai$Period == "2004-2018",]
historical_ai <- historical_ai[historical_ai$ctry_code == "JP"|historical_ai$ctry_code == "US"|
                                 historical_ai$ctry_code == "CN"|historical_ai$ctry_code == "KR",]

test <- final_rca_all_permutations_df[final_rca_all_permutations_df$permutation_number == 0,]
test <- test[test$ctry_code == "JP"|test$ctry_code == "US"|
               test$ctry_code == "CN"|test$ctry_code == "KR",]
sum(historical_ai$RCA_AI) #162.1932
sum(test$RCA) #162.1932

###2.1.2. Intervals 5-years ------
rm(list=ls())
#set the working directory to where you saved the R code:
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

##1.1. Calculate Specializations for Different Time intervals
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

#Calculate per 5-years interval
#1 "1974-1978",
#2 "1979-1983",
#3 "1984-1988",
#4 "1989-1993",
#5 "1994-1998",
#6 "1999-2003",
#7 "2004-2008", #second dataset starts here
#8 "2009-2013",
#9 "2014-2018"
##4.1. Interval 1 (1974-1978)
# Define Time Interval
start_year <- 1973
end_year   <- 1979

###Filter Data for Interval 1 
ipc_all_patents_first_period_df <- ipc_all_patents_part2_df[priority_year > start_year & priority_year < end_year]
ipc_all_patents_first_period_df <- ipc_all_patents_first_period_df[, .(appln_id, ctry_code, techn_field_nr)]

# Calculate AI-Specific RCA for Interval 1
# Load AI-specific data
ai_patents_df <- fread("other_files/IPCs_AI.csv", sep=";", dec=",", header=TRUE)
ai_patents_period_1_df <- ai_patents_df[ai_patents_df$priority_year > start_year & ai_patents_df$priority_year < end_year,]

# Define target countries
target_countries <- c("CN", "JP", "US", "KR") # 

#For MULTIPLE Permutations
num_permutations <- 1000 
list_of_permuted_dfs <- vector("list", length = num_permutations)

for (p in 1:num_permutations) {
  if (p %% 100 == 0) print(paste("Permutation number:", p)) # Progress indicator
  
  # This dataframe will hold the permuted AI patents for target countries ONLY for THIS iteration
  permuted_ai_for_target_countries_iter <- data.frame()
  
  for (country in target_countries) {
    # 1. Identify and Count ACTUAL AI patents for the current country from the original AI dataset
    actual_ai_appln_ids_country <- ai_patents_period_1_df %>%
      filter(ctry_code == country) %>%
      distinct(appln_id) %>%
      pull(appln_id)
    
    n_ai_country <- length(actual_ai_appln_ids_country)
    
    if (n_ai_country == 0) {
      # print(paste("No AI patents found for", country, "in original AI data. Skipping for perm", p))
      next # Skip to the next country if no AI patents to replace
    }
    
    # 2. Prepare the pool of ALL patents for the current country from the general dataset
    country_all_patents_pool <- ipc_all_patents_first_period_df %>%
      filter(ctry_code == country) %>%
      distinct(appln_id)
    
    if (nrow(country_all_patents_pool) == 0) {
      # print(paste("No patents in general pool for", country, ". Skipping for perm", p))
      next
    }
    
    # Handle cases where the pool is smaller than the number of AI patents to sample
    # This is unlikely if ipc_all_patents_first_period_df is complete, but good for robustness
    sample_size <- min(n_ai_country, nrow(country_all_patents_pool))
    replace_sampling <- FALSE
    if (n_ai_country > nrow(country_all_patents_pool)) {
      if(nrow(country_all_patents_pool) < n_ai_country && !replace_sampling){
        next # Skip this country for this permutation if not enough patents
      }
    }
    
    # 3. Randomly select an equivalent number of unique appln_ids from this country's general pool
    random_appln_ids_country <- sample(country_all_patents_pool$appln_id,
                                       size = sample_size, # Use adjusted sample_size
                                       replace = replace_sampling) # Use replace_sampling flag
    
    # 4. Get all rows for these randomly selected patents from the ipc_all_patents_first_period_df
    randomly_selected_patents_df_country <- ipc_all_patents_first_period_df %>%
      filter(appln_id %in% random_appln_ids_country & ctry_code == country)
    
    # 5. Add these randomly selected patents for the current country to the iteration's df
    if (nrow(randomly_selected_patents_df_country) > 0) {
      permuted_ai_for_target_countries_iter <- bind_rows(
        permuted_ai_for_target_countries_iter,
        randomly_selected_patents_df_country
      )
    }
  } # End of country loop
  
  # Add the permutation number to all rows of this iteration's dataframe
  if (nrow(permuted_ai_for_target_countries_iter) > 0) {
    permuted_ai_for_target_countries_iter$permutation_number <- p
  }
  
  # Store the dataframe for this iteration in the list
  list_of_permuted_dfs[[p]] <- permuted_ai_for_target_countries_iter
  
} # End of permutation loop

# Combine all permuted dataframes from the list into one large dataframe
final_permuted_dataset <- bind_rows(list_of_permuted_dfs)

#pick the original AI patents and save them as a final_permuted_dataset$permutation_number == 0 (so
#that I can double-check in the end that the calculation remains correct);
#then, separate unique appln_ids, as it was being done already, merge it back to the dataset
#and finally, rbind, for each permutation, the original patents that are NOT from the selected countries, 
'%notin%' <- Negate('%in%')
not_selected_AI <- ai_patents_period_1_df[ai_patents_period_1_df$ctry_code %notin% target_countries, c("appln_id", "ctry_code")]
table(not_selected_AI$ctry_code)
#repeat the original dataset of non-selected countries a num_permutations number of times;

# Create a list to hold the replicated dataframes
list_of_replicated_not_selected_ai <- vector("list", length = num_permutations + 1)

# Loop from 0 to num_permutations
for (i in 0:num_permutations) {
  # Create a copy of the not_selected_AI dataframe for this iteration
  temp_df <- not_selected_AI
  
  # Add the permutation_number column
  temp_df$permutation_number <- i
  
  # Store it in the list (adjust index because list is 1-based, i is 0-based)
  list_of_replicated_not_selected_ai[[i + 1]] <- temp_df
  
  if (i %% 100 == 0) print(paste("Replicating not_selected_AI for permutation_number:", i))
}

# Combine all replicated dataframes from the list into one large dataframe
replicated_not_selected_ai_final <- bind_rows(list_of_replicated_not_selected_ai)
table(replicated_not_selected_ai_final$permutation_number)

original_selected_AI <- ai_patents_period_1_df[ai_patents_period_1_df$ctry_code %in% target_countries, c("appln_id", "ctry_code")]
table(original_selected_AI$ctry_code)
original_selected_AI$permutation_number <- 0

#exclude the techn_field column (I don't need it for now)
final_permuted_dataset <- subset(final_permuted_dataset, select = -c(techn_field_nr) )
#merge both in the "reference" dataset, when permutation = 0
original_selected_AI <- rbind(original_selected_AI, replicated_not_selected_ai_final)
rm(replicated_not_selected_ai_final)
table(original_selected_AI$ctry_code) #the non selected countries appear 11 times more, because they appear already for all the permutations
table(ai_patents_period_1_df$ctry_code) 
table(original_selected_AI$permutation_number)
length(unique(original_selected_AI$appln_id)) #435
length(unique(ai_patents_period_1_df$appln_id)) #435

#rbind it with the original dataset:
final_permuted_dataset <- rbind(original_selected_AI,final_permuted_dataset) 
table(final_permuted_dataset$permutation_number)

#final_permuted_dataset2<- final_permuted_dataset
#pick only the unique appln_id 
#final_permuted_dataset$ctry_code <- "AI_pat"
#table(final_permuted_dataset$ctry_code)
#backup<-final_permuted_dataset

# Join technological fields info to AI patents
final_permuted_dataset <- distinct_at(final_permuted_dataset, vars(appln_id,permutation_number), .keep_all = T)
final_permuted_dataset <- subset(final_permuted_dataset, select = -c(ctry_code) )

final_permuted_dataset <- left_join(final_permuted_dataset, ipc_all_patents_first_period_df, by = "appln_id")
#drop repeated information
#attention here 1
#if I drop the repeated information, the RCA deviates slightly from the original one, because I didn't do it in the original (it's not necessarily needed, since even repeated information has some value)
#final_permuted_dataset <- distinct_at(final_permuted_dataset, vars(appln_id,permutation_number, ctry_code,techn_field_nr), .keep_all = T)

#4. Change the RTA formula so that it calculates RTAs per permutation_number (I could possible just apply a group_by(permutation_number) or so
#5. then, I should check if the calculation is correct for the original dataset when permutation_number == 0, and if so,
#I plot these 1000 points  for each of the 4 countries, throw a regression line on the ones where 
#permutation_number above 0 (i.e., the artificial ones), and throw another line on permutation_number = 0 and use it as a reference;
group_by_applnID_perm <- function(data){
  data %>%
    group_by(appln_id, permutation_number) %>% # Add permutation_number if an appln_id might exist across permutations
    # If appln_id is unique within each permutation, then just appln_id is fine
    mutate(field_weight = 1 / n()) %>%
    ungroup()
}

group_by_ctry_techn_field_perm <- function(data){
  data %>% # Removed %<>% as it's better to assign explicitly in the main flow
    group_by(permutation_number, ctry_code, techn_field_nr) %>% # Add permutation_number
    summarise(n_tech_reg = sum(field_weight), .groups = 'drop') %>% # Use .groups = 'drop'
    # ungroup() %>% # .groups = 'drop' handles this
    drop_na()
}

#Main Workflow incorporating permutations

# 1. Compute weighted fields for permuted AI patents
print("Step 1: Calculating field weights...")
region_tech_fields_perm_df <- group_by_applnID_perm(final_permuted_dataset)

# 2. Aggregate at the permutation-country-technology field level
print("Step 2: Aggregating by country and technology field per permutation...")
region_tech_fields_perm_df <- group_by_ctry_techn_field_perm(region_tech_fields_perm_df)
print("Counts of records per permutation_number after aggregation:")
print(table(region_tech_fields_perm_df$permutation_number))

# 3. Process per permutation for matrix creation and RCA calculation
#    This is the trickiest part. We need to do the pivot_wider and location_quotient
#    for each permutation_number separately.
#    We can use group_by() %>% group_split() %>% map() or group_by() %>% do()

print("Step 3: Calculating RCA per permutation...")

# Using dplyr::group_by and then purrr::map or a loop
# The result will be a list of dataframes (one RCA df per permutation)
list_of_rca_dfs <- region_tech_fields_perm_df %>%
  group_by(permutation_number) %>%
  group_split() %>% # This splits the df into a list of dfs, one for each permutation
  purrr::map(~{
    current_permutation_number <- unique(.x$permutation_number)
    print(paste("Processing RCA for permutation_number:", current_permutation_number))
    
    # Matrix creation for the current permutation's data
    mat_reg_tech_perm_AI <- .x %>%
      select(-permutation_number) %>% # Temporarily remove for pivot if it causes issues
      arrange(techn_field_nr, ctry_code) %>%
      pivot_wider(names_from = techn_field_nr,
                  values_from = n_tech_reg,
                  values_fill = 0) # Changed from list(n_tech_reg = 0) for simplicity
    
    # Check if ctry_code column exists and is not empty
    if (!"ctry_code" %in% names(mat_reg_tech_perm_AI) || nrow(mat_reg_tech_perm_AI) == 0 || all(is.na(mat_reg_tech_perm_AI$ctry_code))) {
      print(paste("Skipping permutation", current_permutation_number, "due to missing ctry_code or empty data after pivot."))
      return(NULL) # Return NULL or an empty tibble
    }
    
    # Check for duplicate ctry_codes which would prevent rownames_to_column
    if (any(duplicated(mat_reg_tech_perm_AI$ctry_code))) {
      print(paste("Warning: Duplicate ctry_code found for permutation", current_permutation_number, ". Aggregating or handling needed."))
      return(tibble(permutation_number = current_permutation_number, error="duplicate ctry_code"))
    }
    
    
    mat_reg_tech_perm_AI <- mat_reg_tech_perm_AI %>%
      remove_rownames() %>%
      column_to_rownames(var = "ctry_code") %>%
      as.matrix() %>%  round()# No rounding here, location_quotient might prefer raw numbers
    
    # RTA calculation
    # Ensure matrix is suitable (e.g., no NA/NaN/Inf that location_quotient can't handle)
    if (nrow(mat_reg_tech_perm_AI) == 0 || ncol(mat_reg_tech_perm_AI) == 0) {
      print(paste("Skipping RCA for permutation", current_permutation_number, "due to empty matrix."))
      return(NULL)
    }
    
    # Ensure there are at least two columns for location_quotient (ctry_code was one)
    if (ncol(mat_reg_tech_perm_AI) < 1) { # If only ctry_code was present and now it's rownames
      print(paste("Skipping RCA for permutation", current_permutation_number, "due to insufficient columns in matrix."))
      return(NULL)
    }
    
    
    # Check for all zero rows/columns if location_quotient is sensitive
    rca_results_perm <- tryCatch({
      mat_reg_tech_perm_AI %>%
        location_quotient(binary = FALSE) %>% 
        as.data.frame() %>%
        rownames_to_column("ctry_code") %>%
        as_tibble() %>%
        gather(key = "techn_field_nr", value = "RCA", -ctry_code) %>%
        arrange(ctry_code, techn_field_nr) %>%
        mutate(permutation_number = current_permutation_number) # Add back permutation number
    }, error = function(e) {
      print(paste("Error in location_quotient for permutation", current_permutation_number, ":", e$message))
      return(tibble(permutation_number = current_permutation_number, ctry_code=NA, techn_field_nr=NA, RCA=NA, error_message = e$message)) # Return an empty or error-marked tibble
    })
    
    return(rca_results_perm)
  })

# Combine the list of RCA dataframes into one final dataframe
final_rca_all_permutations_df <- bind_rows(list_of_rca_dfs)

# Check for errors in the error column
if("error_message" %in% names(final_rca_all_permutations_df)){
  print("Permutations with errors during RCA calculation:")
  print(final_rca_all_permutations_df %>% filter(!is.na(error_message)))
}

write.csv2(final_rca_all_permutations_df, 
           file = "Files_created_with_the_code/data/files_code_Fields_analysis/robustness/final_rca_all_permutations_df_5y_1st_Period.csv", 
           row.names = FALSE)

#so, final_rca_all_permutations_df is the result....
Robustness_data_RTA <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/robustness_automated/final_ipc_rcas_df.csv", 
                                sep = ";", header = TRUE, dec=",")

# Replace NA with 0 and sum the three parts
Robustness_data_RTA[is.na(Robustness_data_RTA)] <- 0
Robustness_data_RTA_5_years <- Robustness_data_RTA[
  Robustness_data_RTA$Period %in% c(
    "1974-1978",
    "1979-1983",
    "1984-1988",
    "1989-1993",
    "1994-1998",
    "1999-2003",
    "2004-2008",
    "2009-2013",
    "2014-2018"
  ), ]
rm(Robustness_data_RTA)

Robustness_data_RTA_5_years_1st <- Robustness_data_RTA_5_years[Robustness_data_RTA_5_years$Period == "1974-1978",]
Robustness_data_RTA_5_years_1st <- Robustness_data_RTA_5_years_1st[Robustness_data_RTA_5_years_1st$ctry_code == "JP"|
                                                                     Robustness_data_RTA_5_years_1st$ctry_code == "US"|
                                                                     Robustness_data_RTA_5_years_1st$ctry_code == "CN"|
                                                                     Robustness_data_RTA_5_years_1st$ctry_code == "KR",]

test <- final_rca_all_permutations_df[final_rca_all_permutations_df$permutation_number == 0,]
test <- test[test$ctry_code == "JP"|test$ctry_code == "US"|
               test$ctry_code == "CN"|test$ctry_code == "KR",]
sum(Robustness_data_RTA_5_years_1st$RCA_AI) #8
sum(test$RCA) #8

rm(final_rca_all_permutations_df, Robustness_data_RTA_5_years_1st, ipc_all_patents_first_period_df, ai_patents_period_1_df,
   country_all_patents_pool, final_permuted_dataset, test, list_of_permuted_dfs,list_of_rca_dfs, 
   list_of_replicated_not_selected_ai, not_selected_AI, original_selected_AI, permuted_ai_for_target_countries_iter, 
   randomly_selected_patents_df_country,region_tech_fields_perm_df, temp_df)

##Interval 2 (1979-1983
##"1979-1983",

# Define Time Interval 
start_year <- 1978
end_year   <- 1984

###Filter Data for Interval 2 
ipc_all_patents_second_period_df <- ipc_all_patents_part2_df[priority_year > start_year & priority_year < end_year]
ipc_all_patents_second_period_df <- ipc_all_patents_second_period_df[, .(appln_id, ctry_code, techn_field_nr)]

# Calculate AI-Specific RCA for Interval 2
ai_patents_period_2_df <- ai_patents_df[ai_patents_df$priority_year > start_year & ai_patents_df$priority_year < end_year,]

list_of_permuted_dfs <- vector("list", length = num_permutations)

for (p in 1:num_permutations) {
  if (p %% 100 == 0) print(paste("Permutation number:", p)) # Progress indicator
  
  # This dataframe will hold the permuted AI patents for target countries ONLY for THIS iteration
  permuted_ai_for_target_countries_iter <- data.frame()
  
  for (country in target_countries) {
    # 1. Identify and Count ACTUAL AI patents for the current country from the original AI dataset
    actual_ai_appln_ids_country <- ai_patents_period_2_df %>%
      filter(ctry_code == country) %>%
      distinct(appln_id) %>%
      pull(appln_id)
    
    n_ai_country <- length(actual_ai_appln_ids_country)
    
    if (n_ai_country == 0) {
      # print(paste("No AI patents found for", country, "in original AI data. Skipping for perm", p))
      next # Skip to the next country if no AI patents to replace
    }
    
    # 2. Prepare the pool of ALL patents for the current country from the general dataset
    country_all_patents_pool <- ipc_all_patents_second_period_df %>%
      filter(ctry_code == country) %>%
      distinct(appln_id)
    
    if (nrow(country_all_patents_pool) == 0) {
      # print(paste("No patents in general pool for", country, ". Skipping for perm", p))
      next
    }
    
    # Handle cases where the pool is smaller than the number of AI patents to sample
    # This is unlikely if ipc_all_patents_second_period_df is complete, but good for robustness
    sample_size <- min(n_ai_country, nrow(country_all_patents_pool))
    replace_sampling <- FALSE
    if (n_ai_country > nrow(country_all_patents_pool)) {
      if(nrow(country_all_patents_pool) < n_ai_country && !replace_sampling){
        next # Skip this country for this permutation if not enough patents
      }
    }
    
    # 3. Randomly select an equivalent number of unique appln_ids from this country's general pool
    random_appln_ids_country <- sample(country_all_patents_pool$appln_id,
                                       size = sample_size, # Use adjusted sample_size
                                       replace = replace_sampling) # Use replace_sampling flag
    
    # 4. Get all rows for these randomly selected patents from the ipc_all_patents_second_period_df
    randomly_selected_patents_df_country <- ipc_all_patents_second_period_df %>%
      filter(appln_id %in% random_appln_ids_country & ctry_code == country)
    
    # 5. Add these randomly selected patents for the current country to the iteration's df
    if (nrow(randomly_selected_patents_df_country) > 0) {
      permuted_ai_for_target_countries_iter <- bind_rows(
        permuted_ai_for_target_countries_iter,
        randomly_selected_patents_df_country
      )
    }
  } # End of country loop
  
  # Add the permutation number to all rows of this iteration's dataframe
  if (nrow(permuted_ai_for_target_countries_iter) > 0) {
    permuted_ai_for_target_countries_iter$permutation_number <- p
  }
  
  # Store the dataframe for this iteration in the list
  list_of_permuted_dfs[[p]] <- permuted_ai_for_target_countries_iter
  
} # End of permutation loop

# Combine all permuted dataframes from the list into one large dataframe
final_permuted_dataset <- bind_rows(list_of_permuted_dfs)

not_selected_AI <- ai_patents_period_2_df[ai_patents_period_2_df$ctry_code %notin% target_countries, c("appln_id", "ctry_code")]

#repeat the original dataset of non-selected countries a num_permutations number of times;
# Create a list to hold the replicated dataframes
list_of_replicated_not_selected_ai <- vector("list", length = num_permutations + 1)

# Loop from 0 to num_permutations
for (i in 0:num_permutations) {
  # Create a copy of the not_selected_AI dataframe for this iteration
  temp_df <- not_selected_AI
  
  # Add the permutation_number column
  temp_df$permutation_number <- i
  
  # Store it in the list (adjust index because list is 1-based, i is 0-based)
  list_of_replicated_not_selected_ai[[i + 1]] <- temp_df
  
  if (i %% 100 == 0) print(paste("Replicating not_selected_AI for permutation_number:", i))
}

# Combine all replicated dataframes from the list into one large dataframe
replicated_not_selected_ai_final <- bind_rows(list_of_replicated_not_selected_ai)
table(replicated_not_selected_ai_final$ctry_code) #everything but selected countries, as expected
table(replicated_not_selected_ai_final$permutation_number)

original_selected_AI <- ai_patents_period_2_df[ai_patents_period_2_df$ctry_code %in% target_countries, c("appln_id", "ctry_code")]
table(original_selected_AI$ctry_code)
original_selected_AI$permutation_number <- 0

#exclude the techn_field column (I don't need it for now)
final_permuted_dataset <- subset(final_permuted_dataset, select = -c(techn_field_nr) )
#merge both in the "reference" dataset, when permutation = 0
original_selected_AI <- rbind(original_selected_AI, replicated_not_selected_ai_final)
rm(replicated_not_selected_ai_final)
table(original_selected_AI$ctry_code) #the non selected countries appear 11 times more, because they appear already for all the permutations
table(ai_patents_period_2_df$ctry_code) 
table(original_selected_AI$permutation_number)
length(unique(original_selected_AI$appln_id)) #7887
length(unique(ai_patents_period_2_df$appln_id)) #7887

#rbind it with the original dataset:
final_permuted_dataset <- rbind(original_selected_AI,final_permuted_dataset) 
table(final_permuted_dataset$permutation_number)

# Join technological fields info to AI patents
final_permuted_dataset <- distinct_at(final_permuted_dataset, vars(appln_id,permutation_number), .keep_all = T)
final_permuted_dataset <- subset(final_permuted_dataset, select = -c(ctry_code) )

final_permuted_dataset <- left_join(final_permuted_dataset, ipc_all_patents_second_period_df, by = "appln_id")
#drop repeated information
#attention here 2
#if I drop the repeated information, the RCA deviates slightly from the original one, because I didn't do it in the original (it's not necessarily needed, since even repeated information has some value)
#final_permuted_dataset <- distinct_at(final_permuted_dataset, vars(appln_id,permutation_number, ctry_code,techn_field_nr), .keep_all = T)

# 1. Compute weighted fields for permuted AI patents
print("Step 1: Calculating field weights...")
region_tech_fields_perm_df <- group_by_applnID_perm(final_permuted_dataset)

# 2. Aggregate at the permutation-country-technology field level
print("Step 2: Aggregating by country and technology field per permutation...")
region_tech_fields_perm_df <- group_by_ctry_techn_field_perm(region_tech_fields_perm_df)
print("Counts of records per permutation_number after aggregation:")
print(table(region_tech_fields_perm_df$permutation_number))

# 3. Process per permutation for matrix creation and RCA calculation
#    This is the trickiest part. We need to do the pivot_wider and location_quotient
#    for each permutation_number separately.
#    We can use group_by() %>% group_split() %>% map() or group_by() %>% do()

print("Step 3: Calculating RCA per permutation...")

# Using dplyr::group_by and then purrr::map or a loop
# The result will be a list of dataframes (one RCA df per permutation)
list_of_rca_dfs <- region_tech_fields_perm_df %>%
  group_by(permutation_number) %>%
  group_split() %>% # This splits the df into a list of dfs, one for each permutation
  purrr::map(~{
    current_permutation_number <- unique(.x$permutation_number)
    print(paste("Processing RCA for permutation_number:", current_permutation_number))
    
    # Matrix creation for the current permutation's data
    mat_reg_tech_perm_AI <- .x %>%
      select(-permutation_number) %>% # Temporarily remove for pivot if it causes issues
      arrange(techn_field_nr, ctry_code) %>%
      pivot_wider(names_from = techn_field_nr,
                  values_from = n_tech_reg,
                  values_fill = 0) # Changed from list(n_tech_reg = 0) for simplicity
    
    # Check if ctry_code column exists and is not empty
    if (!"ctry_code" %in% names(mat_reg_tech_perm_AI) || nrow(mat_reg_tech_perm_AI) == 0 || all(is.na(mat_reg_tech_perm_AI$ctry_code))) {
      print(paste("Skipping permutation", current_permutation_number, "due to missing ctry_code or empty data after pivot."))
      return(NULL) # Return NULL or an empty tibble
    }
    
    # Check for duplicate ctry_codes which would prevent rownames_to_column
    if (any(duplicated(mat_reg_tech_perm_AI$ctry_code))) {
      print(paste("Warning: Duplicate ctry_code found for permutation", current_permutation_number, ". Aggregating or handling needed."))
      return(tibble(permutation_number = current_permutation_number, error="duplicate ctry_code"))
    }
    
    
    mat_reg_tech_perm_AI <- mat_reg_tech_perm_AI %>%
      remove_rownames() %>%
      column_to_rownames(var = "ctry_code") %>%
      as.matrix() %>%  round()# No rounding here, location_quotient might prefer raw numbers
    
    # RTA calculation
    # Ensure matrix is suitable (e.g., no NA/NaN/Inf that location_quotient can't handle)
    if (nrow(mat_reg_tech_perm_AI) == 0 || ncol(mat_reg_tech_perm_AI) == 0) {
      print(paste("Skipping RCA for permutation", current_permutation_number, "due to empty matrix."))
      return(NULL)
    }
    
    # Ensure there are at least two columns for location_quotient (ctry_code was one)
    if (ncol(mat_reg_tech_perm_AI) < 1) { # If only ctry_code was present and now it's rownames
      print(paste("Skipping RCA for permutation", current_permutation_number, "due to insufficient columns in matrix."))
      return(NULL)
    }
    
    
    # Check for all zero rows/columns if location_quotient is sensitive
    rca_results_perm <- tryCatch({
      mat_reg_tech_perm_AI %>%
        location_quotient(binary = FALSE) %>% 
        as.data.frame() %>%
        rownames_to_column("ctry_code") %>%
        as_tibble() %>%
        gather(key = "techn_field_nr", value = "RCA", -ctry_code) %>%
        arrange(ctry_code, techn_field_nr) %>%
        mutate(permutation_number = current_permutation_number) # Add back permutation number
    }, error = function(e) {
      print(paste("Error in location_quotient for permutation", current_permutation_number, ":", e$message))
      return(tibble(permutation_number = current_permutation_number, ctry_code=NA, techn_field_nr=NA, RCA=NA, error_message = e$message)) # Return an empty or error-marked tibble
    })
    
    return(rca_results_perm)
  })

# Combine the list of RCA dataframes into one final dataframe
final_rca_all_permutations_df <- bind_rows(list_of_rca_dfs)

# Check for errors in the error column
if("error_message" %in% names(final_rca_all_permutations_df)){
  print("Permutations with errors during RCA calculation:")
  print(final_rca_all_permutations_df %>% filter(!is.na(error_message)))
}

write.csv2(final_rca_all_permutations_df, 
           file = "Files_created_with_the_code/data/files_code_Fields_analysis/robustness/final_rca_all_permutations_df_5y_2nd_Period.csv", 
           row.names = FALSE)

Robustness_data_RTA_5_years_2nd <- Robustness_data_RTA_5_years[Robustness_data_RTA_5_years$Period == "1979-1983",]
Robustness_data_RTA_5_years_2nd <- Robustness_data_RTA_5_years_2nd[Robustness_data_RTA_5_years_2nd$ctry_code == "JP"|
                                                                     Robustness_data_RTA_5_years_2nd$ctry_code == "US"|
                                                                     Robustness_data_RTA_5_years_2nd$ctry_code == "CN"|
                                                                     Robustness_data_RTA_5_years_2nd$ctry_code == "KR",]

test <- final_rca_all_permutations_df[final_rca_all_permutations_df$permutation_number == 0,]
test <- test[test$ctry_code == "JP"|test$ctry_code == "US"|
               test$ctry_code == "CN"|test$ctry_code == "KR",]
sum(Robustness_data_RTA_5_years_2nd$RCA_AI) #12.22222
sum(test$RCA) #12.22222

rm(final_rca_all_permutations_df, Robustness_data_RTA_5_years_2nd, ipc_all_patents_second_period_df, ai_patents_period_2_df,
   country_all_patents_pool, final_permuted_dataset, test, list_of_permuted_dfs,list_of_rca_dfs, 
   list_of_replicated_not_selected_ai, not_selected_AI, original_selected_AI, permuted_ai_for_target_countries_iter, 
   randomly_selected_patents_df_country,region_tech_fields_perm_df, temp_df)

## Interval 3 (1984-1988
#"1984-1988",

# Define Time Interval 
start_year <- 1983
end_year   <- 1989

###Filter Data for Interval 2 
ipc_all_patents_second_period_df <- ipc_all_patents_part2_df[priority_year > start_year & priority_year < end_year]
ipc_all_patents_second_period_df <- ipc_all_patents_second_period_df[, .(appln_id, ctry_code, techn_field_nr)]

# Calculate AI-Specific RCA for Interval 2
ai_patents_period_2_df <- ai_patents_df[ai_patents_df$priority_year > start_year & ai_patents_df$priority_year < end_year,]

list_of_permuted_dfs <- vector("list", length = num_permutations)

for (p in 1:num_permutations) {
  if (p %% 100 == 0) print(paste("Permutation number:", p)) # Progress indicator
  
  # This dataframe will hold the permuted AI patents for target countries ONLY for THIS iteration
  permuted_ai_for_target_countries_iter <- data.frame()
  
  for (country in target_countries) {
    # 1. Identify and Count ACTUAL AI patents for the current country from the original AI dataset
    actual_ai_appln_ids_country <- ai_patents_period_2_df %>%
      filter(ctry_code == country) %>%
      distinct(appln_id) %>%
      pull(appln_id)
    
    n_ai_country <- length(actual_ai_appln_ids_country)
    
    if (n_ai_country == 0) {
      # print(paste("No AI patents found for", country, "in original AI data. Skipping for perm", p))
      next # Skip to the next country if no AI patents to replace
    }
    
    # 2. Prepare the pool of ALL patents for the current country from the general dataset
    country_all_patents_pool <- ipc_all_patents_second_period_df %>%
      filter(ctry_code == country) %>%
      distinct(appln_id)
    
    if (nrow(country_all_patents_pool) == 0) {
      # print(paste("No patents in general pool for", country, ". Skipping for perm", p))
      next
    }
    
    # Handle cases where the pool is smaller than the number of AI patents to sample
    # This is unlikely if ipc_all_patents_second_period_df is complete, but good for robustness
    sample_size <- min(n_ai_country, nrow(country_all_patents_pool))
    replace_sampling <- FALSE
    if (n_ai_country > nrow(country_all_patents_pool)) {
      if(nrow(country_all_patents_pool) < n_ai_country && !replace_sampling){
        next # Skip this country for this permutation if not enough patents
      }
    }
    
    # 3. Randomly select an equivalent number of unique appln_ids from this country's general pool
    random_appln_ids_country <- sample(country_all_patents_pool$appln_id,
                                       size = sample_size, # Use adjusted sample_size
                                       replace = replace_sampling) # Use replace_sampling flag
    
    # 4. Get all rows for these randomly selected patents from the ipc_all_patents_second_period_df
    randomly_selected_patents_df_country <- ipc_all_patents_second_period_df %>%
      filter(appln_id %in% random_appln_ids_country & ctry_code == country)
    
    # 5. Add these randomly selected patents for the current country to the iteration's df
    if (nrow(randomly_selected_patents_df_country) > 0) {
      permuted_ai_for_target_countries_iter <- bind_rows(
        permuted_ai_for_target_countries_iter,
        randomly_selected_patents_df_country
      )
    }
  } # End of country loop
  
  # Add the permutation number to all rows of this iteration's dataframe
  if (nrow(permuted_ai_for_target_countries_iter) > 0) {
    permuted_ai_for_target_countries_iter$permutation_number <- p
  }
  
  # Store the dataframe for this iteration in the list
  list_of_permuted_dfs[[p]] <- permuted_ai_for_target_countries_iter
  
} # End of permutation loop

# Combine all permuted dataframes from the list into one large dataframe
final_permuted_dataset <- bind_rows(list_of_permuted_dfs)

not_selected_AI <- ai_patents_period_2_df[ai_patents_period_2_df$ctry_code %notin% target_countries, c("appln_id", "ctry_code")]

#repeat the original dataset of non-selected countries a num_permutations number of times;
# Create a list to hold the replicated dataframes
list_of_replicated_not_selected_ai <- vector("list", length = num_permutations + 1)

# Loop from 0 to num_permutations
for (i in 0:num_permutations) {
  # Create a copy of the not_selected_AI dataframe for this iteration
  temp_df <- not_selected_AI
  
  # Add the permutation_number column
  temp_df$permutation_number <- i
  
  # Store it in the list (adjust index because list is 1-based, i is 0-based)
  list_of_replicated_not_selected_ai[[i + 1]] <- temp_df
  
  if (i %% 100 == 0) print(paste("Replicating not_selected_AI for permutation_number:", i))
}

# Combine all replicated dataframes from the list into one large dataframe
replicated_not_selected_ai_final <- bind_rows(list_of_replicated_not_selected_ai)
table(replicated_not_selected_ai_final$ctry_code) #everything but selected countries, as expected
table(replicated_not_selected_ai_final$permutation_number)

original_selected_AI <- ai_patents_period_2_df[ai_patents_period_2_df$ctry_code %in% target_countries, c("appln_id", "ctry_code")]
table(original_selected_AI$ctry_code)
original_selected_AI$permutation_number <- 0

#exclude the techn_field column (I don't need it for now)
final_permuted_dataset <- subset(final_permuted_dataset, select = -c(techn_field_nr) )
#merge both in the "reference" dataset, when permutation = 0
original_selected_AI <- rbind(original_selected_AI, replicated_not_selected_ai_final)
rm(replicated_not_selected_ai_final)
table(original_selected_AI$ctry_code) #the non selected countries appear 11 times more, because they appear already for all the permutations
table(ai_patents_period_2_df$ctry_code) 
table(original_selected_AI$permutation_number)
length(unique(original_selected_AI$appln_id)) #7887
length(unique(ai_patents_period_2_df$appln_id)) #7887

#rbind it with the original dataset:
final_permuted_dataset <- rbind(original_selected_AI,final_permuted_dataset) 
table(final_permuted_dataset$permutation_number)

# Join technological fields info to AI patents
final_permuted_dataset <- distinct_at(final_permuted_dataset, vars(appln_id,permutation_number), .keep_all = T)
final_permuted_dataset <- subset(final_permuted_dataset, select = -c(ctry_code) )

final_permuted_dataset <- left_join(final_permuted_dataset, ipc_all_patents_second_period_df, by = "appln_id")
#drop repeated information
#attention here 2
#if I drop the repeated information, the RCA deviates slightly from the original one, because I didn't do it in the original (it's not necessarily needed, since even repeated information has some value)
#final_permuted_dataset <- distinct_at(final_permuted_dataset, vars(appln_id,permutation_number, ctry_code,techn_field_nr), .keep_all = T)

# 1. Compute weighted fields for permuted AI patents
print("Step 1: Calculating field weights...")
region_tech_fields_perm_df <- group_by_applnID_perm(final_permuted_dataset)

# 2. Aggregate at the permutation-country-technology field level
print("Step 2: Aggregating by country and technology field per permutation...")
region_tech_fields_perm_df <- group_by_ctry_techn_field_perm(region_tech_fields_perm_df)
print("Counts of records per permutation_number after aggregation:")
print(table(region_tech_fields_perm_df$permutation_number))

# 3. Process per permutation for matrix creation and RCA calculation
#    This is the trickiest part. We need to do the pivot_wider and location_quotient
#    for each permutation_number separately.
#    We can use group_by() %>% group_split() %>% map() or group_by() %>% do()

print("Step 3: Calculating RCA per permutation...")

# Using dplyr::group_by and then purrr::map or a loop
# The result will be a list of dataframes (one RCA df per permutation)
list_of_rca_dfs <- region_tech_fields_perm_df %>%
  group_by(permutation_number) %>%
  group_split() %>% # This splits the df into a list of dfs, one for each permutation
  purrr::map(~{
    current_permutation_number <- unique(.x$permutation_number)
    print(paste("Processing RCA for permutation_number:", current_permutation_number))
    
    # Matrix creation for the current permutation's data
    mat_reg_tech_perm_AI <- .x %>%
      select(-permutation_number) %>% # Temporarily remove for pivot if it causes issues
      arrange(techn_field_nr, ctry_code) %>%
      pivot_wider(names_from = techn_field_nr,
                  values_from = n_tech_reg,
                  values_fill = 0) # Changed from list(n_tech_reg = 0) for simplicity
    
    # Check if ctry_code column exists and is not empty
    if (!"ctry_code" %in% names(mat_reg_tech_perm_AI) || nrow(mat_reg_tech_perm_AI) == 0 || all(is.na(mat_reg_tech_perm_AI$ctry_code))) {
      print(paste("Skipping permutation", current_permutation_number, "due to missing ctry_code or empty data after pivot."))
      return(NULL) # Return NULL or an empty tibble
    }
    
    # Check for duplicate ctry_codes which would prevent rownames_to_column
    if (any(duplicated(mat_reg_tech_perm_AI$ctry_code))) {
      print(paste("Warning: Duplicate ctry_code found for permutation", current_permutation_number, ". Aggregating or handling needed."))
      return(tibble(permutation_number = current_permutation_number, error="duplicate ctry_code"))
    }
    
    
    mat_reg_tech_perm_AI <- mat_reg_tech_perm_AI %>%
      remove_rownames() %>%
      column_to_rownames(var = "ctry_code") %>%
      as.matrix() %>%  round()# No rounding here, location_quotient might prefer raw numbers
    
    # RTA calculation
    # Ensure matrix is suitable (e.g., no NA/NaN/Inf that location_quotient can't handle)
    if (nrow(mat_reg_tech_perm_AI) == 0 || ncol(mat_reg_tech_perm_AI) == 0) {
      print(paste("Skipping RCA for permutation", current_permutation_number, "due to empty matrix."))
      return(NULL)
    }
    
    # Ensure there are at least two columns for location_quotient (ctry_code was one)
    if (ncol(mat_reg_tech_perm_AI) < 1) { # If only ctry_code was present and now it's rownames
      print(paste("Skipping RCA for permutation", current_permutation_number, "due to insufficient columns in matrix."))
      return(NULL)
    }
    
    
    # Check for all zero rows/columns if location_quotient is sensitive
    rca_results_perm <- tryCatch({
      mat_reg_tech_perm_AI %>%
        location_quotient(binary = FALSE) %>% 
        as.data.frame() %>%
        rownames_to_column("ctry_code") %>%
        as_tibble() %>%
        gather(key = "techn_field_nr", value = "RCA", -ctry_code) %>%
        arrange(ctry_code, techn_field_nr) %>%
        mutate(permutation_number = current_permutation_number) # Add back permutation number
    }, error = function(e) {
      print(paste("Error in location_quotient for permutation", current_permutation_number, ":", e$message))
      return(tibble(permutation_number = current_permutation_number, ctry_code=NA, techn_field_nr=NA, RCA=NA, error_message = e$message)) # Return an empty or error-marked tibble
    })
    
    return(rca_results_perm)
  })

# Combine the list of RCA dataframes into one final dataframe
final_rca_all_permutations_df <- bind_rows(list_of_rca_dfs)

# Check for errors in the error column
if("error_message" %in% names(final_rca_all_permutations_df)){
  print("Permutations with errors during RCA calculation:")
  print(final_rca_all_permutations_df %>% filter(!is.na(error_message)))
}

write.csv2(final_rca_all_permutations_df, 
           file = "Files_created_with_the_code/data/files_code_Fields_analysis/robustness/final_rca_all_permutations_df_5y_3rd_Period.csv", 
           row.names = FALSE)

Robustness_data_RTA_5_years_2nd <- Robustness_data_RTA_5_years[Robustness_data_RTA_5_years$Period == "1984-1988",]
Robustness_data_RTA_5_years_2nd <- Robustness_data_RTA_5_years_2nd[Robustness_data_RTA_5_years_2nd$ctry_code == "JP"|
                                                                     Robustness_data_RTA_5_years_2nd$ctry_code == "US"|
                                                                     Robustness_data_RTA_5_years_2nd$ctry_code == "CN"|
                                                                     Robustness_data_RTA_5_years_2nd$ctry_code == "KR",]

test <- final_rca_all_permutations_df[final_rca_all_permutations_df$permutation_number == 0,]
test <- test[test$ctry_code == "JP"|test$ctry_code == "US"|
               test$ctry_code == "CN"|test$ctry_code == "KR",]
sum(Robustness_data_RTA_5_years_2nd$RCA_AI) #43.71137
sum(test$RCA) #43.71361

rm(final_rca_all_permutations_df, Robustness_data_RTA_5_years_2nd, ipc_all_patents_second_period_df, ai_patents_period_2_df,
   country_all_patents_pool, final_permuted_dataset, test, list_of_permuted_dfs,list_of_rca_dfs, 
   list_of_replicated_not_selected_ai, not_selected_AI, original_selected_AI, permuted_ai_for_target_countries_iter, 
   randomly_selected_patents_df_country,region_tech_fields_perm_df, temp_df)

## Interval 4 (1989-1993)
#"1989-1993",

# Define Time Interval 
start_year <- 1988
end_year   <- 1994

###Filter Data for Interval 2 
ipc_all_patents_second_period_df <- ipc_all_patents_part2_df[priority_year > start_year & priority_year < end_year]
ipc_all_patents_second_period_df <- ipc_all_patents_second_period_df[, .(appln_id, ctry_code, techn_field_nr)]

# Calculate AI-Specific RCA for Interval 2
ai_patents_period_2_df <- ai_patents_df[ai_patents_df$priority_year > start_year & ai_patents_df$priority_year < end_year,]

list_of_permuted_dfs <- vector("list", length = num_permutations)

for (p in 1:num_permutations) {
  if (p %% 100 == 0) print(paste("Permutation number:", p)) # Progress indicator
  
  # This dataframe will hold the permuted AI patents for target countries ONLY for THIS iteration
  permuted_ai_for_target_countries_iter <- data.frame()
  
  for (country in target_countries) {
    # 1. Identify and Count ACTUAL AI patents for the current country from the original AI dataset
    actual_ai_appln_ids_country <- ai_patents_period_2_df %>%
      filter(ctry_code == country) %>%
      distinct(appln_id) %>%
      pull(appln_id)
    
    n_ai_country <- length(actual_ai_appln_ids_country)
    
    if (n_ai_country == 0) {
      # print(paste("No AI patents found for", country, "in original AI data. Skipping for perm", p))
      next # Skip to the next country if no AI patents to replace
    }
    
    # 2. Prepare the pool of ALL patents for the current country from the general dataset
    country_all_patents_pool <- ipc_all_patents_second_period_df %>%
      filter(ctry_code == country) %>%
      distinct(appln_id)
    
    if (nrow(country_all_patents_pool) == 0) {
      # print(paste("No patents in general pool for", country, ". Skipping for perm", p))
      next
    }
    
    # Handle cases where the pool is smaller than the number of AI patents to sample
    # This is unlikely if ipc_all_patents_second_period_df is complete, but good for robustness
    sample_size <- min(n_ai_country, nrow(country_all_patents_pool))
    replace_sampling <- FALSE
    if (n_ai_country > nrow(country_all_patents_pool)) {
      if(nrow(country_all_patents_pool) < n_ai_country && !replace_sampling){
        next # Skip this country for this permutation if not enough patents
      }
    }
    
    # 3. Randomly select an equivalent number of unique appln_ids from this country's general pool
    random_appln_ids_country <- sample(country_all_patents_pool$appln_id,
                                       size = sample_size, # Use adjusted sample_size
                                       replace = replace_sampling) # Use replace_sampling flag
    
    # 4. Get all rows for these randomly selected patents from the ipc_all_patents_second_period_df
    randomly_selected_patents_df_country <- ipc_all_patents_second_period_df %>%
      filter(appln_id %in% random_appln_ids_country & ctry_code == country)
    
    # 5. Add these randomly selected patents for the current country to the iteration's df
    if (nrow(randomly_selected_patents_df_country) > 0) {
      permuted_ai_for_target_countries_iter <- bind_rows(
        permuted_ai_for_target_countries_iter,
        randomly_selected_patents_df_country
      )
    }
  } # End of country loop
  
  # Add the permutation number to all rows of this iteration's dataframe
  if (nrow(permuted_ai_for_target_countries_iter) > 0) {
    permuted_ai_for_target_countries_iter$permutation_number <- p
  }
  
  # Store the dataframe for this iteration in the list
  list_of_permuted_dfs[[p]] <- permuted_ai_for_target_countries_iter
  
} # End of permutation loop

# Combine all permuted dataframes from the list into one large dataframe
final_permuted_dataset <- bind_rows(list_of_permuted_dfs)

not_selected_AI <- ai_patents_period_2_df[ai_patents_period_2_df$ctry_code %notin% target_countries, c("appln_id", "ctry_code")]

#repeat the original dataset of non-selected countries a num_permutations number of times;
# Create a list to hold the replicated dataframes
list_of_replicated_not_selected_ai <- vector("list", length = num_permutations + 1)

# Loop from 0 to num_permutations
for (i in 0:num_permutations) {
  # Create a copy of the not_selected_AI dataframe for this iteration
  temp_df <- not_selected_AI
  
  # Add the permutation_number column
  temp_df$permutation_number <- i
  
  # Store it in the list (adjust index because list is 1-based, i is 0-based)
  list_of_replicated_not_selected_ai[[i + 1]] <- temp_df
  
  if (i %% 100 == 0) print(paste("Replicating not_selected_AI for permutation_number:", i))
}

# Combine all replicated dataframes from the list into one large dataframe
replicated_not_selected_ai_final <- bind_rows(list_of_replicated_not_selected_ai)
table(replicated_not_selected_ai_final$ctry_code) #everything but selected countries, as expected
table(replicated_not_selected_ai_final$permutation_number)

original_selected_AI <- ai_patents_period_2_df[ai_patents_period_2_df$ctry_code %in% target_countries, c("appln_id", "ctry_code")]
table(original_selected_AI$ctry_code)
original_selected_AI$permutation_number <- 0

#exclude the techn_field column (I don't need it for now)
final_permuted_dataset <- subset(final_permuted_dataset, select = -c(techn_field_nr) )
#merge both in the "reference" dataset, when permutation = 0
original_selected_AI <- rbind(original_selected_AI, replicated_not_selected_ai_final)
rm(replicated_not_selected_ai_final)
table(original_selected_AI$ctry_code) #the non selected countries appear 11 times more, because they appear already for all the permutations
table(ai_patents_period_2_df$ctry_code) 
table(original_selected_AI$permutation_number)
length(unique(original_selected_AI$appln_id)) #7887
length(unique(ai_patents_period_2_df$appln_id)) #7887

#rbind it with the original dataset:
final_permuted_dataset <- rbind(original_selected_AI,final_permuted_dataset) 
table(final_permuted_dataset$permutation_number)

# Join technological fields info to AI patents
final_permuted_dataset <- distinct_at(final_permuted_dataset, vars(appln_id,permutation_number), .keep_all = T)
final_permuted_dataset <- subset(final_permuted_dataset, select = -c(ctry_code) )

final_permuted_dataset <- left_join(final_permuted_dataset, ipc_all_patents_second_period_df, by = "appln_id")
#drop repeated information
#attention here 2
#if I drop the repeated information, the RCA deviates slightly from the original one, because I didn't do it in the original (it's not necessarily needed, since even repeated information has some value)
#final_permuted_dataset <- distinct_at(final_permuted_dataset, vars(appln_id,permutation_number, ctry_code,techn_field_nr), .keep_all = T)

# 1. Compute weighted fields for permuted AI patents
print("Step 1: Calculating field weights...")
region_tech_fields_perm_df <- group_by_applnID_perm(final_permuted_dataset)

# 2. Aggregate at the permutation-country-technology field level
print("Step 2: Aggregating by country and technology field per permutation...")
region_tech_fields_perm_df <- group_by_ctry_techn_field_perm(region_tech_fields_perm_df)
print("Counts of records per permutation_number after aggregation:")
print(table(region_tech_fields_perm_df$permutation_number))

# 3. Process per permutation for matrix creation and RCA calculation
#    This is the trickiest part. We need to do the pivot_wider and location_quotient
#    for each permutation_number separately.
#    We can use group_by() %>% group_split() %>% map() or group_by() %>% do()

print("Step 3: Calculating RCA per permutation...")

# Using dplyr::group_by and then purrr::map or a loop
# The result will be a list of dataframes (one RCA df per permutation)
list_of_rca_dfs <- region_tech_fields_perm_df %>%
  group_by(permutation_number) %>%
  group_split() %>% # This splits the df into a list of dfs, one for each permutation
  purrr::map(~{
    current_permutation_number <- unique(.x$permutation_number)
    print(paste("Processing RCA for permutation_number:", current_permutation_number))
    
    # Matrix creation for the current permutation's data
    mat_reg_tech_perm_AI <- .x %>%
      select(-permutation_number) %>% # Temporarily remove for pivot if it causes issues
      arrange(techn_field_nr, ctry_code) %>%
      pivot_wider(names_from = techn_field_nr,
                  values_from = n_tech_reg,
                  values_fill = 0) # Changed from list(n_tech_reg = 0) for simplicity
    
    # Check if ctry_code column exists and is not empty
    if (!"ctry_code" %in% names(mat_reg_tech_perm_AI) || nrow(mat_reg_tech_perm_AI) == 0 || all(is.na(mat_reg_tech_perm_AI$ctry_code))) {
      print(paste("Skipping permutation", current_permutation_number, "due to missing ctry_code or empty data after pivot."))
      return(NULL) # Return NULL or an empty tibble
    }
    
    # Check for duplicate ctry_codes which would prevent rownames_to_column
    if (any(duplicated(mat_reg_tech_perm_AI$ctry_code))) {
      print(paste("Warning: Duplicate ctry_code found for permutation", current_permutation_number, ". Aggregating or handling needed."))
      return(tibble(permutation_number = current_permutation_number, error="duplicate ctry_code"))
    }
    
    
    mat_reg_tech_perm_AI <- mat_reg_tech_perm_AI %>%
      remove_rownames() %>%
      column_to_rownames(var = "ctry_code") %>%
      as.matrix() %>%  round()# No rounding here, location_quotient might prefer raw numbers
    
    # RTA calculation
    # Ensure matrix is suitable (e.g., no NA/NaN/Inf that location_quotient can't handle)
    if (nrow(mat_reg_tech_perm_AI) == 0 || ncol(mat_reg_tech_perm_AI) == 0) {
      print(paste("Skipping RCA for permutation", current_permutation_number, "due to empty matrix."))
      return(NULL)
    }
    
    # Ensure there are at least two columns for location_quotient (ctry_code was one)
    if (ncol(mat_reg_tech_perm_AI) < 1) { # If only ctry_code was present and now it's rownames
      print(paste("Skipping RCA for permutation", current_permutation_number, "due to insufficient columns in matrix."))
      return(NULL)
    }
    
    
    # Check for all zero rows/columns if location_quotient is sensitive
    rca_results_perm <- tryCatch({
      mat_reg_tech_perm_AI %>%
        location_quotient(binary = FALSE) %>% 
        as.data.frame() %>%
        rownames_to_column("ctry_code") %>%
        as_tibble() %>%
        gather(key = "techn_field_nr", value = "RCA", -ctry_code) %>%
        arrange(ctry_code, techn_field_nr) %>%
        mutate(permutation_number = current_permutation_number) # Add back permutation number
    }, error = function(e) {
      print(paste("Error in location_quotient for permutation", current_permutation_number, ":", e$message))
      return(tibble(permutation_number = current_permutation_number, ctry_code=NA, techn_field_nr=NA, RCA=NA, error_message = e$message)) # Return an empty or error-marked tibble
    })
    
    return(rca_results_perm)
  })

# Combine the list of RCA dataframes into one final dataframe
final_rca_all_permutations_df <- bind_rows(list_of_rca_dfs)

# Check for errors in the error column
if("error_message" %in% names(final_rca_all_permutations_df)){
  print("Permutations with errors during RCA calculation:")
  print(final_rca_all_permutations_df %>% filter(!is.na(error_message)))
}

write.csv2(final_rca_all_permutations_df, 
           file = "Files_created_with_the_code/data/files_code_Fields_analysis/robustness/final_rca_all_permutations_df_5y_4th_Period.csv", 
           row.names = FALSE)

Robustness_data_RTA_5_years_2nd <- Robustness_data_RTA_5_years[Robustness_data_RTA_5_years$Period == "1989-1993",]
Robustness_data_RTA_5_years_2nd <- Robustness_data_RTA_5_years_2nd[Robustness_data_RTA_5_years_2nd$ctry_code == "JP"|
                                                                     Robustness_data_RTA_5_years_2nd$ctry_code == "US"|
                                                                     Robustness_data_RTA_5_years_2nd$ctry_code == "CN"|
                                                                     Robustness_data_RTA_5_years_2nd$ctry_code == "KR",]

test <- final_rca_all_permutations_df[final_rca_all_permutations_df$permutation_number == 0,]
test <- test[test$ctry_code == "JP"|test$ctry_code == "US"|
               test$ctry_code == "CN"|test$ctry_code == "KR",]
sum(Robustness_data_RTA_5_years_2nd$RCA_AI) #154.1647
sum(test$RCA) #154.6541

rm(final_rca_all_permutations_df, Robustness_data_RTA_5_years_2nd, ipc_all_patents_second_period_df, ai_patents_period_2_df,
   country_all_patents_pool, final_permuted_dataset, test, list_of_permuted_dfs,list_of_rca_dfs, 
   list_of_replicated_not_selected_ai, not_selected_AI, original_selected_AI, permuted_ai_for_target_countries_iter, 
   randomly_selected_patents_df_country,region_tech_fields_perm_df, temp_df)

##Interval 5 (1994-1998)
#"1994-1998"

# Define Time Interval 
start_year <- 1993
end_year   <- 1999

###Filter Data for Interval 2 
ipc_all_patents_second_period_df <- ipc_all_patents_part2_df[priority_year > start_year & priority_year < end_year]
ipc_all_patents_second_period_df <- ipc_all_patents_second_period_df[, .(appln_id, ctry_code, techn_field_nr)]

# Calculate AI-Specific RCA for Interval 2
ai_patents_period_2_df <- ai_patents_df[ai_patents_df$priority_year > start_year & ai_patents_df$priority_year < end_year,]

list_of_permuted_dfs <- vector("list", length = num_permutations)

for (p in 1:num_permutations) {
  if (p %% 100 == 0) print(paste("Permutation number:", p)) # Progress indicator
  
  # This dataframe will hold the permuted AI patents for target countries ONLY for THIS iteration
  permuted_ai_for_target_countries_iter <- data.frame()
  
  for (country in target_countries) {
    # 1. Identify and Count ACTUAL AI patents for the current country from the original AI dataset
    actual_ai_appln_ids_country <- ai_patents_period_2_df %>%
      filter(ctry_code == country) %>%
      distinct(appln_id) %>%
      pull(appln_id)
    
    n_ai_country <- length(actual_ai_appln_ids_country)
    
    if (n_ai_country == 0) {
      # print(paste("No AI patents found for", country, "in original AI data. Skipping for perm", p))
      next # Skip to the next country if no AI patents to replace
    }
    
    # 2. Prepare the pool of ALL patents for the current country from the general dataset
    country_all_patents_pool <- ipc_all_patents_second_period_df %>%
      filter(ctry_code == country) %>%
      distinct(appln_id)
    
    if (nrow(country_all_patents_pool) == 0) {
      # print(paste("No patents in general pool for", country, ". Skipping for perm", p))
      next
    }
    
    # Handle cases where the pool is smaller than the number of AI patents to sample
    # This is unlikely if ipc_all_patents_second_period_df is complete, but good for robustness
    sample_size <- min(n_ai_country, nrow(country_all_patents_pool))
    replace_sampling <- FALSE
    if (n_ai_country > nrow(country_all_patents_pool)) {
      if(nrow(country_all_patents_pool) < n_ai_country && !replace_sampling){
        next # Skip this country for this permutation if not enough patents
      }
    }
    
    # 3. Randomly select an equivalent number of unique appln_ids from this country's general pool
    random_appln_ids_country <- sample(country_all_patents_pool$appln_id,
                                       size = sample_size, # Use adjusted sample_size
                                       replace = replace_sampling) # Use replace_sampling flag
    
    # 4. Get all rows for these randomly selected patents from the ipc_all_patents_second_period_df
    randomly_selected_patents_df_country <- ipc_all_patents_second_period_df %>%
      filter(appln_id %in% random_appln_ids_country & ctry_code == country)
    
    # 5. Add these randomly selected patents for the current country to the iteration's df
    if (nrow(randomly_selected_patents_df_country) > 0) {
      permuted_ai_for_target_countries_iter <- bind_rows(
        permuted_ai_for_target_countries_iter,
        randomly_selected_patents_df_country
      )
    }
  } # End of country loop
  
  # Add the permutation number to all rows of this iteration's dataframe
  if (nrow(permuted_ai_for_target_countries_iter) > 0) {
    permuted_ai_for_target_countries_iter$permutation_number <- p
  }
  
  # Store the dataframe for this iteration in the list
  list_of_permuted_dfs[[p]] <- permuted_ai_for_target_countries_iter
  
} # End of permutation loop

# Combine all permuted dataframes from the list into one large dataframe
final_permuted_dataset <- bind_rows(list_of_permuted_dfs)

not_selected_AI <- ai_patents_period_2_df[ai_patents_period_2_df$ctry_code %notin% target_countries, c("appln_id", "ctry_code")]

#repeat the original dataset of non-selected countries a num_permutations number of times;
# Create a list to hold the replicated dataframes
list_of_replicated_not_selected_ai <- vector("list", length = num_permutations + 1)

# Loop from 0 to num_permutations
for (i in 0:num_permutations) {
  # Create a copy of the not_selected_AI dataframe for this iteration
  temp_df <- not_selected_AI
  
  # Add the permutation_number column
  temp_df$permutation_number <- i
  
  # Store it in the list (adjust index because list is 1-based, i is 0-based)
  list_of_replicated_not_selected_ai[[i + 1]] <- temp_df
  
  if (i %% 100 == 0) print(paste("Replicating not_selected_AI for permutation_number:", i))
}

# Combine all replicated dataframes from the list into one large dataframe
replicated_not_selected_ai_final <- bind_rows(list_of_replicated_not_selected_ai)
table(replicated_not_selected_ai_final$ctry_code) #everything but selected countries, as expected
table(replicated_not_selected_ai_final$permutation_number)

original_selected_AI <- ai_patents_period_2_df[ai_patents_period_2_df$ctry_code %in% target_countries, c("appln_id", "ctry_code")]
table(original_selected_AI$ctry_code)
original_selected_AI$permutation_number <- 0

#exclude the techn_field column (I don't need it for now)
final_permuted_dataset <- subset(final_permuted_dataset, select = -c(techn_field_nr) )
#merge both in the "reference" dataset, when permutation = 0
original_selected_AI <- rbind(original_selected_AI, replicated_not_selected_ai_final)
rm(replicated_not_selected_ai_final)
table(original_selected_AI$ctry_code) #the non selected countries appear 11 times more, because they appear already for all the permutations
table(ai_patents_period_2_df$ctry_code) 
table(original_selected_AI$permutation_number)
length(unique(original_selected_AI$appln_id)) #7887
length(unique(ai_patents_period_2_df$appln_id)) #7887

#rbind it with the original dataset:
final_permuted_dataset <- rbind(original_selected_AI,final_permuted_dataset) 
table(final_permuted_dataset$permutation_number)

# Join technological fields info to AI patents
final_permuted_dataset <- distinct_at(final_permuted_dataset, vars(appln_id,permutation_number), .keep_all = T)
final_permuted_dataset <- subset(final_permuted_dataset, select = -c(ctry_code) )

final_permuted_dataset <- left_join(final_permuted_dataset, ipc_all_patents_second_period_df, by = "appln_id")
#drop repeated information
#attention here 2
#if I drop the repeated information, the RCA deviates slightly from the original one, because I didn't do it in the original (it's not necessarily needed, since even repeated information has some value)
#final_permuted_dataset <- distinct_at(final_permuted_dataset, vars(appln_id,permutation_number, ctry_code,techn_field_nr), .keep_all = T)

# 1. Compute weighted fields for permuted AI patents
print("Step 1: Calculating field weights...")
region_tech_fields_perm_df <- group_by_applnID_perm(final_permuted_dataset)

# 2. Aggregate at the permutation-country-technology field level
print("Step 2: Aggregating by country and technology field per permutation...")
region_tech_fields_perm_df <- group_by_ctry_techn_field_perm(region_tech_fields_perm_df)
print("Counts of records per permutation_number after aggregation:")
print(table(region_tech_fields_perm_df$permutation_number))

# 3. Process per permutation for matrix creation and RCA calculation
#    This is the trickiest part. We need to do the pivot_wider and location_quotient
#    for each permutation_number separately.
#    We can use group_by() %>% group_split() %>% map() or group_by() %>% do()

print("Step 3: Calculating RCA per permutation...")

# Using dplyr::group_by and then purrr::map or a loop
# The result will be a list of dataframes (one RCA df per permutation)
list_of_rca_dfs <- region_tech_fields_perm_df %>%
  group_by(permutation_number) %>%
  group_split() %>% # This splits the df into a list of dfs, one for each permutation
  purrr::map(~{
    current_permutation_number <- unique(.x$permutation_number)
    print(paste("Processing RCA for permutation_number:", current_permutation_number))
    
    # Matrix creation for the current permutation's data
    mat_reg_tech_perm_AI <- .x %>%
      select(-permutation_number) %>% # Temporarily remove for pivot if it causes issues
      arrange(techn_field_nr, ctry_code) %>%
      pivot_wider(names_from = techn_field_nr,
                  values_from = n_tech_reg,
                  values_fill = 0) # Changed from list(n_tech_reg = 0) for simplicity
    
    # Check if ctry_code column exists and is not empty
    if (!"ctry_code" %in% names(mat_reg_tech_perm_AI) || nrow(mat_reg_tech_perm_AI) == 0 || all(is.na(mat_reg_tech_perm_AI$ctry_code))) {
      print(paste("Skipping permutation", current_permutation_number, "due to missing ctry_code or empty data after pivot."))
      return(NULL) # Return NULL or an empty tibble
    }
    
    # Check for duplicate ctry_codes which would prevent rownames_to_column
    if (any(duplicated(mat_reg_tech_perm_AI$ctry_code))) {
      print(paste("Warning: Duplicate ctry_code found for permutation", current_permutation_number, ". Aggregating or handling needed."))
      return(tibble(permutation_number = current_permutation_number, error="duplicate ctry_code"))
    }
    
    
    mat_reg_tech_perm_AI <- mat_reg_tech_perm_AI %>%
      remove_rownames() %>%
      column_to_rownames(var = "ctry_code") %>%
      as.matrix() %>%  round()# No rounding here, location_quotient might prefer raw numbers
    
    # RTA calculation
    # Ensure matrix is suitable (e.g., no NA/NaN/Inf that location_quotient can't handle)
    if (nrow(mat_reg_tech_perm_AI) == 0 || ncol(mat_reg_tech_perm_AI) == 0) {
      print(paste("Skipping RCA for permutation", current_permutation_number, "due to empty matrix."))
      return(NULL)
    }
    
    # Ensure there are at least two columns for location_quotient (ctry_code was one)
    if (ncol(mat_reg_tech_perm_AI) < 1) { # If only ctry_code was present and now it's rownames
      print(paste("Skipping RCA for permutation", current_permutation_number, "due to insufficient columns in matrix."))
      return(NULL)
    }
    
    
    # Check for all zero rows/columns if location_quotient is sensitive
    rca_results_perm <- tryCatch({
      mat_reg_tech_perm_AI %>%
        location_quotient(binary = FALSE) %>% 
        as.data.frame() %>%
        rownames_to_column("ctry_code") %>%
        as_tibble() %>%
        gather(key = "techn_field_nr", value = "RCA", -ctry_code) %>%
        arrange(ctry_code, techn_field_nr) %>%
        mutate(permutation_number = current_permutation_number) # Add back permutation number
    }, error = function(e) {
      print(paste("Error in location_quotient for permutation", current_permutation_number, ":", e$message))
      return(tibble(permutation_number = current_permutation_number, ctry_code=NA, techn_field_nr=NA, RCA=NA, error_message = e$message)) # Return an empty or error-marked tibble
    })
    
    return(rca_results_perm)
  })

# Combine the list of RCA dataframes into one final dataframe
final_rca_all_permutations_df <- bind_rows(list_of_rca_dfs)

# Check for errors in the error column
if("error_message" %in% names(final_rca_all_permutations_df)){
  print("Permutations with errors during RCA calculation:")
  print(final_rca_all_permutations_df %>% filter(!is.na(error_message)))
}

write.csv2(final_rca_all_permutations_df, 
           file = "Files_created_with_the_code/data/files_code_Fields_analysis/robustness/final_rca_all_permutations_df_5y_5th_Period.csv", 
           row.names = FALSE)

Robustness_data_RTA_5_years_2nd <- Robustness_data_RTA_5_years[Robustness_data_RTA_5_years$Period == "1994-1998",]
Robustness_data_RTA_5_years_2nd <- Robustness_data_RTA_5_years_2nd[Robustness_data_RTA_5_years_2nd$ctry_code == "JP"|
                                                                     Robustness_data_RTA_5_years_2nd$ctry_code == "US"|
                                                                     Robustness_data_RTA_5_years_2nd$ctry_code == "CN"|
                                                                     Robustness_data_RTA_5_years_2nd$ctry_code == "KR",]

test <- final_rca_all_permutations_df[final_rca_all_permutations_df$permutation_number == 0,]
test <- test[test$ctry_code == "JP"|test$ctry_code == "US"|
               test$ctry_code == "CN"|test$ctry_code == "KR",]
sum(Robustness_data_RTA_5_years_2nd$RCA_AI) #121.7042
sum(test$RCA) #120.2579

rm(final_rca_all_permutations_df, Robustness_data_RTA_5_years_2nd, ipc_all_patents_second_period_df, ai_patents_period_2_df,
   country_all_patents_pool, final_permuted_dataset, test, list_of_permuted_dfs,list_of_rca_dfs, 
   list_of_replicated_not_selected_ai, not_selected_AI, original_selected_AI, permuted_ai_for_target_countries_iter, 
   randomly_selected_patents_df_country,region_tech_fields_perm_df, temp_df)

##Interval 6 (1999-2003)
#"1999-2003",
# Define Time Interval 
start_year <- 1998
end_year   <- 2004

###Filter Data for Interval 2 
ipc_all_patents_second_period_df <- ipc_all_patents_part2_df[priority_year > start_year & priority_year < end_year]
#table(ipc_all_patents_second_period_df$priority_year) #correct
#table(ipc_all_patents_part2_df$priority_year) #last available year is 2003, which is correct;
ipc_all_patents_second_period_df <- ipc_all_patents_second_period_df[, .(appln_id, ctry_code, techn_field_nr)]

# Calculate AI-Specific RCA for Interval 2
ai_patents_period_2_df <- ai_patents_df[ai_patents_df$priority_year > start_year & ai_patents_df$priority_year < end_year,]

list_of_permuted_dfs <- vector("list", length = num_permutations)

for (p in 1:num_permutations) {
  if (p %% 100 == 0) print(paste("Permutation number:", p)) # Progress indicator
  
  # This dataframe will hold the permuted AI patents for target countries ONLY for THIS iteration
  permuted_ai_for_target_countries_iter <- data.frame()
  
  for (country in target_countries) {
    # 1. Identify and Count ACTUAL AI patents for the current country from the original AI dataset
    actual_ai_appln_ids_country <- ai_patents_period_2_df %>%
      filter(ctry_code == country) %>%
      distinct(appln_id) %>%
      pull(appln_id)
    
    n_ai_country <- length(actual_ai_appln_ids_country)
    
    if (n_ai_country == 0) {
      # print(paste("No AI patents found for", country, "in original AI data. Skipping for perm", p))
      next # Skip to the next country if no AI patents to replace
    }
    
    # 2. Prepare the pool of ALL patents for the current country from the general dataset
    country_all_patents_pool <- ipc_all_patents_second_period_df %>%
      filter(ctry_code == country) %>%
      distinct(appln_id)
    
    if (nrow(country_all_patents_pool) == 0) {
      # print(paste("No patents in general pool for", country, ". Skipping for perm", p))
      next
    }
    
    # Handle cases where the pool is smaller than the number of AI patents to sample
    # This is unlikely if ipc_all_patents_second_period_df is complete, but good for robustness
    sample_size <- min(n_ai_country, nrow(country_all_patents_pool))
    replace_sampling <- FALSE
    if (n_ai_country > nrow(country_all_patents_pool)) {
      if(nrow(country_all_patents_pool) < n_ai_country && !replace_sampling){
        next # Skip this country for this permutation if not enough patents
      }
    }
    
    # 3. Randomly select an equivalent number of unique appln_ids from this country's general pool
    random_appln_ids_country <- sample(country_all_patents_pool$appln_id,
                                       size = sample_size, # Use adjusted sample_size
                                       replace = replace_sampling) # Use replace_sampling flag
    
    # 4. Get all rows for these randomly selected patents from the ipc_all_patents_second_period_df
    randomly_selected_patents_df_country <- ipc_all_patents_second_period_df %>%
      filter(appln_id %in% random_appln_ids_country & ctry_code == country)
    
    # 5. Add these randomly selected patents for the current country to the iteration's df
    if (nrow(randomly_selected_patents_df_country) > 0) {
      permuted_ai_for_target_countries_iter <- bind_rows(
        permuted_ai_for_target_countries_iter,
        randomly_selected_patents_df_country
      )
    }
  } # End of country loop
  
  # Add the permutation number to all rows of this iteration's dataframe
  if (nrow(permuted_ai_for_target_countries_iter) > 0) {
    permuted_ai_for_target_countries_iter$permutation_number <- p
  }
  
  # Store the dataframe for this iteration in the list
  list_of_permuted_dfs[[p]] <- permuted_ai_for_target_countries_iter
  
} # End of permutation loop

# Combine all permuted dataframes from the list into one large dataframe
final_permuted_dataset <- bind_rows(list_of_permuted_dfs)

not_selected_AI <- ai_patents_period_2_df[ai_patents_period_2_df$ctry_code %notin% target_countries, c("appln_id", "ctry_code")]

#repeat the original dataset of non-selected countries a num_permutations number of times;
# Create a list to hold the replicated dataframes
list_of_replicated_not_selected_ai <- vector("list", length = num_permutations + 1)

# Loop from 0 to num_permutations
for (i in 0:num_permutations) {
  # Create a copy of the not_selected_AI dataframe for this iteration
  temp_df <- not_selected_AI
  
  # Add the permutation_number column
  temp_df$permutation_number <- i
  
  # Store it in the list (adjust index because list is 1-based, i is 0-based)
  list_of_replicated_not_selected_ai[[i + 1]] <- temp_df
  
  if (i %% 100 == 0) print(paste("Replicating not_selected_AI for permutation_number:", i))
}

# Combine all replicated dataframes from the list into one large dataframe
replicated_not_selected_ai_final <- bind_rows(list_of_replicated_not_selected_ai)
table(replicated_not_selected_ai_final$ctry_code) #everything but selected countries, as expected
table(replicated_not_selected_ai_final$permutation_number)

original_selected_AI <- ai_patents_period_2_df[ai_patents_period_2_df$ctry_code %in% target_countries, c("appln_id", "ctry_code")]
table(original_selected_AI$ctry_code)
original_selected_AI$permutation_number <- 0

#exclude the techn_field column (I don't need it for now)
final_permuted_dataset <- subset(final_permuted_dataset, select = -c(techn_field_nr) )
#merge both in the "reference" dataset, when permutation = 0
original_selected_AI <- rbind(original_selected_AI, replicated_not_selected_ai_final)
rm(replicated_not_selected_ai_final)
table(original_selected_AI$ctry_code) #the non selected countries appear 11 times more, because they appear already for all the permutations
table(ai_patents_period_2_df$ctry_code) 
table(original_selected_AI$permutation_number)
length(unique(original_selected_AI$appln_id)) #7887
length(unique(ai_patents_period_2_df$appln_id)) #7887

#rbind it with the original dataset:
final_permuted_dataset <- rbind(original_selected_AI,final_permuted_dataset) 
table(final_permuted_dataset$permutation_number)

# Join technological fields info to AI patents
final_permuted_dataset <- distinct_at(final_permuted_dataset, vars(appln_id,permutation_number), .keep_all = T)
final_permuted_dataset <- subset(final_permuted_dataset, select = -c(ctry_code) )

final_permuted_dataset <- left_join(final_permuted_dataset, ipc_all_patents_second_period_df, by = "appln_id")
#drop repeated information
#attention here 2
#if I drop the repeated information, the RCA deviates slightly from the original one, because I didn't do it in the original (it's not necessarily needed, since even repeated information has some value)
#final_permuted_dataset <- distinct_at(final_permuted_dataset, vars(appln_id,permutation_number, ctry_code,techn_field_nr), .keep_all = T)

# 1. Compute weighted fields for permuted AI patents
print("Step 1: Calculating field weights...")
region_tech_fields_perm_df <- group_by_applnID_perm(final_permuted_dataset)

# 2. Aggregate at the permutation-country-technology field level
print("Step 2: Aggregating by country and technology field per permutation...")
region_tech_fields_perm_df <- group_by_ctry_techn_field_perm(region_tech_fields_perm_df)
print("Counts of records per permutation_number after aggregation:")
print(table(region_tech_fields_perm_df$permutation_number))

# 3. Process per permutation for matrix creation and RCA calculation
#    This is the trickiest part. We need to do the pivot_wider and location_quotient
#    for each permutation_number separately.
#    We can use group_by() %>% group_split() %>% map() or group_by() %>% do()

print("Step 3: Calculating RCA per permutation...")

# Using dplyr::group_by and then purrr::map or a loop
# The result will be a list of dataframes (one RCA df per permutation)
list_of_rca_dfs <- region_tech_fields_perm_df %>%
  group_by(permutation_number) %>%
  group_split() %>% # This splits the df into a list of dfs, one for each permutation
  purrr::map(~{
    current_permutation_number <- unique(.x$permutation_number)
    print(paste("Processing RCA for permutation_number:", current_permutation_number))
    
    # Matrix creation for the current permutation's data
    mat_reg_tech_perm_AI <- .x %>%
      select(-permutation_number) %>% # Temporarily remove for pivot if it causes issues
      arrange(techn_field_nr, ctry_code) %>%
      pivot_wider(names_from = techn_field_nr,
                  values_from = n_tech_reg,
                  values_fill = 0) # Changed from list(n_tech_reg = 0) for simplicity
    
    # Check if ctry_code column exists and is not empty
    if (!"ctry_code" %in% names(mat_reg_tech_perm_AI) || nrow(mat_reg_tech_perm_AI) == 0 || all(is.na(mat_reg_tech_perm_AI$ctry_code))) {
      print(paste("Skipping permutation", current_permutation_number, "due to missing ctry_code or empty data after pivot."))
      return(NULL) # Return NULL or an empty tibble
    }
    
    # Check for duplicate ctry_codes which would prevent rownames_to_column
    if (any(duplicated(mat_reg_tech_perm_AI$ctry_code))) {
      print(paste("Warning: Duplicate ctry_code found for permutation", current_permutation_number, ". Aggregating or handling needed."))
      return(tibble(permutation_number = current_permutation_number, error="duplicate ctry_code"))
    }
    
    
    mat_reg_tech_perm_AI <- mat_reg_tech_perm_AI %>%
      remove_rownames() %>%
      column_to_rownames(var = "ctry_code") %>%
      as.matrix() %>%  round()# No rounding here, location_quotient might prefer raw numbers
    
    # RTA calculation
    # Ensure matrix is suitable (e.g., no NA/NaN/Inf that location_quotient can't handle)
    if (nrow(mat_reg_tech_perm_AI) == 0 || ncol(mat_reg_tech_perm_AI) == 0) {
      print(paste("Skipping RCA for permutation", current_permutation_number, "due to empty matrix."))
      return(NULL)
    }
    
    # Ensure there are at least two columns for location_quotient (ctry_code was one)
    if (ncol(mat_reg_tech_perm_AI) < 1) { # If only ctry_code was present and now it's rownames
      print(paste("Skipping RCA for permutation", current_permutation_number, "due to insufficient columns in matrix."))
      return(NULL)
    }
    
    
    # Check for all zero rows/columns if location_quotient is sensitive
    rca_results_perm <- tryCatch({
      mat_reg_tech_perm_AI %>%
        location_quotient(binary = FALSE) %>% 
        as.data.frame() %>%
        rownames_to_column("ctry_code") %>%
        as_tibble() %>%
        gather(key = "techn_field_nr", value = "RCA", -ctry_code) %>%
        arrange(ctry_code, techn_field_nr) %>%
        mutate(permutation_number = current_permutation_number) # Add back permutation number
    }, error = function(e) {
      print(paste("Error in location_quotient for permutation", current_permutation_number, ":", e$message))
      return(tibble(permutation_number = current_permutation_number, ctry_code=NA, techn_field_nr=NA, RCA=NA, error_message = e$message)) # Return an empty or error-marked tibble
    })
    
    return(rca_results_perm)
  })

# Combine the list of RCA dataframes into one final dataframe
final_rca_all_permutations_df <- bind_rows(list_of_rca_dfs)

# Check for errors in the error column
if("error_message" %in% names(final_rca_all_permutations_df)){
  print("Permutations with errors during RCA calculation:")
  print(final_rca_all_permutations_df %>% filter(!is.na(error_message)))
}

write.csv2(final_rca_all_permutations_df, 
           file = "Files_created_with_the_code/data/files_code_Fields_analysis/robustness/final_rca_all_permutations_df_5y_6th_Period.csv", 
           row.names = FALSE)

Robustness_data_RTA_5_years_2nd <- Robustness_data_RTA_5_years[Robustness_data_RTA_5_years$Period == "1999-2003",]
Robustness_data_RTA_5_years_2nd <- Robustness_data_RTA_5_years_2nd[Robustness_data_RTA_5_years_2nd$ctry_code == "JP"|
                                                                     Robustness_data_RTA_5_years_2nd$ctry_code == "US"|
                                                                     Robustness_data_RTA_5_years_2nd$ctry_code == "CN"|
                                                                     Robustness_data_RTA_5_years_2nd$ctry_code == "KR",]

test <- final_rca_all_permutations_df[final_rca_all_permutations_df$permutation_number == 0,]
test <- test[test$ctry_code == "JP"|test$ctry_code == "US"|
               test$ctry_code == "CN"|test$ctry_code == "KR",]
sum(Robustness_data_RTA_5_years_2nd$RCA_AI) #147.1226
sum(test$RCA) #146.935

rm(final_rca_all_permutations_df, Robustness_data_RTA_5_years_2nd, ipc_all_patents_second_period_df, ai_patents_period_2_df,
   country_all_patents_pool, final_permuted_dataset, test, list_of_permuted_dfs,list_of_rca_dfs, 
   list_of_replicated_not_selected_ai, not_selected_AI, original_selected_AI, permuted_ai_for_target_countries_iter, 
   randomly_selected_patents_df_country,region_tech_fields_perm_df, temp_df)
rm(ipc_all_patents_part2_df)

##Interval 7 (2004 - 2008)
#"2004-2008",

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
rm(ipc_all_patents_part1_df)
#"2004-2008", 7th
#"2009-2013",
#"2014-2018"
start_year <- 2003
end_year   <- 2009

# Calculate AI-Specific RCA for Interval 3
ai_patents_period_3_df <- ai_patents_df[ai_patents_df$priority_year > start_year & ai_patents_df$priority_year < end_year,]

list_of_permuted_dfs <- vector("list", length = num_permutations)

for (p in 1:num_permutations) {
  if (p %% 100 == 0) print(paste("Permutation number:", p)) # Progress indicator
  
  # This dataframe will hold the permuted AI patents for target countries ONLY for THIS iteration
  permuted_ai_for_target_countries_iter <- data.frame()
  
  for (country in target_countries) {
    # 1. Identify and Count ACTUAL AI patents for the current country from the original AI dataset
    actual_ai_appln_ids_country <- ai_patents_period_3_df %>%
      filter(ctry_code == country) %>%
      distinct(appln_id) %>%
      pull(appln_id)
    
    n_ai_country <- length(actual_ai_appln_ids_country)
    
    if (n_ai_country == 0) {
      # print(paste("No AI patents found for", country, "in original AI data. Skipping for perm", p))
      next # Skip to the next country if no AI patents to replace
    }
    
    # 2. Prepare the pool of ALL patents for the current country from the general dataset
    country_all_patents_pool <- ipc_all_patents_third_period_df %>%
      filter(ctry_code == country) %>%
      distinct(appln_id)
    
    if (nrow(country_all_patents_pool) == 0) {
      # print(paste("No patents in general pool for", country, ". Skipping for perm", p))
      next
    }
    
    # Handle cases where the pool is smaller than the number of AI patents to sample
    # This is unlikely if ipc_all_patents_third_period_df is complete, but good for robustness
    sample_size <- min(n_ai_country, nrow(country_all_patents_pool))
    replace_sampling <- FALSE
    if (n_ai_country > nrow(country_all_patents_pool)) {
      if(nrow(country_all_patents_pool) < n_ai_country && !replace_sampling){
        next # Skip this country for this permutation if not enough patents
      }
    }
    
    # 3. Randomly select an equivalent number of unique appln_ids from this country's general pool
    random_appln_ids_country <- sample(country_all_patents_pool$appln_id,
                                       size = sample_size, # Use adjusted sample_size
                                       replace = replace_sampling) # Use replace_sampling flag
    
    # 4. Get all rows for these randomly selected patents from the ipc_all_patents_third_period_df
    randomly_selected_patents_df_country <- ipc_all_patents_third_period_df %>%
      filter(appln_id %in% random_appln_ids_country & ctry_code == country)
    
    # 5. Add these randomly selected patents for the current country to the iteration's df
    if (nrow(randomly_selected_patents_df_country) > 0) {
      permuted_ai_for_target_countries_iter <- bind_rows(
        permuted_ai_for_target_countries_iter,
        randomly_selected_patents_df_country
      )
    }
  } # End of country loop
  
  # Add the permutation number to all rows of this iteration's dataframe
  if (nrow(permuted_ai_for_target_countries_iter) > 0) {
    permuted_ai_for_target_countries_iter$permutation_number <- p
  }
  
  # Store the dataframe for this iteration in the list
  list_of_permuted_dfs[[p]] <- permuted_ai_for_target_countries_iter
  
} # End of permutation loop

# Combine all permuted dataframes from the list into one large dataframe
final_permuted_dataset <- bind_rows(list_of_permuted_dfs)

not_selected_AI <- ai_patents_period_3_df[ai_patents_period_3_df$ctry_code %notin% target_countries, c("appln_id", "ctry_code")]

#repeat the original dataset of non-selected countries a num_permutations number of times;
# Create a list to hold the replicated dataframes
list_of_replicated_not_selected_ai <- vector("list", length = num_permutations + 1)

# Loop from 0 to num_permutations
for (i in 0:num_permutations) {
  # Create a copy of the not_selected_AI dataframe for this iteration
  temp_df <- not_selected_AI
  
  # Add the permutation_number column
  temp_df$permutation_number <- i
  
  # Store it in the list (adjust index because list is 1-based, i is 0-based)
  list_of_replicated_not_selected_ai[[i + 1]] <- temp_df
  
  if (i %% 100 == 0) print(paste("Replicating not_selected_AI for permutation_number:", i))
}

# Combine all replicated dataframes from the list into one large dataframe
replicated_not_selected_ai_final <- bind_rows(list_of_replicated_not_selected_ai)
table(replicated_not_selected_ai_final$meuctry_code) #everything but selected countries, as expected
table(replicated_not_selected_ai_final$permutation_number)

original_selected_AI <- ai_patents_period_3_df[ai_patents_period_3_df$ctry_code %in% target_countries, c("appln_id", "ctry_code")]
table(original_selected_AI$ctry_code)
original_selected_AI$permutation_number <- 0

#exclude the techn_field column (I don't need it for now)
final_permuted_dataset <- subset(final_permuted_dataset, select = -c(techn_field_nr) )
#merge both in the "reference" dataset, when permutation = 0
original_selected_AI <- rbind(original_selected_AI, replicated_not_selected_ai_final)
rm(replicated_not_selected_ai_final)
table(original_selected_AI$ctry_code) #the non selected countries appear 11 times more, because they appear already for all the permutations
table(ai_patents_period_3_df$ctry_code) 
table(original_selected_AI$permutation_number)
length(unique(original_selected_AI$appln_id)) #34576
length(unique(ai_patents_period_3_df$appln_id)) #34576

#rbind it with the original dataset:
final_permuted_dataset <- rbind(original_selected_AI,final_permuted_dataset) 
table(final_permuted_dataset$permutation_number)

# Join technological fields info to AI patents
final_permuted_dataset <- distinct_at(final_permuted_dataset, vars(appln_id,permutation_number), .keep_all = T)
final_permuted_dataset <- subset(final_permuted_dataset, select = -c(ctry_code) )

final_permuted_dataset <- left_join(final_permuted_dataset, ipc_all_patents_third_period_df, by = "appln_id")
#drop repeated information
#attention here 3
#if I drop the repeated information, the RCA deviates slightly from the original one, because I didn't do it in the original (it's not necessarily needed, since even repeated information has some value)
#final_permuted_dataset <- distinct_at(final_permuted_dataset, vars(appln_id,permutation_number, ctry_code,techn_field_nr), .keep_all = T)

# 1. Compute weighted fields for permuted AI patents
print("Step 1: Calculating field weights...")
region_tech_fields_perm_df <- group_by_applnID_perm(final_permuted_dataset)

# 2. Aggregate at the permutation-country-technology field level
print("Step 2: Aggregating by country and technology field per permutation...")
region_tech_fields_perm_df <- group_by_ctry_techn_field_perm(region_tech_fields_perm_df)
print("Counts of records per permutation_number after aggregation:")
print(table(region_tech_fields_perm_df$permutation_number))

# 3. Process per permutation for matrix creation and RCA calculation
#    This is the trickiest part. We need to do the pivot_wider and location_quotient
#    for each permutation_number separately.
#    We can use group_by() %>% group_split() %>% map() or group_by() %>% do()

print("Step 3: Calculating RCA per permutation...")

# Using dplyr::group_by and then purrr::map or a loop
# The result will be a list of dataframes (one RCA df per permutation)
list_of_rca_dfs <- region_tech_fields_perm_df %>%
  group_by(permutation_number) %>%
  group_split() %>% # This splits the df into a list of dfs, one for each permutation
  purrr::map(~{
    current_permutation_number <- unique(.x$permutation_number)
    print(paste("Processing RCA for permutation_number:", current_permutation_number))
    
    # Matrix creation for the current permutation's data
    mat_reg_tech_perm_AI <- .x %>%
      select(-permutation_number) %>% # Temporarily remove for pivot if it causes issues
      arrange(techn_field_nr, ctry_code) %>%
      pivot_wider(names_from = techn_field_nr,
                  values_from = n_tech_reg,
                  values_fill = 0) # Changed from list(n_tech_reg = 0) for simplicity
    
    # Check if ctry_code column exists and is not empty
    if (!"ctry_code" %in% names(mat_reg_tech_perm_AI) || nrow(mat_reg_tech_perm_AI) == 0 || all(is.na(mat_reg_tech_perm_AI$ctry_code))) {
      print(paste("Skipping permutation", current_permutation_number, "due to missing ctry_code or empty data after pivot."))
      return(NULL) # Return NULL or an empty tibble
    }
    
    # Check for duplicate ctry_codes which would prevent rownames_to_column
    if (any(duplicated(mat_reg_tech_perm_AI$ctry_code))) {
      print(paste("Warning: Duplicate ctry_code found for permutation", current_permutation_number, ". Aggregating or handling needed."))
      return(tibble(permutation_number = current_permutation_number, error="duplicate ctry_code"))
    }
    
    
    mat_reg_tech_perm_AI <- mat_reg_tech_perm_AI %>%
      remove_rownames() %>%
      column_to_rownames(var = "ctry_code") %>%
      as.matrix() %>%  round()# No rounding here, location_quotient might prefer raw numbers
    
    # RTA calculation
    # Ensure matrix is suitable (e.g., no NA/NaN/Inf that location_quotient can't handle)
    if (nrow(mat_reg_tech_perm_AI) == 0 || ncol(mat_reg_tech_perm_AI) == 0) {
      print(paste("Skipping RCA for permutation", current_permutation_number, "due to empty matrix."))
      return(NULL)
    }
    
    # Ensure there are at least two columns for location_quotient (ctry_code was one)
    if (ncol(mat_reg_tech_perm_AI) < 1) { # If only ctry_code was present and now it's rownames
      print(paste("Skipping RCA for permutation", current_permutation_number, "due to insufficient columns in matrix."))
      return(NULL)
    }
    
    
    # Check for all zero rows/columns if location_quotient is sensitive
    rca_results_perm <- tryCatch({
      mat_reg_tech_perm_AI %>%
        location_quotient(binary = FALSE) %>% 
        as.data.frame() %>%
        rownames_to_column("ctry_code") %>%
        as_tibble() %>%
        gather(key = "techn_field_nr", value = "RCA", -ctry_code) %>%
        arrange(ctry_code, techn_field_nr) %>%
        mutate(permutation_number = current_permutation_number) # Add back permutation number
    }, error = function(e) {
      print(paste("Error in location_quotient for permutation", current_permutation_number, ":", e$message))
      return(tibble(permutation_number = current_permutation_number, ctry_code=NA, techn_field_nr=NA, RCA=NA, error_message = e$message)) # Return an empty or error-marked tibble
    })
    
    return(rca_results_perm)
  })

# Combine the list of RCA dataframes into one final dataframe
final_rca_all_permutations_df <- bind_rows(list_of_rca_dfs)

# Check for errors in the error column
if("error_message" %in% names(final_rca_all_permutations_df)){
  print("Permutations with errors during RCA calculation:")
  print(final_rca_all_permutations_df %>% filter(!is.na(error_message)))
}

write.csv2(final_rca_all_permutations_df, 
           file = "Files_created_with_the_code/data/files_code_Fields_analysis/robustness/final_rca_all_permutations_df_5y_7th_Period.csv", 
           row.names = FALSE)

Robustness_data_RTA_5_years_7th <- Robustness_data_RTA_5_years[Robustness_data_RTA_5_years$Period == "2004-2008",]
Robustness_data_RTA_5_years_7th <- Robustness_data_RTA_5_years_7th[Robustness_data_RTA_5_years_7th$ctry_code == "JP"|
                                                                     Robustness_data_RTA_5_years_7th$ctry_code == "US"|
                                                                     Robustness_data_RTA_5_years_7th$ctry_code == "CN"|
                                                                     Robustness_data_RTA_5_years_7th$ctry_code == "KR",]

test <- final_rca_all_permutations_df[final_rca_all_permutations_df$permutation_number == 0,]
test <- test[test$ctry_code == "JP"|test$ctry_code == "US"|
               test$ctry_code == "CN"|test$ctry_code == "KR",]
sum(Robustness_data_RTA_5_years_7th$RCA_AI) #141.237
sum(test$RCA) #140.6348

rm(final_rca_all_permutations_df, Robustness_data_RTA_5_years_7th, ai_patents_period_2_df,
   country_all_patents_pool, final_permuted_dataset, test, list_of_permuted_dfs,list_of_rca_dfs, 
   list_of_replicated_not_selected_ai, not_selected_AI, original_selected_AI, permuted_ai_for_target_countries_iter, 
   randomly_selected_patents_df_country,region_tech_fields_perm_df, temp_df, ai_patents_period_3_df)

##Interval 8 (2009-2013)
start_year <- 2008
end_year   <- 2014

# Calculate AI-Specific RCA for Interval 3
ai_patents_period_3_df <- ai_patents_df[ai_patents_df$priority_year > start_year & ai_patents_df$priority_year < end_year,]

list_of_permuted_dfs <- vector("list", length = num_permutations)

for (p in 1:num_permutations) {
  if (p %% 100 == 0) print(paste("Permutation number:", p)) # Progress indicator
  
  # This dataframe will hold the permuted AI patents for target countries ONLY for THIS iteration
  permuted_ai_for_target_countries_iter <- data.frame()
  
  for (country in target_countries) {
    # 1. Identify and Count ACTUAL AI patents for the current country from the original AI dataset
    actual_ai_appln_ids_country <- ai_patents_period_3_df %>%
      filter(ctry_code == country) %>%
      distinct(appln_id) %>%
      pull(appln_id)
    
    n_ai_country <- length(actual_ai_appln_ids_country)
    
    if (n_ai_country == 0) {
      # print(paste("No AI patents found for", country, "in original AI data. Skipping for perm", p))
      next # Skip to the next country if no AI patents to replace
    }
    
    # 2. Prepare the pool of ALL patents for the current country from the general dataset
    country_all_patents_pool <- ipc_all_patents_third_period_df %>%
      filter(ctry_code == country) %>%
      distinct(appln_id)
    
    if (nrow(country_all_patents_pool) == 0) {
      # print(paste("No patents in general pool for", country, ". Skipping for perm", p))
      next
    }
    
    # Handle cases where the pool is smaller than the number of AI patents to sample
    # This is unlikely if ipc_all_patents_third_period_df is complete, but good for robustness
    sample_size <- min(n_ai_country, nrow(country_all_patents_pool))
    replace_sampling <- FALSE
    if (n_ai_country > nrow(country_all_patents_pool)) {
      if(nrow(country_all_patents_pool) < n_ai_country && !replace_sampling){
        next # Skip this country for this permutation if not enough patents
      }
    }
    
    # 3. Randomly select an equivalent number of unique appln_ids from this country's general pool
    random_appln_ids_country <- sample(country_all_patents_pool$appln_id,
                                       size = sample_size, # Use adjusted sample_size
                                       replace = replace_sampling) # Use replace_sampling flag
    
    # 4. Get all rows for these randomly selected patents from the ipc_all_patents_third_period_df
    randomly_selected_patents_df_country <- ipc_all_patents_third_period_df %>%
      filter(appln_id %in% random_appln_ids_country & ctry_code == country)
    
    # 5. Add these randomly selected patents for the current country to the iteration's df
    if (nrow(randomly_selected_patents_df_country) > 0) {
      permuted_ai_for_target_countries_iter <- bind_rows(
        permuted_ai_for_target_countries_iter,
        randomly_selected_patents_df_country
      )
    }
  } # End of country loop
  
  # Add the permutation number to all rows of this iteration's dataframe
  if (nrow(permuted_ai_for_target_countries_iter) > 0) {
    permuted_ai_for_target_countries_iter$permutation_number <- p
  }
  
  # Store the dataframe for this iteration in the list
  list_of_permuted_dfs[[p]] <- permuted_ai_for_target_countries_iter
  
} # End of permutation loop

# Combine all permuted dataframes from the list into one large dataframe
final_permuted_dataset <- bind_rows(list_of_permuted_dfs)

not_selected_AI <- ai_patents_period_3_df[ai_patents_period_3_df$ctry_code %notin% target_countries, c("appln_id", "ctry_code")]

#repeat the original dataset of non-selected countries a num_permutations number of times;
# Create a list to hold the replicated dataframes
list_of_replicated_not_selected_ai <- vector("list", length = num_permutations + 1)

# Loop from 0 to num_permutations
for (i in 0:num_permutations) {
  # Create a copy of the not_selected_AI dataframe for this iteration
  temp_df <- not_selected_AI
  
  # Add the permutation_number column
  temp_df$permutation_number <- i
  
  # Store it in the list (adjust index because list is 1-based, i is 0-based)
  list_of_replicated_not_selected_ai[[i + 1]] <- temp_df
  
  if (i %% 100 == 0) print(paste("Replicating not_selected_AI for permutation_number:", i))
}

# Combine all replicated dataframes from the list into one large dataframe
replicated_not_selected_ai_final <- bind_rows(list_of_replicated_not_selected_ai)
table(replicated_not_selected_ai_final$meuctry_code) #everything but selected countries, as expected
table(replicated_not_selected_ai_final$permutation_number)

original_selected_AI <- ai_patents_period_3_df[ai_patents_period_3_df$ctry_code %in% target_countries, c("appln_id", "ctry_code")]
table(original_selected_AI$ctry_code)
original_selected_AI$permutation_number <- 0

#exclude the techn_field column (I don't need it for now)
final_permuted_dataset <- subset(final_permuted_dataset, select = -c(techn_field_nr) )
#merge both in the "reference" dataset, when permutation = 0
original_selected_AI <- rbind(original_selected_AI, replicated_not_selected_ai_final)
rm(replicated_not_selected_ai_final)
table(original_selected_AI$ctry_code) #the non selected countries appear 11 times more, because they appear already for all the permutations
table(ai_patents_period_3_df$ctry_code) 
table(original_selected_AI$permutation_number)
length(unique(original_selected_AI$appln_id)) #34576
length(unique(ai_patents_period_3_df$appln_id)) #34576

#rbind it with the original dataset:
final_permuted_dataset <- rbind(original_selected_AI,final_permuted_dataset) 
table(final_permuted_dataset$permutation_number)

# Join technological fields info to AI patents
final_permuted_dataset <- distinct_at(final_permuted_dataset, vars(appln_id,permutation_number), .keep_all = T)
final_permuted_dataset <- subset(final_permuted_dataset, select = -c(ctry_code) )

final_permuted_dataset <- left_join(final_permuted_dataset, ipc_all_patents_third_period_df, by = "appln_id")
#drop repeated information
#attention here 3
#if I drop the repeated information, the RCA deviates slightly from the original one, because I didn't do it in the original (it's not necessarily needed, since even repeated information has some value)
#final_permuted_dataset <- distinct_at(final_permuted_dataset, vars(appln_id,permutation_number, ctry_code,techn_field_nr), .keep_all = T)

# 1. Compute weighted fields for permuted AI patents
print("Step 1: Calculating field weights...")
region_tech_fields_perm_df <- group_by_applnID_perm(final_permuted_dataset)

# 2. Aggregate at the permutation-country-technology field level
print("Step 2: Aggregating by country and technology field per permutation...")
region_tech_fields_perm_df <- group_by_ctry_techn_field_perm(region_tech_fields_perm_df)
print("Counts of records per permutation_number after aggregation:")
print(table(region_tech_fields_perm_df$permutation_number))

# 3. Process per permutation for matrix creation and RCA calculation
#    This is the trickiest part. We need to do the pivot_wider and location_quotient
#    for each permutation_number separately.
#    We can use group_by() %>% group_split() %>% map() or group_by() %>% do()

print("Step 3: Calculating RCA per permutation...")

# Using dplyr::group_by and then purrr::map or a loop
# The result will be a list of dataframes (one RCA df per permutation)
list_of_rca_dfs <- region_tech_fields_perm_df %>%
  group_by(permutation_number) %>%
  group_split() %>% # This splits the df into a list of dfs, one for each permutation
  purrr::map(~{
    current_permutation_number <- unique(.x$permutation_number)
    print(paste("Processing RCA for permutation_number:", current_permutation_number))
    
    # Matrix creation for the current permutation's data
    mat_reg_tech_perm_AI <- .x %>%
      select(-permutation_number) %>% # Temporarily remove for pivot if it causes issues
      arrange(techn_field_nr, ctry_code) %>%
      pivot_wider(names_from = techn_field_nr,
                  values_from = n_tech_reg,
                  values_fill = 0) # Changed from list(n_tech_reg = 0) for simplicity
    
    # Check if ctry_code column exists and is not empty
    if (!"ctry_code" %in% names(mat_reg_tech_perm_AI) || nrow(mat_reg_tech_perm_AI) == 0 || all(is.na(mat_reg_tech_perm_AI$ctry_code))) {
      print(paste("Skipping permutation", current_permutation_number, "due to missing ctry_code or empty data after pivot."))
      return(NULL) # Return NULL or an empty tibble
    }
    
    # Check for duplicate ctry_codes which would prevent rownames_to_column
    if (any(duplicated(mat_reg_tech_perm_AI$ctry_code))) {
      print(paste("Warning: Duplicate ctry_code found for permutation", current_permutation_number, ". Aggregating or handling needed."))
      return(tibble(permutation_number = current_permutation_number, error="duplicate ctry_code"))
    }
    
    
    mat_reg_tech_perm_AI <- mat_reg_tech_perm_AI %>%
      remove_rownames() %>%
      column_to_rownames(var = "ctry_code") %>%
      as.matrix() %>%  round()# No rounding here, location_quotient might prefer raw numbers
    
    # RTA calculation
    # Ensure matrix is suitable (e.g., no NA/NaN/Inf that location_quotient can't handle)
    if (nrow(mat_reg_tech_perm_AI) == 0 || ncol(mat_reg_tech_perm_AI) == 0) {
      print(paste("Skipping RCA for permutation", current_permutation_number, "due to empty matrix."))
      return(NULL)
    }
    
    # Ensure there are at least two columns for location_quotient (ctry_code was one)
    if (ncol(mat_reg_tech_perm_AI) < 1) { # If only ctry_code was present and now it's rownames
      print(paste("Skipping RCA for permutation", current_permutation_number, "due to insufficient columns in matrix."))
      return(NULL)
    }
    
    
    # Check for all zero rows/columns if location_quotient is sensitive
    rca_results_perm <- tryCatch({
      mat_reg_tech_perm_AI %>%
        location_quotient(binary = FALSE) %>% 
        as.data.frame() %>%
        rownames_to_column("ctry_code") %>%
        as_tibble() %>%
        gather(key = "techn_field_nr", value = "RCA", -ctry_code) %>%
        arrange(ctry_code, techn_field_nr) %>%
        mutate(permutation_number = current_permutation_number) # Add back permutation number
    }, error = function(e) {
      print(paste("Error in location_quotient for permutation", current_permutation_number, ":", e$message))
      return(tibble(permutation_number = current_permutation_number, ctry_code=NA, techn_field_nr=NA, RCA=NA, error_message = e$message)) # Return an empty or error-marked tibble
    })
    
    return(rca_results_perm)
  })

# Combine the list of RCA dataframes into one final dataframe
final_rca_all_permutations_df <- bind_rows(list_of_rca_dfs)

# Check for errors in the error column
if("error_message" %in% names(final_rca_all_permutations_df)){
  print("Permutations with errors during RCA calculation:")
  print(final_rca_all_permutations_df %>% filter(!is.na(error_message)))
}

write.csv2(final_rca_all_permutations_df, 
           file = "Files_created_with_the_code/data/files_code_Fields_analysis/robustness/final_rca_all_permutations_df_5y_8th_Period.csv", 
           row.names = FALSE)

Robustness_data_RTA_5_years_7th <- Robustness_data_RTA_5_years[Robustness_data_RTA_5_years$Period == "2009-2013",]
Robustness_data_RTA_5_years_7th <- Robustness_data_RTA_5_years_7th[Robustness_data_RTA_5_years_7th$ctry_code == "JP"|
                                                                     Robustness_data_RTA_5_years_7th$ctry_code == "US"|
                                                                     Robustness_data_RTA_5_years_7th$ctry_code == "CN"|
                                                                     Robustness_data_RTA_5_years_7th$ctry_code == "KR",]

test <- final_rca_all_permutations_df[final_rca_all_permutations_df$permutation_number == 0,]
test <- test[test$ctry_code == "JP"|test$ctry_code == "US"|
               test$ctry_code == "CN"|test$ctry_code == "KR",]
sum(Robustness_data_RTA_5_years_7th$RCA_AI) #136.4363
sum(test$RCA) #136.3552

rm(final_rca_all_permutations_df, Robustness_data_RTA_5_years_7th,
   country_all_patents_pool, final_permuted_dataset, test, list_of_permuted_dfs,list_of_rca_dfs, 
   list_of_replicated_not_selected_ai, not_selected_AI, original_selected_AI, permuted_ai_for_target_countries_iter, 
   randomly_selected_patents_df_country,region_tech_fields_perm_df, temp_df, ai_patents_period_3_df)

##Interval 9 (2014-2018)
start_year <- 2013
end_year   <- 2019

# Calculate AI-Specific RCA for Interval 3
ai_patents_period_3_df <- ai_patents_df[ai_patents_df$priority_year > start_year & ai_patents_df$priority_year < end_year,]

list_of_permuted_dfs <- vector("list", length = num_permutations)

for (p in 1:num_permutations) {
  if (p %% 100 == 0) print(paste("Permutation number:", p)) # Progress indicator
  
  # This dataframe will hold the permuted AI patents for target countries ONLY for THIS iteration
  permuted_ai_for_target_countries_iter <- data.frame()
  
  for (country in target_countries) {
    # 1. Identify and Count ACTUAL AI patents for the current country from the original AI dataset
    actual_ai_appln_ids_country <- ai_patents_period_3_df %>%
      filter(ctry_code == country) %>%
      distinct(appln_id) %>%
      pull(appln_id)
    
    n_ai_country <- length(actual_ai_appln_ids_country)
    
    if (n_ai_country == 0) {
      # print(paste("No AI patents found for", country, "in original AI data. Skipping for perm", p))
      next # Skip to the next country if no AI patents to replace
    }
    
    # 2. Prepare the pool of ALL patents for the current country from the general dataset
    country_all_patents_pool <- ipc_all_patents_third_period_df %>%
      filter(ctry_code == country) %>%
      distinct(appln_id)
    
    if (nrow(country_all_patents_pool) == 0) {
      # print(paste("No patents in general pool for", country, ". Skipping for perm", p))
      next
    }
    
    # Handle cases where the pool is smaller than the number of AI patents to sample
    # This is unlikely if ipc_all_patents_third_period_df is complete, but good for robustness
    sample_size <- min(n_ai_country, nrow(country_all_patents_pool))
    replace_sampling <- FALSE
    if (n_ai_country > nrow(country_all_patents_pool)) {
      if(nrow(country_all_patents_pool) < n_ai_country && !replace_sampling){
        next # Skip this country for this permutation if not enough patents
      }
    }
    
    # 3. Randomly select an equivalent number of unique appln_ids from this country's general pool
    random_appln_ids_country <- sample(country_all_patents_pool$appln_id,
                                       size = sample_size, # Use adjusted sample_size
                                       replace = replace_sampling) # Use replace_sampling flag
    
    # 4. Get all rows for these randomly selected patents from the ipc_all_patents_third_period_df
    randomly_selected_patents_df_country <- ipc_all_patents_third_period_df %>%
      filter(appln_id %in% random_appln_ids_country & ctry_code == country)
    
    # 5. Add these randomly selected patents for the current country to the iteration's df
    if (nrow(randomly_selected_patents_df_country) > 0) {
      permuted_ai_for_target_countries_iter <- bind_rows(
        permuted_ai_for_target_countries_iter,
        randomly_selected_patents_df_country
      )
    }
  } # End of country loop
  
  # Add the permutation number to all rows of this iteration's dataframe
  if (nrow(permuted_ai_for_target_countries_iter) > 0) {
    permuted_ai_for_target_countries_iter$permutation_number <- p
  }
  
  # Store the dataframe for this iteration in the list
  list_of_permuted_dfs[[p]] <- permuted_ai_for_target_countries_iter
  
} # End of permutation loop

# Combine all permuted dataframes from the list into one large dataframe
final_permuted_dataset <- bind_rows(list_of_permuted_dfs)

not_selected_AI <- ai_patents_period_3_df[ai_patents_period_3_df$ctry_code %notin% target_countries, c("appln_id", "ctry_code")]

#repeat the original dataset of non-selected countries a num_permutations number of times;
# Create a list to hold the replicated dataframes
list_of_replicated_not_selected_ai <- vector("list", length = num_permutations + 1)

# Loop from 0 to num_permutations
for (i in 0:num_permutations) {
  # Create a copy of the not_selected_AI dataframe for this iteration
  temp_df <- not_selected_AI
  
  # Add the permutation_number column
  temp_df$permutation_number <- i
  
  # Store it in the list (adjust index because list is 1-based, i is 0-based)
  list_of_replicated_not_selected_ai[[i + 1]] <- temp_df
  
  if (i %% 100 == 0) print(paste("Replicating not_selected_AI for permutation_number:", i))
}

# Combine all replicated dataframes from the list into one large dataframe
replicated_not_selected_ai_final <- bind_rows(list_of_replicated_not_selected_ai)
table(replicated_not_selected_ai_final$meuctry_code) #everything but selected countries, as expected
table(replicated_not_selected_ai_final$permutation_number)

original_selected_AI <- ai_patents_period_3_df[ai_patents_period_3_df$ctry_code %in% target_countries, c("appln_id", "ctry_code")]
table(original_selected_AI$ctry_code)
original_selected_AI$permutation_number <- 0

#exclude the techn_field column (I don't need it for now)
final_permuted_dataset <- subset(final_permuted_dataset, select = -c(techn_field_nr) )
#merge both in the "reference" dataset, when permutation = 0
original_selected_AI <- rbind(original_selected_AI, replicated_not_selected_ai_final)
rm(replicated_not_selected_ai_final)
table(original_selected_AI$ctry_code) #the non selected countries appear 11 times more, because they appear already for all the permutations
table(ai_patents_period_3_df$ctry_code) 
table(original_selected_AI$permutation_number)
length(unique(original_selected_AI$appln_id)) #34576
length(unique(ai_patents_period_3_df$appln_id)) #34576

#rbind it with the original dataset:
final_permuted_dataset <- rbind(original_selected_AI,final_permuted_dataset) 
table(final_permuted_dataset$permutation_number)

# Join technological fields info to AI patents
final_permuted_dataset <- distinct_at(final_permuted_dataset, vars(appln_id,permutation_number), .keep_all = T)
final_permuted_dataset <- subset(final_permuted_dataset, select = -c(ctry_code) )

final_permuted_dataset <- left_join(final_permuted_dataset, ipc_all_patents_third_period_df, by = "appln_id")
#drop repeated information
#attention here 3
#if I drop the repeated information, the RCA deviates slightly from the original one, because I didn't do it in the original (it's not necessarily needed, since even repeated information has some value)
#final_permuted_dataset <- distinct_at(final_permuted_dataset, vars(appln_id,permutation_number, ctry_code,techn_field_nr), .keep_all = T)

# 1. Compute weighted fields for permuted AI patents
print("Step 1: Calculating field weights...")
region_tech_fields_perm_df <- group_by_applnID_perm(final_permuted_dataset)

# 2. Aggregate at the permutation-country-technology field level
print("Step 2: Aggregating by country and technology field per permutation...")
region_tech_fields_perm_df <- group_by_ctry_techn_field_perm(region_tech_fields_perm_df)
print("Counts of records per permutation_number after aggregation:")
print(table(region_tech_fields_perm_df$permutation_number))

# 3. Process per permutation for matrix creation and RCA calculation
#    This is the trickiest part. We need to do the pivot_wider and location_quotient
#    for each permutation_number separately.
#    We can use group_by() %>% group_split() %>% map() or group_by() %>% do()

print("Step 3: Calculating RCA per permutation...")

# Using dplyr::group_by and then purrr::map or a loop
# The result will be a list of dataframes (one RCA df per permutation)
list_of_rca_dfs <- region_tech_fields_perm_df %>%
  group_by(permutation_number) %>%
  group_split() %>% # This splits the df into a list of dfs, one for each permutation
  purrr::map(~{
    current_permutation_number <- unique(.x$permutation_number)
    print(paste("Processing RCA for permutation_number:", current_permutation_number))
    
    # Matrix creation for the current permutation's data
    mat_reg_tech_perm_AI <- .x %>%
      select(-permutation_number) %>% # Temporarily remove for pivot if it causes issues
      arrange(techn_field_nr, ctry_code) %>%
      pivot_wider(names_from = techn_field_nr,
                  values_from = n_tech_reg,
                  values_fill = 0) # Changed from list(n_tech_reg = 0) for simplicity
    
    # Check if ctry_code column exists and is not empty
    if (!"ctry_code" %in% names(mat_reg_tech_perm_AI) || nrow(mat_reg_tech_perm_AI) == 0 || all(is.na(mat_reg_tech_perm_AI$ctry_code))) {
      print(paste("Skipping permutation", current_permutation_number, "due to missing ctry_code or empty data after pivot."))
      return(NULL) # Return NULL or an empty tibble
    }
    
    # Check for duplicate ctry_codes which would prevent rownames_to_column
    if (any(duplicated(mat_reg_tech_perm_AI$ctry_code))) {
      print(paste("Warning: Duplicate ctry_code found for permutation", current_permutation_number, ". Aggregating or handling needed."))
      return(tibble(permutation_number = current_permutation_number, error="duplicate ctry_code"))
    }
    
    
    mat_reg_tech_perm_AI <- mat_reg_tech_perm_AI %>%
      remove_rownames() %>%
      column_to_rownames(var = "ctry_code") %>%
      as.matrix() %>%  round()# No rounding here, location_quotient might prefer raw numbers
    
    # RTA calculation
    # Ensure matrix is suitable (e.g., no NA/NaN/Inf that location_quotient can't handle)
    if (nrow(mat_reg_tech_perm_AI) == 0 || ncol(mat_reg_tech_perm_AI) == 0) {
      print(paste("Skipping RCA for permutation", current_permutation_number, "due to empty matrix."))
      return(NULL)
    }
    
    # Ensure there are at least two columns for location_quotient (ctry_code was one)
    if (ncol(mat_reg_tech_perm_AI) < 1) { # If only ctry_code was present and now it's rownames
      print(paste("Skipping RCA for permutation", current_permutation_number, "due to insufficient columns in matrix."))
      return(NULL)
    }
    
    
    # Check for all zero rows/columns if location_quotient is sensitive
    rca_results_perm <- tryCatch({
      mat_reg_tech_perm_AI %>%
        location_quotient(binary = FALSE) %>% 
        as.data.frame() %>%
        rownames_to_column("ctry_code") %>%
        as_tibble() %>%
        gather(key = "techn_field_nr", value = "RCA", -ctry_code) %>%
        arrange(ctry_code, techn_field_nr) %>%
        mutate(permutation_number = current_permutation_number) # Add back permutation number
    }, error = function(e) {
      print(paste("Error in location_quotient for permutation", current_permutation_number, ":", e$message))
      return(tibble(permutation_number = current_permutation_number, ctry_code=NA, techn_field_nr=NA, RCA=NA, error_message = e$message)) # Return an empty or error-marked tibble
    })
    
    return(rca_results_perm)
  })

# Combine the list of RCA dataframes into one final dataframe
final_rca_all_permutations_df <- bind_rows(list_of_rca_dfs)

# Check for errors in the error column
if("error_message" %in% names(final_rca_all_permutations_df)){
  print("Permutations with errors during RCA calculation:")
  print(final_rca_all_permutations_df %>% filter(!is.na(error_message)))
}

write.csv2(final_rca_all_permutations_df, 
           file = "Files_created_with_the_code/data/files_code_Fields_analysis/robustness/final_rca_all_permutations_df_5y_9th_Period.csv", 
           row.names = FALSE)

Robustness_data_RTA_5_years_7th <- Robustness_data_RTA_5_years[Robustness_data_RTA_5_years$Period == "2014-2018",]
Robustness_data_RTA_5_years_7th <- Robustness_data_RTA_5_years_7th[Robustness_data_RTA_5_years_7th$ctry_code == "JP"|
                                                                     Robustness_data_RTA_5_years_7th$ctry_code == "US"|
                                                                     Robustness_data_RTA_5_years_7th$ctry_code == "CN"|
                                                                     Robustness_data_RTA_5_years_7th$ctry_code == "KR",]

test <- final_rca_all_permutations_df[final_rca_all_permutations_df$permutation_number == 0,]
test <- test[test$ctry_code == "JP"|test$ctry_code == "US"|
               test$ctry_code == "CN"|test$ctry_code == "KR",]
sum(Robustness_data_RTA_5_years_7th$RCA_AI) #165.4422
sum(test$RCA) #165.1906

###2.1.3.Read and combine everything for techn field level------
####2.1.3.1.For 15-years-----
rm(list=ls())
#historical data for double-check
historical_ai <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/RCA_4countries_detailed.csv", 
                          sep = ";", header = TRUE, dec=",")

#read permutations
#1st period
Permutations_1st <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/robustness/final_rca_all_permutations_df_1st_Period.csv", 
                             sep = ";", dec = ",", header = TRUE)
Permutations_1st$Period <- "1974-1988"
Permutations_1st <- Permutations_1st[Permutations_1st$ctry_code == "JP"|Permutations_1st$ctry_code == "US"|
                                       Permutations_1st$ctry_code == "CN"|Permutations_1st$ctry_code == "KR",]
Reference_1st <- Permutations_1st[Permutations_1st$permutation_number == 0,]

#2nd period
Permutations_2nd <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/robustness/final_rca_all_permutations_df_2nd_Period.csv", 
                             sep = ";", dec = ",", header = TRUE)
Permutations_2nd$Period <- "1989-2003"
Permutations_2nd <- Permutations_2nd[Permutations_2nd$ctry_code == "JP"|Permutations_2nd$ctry_code == "US"|
                                       Permutations_2nd$ctry_code == "CN"|Permutations_2nd$ctry_code == "KR",]
Reference_2nd <- Permutations_2nd[Permutations_2nd$permutation_number == 0,]

#3rd period
Permutations_3rd <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/robustness/final_rca_all_permutations_df_3rd_Period.csv", 
                             sep = ";", dec = ",", header = TRUE)
Permutations_3rd$Period <- "2004-2018"
Permutations_3rd <- Permutations_3rd[Permutations_3rd$ctry_code == "JP"|Permutations_3rd$ctry_code == "US"|
                                       Permutations_3rd$ctry_code == "CN"|Permutations_3rd$ctry_code == "KR",]
Reference_3rd <- Permutations_3rd[Permutations_3rd$permutation_number == 0,]

#double-check with the baseline
historical_ai_1st <- historical_ai[historical_ai$Period == "1974-1988",]
historical_ai_1st <- historical_ai_1st[historical_ai_1st$ctry_code == "JP"|historical_ai_1st$ctry_code == "US"|
                                         historical_ai_1st$ctry_code == "CN"|historical_ai_1st$ctry_code == "KR",]
sum(historical_ai_1st$RCA_AI) #43.35699
sum(Reference_1st$RCA) #43.35699

historical_ai_2nd <- historical_ai[historical_ai$Period == "1989-2003",]
historical_ai_2nd <- historical_ai_2nd[historical_ai_2nd$ctry_code == "JP"|historical_ai_2nd$ctry_code == "US"|
                                         historical_ai_2nd$ctry_code == "CN"|historical_ai_2nd$ctry_code == "KR",]
sum(historical_ai_2nd$RCA_AI) #151.1699
sum(Reference_2nd$RCA) #151.1699

historical_ai_3rd <- historical_ai[historical_ai$Period == "2004-2018",]
historical_ai_3rd <- historical_ai_3rd[historical_ai_3rd$ctry_code == "JP"|historical_ai_3rd$ctry_code == "US"|
                                         historical_ai_3rd$ctry_code == "CN"|historical_ai_3rd$ctry_code == "KR",]
sum(historical_ai_3rd$RCA_AI) #162.1932
sum(Reference_3rd$RCA) #162.1932
#all correct!

#now, for making it sure that the technological fields don't disappear when there is no AI specialization:
# Define all possible techn_field_nr values
all_techn_fields <- 1:35
all_periods  <- c("1974-1988", "1989-2003", "2004-2018")
all_ctry_codes <- c("CN", "JP", "KR", "US") # Define ALL countries you want in the final dataset

all_permutation_numbers <- unique(Permutations_1st$permutation_number)
if (length(all_permutation_numbers) == 0) { # Handle case where Permutations_1st might be empty
  all_permutation_numbers <- 0 # Or the default/expected range, e.g., 0:1000
}

scaffold_groups <- expand.grid(
  ctry_code = all_ctry_codes,
  permutation_number = all_permutation_numbers,
  Period = all_periods,
  stringsAsFactors = FALSE # Important for character vectors
) %>% as_tibble()

# --- Now expand this group scaffold by all technological fields ---
scaffold_df <- scaffold_groups %>%
  expand(nesting(ctry_code, permutation_number, Period), # Uses ALL defined combinations
         techn_field_nr = all_techn_fields)
# No need to specify Period again here if it's in nesting()

#for the first period:

# 2. Left join the original data to this scaffold
Permutations_1st_completed_v2 <- left_join(
  scaffold_df,
  Permutations_1st,
  by = c("ctry_code", "permutation_number", "Period", "techn_field_nr")
)

# 3. Fill NA values in RCA with 0 (for the newly created rows)
Permutations_1st_completed_v2 <- Permutations_1st_completed_v2 %>%
  mutate(RCA = ifelse(is.na(RCA), 0, RCA)) %>%
  arrange(permutation_number, ctry_code, Period, techn_field_nr)

#for the second period:
all_permutation_numbers <- unique(Permutations_1st$permutation_number)
if (length(all_permutation_numbers) == 0) { # Handle case where Permutations_1st might be empty
  all_permutation_numbers <- 0 # Or the default/expected range, e.g., 0:1000
}

scaffold_groups <- expand.grid(
  ctry_code = all_ctry_codes,
  permutation_number = all_permutation_numbers,
  Period = all_periods,
  stringsAsFactors = FALSE # Important for character vectors
) %>% as_tibble()

# --- Now expand this group scaffold by all technological fields ---
scaffold_df <- scaffold_groups %>%
  expand(nesting(ctry_code, permutation_number, Period), # Uses ALL defined combinations
         techn_field_nr = all_techn_fields)

# 2. Left join the original data to this scaffold
Permutations_2nd_completed_v2 <- left_join(
  scaffold_df,
  Permutations_2nd,
  by = c("ctry_code", "permutation_number", "Period", "techn_field_nr")
)

# 3. Fill NA values in RCA with 0 (for the newly created rows)
Permutations_2nd_completed_v2 <- Permutations_2nd_completed_v2 %>%
  mutate(RCA = ifelse(is.na(RCA), 0, RCA)) %>%
  arrange(permutation_number, ctry_code, Period, techn_field_nr)

#for the third period:
all_permutation_numbers <- unique(Permutations_1st$permutation_number)
if (length(all_permutation_numbers) == 0) { # Handle case where Permutations_1st might be empty
  all_permutation_numbers <- 0 # Or the default/expected range, e.g., 0:1000
}

scaffold_groups <- expand.grid(
  ctry_code = all_ctry_codes,
  permutation_number = all_permutation_numbers,
  Period = all_periods,
  stringsAsFactors = FALSE # Important for character vectors
) %>% as_tibble()

# --- Now expand this group scaffold by all technological fields ---
scaffold_df <- scaffold_groups %>%
  expand(nesting(ctry_code, permutation_number, Period), # Uses ALL defined combinations
         techn_field_nr = all_techn_fields)

# 2. Left join the original data to this scaffold
Permutations_3rd_completed_v2 <- left_join(
  scaffold_df,
  Permutations_3rd,
  by = c("ctry_code", "permutation_number", "Period", "techn_field_nr")
)

# 3. Fill NA values in RCA with 0 (for the newly created rows)
Permutations_3rd_completed_v2 <- Permutations_3rd_completed_v2 %>%
  mutate(RCA = ifelse(is.na(RCA), 0, RCA)) %>%
  arrange(permutation_number, ctry_code, Period, techn_field_nr)

#now, left_join everything
Permutations_1st <- left_join(historical_ai_1st, Permutations_1st_completed_v2, 
                              by = c("ctry_code","techn_field_nr", "Period"))

Permutations_2nd <- left_join(historical_ai_2nd, Permutations_2nd_completed_v2, 
                              by = c("ctry_code","techn_field_nr", "Period"))

Permutations_3rd <- left_join(historical_ai_3rd, Permutations_3rd_completed_v2, 
                              by = c("ctry_code","techn_field_nr", "Period"))

Permutations_all <- rbind(Permutations_1st, Permutations_2nd, Permutations_3rd)
rm(Permutations_1st, Permutations_2nd, Permutations_3rd, historical_ai_1st, 
   historical_ai_2nd, historical_ai_3rd, historical_ai, Permutations_1st_completed_v2,
   Permutations_2nd_completed_v2, Permutations_3rd_completed_v2, scaffold_df, unique_groups)

Permutations_all_1000 <- Permutations_all[Permutations_all$permutation_number <= 1000,]

#create the overlapping variables
Threshold <- 1
Permutations_all_1000$Round_general <- ifelse(Permutations_all_1000$RCA_Gen < Threshold, 0, 1)
Permutations_all_1000$Round_AI <- ifelse(Permutations_all_1000$RCA < Threshold, 0, 1)
Permutations_all_1000$justGeneral <- ifelse(Permutations_all_1000$RCA_Gen >= Threshold & 
                                              Permutations_all_1000$RCA < Threshold, 1, 0)
Permutations_all_1000$justAI <- ifelse(Permutations_all_1000$RCA >= Threshold & 
                                              Permutations_all_1000$RCA_Gen < Threshold, 1, 0)
#fix Total_RCA:
Permutations_all_1000$Total_RCA <- Permutations_all_1000$Round_general + 2*Permutations_all_1000$Round_AI

test <-Permutations_all_1000[Permutations_all_1000$ctry_code == "JP" & Permutations_all_1000$permutation_number==0 &
                               Permutations_all_1000$Period == "1974-1988",]

# --- 1. Calculate Percentages ---

# Total number of unique technological fields (assuming it's fixed at 35)
total_tech_fields <- 35

period_levels <- sort(unique(Permutations_all_1000$Period))
Permutations_all_1000 <- Permutations_all_1000 %>%
  mutate(Period = factor(Period, levels = period_levels, ordered = TRUE))

unique(Permutations_all_1000$Period)
# --- Step 1: Identify Persistent Fields ---
# We need to group by the lowest level (permutation, country, tech_field)
# and then look at the previous period's status.

persistent_fields_df <- Permutations_all_1000 %>%
  # Group by the entities for which persistence is tracked over time
  group_by(permutation_number, ctry_code, techn_field_nr) %>%
  arrange(Period) %>% # IMPORTANT: Ensure periods are sorted within each group
  mutate(
    # Was it coinciding in the PREVIOUS period?
    prev_coinciding = lag(Total_RCA == 3, default = FALSE), # default = FALSE for the first period
    # Is it coinciding NOW?
    current_coinciding = (Total_RCA == 3),
    # Is it persistently coinciding (now AND previously)?
    persistent_coinciding_field = current_coinciding & prev_coinciding,
    
    # Was it justGeneral in the PREVIOUS period?
    prev_justGeneral = lag(justGeneral == 1, default = FALSE),
    # Is it justGeneral NOW?
    current_justGeneral = (justGeneral == 1),
    # Is it persistently justGeneral?
    persistent_justGeneral_field = current_justGeneral & prev_justGeneral,
    
    # Was it justAI in the PREVIOUS period?
    prev_justAI = lag(justAI == 1, default = FALSE),
    # Is it justAI NOW?
    current_justAI = (justAI == 1),
    # Is it persistently justAI?
    persistent_justAI_field = current_justAI & prev_justAI,
    
    # Was it Round_AI in the PREVIOUS period?
    prev_Round_AI = lag(Round_AI == 1, default = FALSE),
    # Is it Round_AI NOW?
    current_Round_AI = (Round_AI == 1),
    # Is it persistently Round_AI?
    persistent_Round_AI_field = current_Round_AI & prev_Round_AI,
    
    # Was it Round_general in the PREVIOUS period?
    prev_Round_general = lag(Round_general == 1, default = FALSE),
    # Is it Round_general NOW?
    current_Round_general = (Round_general == 1),
    # Is it persistently Round_general?
    persistent_Round_general_field = current_Round_general & prev_Round_general,
    
    # Was it coinciding and then justAI?
    com_AI_prev_coinciding = current_justAI & prev_coinciding,
    
    # Was it justAI and then coinciding?
    com_coinciding_prev_AI = current_coinciding & prev_justAI, #prev_justAI #prev_Round_AI
    
    # Was it justGeneral and then justAI?
    com_AI_prev_justGeneral = current_justAI & prev_justGeneral, 
    
    #sustains AI in core fields? 6,7,10,12
    sust_core_fields = current_Round_AI & prev_Round_AI &
      (techn_field_nr %in% c(6, 7, 10, 12)), #c(6, 7, 10, 12)), #c(6, 7, 10, 12, 4, 5, 11)
    
    # sustains AI in NOT core fields? 
    sust_NOT_core_fields = current_Round_AI & prev_Round_AI &
      !(techn_field_nr %in% c(6, 7, 10, 12)),
    
    # persistent coinciding in AI core fields? 
    persistent_coinciding_field_core_fields = current_coinciding & prev_coinciding &
      (techn_field_nr %in% c(6, 7, 10, 12)),
    
    # persistent coinciding in NOT AI core fields? 
    persistent_coinciding_field_NOT_core_fields = current_coinciding & prev_coinciding &
      !(techn_field_nr %in% c(6, 7, 10, 12)),
    
    #AI in core fields? 6,7,10,12
    AI_core_fields = current_Round_AI & (techn_field_nr %in% c(6, 7, 10, 12)),
    
    #AI NOT in core fields? 6,7,10,12
    AI_not_core_fields = current_Round_AI & !(techn_field_nr %in% c(6, 7, 10, 12))
  ) %>%
  ungroup() # Ungroup before the next summary

# --- Step 2: Aggregate Counts and Calculate Original Shares ---
# Now, group by permutation, country, and Period to sum the persistent fields
# and also calculate the original metrics as before.

summary_and_persistence_df <- persistent_fields_df %>%
  group_by(permutation_number, ctry_code, Period) %>%
  summarise(
    coinciding_fields_current = sum(current_coinciding, na.rm = TRUE),
    justGeneral_fields_current = sum(current_justGeneral, na.rm = TRUE),
    justAI_fields_current = sum(current_justAI, na.rm = TRUE),
    Round_AI_fields_current = sum(current_Round_AI, na.rm = TRUE),
    Round_general_fields_current = sum(current_Round_general, na.rm = TRUE),
    # Counts of fields that were specialized for at least 2 consecutive periods (ending in current period)
    n_persistent_coinciding = sum(persistent_coinciding_field, na.rm = TRUE),
    n_persistent_justGeneral = sum(persistent_justGeneral_field, na.rm = TRUE),
    n_persistent_justAI = sum(persistent_justAI_field, na.rm = TRUE),
    n_persistent_Round_AI = sum(persistent_Round_AI_field, na.rm = TRUE),
    n_AI_prev_coinciding = sum(com_AI_prev_coinciding, na.rm = TRUE),
    n_coinciding_prev_AI = sum(com_coinciding_prev_AI, na.rm = TRUE), 
    n_AI_prev_gen = sum(com_AI_prev_justGeneral, na.rm = TRUE),
    n_persistent_core_fields = sum(sust_core_fields, na.rm = TRUE),
    n_persistent_NOT_core_fields = sum(sust_NOT_core_fields, na.rm = TRUE), 
    n_persistent_coin_core_fields = sum(persistent_coinciding_field_core_fields, na.rm = TRUE),
    n_persistent_coin_NOT_core_fields = sum(persistent_coinciding_field_NOT_core_fields, na.rm = TRUE),
    
    AI_core_fields = sum(AI_core_fields, na.rm = TRUE),
    AI_not_core_fields = sum(AI_not_core_fields, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    # Shares for the original calculation based on new counts
    # Be careful with the denominator here:
    # If you want share of coinciding relative to coinciding + justGeneral for the *current* period:
    Share_coinciding_original_calc = if_else(
      (coinciding_fields_current + justGeneral_fields_current) > 0,
      coinciding_fields_current / (coinciding_fields_current + justGeneral_fields_current),
      0 # Or NA
    ),
    Share_justAI_calc = if_else(
      (coinciding_fields_current + justGeneral_fields_current) > 0,
      Round_AI_fields_current / (35), #coinciding_fields_current  + justAI_fields_current + justGeneral_fields_current
      0 # Or NA
    ),
    Share_justgeneral_calc = if_else(
      (coinciding_fields_current + justGeneral_fields_current) > 0,
      Round_general_fields_current / (35), #coinciding_fields_current + justGeneral_fields_current + justAI_fields_current
      0 # Or NA
    ))

# Separate actual observed data (permutation_number == 0)
actual_shares <- summary_and_persistence_df %>%
  filter(permutation_number == 0) %>%
  select(ctry_code, Period, Actual_Share_coinciding = Share_coinciding_original_calc,
         Actual_Share_Round_AI = Share_justAI_calc,
         Actual_Share_Round_general = Share_justgeneral_calc,
         
         Actual_persistent_coinciding = n_persistent_coinciding,
         Actual_persistent_justGeneral = n_persistent_justGeneral,
         Actual_persistent_justAI = n_persistent_justAI,
         
         Actual_n_persistent_Round_AI = n_persistent_Round_AI, # Is it persistently Round_AI?
         Actual_n_AI_prev_coinciding = n_AI_prev_coinciding, # Was it coinciding and then justAI?
         Actual_n_coinciding_prev_AI = n_coinciding_prev_AI, # Was it justAI and then coinciding?
         Actual_n_AI_prev_gen = n_AI_prev_gen, #Was it justGeneral and then justAI?
         Actual_n_persistent_core_fields = n_persistent_core_fields, #sustains AI in core fields?
         Actual_n_persistent_NOT_core_fields = n_persistent_NOT_core_fields,#sustains AI in NOT core fields?
         Actual_n_persistent_coin_core_fields = n_persistent_coin_core_fields, # persistent coinciding in AI core fields? 
         Actual_n_persistent_coin_NOT_core_fields = n_persistent_coin_NOT_core_fields,# persistent coinciding in NOT AI core fields? 
         
         Actual_AI_core_fields = AI_core_fields,
         Actual_AI_not_core_fields  = AI_not_core_fields
         ) #14 variables, with the last 8 ones being there just for the regressions

write.csv2(actual_shares, 
           file = "Files_created_with_the_code/data/files_code_Fields_analysis/robustness/actual_shares_15_years.csv", 
           row.names = FALSE)

# Permuted data (permutation_number > 0)
permuted_shares <- summary_and_persistence_df %>%
  filter(permutation_number > 0)

# --- 2. Summarize Permuted Data ---
# Calculate mean and 95% confidence interval (2.5th and 97.5th percentiles)
# for the share of coinciding specializations from permutations
permuted_summary <- permuted_shares %>%
  group_by(ctry_code, Period) %>%
  summarise(
    Mean_Permuted_Share = mean(Share_coinciding_original_calc, na.rm = TRUE),
    Lower_CI_Permuted = quantile(Share_coinciding_original_calc, probs = 0.025, na.rm = TRUE),
    Upper_CI_Permuted = quantile(Share_coinciding_original_calc, probs = 0.975, na.rm = TRUE),
    
    Mean_Permuted_Share_sust_coinc = mean(n_persistent_coinciding, na.rm = TRUE),
    Lower_CI_Permuted_sust_coinc = quantile(n_persistent_coinciding, probs = 0.025, na.rm = TRUE),
    Upper_CI_Permuted_sust_coinc = quantile(n_persistent_coinciding, probs = 0.975, na.rm = TRUE),
    
    Mean_Permuted_Share_gen_coinc = mean(n_persistent_justGeneral, na.rm = TRUE),
    Lower_CI_Permuted_gen_coinc = quantile(n_persistent_justGeneral, probs = 0.025, na.rm = TRUE),
    Upper_CI_Permuted_gen_coinc = quantile(n_persistent_justGeneral, probs = 0.975, na.rm = TRUE),
    
    Mean_Permuted_Share_ai_coinc = mean(n_persistent_justAI, na.rm = TRUE),
    Lower_CI_Permuted_ai_coinc = quantile(n_persistent_justAI, probs = 0.025, na.rm = TRUE),
    Upper_CI_Permuted_ai_coinc = quantile(n_persistent_justAI, probs = 0.975, na.rm = TRUE),
    
    #news
    Mean_Permuted_Share_ai_excl_coinc = mean(n_persistent_Round_AI, na.rm = TRUE),
    Lower_CI_Permuted_ai_excl_coinc = quantile(n_persistent_Round_AI, probs = 0.025, na.rm = TRUE),
    Upper_CI_Permuted_ai_excl_coinc = quantile(n_persistent_Round_AI, probs = 0.975, na.rm = TRUE),
    
    Mean_Permuted_Share_AI_prev_coinciding = mean(n_AI_prev_coinciding, na.rm = TRUE),
    Lower_CI_Permuted_AI_prev_coinciding = quantile(n_AI_prev_coinciding, probs = 0.025, na.rm = TRUE),
    Upper_CI_Permuted_AI_prev_coinciding = quantile(n_AI_prev_coinciding, probs = 0.975, na.rm = TRUE),
    
    Mean_Permuted_Share_coinciding_prev_AI = mean(n_coinciding_prev_AI, na.rm = TRUE),
    Lower_CI_Permuted_coinciding_prev_AI = quantile(n_coinciding_prev_AI, probs = 0.025, na.rm = TRUE),
    Upper_CI_Permuted_coinciding_prev_AI = quantile(n_coinciding_prev_AI, probs = 0.975, na.rm = TRUE),
    
    Mean_Permuted_Share_AI_prev_gen = mean(n_AI_prev_gen, na.rm = TRUE),
    Lower_CI_Permuted_AI_prev_gen = quantile(n_AI_prev_gen, probs = 0.025, na.rm = TRUE),
    Upper_CI_Permuted_AI_prev_gen = quantile(n_AI_prev_gen, probs = 0.975, na.rm = TRUE),
    
    Mean_Permuted_core_fields = mean(n_persistent_core_fields, na.rm = TRUE),
    Lower_CI_core_fields = quantile(n_persistent_core_fields, probs = 0.025, na.rm = TRUE),
    Upper_CI_core_fields = quantile(n_persistent_core_fields, probs = 0.975, na.rm = TRUE),
    
    Mean_Permuted_NOT_core_fields = mean(n_persistent_NOT_core_fields, na.rm = TRUE),
    Lower_CI_NOT_core_fields = quantile(n_persistent_NOT_core_fields, probs = 0.025, na.rm = TRUE),
    Upper_CI_NOT_core_fields = quantile(n_persistent_NOT_core_fields, probs = 0.975, na.rm = TRUE),
    
    Mean_Permuted_coin_core_fields = mean(n_persistent_coin_core_fields, na.rm = TRUE),
    Lower_CI_coin_core_fields = quantile(n_persistent_coin_core_fields, probs = 0.025, na.rm = TRUE),
    Upper_CI_coin_core_fields = quantile(n_persistent_coin_core_fields, probs = 0.975, na.rm = TRUE),
    
    Mean_Permuted_coin_NOT_core_fields = mean(n_persistent_coin_NOT_core_fields, na.rm = TRUE),
    Lower_CI_coin_NOT_core_fields = quantile(n_persistent_coin_NOT_core_fields, probs = 0.025, na.rm = TRUE),
    Upper_CI_coin_NOT_core_fields = quantile(n_persistent_coin_NOT_core_fields, probs = 0.975, na.rm = TRUE),
    
    Mean_Permuted_Share_AIround = mean(Share_justAI_calc, na.rm = TRUE),
    Lower_CI_Permuted_AIround = quantile(Share_justAI_calc, probs = 0.025, na.rm = TRUE),
    Upper_CI_Permuted_AIround = quantile(Share_justAI_calc, probs = 0.975, na.rm = TRUE),
    
    Mean_Permuted_Share_generalround = mean(Share_justgeneral_calc, na.rm = TRUE),
    Lower_CI_Permuted_generalround = quantile(Share_justgeneral_calc, probs = 0.025, na.rm = TRUE),
    Upper_CI_Permuted_generalround = quantile(Share_justgeneral_calc, probs = 0.975, na.rm = TRUE),
    
    .groups = 'drop'
  )

# --- 3. Combine Data for Plotting ---
plot_data <- actual_shares %>%
  left_join(permuted_summary, by = c("ctry_code", "Period")) %>%
  # Ensure Period is an ordered factor for plotting if it's not already
  mutate(Period = factor(Period, levels = unique(Permutations_all_1000$Period), ordered = TRUE),
         Country = factor(ctry_code)) # Use 'Country' for consistency with the example plot

# --- 4. Plot ---
plot_data$Country <- gsub("US", "USA", str_trim(plot_data$Country))
plot_data$Country <- gsub("CN", "China", str_trim(plot_data$Country))
plot_data$Country <- gsub("JP", "Japan", str_trim(plot_data$Country))
plot_data$Country <- gsub("KR", "South Korea", str_trim(plot_data$Country))

country_colors <- c("China" = "#1B9E77", "Japan" = "#D95F02", "South Korea" = "#7570B3", "USA" = "#E7298A")
country_shapes <- c("China" = 21, "Japan" = 22, "South Korea" = 24, "USA" = 23) # Assuming ctry_code maps to these

#order country levels
plot_data$Country <- factor(plot_data$Country, levels = c("Japan", "USA","South Korea","China"))

significance_plot_save <- 
  ggplot(plot_data, aes(x = Period, group = Country)) +
  # Ribbon for permuted confidence interval
  geom_ribbon(aes(ymin = Lower_CI_Permuted, ymax = Upper_CI_Permuted, fill = Country), alpha = 0.2, show.legend = FALSE) +
  # Line for the mean of permuted shares
  geom_line(aes(y = Mean_Permuted_Share, color = Country), linetype = "dotted", size = 1, show.legend = FALSE) +
  # Line for actual observed shares
  geom_line(aes(y = Actual_Share_coinciding, color = Country), linetype = "dashed", size = 1.5) +
  # Points for actual observed shares
  geom_point(aes(y = Actual_Share_coinciding, shape = Country, fill = Country, color = Country), size = 5) + # Color for border
  
  scale_shape_manual(values = country_shapes, name = "Country") +
  scale_fill_manual(values = country_colors, name = "Country", guide = "legend") + # Ensure fill legend is shown
  scale_color_manual(values = country_colors, name = "Country", guide = "legend") + # Ensure color legend is shown
  xlab("Interval") +
  ylab("Share of break-in specialisations (%)") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + # Use percent_format
  theme_classic(base_size = 14) +
  theme(
    strip.background = element_blank(), # Optional: remove facet strip background
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "top", # Or "bottom", "right", "left"
    axis.text.x = element_text(angle = 45, hjust = 1) # Rotate x-axis labels if needed
  ) +
  facet_wrap(~ Country, scales = "free_y", ncol = 4) + theme(legend.position="none") + 
    ggtitle(paste0("a) Technological field - 15-year Intervals"))

####2.1.3.2. For 5-years ------
rm(list = setdiff(ls(), "significance_plot_save"))
#historical data for double-check
Robustness_data_RTA <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/robustness_automated/final_ipc_rcas_df.csv", 
                                sep = ";", header = TRUE, dec=",")

# Replace NA with 0 and sum the three parts
Robustness_data_RTA[is.na(Robustness_data_RTA)] <- 0
Robustness_data_RTA_5_years <- Robustness_data_RTA[
  Robustness_data_RTA$Period %in% c(
    "1974-1978",
    "1979-1983",
    "1984-1988",
    "1989-1993",
    "1994-1998",
    "1999-2003",
    "2004-2008",
    "2009-2013",
    "2014-2018"
  ), ]
rm(Robustness_data_RTA)

#read permutations
#1st period
Permutations_1st <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/robustness/final_rca_all_permutations_df_5y_1st_Period.csv", 
                             sep = ";", dec = ",", header = TRUE)
Permutations_1st$Period <- "1974-1978"
Permutations_1st <- Permutations_1st[Permutations_1st$ctry_code == "JP"|Permutations_1st$ctry_code == "US"|
                                       Permutations_1st$ctry_code == "CN"|Permutations_1st$ctry_code == "KR",]
Reference_1st <- Permutations_1st[Permutations_1st$permutation_number == 0,]

#2nd period
Permutations_2nd <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/robustness/final_rca_all_permutations_df_5y_2nd_Period.csv", 
                             sep = ";", dec = ",", header = TRUE)
Permutations_2nd$Period <- "1979-1983"
Permutations_2nd <- Permutations_2nd[Permutations_2nd$ctry_code == "JP"|Permutations_2nd$ctry_code == "US"|
                                       Permutations_2nd$ctry_code == "CN"|Permutations_2nd$ctry_code == "KR",]
Reference_2nd <- Permutations_2nd[Permutations_2nd$permutation_number == 0,]

#3rd period
Permutations_3rd <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/robustness/final_rca_all_permutations_df_5y_3rd_Period.csv", 
                             sep = ";", dec = ",", header = TRUE)
Permutations_3rd$Period <- "1984-1988"
Permutations_3rd <- Permutations_3rd[Permutations_3rd$ctry_code == "JP"|Permutations_3rd$ctry_code == "US"|
                                       Permutations_3rd$ctry_code == "CN"|Permutations_3rd$ctry_code == "KR",]
Reference_3rd <- Permutations_3rd[Permutations_3rd$permutation_number == 0,]

#4th period
Permutations_4th <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/robustness/final_rca_all_permutations_df_5y_4th_Period.csv", 
                             sep = ";", dec = ",", header = TRUE)
Permutations_4th$Period <- "1989-1993"
Permutations_4th <- Permutations_4th[Permutations_4th$ctry_code == "JP"|Permutations_4th$ctry_code == "US"|
                                       Permutations_4th$ctry_code == "CN"|Permutations_4th$ctry_code == "KR",]

#5th period
Permutations_5th <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/robustness/final_rca_all_permutations_df_5y_5th_Period.csv", 
                             sep = ";", dec = ",", header = TRUE)
Permutations_5th$Period <- "1994-1998"
Permutations_5th <- Permutations_5th[Permutations_5th$ctry_code == "JP"|Permutations_5th$ctry_code == "US"|
                                       Permutations_5th$ctry_code == "CN"|Permutations_5th$ctry_code == "KR",]

#6th period
Permutations_6th <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/robustness/final_rca_all_permutations_df_5y_6th_Period.csv", 
                             sep = ";", dec = ",", header = TRUE)
Permutations_6th$Period <- "1999-2003"
Permutations_6th <- Permutations_6th[Permutations_6th$ctry_code == "JP"|Permutations_6th$ctry_code == "US"|
                                       Permutations_6th$ctry_code == "CN"|Permutations_6th$ctry_code == "KR",]

#7th period
Permutations_7th <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/robustness/final_rca_all_permutations_df_5y_7th_Period.csv", 
                             sep = ";", dec = ",", header = TRUE)
Permutations_7th$Period <- "2004-2008"
Permutations_7th <- Permutations_7th[Permutations_7th$ctry_code == "JP"|Permutations_7th$ctry_code == "US"|
                                       Permutations_7th$ctry_code == "CN"|Permutations_7th$ctry_code == "KR",]

#8th period
Permutations_8th <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/robustness/final_rca_all_permutations_df_5y_8th_Period.csv", 
                             sep = ";", dec = ",", header = TRUE)
Permutations_8th$Period <- "2009-2013"
Permutations_8th <- Permutations_8th[Permutations_8th$ctry_code == "JP"|Permutations_8th$ctry_code == "US"|
                                       Permutations_8th$ctry_code == "CN"|Permutations_8th$ctry_code == "KR",]

#9th period
Permutations_9th <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/robustness/final_rca_all_permutations_df_5y_9th_Period.csv", 
                             sep = ";", dec = ",", header = TRUE)
Permutations_9th$Period <- "2014-2018"
Permutations_9th <- Permutations_9th[Permutations_9th$ctry_code == "JP"|Permutations_9th$ctry_code == "US"|
                                       Permutations_9th$ctry_code == "CN"|Permutations_9th$ctry_code == "KR",]

#double-check with the baseline for the first 3 periods
historical_ai_1st <- Robustness_data_RTA_5_years[Robustness_data_RTA_5_years$Period == "1974-1978",]
historical_ai_1st <- historical_ai_1st[historical_ai_1st$ctry_code == "JP"|historical_ai_1st$ctry_code == "US"|
                                         historical_ai_1st$ctry_code == "CN"|historical_ai_1st$ctry_code == "KR",]
sum(historical_ai_1st$RCA_AI) #8
sum(Reference_1st$RCA) #8

historical_ai_2nd <- Robustness_data_RTA_5_years[Robustness_data_RTA_5_years$Period == "1979-1983",]
historical_ai_2nd <- historical_ai_2nd[historical_ai_2nd$ctry_code == "JP"|historical_ai_2nd$ctry_code == "US"|
                                         historical_ai_2nd$ctry_code == "CN"|historical_ai_2nd$ctry_code == "KR",]
sum(historical_ai_2nd$RCA_AI) #12.22222
sum(Reference_2nd$RCA) #12.22222

historical_ai_3rd <- Robustness_data_RTA_5_years[Robustness_data_RTA_5_years$Period == "1984-1988",]
historical_ai_3rd <- historical_ai_3rd[historical_ai_3rd$ctry_code == "JP"|historical_ai_3rd$ctry_code == "US"|
                                         historical_ai_3rd$ctry_code == "CN"|historical_ai_3rd$ctry_code == "KR",]
sum(historical_ai_3rd$RCA_AI) #43.71137
sum(Reference_3rd$RCA) #43.71137
#all correct!
rm(Reference_1st,Reference_2nd,Reference_3rd)

historical_ai_4th <- Robustness_data_RTA_5_years[Robustness_data_RTA_5_years$Period == "1989-1993",]
historical_ai_4th <- historical_ai_4th[historical_ai_4th$ctry_code == "JP"|historical_ai_4th$ctry_code == "US"|
                                         historical_ai_4th$ctry_code == "CN"|historical_ai_4th$ctry_code == "KR",]

historical_ai_5th <- Robustness_data_RTA_5_years[Robustness_data_RTA_5_years$Period == "1994-1998",]
historical_ai_5th <- historical_ai_5th[historical_ai_5th$ctry_code == "JP"|historical_ai_5th$ctry_code == "US"|
                                         historical_ai_5th$ctry_code == "CN"|historical_ai_5th$ctry_code == "KR",]

historical_ai_6th <- Robustness_data_RTA_5_years[Robustness_data_RTA_5_years$Period == "1999-2003",]
historical_ai_6th <- historical_ai_6th[historical_ai_6th$ctry_code == "JP"|historical_ai_6th$ctry_code == "US"|
                                         historical_ai_6th$ctry_code == "CN"|historical_ai_6th$ctry_code == "KR",]

historical_ai_7th <- Robustness_data_RTA_5_years[Robustness_data_RTA_5_years$Period == "2004-2008",]
historical_ai_7th <- historical_ai_7th[historical_ai_7th$ctry_code == "JP"|historical_ai_7th$ctry_code == "US"|
                                         historical_ai_7th$ctry_code == "CN"|historical_ai_7th$ctry_code == "KR",]

historical_ai_8th <- Robustness_data_RTA_5_years[Robustness_data_RTA_5_years$Period == "2009-2013",]
historical_ai_8th <- historical_ai_8th[historical_ai_8th$ctry_code == "JP"|historical_ai_8th$ctry_code == "US"|
                                         historical_ai_8th$ctry_code == "CN"|historical_ai_8th$ctry_code == "KR",]

historical_ai_9th <- Robustness_data_RTA_5_years[Robustness_data_RTA_5_years$Period == "2014-2018",]
historical_ai_9th <- historical_ai_9th[historical_ai_9th$ctry_code == "JP"|historical_ai_9th$ctry_code == "US"|
                                         historical_ai_9th$ctry_code == "CN"|historical_ai_9th$ctry_code == "KR",]

#now, for making it sure that the technological fields don't disappear when there is no AI specialization:
# Define all possible techn_field_nr values
all_techn_fields <- 1:35
all_periods  <- c("1974-1978",
                  "1979-1983",
                  "1984-1988",
                  "1989-1993",
                  "1994-1998",
                  "1999-2003",
                  "2004-2008",
                  "2009-2013",
                  "2014-2018")
all_ctry_codes <- c("CN", "JP", "KR", "US") # Define ALL countries you want in the final dataset

all_permutation_numbers <- unique(Permutations_1st$permutation_number)
if (length(all_permutation_numbers) == 0) { # Handle case where Permutations_1st might be empty
  all_permutation_numbers <- 0 # Or the default/expected range, e.g., 0:1000
}

scaffold_groups <- expand.grid(
  ctry_code = all_ctry_codes,
  permutation_number = all_permutation_numbers,
  Period = all_periods,
  stringsAsFactors = FALSE # Important for character vectors
) %>% as_tibble()

# --- Now expand this group scaffold by all technological fields ---
scaffold_df <- scaffold_groups %>%
  expand(nesting(ctry_code, permutation_number, Period), # Uses ALL defined combinations
         techn_field_nr = all_techn_fields)

#for the first period:
# 1. Create a scaffold of all desired combinations
#    Get unique combinations of ctry_code, permutation_number, Period from the data
#unique_groups <- Permutations_1st %>%
#  distinct(ctry_code, permutation_number, Period)

#scaffold_df <- unique_groups %>%
#  expand(nesting(ctry_code, permutation_number, Period), # Uses existing combinations
#         techn_field_nr = all_techn_fields)

# 2. Left join the original data to this scaffold
Permutations_1st_completed_v2 <- left_join(
  scaffold_df,
  Permutations_1st,
  by = c("ctry_code", "permutation_number", "Period", "techn_field_nr")
)

# 3. Fill NA values in RCA with 0 (for the newly created rows)
Permutations_1st_completed_v2 <- Permutations_1st_completed_v2 %>%
  mutate(RCA = ifelse(is.na(RCA), 0, RCA)) %>%
  arrange(permutation_number, ctry_code, Period, techn_field_nr)

#for the second period:
scaffold_groups <- expand.grid(
  ctry_code = all_ctry_codes,
  permutation_number = all_permutation_numbers,
  Period = all_periods,
  stringsAsFactors = FALSE # Important for character vectors
) %>% as_tibble()

# --- Now expand this group scaffold by all technological fields ---
scaffold_df <- scaffold_groups %>%
  expand(nesting(ctry_code, permutation_number, Period), # Uses ALL defined combinations
         techn_field_nr = all_techn_fields)
# 2. Left join the original data to this scaffold
Permutations_2nd_completed_v2 <- left_join(
  scaffold_df,
  Permutations_2nd,
  by = c("ctry_code", "permutation_number", "Period", "techn_field_nr")
)

# 3. Fill NA values in RCA with 0 (for the newly created rows)
Permutations_2nd_completed_v2 <- Permutations_2nd_completed_v2 %>%
  mutate(RCA = ifelse(is.na(RCA), 0, RCA)) %>%
  arrange(permutation_number, ctry_code, Period, techn_field_nr)

#for the third period:
scaffold_groups <- expand.grid(
  ctry_code = all_ctry_codes,
  permutation_number = all_permutation_numbers,
  Period = all_periods,
  stringsAsFactors = FALSE # Important for character vectors
) %>% as_tibble()

# --- Now expand this group scaffold by all technological fields ---
scaffold_df <- scaffold_groups %>%
  expand(nesting(ctry_code, permutation_number, Period), # Uses ALL defined combinations
         techn_field_nr = all_techn_fields)

# 2. Left join the original data to this scaffold
Permutations_3rd_completed_v2 <- left_join(
  scaffold_df,
  Permutations_3rd,
  by = c("ctry_code", "permutation_number", "Period", "techn_field_nr")
)

# 3. Fill NA values in RCA with 0 (for the newly created rows)
Permutations_3rd_completed_v2 <- Permutations_3rd_completed_v2 %>%
  mutate(RCA = ifelse(is.na(RCA), 0, RCA)) %>%
  arrange(permutation_number, ctry_code, Period, techn_field_nr)

#for the fourth period:
scaffold_groups <- expand.grid(
  ctry_code = all_ctry_codes,
  permutation_number = all_permutation_numbers,
  Period = all_periods,
  stringsAsFactors = FALSE # Important for character vectors
) %>% as_tibble()

# --- Now expand this group scaffold by all technological fields ---
scaffold_df <- scaffold_groups %>%
  expand(nesting(ctry_code, permutation_number, Period), # Uses ALL defined combinations
         techn_field_nr = all_techn_fields)

# 2. Left join the original data to this scaffold
Permutations_4th_completed_v2 <- left_join(
  scaffold_df,
  Permutations_4th,
  by = c("ctry_code", "permutation_number", "Period", "techn_field_nr")
)

# 3. Fill NA values in RCA with 0 (for the newly created rows)
Permutations_4th_completed_v2 <- Permutations_4th_completed_v2 %>%
  mutate(RCA = ifelse(is.na(RCA), 0, RCA)) %>%
  arrange(permutation_number, ctry_code, Period, techn_field_nr)

#for the fifth period:
scaffold_groups <- expand.grid(
  ctry_code = all_ctry_codes,
  permutation_number = all_permutation_numbers,
  Period = all_periods,
  stringsAsFactors = FALSE # Important for character vectors
) %>% as_tibble()

# --- Now expand this group scaffold by all technological fields ---
scaffold_df <- scaffold_groups %>%
  expand(nesting(ctry_code, permutation_number, Period), # Uses ALL defined combinations
         techn_field_nr = all_techn_fields)

# 2. Left join the original data to this scaffold
Permutations_5th_completed_v2 <- left_join(
  scaffold_df,
  Permutations_5th,
  by = c("ctry_code", "permutation_number", "Period", "techn_field_nr")
)

# 3. Fill NA values in RCA with 0 (for the newly created rows)
Permutations_5th_completed_v2 <- Permutations_5th_completed_v2 %>%
  mutate(RCA = ifelse(is.na(RCA), 0, RCA)) %>%
  arrange(permutation_number, ctry_code, Period, techn_field_nr)

#for the sixth period:
scaffold_groups <- expand.grid(
  ctry_code = all_ctry_codes,
  permutation_number = all_permutation_numbers,
  Period = all_periods,
  stringsAsFactors = FALSE # Important for character vectors
) %>% as_tibble()

# --- Now expand this group scaffold by all technological fields ---
scaffold_df <- scaffold_groups %>%
  expand(nesting(ctry_code, permutation_number, Period), # Uses ALL defined combinations
         techn_field_nr = all_techn_fields)

# 2. Left join the original data to this scaffold
Permutations_6th_completed_v2 <- left_join(
  scaffold_df,
  Permutations_6th,
  by = c("ctry_code", "permutation_number", "Period", "techn_field_nr")
)

# 3. Fill NA values in RCA with 0 (for the newly created rows)
Permutations_6th_completed_v2 <- Permutations_6th_completed_v2 %>%
  mutate(RCA = ifelse(is.na(RCA), 0, RCA)) %>%
  arrange(permutation_number, ctry_code, Period, techn_field_nr)

#for the seventh period:
scaffold_groups <- expand.grid(
  ctry_code = all_ctry_codes,
  permutation_number = all_permutation_numbers,
  Period = all_periods,
  stringsAsFactors = FALSE # Important for character vectors
) %>% as_tibble()

# --- Now expand this group scaffold by all technological fields ---
scaffold_df <- scaffold_groups %>%
  expand(nesting(ctry_code, permutation_number, Period), # Uses ALL defined combinations
         techn_field_nr = all_techn_fields)

# 2. Left join the original data to this scaffold
Permutations_7th_completed_v2 <- left_join(
  scaffold_df,
  Permutations_7th,
  by = c("ctry_code", "permutation_number", "Period", "techn_field_nr")
)

# 3. Fill NA values in RCA with 0 (for the newly created rows)
Permutations_7th_completed_v2 <- Permutations_7th_completed_v2 %>%
  mutate(RCA = ifelse(is.na(RCA), 0, RCA)) %>%
  arrange(permutation_number, ctry_code, Period, techn_field_nr)

#for the eigth period:
scaffold_groups <- expand.grid(
  ctry_code = all_ctry_codes,
  permutation_number = all_permutation_numbers,
  Period = all_periods,
  stringsAsFactors = FALSE # Important for character vectors
) %>% as_tibble()

# --- Now expand this group scaffold by all technological fields ---
scaffold_df <- scaffold_groups %>%
  expand(nesting(ctry_code, permutation_number, Period), # Uses ALL defined combinations
         techn_field_nr = all_techn_fields)

# 2. Left join the original data to this scaffold
Permutations_8th_completed_v2 <- left_join(
  scaffold_df,
  Permutations_8th,
  by = c("ctry_code", "permutation_number", "Period", "techn_field_nr")
)

# 3. Fill NA values in RCA with 0 (for the newly created rows)
Permutations_8th_completed_v2 <- Permutations_8th_completed_v2 %>%
  mutate(RCA = ifelse(is.na(RCA), 0, RCA)) %>%
  arrange(permutation_number, ctry_code, Period, techn_field_nr)

#for the nineth period:
scaffold_groups <- expand.grid(
  ctry_code = all_ctry_codes,
  permutation_number = all_permutation_numbers,
  Period = all_periods,
  stringsAsFactors = FALSE # Important for character vectors
) %>% as_tibble()

# --- Now expand this group scaffold by all technological fields ---
scaffold_df <- scaffold_groups %>%
  expand(nesting(ctry_code, permutation_number, Period), # Uses ALL defined combinations
         techn_field_nr = all_techn_fields)

# 2. Left join the original data to this scaffold
Permutations_9th_completed_v2 <- left_join(
  scaffold_df,
  Permutations_9th,
  by = c("ctry_code", "permutation_number", "Period", "techn_field_nr")
)

# 3. Fill NA values in RCA with 0 (for the newly created rows)
Permutations_9th_completed_v2 <- Permutations_9th_completed_v2 %>%
  mutate(RCA = ifelse(is.na(RCA), 0, RCA)) %>%
  arrange(permutation_number, ctry_code, Period, techn_field_nr)

#now, left_join everything
Permutations_1st <- left_join(historical_ai_1st, Permutations_1st_completed_v2, 
                              by = c("ctry_code","techn_field_nr", "Period"))
#table(Permutations_1st$ctry_code)
#test <- Permutations_1st[Permutations_1st$ctry_code == "JP",]
#table(test$permutation_number) #hence, now it's fixed with 35 observations consistently
#length(unique(test$permutation_number)) #5001 for JP, which is correct, although the number of techn fields varies between 33 and 35; there is only 25 for the 0 permutation

Permutations_2nd <- left_join(historical_ai_2nd, Permutations_2nd_completed_v2, 
                              by = c("ctry_code","techn_field_nr", "Period"))

Permutations_3rd <- left_join(historical_ai_3rd, Permutations_3rd_completed_v2, 
                              by = c("ctry_code","techn_field_nr", "Period"))

Permutations_4th <- left_join(historical_ai_4th, Permutations_4th_completed_v2, 
                              by = c("ctry_code","techn_field_nr", "Period"))

Permutations_5th <- left_join(historical_ai_5th, Permutations_5th_completed_v2, 
                              by = c("ctry_code","techn_field_nr", "Period"))

Permutations_6th <- left_join(historical_ai_6th, Permutations_6th_completed_v2, 
                              by = c("ctry_code","techn_field_nr", "Period"))

Permutations_7th <- left_join(historical_ai_7th, Permutations_7th_completed_v2, 
                              by = c("ctry_code","techn_field_nr", "Period"))

Permutations_8th <- left_join(historical_ai_8th, Permutations_8th_completed_v2, 
                              by = c("ctry_code","techn_field_nr", "Period"))

Permutations_9th <- left_join(historical_ai_9th, Permutations_9th_completed_v2, 
                              by = c("ctry_code","techn_field_nr", "Period"))

Permutations_all <- rbind(Permutations_1st, Permutations_2nd, Permutations_3rd,Permutations_4th,Permutations_5th,
                          Permutations_6th,Permutations_7th,Permutations_8th,Permutations_9th)

rm(Permutations_1st, Permutations_2nd, Permutations_3rd,Permutations_4th,Permutations_5th,
   Permutations_6th,Permutations_7th,Permutations_8th,Permutations_9th,
   historical_ai_1st, historical_ai_2nd, historical_ai_3rd, historical_ai_4th,historical_ai_5th,
   historical_ai_6th,historical_ai_7th,historical_ai_8th,historical_ai_9th,
   Robustness_data_RTA_5_years, Permutations_1st_completed_v2, Permutations_2nd_completed_v2, 
   Permutations_3rd_completed_v2, Permutations_4th_completed_v2,Permutations_5th_completed_v2,
   Permutations_6th_completed_v2,Permutations_7th_completed_v2,Permutations_8th_completed_v2,
   Permutations_9th_completed_v2,scaffold_df, unique_groups)

Permutations_all_1000 <- Permutations_all[Permutations_all$permutation_number <= 1000,]

#create the overlapping variables
Threshold <- 1
Permutations_all_1000$Round_general <- ifelse(Permutations_all_1000$RCA_Gen < Threshold, 0, 1)
Permutations_all_1000$Round_AI <- ifelse(Permutations_all_1000$RCA < Threshold, 0, 1)
Permutations_all_1000$justGeneral <- ifelse(Permutations_all_1000$RCA_Gen >= Threshold & 
                                              Permutations_all_1000$RCA < Threshold, 1, 0)
Permutations_all_1000$justAI <- ifelse(Permutations_all_1000$RCA >= Threshold & 
                                         Permutations_all_1000$RCA_Gen < Threshold, 1, 0)
#fix Total_RCA:
Permutations_all_1000$Total_RCA <- Permutations_all_1000$Round_general + 2*Permutations_all_1000$Round_AI

test <-Permutations_all_1000[Permutations_all_1000$ctry_code == "JP" & Permutations_all_1000$permutation_number==0 &
                               Permutations_all_1000$Period == "1974-1978",]

# --- 1. Calculate Percentages ---
# Total number of unique technological fields (assuming it's fixed at 35)
total_tech_fields <- 35

period_levels <- sort(unique(Permutations_all_1000$Period))
Permutations_all_1000 <- Permutations_all_1000 %>%
  mutate(Period = factor(Period, levels = period_levels, ordered = TRUE))

unique(Permutations_all_1000$Period)
#drop na cases
Permutations_all_1000 <- Permutations_all_1000[!is.na(Permutations_all_1000$ctry_code), ]

#here
# --- Step 1: Identify Persistent Fields ---
# We need to group by the lowest level (permutation, country, tech_field)
# and then look at the previous period's status.

persistent_fields_df <- Permutations_all_1000 %>%
  # Group by the entities for which persistence is tracked over time
  group_by(permutation_number, ctry_code, techn_field_nr) %>%
  arrange(Period) %>% # IMPORTANT: Ensure periods are sorted within each group
  mutate(
    # Was it coinciding in the PREVIOUS period?
    prev_coinciding = lag(Total_RCA == 3, default = FALSE), # default = FALSE for the first period
    # Is it coinciding NOW?
    current_coinciding = (Total_RCA == 3),
    # Is it persistently coinciding (now AND previously)?
    persistent_coinciding_field = current_coinciding & prev_coinciding,
    
    # Was it justGeneral in the PREVIOUS period?
    prev_justGeneral = lag(justGeneral == 1, default = FALSE),
    # Is it justGeneral NOW?
    current_justGeneral = (justGeneral == 1),
    # Is it persistently justGeneral?
    persistent_justGeneral_field = current_justGeneral & prev_justGeneral,
    
    # Was it justAI in the PREVIOUS period?
    prev_justAI = lag(justAI == 1, default = FALSE),
    # Is it justAI NOW?
    current_justAI = (justAI == 1),
    # Is it persistently justAI?
    persistent_justAI_field = current_justAI & prev_justAI,
    
    # Was it Round_AI in the PREVIOUS period?
    prev_Round_AI = lag(Round_AI == 1, default = FALSE),
    # Is it Round_AI NOW?
    current_Round_AI = (Round_AI == 1),
    # Is it persistently Round_AI?
    persistent_Round_AI_field = current_Round_AI & prev_Round_AI,
    
    # Was it Round_general in the PREVIOUS period?
    prev_Round_general = lag(Round_general == 1, default = FALSE),
    # Is it Round_general NOW?
    current_Round_general = (Round_general == 1),
    # Is it persistently Round_general?
    persistent_Round_general_field = current_Round_general & prev_Round_general,
    
    # Was it coinciding and then justAI?
    com_AI_prev_coinciding = current_justAI & prev_coinciding,
    
    # Was it justAI and then coinciding?
    com_coinciding_prev_AI = current_coinciding & prev_justAI, #prev_justAI #prev_Round_AI
    
    # Was it justGeneral and then justAI?
    com_AI_prev_justGeneral = current_justAI & prev_justGeneral, 
    
    #sustains AI in core fields? 6,7,10,12
    sust_core_fields = current_Round_AI & prev_Round_AI &
      (techn_field_nr %in% c(6, 7, 10, 12)), #c(6, 7, 10, 12)), #c(6, 7, 10, 12, 4, 5, 11)
    
    # sustains AI in NOT core fields? 
    sust_NOT_core_fields = current_Round_AI & prev_Round_AI &
      !(techn_field_nr %in% c(6, 7, 10, 12)),
    
    # persistent coinciding in AI core fields? 
    persistent_coinciding_field_core_fields = current_coinciding & prev_coinciding &
      (techn_field_nr %in% c(6, 7, 10, 12)),
    
    # persistent coinciding in NOT AI core fields? 
    persistent_coinciding_field_NOT_core_fields = current_coinciding & prev_coinciding &
      !(techn_field_nr %in% c(6, 7, 10, 12)),
    
    #AI in core fields? 6,7,10,12
    AI_core_fields = current_Round_AI & (techn_field_nr %in% c(6, 7, 10, 12)),
    
    #AI NOT in core fields? 6,7,10,12
    AI_not_core_fields = current_Round_AI & !(techn_field_nr %in% c(6, 7, 10, 12))
  ) %>%
  ungroup() # Ungroup before the next summary

# --- Step 2: Aggregate Counts and Calculate Original Shares ---
# Now, group by permutation, country, and Period to sum the persistent fields
# and also calculate the original metrics as before.

summary_and_persistence_df <- persistent_fields_df %>%
  group_by(permutation_number, ctry_code, Period) %>%
  summarise(
    coinciding_fields_current = sum(current_coinciding, na.rm = TRUE),
    justGeneral_fields_current = sum(current_justGeneral, na.rm = TRUE),
    justAI_fields_current = sum(current_justAI, na.rm = TRUE),
    Round_AI_fields_current = sum(current_Round_AI, na.rm = TRUE),
    Round_general_fields_current = sum(current_Round_general, na.rm = TRUE),
    # Counts of fields that were specialized for at least 2 consecutive periods (ending in current period)
    n_persistent_coinciding = sum(persistent_coinciding_field, na.rm = TRUE),
    n_persistent_justGeneral = sum(persistent_justGeneral_field, na.rm = TRUE),
    n_persistent_justAI = sum(persistent_justAI_field, na.rm = TRUE),
    n_persistent_Round_AI = sum(persistent_Round_AI_field, na.rm = TRUE),
    n_AI_prev_coinciding = sum(com_AI_prev_coinciding, na.rm = TRUE),
    n_coinciding_prev_AI = sum(com_coinciding_prev_AI, na.rm = TRUE), 
    n_AI_prev_gen = sum(com_AI_prev_justGeneral, na.rm = TRUE),
    n_persistent_core_fields = sum(sust_core_fields, na.rm = TRUE),
    n_persistent_NOT_core_fields = sum(sust_NOT_core_fields, na.rm = TRUE), 
    n_persistent_coin_core_fields = sum(persistent_coinciding_field_core_fields, na.rm = TRUE),
    n_persistent_coin_NOT_core_fields = sum(persistent_coinciding_field_NOT_core_fields, na.rm = TRUE),
    
    AI_core_fields = sum(AI_core_fields, na.rm = TRUE),
    AI_not_core_fields = sum(AI_not_core_fields, na.rm = TRUE), #persistent_Round_general_field
    n_persistent_Round_general_field = sum(persistent_Round_general_field, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    # Shares for the original calculation based on new counts
    # Be careful with the denominator here:
    # If you want share of coinciding relative to coinciding + justGeneral for the *current* period:
    Share_coinciding_original_calc = if_else(
      (coinciding_fields_current + justGeneral_fields_current) > 0,
      coinciding_fields_current / (coinciding_fields_current + justGeneral_fields_current),
      0 # Or NA
    ),
    Share_justAI_calc = if_else(
      (coinciding_fields_current + justGeneral_fields_current) > 0,
      Round_AI_fields_current / (35), #coinciding_fields_current  + justAI_fields_current + justGeneral_fields_current
      0 # Or NA
    ),
    Share_justgeneral_calc = if_else(
      (coinciding_fields_current + justGeneral_fields_current) > 0,
      Round_general_fields_current / (35), #coinciding_fields_current + justGeneral_fields_current + justAI_fields_current
      0 # Or NA
    ))

# Separate actual observed data (permutation_number == 0)
actual_shares <- summary_and_persistence_df %>%
  filter(permutation_number == 0) %>%
  select(ctry_code, Period, Actual_Share_coinciding = Share_coinciding_original_calc,
         Actual_Share_Round_AI = Share_justAI_calc,
         Actual_Share_Round_general = Share_justgeneral_calc,
         
         Actual_persistent_coinciding = n_persistent_coinciding,
         Actual_persistent_justGeneral = n_persistent_justGeneral,
         Actual_persistent_justAI = n_persistent_justAI,
         
         Actual_n_persistent_Round_AI = n_persistent_Round_AI, # Is it persistently Round_AI?
         Actual_n_AI_prev_coinciding = n_AI_prev_coinciding, # Was it coinciding and then justAI?
         Actual_n_coinciding_prev_AI = n_coinciding_prev_AI, # Was it justAI and then coinciding?
         Actual_n_AI_prev_gen = n_AI_prev_gen, #Was it justGeneral and then justAI?
         Actual_n_persistent_core_fields = n_persistent_core_fields, #sustains AI in core fields?
         Actual_n_persistent_NOT_core_fields = n_persistent_NOT_core_fields,#sustains AI in NOT core fields?
         Actual_n_persistent_coin_core_fields = n_persistent_coin_core_fields, # persistent coinciding in AI core fields? 
         Actual_n_persistent_coin_NOT_core_fields = n_persistent_coin_NOT_core_fields,# persistent coinciding in NOT AI core fields? 
         
         Actual_AI_core_fields = AI_core_fields,
         Actual_AI_not_core_fields  = AI_not_core_fields,
         Actual_persistent_General_all  = n_persistent_Round_general_field
  ) #14 variables, with the last 8 ones being there just for the regressions

write.csv2(actual_shares, 
           file = "Files_created_with_the_code/data/files_code_Fields_analysis/robustness/actual_shares_5_years.csv", 
           row.names = FALSE)

# Permuted data (permutation_number > 0)
permuted_shares <- summary_and_persistence_df %>%
  filter(permutation_number > 0)

# --- 2. Summarize Permuted Data ---
# Calculate mean and 95% confidence interval (2.5th and 97.5th percentiles)
# for the share of coinciding specializations from permutations
permuted_summary <- permuted_shares %>%
  group_by(ctry_code, Period) %>%
  summarise(
    Mean_Permuted_Share = mean(Share_coinciding_original_calc, na.rm = TRUE),
    Lower_CI_Permuted = quantile(Share_coinciding_original_calc, probs = 0.025, na.rm = TRUE),
    Upper_CI_Permuted = quantile(Share_coinciding_original_calc, probs = 0.975, na.rm = TRUE),
    
    Mean_Permuted_Share_sust_coinc = mean(n_persistent_coinciding, na.rm = TRUE),
    Lower_CI_Permuted_sust_coinc = quantile(n_persistent_coinciding, probs = 0.025, na.rm = TRUE),
    Upper_CI_Permuted_sust_coinc = quantile(n_persistent_coinciding, probs = 0.975, na.rm = TRUE),
    
    Mean_Permuted_Share_gen_coinc = mean(n_persistent_justGeneral, na.rm = TRUE),
    Lower_CI_Permuted_gen_coinc = quantile(n_persistent_justGeneral, probs = 0.025, na.rm = TRUE),
    Upper_CI_Permuted_gen_coinc = quantile(n_persistent_justGeneral, probs = 0.975, na.rm = TRUE),
    
    Mean_Permuted_Share_ai_coinc = mean(n_persistent_justAI, na.rm = TRUE),
    Lower_CI_Permuted_ai_coinc = quantile(n_persistent_justAI, probs = 0.025, na.rm = TRUE),
    Upper_CI_Permuted_ai_coinc = quantile(n_persistent_justAI, probs = 0.975, na.rm = TRUE),
    .groups = 'drop'
  )

# --- 3. Combine Data for Plotting ---
plot_data <- actual_shares %>%
  left_join(permuted_summary, by = c("ctry_code", "Period")) %>%
  # Ensure Period is an ordered factor for plotting if it's not already
  mutate(Period = factor(Period, levels = unique(Permutations_all_1000$Period), ordered = TRUE),
         Country = factor(ctry_code)) # Use 'Country' for consistency with the example plot

# --- 4. Plot ---
# Define the same color palette you used
plot_data$Country <- gsub("US", "USA", str_trim(plot_data$Country))
plot_data$Country <- gsub("CN", "China", str_trim(plot_data$Country))
plot_data$Country <- gsub("JP", "Japan", str_trim(plot_data$Country))
plot_data$Country <- gsub("KR", "South Korea", str_trim(plot_data$Country))

country_colors <- c("China" = "#1B9E77", "Japan" = "#D95F02", "South Korea" = "#7570B3", "USA" = "#E7298A")
country_shapes <- c("China" = 21, "Japan" = 22, "South Korea" = 24, "USA" = 23) # Assuming ctry_code maps to these

#order country levels
plot_data$Country <- factor(plot_data$Country, levels = c("Japan", "USA","South Korea","China"))

# Create the plot, faceted by Country
significance_plot_save2 <- 
  ggplot(plot_data, aes(x = Period, group = Country)) +
  # Ribbon for permuted confidence interval
  geom_ribbon(aes(ymin = Lower_CI_Permuted, ymax = Upper_CI_Permuted, fill = Country), alpha = 0.2, show.legend = FALSE) +
  # Line for the mean of permuted shares
  geom_line(aes(y = Mean_Permuted_Share, color = Country), linetype = "dotted", size = 1, show.legend = FALSE) +
  # Line for actual observed shares
  geom_line(aes(y = Actual_Share_coinciding, color = Country), linetype = "dashed", size = 1.5) +
  # Points for actual observed shares
  geom_point(aes(y = Actual_Share_coinciding, shape = Country, fill = Country, color = Country), size = 5) + # Color for border
  
  scale_shape_manual(values = country_shapes, name = "Country") +
  scale_fill_manual(values = country_colors, name = "Country", guide = "legend") + # Ensure fill legend is shown
  scale_color_manual(values = country_colors, name = "Country", guide = "legend") + # Ensure color legend is shown
  xlab("Interval") +
  ylab("Share of break-in specialisations (%)") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + # Use percent_format
  theme_classic(base_size = 14) +
  theme(
    strip.background = element_blank(), # Optional: remove facet strip background
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "top", # Or "bottom", "right", "left"
    axis.text.x = element_text(angle = 45, hjust = 1) # Rotate x-axis labels if needed
  ) +
  facet_wrap(~ Country, scales = "free_y", ncol = 4) + 
  ggtitle(paste0("b) Technological field - 5-year Intervals")) + theme(legend.position="none")

jpeg("Files_created_with_the_code/figures/robustness/Figure_11_permutations_techn_field.jpg", width = 14, height = 10, units = 'in', res = 300)
grid.arrange(significance_plot_save, significance_plot_save2, ncol = 1)
dev.off()

##2.2.Subclass (4-digits) level -----
###2.2.1. Intervals 15-years-----
rm(list=ls())
#set the working directory to where you saved the R code:
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

group_by_applnID_perm <- function(data){
  data %>%
    group_by(appln_id, permutation_number) %>% # Add permutation_number if an appln_id might exist across permutations
    # If appln_id is unique within each permutation, then just appln_id is fine
    mutate(field_weight = 1 / n()) %>%
    ungroup()
}

group_by_ctry_and_subclass <- function(data){
  data %<>%
    group_by(permutation_number,ctry_code, Subclass) %>%
    summarise(n_tech_reg = sum(field_weight), .groups = 'drop') %>%
    drop_na() 
}

##1.1. Calculate Specializations for Different Time intervals
###1.1.1 Read and prepare big files
# Reading large files in parts to avoid memory issues
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

#we combine the 5 files:
IPC_all_patents_FirstPeriod <- rbind(IPC_all_patents_Part1, IPC_all_patents_Part2, IPC_all_patents_Part3, IPC_all_patents_Part4, IPC_all_patents_Part5)
#and exclude the 3 big ones we just used, so we have back our memory:
rm(IPC_all_patents_Part1, IPC_all_patents_Part2, IPC_all_patents_Part3, IPC_all_patents_Part4, IPC_all_patents_Part5)

length(unique(IPC_all_patents_FirstPeriod$appln_id))#4,820,523 unique publication numbers (i.e., unique priorities) out of
length(IPC_all_patents_FirstPeriod$appln_id) #28,705,762 lines of data
table(IPC_all_patents_FirstPeriod$priority_year)
#we pick just the subclass for analysis:
IPC_all_patents_FirstPeriod$Subclass <- substr(IPC_all_patents_FirstPeriod$ipc_class_symbol,1,4)
setnames(IPC_all_patents_FirstPeriod, c("appln_id", "ctry_code", "ipc_class_symbol", "priority_year","Subclass"))

# Load AI-specific data
ai_patents_df <- fread("other_files/IPCs_AI.csv", sep=";", dec=",", header=TRUE)

#now we pick just the subclass for analysis:
ai_patents_df$Subclass <- substr(ai_patents_df$ipc_class_symbol,1,4)

###Calculate per interval
####Interval 1 (1974 - 1988) 
# Define Time Interval
start_year <- 1973
end_year   <- 1989

###Filter Data for Interval 1 
ipc_all_patents_first_period_df <- IPC_all_patents_FirstPeriod[priority_year > start_year & priority_year < end_year]
ipc_all_patents_first_period_df <- ipc_all_patents_first_period_df[, .(appln_id, ctry_code, Subclass)]

# Calculate AI-Specific RCA for Interval 1
ai_patents_period_1_df <- ai_patents_df[ai_patents_df$priority_year > start_year & ai_patents_df$priority_year < end_year,]

# Define target countries
target_countries <- c("CN", "JP", "US", "KR") # 

#For MULTIPLE Permutations
num_permutations <- 1000 
list_of_permuted_dfs <- vector("list", length = num_permutations)

#table(ai_patents_period_1_df$ctry_code)
#table(ai_patents_period_1_df$priority_year)
for (p in 1:num_permutations) {
  if (p %% 100 == 0) print(paste("Permutation number:", p)) # Progress indicator
  
  # This dataframe will hold the permuted AI patents for target countries ONLY for THIS iteration
  permuted_ai_for_target_countries_iter <- data.frame()
  
  for (country in target_countries) {
    # 1. Identify and Count ACTUAL AI patents for the current country from the original AI dataset
    actual_ai_appln_ids_country <- ai_patents_period_1_df %>%
      filter(ctry_code == country) %>%
      distinct(appln_id) %>%
      pull(appln_id)
    
    n_ai_country <- length(actual_ai_appln_ids_country)
    
    if (n_ai_country == 0) {
      # print(paste("No AI patents found for", country, "in original AI data. Skipping for perm", p))
      next # Skip to the next country if no AI patents to replace
    }
    
    # 2. Prepare the pool of ALL patents for the current country from the general dataset
    country_all_patents_pool <- ipc_all_patents_first_period_df %>%
      filter(ctry_code == country) %>%
      distinct(appln_id)
    
    if (nrow(country_all_patents_pool) == 0) {
      # print(paste("No patents in general pool for", country, ". Skipping for perm", p))
      next
    }
    
    # Handle cases where the pool is smaller than the number of AI patents to sample
    # This is unlikely if ipc_all_patents_first_period_df is complete, but good for robustness
    sample_size <- min(n_ai_country, nrow(country_all_patents_pool))
    replace_sampling <- FALSE
    if (n_ai_country > nrow(country_all_patents_pool)) {
      if(nrow(country_all_patents_pool) < n_ai_country && !replace_sampling){
        next # Skip this country for this permutation if not enough patents
      }
    }
    
    # 3. Randomly select an equivalent number of unique appln_ids from this country's general pool
    random_appln_ids_country <- sample(country_all_patents_pool$appln_id,
                                       size = sample_size, # Use adjusted sample_size
                                       replace = replace_sampling) # Use replace_sampling flag
    
    # 4. Get all rows for these randomly selected patents from the ipc_all_patents_first_period_df
    randomly_selected_patents_df_country <- ipc_all_patents_first_period_df %>%
      filter(appln_id %in% random_appln_ids_country & ctry_code == country)
    
    # 5. Add these randomly selected patents for the current country to the iteration's df
    if (nrow(randomly_selected_patents_df_country) > 0) {
      permuted_ai_for_target_countries_iter <- bind_rows(
        permuted_ai_for_target_countries_iter,
        randomly_selected_patents_df_country
      )
    }
  } # End of country loop
  
  # Add the permutation number to all rows of this iteration's dataframe
  if (nrow(permuted_ai_for_target_countries_iter) > 0) {
    permuted_ai_for_target_countries_iter$permutation_number <- p
  }
  
  # Store the dataframe for this iteration in the list
  list_of_permuted_dfs[[p]] <- permuted_ai_for_target_countries_iter
  
} # End of permutation loop

# Combine all permuted dataframes from the list into one large dataframe
final_permuted_dataset <- bind_rows(list_of_permuted_dfs)

#pick the original AI patents and save them as a final_permuted_dataset$permutation_number == 0 (so
#that I can double-check in the end that the calculation remains correct);
'%notin%' <- Negate('%in%')
not_selected_AI <- ai_patents_period_1_df[ai_patents_period_1_df$ctry_code %notin% target_countries, c("appln_id", "ctry_code")]
table(not_selected_AI$ctry_code)
#repeat the original dataset of non-selected countries a num_permutations number of times;

# Create a list to hold the replicated dataframes
list_of_replicated_not_selected_ai <- vector("list", length = num_permutations + 1)

# Loop from 0 to num_permutations
for (i in 0:num_permutations) {
  # Create a copy of the not_selected_AI dataframe for this iteration
  temp_df <- not_selected_AI
  
  # Add the permutation_number column
  temp_df$permutation_number <- i
  
  # Store it in the list (adjust index because list is 1-based, i is 0-based)
  list_of_replicated_not_selected_ai[[i + 1]] <- temp_df
  
  if (i %% 100 == 0) print(paste("Replicating not_selected_AI for permutation_number:", i))
}

# Combine all replicated dataframes from the list into one large dataframe
replicated_not_selected_ai_final <- bind_rows(list_of_replicated_not_selected_ai)
table(replicated_not_selected_ai_final$permutation_number)

original_selected_AI <- ai_patents_period_1_df[ai_patents_period_1_df$ctry_code %in% target_countries, c("appln_id", "ctry_code")]
table(original_selected_AI$ctry_code)
original_selected_AI$permutation_number <- 0

#exclude the Subclass column (I don't need it for now)
final_permuted_dataset <- subset(final_permuted_dataset, select = -c(Subclass) )
#merge both in the "reference" dataset, when permutation = 0
original_selected_AI <- rbind(original_selected_AI, replicated_not_selected_ai_final)
rm(replicated_not_selected_ai_final)
table(original_selected_AI$ctry_code) #the non selected countries appear x times more, because they appear already for all the permutations
table(ai_patents_period_1_df$ctry_code) 
table(original_selected_AI$permutation_number)
length(unique(original_selected_AI$appln_id)) #435
length(unique(ai_patents_period_1_df$appln_id)) #435

#rbind it with the original dataset:
final_permuted_dataset <- rbind(original_selected_AI,final_permuted_dataset) 
table(final_permuted_dataset$permutation_number)

# Join technological fields info to AI patents
final_permuted_dataset <- distinct_at(final_permuted_dataset, vars(appln_id,permutation_number), .keep_all = T)
final_permuted_dataset <- subset(final_permuted_dataset, select = -c(ctry_code) )

final_permuted_dataset <- left_join(final_permuted_dataset, ipc_all_patents_first_period_df, by = "appln_id")

#Main Workflow incorporating permutations

# 1. Compute weighted fields for permuted AI patents
print("Step 1: Calculating field weights...")
region_tech_fields_perm_df <- group_by_applnID_perm(final_permuted_dataset)

# 2. Aggregate at the permutation-country-technology field level
print("Step 2: Aggregating by country and technology field per permutation...")
region_tech_fields_perm_df <- group_by_ctry_and_subclass(region_tech_fields_perm_df)
print("Counts of records per permutation_number after aggregation:")
print(table(region_tech_fields_perm_df$permutation_number))

print("Step 3: Calculating RCA per permutation...")

# Using dplyr::group_by and then purrr::map or a loop
# The result will be a list of dataframes (one RCA df per permutation)
list_of_rca_dfs <- region_tech_fields_perm_df %>%
  group_by(permutation_number) %>%
  group_split() %>% # This splits the df into a list of dfs, one for each permutation
  purrr::map(~{
    current_permutation_number <- unique(.x$permutation_number)
    print(paste("Processing RCA for permutation_number:", current_permutation_number))
    
    # Matrix creation for the current permutation's data
    mat_reg_tech_perm_AI <- .x %>%
      select(-permutation_number) %>% # Temporarily remove for pivot if it causes issues
      arrange(Subclass, ctry_code) %>%
      pivot_wider(names_from = Subclass,
                  values_from = n_tech_reg,
                  values_fill = 0) # Changed from list(n_tech_reg = 0) for simplicity
    
    # Check if ctry_code column exists and is not empty
    if (!"ctry_code" %in% names(mat_reg_tech_perm_AI) || nrow(mat_reg_tech_perm_AI) == 0 || all(is.na(mat_reg_tech_perm_AI$ctry_code))) {
      print(paste("Skipping permutation", current_permutation_number, "due to missing ctry_code or empty data after pivot."))
      return(NULL) # Return NULL or an empty tibble
    }
    
    # Check for duplicate ctry_codes which would prevent rownames_to_column
    if (any(duplicated(mat_reg_tech_perm_AI$ctry_code))) {
      print(paste("Warning: Duplicate ctry_code found for permutation", current_permutation_number, ". Aggregating or handling needed."))
      return(tibble(permutation_number = current_permutation_number, error="duplicate ctry_code"))
    }
    
    
    mat_reg_tech_perm_AI <- mat_reg_tech_perm_AI %>%
      remove_rownames() %>%
      column_to_rownames(var = "ctry_code") %>%
      as.matrix() %>%  round()# No rounding here, location_quotient might prefer raw numbers
    
    # RTA calculation
    # Ensure matrix is suitable (e.g., no NA/NaN/Inf that location_quotient can't handle)
    if (nrow(mat_reg_tech_perm_AI) == 0 || ncol(mat_reg_tech_perm_AI) == 0) {
      print(paste("Skipping RCA for permutation", current_permutation_number, "due to empty matrix."))
      return(NULL)
    }
    
    # Ensure there are at least two columns for location_quotient (ctry_code was one)
    if (ncol(mat_reg_tech_perm_AI) < 1) { # If only ctry_code was present and now it's rownames
      print(paste("Skipping RCA for permutation", current_permutation_number, "due to insufficient columns in matrix."))
      return(NULL)
    }
    
    
    # Check for all zero rows/columns if location_quotient is sensitive
    rca_results_perm <- tryCatch({
      mat_reg_tech_perm_AI %>%
        location_quotient(binary = FALSE) %>% 
        as.data.frame() %>%
        rownames_to_column("ctry_code") %>%
        as_tibble() %>%
        gather(key = "Subclass", value = "RCA", -ctry_code) %>%
        arrange(ctry_code, Subclass) %>%
        mutate(permutation_number = current_permutation_number) # Add back permutation number
    }, error = function(e) {
      print(paste("Error in location_quotient for permutation", current_permutation_number, ":", e$message))
      return(tibble(permutation_number = current_permutation_number, ctry_code=NA, Subclass=NA, RCA=NA, error_message = e$message)) # Return an empty or error-marked tibble
    })
    
    return(rca_results_perm)
  })

# Combine the list of RCA dataframes into one final dataframe
final_rca_all_permutations_df <- bind_rows(list_of_rca_dfs)

# Check for errors in the error column
if("error_message" %in% names(final_rca_all_permutations_df)){
  print("Permutations with errors during RCA calculation:")
  print(final_rca_all_permutations_df %>% filter(!is.na(error_message)))
}

write.csv2(final_rca_all_permutations_df, 
           file = "Files_created_with_the_code/data/files_code_4-digits_analysis/robustness/final_rca_all_permutations_df_1st_Period.csv", 
           row.names = FALSE)

#so, final_rca_all_permutations_df is the result....
historical_ai <- read.csv("Files_created_with_the_code/data/files_code_4-digits_analysis/IPC_RCAs_subclass.csv", 
                          sep = ";", header = TRUE, dec=",")
historical_ai <- historical_ai[historical_ai$Period == "1974-1988",]
historical_ai <- historical_ai[historical_ai$ctry_code == "JP"|historical_ai$ctry_code == "US",]

test <- final_rca_all_permutations_df[final_rca_all_permutations_df$permutation_number == 0,]
test <- test[test$ctry_code == "JP"|test$ctry_code == "US"|
               test$ctry_code == "CN"|test$ctry_code == "KR",]
sum(historical_ai$RCA_AI, na.rm = T) #81.90424
sum(test$RCA, na.rm = T) #81.90424
table(historical_ai$ctry_code)

rm(final_rca_all_permutations_df, historical_ai, ipc_all_patents_first_period_df, ai_patents_period_1_df,
   country_all_patents_pool, final_permuted_dataset, test, list_of_permuted_dfs,list_of_rca_dfs, 
   list_of_replicated_not_selected_ai, not_selected_AI, original_selected_AI, permuted_ai_for_target_countries_iter, 
   randomly_selected_patents_df_country,region_tech_fields_perm_df, temp_df)

#### Interval 2 (1989 - 2003) 
# Define Time Interval 
start_year <- 1988
end_year   <- 2004

ipc_all_patents_first_period_df <- IPC_all_patents_FirstPeriod[priority_year > start_year & priority_year < end_year]
table(ipc_all_patents_first_period_df$priority_year)
ipc_all_patents_first_period_df <- ipc_all_patents_first_period_df[, .(appln_id, ctry_code, Subclass)]

# Calculate AI-Specific RCA for Interval 1
ai_patents_period_1_df <- ai_patents_df[ai_patents_df$priority_year > start_year & ai_patents_df$priority_year < end_year,]

list_of_permuted_dfs <- vector("list", length = num_permutations)

for (p in 1:num_permutations) {
  if (p %% 100 == 0) print(paste("Permutation number:", p)) # Progress indicator
  
  # This dataframe will hold the permuted AI patents for target countries ONLY for THIS iteration
  permuted_ai_for_target_countries_iter <- data.frame()
  
  for (country in target_countries) {
    # 1. Identify and Count ACTUAL AI patents for the current country from the original AI dataset
    actual_ai_appln_ids_country <- ai_patents_period_1_df %>%
      filter(ctry_code == country) %>%
      distinct(appln_id) %>%
      pull(appln_id)
    
    n_ai_country <- length(actual_ai_appln_ids_country)
    
    if (n_ai_country == 0) {
      # print(paste("No AI patents found for", country, "in original AI data. Skipping for perm", p))
      next # Skip to the next country if no AI patents to replace
    }
    
    # 2. Prepare the pool of ALL patents for the current country from the general dataset
    country_all_patents_pool <- ipc_all_patents_first_period_df %>%
      filter(ctry_code == country) %>%
      distinct(appln_id)
    
    if (nrow(country_all_patents_pool) == 0) {
      # print(paste("No patents in general pool for", country, ". Skipping for perm", p))
      next
    }
    
    # Handle cases where the pool is smaller than the number of AI patents to sample
    # This is unlikely if ipc_all_patents_first_period_df is complete, but good for robustness
    sample_size <- min(n_ai_country, nrow(country_all_patents_pool))
    replace_sampling <- FALSE
    if (n_ai_country > nrow(country_all_patents_pool)) {
      if(nrow(country_all_patents_pool) < n_ai_country && !replace_sampling){
        next # Skip this country for this permutation if not enough patents
      }
    }
    
    # 3. Randomly select an equivalent number of unique appln_ids from this country's general pool
    random_appln_ids_country <- sample(country_all_patents_pool$appln_id,
                                       size = sample_size, # Use adjusted sample_size
                                       replace = replace_sampling) # Use replace_sampling flag
    
    # 4. Get all rows for these randomly selected patents from the ipc_all_patents_first_period_df
    randomly_selected_patents_df_country <- ipc_all_patents_first_period_df %>%
      filter(appln_id %in% random_appln_ids_country & ctry_code == country)
    
    # 5. Add these randomly selected patents for the current country to the iteration's df
    if (nrow(randomly_selected_patents_df_country) > 0) {
      permuted_ai_for_target_countries_iter <- bind_rows(
        permuted_ai_for_target_countries_iter,
        randomly_selected_patents_df_country
      )
    }
  } # End of country loop
  
  # Add the permutation number to all rows of this iteration's dataframe
  if (nrow(permuted_ai_for_target_countries_iter) > 0) {
    permuted_ai_for_target_countries_iter$permutation_number <- p
  }
  
  # Store the dataframe for this iteration in the list
  list_of_permuted_dfs[[p]] <- permuted_ai_for_target_countries_iter
  
} # End of permutation loop

# Combine all permuted dataframes from the list into one large dataframe
final_permuted_dataset <- bind_rows(list_of_permuted_dfs)

not_selected_AI <- ai_patents_period_1_df[ai_patents_period_1_df$ctry_code %notin% target_countries, c("appln_id", "ctry_code")]

# Create a list to hold the replicated dataframes
list_of_replicated_not_selected_ai <- vector("list", length = num_permutations + 1)

# Loop from 0 to num_permutations
for (i in 0:num_permutations) {
  # Create a copy of the not_selected_AI dataframe for this iteration
  temp_df <- not_selected_AI
  
  # Add the permutation_number column
  temp_df$permutation_number <- i
  
  # Store it in the list (adjust index because list is 1-based, i is 0-based)
  list_of_replicated_not_selected_ai[[i + 1]] <- temp_df
  
  if (i %% 100 == 0) print(paste("Replicating not_selected_AI for permutation_number:", i))
}

# Combine all replicated dataframes from the list into one large dataframe
replicated_not_selected_ai_final <- bind_rows(list_of_replicated_not_selected_ai)
table(replicated_not_selected_ai_final$permutation_number)

original_selected_AI <- ai_patents_period_1_df[ai_patents_period_1_df$ctry_code %in% target_countries, c("appln_id", "ctry_code")]
table(original_selected_AI$ctry_code)
original_selected_AI$permutation_number <- 0

#exclude the Subclass column (I don't need it for now)
final_permuted_dataset <- subset(final_permuted_dataset, select = -c(Subclass) )
#merge both in the "reference" dataset, when permutation = 0
original_selected_AI <- rbind(original_selected_AI, replicated_not_selected_ai_final)
rm(replicated_not_selected_ai_final)
table(original_selected_AI$ctry_code) #the non selected countries appear x times more, because they appear already for all the permutations
table(ai_patents_period_1_df$ctry_code) 
table(original_selected_AI$permutation_number)
length(unique(original_selected_AI$appln_id)) #7887
length(unique(ai_patents_period_1_df$appln_id)) #7887

#rbind it with the original dataset:
final_permuted_dataset <- rbind(original_selected_AI,final_permuted_dataset) 
table(final_permuted_dataset$permutation_number)

# Join technological fields info to AI patents
final_permuted_dataset <- distinct_at(final_permuted_dataset, vars(appln_id,permutation_number), .keep_all = T)
final_permuted_dataset <- subset(final_permuted_dataset, select = -c(ctry_code) )

final_permuted_dataset <- left_join(final_permuted_dataset, ipc_all_patents_first_period_df, by = "appln_id")

#Main Workflow incorporating permutations
# 1. Compute weighted fields for permuted AI patents
print("Step 1: Calculating field weights...")
region_tech_fields_perm_df <- group_by_applnID_perm(final_permuted_dataset)

# 2. Aggregate at the permutation-country-technology field level
print("Step 2: Aggregating by country and technology field per permutation...")
region_tech_fields_perm_df <- group_by_ctry_and_subclass(region_tech_fields_perm_df)
print("Counts of records per permutation_number after aggregation:")
print(table(region_tech_fields_perm_df$permutation_number))

print("Step 3: Calculating RCA per permutation...")

# Using dplyr::group_by and then purrr::map or a loop
# The result will be a list of dataframes (one RCA df per permutation)
list_of_rca_dfs <- region_tech_fields_perm_df %>%
  group_by(permutation_number) %>%
  group_split() %>% # This splits the df into a list of dfs, one for each permutation
  purrr::map(~{
    current_permutation_number <- unique(.x$permutation_number)
    print(paste("Processing RCA for permutation_number:", current_permutation_number))
    
    # Matrix creation for the current permutation's data
    mat_reg_tech_perm_AI <- .x %>%
      select(-permutation_number) %>% # Temporarily remove for pivot if it causes issues
      arrange(Subclass, ctry_code) %>%
      pivot_wider(names_from = Subclass,
                  values_from = n_tech_reg,
                  values_fill = 0) # Changed from list(n_tech_reg = 0) for simplicity
    
    # Check if ctry_code column exists and is not empty
    if (!"ctry_code" %in% names(mat_reg_tech_perm_AI) || nrow(mat_reg_tech_perm_AI) == 0 || all(is.na(mat_reg_tech_perm_AI$ctry_code))) {
      print(paste("Skipping permutation", current_permutation_number, "due to missing ctry_code or empty data after pivot."))
      return(NULL) # Return NULL or an empty tibble
    }
    
    # Check for duplicate ctry_codes which would prevent rownames_to_column
    if (any(duplicated(mat_reg_tech_perm_AI$ctry_code))) {
      print(paste("Warning: Duplicate ctry_code found for permutation", current_permutation_number, ". Aggregating or handling needed."))
      return(tibble(permutation_number = current_permutation_number, error="duplicate ctry_code"))
    }
    
    
    mat_reg_tech_perm_AI <- mat_reg_tech_perm_AI %>%
      remove_rownames() %>%
      column_to_rownames(var = "ctry_code") %>%
      as.matrix() %>%  round()# No rounding here, location_quotient might prefer raw numbers
    
    # RTA calculation
    # Ensure matrix is suitable (e.g., no NA/NaN/Inf that location_quotient can't handle)
    if (nrow(mat_reg_tech_perm_AI) == 0 || ncol(mat_reg_tech_perm_AI) == 0) {
      print(paste("Skipping RCA for permutation", current_permutation_number, "due to empty matrix."))
      return(NULL)
    }
    
    # Ensure there are at least two columns for location_quotient (ctry_code was one)
    if (ncol(mat_reg_tech_perm_AI) < 1) { # If only ctry_code was present and now it's rownames
      print(paste("Skipping RCA for permutation", current_permutation_number, "due to insufficient columns in matrix."))
      return(NULL)
    }
    
    
    # Check for all zero rows/columns if location_quotient is sensitive
    rca_results_perm <- tryCatch({
      mat_reg_tech_perm_AI %>%
        location_quotient(binary = FALSE) %>% 
        as.data.frame() %>%
        rownames_to_column("ctry_code") %>%
        as_tibble() %>%
        gather(key = "Subclass", value = "RCA", -ctry_code) %>%
        arrange(ctry_code, Subclass) %>%
        mutate(permutation_number = current_permutation_number) # Add back permutation number
    }, error = function(e) {
      print(paste("Error in location_quotient for permutation", current_permutation_number, ":", e$message))
      return(tibble(permutation_number = current_permutation_number, ctry_code=NA, Subclass=NA, RCA=NA, error_message = e$message)) # Return an empty or error-marked tibble
    })
    
    return(rca_results_perm)
  })

# Combine the list of RCA dataframes into one final dataframe
final_rca_all_permutations_df <- bind_rows(list_of_rca_dfs)

# Check for errors in the error column
if("error_message" %in% names(final_rca_all_permutations_df)){
  print("Permutations with errors during RCA calculation:")
  print(final_rca_all_permutations_df %>% filter(!is.na(error_message)))
}

write.csv2(final_rca_all_permutations_df, 
           file = "Files_created_with_the_code/data/files_code_4-digits_analysis/robustness/final_rca_all_permutations_df_2nd_Period.csv", 
           row.names = FALSE)

#so, final_rca_all_permutations_df is the result....
historical_ai <- read.csv("Files_created_with_the_code/data/files_code_4-digits_analysis/IPC_RCAs_subclass.csv", 
                          sep = ";", header = TRUE, dec=",")
historical_ai <- historical_ai[historical_ai$Period == "1989-2003",]
historical_ai <- historical_ai[historical_ai$ctry_code == "JP"|historical_ai$ctry_code == "US"|
                                 historical_ai$ctry_code == "CN"|historical_ai$ctry_code == "KR",]

test <- final_rca_all_permutations_df[final_rca_all_permutations_df$permutation_number == 0,]
test <- test[test$ctry_code == "JP"|test$ctry_code == "US"|
               test$ctry_code == "CN"|test$ctry_code == "KR",]
sum(historical_ai$RCA_AI, na.rm = T) #1245.661
sum(test$RCA, na.rm = T) #1245.661
table(historical_ai$ctry_code)

rm(final_rca_all_permutations_df, historical_ai, ipc_all_patents_first_period_df, ai_patents_period_1_df,
   country_all_patents_pool, final_permuted_dataset, test, list_of_permuted_dfs,list_of_rca_dfs, 
   list_of_replicated_not_selected_ai, not_selected_AI, original_selected_AI, permuted_ai_for_target_countries_iter, 
   randomly_selected_patents_df_country,region_tech_fields_perm_df, temp_df)
rm(IPC_all_patents_FirstPeriod)

#### Interval 3 (2004 - 2018) 
# Load Additional Data for Interval 3
IPC_all_patents_ThirdPeriod <- fread("large_files/All_patents_and_IPC_codes_Part1.csv", header = F)
names(IPC_all_patents_ThirdPeriod) <- c("appln_id", "ctry_code", "ipc_class_symbol", "priority_year")
length(unique(IPC_all_patents_ThirdPeriod$appln_id))#16,271,712 unique publication numbers

#we pick just the subclass for analysis:
IPC_all_patents_ThirdPeriod$Subclass <- substr(IPC_all_patents_ThirdPeriod$ipc_class_symbol,1,4)
setnames(IPC_all_patents_ThirdPeriod, c("appln_id", "ctry_code", "ipc_class_symbol", "priority_year","Subclass"))

# Filter Data for Interval 3 (2004-2018) 
# This dataset only includes patents from 2004 to 2018, so no filter needed
#table(IPC_all_patents_ThirdPeriod$priority_year)
IPC_all_patents_ThirdPeriod <- IPC_all_patents_ThirdPeriod[, .(appln_id, ctry_code, Subclass)]

start_year <- 2003
end_year   <- 2019

# Calculate AI-Specific RCA for Interval 1
ai_patents_period_1_df <- ai_patents_df[ai_patents_df$priority_year > start_year & ai_patents_df$priority_year < end_year,]

list_of_permuted_dfs <- vector("list", length = num_permutations)

#table(ai_patents_period_1_df$ctry_code)
#table(ai_patents_period_1_df$priority_year)
for (p in 1:num_permutations) {
  if (p %% 100 == 0) print(paste("Permutation number:", p)) # Progress indicator
  
  # This dataframe will hold the permuted AI patents for target countries ONLY for THIS iteration
  permuted_ai_for_target_countries_iter <- data.frame()
  
  for (country in target_countries) {
    # 1. Identify and Count ACTUAL AI patents for the current country from the original AI dataset
    actual_ai_appln_ids_country <- ai_patents_period_1_df %>%
      filter(ctry_code == country) %>%
      distinct(appln_id) %>%
      pull(appln_id)
    
    n_ai_country <- length(actual_ai_appln_ids_country)
    
    if (n_ai_country == 0) {
      # print(paste("No AI patents found for", country, "in original AI data. Skipping for perm", p))
      next # Skip to the next country if no AI patents to replace
    }
    
    # 2. Prepare the pool of ALL patents for the current country from the general dataset
    country_all_patents_pool <- IPC_all_patents_ThirdPeriod %>%
      filter(ctry_code == country) %>%
      distinct(appln_id)
    
    if (nrow(country_all_patents_pool) == 0) {
      # print(paste("No patents in general pool for", country, ". Skipping for perm", p))
      next
    }
    
    # Handle cases where the pool is smaller than the number of AI patents to sample
    # This is unlikely if IPC_all_patents_ThirdPeriod is complete, but good for robustness
    sample_size <- min(n_ai_country, nrow(country_all_patents_pool))
    replace_sampling <- FALSE
    if (n_ai_country > nrow(country_all_patents_pool)) {
      if(nrow(country_all_patents_pool) < n_ai_country && !replace_sampling){
        next # Skip this country for this permutation if not enough patents
      }
    }
    
    # 3. Randomly select an equivalent number of unique appln_ids from this country's general pool
    random_appln_ids_country <- sample(country_all_patents_pool$appln_id,
                                       size = sample_size, # Use adjusted sample_size
                                       replace = replace_sampling) # Use replace_sampling flag
    
    # 4. Get all rows for these randomly selected patents from the IPC_all_patents_ThirdPeriod
    randomly_selected_patents_df_country <- IPC_all_patents_ThirdPeriod %>%
      filter(appln_id %in% random_appln_ids_country & ctry_code == country)
    
    # 5. Add these randomly selected patents for the current country to the iteration's df
    if (nrow(randomly_selected_patents_df_country) > 0) {
      permuted_ai_for_target_countries_iter <- bind_rows(
        permuted_ai_for_target_countries_iter,
        randomly_selected_patents_df_country
      )
    }
  } # End of country loop
  
  # Add the permutation number to all rows of this iteration's dataframe
  if (nrow(permuted_ai_for_target_countries_iter) > 0) {
    permuted_ai_for_target_countries_iter$permutation_number <- p
  }
  
  # Store the dataframe for this iteration in the list
  list_of_permuted_dfs[[p]] <- permuted_ai_for_target_countries_iter
  
} # End of permutation loop

# Combine all permuted dataframes from the list into one large dataframe
final_permuted_dataset <- bind_rows(list_of_permuted_dfs)

not_selected_AI <- ai_patents_period_1_df[ai_patents_period_1_df$ctry_code %notin% target_countries, c("appln_id", "ctry_code")]

# Create a list to hold the replicated dataframes
list_of_replicated_not_selected_ai <- vector("list", length = num_permutations + 1)

# Loop from 0 to num_permutations
for (i in 0:num_permutations) {
  # Create a copy of the not_selected_AI dataframe for this iteration
  temp_df <- not_selected_AI
  
  # Add the permutation_number column
  temp_df$permutation_number <- i
  
  # Store it in the list (adjust index because list is 1-based, i is 0-based)
  list_of_replicated_not_selected_ai[[i + 1]] <- temp_df
  
  if (i %% 100 == 0) print(paste("Replicating not_selected_AI for permutation_number:", i))
}

# Combine all replicated dataframes from the list into one large dataframe
replicated_not_selected_ai_final <- bind_rows(list_of_replicated_not_selected_ai)
table(replicated_not_selected_ai_final$permutation_number)

original_selected_AI <- ai_patents_period_1_df[ai_patents_period_1_df$ctry_code %in% target_countries, c("appln_id", "ctry_code")]
table(original_selected_AI$ctry_code)
original_selected_AI$permutation_number <- 0

#exclude the Subclass column (I don't need it for now)
final_permuted_dataset <- subset(final_permuted_dataset, select = -c(Subclass) )
#merge both in the "reference" dataset, when permutation = 0
original_selected_AI <- rbind(original_selected_AI, replicated_not_selected_ai_final)
rm(replicated_not_selected_ai_final)
table(original_selected_AI$ctry_code) #the non selected countries appear x times more, because they appear already for all the permutations
table(ai_patents_period_1_df$ctry_code) 
table(original_selected_AI$permutation_number)
length(unique(original_selected_AI$appln_id)) #34576
length(unique(ai_patents_period_1_df$appln_id)) #34576

#rbind it with the original dataset:
final_permuted_dataset <- rbind(original_selected_AI,final_permuted_dataset) 
table(final_permuted_dataset$permutation_number)

# Join technological fields info to AI patents
final_permuted_dataset <- distinct_at(final_permuted_dataset, vars(appln_id,permutation_number), .keep_all = T)
final_permuted_dataset <- subset(final_permuted_dataset, select = -c(ctry_code) )

final_permuted_dataset <- left_join(final_permuted_dataset, IPC_all_patents_ThirdPeriod, by = "appln_id")

#Main Workflow incorporating permutations
# 1. Compute weighted fields for permuted AI patents
print("Step 1: Calculating field weights...")
region_tech_fields_perm_df <- group_by_applnID_perm(final_permuted_dataset)

# 2. Aggregate at the permutation-country-technology field level
print("Step 2: Aggregating by country and technology field per permutation...")
region_tech_fields_perm_df <- group_by_ctry_and_subclass(region_tech_fields_perm_df)
print("Counts of records per permutation_number after aggregation:")
print(table(region_tech_fields_perm_df$permutation_number))

print("Step 3: Calculating RCA per permutation...")

# Using dplyr::group_by and then purrr::map or a loop
# The result will be a list of dataframes (one RCA df per permutation)
list_of_rca_dfs <- region_tech_fields_perm_df %>%
  group_by(permutation_number) %>%
  group_split() %>% # This splits the df into a list of dfs, one for each permutation
  purrr::map(~{
    current_permutation_number <- unique(.x$permutation_number)
    print(paste("Processing RCA for permutation_number:", current_permutation_number))
    
    # Matrix creation for the current permutation's data
    mat_reg_tech_perm_AI <- .x %>%
      select(-permutation_number) %>% # Temporarily remove for pivot if it causes issues
      arrange(Subclass, ctry_code) %>%
      pivot_wider(names_from = Subclass,
                  values_from = n_tech_reg,
                  values_fill = 0) # Changed from list(n_tech_reg = 0) for simplicity
    
    # Check if ctry_code column exists and is not empty
    if (!"ctry_code" %in% names(mat_reg_tech_perm_AI) || nrow(mat_reg_tech_perm_AI) == 0 || all(is.na(mat_reg_tech_perm_AI$ctry_code))) {
      print(paste("Skipping permutation", current_permutation_number, "due to missing ctry_code or empty data after pivot."))
      return(NULL) # Return NULL or an empty tibble
    }
    
    # Check for duplicate ctry_codes which would prevent rownames_to_column
    if (any(duplicated(mat_reg_tech_perm_AI$ctry_code))) {
      print(paste("Warning: Duplicate ctry_code found for permutation", current_permutation_number, ". Aggregating or handling needed."))
      return(tibble(permutation_number = current_permutation_number, error="duplicate ctry_code"))
    }
    
    
    mat_reg_tech_perm_AI <- mat_reg_tech_perm_AI %>%
      remove_rownames() %>%
      column_to_rownames(var = "ctry_code") %>%
      as.matrix() %>%  round()# No rounding here, location_quotient might prefer raw numbers
    
    # RTA calculation
    # Ensure matrix is suitable (e.g., no NA/NaN/Inf that location_quotient can't handle)
    if (nrow(mat_reg_tech_perm_AI) == 0 || ncol(mat_reg_tech_perm_AI) == 0) {
      print(paste("Skipping RCA for permutation", current_permutation_number, "due to empty matrix."))
      return(NULL)
    }
    
    # Ensure there are at least two columns for location_quotient (ctry_code was one)
    if (ncol(mat_reg_tech_perm_AI) < 1) { # If only ctry_code was present and now it's rownames
      print(paste("Skipping RCA for permutation", current_permutation_number, "due to insufficient columns in matrix."))
      return(NULL)
    }
    
    
    # Check for all zero rows/columns if location_quotient is sensitive
    rca_results_perm <- tryCatch({
      mat_reg_tech_perm_AI %>%
        location_quotient(binary = FALSE) %>% 
        as.data.frame() %>%
        rownames_to_column("ctry_code") %>%
        as_tibble() %>%
        gather(key = "Subclass", value = "RCA", -ctry_code) %>%
        arrange(ctry_code, Subclass) %>%
        mutate(permutation_number = current_permutation_number) # Add back permutation number
    }, error = function(e) {
      print(paste("Error in location_quotient for permutation", current_permutation_number, ":", e$message))
      return(tibble(permutation_number = current_permutation_number, ctry_code=NA, Subclass=NA, RCA=NA, error_message = e$message)) # Return an empty or error-marked tibble
    })
    
    return(rca_results_perm)
  })

# Combine the list of RCA dataframes into one final dataframe
final_rca_all_permutations_df <- bind_rows(list_of_rca_dfs)

# Check for errors in the error column
if("error_message" %in% names(final_rca_all_permutations_df)){
  print("Permutations with errors during RCA calculation:")
  print(final_rca_all_permutations_df %>% filter(!is.na(error_message)))
}

write.csv2(final_rca_all_permutations_df, 
           file = "Files_created_with_the_code/data/files_code_4-digits_analysis/robustness/final_rca_all_permutations_df_3rd_Period.csv", 
           row.names = FALSE)

#so, final_rca_all_permutations_df is the result....
historical_ai <- read.csv("Files_created_with_the_code/data/files_code_4-digits_analysis/IPC_RCAs_subclass.csv", 
                          sep = ";", header = TRUE, dec=",")
historical_ai <- historical_ai[historical_ai$Period == "2004-2018",]
historical_ai <- historical_ai[historical_ai$ctry_code == "JP"|historical_ai$ctry_code == "US"|
                                 historical_ai$ctry_code == "CN"|historical_ai$ctry_code == "KR",]

test <- final_rca_all_permutations_df[final_rca_all_permutations_df$permutation_number == 0,]
test <- test[test$ctry_code == "JP"|test$ctry_code == "US"|
               test$ctry_code == "CN"|test$ctry_code == "KR",]
sum(historical_ai$RCA_AI, na.rm = T) #1451.323
sum(test$RCA, na.rm = T) #1451.323
table(historical_ai$ctry_code)

rm(list=ls())
group_by_applnID_perm <- function(data){
  data %>%
    group_by(appln_id, permutation_number) %>% # Add permutation_number if an appln_id might exist across permutations
    # If appln_id is unique within each permutation, then just appln_id is fine
    mutate(field_weight = 1 / n()) %>%
    ungroup()
}

#group_by_ctry_techn_field_perm

group_by_ctry_and_subclass <- function(data){
  data %<>%
    group_by(permutation_number,ctry_code, Subclass) %>%
    summarise(n_tech_reg = sum(field_weight), .groups = 'drop') %>%
    drop_na() 
}

'%notin%' <- Negate('%in%')

##1.1. Calculate Specializations for Different Time intervals
###1.1.1 Read and prepare big files
ipc_all_patents_part2_df <- fread("large_files/All_patents_and_IPC_codes_Part2.csv", 
                                  header = FALSE)

setnames(ipc_all_patents_part2_df, c("appln_id", "ctry_code", "ipc_class_symbol", "priority_year"))

ipc_all_patents_part2_df$Subclass <- substr(ipc_all_patents_part2_df$ipc_class_symbol,1,4)
setnames(ipc_all_patents_part2_df, c("appln_id", "ctry_code", "ipc_class_symbol", "priority_year","Subclass"))

# Load AI-specific data
ai_patents_df <- fread("other_files/IPCs_AI.csv", sep=";", dec=",", header=TRUE)
#ai_patents_df$ctry_code <- "AI_pat"
#now we pick just the subclass for analysis:
ai_patents_df$Subclass <- substr(ai_patents_df$ipc_class_symbol,1,4)

###2.2.2. Intervals 5-years-----
##Interval 1 (1974-1978) 
# Define Time Interval
start_year <- 1973
end_year   <- 1979

###Filter Data for Interval 1 
ipc_all_patents_first_period_df <- ipc_all_patents_part2_df[priority_year > start_year & priority_year < end_year]
ipc_all_patents_first_period_df <- ipc_all_patents_first_period_df[, .(appln_id, ctry_code, Subclass)]

# Calculate AI-Specific RCA for Interval 1
ai_patents_period_1_df <- ai_patents_df[ai_patents_df$priority_year > start_year & ai_patents_df$priority_year < end_year,]

# Define target countries
target_countries <- c("CN", "JP", "US", "KR") # 

#For MULTIPLE Permutations
num_permutations <- 1000 
list_of_permuted_dfs <- vector("list", length = num_permutations)

for (p in 1:num_permutations) {
  if (p %% 100 == 0) print(paste("Permutation number:", p)) # Progress indicator
  
  # This dataframe will hold the permuted AI patents for target countries ONLY for THIS iteration
  permuted_ai_for_target_countries_iter <- data.frame()
  
  for (country in target_countries) {
    # 1. Identify and Count ACTUAL AI patents for the current country from the original AI dataset
    actual_ai_appln_ids_country <- ai_patents_period_1_df %>%
      filter(ctry_code == country) %>%
      distinct(appln_id) %>%
      pull(appln_id)
    
    n_ai_country <- length(actual_ai_appln_ids_country)
    
    if (n_ai_country == 0) {
      # print(paste("No AI patents found for", country, "in original AI data. Skipping for perm", p))
      next # Skip to the next country if no AI patents to replace
    }
    
    # 2. Prepare the pool of ALL patents for the current country from the general dataset
    country_all_patents_pool <- ipc_all_patents_first_period_df %>%
      filter(ctry_code == country) %>%
      distinct(appln_id)
    
    if (nrow(country_all_patents_pool) == 0) {
      # print(paste("No patents in general pool for", country, ". Skipping for perm", p))
      next
    }
    
    # Handle cases where the pool is smaller than the number of AI patents to sample
    # This is unlikely if ipc_all_patents_first_period_df is complete, but good for robustness
    sample_size <- min(n_ai_country, nrow(country_all_patents_pool))
    replace_sampling <- FALSE
    if (n_ai_country > nrow(country_all_patents_pool)) {
      if(nrow(country_all_patents_pool) < n_ai_country && !replace_sampling){
        next # Skip this country for this permutation if not enough patents
      }
    }
    
    # 3. Randomly select an equivalent number of unique appln_ids from this country's general pool
    random_appln_ids_country <- sample(country_all_patents_pool$appln_id,
                                       size = sample_size, # Use adjusted sample_size
                                       replace = replace_sampling) # Use replace_sampling flag
    
    # 4. Get all rows for these randomly selected patents from the ipc_all_patents_first_period_df
    randomly_selected_patents_df_country <- ipc_all_patents_first_period_df %>%
      filter(appln_id %in% random_appln_ids_country & ctry_code == country)
    
    # 5. Add these randomly selected patents for the current country to the iteration's df
    if (nrow(randomly_selected_patents_df_country) > 0) {
      permuted_ai_for_target_countries_iter <- bind_rows(
        permuted_ai_for_target_countries_iter,
        randomly_selected_patents_df_country
      )
    }
  } # End of country loop
  
  # Add the permutation number to all rows of this iteration's dataframe
  if (nrow(permuted_ai_for_target_countries_iter) > 0) {
    permuted_ai_for_target_countries_iter$permutation_number <- p
  }
  
  # Store the dataframe for this iteration in the list
  list_of_permuted_dfs[[p]] <- permuted_ai_for_target_countries_iter
  
} # End of permutation loop

# Combine all permuted dataframes from the list into one large dataframe
final_permuted_dataset <- bind_rows(list_of_permuted_dfs)

not_selected_AI <- ai_patents_period_1_df[ai_patents_period_1_df$ctry_code %notin% target_countries, c("appln_id", "ctry_code")]

# Create a list to hold the replicated dataframes
list_of_replicated_not_selected_ai <- vector("list", length = num_permutations + 1)

# Loop from 0 to num_permutations
for (i in 0:num_permutations) {
  # Create a copy of the not_selected_AI dataframe for this iteration
  temp_df <- not_selected_AI
  
  # Add the permutation_number column
  temp_df$permutation_number <- i
  
  # Store it in the list (adjust index because list is 1-based, i is 0-based)
  list_of_replicated_not_selected_ai[[i + 1]] <- temp_df
  
  if (i %% 100 == 0) print(paste("Replicating not_selected_AI for permutation_number:", i))
}

# Combine all replicated dataframes from the list into one large dataframe
replicated_not_selected_ai_final <- bind_rows(list_of_replicated_not_selected_ai)
table(replicated_not_selected_ai_final$permutation_number)

original_selected_AI <- ai_patents_period_1_df[ai_patents_period_1_df$ctry_code %in% target_countries, c("appln_id", "ctry_code")]
table(original_selected_AI$ctry_code)
original_selected_AI$permutation_number <- 0

#exclude the Subclass column (I don't need it for now)
final_permuted_dataset <- subset(final_permuted_dataset, select = -c(Subclass) )
#merge both in the "reference" dataset, when permutation = 0
original_selected_AI <- rbind(original_selected_AI, replicated_not_selected_ai_final)
rm(replicated_not_selected_ai_final)
table(original_selected_AI$ctry_code) #the non selected countries appear x times more, because they appear already for all the permutations
table(ai_patents_period_1_df$ctry_code) 
table(original_selected_AI$permutation_number)
length(unique(original_selected_AI$appln_id)) #7
length(unique(ai_patents_period_1_df$appln_id)) #7

#rbind it with the original dataset:
final_permuted_dataset <- rbind(original_selected_AI,final_permuted_dataset) 
table(final_permuted_dataset$permutation_number)

# Join technological fields info to AI patents
final_permuted_dataset <- distinct_at(final_permuted_dataset, vars(appln_id,permutation_number), .keep_all = T)
final_permuted_dataset <- subset(final_permuted_dataset, select = -c(ctry_code) )

final_permuted_dataset <- left_join(final_permuted_dataset, ipc_all_patents_first_period_df, by = "appln_id")

#Main Workflow incorporating permutations
# 1. Compute weighted fields for permuted AI patents
print("Step 1: Calculating field weights...")
region_tech_fields_perm_df <- group_by_applnID_perm(final_permuted_dataset)

# 2. Aggregate at the permutation-country-technology field level
print("Step 2: Aggregating by country and technology field per permutation...")
region_tech_fields_perm_df <- group_by_ctry_and_subclass(region_tech_fields_perm_df)
print("Counts of records per permutation_number after aggregation:")
print(table(region_tech_fields_perm_df$permutation_number))

print("Step 3: Calculating RCA per permutation...")

# Using dplyr::group_by and then purrr::map or a loop
# The result will be a list of dataframes (one RCA df per permutation)
list_of_rca_dfs <- region_tech_fields_perm_df %>%
  group_by(permutation_number) %>%
  group_split() %>% # This splits the df into a list of dfs, one for each permutation
  purrr::map(~{
    current_permutation_number <- unique(.x$permutation_number)
    print(paste("Processing RCA for permutation_number:", current_permutation_number))
    
    # Matrix creation for the current permutation's data
    mat_reg_tech_perm_AI <- .x %>%
      select(-permutation_number) %>% # Temporarily remove for pivot if it causes issues
      arrange(Subclass, ctry_code) %>%
      pivot_wider(names_from = Subclass,
                  values_from = n_tech_reg,
                  values_fill = 0) # Changed from list(n_tech_reg = 0) for simplicity
    
    # Check if ctry_code column exists and is not empty
    if (!"ctry_code" %in% names(mat_reg_tech_perm_AI) || nrow(mat_reg_tech_perm_AI) == 0 || all(is.na(mat_reg_tech_perm_AI$ctry_code))) {
      print(paste("Skipping permutation", current_permutation_number, "due to missing ctry_code or empty data after pivot."))
      return(NULL) # Return NULL or an empty tibble
    }
    
    # Check for duplicate ctry_codes which would prevent rownames_to_column
    if (any(duplicated(mat_reg_tech_perm_AI$ctry_code))) {
      print(paste("Warning: Duplicate ctry_code found for permutation", current_permutation_number, ". Aggregating or handling needed."))
      return(tibble(permutation_number = current_permutation_number, error="duplicate ctry_code"))
    }
    
    
    mat_reg_tech_perm_AI <- mat_reg_tech_perm_AI %>%
      remove_rownames() %>%
      column_to_rownames(var = "ctry_code") %>%
      as.matrix() %>%  round()# No rounding here, location_quotient might prefer raw numbers
    
    # RTA calculation
    # Ensure matrix is suitable (e.g., no NA/NaN/Inf that location_quotient can't handle)
    if (nrow(mat_reg_tech_perm_AI) == 0 || ncol(mat_reg_tech_perm_AI) == 0) {
      print(paste("Skipping RCA for permutation", current_permutation_number, "due to empty matrix."))
      return(NULL)
    }
    
    # Ensure there are at least two columns for location_quotient (ctry_code was one)
    if (ncol(mat_reg_tech_perm_AI) < 1) { # If only ctry_code was present and now it's rownames
      print(paste("Skipping RCA for permutation", current_permutation_number, "due to insufficient columns in matrix."))
      return(NULL)
    }
    
    
    # Check for all zero rows/columns if location_quotient is sensitive
    rca_results_perm <- tryCatch({
      mat_reg_tech_perm_AI %>%
        location_quotient(binary = FALSE) %>% # Assuming this is your function
        as.data.frame() %>%
        rownames_to_column("ctry_code") %>%
        as_tibble() %>%
        gather(key = "Subclass", value = "RCA", -ctry_code) %>%
        arrange(ctry_code, Subclass) %>%
        mutate(permutation_number = current_permutation_number) # Add back permutation number
    }, error = function(e) {
      print(paste("Error in location_quotient for permutation", current_permutation_number, ":", e$message))
      return(tibble(permutation_number = current_permutation_number, ctry_code=NA, Subclass=NA, RCA=NA, error_message = e$message)) # Return an empty or error-marked tibble
    })
    
    return(rca_results_perm)
  })

# Combine the list of RCA dataframes into one final dataframe
final_rca_all_permutations_df <- bind_rows(list_of_rca_dfs)

# Check for errors in the error column
if("error_message" %in% names(final_rca_all_permutations_df)){
  print("Permutations with errors during RCA calculation:")
  print(final_rca_all_permutations_df %>% filter(!is.na(error_message)))
}

write.csv2(final_rca_all_permutations_df, 
           file = "Files_created_with_the_code/data/files_code_4-digits_analysis/robustness/final_rca_all_permutations_df_5y_1st_Period.csv", 
           row.names = FALSE)

#so, final_rca_all_permutations_df is the result....
rm(final_rca_all_permutations_df, ipc_all_patents_first_period_df, ai_patents_period_1_df,
   country_all_patents_pool, final_permuted_dataset, list_of_permuted_dfs,list_of_rca_dfs, 
   list_of_replicated_not_selected_ai, not_selected_AI, original_selected_AI, permuted_ai_for_target_countries_iter, 
   randomly_selected_patents_df_country,region_tech_fields_perm_df, temp_df)

##Interval 2 (1979-1983) 
##"1979-1983",

# Define Time Interval 
start_year <- 1978
end_year   <- 1984

###Filter Data for Interval 2 
ipc_all_patents_first_period_df <- ipc_all_patents_part2_df[priority_year > start_year & priority_year < end_year]
ipc_all_patents_first_period_df <- ipc_all_patents_first_period_df[, .(appln_id, ctry_code, Subclass)]

# Calculate AI-Specific RCA for Interval 1
ai_patents_period_1_df <- ai_patents_df[ai_patents_df$priority_year > start_year & ai_patents_df$priority_year < end_year,]

#For MULTIPLE Permutations
list_of_permuted_dfs <- vector("list", length = num_permutations)

for (p in 1:num_permutations) {
  if (p %% 100 == 0) print(paste("Permutation number:", p)) # Progress indicator
  
  # This dataframe will hold the permuted AI patents for target countries ONLY for THIS iteration
  permuted_ai_for_target_countries_iter <- data.frame()
  
  for (country in target_countries) {
    # 1. Identify and Count ACTUAL AI patents for the current country from the original AI dataset
    actual_ai_appln_ids_country <- ai_patents_period_1_df %>%
      filter(ctry_code == country) %>%
      distinct(appln_id) %>%
      pull(appln_id)
    
    n_ai_country <- length(actual_ai_appln_ids_country)
    
    if (n_ai_country == 0) {
      # print(paste("No AI patents found for", country, "in original AI data. Skipping for perm", p))
      next # Skip to the next country if no AI patents to replace
    }
    
    # 2. Prepare the pool of ALL patents for the current country from the general dataset
    country_all_patents_pool <- ipc_all_patents_first_period_df %>%
      filter(ctry_code == country) %>%
      distinct(appln_id)
    
    if (nrow(country_all_patents_pool) == 0) {
      # print(paste("No patents in general pool for", country, ". Skipping for perm", p))
      next
    }
    
    # Handle cases where the pool is smaller than the number of AI patents to sample
    # This is unlikely if ipc_all_patents_first_period_df is complete, but good for robustness
    sample_size <- min(n_ai_country, nrow(country_all_patents_pool))
    replace_sampling <- FALSE
    if (n_ai_country > nrow(country_all_patents_pool)) {
      if(nrow(country_all_patents_pool) < n_ai_country && !replace_sampling){
        next # Skip this country for this permutation if not enough patents
      }
    }
    
    # 3. Randomly select an equivalent number of unique appln_ids from this country's general pool
    random_appln_ids_country <- sample(country_all_patents_pool$appln_id,
                                       size = sample_size, # Use adjusted sample_size
                                       replace = replace_sampling) # Use replace_sampling flag
    
    # 4. Get all rows for these randomly selected patents from the ipc_all_patents_first_period_df
    randomly_selected_patents_df_country <- ipc_all_patents_first_period_df %>%
      filter(appln_id %in% random_appln_ids_country & ctry_code == country)
    
    # 5. Add these randomly selected patents for the current country to the iteration's df
    if (nrow(randomly_selected_patents_df_country) > 0) {
      permuted_ai_for_target_countries_iter <- bind_rows(
        permuted_ai_for_target_countries_iter,
        randomly_selected_patents_df_country
      )
    }
  } # End of country loop
  
  # Add the permutation number to all rows of this iteration's dataframe
  if (nrow(permuted_ai_for_target_countries_iter) > 0) {
    permuted_ai_for_target_countries_iter$permutation_number <- p
  }
  
  # Store the dataframe for this iteration in the list
  list_of_permuted_dfs[[p]] <- permuted_ai_for_target_countries_iter
  
} # End of permutation loop

# Combine all permuted dataframes from the list into one large dataframe
final_permuted_dataset <- bind_rows(list_of_permuted_dfs)

not_selected_AI <- ai_patents_period_1_df[ai_patents_period_1_df$ctry_code %notin% target_countries, c("appln_id", "ctry_code")]

# Create a list to hold the replicated dataframes
list_of_replicated_not_selected_ai <- vector("list", length = num_permutations + 1)

# Loop from 0 to num_permutations
for (i in 0:num_permutations) {
  # Create a copy of the not_selected_AI dataframe for this iteration
  temp_df <- not_selected_AI
  
  # Add the permutation_number column
  temp_df$permutation_number <- i
  
  # Store it in the list (adjust index because list is 1-based, i is 0-based)
  list_of_replicated_not_selected_ai[[i + 1]] <- temp_df
  
  if (i %% 100 == 0) print(paste("Replicating not_selected_AI for permutation_number:", i))
}

# Combine all replicated dataframes from the list into one large dataframe
replicated_not_selected_ai_final <- bind_rows(list_of_replicated_not_selected_ai)
table(replicated_not_selected_ai_final$permutation_number)

original_selected_AI <- ai_patents_period_1_df[ai_patents_period_1_df$ctry_code %in% target_countries, c("appln_id", "ctry_code")]
table(original_selected_AI$ctry_code)
original_selected_AI$permutation_number <- 0

#exclude the Subclass column (I don't need it for now)
final_permuted_dataset <- subset(final_permuted_dataset, select = -c(Subclass) )
#merge both in the "reference" dataset, when permutation = 0
original_selected_AI <- rbind(original_selected_AI, replicated_not_selected_ai_final)
rm(replicated_not_selected_ai_final)
table(original_selected_AI$ctry_code) #the non selected countries appear x times more, because they appear already for all the permutations
table(ai_patents_period_1_df$ctry_code) 
table(original_selected_AI$permutation_number)
length(unique(original_selected_AI$appln_id)) #10
length(unique(ai_patents_period_1_df$appln_id)) #10

#rbind it with the original dataset:
final_permuted_dataset <- rbind(original_selected_AI,final_permuted_dataset) 
table(final_permuted_dataset$permutation_number)

# Join technological fields info to AI patents
final_permuted_dataset <- distinct_at(final_permuted_dataset, vars(appln_id,permutation_number), .keep_all = T)
final_permuted_dataset <- subset(final_permuted_dataset, select = -c(ctry_code) )

final_permuted_dataset <- left_join(final_permuted_dataset, ipc_all_patents_first_period_df, by = "appln_id")

#Main Workflow incorporating permutations
# 1. Compute weighted fields for permuted AI patents
print("Step 1: Calculating field weights...")
region_tech_fields_perm_df <- group_by_applnID_perm(final_permuted_dataset)

# 2. Aggregate at the permutation-country-technology field level
print("Step 2: Aggregating by country and technology field per permutation...")
region_tech_fields_perm_df <- group_by_ctry_and_subclass(region_tech_fields_perm_df)
print("Counts of records per permutation_number after aggregation:")
print(table(region_tech_fields_perm_df$permutation_number))

print("Step 3: Calculating RCA per permutation...")

# Using dplyr::group_by and then purrr::map or a loop
# The result will be a list of dataframes (one RCA df per permutation)
list_of_rca_dfs <- region_tech_fields_perm_df %>%
  group_by(permutation_number) %>%
  group_split() %>% # This splits the df into a list of dfs, one for each permutation
  purrr::map(~{
    current_permutation_number <- unique(.x$permutation_number)
    print(paste("Processing RCA for permutation_number:", current_permutation_number))
    
    # Matrix creation for the current permutation's data
    mat_reg_tech_perm_AI <- .x %>%
      select(-permutation_number) %>% # Temporarily remove for pivot if it causes issues
      arrange(Subclass, ctry_code) %>%
      pivot_wider(names_from = Subclass,
                  values_from = n_tech_reg,
                  values_fill = 0) # Changed from list(n_tech_reg = 0) for simplicity
    
    # Check if ctry_code column exists and is not empty
    if (!"ctry_code" %in% names(mat_reg_tech_perm_AI) || nrow(mat_reg_tech_perm_AI) == 0 || all(is.na(mat_reg_tech_perm_AI$ctry_code))) {
      print(paste("Skipping permutation", current_permutation_number, "due to missing ctry_code or empty data after pivot."))
      return(NULL) # Return NULL or an empty tibble
    }
    
    # Check for duplicate ctry_codes which would prevent rownames_to_column
    if (any(duplicated(mat_reg_tech_perm_AI$ctry_code))) {
      print(paste("Warning: Duplicate ctry_code found for permutation", current_permutation_number, ". Aggregating or handling needed."))
      return(tibble(permutation_number = current_permutation_number, error="duplicate ctry_code"))
    }
    
    
    mat_reg_tech_perm_AI <- mat_reg_tech_perm_AI %>%
      remove_rownames() %>%
      column_to_rownames(var = "ctry_code") %>%
      as.matrix() %>%  round()# No rounding here, location_quotient might prefer raw numbers
    
    # RTA calculation
    # Ensure matrix is suitable (e.g., no NA/NaN/Inf that location_quotient can't handle)
    if (nrow(mat_reg_tech_perm_AI) == 0 || ncol(mat_reg_tech_perm_AI) == 0) {
      print(paste("Skipping RCA for permutation", current_permutation_number, "due to empty matrix."))
      return(NULL)
    }
    
    # Ensure there are at least two columns for location_quotient (ctry_code was one)
    if (ncol(mat_reg_tech_perm_AI) < 1) { # If only ctry_code was present and now it's rownames
      print(paste("Skipping RCA for permutation", current_permutation_number, "due to insufficient columns in matrix."))
      return(NULL)
    }
    
    
    # Check for all zero rows/columns if location_quotient is sensitive
    rca_results_perm <- tryCatch({
      mat_reg_tech_perm_AI %>%
        location_quotient(binary = FALSE) %>% # Assuming this is your function
        as.data.frame() %>%
        rownames_to_column("ctry_code") %>%
        as_tibble() %>%
        gather(key = "Subclass", value = "RCA", -ctry_code) %>%
        arrange(ctry_code, Subclass) %>%
        mutate(permutation_number = current_permutation_number) # Add back permutation number
    }, error = function(e) {
      print(paste("Error in location_quotient for permutation", current_permutation_number, ":", e$message))
      return(tibble(permutation_number = current_permutation_number, ctry_code=NA, Subclass=NA, RCA=NA, error_message = e$message)) # Return an empty or error-marked tibble
    })
    
    return(rca_results_perm)
  })

# Combine the list of RCA dataframes into one final dataframe
final_rca_all_permutations_df <- bind_rows(list_of_rca_dfs)

# Check for errors in the error column
if("error_message" %in% names(final_rca_all_permutations_df)){
  print("Permutations with errors during RCA calculation:")
  print(final_rca_all_permutations_df %>% filter(!is.na(error_message)))
}

write.csv2(final_rca_all_permutations_df, 
           file = "Files_created_with_the_code/data/files_code_4-digits_analysis/robustness/final_rca_all_permutations_df_5y_2nd_Period.csv", 
           row.names = FALSE)

rm(final_rca_all_permutations_df, ipc_all_patents_first_period_df, ai_patents_period_1_df,
   country_all_patents_pool, final_permuted_dataset, list_of_permuted_dfs,list_of_rca_dfs, 
   list_of_replicated_not_selected_ai, not_selected_AI, original_selected_AI, permuted_ai_for_target_countries_iter, 
   randomly_selected_patents_df_country,region_tech_fields_perm_df, temp_df)

##Interval 3 (1984-1988) 
#"1984-1988",

# Define Time Interval 
start_year <- 1983
end_year   <- 1989

###Filter Data for Interval 3 
ipc_all_patents_first_period_df <- ipc_all_patents_part2_df[priority_year > start_year & priority_year < end_year]
ipc_all_patents_first_period_df <- ipc_all_patents_first_period_df[, .(appln_id, ctry_code, Subclass)]

# Calculate AI-Specific RCA for Interval 1
ai_patents_period_1_df <- ai_patents_df[ai_patents_df$priority_year > start_year & ai_patents_df$priority_year < end_year,]

#For MULTIPLE Permutations
list_of_permuted_dfs <- vector("list", length = num_permutations)

for (p in 1:num_permutations) {
  if (p %% 100 == 0) print(paste("Permutation number:", p)) # Progress indicator
  
  # This dataframe will hold the permuted AI patents for target countries ONLY for THIS iteration
  permuted_ai_for_target_countries_iter <- data.frame()
  
  for (country in target_countries) {
    # 1. Identify and Count ACTUAL AI patents for the current country from the original AI dataset
    actual_ai_appln_ids_country <- ai_patents_period_1_df %>%
      filter(ctry_code == country) %>%
      distinct(appln_id) %>%
      pull(appln_id)
    
    n_ai_country <- length(actual_ai_appln_ids_country)
    
    if (n_ai_country == 0) {
      # print(paste("No AI patents found for", country, "in original AI data. Skipping for perm", p))
      next # Skip to the next country if no AI patents to replace
    }
    
    # 2. Prepare the pool of ALL patents for the current country from the general dataset
    country_all_patents_pool <- ipc_all_patents_first_period_df %>%
      filter(ctry_code == country) %>%
      distinct(appln_id)
    
    if (nrow(country_all_patents_pool) == 0) {
      # print(paste("No patents in general pool for", country, ". Skipping for perm", p))
      next
    }
    
    # Handle cases where the pool is smaller than the number of AI patents to sample
    # This is unlikely if ipc_all_patents_first_period_df is complete, but good for robustness
    sample_size <- min(n_ai_country, nrow(country_all_patents_pool))
    replace_sampling <- FALSE
    if (n_ai_country > nrow(country_all_patents_pool)) {
      if(nrow(country_all_patents_pool) < n_ai_country && !replace_sampling){
        next # Skip this country for this permutation if not enough patents
      }
    }
    
    # 3. Randomly select an equivalent number of unique appln_ids from this country's general pool
    random_appln_ids_country <- sample(country_all_patents_pool$appln_id,
                                       size = sample_size, # Use adjusted sample_size
                                       replace = replace_sampling) # Use replace_sampling flag
    
    # 4. Get all rows for these randomly selected patents from the ipc_all_patents_first_period_df
    randomly_selected_patents_df_country <- ipc_all_patents_first_period_df %>%
      filter(appln_id %in% random_appln_ids_country & ctry_code == country)
    
    # 5. Add these randomly selected patents for the current country to the iteration's df
    if (nrow(randomly_selected_patents_df_country) > 0) {
      permuted_ai_for_target_countries_iter <- bind_rows(
        permuted_ai_for_target_countries_iter,
        randomly_selected_patents_df_country
      )
    }
  } # End of country loop
  
  # Add the permutation number to all rows of this iteration's dataframe
  if (nrow(permuted_ai_for_target_countries_iter) > 0) {
    permuted_ai_for_target_countries_iter$permutation_number <- p
  }
  
  # Store the dataframe for this iteration in the list
  list_of_permuted_dfs[[p]] <- permuted_ai_for_target_countries_iter
  
} # End of permutation loop

# Combine all permuted dataframes from the list into one large dataframe
final_permuted_dataset <- bind_rows(list_of_permuted_dfs)

not_selected_AI <- ai_patents_period_1_df[ai_patents_period_1_df$ctry_code %notin% target_countries, c("appln_id", "ctry_code")]

# Create a list to hold the replicated dataframes
list_of_replicated_not_selected_ai <- vector("list", length = num_permutations + 1)

# Loop from 0 to num_permutations
for (i in 0:num_permutations) {
  # Create a copy of the not_selected_AI dataframe for this iteration
  temp_df <- not_selected_AI
  
  # Add the permutation_number column
  temp_df$permutation_number <- i
  
  # Store it in the list (adjust index because list is 1-based, i is 0-based)
  list_of_replicated_not_selected_ai[[i + 1]] <- temp_df
  
  if (i %% 100 == 0) print(paste("Replicating not_selected_AI for permutation_number:", i))
}

# Combine all replicated dataframes from the list into one large dataframe
replicated_not_selected_ai_final <- bind_rows(list_of_replicated_not_selected_ai)
table(replicated_not_selected_ai_final$permutation_number)

original_selected_AI <- ai_patents_period_1_df[ai_patents_period_1_df$ctry_code %in% target_countries, c("appln_id", "ctry_code")]
table(original_selected_AI$ctry_code)
original_selected_AI$permutation_number <- 0

#exclude the Subclass column (I don't need it for now)
final_permuted_dataset <- subset(final_permuted_dataset, select = -c(Subclass) )
#merge both in the "reference" dataset, when permutation = 0
original_selected_AI <- rbind(original_selected_AI, replicated_not_selected_ai_final)
rm(replicated_not_selected_ai_final)
table(original_selected_AI$ctry_code) #the non selected countries appear x times more, because they appear already for all the permutations
table(ai_patents_period_1_df$ctry_code) 
table(original_selected_AI$permutation_number)
length(unique(original_selected_AI$appln_id)) #10
length(unique(ai_patents_period_1_df$appln_id)) #10

#rbind it with the original dataset:
final_permuted_dataset <- rbind(original_selected_AI,final_permuted_dataset) 
table(final_permuted_dataset$permutation_number)

# Join technological fields info to AI patents
final_permuted_dataset <- distinct_at(final_permuted_dataset, vars(appln_id,permutation_number), .keep_all = T)
final_permuted_dataset <- subset(final_permuted_dataset, select = -c(ctry_code) )

final_permuted_dataset <- left_join(final_permuted_dataset, ipc_all_patents_first_period_df, by = "appln_id")

#Main Workflow incorporating permutations
# 1. Compute weighted fields for permuted AI patents
print("Step 1: Calculating field weights...")
region_tech_fields_perm_df <- group_by_applnID_perm(final_permuted_dataset)

# 2. Aggregate at the permutation-country-technology field level
print("Step 2: Aggregating by country and technology field per permutation...")
region_tech_fields_perm_df <- group_by_ctry_and_subclass(region_tech_fields_perm_df)
print("Counts of records per permutation_number after aggregation:")
print(table(region_tech_fields_perm_df$permutation_number))

print("Step 3: Calculating RCA per permutation...")

# Using dplyr::group_by and then purrr::map or a loop
# The result will be a list of dataframes (one RCA df per permutation)
list_of_rca_dfs <- region_tech_fields_perm_df %>%
  group_by(permutation_number) %>%
  group_split() %>% # This splits the df into a list of dfs, one for each permutation
  purrr::map(~{
    current_permutation_number <- unique(.x$permutation_number)
    print(paste("Processing RCA for permutation_number:", current_permutation_number))
    
    # Matrix creation for the current permutation's data
    mat_reg_tech_perm_AI <- .x %>%
      select(-permutation_number) %>% # Temporarily remove for pivot if it causes issues
      arrange(Subclass, ctry_code) %>%
      pivot_wider(names_from = Subclass,
                  values_from = n_tech_reg,
                  values_fill = 0) # Changed from list(n_tech_reg = 0) for simplicity
    
    # Check if ctry_code column exists and is not empty
    if (!"ctry_code" %in% names(mat_reg_tech_perm_AI) || nrow(mat_reg_tech_perm_AI) == 0 || all(is.na(mat_reg_tech_perm_AI$ctry_code))) {
      print(paste("Skipping permutation", current_permutation_number, "due to missing ctry_code or empty data after pivot."))
      return(NULL) # Return NULL or an empty tibble
    }
    
    # Check for duplicate ctry_codes which would prevent rownames_to_column
    if (any(duplicated(mat_reg_tech_perm_AI$ctry_code))) {
      print(paste("Warning: Duplicate ctry_code found for permutation", current_permutation_number, ". Aggregating or handling needed."))
      return(tibble(permutation_number = current_permutation_number, error="duplicate ctry_code"))
    }
    
    
    mat_reg_tech_perm_AI <- mat_reg_tech_perm_AI %>%
      remove_rownames() %>%
      column_to_rownames(var = "ctry_code") %>%
      as.matrix() %>%  round()# No rounding here, location_quotient might prefer raw numbers
    
    # RTA calculation
    # Ensure matrix is suitable (e.g., no NA/NaN/Inf that location_quotient can't handle)
    if (nrow(mat_reg_tech_perm_AI) == 0 || ncol(mat_reg_tech_perm_AI) == 0) {
      print(paste("Skipping RCA for permutation", current_permutation_number, "due to empty matrix."))
      return(NULL)
    }
    
    # Ensure there are at least two columns for location_quotient (ctry_code was one)
    if (ncol(mat_reg_tech_perm_AI) < 1) { # If only ctry_code was present and now it's rownames
      print(paste("Skipping RCA for permutation", current_permutation_number, "due to insufficient columns in matrix."))
      return(NULL)
    }
    
    
    # Check for all zero rows/columns if location_quotient is sensitive
    rca_results_perm <- tryCatch({
      mat_reg_tech_perm_AI %>%
        location_quotient(binary = FALSE) %>% # Assuming this is your function
        as.data.frame() %>%
        rownames_to_column("ctry_code") %>%
        as_tibble() %>%
        gather(key = "Subclass", value = "RCA", -ctry_code) %>%
        arrange(ctry_code, Subclass) %>%
        mutate(permutation_number = current_permutation_number) # Add back permutation number
    }, error = function(e) {
      print(paste("Error in location_quotient for permutation", current_permutation_number, ":", e$message))
      return(tibble(permutation_number = current_permutation_number, ctry_code=NA, Subclass=NA, RCA=NA, error_message = e$message)) # Return an empty or error-marked tibble
    })
    
    return(rca_results_perm)
  })

# Combine the list of RCA dataframes into one final dataframe
final_rca_all_permutations_df <- bind_rows(list_of_rca_dfs)

# Check for errors in the error column
if("error_message" %in% names(final_rca_all_permutations_df)){
  print("Permutations with errors during RCA calculation:")
  print(final_rca_all_permutations_df %>% filter(!is.na(error_message)))
}

write.csv2(final_rca_all_permutations_df, 
           file = "Files_created_with_the_code/data/files_code_4-digits_analysis/robustness/final_rca_all_permutations_df_5y_3rd_Period.csv", 
           row.names = FALSE)

rm(final_rca_all_permutations_df, ipc_all_patents_first_period_df, ai_patents_period_1_df,
   country_all_patents_pool, final_permuted_dataset, list_of_permuted_dfs,list_of_rca_dfs, 
   list_of_replicated_not_selected_ai, not_selected_AI, original_selected_AI, permuted_ai_for_target_countries_iter, 
   randomly_selected_patents_df_country,region_tech_fields_perm_df, temp_df)

##Interval 4 (1989-1993)
# Define Time Interval 
start_year <- 1988
end_year   <- 1994

###Filter Data for Interval 4 
ipc_all_patents_first_period_df <- ipc_all_patents_part2_df[priority_year > start_year & priority_year < end_year]
ipc_all_patents_first_period_df <- ipc_all_patents_first_period_df[, .(appln_id, ctry_code, Subclass)]

# Calculate AI-Specific RCA for Interval 1
ai_patents_period_1_df <- ai_patents_df[ai_patents_df$priority_year > start_year & ai_patents_df$priority_year < end_year,]

#For MULTIPLE Permutations
list_of_permuted_dfs <- vector("list", length = num_permutations)

for (p in 1:num_permutations) {
  if (p %% 100 == 0) print(paste("Permutation number:", p)) # Progress indicator
  
  # This dataframe will hold the permuted AI patents for target countries ONLY for THIS iteration
  permuted_ai_for_target_countries_iter <- data.frame()
  
  for (country in target_countries) {
    # 1. Identify and Count ACTUAL AI patents for the current country from the original AI dataset
    actual_ai_appln_ids_country <- ai_patents_period_1_df %>%
      filter(ctry_code == country) %>%
      distinct(appln_id) %>%
      pull(appln_id)
    
    n_ai_country <- length(actual_ai_appln_ids_country)
    
    if (n_ai_country == 0) {
      # print(paste("No AI patents found for", country, "in original AI data. Skipping for perm", p))
      next # Skip to the next country if no AI patents to replace
    }
    
    # 2. Prepare the pool of ALL patents for the current country from the general dataset
    country_all_patents_pool <- ipc_all_patents_first_period_df %>%
      filter(ctry_code == country) %>%
      distinct(appln_id)
    
    if (nrow(country_all_patents_pool) == 0) {
      # print(paste("No patents in general pool for", country, ". Skipping for perm", p))
      next
    }
    
    # Handle cases where the pool is smaller than the number of AI patents to sample
    # This is unlikely if ipc_all_patents_first_period_df is complete, but good for robustness
    sample_size <- min(n_ai_country, nrow(country_all_patents_pool))
    replace_sampling <- FALSE
    if (n_ai_country > nrow(country_all_patents_pool)) {
      if(nrow(country_all_patents_pool) < n_ai_country && !replace_sampling){
        next # Skip this country for this permutation if not enough patents
      }
    }
    
    # 3. Randomly select an equivalent number of unique appln_ids from this country's general pool
    random_appln_ids_country <- sample(country_all_patents_pool$appln_id,
                                       size = sample_size, # Use adjusted sample_size
                                       replace = replace_sampling) # Use replace_sampling flag
    
    # 4. Get all rows for these randomly selected patents from the ipc_all_patents_first_period_df
    randomly_selected_patents_df_country <- ipc_all_patents_first_period_df %>%
      filter(appln_id %in% random_appln_ids_country & ctry_code == country)
    
    # 5. Add these randomly selected patents for the current country to the iteration's df
    if (nrow(randomly_selected_patents_df_country) > 0) {
      permuted_ai_for_target_countries_iter <- bind_rows(
        permuted_ai_for_target_countries_iter,
        randomly_selected_patents_df_country
      )
    }
  } # End of country loop
  
  # Add the permutation number to all rows of this iteration's dataframe
  if (nrow(permuted_ai_for_target_countries_iter) > 0) {
    permuted_ai_for_target_countries_iter$permutation_number <- p
  }
  
  # Store the dataframe for this iteration in the list
  list_of_permuted_dfs[[p]] <- permuted_ai_for_target_countries_iter
  
} # End of permutation loop

# Combine all permuted dataframes from the list into one large dataframe
final_permuted_dataset <- bind_rows(list_of_permuted_dfs)

not_selected_AI <- ai_patents_period_1_df[ai_patents_period_1_df$ctry_code %notin% target_countries, c("appln_id", "ctry_code")]

# Create a list to hold the replicated dataframes
list_of_replicated_not_selected_ai <- vector("list", length = num_permutations + 1)

# Loop from 0 to num_permutations
for (i in 0:num_permutations) {
  # Create a copy of the not_selected_AI dataframe for this iteration
  temp_df <- not_selected_AI
  
  # Add the permutation_number column
  temp_df$permutation_number <- i
  
  # Store it in the list (adjust index because list is 1-based, i is 0-based)
  list_of_replicated_not_selected_ai[[i + 1]] <- temp_df
  
  if (i %% 100 == 0) print(paste("Replicating not_selected_AI for permutation_number:", i))
}

# Combine all replicated dataframes from the list into one large dataframe
replicated_not_selected_ai_final <- bind_rows(list_of_replicated_not_selected_ai)
table(replicated_not_selected_ai_final$permutation_number)

original_selected_AI <- ai_patents_period_1_df[ai_patents_period_1_df$ctry_code %in% target_countries, c("appln_id", "ctry_code")]
table(original_selected_AI$ctry_code)
original_selected_AI$permutation_number <- 0

#exclude the Subclass column (I don't need it for now)
final_permuted_dataset <- subset(final_permuted_dataset, select = -c(Subclass) )
#merge both in the "reference" dataset, when permutation = 0
original_selected_AI <- rbind(original_selected_AI, replicated_not_selected_ai_final)
rm(replicated_not_selected_ai_final)
table(original_selected_AI$ctry_code) #the non selected countries appear x times more, because they appear already for all the permutations
table(ai_patents_period_1_df$ctry_code) 
table(original_selected_AI$permutation_number)
length(unique(original_selected_AI$appln_id)) #10
length(unique(ai_patents_period_1_df$appln_id)) #10

#rbind it with the original dataset:
final_permuted_dataset <- rbind(original_selected_AI,final_permuted_dataset) 
table(final_permuted_dataset$permutation_number)

# Join technological fields info to AI patents
final_permuted_dataset <- distinct_at(final_permuted_dataset, vars(appln_id,permutation_number), .keep_all = T)
final_permuted_dataset <- subset(final_permuted_dataset, select = -c(ctry_code) )

final_permuted_dataset <- left_join(final_permuted_dataset, ipc_all_patents_first_period_df, by = "appln_id")

#Main Workflow incorporating permutations
# 1. Compute weighted fields for permuted AI patents
print("Step 1: Calculating field weights...")
region_tech_fields_perm_df <- group_by_applnID_perm(final_permuted_dataset)

# 2. Aggregate at the permutation-country-technology field level
print("Step 2: Aggregating by country and technology field per permutation...")
region_tech_fields_perm_df <- group_by_ctry_and_subclass(region_tech_fields_perm_df)
print("Counts of records per permutation_number after aggregation:")
print(table(region_tech_fields_perm_df$permutation_number))

print("Step 3: Calculating RCA per permutation...")

# Using dplyr::group_by and then purrr::map or a loop
# The result will be a list of dataframes (one RCA df per permutation)
list_of_rca_dfs <- region_tech_fields_perm_df %>%
  group_by(permutation_number) %>%
  group_split() %>% # This splits the df into a list of dfs, one for each permutation
  purrr::map(~{
    current_permutation_number <- unique(.x$permutation_number)
    print(paste("Processing RCA for permutation_number:", current_permutation_number))
    
    # Matrix creation for the current permutation's data
    mat_reg_tech_perm_AI <- .x %>%
      select(-permutation_number) %>% # Temporarily remove for pivot if it causes issues
      arrange(Subclass, ctry_code) %>%
      pivot_wider(names_from = Subclass,
                  values_from = n_tech_reg,
                  values_fill = 0) # Changed from list(n_tech_reg = 0) for simplicity
    
    # Check if ctry_code column exists and is not empty
    if (!"ctry_code" %in% names(mat_reg_tech_perm_AI) || nrow(mat_reg_tech_perm_AI) == 0 || all(is.na(mat_reg_tech_perm_AI$ctry_code))) {
      print(paste("Skipping permutation", current_permutation_number, "due to missing ctry_code or empty data after pivot."))
      return(NULL) # Return NULL or an empty tibble
    }
    
    # Check for duplicate ctry_codes which would prevent rownames_to_column
    if (any(duplicated(mat_reg_tech_perm_AI$ctry_code))) {
      print(paste("Warning: Duplicate ctry_code found for permutation", current_permutation_number, ". Aggregating or handling needed."))
      return(tibble(permutation_number = current_permutation_number, error="duplicate ctry_code"))
    }
    
    
    mat_reg_tech_perm_AI <- mat_reg_tech_perm_AI %>%
      remove_rownames() %>%
      column_to_rownames(var = "ctry_code") %>%
      as.matrix() %>%  round()# No rounding here, location_quotient might prefer raw numbers
    
    # RTA calculation
    # Ensure matrix is suitable (e.g., no NA/NaN/Inf that location_quotient can't handle)
    if (nrow(mat_reg_tech_perm_AI) == 0 || ncol(mat_reg_tech_perm_AI) == 0) {
      print(paste("Skipping RCA for permutation", current_permutation_number, "due to empty matrix."))
      return(NULL)
    }
    
    # Ensure there are at least two columns for location_quotient (ctry_code was one)
    if (ncol(mat_reg_tech_perm_AI) < 1) { # If only ctry_code was present and now it's rownames
      print(paste("Skipping RCA for permutation", current_permutation_number, "due to insufficient columns in matrix."))
      return(NULL)
    }
    
    
    # Check for all zero rows/columns if location_quotient is sensitive
    rca_results_perm <- tryCatch({
      mat_reg_tech_perm_AI %>%
        location_quotient(binary = FALSE) %>% # Assuming this is your function
        as.data.frame() %>%
        rownames_to_column("ctry_code") %>%
        as_tibble() %>%
        gather(key = "Subclass", value = "RCA", -ctry_code) %>%
        arrange(ctry_code, Subclass) %>%
        mutate(permutation_number = current_permutation_number) # Add back permutation number
    }, error = function(e) {
      print(paste("Error in location_quotient for permutation", current_permutation_number, ":", e$message))
      return(tibble(permutation_number = current_permutation_number, ctry_code=NA, Subclass=NA, RCA=NA, error_message = e$message)) # Return an empty or error-marked tibble
    })
    
    return(rca_results_perm)
  })

# Combine the list of RCA dataframes into one final dataframe
final_rca_all_permutations_df <- bind_rows(list_of_rca_dfs)

# Check for errors in the error column
if("error_message" %in% names(final_rca_all_permutations_df)){
  print("Permutations with errors during RCA calculation:")
  print(final_rca_all_permutations_df %>% filter(!is.na(error_message)))
}

write.csv2(final_rca_all_permutations_df, 
           file = "Files_created_with_the_code/data/files_code_4-digits_analysis/robustness/final_rca_all_permutations_df_5y_4th_Period.csv", 
           row.names = FALSE)

rm(final_rca_all_permutations_df, ipc_all_patents_first_period_df, ai_patents_period_1_df,
   country_all_patents_pool, final_permuted_dataset, list_of_permuted_dfs,list_of_rca_dfs, 
   list_of_replicated_not_selected_ai, not_selected_AI, original_selected_AI, permuted_ai_for_target_countries_iter, 
   randomly_selected_patents_df_country,region_tech_fields_perm_df, temp_df)

##Interval 5 (1994-1998)
#"1994-1998"

# Define Time Interval 
start_year <- 1993
end_year   <- 1999

###Filter Data for Interval 5 
ipc_all_patents_first_period_df <- ipc_all_patents_part2_df[priority_year > start_year & priority_year < end_year]
ipc_all_patents_first_period_df <- ipc_all_patents_first_period_df[, .(appln_id, ctry_code, Subclass)]

# Calculate AI-Specific RCA for Interval 1
ai_patents_period_1_df <- ai_patents_df[ai_patents_df$priority_year > start_year & ai_patents_df$priority_year < end_year,]

#For MULTIPLE Permutations
list_of_permuted_dfs <- vector("list", length = num_permutations)

for (p in 1:num_permutations) {
  if (p %% 100 == 0) print(paste("Permutation number:", p)) # Progress indicator
  
  # This dataframe will hold the permuted AI patents for target countries ONLY for THIS iteration
  permuted_ai_for_target_countries_iter <- data.frame()
  
  for (country in target_countries) {
    # 1. Identify and Count ACTUAL AI patents for the current country from the original AI dataset
    actual_ai_appln_ids_country <- ai_patents_period_1_df %>%
      filter(ctry_code == country) %>%
      distinct(appln_id) %>%
      pull(appln_id)
    
    n_ai_country <- length(actual_ai_appln_ids_country)
    
    if (n_ai_country == 0) {
      # print(paste("No AI patents found for", country, "in original AI data. Skipping for perm", p))
      next # Skip to the next country if no AI patents to replace
    }
    
    # 2. Prepare the pool of ALL patents for the current country from the general dataset
    country_all_patents_pool <- ipc_all_patents_first_period_df %>%
      filter(ctry_code == country) %>%
      distinct(appln_id)
    
    if (nrow(country_all_patents_pool) == 0) {
      # print(paste("No patents in general pool for", country, ". Skipping for perm", p))
      next
    }
    
    # Handle cases where the pool is smaller than the number of AI patents to sample
    # This is unlikely if ipc_all_patents_first_period_df is complete, but good for robustness
    sample_size <- min(n_ai_country, nrow(country_all_patents_pool))
    replace_sampling <- FALSE
    if (n_ai_country > nrow(country_all_patents_pool)) {
      if(nrow(country_all_patents_pool) < n_ai_country && !replace_sampling){
        next # Skip this country for this permutation if not enough patents
      }
    }
    
    # 3. Randomly select an equivalent number of unique appln_ids from this country's general pool
    random_appln_ids_country <- sample(country_all_patents_pool$appln_id,
                                       size = sample_size, # Use adjusted sample_size
                                       replace = replace_sampling) # Use replace_sampling flag
    
    # 4. Get all rows for these randomly selected patents from the ipc_all_patents_first_period_df
    randomly_selected_patents_df_country <- ipc_all_patents_first_period_df %>%
      filter(appln_id %in% random_appln_ids_country & ctry_code == country)
    
    # 5. Add these randomly selected patents for the current country to the iteration's df
    if (nrow(randomly_selected_patents_df_country) > 0) {
      permuted_ai_for_target_countries_iter <- bind_rows(
        permuted_ai_for_target_countries_iter,
        randomly_selected_patents_df_country
      )
    }
  } # End of country loop
  
  # Add the permutation number to all rows of this iteration's dataframe
  if (nrow(permuted_ai_for_target_countries_iter) > 0) {
    permuted_ai_for_target_countries_iter$permutation_number <- p
  }
  
  # Store the dataframe for this iteration in the list
  list_of_permuted_dfs[[p]] <- permuted_ai_for_target_countries_iter
  
} # End of permutation loop

# Combine all permuted dataframes from the list into one large dataframe
final_permuted_dataset <- bind_rows(list_of_permuted_dfs)

not_selected_AI <- ai_patents_period_1_df[ai_patents_period_1_df$ctry_code %notin% target_countries, c("appln_id", "ctry_code")]

# Create a list to hold the replicated dataframes
list_of_replicated_not_selected_ai <- vector("list", length = num_permutations + 1)

# Loop from 0 to num_permutations
for (i in 0:num_permutations) {
  # Create a copy of the not_selected_AI dataframe for this iteration
  temp_df <- not_selected_AI
  
  # Add the permutation_number column
  temp_df$permutation_number <- i
  
  # Store it in the list (adjust index because list is 1-based, i is 0-based)
  list_of_replicated_not_selected_ai[[i + 1]] <- temp_df
  
  if (i %% 100 == 0) print(paste("Replicating not_selected_AI for permutation_number:", i))
}

# Combine all replicated dataframes from the list into one large dataframe
replicated_not_selected_ai_final <- bind_rows(list_of_replicated_not_selected_ai)
table(replicated_not_selected_ai_final$permutation_number)

original_selected_AI <- ai_patents_period_1_df[ai_patents_period_1_df$ctry_code %in% target_countries, c("appln_id", "ctry_code")]
table(original_selected_AI$ctry_code)
original_selected_AI$permutation_number <- 0

#exclude the Subclass column (I don't need it for now)
final_permuted_dataset <- subset(final_permuted_dataset, select = -c(Subclass) )
#merge both in the "reference" dataset, when permutation = 0
original_selected_AI <- rbind(original_selected_AI, replicated_not_selected_ai_final)
rm(replicated_not_selected_ai_final)
table(original_selected_AI$ctry_code) #the non selected countries appear x times more, because they appear already for all the permutations
table(ai_patents_period_1_df$ctry_code) 
table(original_selected_AI$permutation_number)
length(unique(original_selected_AI$appln_id)) #10
length(unique(ai_patents_period_1_df$appln_id)) #10

#rbind it with the original dataset:
final_permuted_dataset <- rbind(original_selected_AI,final_permuted_dataset) 
table(final_permuted_dataset$permutation_number)

# Join technological fields info to AI patents
final_permuted_dataset <- distinct_at(final_permuted_dataset, vars(appln_id,permutation_number), .keep_all = T)
final_permuted_dataset <- subset(final_permuted_dataset, select = -c(ctry_code) )

final_permuted_dataset <- left_join(final_permuted_dataset, ipc_all_patents_first_period_df, by = "appln_id")

#Main Workflow incorporating permutations
# 1. Compute weighted fields for permuted AI patents
print("Step 1: Calculating field weights...")
region_tech_fields_perm_df <- group_by_applnID_perm(final_permuted_dataset)

# 2. Aggregate at the permutation-country-technology field level
print("Step 2: Aggregating by country and technology field per permutation...")
region_tech_fields_perm_df <- group_by_ctry_and_subclass(region_tech_fields_perm_df)
print("Counts of records per permutation_number after aggregation:")
print(table(region_tech_fields_perm_df$permutation_number))

print("Step 3: Calculating RCA per permutation...")

# Using dplyr::group_by and then purrr::map or a loop
# The result will be a list of dataframes (one RCA df per permutation)
list_of_rca_dfs <- region_tech_fields_perm_df %>%
  group_by(permutation_number) %>%
  group_split() %>% # This splits the df into a list of dfs, one for each permutation
  purrr::map(~{
    current_permutation_number <- unique(.x$permutation_number)
    print(paste("Processing RCA for permutation_number:", current_permutation_number))
    
    # Matrix creation for the current permutation's data
    mat_reg_tech_perm_AI <- .x %>%
      select(-permutation_number) %>% # Temporarily remove for pivot if it causes issues
      arrange(Subclass, ctry_code) %>%
      pivot_wider(names_from = Subclass,
                  values_from = n_tech_reg,
                  values_fill = 0) # Changed from list(n_tech_reg = 0) for simplicity
    
    # Check if ctry_code column exists and is not empty
    if (!"ctry_code" %in% names(mat_reg_tech_perm_AI) || nrow(mat_reg_tech_perm_AI) == 0 || all(is.na(mat_reg_tech_perm_AI$ctry_code))) {
      print(paste("Skipping permutation", current_permutation_number, "due to missing ctry_code or empty data after pivot."))
      return(NULL) # Return NULL or an empty tibble
    }
    
    # Check for duplicate ctry_codes which would prevent rownames_to_column
    if (any(duplicated(mat_reg_tech_perm_AI$ctry_code))) {
      print(paste("Warning: Duplicate ctry_code found for permutation", current_permutation_number, ". Aggregating or handling needed."))
      return(tibble(permutation_number = current_permutation_number, error="duplicate ctry_code"))
    }
    
    
    mat_reg_tech_perm_AI <- mat_reg_tech_perm_AI %>%
      remove_rownames() %>%
      column_to_rownames(var = "ctry_code") %>%
      as.matrix() %>%  round()# No rounding here, location_quotient might prefer raw numbers
    
    # RTA calculation
    # Ensure matrix is suitable (e.g., no NA/NaN/Inf that location_quotient can't handle)
    if (nrow(mat_reg_tech_perm_AI) == 0 || ncol(mat_reg_tech_perm_AI) == 0) {
      print(paste("Skipping RCA for permutation", current_permutation_number, "due to empty matrix."))
      return(NULL)
    }
    
    # Ensure there are at least two columns for location_quotient (ctry_code was one)
    if (ncol(mat_reg_tech_perm_AI) < 1) { # If only ctry_code was present and now it's rownames
      print(paste("Skipping RCA for permutation", current_permutation_number, "due to insufficient columns in matrix."))
      return(NULL)
    }
    
    
    # Check for all zero rows/columns if location_quotient is sensitive
    rca_results_perm <- tryCatch({
      mat_reg_tech_perm_AI %>%
        location_quotient(binary = FALSE) %>% # Assuming this is your function
        as.data.frame() %>%
        rownames_to_column("ctry_code") %>%
        as_tibble() %>%
        gather(key = "Subclass", value = "RCA", -ctry_code) %>%
        arrange(ctry_code, Subclass) %>%
        mutate(permutation_number = current_permutation_number) # Add back permutation number
    }, error = function(e) {
      print(paste("Error in location_quotient for permutation", current_permutation_number, ":", e$message))
      return(tibble(permutation_number = current_permutation_number, ctry_code=NA, Subclass=NA, RCA=NA, error_message = e$message)) # Return an empty or error-marked tibble
    })
    
    return(rca_results_perm)
  })

# Combine the list of RCA dataframes into one final dataframe
final_rca_all_permutations_df <- bind_rows(list_of_rca_dfs)

# Check for errors in the error column
if("error_message" %in% names(final_rca_all_permutations_df)){
  print("Permutations with errors during RCA calculation:")
  print(final_rca_all_permutations_df %>% filter(!is.na(error_message)))
}

write.csv2(final_rca_all_permutations_df, 
           file = "Files_created_with_the_code/data/files_code_4-digits_analysis/robustness/final_rca_all_permutations_df_5y_5th_Period.csv", 
           row.names = FALSE)

rm(final_rca_all_permutations_df, ipc_all_patents_first_period_df, ai_patents_period_1_df,
   country_all_patents_pool, final_permuted_dataset, list_of_permuted_dfs,list_of_rca_dfs, 
   list_of_replicated_not_selected_ai, not_selected_AI, original_selected_AI, permuted_ai_for_target_countries_iter, 
   randomly_selected_patents_df_country,region_tech_fields_perm_df, temp_df)

##Interval 6 (1999-2003) 
# Define Time Interval 
start_year <- 1998
end_year   <- 2004

###Filter Data for Interval 6
ipc_all_patents_first_period_df <- ipc_all_patents_part2_df[priority_year > start_year & priority_year < end_year]
ipc_all_patents_first_period_df <- ipc_all_patents_first_period_df[, .(appln_id, ctry_code, Subclass)]

# Calculate AI-Specific RCA for Interval 1
ai_patents_period_1_df <- ai_patents_df[ai_patents_df$priority_year > start_year & ai_patents_df$priority_year < end_year,]

#For MULTIPLE Permutations
list_of_permuted_dfs <- vector("list", length = num_permutations)

for (p in 1:num_permutations) {
  if (p %% 100 == 0) print(paste("Permutation number:", p)) # Progress indicator
  
  # This dataframe will hold the permuted AI patents for target countries ONLY for THIS iteration
  permuted_ai_for_target_countries_iter <- data.frame()
  
  for (country in target_countries) {
    # 1. Identify and Count ACTUAL AI patents for the current country from the original AI dataset
    actual_ai_appln_ids_country <- ai_patents_period_1_df %>%
      filter(ctry_code == country) %>%
      distinct(appln_id) %>%
      pull(appln_id)
    
    n_ai_country <- length(actual_ai_appln_ids_country)
    
    if (n_ai_country == 0) {
      # print(paste("No AI patents found for", country, "in original AI data. Skipping for perm", p))
      next # Skip to the next country if no AI patents to replace
    }
    
    # 2. Prepare the pool of ALL patents for the current country from the general dataset
    country_all_patents_pool <- ipc_all_patents_first_period_df %>%
      filter(ctry_code == country) %>%
      distinct(appln_id)
    
    if (nrow(country_all_patents_pool) == 0) {
      # print(paste("No patents in general pool for", country, ". Skipping for perm", p))
      next
    }
    
    # Handle cases where the pool is smaller than the number of AI patents to sample
    # This is unlikely if ipc_all_patents_first_period_df is complete, but good for robustness
    sample_size <- min(n_ai_country, nrow(country_all_patents_pool))
    replace_sampling <- FALSE
    if (n_ai_country > nrow(country_all_patents_pool)) {
      if(nrow(country_all_patents_pool) < n_ai_country && !replace_sampling){
        next # Skip this country for this permutation if not enough patents
      }
    }
    
    # 3. Randomly select an equivalent number of unique appln_ids from this country's general pool
    random_appln_ids_country <- sample(country_all_patents_pool$appln_id,
                                       size = sample_size, # Use adjusted sample_size
                                       replace = replace_sampling) # Use replace_sampling flag
    
    # 4. Get all rows for these randomly selected patents from the ipc_all_patents_first_period_df
    randomly_selected_patents_df_country <- ipc_all_patents_first_period_df %>%
      filter(appln_id %in% random_appln_ids_country & ctry_code == country)
    
    # 5. Add these randomly selected patents for the current country to the iteration's df
    if (nrow(randomly_selected_patents_df_country) > 0) {
      permuted_ai_for_target_countries_iter <- bind_rows(
        permuted_ai_for_target_countries_iter,
        randomly_selected_patents_df_country
      )
    }
  } # End of country loop
  
  # Add the permutation number to all rows of this iteration's dataframe
  if (nrow(permuted_ai_for_target_countries_iter) > 0) {
    permuted_ai_for_target_countries_iter$permutation_number <- p
  }
  
  # Store the dataframe for this iteration in the list
  list_of_permuted_dfs[[p]] <- permuted_ai_for_target_countries_iter
  
} # End of permutation loop

# Combine all permuted dataframes from the list into one large dataframe
final_permuted_dataset <- bind_rows(list_of_permuted_dfs)

not_selected_AI <- ai_patents_period_1_df[ai_patents_period_1_df$ctry_code %notin% target_countries, c("appln_id", "ctry_code")]

# Create a list to hold the replicated dataframes
list_of_replicated_not_selected_ai <- vector("list", length = num_permutations + 1)

# Loop from 0 to num_permutations
for (i in 0:num_permutations) {
  # Create a copy of the not_selected_AI dataframe for this iteration
  temp_df <- not_selected_AI
  
  # Add the permutation_number column
  temp_df$permutation_number <- i
  
  # Store it in the list (adjust index because list is 1-based, i is 0-based)
  list_of_replicated_not_selected_ai[[i + 1]] <- temp_df
  
  if (i %% 100 == 0) print(paste("Replicating not_selected_AI for permutation_number:", i))
}

# Combine all replicated dataframes from the list into one large dataframe
replicated_not_selected_ai_final <- bind_rows(list_of_replicated_not_selected_ai)
table(replicated_not_selected_ai_final$permutation_number)

original_selected_AI <- ai_patents_period_1_df[ai_patents_period_1_df$ctry_code %in% target_countries, c("appln_id", "ctry_code")]
table(original_selected_AI$ctry_code)
original_selected_AI$permutation_number <- 0

#exclude the Subclass column (I don't need it for now)
final_permuted_dataset <- subset(final_permuted_dataset, select = -c(Subclass) )
#merge both in the "reference" dataset, when permutation = 0
original_selected_AI <- rbind(original_selected_AI, replicated_not_selected_ai_final)
rm(replicated_not_selected_ai_final)
table(original_selected_AI$ctry_code) #the non selected countries appear x times more, because they appear already for all the permutations
table(ai_patents_period_1_df$ctry_code) 
table(original_selected_AI$permutation_number)
length(unique(original_selected_AI$appln_id)) #10
length(unique(ai_patents_period_1_df$appln_id)) #10

#rbind it with the original dataset:
final_permuted_dataset <- rbind(original_selected_AI,final_permuted_dataset) 
table(final_permuted_dataset$permutation_number)

# Join technological fields info to AI patents
final_permuted_dataset <- distinct_at(final_permuted_dataset, vars(appln_id,permutation_number), .keep_all = T)
final_permuted_dataset <- subset(final_permuted_dataset, select = -c(ctry_code) )

final_permuted_dataset <- left_join(final_permuted_dataset, ipc_all_patents_first_period_df, by = "appln_id")

#Main Workflow incorporating permutations
# 1. Compute weighted fields for permuted AI patents
print("Step 1: Calculating field weights...")
region_tech_fields_perm_df <- group_by_applnID_perm(final_permuted_dataset)

# 2. Aggregate at the permutation-country-technology field level
print("Step 2: Aggregating by country and technology field per permutation...")
region_tech_fields_perm_df <- group_by_ctry_and_subclass(region_tech_fields_perm_df)
print("Counts of records per permutation_number after aggregation:")
print(table(region_tech_fields_perm_df$permutation_number))

print("Step 3: Calculating RCA per permutation...")

# Using dplyr::group_by and then purrr::map or a loop
# The result will be a list of dataframes (one RCA df per permutation)
list_of_rca_dfs <- region_tech_fields_perm_df %>%
  group_by(permutation_number) %>%
  group_split() %>% # This splits the df into a list of dfs, one for each permutation
  purrr::map(~{
    current_permutation_number <- unique(.x$permutation_number)
    print(paste("Processing RCA for permutation_number:", current_permutation_number))
    
    # Matrix creation for the current permutation's data
    mat_reg_tech_perm_AI <- .x %>%
      select(-permutation_number) %>% # Temporarily remove for pivot if it causes issues
      arrange(Subclass, ctry_code) %>%
      pivot_wider(names_from = Subclass,
                  values_from = n_tech_reg,
                  values_fill = 0) # Changed from list(n_tech_reg = 0) for simplicity
    
    # Check if ctry_code column exists and is not empty
    if (!"ctry_code" %in% names(mat_reg_tech_perm_AI) || nrow(mat_reg_tech_perm_AI) == 0 || all(is.na(mat_reg_tech_perm_AI$ctry_code))) {
      print(paste("Skipping permutation", current_permutation_number, "due to missing ctry_code or empty data after pivot."))
      return(NULL) # Return NULL or an empty tibble
    }
    
    # Check for duplicate ctry_codes which would prevent rownames_to_column
    if (any(duplicated(mat_reg_tech_perm_AI$ctry_code))) {
      print(paste("Warning: Duplicate ctry_code found for permutation", current_permutation_number, ". Aggregating or handling needed."))
      return(tibble(permutation_number = current_permutation_number, error="duplicate ctry_code"))
    }
    
    
    mat_reg_tech_perm_AI <- mat_reg_tech_perm_AI %>%
      remove_rownames() %>%
      column_to_rownames(var = "ctry_code") %>%
      as.matrix() %>%  round()# No rounding here, location_quotient might prefer raw numbers
    
    # RTA calculation
    # Ensure matrix is suitable (e.g., no NA/NaN/Inf that location_quotient can't handle)
    if (nrow(mat_reg_tech_perm_AI) == 0 || ncol(mat_reg_tech_perm_AI) == 0) {
      print(paste("Skipping RCA for permutation", current_permutation_number, "due to empty matrix."))
      return(NULL)
    }
    
    # Ensure there are at least two columns for location_quotient (ctry_code was one)
    if (ncol(mat_reg_tech_perm_AI) < 1) { # If only ctry_code was present and now it's rownames
      print(paste("Skipping RCA for permutation", current_permutation_number, "due to insufficient columns in matrix."))
      return(NULL)
    }
    
    
    # Check for all zero rows/columns if location_quotient is sensitive
    rca_results_perm <- tryCatch({
      mat_reg_tech_perm_AI %>%
        location_quotient(binary = FALSE) %>% # Assuming this is your function
        as.data.frame() %>%
        rownames_to_column("ctry_code") %>%
        as_tibble() %>%
        gather(key = "Subclass", value = "RCA", -ctry_code) %>%
        arrange(ctry_code, Subclass) %>%
        mutate(permutation_number = current_permutation_number) # Add back permutation number
    }, error = function(e) {
      print(paste("Error in location_quotient for permutation", current_permutation_number, ":", e$message))
      return(tibble(permutation_number = current_permutation_number, ctry_code=NA, Subclass=NA, RCA=NA, error_message = e$message)) # Return an empty or error-marked tibble
    })
    
    return(rca_results_perm)
  })

# Combine the list of RCA dataframes into one final dataframe
final_rca_all_permutations_df <- bind_rows(list_of_rca_dfs)

# Check for errors in the error column
if("error_message" %in% names(final_rca_all_permutations_df)){
  print("Permutations with errors during RCA calculation:")
  print(final_rca_all_permutations_df %>% filter(!is.na(error_message)))
}

write.csv2(final_rca_all_permutations_df, 
           file = "Files_created_with_the_code/data/files_code_4-digits_analysis/robustness/final_rca_all_permutations_df_5y_6th_Period.csv", 
           row.names = FALSE)

rm(final_rca_all_permutations_df, ipc_all_patents_first_period_df, ai_patents_period_1_df,
   country_all_patents_pool, final_permuted_dataset, list_of_permuted_dfs,list_of_rca_dfs, 
   list_of_replicated_not_selected_ai, not_selected_AI, original_selected_AI, permuted_ai_for_target_countries_iter, 
   randomly_selected_patents_df_country,region_tech_fields_perm_df, temp_df)

##Interval 7 (2004 - 2008) 
rm(ipc_all_patents_part2_df)
ipc_all_patents_part2_df <- fread("large_files/All_patents_and_IPC_codes_Part1.csv", 
                                  header = FALSE)

setnames(ipc_all_patents_part2_df, c("appln_id", "ctry_code", "ipc_class_symbol", "priority_year"))

ipc_all_patents_part2_df$Subclass <- substr(ipc_all_patents_part2_df$ipc_class_symbol,1,4)
setnames(ipc_all_patents_part2_df, c("appln_id", "ctry_code", "ipc_class_symbol", "priority_year","Subclass"))

start_year <- 2003
end_year   <- 2009

# Calculate AI-Specific RCA for Interval 7
ipc_all_patents_first_period_df <- ipc_all_patents_part2_df[priority_year > start_year & priority_year < end_year]
ipc_all_patents_first_period_df <- ipc_all_patents_first_period_df[, .(appln_id, ctry_code, Subclass)]

# Calculate AI-Specific RCA for Interval 1
ai_patents_period_1_df <- ai_patents_df[ai_patents_df$priority_year > start_year & ai_patents_df$priority_year < end_year,]

#For MULTIPLE Permutations
list_of_permuted_dfs <- vector("list", length = num_permutations)

for (p in 1:num_permutations) {
  if (p %% 100 == 0) print(paste("Permutation number:", p)) # Progress indicator
  
  # This dataframe will hold the permuted AI patents for target countries ONLY for THIS iteration
  permuted_ai_for_target_countries_iter <- data.frame()
  
  for (country in target_countries) {
    # 1. Identify and Count ACTUAL AI patents for the current country from the original AI dataset
    actual_ai_appln_ids_country <- ai_patents_period_1_df %>%
      filter(ctry_code == country) %>%
      distinct(appln_id) %>%
      pull(appln_id)
    
    n_ai_country <- length(actual_ai_appln_ids_country)
    
    if (n_ai_country == 0) {
      # print(paste("No AI patents found for", country, "in original AI data. Skipping for perm", p))
      next # Skip to the next country if no AI patents to replace
    }
    
    # 2. Prepare the pool of ALL patents for the current country from the general dataset
    country_all_patents_pool <- ipc_all_patents_first_period_df %>%
      filter(ctry_code == country) %>%
      distinct(appln_id)
    
    if (nrow(country_all_patents_pool) == 0) {
      # print(paste("No patents in general pool for", country, ". Skipping for perm", p))
      next
    }
    
    # Handle cases where the pool is smaller than the number of AI patents to sample
    # This is unlikely if ipc_all_patents_first_period_df is complete, but good for robustness
    sample_size <- min(n_ai_country, nrow(country_all_patents_pool))
    replace_sampling <- FALSE
    if (n_ai_country > nrow(country_all_patents_pool)) {
      if(nrow(country_all_patents_pool) < n_ai_country && !replace_sampling){
        next # Skip this country for this permutation if not enough patents
      }
    }
    
    # 3. Randomly select an equivalent number of unique appln_ids from this country's general pool
    random_appln_ids_country <- sample(country_all_patents_pool$appln_id,
                                       size = sample_size, # Use adjusted sample_size
                                       replace = replace_sampling) # Use replace_sampling flag
    
    # 4. Get all rows for these randomly selected patents from the ipc_all_patents_first_period_df
    randomly_selected_patents_df_country <- ipc_all_patents_first_period_df %>%
      filter(appln_id %in% random_appln_ids_country & ctry_code == country)
    
    # 5. Add these randomly selected patents for the current country to the iteration's df
    if (nrow(randomly_selected_patents_df_country) > 0) {
      permuted_ai_for_target_countries_iter <- bind_rows(
        permuted_ai_for_target_countries_iter,
        randomly_selected_patents_df_country
      )
    }
  } # End of country loop
  
  # Add the permutation number to all rows of this iteration's dataframe
  if (nrow(permuted_ai_for_target_countries_iter) > 0) {
    permuted_ai_for_target_countries_iter$permutation_number <- p
  }
  
  # Store the dataframe for this iteration in the list
  list_of_permuted_dfs[[p]] <- permuted_ai_for_target_countries_iter
  
} # End of permutation loop

# Combine all permuted dataframes from the list into one large dataframe
final_permuted_dataset <- bind_rows(list_of_permuted_dfs)

not_selected_AI <- ai_patents_period_1_df[ai_patents_period_1_df$ctry_code %notin% target_countries, c("appln_id", "ctry_code")]

# Create a list to hold the replicated dataframes
list_of_replicated_not_selected_ai <- vector("list", length = num_permutations + 1)

# Loop from 0 to num_permutations
for (i in 0:num_permutations) {
  # Create a copy of the not_selected_AI dataframe for this iteration
  temp_df <- not_selected_AI
  
  # Add the permutation_number column
  temp_df$permutation_number <- i
  
  # Store it in the list (adjust index because list is 1-based, i is 0-based)
  list_of_replicated_not_selected_ai[[i + 1]] <- temp_df
  
  if (i %% 100 == 0) print(paste("Replicating not_selected_AI for permutation_number:", i))
}

# Combine all replicated dataframes from the list into one large dataframe
replicated_not_selected_ai_final <- bind_rows(list_of_replicated_not_selected_ai)
table(replicated_not_selected_ai_final$permutation_number)

original_selected_AI <- ai_patents_period_1_df[ai_patents_period_1_df$ctry_code %in% target_countries, c("appln_id", "ctry_code")]
table(original_selected_AI$ctry_code)
original_selected_AI$permutation_number <- 0

#exclude the Subclass column (I don't need it for now)
final_permuted_dataset <- subset(final_permuted_dataset, select = -c(Subclass) )
#merge both in the "reference" dataset, when permutation = 0
original_selected_AI <- rbind(original_selected_AI, replicated_not_selected_ai_final)
rm(replicated_not_selected_ai_final)
table(original_selected_AI$ctry_code) #the non selected countries appear x times more, because they appear already for all the permutations
table(ai_patents_period_1_df$ctry_code) 
table(original_selected_AI$permutation_number)
length(unique(original_selected_AI$appln_id)) #10
length(unique(ai_patents_period_1_df$appln_id)) #10

#rbind it with the original dataset:
final_permuted_dataset <- rbind(original_selected_AI,final_permuted_dataset) 
table(final_permuted_dataset$permutation_number)

# Join technological fields info to AI patents
final_permuted_dataset <- distinct_at(final_permuted_dataset, vars(appln_id,permutation_number), .keep_all = T)
final_permuted_dataset <- subset(final_permuted_dataset, select = -c(ctry_code) )

final_permuted_dataset <- left_join(final_permuted_dataset, ipc_all_patents_first_period_df, by = "appln_id")

#Main Workflow incorporating permutations
# 1. Compute weighted fields for permuted AI patents
print("Step 1: Calculating field weights...")
region_tech_fields_perm_df <- group_by_applnID_perm(final_permuted_dataset)

# 2. Aggregate at the permutation-country-technology field level
print("Step 2: Aggregating by country and technology field per permutation...")
region_tech_fields_perm_df <- group_by_ctry_and_subclass(region_tech_fields_perm_df)
print("Counts of records per permutation_number after aggregation:")
print(table(region_tech_fields_perm_df$permutation_number))

print("Step 3: Calculating RCA per permutation...")

# Using dplyr::group_by and then purrr::map or a loop
# The result will be a list of dataframes (one RCA df per permutation)
list_of_rca_dfs <- region_tech_fields_perm_df %>%
  group_by(permutation_number) %>%
  group_split() %>% # This splits the df into a list of dfs, one for each permutation
  purrr::map(~{
    current_permutation_number <- unique(.x$permutation_number)
    print(paste("Processing RCA for permutation_number:", current_permutation_number))
    
    # Matrix creation for the current permutation's data
    mat_reg_tech_perm_AI <- .x %>%
      select(-permutation_number) %>% # Temporarily remove for pivot if it causes issues
      arrange(Subclass, ctry_code) %>%
      pivot_wider(names_from = Subclass,
                  values_from = n_tech_reg,
                  values_fill = 0) # Changed from list(n_tech_reg = 0) for simplicity
    
    # Check if ctry_code column exists and is not empty
    if (!"ctry_code" %in% names(mat_reg_tech_perm_AI) || nrow(mat_reg_tech_perm_AI) == 0 || all(is.na(mat_reg_tech_perm_AI$ctry_code))) {
      print(paste("Skipping permutation", current_permutation_number, "due to missing ctry_code or empty data after pivot."))
      return(NULL) # Return NULL or an empty tibble
    }
    
    # Check for duplicate ctry_codes which would prevent rownames_to_column
    if (any(duplicated(mat_reg_tech_perm_AI$ctry_code))) {
      print(paste("Warning: Duplicate ctry_code found for permutation", current_permutation_number, ". Aggregating or handling needed."))
      return(tibble(permutation_number = current_permutation_number, error="duplicate ctry_code"))
    }
    
    
    mat_reg_tech_perm_AI <- mat_reg_tech_perm_AI %>%
      remove_rownames() %>%
      column_to_rownames(var = "ctry_code") %>%
      as.matrix() %>%  round()# No rounding here, location_quotient might prefer raw numbers
    
    # RTA calculation
    # Ensure matrix is suitable (e.g., no NA/NaN/Inf that location_quotient can't handle)
    if (nrow(mat_reg_tech_perm_AI) == 0 || ncol(mat_reg_tech_perm_AI) == 0) {
      print(paste("Skipping RCA for permutation", current_permutation_number, "due to empty matrix."))
      return(NULL)
    }
    
    # Ensure there are at least two columns for location_quotient (ctry_code was one)
    if (ncol(mat_reg_tech_perm_AI) < 1) { # If only ctry_code was present and now it's rownames
      print(paste("Skipping RCA for permutation", current_permutation_number, "due to insufficient columns in matrix."))
      return(NULL)
    }
    
    
    # Check for all zero rows/columns if location_quotient is sensitive
    rca_results_perm <- tryCatch({
      mat_reg_tech_perm_AI %>%
        location_quotient(binary = FALSE) %>% # Assuming this is your function
        as.data.frame() %>%
        rownames_to_column("ctry_code") %>%
        as_tibble() %>%
        gather(key = "Subclass", value = "RCA", -ctry_code) %>%
        arrange(ctry_code, Subclass) %>%
        mutate(permutation_number = current_permutation_number) # Add back permutation number
    }, error = function(e) {
      print(paste("Error in location_quotient for permutation", current_permutation_number, ":", e$message))
      return(tibble(permutation_number = current_permutation_number, ctry_code=NA, Subclass=NA, RCA=NA, error_message = e$message)) # Return an empty or error-marked tibble
    })
    
    return(rca_results_perm)
  })

# Combine the list of RCA dataframes into one final dataframe
final_rca_all_permutations_df <- bind_rows(list_of_rca_dfs)

# Check for errors in the error column
if("error_message" %in% names(final_rca_all_permutations_df)){
  print("Permutations with errors during RCA calculation:")
  print(final_rca_all_permutations_df %>% filter(!is.na(error_message)))
}

write.csv2(final_rca_all_permutations_df, 
           file = "Files_created_with_the_code/data/files_code_4-digits_analysis/robustness/final_rca_all_permutations_df_5y_7th_Period.csv", 
           row.names = FALSE)

rm(final_rca_all_permutations_df, ipc_all_patents_first_period_df, ai_patents_period_1_df,
   country_all_patents_pool, final_permuted_dataset, list_of_permuted_dfs,list_of_rca_dfs, 
   list_of_replicated_not_selected_ai, not_selected_AI, original_selected_AI, permuted_ai_for_target_countries_iter, 
   randomly_selected_patents_df_country,region_tech_fields_perm_df, temp_df)

##Interval 8 (2009-2013)
start_year <- 2008
end_year   <- 2014

# Calculate AI-Specific RCA for Interval 8
ipc_all_patents_first_period_df <- ipc_all_patents_part2_df[priority_year > start_year & priority_year < end_year]
ipc_all_patents_first_period_df <- ipc_all_patents_first_period_df[, .(appln_id, ctry_code, Subclass)]

# Calculate AI-Specific RCA for Interval 1
ai_patents_period_1_df <- ai_patents_df[ai_patents_df$priority_year > start_year & ai_patents_df$priority_year < end_year,]

#For MULTIPLE Permutations
list_of_permuted_dfs <- vector("list", length = num_permutations)

for (p in 1:num_permutations) {
  if (p %% 100 == 0) print(paste("Permutation number:", p)) # Progress indicator
  
  # This dataframe will hold the permuted AI patents for target countries ONLY for THIS iteration
  permuted_ai_for_target_countries_iter <- data.frame()
  
  for (country in target_countries) {
    # 1. Identify and Count ACTUAL AI patents for the current country from the original AI dataset
    actual_ai_appln_ids_country <- ai_patents_period_1_df %>%
      filter(ctry_code == country) %>%
      distinct(appln_id) %>%
      pull(appln_id)
    
    n_ai_country <- length(actual_ai_appln_ids_country)
    
    if (n_ai_country == 0) {
      # print(paste("No AI patents found for", country, "in original AI data. Skipping for perm", p))
      next # Skip to the next country if no AI patents to replace
    }
    
    # 2. Prepare the pool of ALL patents for the current country from the general dataset
    country_all_patents_pool <- ipc_all_patents_first_period_df %>%
      filter(ctry_code == country) %>%
      distinct(appln_id)
    
    if (nrow(country_all_patents_pool) == 0) {
      # print(paste("No patents in general pool for", country, ". Skipping for perm", p))
      next
    }
    
    # Handle cases where the pool is smaller than the number of AI patents to sample
    # This is unlikely if ipc_all_patents_first_period_df is complete, but good for robustness
    sample_size <- min(n_ai_country, nrow(country_all_patents_pool))
    replace_sampling <- FALSE
    if (n_ai_country > nrow(country_all_patents_pool)) {
      if(nrow(country_all_patents_pool) < n_ai_country && !replace_sampling){
        next # Skip this country for this permutation if not enough patents
      }
    }
    
    # 3. Randomly select an equivalent number of unique appln_ids from this country's general pool
    random_appln_ids_country <- sample(country_all_patents_pool$appln_id,
                                       size = sample_size, # Use adjusted sample_size
                                       replace = replace_sampling) # Use replace_sampling flag
    
    # 4. Get all rows for these randomly selected patents from the ipc_all_patents_first_period_df
    randomly_selected_patents_df_country <- ipc_all_patents_first_period_df %>%
      filter(appln_id %in% random_appln_ids_country & ctry_code == country)
    
    # 5. Add these randomly selected patents for the current country to the iteration's df
    if (nrow(randomly_selected_patents_df_country) > 0) {
      permuted_ai_for_target_countries_iter <- bind_rows(
        permuted_ai_for_target_countries_iter,
        randomly_selected_patents_df_country
      )
    }
  } # End of country loop
  
  # Add the permutation number to all rows of this iteration's dataframe
  if (nrow(permuted_ai_for_target_countries_iter) > 0) {
    permuted_ai_for_target_countries_iter$permutation_number <- p
  }
  
  # Store the dataframe for this iteration in the list
  list_of_permuted_dfs[[p]] <- permuted_ai_for_target_countries_iter
  
} # End of permutation loop

# Combine all permuted dataframes from the list into one large dataframe
final_permuted_dataset <- bind_rows(list_of_permuted_dfs)

not_selected_AI <- ai_patents_period_1_df[ai_patents_period_1_df$ctry_code %notin% target_countries, c("appln_id", "ctry_code")]

# Create a list to hold the replicated dataframes
list_of_replicated_not_selected_ai <- vector("list", length = num_permutations + 1)

# Loop from 0 to num_permutations
for (i in 0:num_permutations) {
  # Create a copy of the not_selected_AI dataframe for this iteration
  temp_df <- not_selected_AI
  
  # Add the permutation_number column
  temp_df$permutation_number <- i
  
  # Store it in the list (adjust index because list is 1-based, i is 0-based)
  list_of_replicated_not_selected_ai[[i + 1]] <- temp_df
  
  if (i %% 100 == 0) print(paste("Replicating not_selected_AI for permutation_number:", i))
}

# Combine all replicated dataframes from the list into one large dataframe
replicated_not_selected_ai_final <- bind_rows(list_of_replicated_not_selected_ai)
table(replicated_not_selected_ai_final$permutation_number)

original_selected_AI <- ai_patents_period_1_df[ai_patents_period_1_df$ctry_code %in% target_countries, c("appln_id", "ctry_code")]
table(original_selected_AI$ctry_code)
original_selected_AI$permutation_number <- 0

#exclude the Subclass column (I don't need it for now)
final_permuted_dataset <- subset(final_permuted_dataset, select = -c(Subclass) )
#merge both in the "reference" dataset, when permutation = 0
original_selected_AI <- rbind(original_selected_AI, replicated_not_selected_ai_final)
rm(replicated_not_selected_ai_final)
table(original_selected_AI$ctry_code) #the non selected countries appear x times more, because they appear already for all the permutations
table(ai_patents_period_1_df$ctry_code) 
table(original_selected_AI$permutation_number)
length(unique(original_selected_AI$appln_id)) #10
length(unique(ai_patents_period_1_df$appln_id)) #10

#rbind it with the original dataset:
final_permuted_dataset <- rbind(original_selected_AI,final_permuted_dataset) 
table(final_permuted_dataset$permutation_number)

# Join technological fields info to AI patents
final_permuted_dataset <- distinct_at(final_permuted_dataset, vars(appln_id,permutation_number), .keep_all = T)
final_permuted_dataset <- subset(final_permuted_dataset, select = -c(ctry_code) )

final_permuted_dataset <- left_join(final_permuted_dataset, ipc_all_patents_first_period_df, by = "appln_id")

#Main Workflow incorporating permutations
# 1. Compute weighted fields for permuted AI patents
print("Step 1: Calculating field weights...")
region_tech_fields_perm_df <- group_by_applnID_perm(final_permuted_dataset)

# 2. Aggregate at the permutation-country-technology field level
print("Step 2: Aggregating by country and technology field per permutation...")
region_tech_fields_perm_df <- group_by_ctry_and_subclass(region_tech_fields_perm_df)
print("Counts of records per permutation_number after aggregation:")
print(table(region_tech_fields_perm_df$permutation_number))

print("Step 3: Calculating RCA per permutation...")

# Using dplyr::group_by and then purrr::map or a loop
# The result will be a list of dataframes (one RCA df per permutation)
list_of_rca_dfs <- region_tech_fields_perm_df %>%
  group_by(permutation_number) %>%
  group_split() %>% # This splits the df into a list of dfs, one for each permutation
  purrr::map(~{
    current_permutation_number <- unique(.x$permutation_number)
    print(paste("Processing RCA for permutation_number:", current_permutation_number))
    
    # Matrix creation for the current permutation's data
    mat_reg_tech_perm_AI <- .x %>%
      select(-permutation_number) %>% # Temporarily remove for pivot if it causes issues
      arrange(Subclass, ctry_code) %>%
      pivot_wider(names_from = Subclass,
                  values_from = n_tech_reg,
                  values_fill = 0) # Changed from list(n_tech_reg = 0) for simplicity
    
    # Check if ctry_code column exists and is not empty
    if (!"ctry_code" %in% names(mat_reg_tech_perm_AI) || nrow(mat_reg_tech_perm_AI) == 0 || all(is.na(mat_reg_tech_perm_AI$ctry_code))) {
      print(paste("Skipping permutation", current_permutation_number, "due to missing ctry_code or empty data after pivot."))
      return(NULL) # Return NULL or an empty tibble
    }
    
    # Check for duplicate ctry_codes which would prevent rownames_to_column
    if (any(duplicated(mat_reg_tech_perm_AI$ctry_code))) {
      print(paste("Warning: Duplicate ctry_code found for permutation", current_permutation_number, ". Aggregating or handling needed."))
      return(tibble(permutation_number = current_permutation_number, error="duplicate ctry_code"))
    }
    
    
    mat_reg_tech_perm_AI <- mat_reg_tech_perm_AI %>%
      remove_rownames() %>%
      column_to_rownames(var = "ctry_code") %>%
      as.matrix() %>%  round()# No rounding here, location_quotient might prefer raw numbers
    
    # RTA calculation
    # Ensure matrix is suitable (e.g., no NA/NaN/Inf that location_quotient can't handle)
    if (nrow(mat_reg_tech_perm_AI) == 0 || ncol(mat_reg_tech_perm_AI) == 0) {
      print(paste("Skipping RCA for permutation", current_permutation_number, "due to empty matrix."))
      return(NULL)
    }
    
    # Ensure there are at least two columns for location_quotient (ctry_code was one)
    if (ncol(mat_reg_tech_perm_AI) < 1) { # If only ctry_code was present and now it's rownames
      print(paste("Skipping RCA for permutation", current_permutation_number, "due to insufficient columns in matrix."))
      return(NULL)
    }
    
    
    # Check for all zero rows/columns if location_quotient is sensitive
    rca_results_perm <- tryCatch({
      mat_reg_tech_perm_AI %>%
        location_quotient(binary = FALSE) %>% # Assuming this is your function
        as.data.frame() %>%
        rownames_to_column("ctry_code") %>%
        as_tibble() %>%
        gather(key = "Subclass", value = "RCA", -ctry_code) %>%
        arrange(ctry_code, Subclass) %>%
        mutate(permutation_number = current_permutation_number) # Add back permutation number
    }, error = function(e) {
      print(paste("Error in location_quotient for permutation", current_permutation_number, ":", e$message))
      return(tibble(permutation_number = current_permutation_number, ctry_code=NA, Subclass=NA, RCA=NA, error_message = e$message)) # Return an empty or error-marked tibble
    })
    
    return(rca_results_perm)
  })

# Combine the list of RCA dataframes into one final dataframe
final_rca_all_permutations_df <- bind_rows(list_of_rca_dfs)

# Check for errors in the error column
if("error_message" %in% names(final_rca_all_permutations_df)){
  print("Permutations with errors during RCA calculation:")
  print(final_rca_all_permutations_df %>% filter(!is.na(error_message)))
}

write.csv2(final_rca_all_permutations_df, 
           file = "Files_created_with_the_code/data/files_code_4-digits_analysis/robustness/final_rca_all_permutations_df_5y_8th_Period.csv", 
           row.names = FALSE)

rm(final_rca_all_permutations_df, ipc_all_patents_first_period_df, ai_patents_period_1_df,
   country_all_patents_pool, final_permuted_dataset, list_of_permuted_dfs,list_of_rca_dfs, 
   list_of_replicated_not_selected_ai, not_selected_AI, original_selected_AI, permuted_ai_for_target_countries_iter, 
   randomly_selected_patents_df_country,region_tech_fields_perm_df, temp_df)

#Interval 9 (2014-2018)
start_year <- 2013
end_year   <- 2019

# Calculate AI-Specific RCA for Interval 9
ipc_all_patents_first_period_df <- ipc_all_patents_part2_df[priority_year > start_year & priority_year < end_year]
ipc_all_patents_first_period_df <- ipc_all_patents_first_period_df[, .(appln_id, ctry_code, Subclass)]

# Calculate AI-Specific RCA for Interval 1
ai_patents_period_1_df <- ai_patents_df[ai_patents_df$priority_year > start_year & ai_patents_df$priority_year < end_year,]

#For MULTIPLE Permutations
list_of_permuted_dfs <- vector("list", length = num_permutations)

for (p in 1:num_permutations) {
  if (p %% 100 == 0) print(paste("Permutation number:", p)) # Progress indicator
  
  # This dataframe will hold the permuted AI patents for target countries ONLY for THIS iteration
  permuted_ai_for_target_countries_iter <- data.frame()
  
  for (country in target_countries) {
    # 1. Identify and Count ACTUAL AI patents for the current country from the original AI dataset
    actual_ai_appln_ids_country <- ai_patents_period_1_df %>%
      filter(ctry_code == country) %>%
      distinct(appln_id) %>%
      pull(appln_id)
    
    n_ai_country <- length(actual_ai_appln_ids_country)
    
    if (n_ai_country == 0) {
      # print(paste("No AI patents found for", country, "in original AI data. Skipping for perm", p))
      next # Skip to the next country if no AI patents to replace
    }
    
    # 2. Prepare the pool of ALL patents for the current country from the general dataset
    country_all_patents_pool <- ipc_all_patents_first_period_df %>%
      filter(ctry_code == country) %>%
      distinct(appln_id)
    
    if (nrow(country_all_patents_pool) == 0) {
      # print(paste("No patents in general pool for", country, ". Skipping for perm", p))
      next
    }
    
    # Handle cases where the pool is smaller than the number of AI patents to sample
    # This is unlikely if ipc_all_patents_first_period_df is complete, but good for robustness
    sample_size <- min(n_ai_country, nrow(country_all_patents_pool))
    replace_sampling <- FALSE
    if (n_ai_country > nrow(country_all_patents_pool)) {
      if(nrow(country_all_patents_pool) < n_ai_country && !replace_sampling){
        next # Skip this country for this permutation if not enough patents
      }
    }
    
    # 3. Randomly select an equivalent number of unique appln_ids from this country's general pool
    random_appln_ids_country <- sample(country_all_patents_pool$appln_id,
                                       size = sample_size, # Use adjusted sample_size
                                       replace = replace_sampling) # Use replace_sampling flag
    
    # 4. Get all rows for these randomly selected patents from the ipc_all_patents_first_period_df
    randomly_selected_patents_df_country <- ipc_all_patents_first_period_df %>%
      filter(appln_id %in% random_appln_ids_country & ctry_code == country)
    
    # 5. Add these randomly selected patents for the current country to the iteration's df
    if (nrow(randomly_selected_patents_df_country) > 0) {
      permuted_ai_for_target_countries_iter <- bind_rows(
        permuted_ai_for_target_countries_iter,
        randomly_selected_patents_df_country
      )
    }
  } # End of country loop
  
  # Add the permutation number to all rows of this iteration's dataframe
  if (nrow(permuted_ai_for_target_countries_iter) > 0) {
    permuted_ai_for_target_countries_iter$permutation_number <- p
  }
  
  # Store the dataframe for this iteration in the list
  list_of_permuted_dfs[[p]] <- permuted_ai_for_target_countries_iter
  
} # End of permutation loop

# Combine all permuted dataframes from the list into one large dataframe
final_permuted_dataset <- bind_rows(list_of_permuted_dfs)

not_selected_AI <- ai_patents_period_1_df[ai_patents_period_1_df$ctry_code %notin% target_countries, c("appln_id", "ctry_code")]

# Create a list to hold the replicated dataframes
list_of_replicated_not_selected_ai <- vector("list", length = num_permutations + 1)

# Loop from 0 to num_permutations
for (i in 0:num_permutations) {
  # Create a copy of the not_selected_AI dataframe for this iteration
  temp_df <- not_selected_AI
  
  # Add the permutation_number column
  temp_df$permutation_number <- i
  
  # Store it in the list (adjust index because list is 1-based, i is 0-based)
  list_of_replicated_not_selected_ai[[i + 1]] <- temp_df
  
  if (i %% 100 == 0) print(paste("Replicating not_selected_AI for permutation_number:", i))
}

# Combine all replicated dataframes from the list into one large dataframe
replicated_not_selected_ai_final <- bind_rows(list_of_replicated_not_selected_ai)
table(replicated_not_selected_ai_final$permutation_number)

original_selected_AI <- ai_patents_period_1_df[ai_patents_period_1_df$ctry_code %in% target_countries, c("appln_id", "ctry_code")]
table(original_selected_AI$ctry_code)
original_selected_AI$permutation_number <- 0

#exclude the Subclass column (I don't need it for now)
final_permuted_dataset <- subset(final_permuted_dataset, select = -c(Subclass) )
#merge both in the "reference" dataset, when permutation = 0
original_selected_AI <- rbind(original_selected_AI, replicated_not_selected_ai_final)
rm(replicated_not_selected_ai_final)
table(original_selected_AI$ctry_code) #the non selected countries appear x times more, because they appear already for all the permutations
table(ai_patents_period_1_df$ctry_code) 
table(original_selected_AI$permutation_number)
length(unique(original_selected_AI$appln_id)) #10
length(unique(ai_patents_period_1_df$appln_id)) #10

#rbind it with the original dataset:
final_permuted_dataset <- rbind(original_selected_AI,final_permuted_dataset) 
table(final_permuted_dataset$permutation_number)

# Join technological fields info to AI patents
final_permuted_dataset <- distinct_at(final_permuted_dataset, vars(appln_id,permutation_number), .keep_all = T)
final_permuted_dataset <- subset(final_permuted_dataset, select = -c(ctry_code) )

final_permuted_dataset <- left_join(final_permuted_dataset, ipc_all_patents_first_period_df, by = "appln_id")

#Main Workflow incorporating permutations
# 1. Compute weighted fields for permuted AI patents
print("Step 1: Calculating field weights...")
region_tech_fields_perm_df <- group_by_applnID_perm(final_permuted_dataset)

# 2. Aggregate at the permutation-country-technology field level
print("Step 2: Aggregating by country and technology field per permutation...")
region_tech_fields_perm_df <- group_by_ctry_and_subclass(region_tech_fields_perm_df)
print("Counts of records per permutation_number after aggregation:")
print(table(region_tech_fields_perm_df$permutation_number))

print("Step 3: Calculating RCA per permutation...")

# Using dplyr::group_by and then purrr::map or a loop
# The result will be a list of dataframes (one RCA df per permutation)
list_of_rca_dfs <- region_tech_fields_perm_df %>%
  group_by(permutation_number) %>%
  group_split() %>% # This splits the df into a list of dfs, one for each permutation
  purrr::map(~{
    current_permutation_number <- unique(.x$permutation_number)
    print(paste("Processing RCA for permutation_number:", current_permutation_number))
    
    # Matrix creation for the current permutation's data
    mat_reg_tech_perm_AI <- .x %>%
      select(-permutation_number) %>% # Temporarily remove for pivot if it causes issues
      arrange(Subclass, ctry_code) %>%
      pivot_wider(names_from = Subclass,
                  values_from = n_tech_reg,
                  values_fill = 0) # Changed from list(n_tech_reg = 0) for simplicity
    
    # Check if ctry_code column exists and is not empty
    if (!"ctry_code" %in% names(mat_reg_tech_perm_AI) || nrow(mat_reg_tech_perm_AI) == 0 || all(is.na(mat_reg_tech_perm_AI$ctry_code))) {
      print(paste("Skipping permutation", current_permutation_number, "due to missing ctry_code or empty data after pivot."))
      return(NULL) # Return NULL or an empty tibble
    }
    
    # Check for duplicate ctry_codes which would prevent rownames_to_column
    if (any(duplicated(mat_reg_tech_perm_AI$ctry_code))) {
      print(paste("Warning: Duplicate ctry_code found for permutation", current_permutation_number, ". Aggregating or handling needed."))
      return(tibble(permutation_number = current_permutation_number, error="duplicate ctry_code"))
    }
    
    
    mat_reg_tech_perm_AI <- mat_reg_tech_perm_AI %>%
      remove_rownames() %>%
      column_to_rownames(var = "ctry_code") %>%
      as.matrix() %>%  round()# No rounding here, location_quotient might prefer raw numbers
    
    # RTA calculation
    # Ensure matrix is suitable (e.g., no NA/NaN/Inf that location_quotient can't handle)
    if (nrow(mat_reg_tech_perm_AI) == 0 || ncol(mat_reg_tech_perm_AI) == 0) {
      print(paste("Skipping RCA for permutation", current_permutation_number, "due to empty matrix."))
      return(NULL)
    }
    
    # Ensure there are at least two columns for location_quotient (ctry_code was one)
    if (ncol(mat_reg_tech_perm_AI) < 1) { # If only ctry_code was present and now it's rownames
      print(paste("Skipping RCA for permutation", current_permutation_number, "due to insufficient columns in matrix."))
      return(NULL)
    }
    
    
    # Check for all zero rows/columns if location_quotient is sensitive
    rca_results_perm <- tryCatch({
      mat_reg_tech_perm_AI %>%
        location_quotient(binary = FALSE) %>% # Assuming this is your function
        as.data.frame() %>%
        rownames_to_column("ctry_code") %>%
        as_tibble() %>%
        gather(key = "Subclass", value = "RCA", -ctry_code) %>%
        arrange(ctry_code, Subclass) %>%
        mutate(permutation_number = current_permutation_number) # Add back permutation number
    }, error = function(e) {
      print(paste("Error in location_quotient for permutation", current_permutation_number, ":", e$message))
      return(tibble(permutation_number = current_permutation_number, ctry_code=NA, Subclass=NA, RCA=NA, error_message = e$message)) # Return an empty or error-marked tibble
    })
    
    return(rca_results_perm)
  })

# Combine the list of RCA dataframes into one final dataframe
final_rca_all_permutations_df <- bind_rows(list_of_rca_dfs)

# Check for errors in the error column
if("error_message" %in% names(final_rca_all_permutations_df)){
  print("Permutations with errors during RCA calculation:")
  print(final_rca_all_permutations_df %>% filter(!is.na(error_message)))
}

write.csv2(final_rca_all_permutations_df, 
           file = "Files_created_with_the_code/data/files_code_4-digits_analysis/robustness/final_rca_all_permutations_df_5y_9th_Period.csv", 
           row.names = FALSE)


###2.2.3.Calculate specialisations subclasses 5-years------
rm(list=ls())
gc()

group_by_applnID <- function(data){
  data %>%
    group_by(appln_id) %>%
    mutate(field_weight = 1 / n()) %>%
    ungroup()
}

# This function aggregates the weighted fields at the country-subclass level.
group_by_ctry_and_subclass <- function(data){
  data %<>%
    group_by(ctry_code, Subclass) %>%
    summarise(n_tech_reg = sum(field_weight)) %>%
    ungroup() %>%
    drop_na() 
}

##Subclasses First to Sixth intervals (1974-2003)
# General Perspective First interval  
IPC_all_patents_Part1 <- fread("large_files/All_patents_and_IPC_codes_Part2.csv", header = F)
names(IPC_all_patents_Part1) <- c("appln_id", "ctry_code", "ipc_class_symbol", "priority_year")

unique(IPC_all_patents_Part1$priority_year)
#first period: 1974-1978
start_year <- 1973
end_year   <- 1979

IPC_all_patents_FirstPeriod <- IPC_all_patents_Part1[priority_year > start_year & priority_year < end_year]
length(unique(IPC_all_patents_FirstPeriod$appln_id))#1,123,341 unique publication numbers (i.e., unique priorities) out of
length(IPC_all_patents_FirstPeriod$appln_id) #6,298,074 lines of data

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

# AI perspective First interval
#For the first interval, which goes from 1974 to 1988, we need only the dataset from Part2:
patents_AI <- read.csv("other_files/IPCs_AI.csv", sep = ";", header = TRUE, dec=",")
#unique(patents_AI$priority_year)
patents_AI_specific_1st <- patents_AI[patents_AI$priority_year > start_year & patents_AI$priority_year < end_year,]
unique(patents_AI_specific_1st$priority_year)

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

Data1period$Period <- paste0(start_year+1,"-",end_year-1)

names(Data1period) <- c("ctry_code", "Subclass", "RCA_Gen", "RCA_AI", "Period")
write.csv2(Data1period, file = "Files_created_with_the_code/data/files_code_4-digits_analysis/Data1period_RCA_subclass_5y.csv", row.names = F)
rm(mat_reg_tech1)

##Interval 2 (1979-1983) 
# Define Time Interval 
start_year <- 1978
end_year   <- 1984

IPC_all_patents_FirstPeriod <- IPC_all_patents_Part1[priority_year > start_year & priority_year < end_year]

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

#unique(patents_AI$priority_year)
patents_AI_specific_1st <- patents_AI[patents_AI$priority_year > start_year & patents_AI$priority_year < end_year,]
unique(patents_AI_specific_1st$priority_year)

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

Data2period <- merge(reg_RCA1, reg_RCA1_AIspecific, all=T, by=c("ctry_code", "Subclass"))
Data2period$Period <- paste0(start_year+1,"-",end_year-1)

names(Data2period) <- c("ctry_code", "Subclass", "RCA_Gen", "RCA_AI", "Period")
write.csv2(Data2period, file = "Files_created_with_the_code/data/files_code_4-digits_analysis/Data2period_RCA_subclass_5y.csv", row.names = F)
rm(mat_reg_tech1)

##Interval 3 (1984-1988) 
# Define Time Interval 
start_year <- 1983
end_year   <- 1989

IPC_all_patents_FirstPeriod <- IPC_all_patents_Part1[priority_year > start_year & priority_year < end_year]

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

patents_AI_specific_1st <- patents_AI[patents_AI$priority_year > start_year & patents_AI$priority_year < end_year,]
unique(patents_AI_specific_1st$priority_year)

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

Data3period <- merge(reg_RCA1, reg_RCA1_AIspecific, all=T, by=c("ctry_code", "Subclass"))

Data3period$Period <- paste0(start_year+1,"-",end_year-1)
names(Data3period) <- c("ctry_code", "Subclass", "RCA_Gen", "RCA_AI", "Period")
write.csv2(Data3period, file = "Files_created_with_the_code/data/files_code_4-digits_analysis/Data3period_RCA_subclass_5y.csv", row.names = F)
rm(mat_reg_tech1)

##Interval 4 (1989-1993)
# Define Time Interval 
start_year <- 1988
end_year   <- 1994

IPC_all_patents_FirstPeriod <- IPC_all_patents_Part1[priority_year > start_year & priority_year < end_year]

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

#unique(patents_AI$priority_year)
patents_AI_specific_1st <- patents_AI[patents_AI$priority_year > start_year & patents_AI$priority_year < end_year,]
unique(patents_AI_specific_1st$priority_year)

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

Data4period <- merge(reg_RCA1, reg_RCA1_AIspecific, all=T, by=c("ctry_code", "Subclass"))
Data4period$Period <- paste0(start_year+1,"-",end_year-1)

names(Data4period) <- c("ctry_code", "Subclass", "RCA_Gen", "RCA_AI", "Period")
write.csv2(Data4period, file = "Files_created_with_the_code/data/files_code_4-digits_analysis/Data4period_RCA_subclass_5y.csv", row.names = F)
rm(mat_reg_tech1)

##Interval 5 (1994-1998)
# Define Time Interval 
start_year <- 1993
end_year   <- 1999

IPC_all_patents_FirstPeriod <- IPC_all_patents_Part1[priority_year > start_year & priority_year < end_year]

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

#unique(patents_AI$priority_year)
patents_AI_specific_1st <- patents_AI[patents_AI$priority_year > start_year & patents_AI$priority_year < end_year,]
unique(patents_AI_specific_1st$priority_year)

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

Data5period <- merge(reg_RCA1, reg_RCA1_AIspecific, all=T, by=c("ctry_code", "Subclass"))

Data5period$Period <- paste0(start_year+1,"-",end_year-1)

names(Data5period) <- c("ctry_code", "Subclass", "RCA_Gen", "RCA_AI", "Period")
write.csv2(Data5period, file = "Files_created_with_the_code/data/files_code_4-digits_analysis/Data5period_RCA_subclass_5y.csv", row.names = F)
rm(mat_reg_tech1)

##Interval 6 (1999-2003) 
#"1999-2003",
# Define Time Interval 
start_year <- 1998
end_year   <- 2004

IPC_all_patents_FirstPeriod <- IPC_all_patents_Part1[priority_year > start_year & priority_year < end_year]
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

#unique(patents_AI$priority_year)
patents_AI_specific_1st <- patents_AI[patents_AI$priority_year > start_year & patents_AI$priority_year < end_year,]
unique(patents_AI_specific_1st$priority_year)

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

Data6period <- merge(reg_RCA1, reg_RCA1_AIspecific, all=T, by=c("ctry_code", "Subclass"))

Data6period$Period <- paste0(start_year+1,"-",end_year-1)

names(Data6period) <- c("ctry_code", "Subclass", "RCA_Gen", "RCA_AI", "Period")
write.csv2(Data6period, file = "Files_created_with_the_code/data/files_code_4-digits_analysis/Data6period_RCA_subclass_5y.csv", row.names = F)
rm(mat_reg_tech1)
rm(IPC_all_patents_Part1)

#Subclasses Seventh to ninth intervals (2004-2018)
IPC_all_patents_Part2 <- fread("large_files/All_patents_and_IPC_codes_Part1.csv", header = F)
names(IPC_all_patents_Part2) <- c("appln_id", "ctry_code", "ipc_class_symbol", "priority_year")

#Interval 7 (2004-2008)
start_year <- 2003
end_year   <- 2009

IPC_all_patents_SecondPeriod <- IPC_all_patents_Part2[priority_year > start_year & priority_year < end_year]
#we pick just the subclass for analysis:
IPC_all_patents_SecondPeriod$Subclass <- substr(IPC_all_patents_SecondPeriod$ipc_class_symbol,1,4)

#now we apply the 2 functions we created at the beginning of this section:
reg_tech1 <- group_by_applnID(IPC_all_patents_SecondPeriod)
rm(IPC_all_patents_SecondPeriod)
reg_tech1 <- group_by_ctry_and_subclass(reg_tech1)

mat_reg_tech1 <- reg_tech1 %>%  arrange(Subclass, ctry_code) %>%
  pivot_wider(names_from = Subclass, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

mat_reg_tech1 %<>% remove_rownames %>% column_to_rownames(var="ctry_code") %>%
  as.matrix() %>%  round()

reg_RCA1 <- mat_reg_tech1 %>% location_quotient(binary = F) %>%   as.data.frame() %>% 
  rownames_to_column("ctry_code") %>%   as_tibble() %>% 
  gather(key = "Subclass", value = "RCA", -ctry_code) %>%  arrange(ctry_code, Subclass)

#unique(patents_AI$priority_year)
patents_AI_specific_1st <- patents_AI[patents_AI$priority_year > start_year & patents_AI$priority_year < end_year,]
unique(patents_AI_specific_1st$priority_year)

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

Data7period <- merge(reg_RCA1, reg_RCA1_AIspecific, all=T, by=c("ctry_code", "Subclass"))
Data7period$Period <- paste0(start_year+1,"-",end_year-1)
names(Data7period) <- c("ctry_code", "Subclass", "RCA_Gen", "RCA_AI", "Period")

write.csv2(Data7period, file = "Files_created_with_the_code/data/files_code_4-digits_analysis/Data7period_RCA_subclass_5y.csv", row.names = F)
rm(mat_reg_tech1)

#Interval 8 (2009-2013)-
start_year <- 2008
end_year   <- 2014

IPC_all_patents_SecondPeriod <- IPC_all_patents_Part2[priority_year > start_year & priority_year < end_year]
#we pick just the subclass for analysis:
IPC_all_patents_SecondPeriod$Subclass <- substr(IPC_all_patents_SecondPeriod$ipc_class_symbol,1,4)

#now we apply the 2 functions we created at the beginning of this section:
reg_tech1 <- group_by_applnID(IPC_all_patents_SecondPeriod)
rm(IPC_all_patents_SecondPeriod)
reg_tech1 <- group_by_ctry_and_subclass(reg_tech1)

mat_reg_tech1 <- reg_tech1 %>%  arrange(Subclass, ctry_code) %>%
  pivot_wider(names_from = Subclass, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

mat_reg_tech1 %<>% remove_rownames %>% column_to_rownames(var="ctry_code") %>%
  as.matrix() %>%  round()

reg_RCA1 <- mat_reg_tech1 %>% location_quotient(binary = F) %>%   as.data.frame() %>% 
  rownames_to_column("ctry_code") %>%   as_tibble() %>% 
  gather(key = "Subclass", value = "RCA", -ctry_code) %>%  arrange(ctry_code, Subclass)

#unique(patents_AI$priority_year)
patents_AI_specific_1st <- patents_AI[patents_AI$priority_year > start_year & patents_AI$priority_year < end_year,]
unique(patents_AI_specific_1st$priority_year)

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

Data8period <- merge(reg_RCA1, reg_RCA1_AIspecific, all=T, by=c("ctry_code", "Subclass"))
Data8period$Period <- paste0(start_year+1,"-",end_year-1)
names(Data8period) <- c("ctry_code", "Subclass", "RCA_Gen", "RCA_AI", "Period")

write.csv2(Data8period, file = "Files_created_with_the_code/data/files_code_4-digits_analysis/Data8period_RCA_subclass_5y.csv", row.names = F)
rm(mat_reg_tech1)

#Interval 9 (2014-2018)-
start_year <- 2013
end_year   <- 2019

IPC_all_patents_SecondPeriod <- IPC_all_patents_Part2[priority_year > start_year & priority_year < end_year]
#we pick just the subclass for analysis:
IPC_all_patents_SecondPeriod$Subclass <- substr(IPC_all_patents_SecondPeriod$ipc_class_symbol,1,4)

#now we apply the 2 functions we created at the beginning of this section:
reg_tech1 <- group_by_applnID(IPC_all_patents_SecondPeriod)
rm(IPC_all_patents_SecondPeriod)
reg_tech1 <- group_by_ctry_and_subclass(reg_tech1)

mat_reg_tech1 <- reg_tech1 %>%  arrange(Subclass, ctry_code) %>%
  pivot_wider(names_from = Subclass, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

mat_reg_tech1 %<>% remove_rownames %>% column_to_rownames(var="ctry_code") %>%
  as.matrix() %>%  round()

reg_RCA1 <- mat_reg_tech1 %>% location_quotient(binary = F) %>%   as.data.frame() %>% 
  rownames_to_column("ctry_code") %>%   as_tibble() %>% 
  gather(key = "Subclass", value = "RCA", -ctry_code) %>%  arrange(ctry_code, Subclass)

#unique(patents_AI$priority_year)
patents_AI_specific_1st <- patents_AI[patents_AI$priority_year > start_year & patents_AI$priority_year < end_year,]
unique(patents_AI_specific_1st$priority_year)

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

Data9period <- merge(reg_RCA1, reg_RCA1_AIspecific, all=T, by=c("ctry_code", "Subclass"))
Data9period$Period <- paste0(start_year+1,"-",end_year-1)
names(Data9period) <- c("ctry_code", "Subclass", "RCA_Gen", "RCA_AI", "Period")

write.csv2(Data9period, file = "Files_created_with_the_code/data/files_code_4-digits_analysis/Data9period_RCA_subclass_5y.csv", row.names = F)
rm(mat_reg_tech1)

IPC_RCAs <- rbind(Data1period, Data2period, Data3period, Data4period,Data5period,Data6period,Data7period,Data8period,Data9period)
write.csv2(IPC_RCAs, file = "Files_created_with_the_code/data/files_code_4-digits_analysis/IPC_RCAs_subclass_5y.csv", row.names = F)

###2.2.4.Read and combine everything for subclass level------
####2.2.4.1. Intervals 15-years----
rm(list = ls())
#historical data for double-check
historical_ai <- read.csv("Files_created_with_the_code/data/files_code_4-digits_analysis/IPC_RCAs_subclass.csv", 
                          sep = ";", header = TRUE, dec=",")

#read permutations
#1st period
Permutations_1st <- read.csv("Files_created_with_the_code/data/files_code_4-digits_analysis/robustness/final_rca_all_permutations_df_1st_Period.csv", 
                             sep = ";", dec = ",", header = TRUE)
Permutations_1st$Period <- "1974-1988"
Permutations_1st <- Permutations_1st[Permutations_1st$ctry_code == "JP"|Permutations_1st$ctry_code == "US"|
                                       Permutations_1st$ctry_code == "CN"|Permutations_1st$ctry_code == "KR",]
Reference_1st <- Permutations_1st[Permutations_1st$permutation_number == 0,]

#2nd period
Permutations_2nd <- read.csv("Files_created_with_the_code/data/files_code_4-digits_analysis/robustness/final_rca_all_permutations_df_2nd_Period.csv", 
                             sep = ";", dec = ",", header = TRUE)
Permutations_2nd$Period <- "1989-2003"
Permutations_2nd <- Permutations_2nd[Permutations_2nd$ctry_code == "JP"|Permutations_2nd$ctry_code == "US"|
                                       Permutations_2nd$ctry_code == "CN"|Permutations_2nd$ctry_code == "KR",]
Reference_2nd <- Permutations_2nd[Permutations_2nd$permutation_number == 0,]

#3rd period
Permutations_3rd <- read.csv("Files_created_with_the_code/data/files_code_4-digits_analysis/robustness/final_rca_all_permutations_df_3rd_Period.csv", 
                             sep = ";", dec = ",", header = TRUE)
Permutations_3rd$Period <- "2004-2018"
Permutations_3rd <- Permutations_3rd[Permutations_3rd$ctry_code == "JP"|Permutations_3rd$ctry_code == "US"|
                                       Permutations_3rd$ctry_code == "CN"|Permutations_3rd$ctry_code == "KR",]
Reference_3rd <- Permutations_3rd[Permutations_3rd$permutation_number == 0,]

#double-check with the baseline
historical_ai_1st <- historical_ai[historical_ai$Period == "1974-1988",]
historical_ai_1st <- historical_ai_1st[historical_ai_1st$ctry_code == "JP"|historical_ai_1st$ctry_code == "US"|
                                         historical_ai_1st$ctry_code == "CN"|historical_ai_1st$ctry_code == "KR",]
sum(historical_ai_1st$RCA_AI, na.rm=T) #81.90424
sum(Reference_1st$RCA) #81.90424

historical_ai_2nd <- historical_ai[historical_ai$Period == "1989-2003",]
historical_ai_2nd <- historical_ai_2nd[historical_ai_2nd$ctry_code == "JP"|historical_ai_2nd$ctry_code == "US"|
                                         historical_ai_2nd$ctry_code == "CN"|historical_ai_2nd$ctry_code == "KR",]
sum(historical_ai_2nd$RCA_AI, na.rm=T) #1245.661
sum(Reference_2nd$RCA) #1245.661

historical_ai_3rd <- historical_ai[historical_ai$Period == "2004-2018",]
historical_ai_3rd <- historical_ai_3rd[historical_ai_3rd$ctry_code == "JP"|historical_ai_3rd$ctry_code == "US"|
                                         historical_ai_3rd$ctry_code == "CN"|historical_ai_3rd$ctry_code == "KR",]
sum(historical_ai_3rd$RCA_AI, na.rm=T) #1451.323
sum(Reference_3rd$RCA) #1451.323
#all correct!

#now, for making it sure that the technological fields don't disappear when there is no AI specialization:
# Define all possible techn_field_nr values
all_subclasses <- unique(historical_ai$Subclass) #653; it should be 645 if ALL TECHNOLOGIES WERE INCLUDED (BUT IT'S JUST AI); there are then at
#least 8 codes that are probably typos from PATSTAT or from the patent office related to the place that filled them; the codes that
#don't exist are: "B33Y" "C13C" "C13H" "C99Z" "G16H" "G99Z" "A99Z" "B99Z" "D99Z" "E99Z" "F99Z"  "H99Z" (12 codes, 641 should remain)
all_subclasses <- all_subclasses[!all_subclasses %in% c("B33Y", "C13C", "C13H", "C99Z", "G16H", "G99Z", "A99Z", "B99Z", "D99Z", "E99Z", "F99Z", "H99Z")]
all_periods  <- c("1974-1988", "1989-2003", "2004-2018")
all_ctry_codes <- c("CN", "JP", "KR", "US") # Define ALL countries you want in the final dataset

all_permutation_numbers <- unique(Permutations_1st$permutation_number)
if (length(all_permutation_numbers) == 0) { # Handle case where Permutations_1st might be empty
  all_permutation_numbers <- 0 # Or your default/expected range, e.g., 0:1000
}

scaffold_groups <- expand.grid(
  ctry_code = all_ctry_codes,
  permutation_number = all_permutation_numbers,
  Period = all_periods,
  stringsAsFactors = FALSE # Important for character vectors
) %>% as_tibble()

# --- Now expand this group scaffold by all technological fields ---
scaffold_df <- scaffold_groups %>%
  expand(nesting(ctry_code, permutation_number, Period), # Uses ALL defined combinations
         Subclass = all_subclasses)

#for the first period:
# 2. Left join your original data to this scaffold
Permutations_1st_completed_v2 <- left_join(
  scaffold_df,
  Permutations_1st,
  by = c("ctry_code", "permutation_number", "Period", "Subclass")
)

# 3. Fill NA values in RCA with 0 (for the newly created rows)
Permutations_1st_completed_v2 <- Permutations_1st_completed_v2 %>%
  mutate(RCA = ifelse(is.na(RCA), 0, RCA)) %>%
  arrange(permutation_number, ctry_code, Period, Subclass)

#for the second period:
scaffold_groups <- expand.grid(
  ctry_code = all_ctry_codes,
  permutation_number = all_permutation_numbers,
  Period = all_periods,
  stringsAsFactors = FALSE # Important for character vectors
) %>% as_tibble()

# --- Now expand this group scaffold by all technological fields ---
scaffold_df <- scaffold_groups %>%
  expand(nesting(ctry_code, permutation_number, Period), # Uses ALL defined combinations
         Subclass = all_subclasses)

# 2. Left join your original data to this scaffold
Permutations_2nd_completed_v2 <- left_join(
  scaffold_df,
  Permutations_2nd,
  by = c("ctry_code", "permutation_number", "Period", "Subclass")
)

# 3. Fill NA values in RCA with 0 (for the newly created rows)
Permutations_2nd_completed_v2 <- Permutations_2nd_completed_v2 %>%
  mutate(RCA = ifelse(is.na(RCA), 0, RCA)) %>%
  arrange(permutation_number, ctry_code, Period, Subclass)

#for the third period:
scaffold_groups <- expand.grid(
  ctry_code = all_ctry_codes,
  permutation_number = all_permutation_numbers,
  Period = all_periods,
  stringsAsFactors = FALSE # Important for character vectors
) %>% as_tibble()

# --- Now expand this group scaffold by all technological fields ---
scaffold_df <- scaffold_groups %>%
  expand(nesting(ctry_code, permutation_number, Period), # Uses ALL defined combinations
         Subclass = all_subclasses)

# 2. Left join your original data to this scaffold
Permutations_3rd_completed_v2 <- left_join(
  scaffold_df,
  Permutations_3rd,
  by = c("ctry_code", "permutation_number", "Period", "Subclass")
)

# 3. Fill NA values in RCA with 0 (for the newly created rows)
Permutations_3rd_completed_v2 <- Permutations_3rd_completed_v2 %>%
  mutate(RCA = ifelse(is.na(RCA), 0, RCA)) %>%
  arrange(permutation_number, ctry_code, Period, Subclass)

#now, left_join everything
Permutations_1st <- left_join(historical_ai_1st, Permutations_1st_completed_v2, 
                              by = c("ctry_code","Subclass", "Period"))
Permutations_2nd <- left_join(historical_ai_2nd, Permutations_2nd_completed_v2, 
                              by = c("ctry_code","Subclass", "Period"))
Permutations_3rd <- left_join(historical_ai_3rd, Permutations_3rd_completed_v2, 
                              by = c("ctry_code","Subclass", "Period"))
Permutations_all <- rbind(Permutations_1st, Permutations_2nd, Permutations_3rd)
rm(Permutations_1st, Permutations_2nd, Permutations_3rd, historical_ai_1st, 
   historical_ai_2nd, historical_ai_3rd, historical_ai, Permutations_1st_completed_v2,
   Permutations_2nd_completed_v2, Permutations_3rd_completed_v2, scaffold_df, unique_groups)

Permutations_all_1000 <- Permutations_all[Permutations_all$permutation_number <= 1000,]

#create the overlapping variables
Threshold <- 1
Permutations_all_1000$Round_general <- ifelse(Permutations_all_1000$RCA_Gen < Threshold, 0, 1)
Permutations_all_1000$Round_AI <- ifelse(Permutations_all_1000$RCA < Threshold, 0, 1)
Permutations_all_1000$justGeneral <- ifelse(Permutations_all_1000$RCA_Gen >= Threshold & 
                                              Permutations_all_1000$RCA < Threshold, 1, 0)
Permutations_all_1000$justAI <- ifelse(Permutations_all_1000$RCA >= Threshold & 
                                         Permutations_all_1000$RCA_Gen < Threshold, 1, 0)
#fix Total_RCA:
Permutations_all_1000$Total_RCA <- Permutations_all_1000$Round_general + 2*Permutations_all_1000$Round_AI

test <-Permutations_all_1000[Permutations_all_1000$ctry_code == "JP" & Permutations_all_1000$permutation_number==0 &
                               Permutations_all_1000$Period == "1974-1988",]
# --- 1. Calculate Percentages ---
# Total number of unique technological fields (assuming it's fixed at 35)
all_subclasses <- 645

period_levels <- sort(unique(Permutations_all_1000$Period))
Permutations_all_1000 <- Permutations_all_1000 %>%
  mutate(Period = factor(Period, levels = period_levels, ordered = TRUE))

unique(Permutations_all_1000$Period)

#drop na cases
Permutations_all_1000 <- Permutations_all_1000[!is.na(Permutations_all_1000$ctry_code), ]

persistent_fields_df <- Permutations_all_1000 %>%
  # Group by the entities for which persistence is tracked over time
  group_by(permutation_number, ctry_code, Subclass) %>%
  arrange(Period) %>% # IMPORTANT: Ensure periods are sorted within each group
  mutate(
    # Was it coinciding in the PREVIOUS period?
    prev_coinciding = lag(Total_RCA == 3, default = FALSE), # default = FALSE for the first period
    # Is it coinciding NOW?
    current_coinciding = (Total_RCA == 3),
    # Is it persistently coinciding (now AND previously)?
    persistent_coinciding_field = current_coinciding & prev_coinciding,
    
    # Was it justGeneral in the PREVIOUS period?
    prev_justGeneral = lag(justGeneral == 1, default = FALSE),
    # Is it justGeneral NOW?
    current_justGeneral = (justGeneral == 1),
    # Is it persistently justGeneral?
    persistent_justGeneral_field = current_justGeneral & prev_justGeneral,
    
    # Was it justAI in the PREVIOUS period?
    prev_justAI = lag(justAI == 1, default = FALSE),
    # Is it justAI NOW?
    current_justAI = (justAI == 1),
    # Is it persistently justAI?
    persistent_justAI_field = current_justAI & prev_justAI,
    
    # Was it Round_AI in the PREVIOUS period?
    prev_Round_AI = lag(Round_AI == 1, default = FALSE),
    # Is it Round_AI NOW?
    current_Round_AI = (Round_AI == 1),
    # Is it persistently Round_AI?
    persistent_Round_AI_field = current_Round_AI & prev_Round_AI,
    
    # Was it coinciding and then justAI?
    com_AI_prev_coinciding = current_justAI & prev_coinciding,
    
    # Was it justAI and then coinciding?
    com_coinciding_prev_AI = current_coinciding & prev_justAI, #prev_justAI #prev_Round_AI
    
    # Was it justGeneral and then justAI?
    com_AI_prev_justGeneral = current_justAI & prev_justGeneral, 
    
    #sustains AI in core fields? 6,7,10,12
    sust_core_fields = current_Round_AI & prev_Round_AI &
      (Subclass %in% c("G01N", "G05B", "G06F","G06K","G06N","G06Q","G06T","G10L","H04L","A61B")), #c(6, 7, 10, 12)), #c(6, 7, 10, 12, 4, 5, 11)
    
    # sustains AI in NOT core fields? 
    sust_NOT_core_fields = current_Round_AI & prev_Round_AI &
      !(Subclass %in% c("G01N", "G05B", "G06F","G06K","G06N","G06Q","G06T","G10L","H04L","A61B")),
    
    # persistent coinciding in AI core fields? 
    persistent_coinciding_field_core_fields = current_coinciding & prev_coinciding &
      (Subclass %in% c("G01N", "G05B", "G06F","G06K","G06N","G06Q","G06T","G10L","H04L","A61B")),
    
    # persistent coinciding in NOT AI core fields? 
    persistent_coinciding_field_NOT_core_fields = current_coinciding & prev_coinciding &
      !(Subclass %in% c("G01N", "G05B", "G06F","G06K","G06N","G06Q","G06T","G10L","H04L","A61B"))
  ) %>%
  ungroup() # Ungroup before the next summary

# --- Step 2: Aggregate Counts and Calculate Original Shares ---
# Now, group by permutation, country, and Period to sum the persistent fields
# and also calculate the original metrics as before.

summary_and_persistence_df <- persistent_fields_df %>%
  group_by(permutation_number, ctry_code, Period) %>%
  summarise(
    # Original counts
    coinciding_fields_current = sum(current_coinciding, na.rm = TRUE),
    justGeneral_fields_current = sum(current_justGeneral, na.rm = TRUE),
    justAI_fields_current = sum(current_justAI, na.rm = TRUE),
    # Counts of fields that were specialized for at least 2 consecutive periods (ending in current period)
    n_persistent_coinciding = sum(persistent_coinciding_field, na.rm = TRUE),
    n_persistent_justGeneral = sum(persistent_justGeneral_field, na.rm = TRUE),
    n_persistent_justAI = sum(persistent_justAI_field, na.rm = TRUE),
    n_persistent_Round_AI = sum(persistent_Round_AI_field, na.rm = TRUE),
    n_AI_prev_coinciding = sum(com_AI_prev_coinciding, na.rm = TRUE),
    n_coinciding_prev_AI = sum(com_coinciding_prev_AI, na.rm = TRUE), 
    n_AI_prev_gen = sum(com_AI_prev_justGeneral, na.rm = TRUE),
    n_persistent_core_fields = sum(sust_core_fields, na.rm = TRUE),
    n_persistent_NOT_core_fields = sum(sust_NOT_core_fields, na.rm = TRUE), 
    n_persistent_coin_core_fields = sum(persistent_coinciding_field_core_fields, na.rm = TRUE),
    n_persistent_coin_NOT_core_fields = sum(persistent_coinciding_field_NOT_core_fields, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    # Shares for your original calculation based on new counts
    # Be careful with the denominator here:
    # If you want share of coinciding relative to coinciding + justGeneral for the *current* period:
    Share_coinciding_original_calc = if_else(
      (coinciding_fields_current + justGeneral_fields_current) > 0,
      coinciding_fields_current / (coinciding_fields_current + justGeneral_fields_current),
      0 # Or NA
    ),
    Share_justGeneral_absolute = justGeneral_fields_current / all_subclasses )

# Separate actual observed data (permutation_number == 0)
actual_shares <- summary_and_persistence_df %>%
  filter(permutation_number == 0) %>%
  select(ctry_code, Period, Actual_Share_coinciding = Share_coinciding_original_calc,
         Actual_persistent_coinciding = n_persistent_coinciding,
         Actual_persistent_justGeneral = n_persistent_justGeneral,
         Actual_persistent_justAI = n_persistent_justAI,
         Actual_persistent_Round_AI = n_persistent_Round_AI,
         Actual_com_AI_prev_coinciding = n_AI_prev_coinciding,
         Actual_com_coinciding_prev_AI = n_coinciding_prev_AI,
         Actual_com_AI_prev_gen = n_AI_prev_gen,
         Actual_persistent_core = n_persistent_core_fields,
         Actual_persistent_NOT_core = n_persistent_NOT_core_fields,
         Actual_persistent_coin_core = n_persistent_coin_core_fields,
         Actual_persistent_coin_NOT_core = n_persistent_coin_NOT_core_fields) 

# Permuted data (permutation_number > 0)
permuted_shares <- summary_and_persistence_df %>%
  filter(permutation_number > 0)

# --- 2. Summarize Permuted Data ---
# Calculate mean and 95% confidence interval (2.5th and 97.5th percentiles)
# for the share of coinciding specializations from permutations
permuted_summary <- permuted_shares %>%
  group_by(ctry_code, Period) %>%
  summarise(
    Mean_Permuted_Share = mean(Share_coinciding_original_calc, na.rm = TRUE),
    Lower_CI_Permuted = quantile(Share_coinciding_original_calc, probs = 0.025, na.rm = TRUE),
    Upper_CI_Permuted = quantile(Share_coinciding_original_calc, probs = 0.975, na.rm = TRUE),
    
    Mean_Permuted_Share_sust_coinc = mean(n_persistent_coinciding, na.rm = TRUE),
    Lower_CI_Permuted_sust_coinc = quantile(n_persistent_coinciding, probs = 0.025, na.rm = TRUE),
    Upper_CI_Permuted_sust_coinc = quantile(n_persistent_coinciding, probs = 0.975, na.rm = TRUE),
    
    Mean_Permuted_Share_gen_coinc = mean(n_persistent_justGeneral, na.rm = TRUE),
    Lower_CI_Permuted_gen_coinc = quantile(n_persistent_justGeneral, probs = 0.025, na.rm = TRUE),
    Upper_CI_Permuted_gen_coinc = quantile(n_persistent_justGeneral, probs = 0.975, na.rm = TRUE),
    
    Mean_Permuted_Share_ai_coinc = mean(n_persistent_justAI, na.rm = TRUE),
    Lower_CI_Permuted_ai_coinc = quantile(n_persistent_justAI, probs = 0.025, na.rm = TRUE),
    Upper_CI_Permuted_ai_coinc = quantile(n_persistent_justAI, probs = 0.975, na.rm = TRUE),
    
    Mean_Permuted_Share_ai_excl_coinc = mean(n_persistent_Round_AI, na.rm = TRUE),
    Lower_CI_Permuted_ai_excl_coinc = quantile(n_persistent_Round_AI, probs = 0.025, na.rm = TRUE),
    Upper_CI_Permuted_ai_excl_coinc = quantile(n_persistent_Round_AI, probs = 0.975, na.rm = TRUE),
    
    Mean_Permuted_Share_AI_prev_coinciding = mean(n_AI_prev_coinciding, na.rm = TRUE),
    Lower_CI_Permuted_AI_prev_coinciding = quantile(n_AI_prev_coinciding, probs = 0.025, na.rm = TRUE),
    Upper_CI_Permuted_AI_prev_coinciding = quantile(n_AI_prev_coinciding, probs = 0.975, na.rm = TRUE),
    
    Mean_Permuted_Share_coinciding_prev_AI = mean(n_coinciding_prev_AI, na.rm = TRUE),
    Lower_CI_Permuted_coinciding_prev_AI = quantile(n_coinciding_prev_AI, probs = 0.025, na.rm = TRUE),
    Upper_CI_Permuted_coinciding_prev_AI = quantile(n_coinciding_prev_AI, probs = 0.975, na.rm = TRUE),
    
    Mean_Permuted_Share_AI_prev_gen = mean(n_AI_prev_gen, na.rm = TRUE),
    Lower_CI_Permuted_AI_prev_gen = quantile(n_AI_prev_gen, probs = 0.025, na.rm = TRUE),
    Upper_CI_Permuted_AI_prev_gen = quantile(n_AI_prev_gen, probs = 0.975, na.rm = TRUE),
    
    Mean_Permuted_core_fields = mean(n_persistent_core_fields, na.rm = TRUE),
    Lower_CI_core_fields = quantile(n_persistent_core_fields, probs = 0.025, na.rm = TRUE),
    Upper_CI_core_fields = quantile(n_persistent_core_fields, probs = 0.975, na.rm = TRUE),
    
    Mean_Permuted_NOT_core_fields = mean(n_persistent_NOT_core_fields, na.rm = TRUE),
    Lower_CI_NOT_core_fields = quantile(n_persistent_NOT_core_fields, probs = 0.025, na.rm = TRUE),
    Upper_CI_NOT_core_fields = quantile(n_persistent_NOT_core_fields, probs = 0.975, na.rm = TRUE),
    
    Mean_Permuted_coin_core_fields = mean(n_persistent_coin_core_fields, na.rm = TRUE),
    Lower_CI_coin_core_fields = quantile(n_persistent_coin_core_fields, probs = 0.025, na.rm = TRUE),
    Upper_CI_coin_core_fields = quantile(n_persistent_coin_core_fields, probs = 0.975, na.rm = TRUE),
    
    Mean_Permuted_coin_NOT_core_fields = mean(n_persistent_coin_NOT_core_fields, na.rm = TRUE),
    Lower_CI_coin_NOT_core_fields = quantile(n_persistent_coin_NOT_core_fields, probs = 0.025, na.rm = TRUE),
    Upper_CI_coin_NOT_core_fields = quantile(n_persistent_coin_NOT_core_fields, probs = 0.975, na.rm = TRUE),
    
    .groups = 'drop'
  )

# --- 3. Combine Data for Plotting ---
plot_data <- actual_shares %>%
  left_join(permuted_summary, by = c("ctry_code", "Period")) %>%
  # Ensure Period is an ordered factor for plotting if it's not already
  mutate(Period = factor(Period, levels = unique(Permutations_all_1000$Period), ordered = TRUE),
         Country = factor(ctry_code)) # Use 'Country' for consistency with your example plot

# --- 4. Plot ---
# Define the same color palette you used
plot_data$Country <- gsub("US", "USA", str_trim(plot_data$Country))
plot_data$Country <- gsub("CN", "China", str_trim(plot_data$Country))
plot_data$Country <- gsub("JP", "Japan", str_trim(plot_data$Country))
plot_data$Country <- gsub("KR", "South Korea", str_trim(plot_data$Country))

country_colors <- c("China" = "#1B9E77", "Japan" = "#D95F02", "South Korea" = "#7570B3", "USA" = "#E7298A")
country_shapes <- c("China" = 21, "Japan" = 22, "South Korea" = 24, "USA" = 23) # Assuming ctry_code maps to these

#order country levels
plot_data$Country <- factor(plot_data$Country, levels = c("Japan", "USA","South Korea","China"))

# Create the plot, faceted by Country
significance_plot_save <- 
  ggplot(plot_data, aes(x = Period, group = Country)) +
  # Ribbon for permuted confidence interval
  geom_ribbon(aes(ymin = Lower_CI_Permuted, ymax = Upper_CI_Permuted, fill = Country), alpha = 0.2, show.legend = FALSE) +
  # Line for the mean of permuted shares
  geom_line(aes(y = Mean_Permuted_Share, color = Country), linetype = "dotted", size = 1, show.legend = FALSE) +
  # Line for actual observed shares
  geom_line(aes(y = Actual_Share_coinciding, color = Country), linetype = "dashed", size = 1.5) +
  # Points for actual observed shares
  geom_point(aes(y = Actual_Share_coinciding, shape = Country, fill = Country, color = Country), size = 5) + # Color for border
  
  scale_shape_manual(values = country_shapes, name = "Country") +
  scale_fill_manual(values = country_colors, name = "Country", guide = "legend") + # Ensure fill legend is shown
  scale_color_manual(values = country_colors, name = "Country", guide = "legend") + # Ensure color legend is shown
  xlab("Interval") +
  ylab("Share of break-in specialisations (%)") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + # Use percent_format
  theme_classic(base_size = 14) +
  theme(
    strip.background = element_blank(), # Optional: remove facet strip background
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "top", # Or "bottom", "right", "left"
    axis.text.x = element_text(angle = 45, hjust = 1) # Rotate x-axis labels if needed
  ) +
  facet_wrap(~ Country, scales = "free_y", ncol = 4) + theme(legend.position="none") + 
  ggtitle(paste0("a) Subclasses - 15-year Intervals"))

####2.2.4.2. Intervals 5-years ------
#rm(list=ls())
rm(list = setdiff(ls(), "significance_plot_save"))

#read permutations
#1st period
Permutations_1st <- read.csv("Files_created_with_the_code/data/files_code_4-digits_analysis/robustness/final_rca_all_permutations_df_5y_1st_Period.csv", 
                             sep = ";", dec = ",", header = TRUE)
Permutations_1st$Period <- "1974-1978"
Permutations_1st <- Permutations_1st[Permutations_1st$ctry_code == "JP"|Permutations_1st$ctry_code == "US"|
                                       Permutations_1st$ctry_code == "CN"|Permutations_1st$ctry_code == "KR",]
#2nd period
Permutations_2nd <- read.csv("Files_created_with_the_code/data/files_code_4-digits_analysis/robustness/final_rca_all_permutations_df_5y_2nd_Period.csv", 
                             sep = ";", dec = ",", header = TRUE)
Permutations_2nd$Period <- "1979-1983"
Permutations_2nd <- Permutations_2nd[Permutations_2nd$ctry_code == "JP"|Permutations_2nd$ctry_code == "US"|
                                       Permutations_2nd$ctry_code == "CN"|Permutations_2nd$ctry_code == "KR",]
#3rd period
Permutations_3rd <- read.csv("Files_created_with_the_code/data/files_code_4-digits_analysis/robustness/final_rca_all_permutations_df_5y_3rd_Period.csv", 
                             sep = ";", dec = ",", header = TRUE)
Permutations_3rd$Period <- "1984-1988"
Permutations_3rd <- Permutations_3rd[Permutations_3rd$ctry_code == "JP"|Permutations_3rd$ctry_code == "US"|
                                       Permutations_3rd$ctry_code == "CN"|Permutations_3rd$ctry_code == "KR",]
#4th period
Permutations_4th <- read.csv("Files_created_with_the_code/data/files_code_4-digits_analysis/robustness/final_rca_all_permutations_df_5y_4th_Period.csv", 
                             sep = ";", dec = ",", header = TRUE)
Permutations_4th$Period <- "1989-1993"
Permutations_4th <- Permutations_4th[Permutations_4th$ctry_code == "JP"|Permutations_4th$ctry_code == "US"|
                                       Permutations_4th$ctry_code == "CN"|Permutations_4th$ctry_code == "KR",]

#5th period
Permutations_5th <- read.csv("Files_created_with_the_code/data/files_code_4-digits_analysis/robustness/final_rca_all_permutations_df_5y_5th_Period.csv", 
                             sep = ";", dec = ",", header = TRUE)
Permutations_5th$Period <- "1994-1998"
Permutations_5th <- Permutations_5th[Permutations_5th$ctry_code == "JP"|Permutations_5th$ctry_code == "US"|
                                       Permutations_5th$ctry_code == "CN"|Permutations_5th$ctry_code == "KR",]

#6th period
Permutations_6th <- read.csv("Files_created_with_the_code/data/files_code_4-digits_analysis/robustness/final_rca_all_permutations_df_5y_6th_Period.csv", 
                             sep = ";", dec = ",", header = TRUE)
Permutations_6th$Period <- "1999-2003"
Permutations_6th <- Permutations_6th[Permutations_6th$ctry_code == "JP"|Permutations_6th$ctry_code == "US"|
                                       Permutations_6th$ctry_code == "CN"|Permutations_6th$ctry_code == "KR",]

#7th period
Permutations_7th <- read.csv("Files_created_with_the_code/data/files_code_4-digits_analysis/robustness/final_rca_all_permutations_df_5y_7th_Period.csv", 
                             sep = ";", dec = ",", header = TRUE)
Permutations_7th$Period <- "2004-2008"
Permutations_7th <- Permutations_7th[Permutations_7th$ctry_code == "JP"|Permutations_7th$ctry_code == "US"|
                                       Permutations_7th$ctry_code == "CN"|Permutations_7th$ctry_code == "KR",]

#8th period
Permutations_8th <- read.csv("Files_created_with_the_code/data/files_code_4-digits_analysis/robustness/final_rca_all_permutations_df_5y_8th_Period.csv", 
                             sep = ";", dec = ",", header = TRUE)
Permutations_8th$Period <- "2009-2013"
Permutations_8th <- Permutations_8th[Permutations_8th$ctry_code == "JP"|Permutations_8th$ctry_code == "US"|
                                       Permutations_8th$ctry_code == "CN"|Permutations_8th$ctry_code == "KR",]

#9th period
Permutations_9th <- read.csv("Files_created_with_the_code/data/files_code_4-digits_analysis/robustness/final_rca_all_permutations_df_5y_9th_Period.csv", 
                             sep = ";", dec = ",", header = TRUE)
Permutations_9th$Period <- "2014-2018"
Permutations_9th <- Permutations_9th[Permutations_9th$ctry_code == "JP"|Permutations_9th$ctry_code == "US"|
                                       Permutations_9th$ctry_code == "CN"|Permutations_9th$ctry_code == "KR",]

Subclasses_1st <- read.csv("Files_created_with_the_code/data/files_code_4-digits_analysis/robustness/final_rca_all_permutations_df_1st_Period.csv", 
                           sep = ";", dec = ",", header = TRUE)
Subclasses_codes_1st <- unique(Subclasses_1st$Subclass)

Subclasses_2nd <- read.csv("Files_created_with_the_code/data/files_code_4-digits_analysis/robustness/final_rca_all_permutations_df_2nd_Period.csv", 
                           sep = ";", dec = ",", header = TRUE)
Subclasses_codes_2nd <- unique(Subclasses_2nd$Subclass)
rm(Subclasses_2nd, Subclasses_1st)

Subclasses_3rd <- read.csv("Files_created_with_the_code/data/files_code_4-digits_analysis/robustness/final_rca_all_permutations_df_3rd_Period.csv", 
                           sep = ";", dec = ",", header = TRUE)
Subclasses_codes_3rd <- unique(Subclasses_3rd$Subclass)
rm(Subclasses_3rd)

Subclasses_codes_1st <- as.data.frame(Subclasses_codes_1st)
Subclasses_codes_2nd <- as.data.frame(Subclasses_codes_2nd)
Subclasses_codes_3rd <- as.data.frame(Subclasses_codes_3rd)
names(Subclasses_codes_1st)<- "Subclass"
names(Subclasses_codes_2nd)<- "Subclass"
names(Subclasses_codes_3rd)<- "Subclass"

Subclasses <- rbind(Subclasses_codes_1st, Subclasses_codes_2nd, Subclasses_codes_3rd)
Subclasses <- unique(Subclasses$Subclass)
#exclude the ones that don't exist
Subclasses <- Subclasses[!Subclasses %in% c("B33Y", "C13C", "C13H", "C99Z", "G16H", "G99Z", "A99Z", "B99Z", "D99Z", "E99Z", "F99Z", "H99Z")]
#from 646 to 637
all_subclasses <- Subclasses
rm(Subclasses_codes_1st, Subclasses_codes_2nd, Subclasses_codes_3rd, Subclasses)
all_periods  <- c("1974-1978",
                  "1979-1983",
                  "1984-1988",
                  "1989-1993",
                  "1994-1998",
                  "1999-2003",
                  "2004-2008",
                  "2009-2013",
                  "2014-2018")
all_ctry_codes <- c("CN", "JP", "KR", "US") # Define ALL countries you want in the final dataset


all_permutation_numbers <- unique(Permutations_1st$permutation_number)
if (length(all_permutation_numbers) == 0) { # Handle case where Permutations_1st might be empty
  all_permutation_numbers <- 0 # Or your default/expected range, e.g., 0:1000
}

scaffold_groups <- expand.grid(
  ctry_code = all_ctry_codes,
  permutation_number = all_permutation_numbers,
  Period = all_periods,
  stringsAsFactors = FALSE # Important for character vectors
) %>% as_tibble()

# --- Now expand this group scaffold by all technological fields ---
scaffold_df <- scaffold_groups %>%
  expand(nesting(ctry_code, permutation_number, Period), # Uses ALL defined combinations
         Subclass = all_subclasses) #Subclass = all_subclasses #techn_field_nr


# 2. Left join your original data to this scaffold
Permutations_1st_completed_v2 <- left_join(
  scaffold_df,
  Permutations_1st,
  by = c("ctry_code", "permutation_number", "Period", "Subclass")
)

# 3. Fill NA values in RCA with 0 (for the newly created rows)
Permutations_1st_completed_v2 <- Permutations_1st_completed_v2 %>%
  mutate(RCA = ifelse(is.na(RCA), 0, RCA)) %>%
  arrange(permutation_number, ctry_code, Period, Subclass)

#for the second period:
scaffold_groups <- expand.grid(
  ctry_code = all_ctry_codes,
  permutation_number = all_permutation_numbers,
  Period = all_periods,
  stringsAsFactors = FALSE # Important for character vectors
) %>% as_tibble()

# --- Now expand this group scaffold by all technological fields ---
scaffold_df <- scaffold_groups %>%
  expand(nesting(ctry_code, permutation_number, Period), # Uses ALL defined combinations
         Subclass = all_subclasses)
# 2. Left join your original data to this scaffold
Permutations_2nd_completed_v2 <- left_join(
  scaffold_df,
  Permutations_2nd,
  by = c("ctry_code", "permutation_number", "Period", "Subclass")
)

# 3. Fill NA values in RCA with 0 (for the newly created rows)
Permutations_2nd_completed_v2 <- Permutations_2nd_completed_v2 %>%
  mutate(RCA = ifelse(is.na(RCA), 0, RCA)) %>%
  arrange(permutation_number, ctry_code, Period, Subclass)

#for the third period:
scaffold_groups <- expand.grid(
  ctry_code = all_ctry_codes,
  permutation_number = all_permutation_numbers,
  Period = all_periods,
  stringsAsFactors = FALSE # Important for character vectors
) %>% as_tibble()

# --- Now expand this group scaffold by all technological fields ---
scaffold_df <- scaffold_groups %>%
  expand(nesting(ctry_code, permutation_number, Period), # Uses ALL defined combinations
         Subclass = all_subclasses)

# 2. Left join your original data to this scaffold
Permutations_3rd_completed_v2 <- left_join(
  scaffold_df,
  Permutations_3rd,
  by = c("ctry_code", "permutation_number", "Period", "Subclass")
)

# 3. Fill NA values in RCA with 0 (for the newly created rows)
Permutations_3rd_completed_v2 <- Permutations_3rd_completed_v2 %>%
  mutate(RCA = ifelse(is.na(RCA), 0, RCA)) %>%
  arrange(permutation_number, ctry_code, Period, Subclass)

#for the fourth period:
scaffold_groups <- expand.grid(
  ctry_code = all_ctry_codes,
  permutation_number = all_permutation_numbers,
  Period = all_periods,
  stringsAsFactors = FALSE # Important for character vectors
) %>% as_tibble()

# --- Now expand this group scaffold by all technological fields ---
scaffold_df <- scaffold_groups %>%
  expand(nesting(ctry_code, permutation_number, Period), # Uses ALL defined combinations
         Subclass = all_subclasses)

# 2. Left join your original data to this scaffold
Permutations_4th_completed_v2 <- left_join(
  scaffold_df,
  Permutations_4th,
  by = c("ctry_code", "permutation_number", "Period", "Subclass")
)

# 3. Fill NA values in RCA with 0 (for the newly created rows)
Permutations_4th_completed_v2 <- Permutations_4th_completed_v2 %>%
  mutate(RCA = ifelse(is.na(RCA), 0, RCA)) %>%
  arrange(permutation_number, ctry_code, Period, Subclass)

#for the fifth period:
scaffold_groups <- expand.grid(
  ctry_code = all_ctry_codes,
  permutation_number = all_permutation_numbers,
  Period = all_periods,
  stringsAsFactors = FALSE # Important for character vectors
) %>% as_tibble()

# --- Now expand this group scaffold by all technological fields ---
scaffold_df <- scaffold_groups %>%
  expand(nesting(ctry_code, permutation_number, Period), # Uses ALL defined combinations
         Subclass = all_subclasses)

# 2. Left join your original data to this scaffold
Permutations_5th_completed_v2 <- left_join(
  scaffold_df,
  Permutations_5th,
  by = c("ctry_code", "permutation_number", "Period", "Subclass")
)

# 3. Fill NA values in RCA with 0 (for the newly created rows)
Permutations_5th_completed_v2 <- Permutations_5th_completed_v2 %>%
  mutate(RCA = ifelse(is.na(RCA), 0, RCA)) %>%
  arrange(permutation_number, ctry_code, Period, Subclass)

#for the sixth period:
scaffold_groups <- expand.grid(
  ctry_code = all_ctry_codes,
  permutation_number = all_permutation_numbers,
  Period = all_periods,
  stringsAsFactors = FALSE # Important for character vectors
) %>% as_tibble()

# --- Now expand this group scaffold by all technological fields ---
scaffold_df <- scaffold_groups %>%
  expand(nesting(ctry_code, permutation_number, Period), # Uses ALL defined combinations
         Subclass = all_subclasses)

# 2. Left join your original data to this scaffold
Permutations_6th_completed_v2 <- left_join(
  scaffold_df,
  Permutations_6th,
  by = c("ctry_code", "permutation_number", "Period", "Subclass")
)

# 3. Fill NA values in RCA with 0 (for the newly created rows)
Permutations_6th_completed_v2 <- Permutations_6th_completed_v2 %>%
  mutate(RCA = ifelse(is.na(RCA), 0, RCA)) %>%
  arrange(permutation_number, ctry_code, Period, Subclass)

#for the seventh period:
scaffold_groups <- expand.grid(
  ctry_code = all_ctry_codes,
  permutation_number = all_permutation_numbers,
  Period = all_periods,
  stringsAsFactors = FALSE # Important for character vectors
) %>% as_tibble()

# --- Now expand this group scaffold by all technological fields ---
scaffold_df <- scaffold_groups %>%
  expand(nesting(ctry_code, permutation_number, Period), # Uses ALL defined combinations
         Subclass = all_subclasses)

# 2. Left join your original data to this scaffold
Permutations_7th_completed_v2 <- left_join(
  scaffold_df,
  Permutations_7th,
  by = c("ctry_code", "permutation_number", "Period", "Subclass")
)

# 3. Fill NA values in RCA with 0 (for the newly created rows)
Permutations_7th_completed_v2 <- Permutations_7th_completed_v2 %>%
  mutate(RCA = ifelse(is.na(RCA), 0, RCA)) %>%
  arrange(permutation_number, ctry_code, Period, Subclass)

#for the eigth period:
scaffold_groups <- expand.grid(
  ctry_code = all_ctry_codes,
  permutation_number = all_permutation_numbers,
  Period = all_periods,
  stringsAsFactors = FALSE # Important for character vectors
) %>% as_tibble()

# --- Now expand this group scaffold by all technological fields ---
scaffold_df <- scaffold_groups %>%
  expand(nesting(ctry_code, permutation_number, Period), # Uses ALL defined combinations
         Subclass = all_subclasses)

# 2. Left join your original data to this scaffold
Permutations_8th_completed_v2 <- left_join(
  scaffold_df,
  Permutations_8th,
  by = c("ctry_code", "permutation_number", "Period", "Subclass")
)

# 3. Fill NA values in RCA with 0 (for the newly created rows)
Permutations_8th_completed_v2 <- Permutations_8th_completed_v2 %>%
  mutate(RCA = ifelse(is.na(RCA), 0, RCA)) %>%
  arrange(permutation_number, ctry_code, Period, Subclass)

#for the nineth period:
scaffold_groups <- expand.grid(
  ctry_code = all_ctry_codes,
  permutation_number = all_permutation_numbers,
  Period = all_periods,
  stringsAsFactors = FALSE # Important for character vectors
) %>% as_tibble()

# --- Now expand this group scaffold by all technological fields ---
scaffold_df <- scaffold_groups %>%
  expand(nesting(ctry_code, permutation_number, Period), # Uses ALL defined combinations
         Subclass = all_subclasses)

# 2. Left join your original data to this scaffold
Permutations_9th_completed_v2 <- left_join(
  scaffold_df,
  Permutations_9th,
  by = c("ctry_code", "permutation_number", "Period", "Subclass")
)

# 3. Fill NA values in RCA with 0 (for the newly created rows)
Permutations_9th_completed_v2 <- Permutations_9th_completed_v2 %>%
  mutate(RCA = ifelse(is.na(RCA), 0, RCA)) %>%
  arrange(permutation_number, ctry_code, Period, Subclass)

#historical data
Robustness_data_RTA_5_years <- read.csv("Files_created_with_the_code/data/files_code_4-digits_analysis/IPC_RCAs_subclass_5y.csv", 
                                        sep = ";", header = TRUE, dec=",")
# Replace NA with 0
Robustness_data_RTA_5_years[is.na(Robustness_data_RTA_5_years)] <- 0

historical_ai_1st <- Robustness_data_RTA_5_years[Robustness_data_RTA_5_years$Period == "1974-1978",]
historical_ai_1st <- historical_ai_1st[historical_ai_1st$ctry_code == "JP"|historical_ai_1st$ctry_code == "US"|
                                         historical_ai_1st$ctry_code == "CN"|historical_ai_1st$ctry_code == "KR",]
sum(historical_ai_1st$RCA_AI) #12
Reference_1st <- Permutations_1st[Permutations_1st$permutation_number == 0,]
sum(Reference_1st$RCA) #12

historical_ai_2nd <- Robustness_data_RTA_5_years[Robustness_data_RTA_5_years$Period == "1979-1983",]
historical_ai_2nd <- historical_ai_2nd[historical_ai_2nd$ctry_code == "JP"|historical_ai_2nd$ctry_code == "US"|
                                         historical_ai_2nd$ctry_code == "CN"|historical_ai_2nd$ctry_code == "KR",]
sum(historical_ai_2nd$RCA_AI) #11.25
Reference_2nd <- Permutations_2nd[Permutations_2nd$permutation_number == 0,]
sum(Reference_2nd$RCA) #11.25

historical_ai_3rd <- Robustness_data_RTA_5_years[Robustness_data_RTA_5_years$Period == "1984-1988",]
historical_ai_3rd <- historical_ai_3rd[historical_ai_3rd$ctry_code == "JP"|historical_ai_3rd$ctry_code == "US"|
                                         historical_ai_3rd$ctry_code == "CN"|historical_ai_3rd$ctry_code == "KR",]
sum(historical_ai_3rd$RCA_AI) #82.93733
Reference_3rd <- Permutations_3rd[Permutations_3rd$permutation_number == 0,]
sum(Reference_3rd$RCA) #82.93733
#all correct!
rm(Reference_1st,Reference_2nd,Reference_3rd)

historical_ai_4th <- Robustness_data_RTA_5_years[Robustness_data_RTA_5_years$Period == "1989-1993",]
historical_ai_4th <- historical_ai_4th[historical_ai_4th$ctry_code == "JP"|historical_ai_4th$ctry_code == "US"|
                                         historical_ai_4th$ctry_code == "CN"|historical_ai_4th$ctry_code == "KR",]

historical_ai_5th <- Robustness_data_RTA_5_years[Robustness_data_RTA_5_years$Period == "1994-1998",]
historical_ai_5th <- historical_ai_5th[historical_ai_5th$ctry_code == "JP"|historical_ai_5th$ctry_code == "US"|
                                         historical_ai_5th$ctry_code == "CN"|historical_ai_5th$ctry_code == "KR",]

historical_ai_6th <- Robustness_data_RTA_5_years[Robustness_data_RTA_5_years$Period == "1999-2003",]
historical_ai_6th <- historical_ai_6th[historical_ai_6th$ctry_code == "JP"|historical_ai_6th$ctry_code == "US"|
                                         historical_ai_6th$ctry_code == "CN"|historical_ai_6th$ctry_code == "KR",]

historical_ai_7th <- Robustness_data_RTA_5_years[Robustness_data_RTA_5_years$Period == "2004-2008",]
historical_ai_7th <- historical_ai_7th[historical_ai_7th$ctry_code == "JP"|historical_ai_7th$ctry_code == "US"|
                                         historical_ai_7th$ctry_code == "CN"|historical_ai_7th$ctry_code == "KR",]

historical_ai_8th <- Robustness_data_RTA_5_years[Robustness_data_RTA_5_years$Period == "2009-2013",]
historical_ai_8th <- historical_ai_8th[historical_ai_8th$ctry_code == "JP"|historical_ai_8th$ctry_code == "US"|
                                         historical_ai_8th$ctry_code == "CN"|historical_ai_8th$ctry_code == "KR",]

historical_ai_9th <- Robustness_data_RTA_5_years[Robustness_data_RTA_5_years$Period == "2014-2018",]
historical_ai_9th <- historical_ai_9th[historical_ai_9th$ctry_code == "JP"|historical_ai_9th$ctry_code == "US"|
                                         historical_ai_9th$ctry_code == "CN"|historical_ai_9th$ctry_code == "KR",]

#now, left_join everything
Permutations_1st <- left_join(historical_ai_1st, Permutations_1st_completed_v2, 
                              by = c("ctry_code","Subclass", "Period"))
Permutations_2nd <- left_join(historical_ai_2nd, Permutations_2nd_completed_v2, 
                              by = c("ctry_code","Subclass", "Period"))

Permutations_3rd <- left_join(historical_ai_3rd, Permutations_3rd_completed_v2, 
                              by = c("ctry_code","Subclass", "Period"))

Permutations_4th <- left_join(historical_ai_4th, Permutations_4th_completed_v2, 
                              by = c("ctry_code","Subclass", "Period"))

Permutations_5th <- left_join(historical_ai_5th, Permutations_5th_completed_v2, 
                              by = c("ctry_code","Subclass", "Period"))

Permutations_6th <- left_join(historical_ai_6th, Permutations_6th_completed_v2, 
                              by = c("ctry_code","Subclass", "Period"))

Permutations_7th <- left_join(historical_ai_7th, Permutations_7th_completed_v2, 
                              by = c("ctry_code","Subclass", "Period"))

Permutations_8th <- left_join(historical_ai_8th, Permutations_8th_completed_v2, 
                              by = c("ctry_code","Subclass", "Period"))

Permutations_9th <- left_join(historical_ai_9th, Permutations_9th_completed_v2, 
                              by = c("ctry_code","Subclass", "Period"))

Permutations_all <- rbind(Permutations_1st, Permutations_2nd, Permutations_3rd,Permutations_4th,Permutations_5th,
                          Permutations_6th,Permutations_7th,Permutations_8th,Permutations_9th)

rm(Permutations_1st, Permutations_2nd, Permutations_3rd,Permutations_4th,Permutations_5th,
   Permutations_6th,Permutations_7th,Permutations_8th,Permutations_9th,
   historical_ai_1st, historical_ai_2nd, historical_ai_3rd, historical_ai_4th,historical_ai_5th,
   historical_ai_6th,historical_ai_7th,historical_ai_8th,historical_ai_9th,
   Robustness_data_RTA_5_years, Permutations_1st_completed_v2, Permutations_2nd_completed_v2, 
   Permutations_3rd_completed_v2, Permutations_4th_completed_v2,Permutations_5th_completed_v2,
   Permutations_6th_completed_v2,Permutations_7th_completed_v2,Permutations_8th_completed_v2,
   Permutations_9th_completed_v2,scaffold_df, unique_groups)

Permutations_all_1000 <- Permutations_all[Permutations_all$permutation_number <= 1000,]

#create the overlapping variables
Threshold <- 1
Permutations_all_1000$Round_general <- ifelse(Permutations_all_1000$RCA_Gen < Threshold, 0, 1)
Permutations_all_1000$Round_AI <- ifelse(Permutations_all_1000$RCA < Threshold, 0, 1)
Permutations_all_1000$justGeneral <- ifelse(Permutations_all_1000$RCA_Gen >= Threshold & 
                                              Permutations_all_1000$RCA < Threshold, 1, 0)
Permutations_all_1000$justAI <- ifelse(Permutations_all_1000$RCA >= Threshold & 
                                         Permutations_all_1000$RCA_Gen < Threshold, 1, 0)
#fix Total_RCA:
Permutations_all_1000$Total_RCA <- Permutations_all_1000$Round_general + 2*Permutations_all_1000$Round_AI

test <-Permutations_all_1000[Permutations_all_1000$ctry_code == "JP" & Permutations_all_1000$permutation_number==0 &
                               Permutations_all_1000$Period == "1974-1978",]
# --- 1. Calculate Percentages ---
# Total number of unique technological fields (assuming it's fixed at 35)
all_subclasses <- 645

period_levels <- sort(unique(Permutations_all_1000$Period))
Permutations_all_1000 <- Permutations_all_1000 %>%
  mutate(Period = factor(Period, levels = period_levels, ordered = TRUE))

unique(Permutations_all_1000$Period)

#drop na cases
Permutations_all_1000 <- Permutations_all_1000[!is.na(Permutations_all_1000$ctry_code), ]

persistent_fields_df <- Permutations_all_1000 %>%
  # Group by the entities for which persistence is tracked over time
  group_by(permutation_number, ctry_code, Subclass) %>%
  arrange(Period) %>% # IMPORTANT: Ensure periods are sorted within each group
  mutate(
    # Was it coinciding in the PREVIOUS period?
    prev_coinciding = lag(Total_RCA == 3, default = FALSE), # default = FALSE for the first period
    # Is it coinciding NOW?
    current_coinciding = (Total_RCA == 3),
    # Is it persistently coinciding (now AND previously)?
    persistent_coinciding_field = current_coinciding & prev_coinciding,
    
    # Was it justGeneral in the PREVIOUS period?
    prev_justGeneral = lag(justGeneral == 1, default = FALSE),
    # Is it justGeneral NOW?
    current_justGeneral = (justGeneral == 1),
    # Is it persistently justGeneral?
    persistent_justGeneral_field = current_justGeneral & prev_justGeneral,
    
    # Was it justAI in the PREVIOUS period?
    prev_justAI = lag(justAI == 1, default = FALSE),
    # Is it justAI NOW?
    current_justAI = (justAI == 1),
    # Is it persistently justAI?
    persistent_justAI_field = current_justAI & prev_justAI,
    
    # Was it Round_AI in the PREVIOUS period?
    prev_Round_AI = lag(Round_AI == 1, default = FALSE),
    # Is it Round_AI NOW?
    current_Round_AI = (Round_AI == 1),
    # Is it persistently Round_AI?
    persistent_Round_AI_field = current_Round_AI & prev_Round_AI,
    
    # Was it coinciding and then justAI?
    com_AI_prev_coinciding = current_justAI & prev_coinciding,
    
    # Was it justAI and then coinciding?
    com_coinciding_prev_AI = current_coinciding & prev_justAI, #prev_justAI #prev_Round_AI
    
    # Was it justGeneral and then justAI?
    com_AI_prev_justGeneral = current_justAI & prev_justGeneral, 
    
    #sustains AI in core fields? 6,7,10,12
    sust_core_fields = current_Round_AI & prev_Round_AI &
      (Subclass %in% c("G01N", "G05B", "G06F","G06K","G06N","G06Q","G06T","G10L","H04L","A61B")), #c(6, 7, 10, 12)), #c(6, 7, 10, 12, 4, 5, 11)
    
    # sustains AI in NOT core fields? 
    sust_NOT_core_fields = current_Round_AI & prev_Round_AI &
      !(Subclass %in% c("G01N", "G05B", "G06F","G06K","G06N","G06Q","G06T","G10L","H04L","A61B")),
    
    # persistent coinciding in AI core fields? 
    persistent_coinciding_field_core_fields = current_coinciding & prev_coinciding &
      (Subclass %in% c("G01N", "G05B", "G06F","G06K","G06N","G06Q","G06T","G10L","H04L","A61B")),
    
    # persistent coinciding in NOT AI core fields? 
    persistent_coinciding_field_NOT_core_fields = current_coinciding & prev_coinciding &
      !(Subclass %in% c("G01N", "G05B", "G06F","G06K","G06N","G06Q","G06T","G10L","H04L","A61B"))
  ) %>%
  ungroup() # Ungroup before the next summary

# --- Step 2: Aggregate Counts and Calculate Original Shares ---
# Now, group by permutation, country, and Period to sum the persistent fields
# and also calculate the original metrics as before.

summary_and_persistence_df <- persistent_fields_df %>%
  group_by(permutation_number, ctry_code, Period) %>%
  summarise(
    # Original counts
    coinciding_fields_current = sum(current_coinciding, na.rm = TRUE),
    justGeneral_fields_current = sum(current_justGeneral, na.rm = TRUE),
    justAI_fields_current = sum(current_justAI, na.rm = TRUE),
    # Counts of fields that were specialized for at least 2 consecutive periods (ending in current period)
    n_persistent_coinciding = sum(persistent_coinciding_field, na.rm = TRUE),
    n_persistent_justGeneral = sum(persistent_justGeneral_field, na.rm = TRUE),
    n_persistent_justAI = sum(persistent_justAI_field, na.rm = TRUE),
    n_persistent_Round_AI = sum(persistent_Round_AI_field, na.rm = TRUE),
    n_AI_prev_coinciding = sum(com_AI_prev_coinciding, na.rm = TRUE),
    n_coinciding_prev_AI = sum(com_coinciding_prev_AI, na.rm = TRUE), 
    n_AI_prev_gen = sum(com_AI_prev_justGeneral, na.rm = TRUE),
    n_persistent_core_fields = sum(sust_core_fields, na.rm = TRUE),
    n_persistent_NOT_core_fields = sum(sust_NOT_core_fields, na.rm = TRUE), 
    n_persistent_coin_core_fields = sum(persistent_coinciding_field_core_fields, na.rm = TRUE),
    n_persistent_coin_NOT_core_fields = sum(persistent_coinciding_field_NOT_core_fields, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    # Shares for your original calculation based on new counts
    # Be careful with the denominator here:
    # If you want share of coinciding relative to coinciding + justGeneral for the *current* period:
    Share_coinciding_original_calc = if_else(
      (coinciding_fields_current + justGeneral_fields_current) > 0,
      coinciding_fields_current / (coinciding_fields_current + justGeneral_fields_current),
      0 # Or NA
    ),
    Share_justGeneral_absolute = justGeneral_fields_current / all_subclasses )

# Separate actual observed data (permutation_number == 0)
actual_shares <- summary_and_persistence_df %>%
  filter(permutation_number == 0) %>%
  select(ctry_code, Period, Actual_Share_coinciding = Share_coinciding_original_calc,
         Actual_persistent_coinciding = n_persistent_coinciding,
         Actual_persistent_justGeneral = n_persistent_justGeneral,
         Actual_persistent_justAI = n_persistent_justAI,
         Actual_persistent_Round_AI = n_persistent_Round_AI,
         Actual_com_AI_prev_coinciding = n_AI_prev_coinciding,
         Actual_com_coinciding_prev_AI = n_coinciding_prev_AI,
         Actual_com_AI_prev_gen = n_AI_prev_gen,
         Actual_persistent_core = n_persistent_core_fields,
         Actual_persistent_NOT_core = n_persistent_NOT_core_fields,
         Actual_persistent_coin_core = n_persistent_coin_core_fields,
         Actual_persistent_coin_NOT_core = n_persistent_coin_NOT_core_fields) 

# Permuted data (permutation_number > 0)
permuted_shares <- summary_and_persistence_df %>%
  filter(permutation_number > 0)

# --- 2. Summarize Permuted Data ---
# Calculate mean and 95% confidence interval (2.5th and 97.5th percentiles)
# for the share of coinciding specializations from permutations
permuted_summary <- permuted_shares %>%
  group_by(ctry_code, Period) %>%
  summarise(
    Mean_Permuted_Share = mean(Share_coinciding_original_calc, na.rm = TRUE),
    Lower_CI_Permuted = quantile(Share_coinciding_original_calc, probs = 0.025, na.rm = TRUE),
    Upper_CI_Permuted = quantile(Share_coinciding_original_calc, probs = 0.975, na.rm = TRUE),
    
    Mean_Permuted_Share_sust_coinc = mean(n_persistent_coinciding, na.rm = TRUE),
    Lower_CI_Permuted_sust_coinc = quantile(n_persistent_coinciding, probs = 0.025, na.rm = TRUE),
    Upper_CI_Permuted_sust_coinc = quantile(n_persistent_coinciding, probs = 0.975, na.rm = TRUE),
    
    Mean_Permuted_Share_gen_coinc = mean(n_persistent_justGeneral, na.rm = TRUE),
    Lower_CI_Permuted_gen_coinc = quantile(n_persistent_justGeneral, probs = 0.025, na.rm = TRUE),
    Upper_CI_Permuted_gen_coinc = quantile(n_persistent_justGeneral, probs = 0.975, na.rm = TRUE),
    
    Mean_Permuted_Share_ai_coinc = mean(n_persistent_justAI, na.rm = TRUE),
    Lower_CI_Permuted_ai_coinc = quantile(n_persistent_justAI, probs = 0.025, na.rm = TRUE),
    Upper_CI_Permuted_ai_coinc = quantile(n_persistent_justAI, probs = 0.975, na.rm = TRUE),
    
    Mean_Permuted_Share_ai_excl_coinc = mean(n_persistent_Round_AI, na.rm = TRUE),
    Lower_CI_Permuted_ai_excl_coinc = quantile(n_persistent_Round_AI, probs = 0.025, na.rm = TRUE),
    Upper_CI_Permuted_ai_excl_coinc = quantile(n_persistent_Round_AI, probs = 0.975, na.rm = TRUE),
    
    Mean_Permuted_Share_AI_prev_coinciding = mean(n_AI_prev_coinciding, na.rm = TRUE),
    Lower_CI_Permuted_AI_prev_coinciding = quantile(n_AI_prev_coinciding, probs = 0.025, na.rm = TRUE),
    Upper_CI_Permuted_AI_prev_coinciding = quantile(n_AI_prev_coinciding, probs = 0.975, na.rm = TRUE),
    
    Mean_Permuted_Share_coinciding_prev_AI = mean(n_coinciding_prev_AI, na.rm = TRUE),
    Lower_CI_Permuted_coinciding_prev_AI = quantile(n_coinciding_prev_AI, probs = 0.025, na.rm = TRUE),
    Upper_CI_Permuted_coinciding_prev_AI = quantile(n_coinciding_prev_AI, probs = 0.975, na.rm = TRUE),
    
    Mean_Permuted_Share_AI_prev_gen = mean(n_AI_prev_gen, na.rm = TRUE),
    Lower_CI_Permuted_AI_prev_gen = quantile(n_AI_prev_gen, probs = 0.025, na.rm = TRUE),
    Upper_CI_Permuted_AI_prev_gen = quantile(n_AI_prev_gen, probs = 0.975, na.rm = TRUE),
    
    Mean_Permuted_core_fields = mean(n_persistent_core_fields, na.rm = TRUE),
    Lower_CI_core_fields = quantile(n_persistent_core_fields, probs = 0.025, na.rm = TRUE),
    Upper_CI_core_fields = quantile(n_persistent_core_fields, probs = 0.975, na.rm = TRUE),
    
    Mean_Permuted_NOT_core_fields = mean(n_persistent_NOT_core_fields, na.rm = TRUE),
    Lower_CI_NOT_core_fields = quantile(n_persistent_NOT_core_fields, probs = 0.025, na.rm = TRUE),
    Upper_CI_NOT_core_fields = quantile(n_persistent_NOT_core_fields, probs = 0.975, na.rm = TRUE),
    
    Mean_Permuted_coin_core_fields = mean(n_persistent_coin_core_fields, na.rm = TRUE),
    Lower_CI_coin_core_fields = quantile(n_persistent_coin_core_fields, probs = 0.025, na.rm = TRUE),
    Upper_CI_coin_core_fields = quantile(n_persistent_coin_core_fields, probs = 0.975, na.rm = TRUE),
    
    Mean_Permuted_coin_NOT_core_fields = mean(n_persistent_coin_NOT_core_fields, na.rm = TRUE),
    Lower_CI_coin_NOT_core_fields = quantile(n_persistent_coin_NOT_core_fields, probs = 0.025, na.rm = TRUE),
    Upper_CI_coin_NOT_core_fields = quantile(n_persistent_coin_NOT_core_fields, probs = 0.975, na.rm = TRUE),
    
    .groups = 'drop'
  )

# --- 3. Combine Data for Plotting ---
plot_data <- actual_shares %>%
  left_join(permuted_summary, by = c("ctry_code", "Period")) %>%
  # Ensure Period is an ordered factor for plotting if it's not already
  mutate(Period = factor(Period, levels = unique(Permutations_all_1000$Period), ordered = TRUE),
         Country = factor(ctry_code)) # Use 'Country' for consistency with your example plot

# --- 4. Plot ---
# Define the same color palette you used
plot_data$Country <- gsub("US", "USA", str_trim(plot_data$Country))
plot_data$Country <- gsub("CN", "China", str_trim(plot_data$Country))
plot_data$Country <- gsub("JP", "Japan", str_trim(plot_data$Country))
plot_data$Country <- gsub("KR", "South Korea", str_trim(plot_data$Country))

country_colors <- c("China" = "#1B9E77", "Japan" = "#D95F02", "South Korea" = "#7570B3", "USA" = "#E7298A")
country_shapes <- c("China" = 21, "Japan" = 22, "South Korea" = 24, "USA" = 23) # Assuming ctry_code maps to these

#order country levels
plot_data$Country <- factor(plot_data$Country, levels = c("Japan", "USA","South Korea","China"))

significance_plot_save2 <- 
  ggplot(plot_data, aes(x = Period, group = Country)) +
  # Ribbon for permuted confidence interval
  geom_ribbon(aes(ymin = Lower_CI_Permuted, ymax = Upper_CI_Permuted, fill = Country), alpha = 0.2, show.legend = FALSE) +
  # Line for the mean of permuted shares
  geom_line(aes(y = Mean_Permuted_Share, color = Country), linetype = "dotted", size = 1, show.legend = FALSE) +
  # Line for actual observed shares
  geom_line(aes(y = Actual_Share_coinciding, color = Country), linetype = "dashed", size = 1.5) +
  # Points for actual observed shares
  geom_point(aes(y = Actual_Share_coinciding, shape = Country, fill = Country, color = Country), size = 5) + # Color for border
  
  scale_shape_manual(values = country_shapes, name = "Country") +
  scale_fill_manual(values = country_colors, name = "Country", guide = "legend") + # Ensure fill legend is shown
  scale_color_manual(values = country_colors, name = "Country", guide = "legend") + # Ensure color legend is shown
  xlab("Interval") +
  ylab("Share of break-in specialisations (%)") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + # Use percent_format
  theme_classic(base_size = 14) +
  theme(
    strip.background = element_blank(), # Optional: remove facet strip background
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "top", # Or "bottom", "right", "left"
    axis.text.x = element_text(angle = 45, hjust = 1) # Rotate x-axis labels if needed
  ) +
  facet_wrap(~ Country, scales = "free_y", ncol = 4) + 
  ggtitle(paste0("b) Subclasses - 5-year Intervals")) + theme(legend.position="none")

jpeg("Files_created_with_the_code/figures/robustness/Figure_12_permutations_subclass.jpg", width = 14, height = 10, units = 'in', res = 300)
grid.arrange(significance_plot_save, significance_plot_save2, ncol = 1)
dev.off()

#THIRD PART: Varying RTA thresholds -----
##3.1.Threshold 1: 0.95 -----
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

Threshold <- .95
IPC_RCAs_Top4$Round_general <- ifelse(IPC_RCAs_Top4$RCA_Gen < Threshold, 0, 1)
IPC_RCAs_Top4$Round_AI <- ifelse(IPC_RCAs_Top4$RCA_AI < Threshold, 0, 1)
#fix Total_RCA:
IPC_RCAs_Top4$Total_RCA_2 <- IPC_RCAs_Top4$Round_general + 2*IPC_RCAs_Top4$Round_AI

IPC_RCAs_Top4$Coiciding <- ifelse(IPC_RCAs_Top4$Total_RCA_2 ==3,1,0)
IPC_RCAs_Top4$justGeneral <- ifelse(IPC_RCAs_Top4$Total_RCA_2 ==1,1,0)
IPC_RCAs_Top4$OnlyAI <- ifelse(IPC_RCAs_Top4$Total_RCA_2 ==2,1,0)

#recreate the file per country per interval, by summing over the 3 columns;
IPC_RCAs <- IPC_RCAs_Top4
IPC_RCAs %<>% 
  group_by(ctry_code,Period) %>%   mutate(Share_coinciding = sum(Coiciding)/(sum(Coiciding)+sum(justGeneral))) %>% 
  mutate(Share_OnlyAI = sum(OnlyAI)/(sum(OnlyAI)+sum(Coiciding))) %>% 
  mutate(sum_coinciding = sum(Coiciding)) %>%   mutate(sum_justGeneral = sum(justGeneral)) %>%
  mutate(sum_OnlyAI = sum(OnlyAI)) %>%  ungroup()

SummaryAllData<-distinct(IPC_RCAs, ctry_code, Period, .keep_all = TRUE) 
colnames(SummaryAllData)[1] <- "Country"

OverlapTechn095<-
  ggplot(data=SummaryAllData, aes(x=Period, y=Share_coinciding, group=Country, shape = Country, color=Country)) +
  geom_point(aes(fill = Country), size=8) +   scale_shape_manual(values=c(21, 22, 24, 23)) +
  xlab("Interval") +  ylab("Share of break-in specialisations (%)") +
  #ggtitle(paste0("Share of break-in specialisations - ",Threshold," RTA threshold")) +
  ggtitle(paste0(Threshold," RTA threshold")) +
  theme_classic() +  geom_line(aes(color=Country), linetype = "dashed", size=1.5)+
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A")) +
  scale_color_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A")) +
  theme(legend.position="none")

##3.2.Threshold 1: 0.90 -----
rm(SummaryAllData, IPC_RCAs)
Threshold <- .90
IPC_RCAs_Top4$Round_general <- ifelse(IPC_RCAs_Top4$RCA_Gen < Threshold, 0, 1)
IPC_RCAs_Top4$Round_AI <- ifelse(IPC_RCAs_Top4$RCA_AI < Threshold, 0, 1)
#fix Total_RCA:
IPC_RCAs_Top4$Total_RCA_2 <- IPC_RCAs_Top4$Round_general + 2*IPC_RCAs_Top4$Round_AI

IPC_RCAs_Top4$Coiciding <- ifelse(IPC_RCAs_Top4$Total_RCA_2 ==3,1,0)
IPC_RCAs_Top4$justGeneral <- ifelse(IPC_RCAs_Top4$Total_RCA_2 ==1,1,0)
IPC_RCAs_Top4$OnlyAI <- ifelse(IPC_RCAs_Top4$Total_RCA_2 ==2,1,0)

#recreate the file per country per interval, by summing over the 3 columns;
IPC_RCAs <- IPC_RCAs_Top4
IPC_RCAs %<>% 
  group_by(ctry_code,Period) %>%   mutate(Share_coinciding = sum(Coiciding)/(sum(Coiciding)+sum(justGeneral))) %>% 
  mutate(Share_OnlyAI = sum(OnlyAI)/(sum(OnlyAI)+sum(Coiciding))) %>% 
  mutate(sum_coinciding = sum(Coiciding)) %>%   mutate(sum_justGeneral = sum(justGeneral)) %>%
  mutate(sum_OnlyAI = sum(OnlyAI)) %>%  ungroup()

SummaryAllData<-distinct(IPC_RCAs, ctry_code, Period, .keep_all = TRUE) 
colnames(SummaryAllData)[1] <- "Country"

OverlapTechn090<-
  ggplot(data=SummaryAllData, aes(x=Period, y=Share_coinciding, group=Country, shape = Country, color=Country)) +
  geom_point(aes(fill = Country), size=8) +   scale_shape_manual(values=c(21, 22, 24, 23)) +
  xlab("Interval") +  ylab("Share of break-in specialisations (%)") +
  ggtitle(paste0(Threshold,"0 RTA threshold")) +
  theme_classic() +  geom_line(aes(color=Country), linetype = "dashed", size=1.5)+
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A")) +
  scale_color_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A")) +
  theme(legend.position="none")

##3.3.Threshold 1: 0.85 -----
rm(SummaryAllData, IPC_RCAs)
Threshold <- .85
IPC_RCAs_Top4$Round_general <- ifelse(IPC_RCAs_Top4$RCA_Gen < Threshold, 0, 1)
IPC_RCAs_Top4$Round_AI <- ifelse(IPC_RCAs_Top4$RCA_AI < Threshold, 0, 1)
#fix Total_RCA:
IPC_RCAs_Top4$Total_RCA_2 <- IPC_RCAs_Top4$Round_general + 2*IPC_RCAs_Top4$Round_AI

IPC_RCAs_Top4$Coiciding <- ifelse(IPC_RCAs_Top4$Total_RCA_2 ==3,1,0)
IPC_RCAs_Top4$justGeneral <- ifelse(IPC_RCAs_Top4$Total_RCA_2 ==1,1,0)
IPC_RCAs_Top4$OnlyAI <- ifelse(IPC_RCAs_Top4$Total_RCA_2 ==2,1,0)

#recreate the file per country per interval, by summing over the 3 columns;
IPC_RCAs <- IPC_RCAs_Top4
IPC_RCAs %<>% 
  group_by(ctry_code,Period) %>%   mutate(Share_coinciding = sum(Coiciding)/(sum(Coiciding)+sum(justGeneral))) %>% 
  mutate(Share_OnlyAI = sum(OnlyAI)/(sum(OnlyAI)+sum(Coiciding))) %>% 
  mutate(sum_coinciding = sum(Coiciding)) %>%   mutate(sum_justGeneral = sum(justGeneral)) %>%
  mutate(sum_OnlyAI = sum(OnlyAI)) %>%  ungroup()

SummaryAllData<-distinct(IPC_RCAs, ctry_code, Period, .keep_all = TRUE) 
colnames(SummaryAllData)[1] <- "Country"

OverlapTechn085<-
  ggplot(data=SummaryAllData, aes(x=Period, y=Share_coinciding, group=Country, shape = Country, color=Country)) +
  geom_point(aes(fill = Country), size=8) +   scale_shape_manual(values=c(21, 22, 24, 23)) +
  xlab("Interval") +  ylab("Share of break-in specialisations (%)") +
  ggtitle(paste0(Threshold," RTA threshold")) +
  theme_classic() +  geom_line(aes(color=Country), linetype = "dashed", size=1.5)+
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A")) +
  scale_color_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A")) +
  theme(legend.position="none")

##3.4.Threshold 1: 1.05 -----
rm(SummaryAllData, IPC_RCAs)
Threshold <- 1.05
IPC_RCAs_Top4$Round_general <- ifelse(IPC_RCAs_Top4$RCA_Gen < Threshold, 0, 1)
IPC_RCAs_Top4$Round_AI <- ifelse(IPC_RCAs_Top4$RCA_AI < Threshold, 0, 1)
#fix Total_RCA:
IPC_RCAs_Top4$Total_RCA_2 <- IPC_RCAs_Top4$Round_general + 2*IPC_RCAs_Top4$Round_AI

IPC_RCAs_Top4$Coiciding <- ifelse(IPC_RCAs_Top4$Total_RCA_2 ==3,1,0)
IPC_RCAs_Top4$justGeneral <- ifelse(IPC_RCAs_Top4$Total_RCA_2 ==1,1,0)
IPC_RCAs_Top4$OnlyAI <- ifelse(IPC_RCAs_Top4$Total_RCA_2 ==2,1,0)

#recreate the file per country per interval, by summing over the 3 columns;
IPC_RCAs <- IPC_RCAs_Top4
IPC_RCAs %<>% 
  group_by(ctry_code,Period) %>%   mutate(Share_coinciding = sum(Coiciding)/(sum(Coiciding)+sum(justGeneral))) %>% 
  mutate(Share_OnlyAI = sum(OnlyAI)/(sum(OnlyAI)+sum(Coiciding))) %>% 
  mutate(sum_coinciding = sum(Coiciding)) %>%   mutate(sum_justGeneral = sum(justGeneral)) %>%
  mutate(sum_OnlyAI = sum(OnlyAI)) %>%  ungroup()

SummaryAllData<-distinct(IPC_RCAs, ctry_code, Period, .keep_all = TRUE) 
colnames(SummaryAllData)[1] <- "Country"

OverlapTechn105<-
  ggplot(data=SummaryAllData, aes(x=Period, y=Share_coinciding, group=Country, shape = Country, color=Country)) +
  geom_point(aes(fill = Country), size=8) +   scale_shape_manual(values=c(21, 22, 24, 23)) +
  xlab("Interval") +  ylab("Share of break-in specialisations (%)") +
  ggtitle(paste0(Threshold," RTA threshold")) +
  theme_classic() +  geom_line(aes(color=Country), linetype = "dashed", size=1.5)+
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A")) +
  scale_color_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A")) +
  theme(legend.position="none")

##3.5.Threshold 1: 1.10 -----
rm(SummaryAllData, IPC_RCAs)
Threshold <- 1.10
IPC_RCAs_Top4$Round_general <- ifelse(IPC_RCAs_Top4$RCA_Gen < Threshold, 0, 1)
IPC_RCAs_Top4$Round_AI <- ifelse(IPC_RCAs_Top4$RCA_AI < Threshold, 0, 1)
#fix Total_RCA:
IPC_RCAs_Top4$Total_RCA_2 <- IPC_RCAs_Top4$Round_general + 2*IPC_RCAs_Top4$Round_AI

IPC_RCAs_Top4$Coiciding <- ifelse(IPC_RCAs_Top4$Total_RCA_2 ==3,1,0)
IPC_RCAs_Top4$justGeneral <- ifelse(IPC_RCAs_Top4$Total_RCA_2 ==1,1,0)
IPC_RCAs_Top4$OnlyAI <- ifelse(IPC_RCAs_Top4$Total_RCA_2 ==2,1,0)

#recreate the file per country per interval, by summing over the 3 columns;
IPC_RCAs <- IPC_RCAs_Top4
IPC_RCAs %<>% 
  group_by(ctry_code,Period) %>%   mutate(Share_coinciding = sum(Coiciding)/(sum(Coiciding)+sum(justGeneral))) %>% 
  mutate(Share_OnlyAI = sum(OnlyAI)/(sum(OnlyAI)+sum(Coiciding))) %>% 
  mutate(sum_coinciding = sum(Coiciding)) %>%   mutate(sum_justGeneral = sum(justGeneral)) %>%
  mutate(sum_OnlyAI = sum(OnlyAI)) %>%  ungroup()

SummaryAllData<-distinct(IPC_RCAs, ctry_code, Period, .keep_all = TRUE) 
colnames(SummaryAllData)[1] <- "Country"

OverlapTechn110<-
  ggplot(data=SummaryAllData, aes(x=Period, y=Share_coinciding, group=Country, shape = Country, color=Country)) +
  geom_point(aes(fill = Country), size=8) +   scale_shape_manual(values=c(21, 22, 24, 23)) +
  xlab("Interval") +  ylab("Share of break-in specialisations (%)") +
  ggtitle(paste0(Threshold,"0 RTA threshold")) +
  theme_classic() +  geom_line(aes(color=Country), linetype = "dashed", size=1.5)+
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A")) +
  scale_color_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A")) +
  theme(legend.position="none")

##3.6.Threshold 1: 1.15 -----
rm(SummaryAllData, IPC_RCAs)
Threshold <- 1.15
IPC_RCAs_Top4$Round_general <- ifelse(IPC_RCAs_Top4$RCA_Gen < Threshold, 0, 1)
IPC_RCAs_Top4$Round_AI <- ifelse(IPC_RCAs_Top4$RCA_AI < Threshold, 0, 1)
#fix Total_RCA:
IPC_RCAs_Top4$Total_RCA_2 <- IPC_RCAs_Top4$Round_general + 2*IPC_RCAs_Top4$Round_AI

IPC_RCAs_Top4$Coiciding <- ifelse(IPC_RCAs_Top4$Total_RCA_2 ==3,1,0)
IPC_RCAs_Top4$justGeneral <- ifelse(IPC_RCAs_Top4$Total_RCA_2 ==1,1,0)
IPC_RCAs_Top4$OnlyAI <- ifelse(IPC_RCAs_Top4$Total_RCA_2 ==2,1,0)

#recreate the file per country per interval, by summing over the 3 columns;
IPC_RCAs <- IPC_RCAs_Top4
IPC_RCAs %<>% 
  group_by(ctry_code,Period) %>%   mutate(Share_coinciding = sum(Coiciding)/(sum(Coiciding)+sum(justGeneral))) %>% 
  mutate(Share_OnlyAI = sum(OnlyAI)/(sum(OnlyAI)+sum(Coiciding))) %>% 
  mutate(sum_coinciding = sum(Coiciding)) %>%   mutate(sum_justGeneral = sum(justGeneral)) %>%
  mutate(sum_OnlyAI = sum(OnlyAI)) %>%  ungroup()

SummaryAllData<-distinct(IPC_RCAs, ctry_code, Period, .keep_all = TRUE) 
colnames(SummaryAllData)[1] <- "Country"

OverlapTechn115<-
  ggplot(data=SummaryAllData, aes(x=Period, y=Share_coinciding, group=Country, shape = Country, color=Country)) +
  geom_point(aes(fill = Country), size=8) +   scale_shape_manual(values=c(21, 22, 24, 23)) +
  xlab("Interval") +  ylab("Share of break-in specialisations (%)") +
  ggtitle(paste0(Threshold," RTA threshold")) +
  theme_classic() +  geom_line(aes(color=Country), linetype = "dashed", size=1.5)+
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A")) +
  scale_color_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A")) +
  theme(legend.position="none")

##3.7.Threshold 1: 0.5 -----
rm(SummaryAllData, IPC_RCAs)
Threshold <- .5
IPC_RCAs_Top4$Round_general <- ifelse(IPC_RCAs_Top4$RCA_Gen < Threshold, 0, 1)
IPC_RCAs_Top4$Round_AI <- ifelse(IPC_RCAs_Top4$RCA_AI < Threshold, 0, 1)
#fix Total_RCA:
IPC_RCAs_Top4$Total_RCA_2 <- IPC_RCAs_Top4$Round_general + 2*IPC_RCAs_Top4$Round_AI

IPC_RCAs_Top4$Coiciding <- ifelse(IPC_RCAs_Top4$Total_RCA_2 ==3,1,0)
IPC_RCAs_Top4$justGeneral <- ifelse(IPC_RCAs_Top4$Total_RCA_2 ==1,1,0)
IPC_RCAs_Top4$OnlyAI <- ifelse(IPC_RCAs_Top4$Total_RCA_2 ==2,1,0)

#recreate the file per country per interval, by summing over the 3 columns;
IPC_RCAs <- IPC_RCAs_Top4
IPC_RCAs %<>% 
  group_by(ctry_code,Period) %>%   mutate(Share_coinciding = sum(Coiciding)/(sum(Coiciding)+sum(justGeneral))) %>% 
  mutate(Share_OnlyAI = sum(OnlyAI)/(sum(OnlyAI)+sum(Coiciding))) %>% 
  mutate(sum_coinciding = sum(Coiciding)) %>%   mutate(sum_justGeneral = sum(justGeneral)) %>%
  mutate(sum_OnlyAI = sum(OnlyAI)) %>%  ungroup()

SummaryAllData<-distinct(IPC_RCAs, ctry_code, Period, .keep_all = TRUE) 
colnames(SummaryAllData)[1] <- "Country"

OverlapTechn050<-
  ggplot(data=SummaryAllData, aes(x=Period, y=Share_coinciding, group=Country, shape = Country, color=Country)) +
  geom_point(aes(fill = Country), size=8) +   scale_shape_manual(values=c(21, 22, 24, 23)) +
  xlab("Interval") +  ylab("Share of break-in specialisations (%)") +
  ggtitle(paste0(Threshold," RTA threshold")) +
  theme_classic() +  geom_line(aes(color=Country), linetype = "dashed", size=1.5)+
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A")) +
  scale_color_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A")) +
  theme(legend.position="none")

##3.8.Threshold 1: 1.5 -----
rm(SummaryAllData, IPC_RCAs)
Threshold <- 1.5
IPC_RCAs_Top4$Round_general <- ifelse(IPC_RCAs_Top4$RCA_Gen < Threshold, 0, 1)
IPC_RCAs_Top4$Round_AI <- ifelse(IPC_RCAs_Top4$RCA_AI < Threshold, 0, 1)
#fix Total_RCA:
IPC_RCAs_Top4$Total_RCA_2 <- IPC_RCAs_Top4$Round_general + 2*IPC_RCAs_Top4$Round_AI

IPC_RCAs_Top4$Coiciding <- ifelse(IPC_RCAs_Top4$Total_RCA_2 ==3,1,0)
IPC_RCAs_Top4$justGeneral <- ifelse(IPC_RCAs_Top4$Total_RCA_2 ==1,1,0)
IPC_RCAs_Top4$OnlyAI <- ifelse(IPC_RCAs_Top4$Total_RCA_2 ==2,1,0)

#recreate the file per country per interval, by summing over the 3 columns;
IPC_RCAs <- IPC_RCAs_Top4
IPC_RCAs %<>% 
  group_by(ctry_code,Period) %>%   mutate(Share_coinciding = sum(Coiciding)/(sum(Coiciding)+sum(justGeneral))) %>% 
  mutate(Share_OnlyAI = sum(OnlyAI)/(sum(OnlyAI)+sum(Coiciding))) %>% 
  mutate(sum_coinciding = sum(Coiciding)) %>%   mutate(sum_justGeneral = sum(justGeneral)) %>%
  mutate(sum_OnlyAI = sum(OnlyAI)) %>%  ungroup()

SummaryAllData<-distinct(IPC_RCAs, ctry_code, Period, .keep_all = TRUE) 
colnames(SummaryAllData)[1] <- "Country"

OverlapTechn150<-
  ggplot(data=SummaryAllData, aes(x=Period, y=Share_coinciding, group=Country, shape = Country, color=Country)) +
  geom_point(aes(fill = Country), size=8) +   scale_shape_manual(values=c(21, 22, 24, 23)) +
  xlab("Interval") +  ylab("Share of break-in specialisations (%)") +
  ggtitle(paste0(Threshold," RTA threshold")) +
  theme_classic() +  geom_line(aes(color=Country), linetype = "dashed", size=1.5)+
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A")) +
  scale_color_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A")) +
  theme(legend.position="none")

##3.9.Combine figures-----
#recreate the reference figure
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

small_plots_grid <- (OverlapTechn085 + OverlapTechn090 + OverlapTechn095) /  # Top row of 3 plots
  (OverlapTechn105 + OverlapTechn110 + OverlapTechn115)    # Bottom row of 3 plots

small_plots_grid_extreme <- (OverlapTechn050 + OverlapTechn + theme(legend.position="none") +  ggtitle("Traditional 1.0 RTA threshold")  + OverlapTechn150)

jpeg("Files_created_with_the_code/figures/robustness/Figure_8_RTA_gradual_change.jpg", width = 12, height = 8, units = 'in', res = 300)
small_plots_grid
dev.off()

jpeg("Files_created_with_the_code/figures/robustness/Figure_9_RTA_extreme.jpg", width = 16, height = 6, units = 'in', res = 300)
small_plots_grid_extreme
dev.off()

#FOURTH PART: Econometrics-----
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

# Model 1 - The simplest model
model1 <- lm(Share_Coinciding ~ rel_density + Period, 
             data = regression_data) 
summary(model1)

# Model 2 - adding number of general specialisations
model2 <- lm(Share_Coinciding ~ rel_density + total_general_specializations + Period + 
               total_AI_specializations, 
             data = regression_data) 
summary(model2)

# Model 3 - controlling for country
model3 <- lm(Share_Coinciding ~ rel_density + total_general_specializations + Period + ctry_code + 
               total_AI_specializations,
             data = regression_data)
summary(model3)

stargazer(model1, model2, model3, type = "html", out = "Files_created_with_the_code/data/files_code_Fields_analysis/models_1_2_3.doc")

#Next models: looking at sustained specialisations
#Model 4 - persistent break-ins
newmodel4 <- lm(actual_persistent_coinciding ~ rel_density + Period+ actual_persistent_general_all + 
                  actual_n_persistent_round_ai, data = regression_data)
summary(newmodel4)

#Model 5 - persistent break-ins
newmodel5 <- lm(actual_persistent_coinciding ~ rel_density + Period+ total_general_specializations + coinciding_specialization +
                  total_AI_specializations, 
             data = regression_data)
summary(newmodel5)

#Model 6 persistent AI-specific
newmodel6 <- lm(actual_n_persistent_round_ai ~ rel_density + Period+ total_general_specializations +
               actual_persistent_coinciding+coinciding_specialization+total_AI_specializations, 
             data = regression_data)
summary(newmodel6)
stargazer(newmodel4, newmodel5, newmodel6, type = "html", out = "Files_created_with_the_code/data/files_code_Fields_analysis/modelsnew_4_to_6.doc")
