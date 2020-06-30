library(EconGeo) # Economic Geography functions
library("data.table") #for reading the big files using fread and for replacing countries names (by AI_pat for example)
library(tidyverse) # Collection of all the good stuff like dplyr, ggplot2 ect.
library(magrittr) # For extra-piping operators (eg. %<>%)
library(plyr)

group_by_applnID <- function (data){
  data %>%
    group_by(appln_id) %>%
    mutate(field_weight = 1 / n()) %>%
    ungroup()
}

group_by_ctry_and_nace <- function (data){
  data %<>%
    group_by(ctry_code, nace2_code) %>%
    summarise(n_tech_reg = sum(field_weight)) %>%
    ungroup() %>%
    drop_na() 
}

#1.2. General Perspective ----
#1.2.1.First Period -  ----
#For the first period, which goes from 1974 to 1988, we need only the dataset from Part2:
setwd("C:/Users/Matheus/Desktop") 
c <- 45233329 -40000000
Nace_all_patents_Part1 <- fread("All_patents_and_Naces_Part2.csv", header = F, nrow = 20000000)
Nace_all_patents_Part2 <- fread("All_patents_and_Naces_Part2.csv", header = F, nrow = 20000000, skip = 20000000)
Nace_all_patents_Part3 <- fread("All_patents_and_Naces_Part2.csv", header = F, nrow = c, skip = 40000000)

names(Nace_all_patents_Part1) <- c("appln_id", "ctry_code", "nace2_code", "weight", "priority_year")
names(Nace_all_patents_Part2) <- c("appln_id", "ctry_code", "nace2_code", "weight", "priority_year")
names(Nace_all_patents_Part3) <- c("appln_id", "ctry_code", "nace2_code", "weight", "priority_year")

#we want to pick only the registers from the period we want (from 1974 to 1988, including both cited years)
a = 1973
b = 1989

Nace_all_patents_Part1 <- Nace_all_patents_Part1[Nace_all_patents_Part1$priority_year < b,]
Nace_all_patents_Part1 <- Nace_all_patents_Part1[Nace_all_patents_Part1$priority_year > a,]

Nace_all_patents_Part2 <- Nace_all_patents_Part2[Nace_all_patents_Part2$priority_year < b,]
Nace_all_patents_Part2 <- Nace_all_patents_Part2[Nace_all_patents_Part2$priority_year > a,]

Nace_all_patents_Part3 <- Nace_all_patents_Part3[Nace_all_patents_Part3$priority_year < b,]
Nace_all_patents_Part3 <- Nace_all_patents_Part3[Nace_all_patents_Part3$priority_year > a,]

#let's drop the columns we won't use (weight and priority_year):
Nace_all_patents_Part1 <- Nace_all_patents_Part1[, c((-4), (-5))]
Nace_all_patents_Part2 <- Nace_all_patents_Part2[, c((-4), (-5))]
Nace_all_patents_Part3 <- Nace_all_patents_Part3[, c((-4), (-5))]

#we combine the 3 files:
Nace_all_patents_FirstPeriod <- rbind(Nace_all_patents_Part1, Nace_all_patents_Part2, Nace_all_patents_Part3)
#and exclude the 3 big ones we just used, so we have back our memory:
rm(Nace_all_patents_Part1, Nace_all_patents_Part2, Nace_all_patents_Part3)

#now we apply the 2 functions we created at the beginning of this section:
reg_tech1 <- group_by_applnID(Nace_all_patents_FirstPeriod)
rm(Nace_all_patents_FirstPeriod)
reg_tech1 <- group_by_ctry_and_nace(reg_tech1)

mat_reg_tech1 <- reg_tech1 %>%
  arrange(nace2_code, ctry_code) %>%
  pivot_wider(names_from = nace2_code, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

rownames(mat_reg_tech1) <- mat_reg_tech1 %>% pull(ctry_code)

mat_reg_tech1 %<>% select(-ctry_code) %>%
  as.matrix() %>%
  round()

reg_RCA1 <- mat_reg_tech1 %>% location.quotient(binary = F) %>% 
  as.data.frame() %>% 
  rownames_to_column("ctry_code") %>% 
  as_tibble() %>% 
  gather(key = "nace2_code", value = "RCA", -ctry_code) %>%
  arrange(ctry_code, nace2_code)

#1.2.1.Second Period -  ----
#For the first period, which goes from 1989 to 2003, we need only the dataset from Part2:
setwd("C:/Users/Matheus/Desktop") 
c <- 45233329 -40000000
Nace_all_patents_Part1 <- fread("All_patents_and_Naces_Part2.csv", header = F, nrow = 20000000)
Nace_all_patents_Part2 <- fread("All_patents_and_Naces_Part2.csv", header = F, nrow = 20000000, skip = 20000000)
Nace_all_patents_Part3 <- fread("All_patents_and_Naces_Part2.csv", header = F, nrow = c, skip = 40000000)

names(Nace_all_patents_Part1) <- c("appln_id", "ctry_code", "nace2_code", "weight", "priority_year")
names(Nace_all_patents_Part2) <- c("appln_id", "ctry_code", "nace2_code", "weight", "priority_year")
names(Nace_all_patents_Part3) <- c("appln_id", "ctry_code", "nace2_code", "weight", "priority_year")

a = 1988
b = 2004

Nace_all_patents_Part1 <- Nace_all_patents_Part1[Nace_all_patents_Part1$priority_year < b,]
Nace_all_patents_Part1 <- Nace_all_patents_Part1[Nace_all_patents_Part1$priority_year > a,]

Nace_all_patents_Part2 <- Nace_all_patents_Part2[Nace_all_patents_Part2$priority_year < b,]
Nace_all_patents_Part2 <- Nace_all_patents_Part2[Nace_all_patents_Part2$priority_year > a,]

Nace_all_patents_Part3 <- Nace_all_patents_Part3[Nace_all_patents_Part3$priority_year < b,]
Nace_all_patents_Part3 <- Nace_all_patents_Part3[Nace_all_patents_Part3$priority_year > a,]

#let's drop the columns we won't use (weight and priority_year):
Nace_all_patents_Part1 <- Nace_all_patents_Part1[, c((-4), (-5))]
Nace_all_patents_Part2 <- Nace_all_patents_Part2[, c((-4), (-5))]
Nace_all_patents_Part3 <- Nace_all_patents_Part3[, c((-4), (-5))]

#we combine the 3 files:
Nace_all_patents_SecondPeriod <- rbind(Nace_all_patents_Part1, Nace_all_patents_Part2, Nace_all_patents_Part3)
#and exclude the 3 big ones we just used, so we have back our memory:
rm(Nace_all_patents_Part1, Nace_all_patents_Part2, Nace_all_patents_Part3)

#now we apply the 2 functions we created at the beginning of this section:
reg_tech2 <- group_by_applnID(Nace_all_patents_SecondPeriod)
rm(Nace_all_patents_SecondPeriod)
reg_tech2 <- group_by_ctry_and_nace(reg_tech2)

mat_reg_tech2 <- reg_tech2 %>%
  arrange(nace2_code, ctry_code) %>%
  pivot_wider(names_from = nace2_code, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

rownames(mat_reg_tech2) <- mat_reg_tech2 %>% pull(ctry_code)

mat_reg_tech2 %<>% select(-ctry_code) %>%
  as.matrix() %>%
  round()

reg_RCA2 <- mat_reg_tech2 %>% location.quotient(binary = F) %>% 
  as.data.frame() %>% 
  rownames_to_column("ctry_code") %>% 
  as_tibble() %>% 
  gather(key = "nace2_code", value = "RCA", -ctry_code) %>%
  arrange(ctry_code, nace2_code)

#1.2.3.Third Period -  ----
#For the third period, which goes from 2004 to 2018, we  need only the dataset from Part1. This
#specific dataset only has patents from 2004 to 2018, so we don't have to filter it. But calculating
#the reg_tech is very computationally expansive, so we have to divide that in 3 parts.
setwd("C:/Users/Matheus/Desktop") 
c <- 58935336-40000000
Nace_all_patents_Part1 <- fread("All_patents_and_Naces_Part1.csv", header = F, nrow = 20000000)
Nace_all_patents_Part2 <- fread("All_patents_and_Naces_Part1.csv", header = F, nrow = 20000000, skip = 20000000)
Nace_all_patents_Part3 <- fread("All_patents_and_Naces_Part1.csv", header = F, nrow = c, skip = 40000000)

names(Nace_all_patents_Part1) <- c("appln_id", "ctry_code", "nace2_code", "weight", "priority_year")
names(Nace_all_patents_Part2) <- c("appln_id", "ctry_code", "nace2_code", "weight", "priority_year")
names(Nace_all_patents_Part3) <- c("appln_id", "ctry_code", "nace2_code", "weight", "priority_year")

Nace_all_patents_Part1 <- Nace_all_patents_Part1[, c((-4), (-5))]
Nace_all_patents_Part2 <- Nace_all_patents_Part2[, c((-4), (-5))]
Nace_all_patents_Part3 <- Nace_all_patents_Part3[, c((-4), (-5))]

#here we divide our calculations of the reg_tech (which was not necessary on the 2 previous periods)
reg_tech4 <- group_by_applnID(Nace_all_patents_Part1)
rm(Nace_all_patents_Part1)
reg_tech4 <- group_by_ctry_and_nace(reg_tech4)

reg_tech5 <- group_by_applnID(Nace_all_patents_Part2)
rm(Nace_all_patents_Part2)
reg_tech5 <- group_by_ctry_and_nace(reg_tech5)

reg_tech6 <- group_by_applnID(Nace_all_patents_Part3)
rm(Nace_all_patents_Part3)
reg_tech6 <- group_by_ctry_and_nace(reg_tech6)

#now we merge them
reg_tech3 <- merge(reg_tech4, reg_tech5, all=T, by=c("ctry_code", "nace2_code"))
reg_tech3 <- merge(reg_tech3, reg_tech6, all=T, by=c("ctry_code", "nace2_code"))

#remove the big files
rm(reg_tech4, reg_tech5, reg_tech6)

#replace NAs, so we don't have problems when summing:
reg_tech3[is.na(reg_tech3)] <- 0

#do the summ, exclude the tables used, and rename the dataset accordingly:
reg_tech3$sum <- rowSums(reg_tech3[,c(3:5)])
reg_tech3 <- reg_tech3[, c((-3), (-4), (-5))]
names(reg_tech3) <- c("ctry_code", "nace2_code", "n_tech_reg")
reg_tech3 <- reg_tech3

mat_reg_tech3 <- reg_tech3 %>%
  arrange(nace2_code, ctry_code) %>%
  pivot_wider(names_from = nace2_code, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

rownames(mat_reg_tech3) <- mat_reg_tech3 %>% pull(ctry_code)

mat_reg_tech3 %<>% select(-ctry_code) %>%
  as.matrix() %>%
  round()

reg_RCA3 <- mat_reg_tech3 %>% location.quotient(binary = F) %>% 
  as.data.frame() %>% 
  rownames_to_column("ctry_code") %>% 
  as_tibble() %>% 
  gather(key = "nace2_code", value = "RCA", -ctry_code) %>%
  arrange(ctry_code, nace2_code)

#1.3. AI perspective ----
#1.3.1.First Period -  ----
#For the first period, which goes from 1974 to 1988, we need only the dataset from Part2:
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

patents_AI_specific_1s <- read.csv("Data_Nace/Nace2_AI csv.csv", sep = ";", header = TRUE, dec=",")

a = 1973
b = 1989

patents_AI_specific_1st <- patents_AI_specific_1st[patents_AI_specific_1st$priority_year < b,]
patents_AI_specific_1st <- patents_AI_specific_1st[patents_AI_specific_1st$priority_year > a,]

#let's drop the columns we won't use:
patents_AI_specific_1st <- patents_AI_specific_1st[, c((-2), (-3), (-6), (-7), (-8), (-10))]

#now we apply the 2 functions we created at the beginning of this section:
reg_tech1 <- group_by_applnID(patents_AI_specific_1st)
rm(patents_AI_specific_1st)
reg_tech1 <- group_by_ctry_and_nace(reg_tech1)

###First Period:
mat_reg_tech1 <- reg_tech1 %>%
  arrange(nace2_code, ctry_code) %>%
  pivot_wider(names_from = nace2_code, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

rownames(mat_reg_tech1) <- mat_reg_tech1 %>% pull(ctry_code)

mat_reg_tech1 %<>% select(-ctry_code) %>%
  as.matrix() %>%
  round()

reg_RCA1_AIspecific <- mat_reg_tech1 %>% location.quotient(binary = F) %>% 
  as.data.frame() %>% 
  rownames_to_column("ctry_code") %>% 
  as_tibble() %>% 
  gather(key = "nace2_code", value = "RCA", -ctry_code) %>%
  arrange(ctry_code, nace2_code)

Data1period <- merge(reg_RCA1, reg_RCA1_AIspecific, all=T, by=c("ctry_code", "nace2_code"))
Data1period$Period <- "1974-1988"
names(Data1period) <- c("ctry_code", "nace2_code", "RCA_Gen", "RCA_AI", "Period")
write.csv2(Data1period, file = "Data_Nace/Data1period_RCA.csv", row.names = F)

#1.3.2.Second Period -  ----
patents_AI_specific_2nd <- read.csv("Data_Nace/Nace2_AI csv.csv", sep = ";", header = TRUE, dec=",")

a = 1988
b = 2004

patents_AI_specific_2nd <- patents_AI_specific_2nd[patents_AI_specific_2nd$priority_year < b,]
patents_AI_specific_2nd <- patents_AI_specific_2nd[patents_AI_specific_2nd$priority_year > a,]

#let's drop the columns we won't use:
patents_AI_specific_2nd <- patents_AI_specific_2nd[, c((-2), (-3), (-6), (-7), (-8), (-10))]

#now we apply the 2 functions we created at the beginning of this section:
reg_tech2 <- group_by_applnID(patents_AI_specific_2nd)
rm(patents_AI_specific_2nd)
reg_tech2 <- group_by_ctry_and_nace(reg_tech2)

###Second Period:
mat_reg_tech2 <- reg_tech2 %>%
  arrange(nace2_code, ctry_code) %>%
  pivot_wider(names_from = nace2_code, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

rownames(mat_reg_tech2) <- mat_reg_tech2 %>% pull(ctry_code)

mat_reg_tech2 %<>% select(-ctry_code) %>%
  as.matrix() %>%
  round()

reg_RCA2_AIspecific <- mat_reg_tech2 %>% location.quotient(binary = F) %>% 
  as.data.frame() %>% 
  rownames_to_column("ctry_code") %>% 
  as_tibble() %>% 
  gather(key = "nace2_code", value = "RCA", -ctry_code) %>%
  arrange(ctry_code, nace2_code)

Data2period <- merge(reg_RCA2, reg_RCA2_AIspecific, all=T, by=c("ctry_code", "nace2_code"))
Data2period$Period <- "1989-2003"
names(Data2period) <- c("ctry_code", "nace2_code", "RCA_Gen", "RCA_AI", "Period")
write.csv2(Data2period, file = "Data_Nace/Data2period_RCA.csv", row.names = F)

#1.3.3.Third Period -  ----
#For the third period, which goes from 2004 to 2018:
patents_AI_specific_3rd <- read.csv("Data_Nace/Nace2_AI csv.csv", sep = ";", header = TRUE, dec=",")

a = 2003
b = 2019

patents_AI_specific_3rd <- patents_AI_specific_3rd[patents_AI_specific_3rd$priority_year < b,]
patents_AI_specific_3rd <- patents_AI_specific_3rd[patents_AI_specific_3rd$priority_year > a,]

#let's drop the columns we won't use:
patents_AI_specific_3rd <- patents_AI_specific_3rd[, c((-2), (-3), (-6), (-7), (-8), (-10))]

#now we apply the 2 functions we created at the beginning of this section:
reg_tech3 <- group_by_applnID(patents_AI_specific_3rd)
rm(patents_AI_specific_3rd)
reg_tech3 <- group_by_ctry_and_nace(reg_tech3)

###Third Period:
mat_reg_tech3 <- reg_tech3 %>%
  arrange(nace2_code, ctry_code) %>%
  pivot_wider(names_from = nace2_code, values_from = n_tech_reg, values_fill = list(n_tech_reg = 0))

rownames(mat_reg_tech3) <- mat_reg_tech3 %>% pull(ctry_code)

mat_reg_tech3 %<>% select(-ctry_code) %>%
  as.matrix() %>%
  round()

reg_RCA3_AIspecific <- mat_reg_tech3 %>% location.quotient(binary = F) %>% 
  as.data.frame() %>% 
  rownames_to_column("ctry_code") %>% 
  as_tibble() %>% 
  gather(key = "nace2_code", value = "RCA", -ctry_code) %>%
  arrange(ctry_code, nace2_code)

Data3period <- merge(reg_RCA3, reg_RCA3_AIspecific, all=T, by=c("ctry_code", "nace2_code"))
Data3period$Period <- "2004-2018"
names(Data3period) <- c("ctry_code", "nace2_code", "RCA_Gen", "RCA_AI", "Period")
write.csv2(Data3period, file = "Data_Nace/Data3period_RCA.csv", row.names = F)

Nace_RCAs <- rbind(Data1period, Data2period, Data3period)

#1.4.Visualization RCAs ----

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

#Select the 4 countries we want
Nace_RCAs_Top4 <- Nace_RCAs[Nace_RCAs$ctry_code == "CN" | 
                                              Nace_RCAs$ctry_code == "KR"| 
                                              Nace_RCAs$ctry_code == "US"| 
                                              Nace_RCAs$ctry_code == "JP", ]

Nace_RCAs_Top4$ctry_code <- as.vector(Nace_RCAs_Top4$ctry_code)

#add new label data:
Nace2LabelData <- read.csv("Summary NACE2 labels.csv", sep = ";", header = TRUE, dec = ",")
Nace_RCAs_Top4$Label <- Nace2LabelData$Summary[match(Nace_RCAs_Top4$nace2_code, Nace2LabelData$nace2_code)]

#select only the top10 labels
Nace_RCAs_Top5 <- Nace_RCAs_Top4
table(Nace_RCAs_Top4$Label)
Nace_RCAs_Top4<- Nace_RCAs_Top4[rowSums(is.na(Nace_RCAs_Top4)) == 0,]

#replace names:
library(stringr)
Nace_RCAs_Top4$ctry_code <- gsub("US", "USA", str_trim(Nace_RCAs_Top4$ctry_code))
Nace_RCAs_Top4$ctry_code <- gsub("CN", "China", str_trim(Nace_RCAs_Top4$ctry_code))
Nace_RCAs_Top4$ctry_code <- gsub("JP", "Japan", str_trim(Nace_RCAs_Top4$ctry_code))
Nace_RCAs_Top4$ctry_code <- gsub("KR", "South Korea", str_trim(Nace_RCAs_Top4$ctry_code))

#Figure General:
Gen <- ddply(Nace_RCAs_Top4, c("Period", "Label"), summarise, Value.mean=log10(mean(RCA_Gen)))
FigGen <- ggplot(Nace_RCAs_Top4,aes(x = log10(RCA_Gen), y=ctry_code, color=Period)) + geom_count(shape=19, alpha=1/1.4, size=4) +
  facet_wrap(~Label, ncol = 5) +
  geom_vline(data=Gen, aes(xintercept=Value.mean,  colour=Period),
             linetype="dashed", size=1) +
  ggtitle("Countries' Performance by Nace code - General Perspective") +
  xlab("LOG10 of the Country' Revealed Comparative Advantage (RCA) index") +
  ylab(NULL)

#Figure AI:
Ais <- ddply(Nace_RCAs_Top4, c("Period", "Label"), summarise, Value.mean=log10(mean(RCA_AI)))
FigAI <- ggplot(Nace_RCAs_Top4,aes(x = log10(RCA_AI), y=ctry_code, color=Period)) + geom_count(shape=19, alpha=1/1.4, size=4) +
  facet_wrap(~Label, ncol = 5) +
  geom_vline(data=Ais, aes(xintercept=Value.mean,  colour=Period),
             linetype="dashed", size=1) +
  ggtitle("Countries' Performance by Nace code - AI-specific Perspective") +
  xlab("LOG10 of the Country' Revealed Comparative Advantage (RCA) index") +
  ylab(NULL)

tiff("Figures_Nace/Plot_Nace_RCA.jpg", width = 14, height = 7, units = 'in', res = 200)
multiplot(FigGen, FigAI, cols=1)
dev.off()
