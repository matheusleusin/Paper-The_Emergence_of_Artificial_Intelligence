#2.4. Visualization ----
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

#2.4.1.Relatedness ----
Relatedness_1st <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/Relatedness_1st_period_IPC.csv", sep = ";", header = TRUE, dec=",")
Relatedness_2nd <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/Relatedness_2nd_period_IPC.csv", sep = ";", header = TRUE, dec=",")
Relatedness_3rd <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/Relatedness_3rd_period_IPC.csv", sep = ";", header = TRUE, dec=",")
Relatedness <- rbind(Relatedness_1st, Relatedness_2nd, Relatedness_3rd)
rm(Relatedness_1st, Relatedness_2nd, Relatedness_3rd)

Relatedness$Period <- gsub("1st", "Period 1 (1974-1988)", str_trim(Relatedness$Period))
Relatedness$Period <- gsub("2nd", "Period 2 (1989-2003)", str_trim(Relatedness$Period))
Relatedness$Period <- gsub("3rd", "Period 3 (2004-2018)", str_trim(Relatedness$Period))

Relatedness1 <- Relatedness[,c(1,2,3)]
names(Relatedness1) <- c("Country", "Period", "Value")
Relatedness1$Indicator <- "Overall Relatedness"

Relatedness2 <- Relatedness[,c(1,2,4)]
names(Relatedness2) <- c("Country", "Period", "Value")
Relatedness2$Indicator <- "AI-core fields"

Relatedness3 <- Relatedness[,c(1,2,5)]
names(Relatedness3) <- c("Country", "Period", "Value")
Relatedness3$Indicator <- "AI-related fields"

Relatedness4 <- Relatedness[,c(1,2,6)]
names(Relatedness4) <- c("Country", "Period", "Value")
Relatedness4$Indicator <- "Surrounding fields"

Relatedness <- rbind(Relatedness1, Relatedness2, Relatedness3, Relatedness4)
rm(Relatedness1, Relatedness2, Relatedness3, Relatedness4)

Relatedness$Indicator <- factor(Relatedness$Indicator, levels = c("Overall Relatedness", "AI-core fields",
                                                                  "AI-related fields", "Surrounding fields"))
Relatedness_AI <- Relatedness[Relatedness$Country == "AI", ]
Relatedness <- Relatedness[Relatedness$Country != "AI", ]

#coloured figures
library(RColorBrewer)
Rel_byP_c_Colour <- 
  ggplot(Relatedness, aes(x=Country, y=Value, fill=Period)) +
  geom_bar(stat="identity", position=position_dodge(), show.legend = F)+theme_minimal()+ xlab(NULL) + ylab("Relatedness") +
  facet_wrap(~Indicator, ncol = 4) + theme_classic() +
  ggtitle("Countries Relatedness in the considered IPC fields") + 
  scale_y_continuous(limits=c(.45,1.5),oob = rescale_none) + 
  scale_fill_brewer(palette = "YlOrRd")

Rel_byAI_c_Colour<- ggplot(Relatedness_AI, aes(x=Country, y=Value, fill=Period)) +
  geom_bar(stat="identity", position=position_dodge(), show.legend = F)+theme_minimal() + labs(x = "") +
  scale_x_discrete(labels = NULL) + ylab("Relatedness") +
  facet_wrap(~Indicator, ncol = 4)  + theme_classic() +
  ggtitle("AI Relatedness in the considered IPC fields")+
  scale_y_continuous(limits=c(.1,2.35),oob = rescale_none) + 
  scale_fill_brewer(palette = "YlOrRd")


#2.4.2.Knowld Comp. AI -----
KnowlComp_1st_AI <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/KnowledgeComp_PerCountry_1st_All_RCAs_AI.csv", sep = ";", header = TRUE, dec=",")
KnowlComp_2nd_AI <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/KnowledgeComp_PerCountry_2nd_All_RCAs_AI.csv", sep = ";", header = TRUE, dec=",")
KnowlComp_3rd_AI <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/KnowledgeComp_PerCountry_3rd_All_RCAs_AI.csv", sep = ";", header = TRUE, dec=",")

KnowlComp_1st_AI$Period <- "Period 1 (1974-1988)"
KnowlComp_2nd_AI$Period <- "Period 2 (1989-2003)"
KnowlComp_3rd_AI$Period <- "Period 3 (2004-2018)"
KnowledgeCompl_AI <- rbind(KnowlComp_1st_AI, KnowlComp_2nd_AI, KnowlComp_3rd_AI)
rm(KnowlComp_1st_AI, KnowlComp_2nd_AI, KnowlComp_3rd_AI)
KnowledgeCompl_AI <- KnowledgeCompl_AI[KnowledgeCompl_AI$X == "AI_pat3", ]
KnowledgeCompl_AI$"AI-core fields" = rowSums(KnowledgeCompl_AI[,c("X6", "X7", "X10", "X12")])
KnowledgeCompl_AI$"Overall Complexity" = rowSums(KnowledgeCompl_AI[,c(2:36)])
KnowledgeCompl_AI$"AI-related fields" = rowSums(KnowledgeCompl_AI[,c("X11", "X4", "X5")])
KnowledgeCompl_AI$"Surrounding fields" = rowSums(KnowledgeCompl_AI[,c("X3", "X2", "X1", "X13", "X25", "X34")])

KnowledgeCompl_AI2<- KnowledgeCompl_AI[,c(1, 38, 39)]
names(KnowledgeCompl_AI2) <- c("Country", "Period", "Value")
KnowledgeCompl_AI2$Indicator <- "AI-core fields"

KnowledgeCompl_AI3<- KnowledgeCompl_AI[,c(1, 38, 40)]
names(KnowledgeCompl_AI3) <- c("Country", "Period", "Value")
KnowledgeCompl_AI3$Indicator <- "Overall Complexity"

KnowledgeCompl_AI4<- KnowledgeCompl_AI[,c(1, 38, 41)]
names(KnowledgeCompl_AI4) <- c("Country", "Period", "Value")
KnowledgeCompl_AI4$Indicator <- "AI-related fields"

KnowledgeCompl_AI5<- KnowledgeCompl_AI[,c(1, 38, 42)]
names(KnowledgeCompl_AI5) <- c("Country", "Period", "Value")
KnowledgeCompl_AI5$Indicator <- "Surrounding fields"

KnowledgeCompl_AI_all <- rbind(KnowledgeCompl_AI2, KnowledgeCompl_AI3, KnowledgeCompl_AI4, KnowledgeCompl_AI5)
KnowledgeCompl_AI_all$Country <- gsub("AI_pat3", "AI", str_trim(KnowledgeCompl_AI_all$Country))
KnowledgeCompl_AI_all$Indicator <- factor(KnowledgeCompl_AI_all$Indicator, levels = c("Overall Complexity", "AI-core fields",
                                                                                      "AI-related fields", "Surrounding fields"))
Comp_byAI_c_Colour<- 
  ggplot(KnowledgeCompl_AI_all, aes(x=Country, y=Value, fill=Period)) +
  geom_bar(stat="identity", position=position_dodge())+theme_minimal() + labs(x = "") +
  scale_x_discrete(labels = NULL) + ylab("Knowledge Complexity (MORt)") +
  facet_wrap(~Indicator, ncol = 4) +
  theme_classic() + theme(legend.position="bottom") +
  ggtitle("AI Knowledge Complexity in the considered IPC fields")+ 
  scale_fill_brewer(palette = "YlOrRd")

#2.4.3.Knowld Comp. Countries -----
KnowlComp_1st <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/KnowledgeComp_1st_Morc.csv", sep = ";", header = F, dec=".")
KnowlComp_2nd <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/KnowledgeComp_2nd_Morc.csv", sep = ";", header = TRUE, dec=".")
KnowlComp_3rd <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/KnowledgeComp_3rd_Morc.csv", sep = ";", header = TRUE, dec=".")

names(KnowlComp_1st) <- c("Country", "MORc", "RCA", "RCA_step0", "RCA_step1", "RCA_step2")
names(KnowlComp_2nd) <- c("Country", "MORc", "RCA", "RCA_step0", "RCA_step1", "RCA_step2")
names(KnowlComp_3rd) <- c("Country", "MORc", "RCA", "RCA_step0", "RCA_step1", "RCA_step2")

KnowlComp_1st$Period <- "Period 1 (1974-1988)"
KnowlComp_2nd$Period <- "Period 2 (1989-2003)"
KnowlComp_3rd$Period <- "Period 3 (2004-2018)"

KnowledgeCompl <- rbind(KnowlComp_1st, KnowlComp_2nd, KnowlComp_3rd)
rm(KnowlComp_1st, KnowlComp_2nd, KnowlComp_3rd)
KnowledgeCompl$Category <- "Overall Complexity"

KnowlComp_1st_Top4 <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/KnowledgeComp_1st_Top4.csv", sep = ";", header = F, dec=".")
KnowlComp_2nd_Top4 <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/KnowledgeComp_2nd_Top4.csv", sep = ";", header = TRUE, dec=".")
KnowlComp_3rd_Top4 <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/KnowledgeComp_3rd_Top4.csv", sep = ";", header = TRUE, dec=".")

names(KnowlComp_1st_Top4) <- c("Country", "MORc", "RCA", "RCA_step0", "RCA_step1", "RCA_step2")
names(KnowlComp_2nd_Top4) <- c("Country", "MORc", "RCA", "RCA_step0", "RCA_step1", "RCA_step2")
names(KnowlComp_3rd_Top4) <- c("Country", "MORc", "RCA", "RCA_step0", "RCA_step1", "RCA_step2")

KnowlComp_1st_Top4$Period <- "Period 1 (1974-1988)"
KnowlComp_2nd_Top4$Period <- "Period 2 (1989-2003)"
KnowlComp_3rd_Top4$Period <- "Period 3 (2004-2018)"

KnowledgeCompl_Top4 <- rbind(KnowlComp_1st_Top4, KnowlComp_2nd_Top4, KnowlComp_3rd_Top4)
rm(KnowlComp_1st_Top4, KnowlComp_2nd_Top4, KnowlComp_3rd_Top4)
KnowledgeCompl_Top4$Category <- "AI-core fields"

KnowlComp_1st_Top3 <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/KnowledgeComp_1st_Top3.csv", sep = ";", header = F, dec=".")
KnowlComp_2nd_Top3 <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/KnowledgeComp_2nd_Top3.csv", sep = ";", header = TRUE, dec=".")
KnowlComp_3rd_Top3 <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/KnowledgeComp_3rd_Top3.csv", sep = ";", header = TRUE, dec=".")

names(KnowlComp_1st_Top3) <- c("Country", "MORc", "RCA", "RCA_step0", "RCA_step1", "RCA_step2")
names(KnowlComp_2nd_Top3) <- c("Country", "MORc", "RCA", "RCA_step0", "RCA_step1", "RCA_step2")
names(KnowlComp_3rd_Top3) <- c("Country", "MORc", "RCA", "RCA_step0", "RCA_step1", "RCA_step2")

KnowlComp_1st_Top3$Period <- "Period 1 (1974-1988)"
KnowlComp_2nd_Top3$Period <- "Period 2 (1989-2003)"
KnowlComp_3rd_Top3$Period <- "Period 3 (2004-2018)"

KnowledgeCompl_Top3 <- rbind(KnowlComp_1st_Top3, KnowlComp_2nd_Top3, KnowlComp_3rd_Top3)
rm(KnowlComp_1st_Top3, KnowlComp_2nd_Top3, KnowlComp_3rd_Top3)
KnowledgeCompl_Top3$Category <- "AI-related fields"

KnowlComp_1st_Surr <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/KnowledgeComp_1st_Surr.csv", sep = ";", header = F, dec=".")
KnowlComp_2nd_Surr <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/KnowledgeComp_2nd_Surr.csv", sep = ";", header = TRUE, dec=".")
KnowlComp_3rd_Surr <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/KnowledgeComp_3rd_Surr.csv", sep = ";", header = TRUE, dec=".")

names(KnowlComp_1st_Surr) <- c("Country", "MORc", "RCA", "RCA_step0", "RCA_step1", "RCA_step2")
names(KnowlComp_2nd_Surr) <- c("Country", "MORc", "RCA", "RCA_step0", "RCA_step1", "RCA_step2")
names(KnowlComp_3rd_Surr) <- c("Country", "MORc", "RCA", "RCA_step0", "RCA_step1", "RCA_step2")

KnowlComp_1st_Surr$Period <- "Period 1 (1974-1988)"
KnowlComp_2nd_Surr$Period <- "Period 2 (1989-2003)"
KnowlComp_3rd_Surr$Period <- "Period 3 (2004-2018)"

KnowledgeCompl_Surr <- rbind(KnowlComp_1st_Surr, KnowlComp_2nd_Surr, KnowlComp_3rd_Surr)
rm(KnowlComp_1st_Surr, KnowlComp_2nd_Surr, KnowlComp_3rd_Surr)
KnowledgeCompl_Surr$Category <- "Surrounding fields"

All_data_knowlComp_Morc <- rbind(KnowledgeCompl, KnowledgeCompl_Top4, KnowledgeCompl_Top3, KnowledgeCompl_Surr)
rm(KnowledgeCompl, KnowledgeCompl_Top4, KnowledgeCompl_Top3, KnowledgeCompl_Surr)
All_data_knowlComp_Morc<-All_data_knowlComp_Morc[All_data_knowlComp_Morc$Country == "US"|
                                                   All_data_knowlComp_Morc$Country == "CN"|
                                                   All_data_knowlComp_Morc$Country == "KR"|
                                                   All_data_knowlComp_Morc$Country == "JP",]

write.csv2(All_data_knowlComp_Morc, file = "Files_created_with_the_code/data/files_code_Fields_analysis/All_data_knowlComp_Morc.csv", row.names = F)
All_data_knowlComp_Morc <- read.csv("Files_created_with_the_code/data/files_code_Fields_analysis/All_data_knowlComp_Morc.csv", sep = ";", header = TRUE, dec=",")
All_data_knowlComp_Morc$Category <- factor(All_data_knowlComp_Morc$Category, levels = c("Overall Complexity", "AI-core fields",
                                                                                        "AI-related fields", "Surrounding fields"))
#figure colour:
Comp_byP_c_Colour <-
  ggplot(All_data_knowlComp_Morc, aes(x=Country, y=RCA_step1, fill=Period)) +
  geom_bar(stat="identity", position=position_dodge())+theme_minimal()+ xlab(NULL) + ylab("Knowledge Complexity (MORc)") +
  facet_wrap(~Category, ncol = 4) +
  theme_classic()  + theme(legend.position="bottom") +
  ggtitle("Countries Knowledge Complexity in the considered IPC fields") + 
  scale_fill_brewer(palette = "YlOrRd")

#regular resolution:
jpeg("Files_created_with_the_code/figures/regular_resolution_ALL_FIGURES_ARE_HERE/Fig3_Relatedness_and_Complex_AICol.jpg", width = 8, height = 6, units = 'in', res = 300)
multiplot(Rel_byAI_c_Colour, Comp_byAI_c_Colour, cols=1) 
dev.off()

#high resolution
jpeg("Files_created_with_the_code/figures/high_resolution/Fig3_Relatedness_and_Complex_AIColHigh.jpg", width = 8, height = 6, units = 'in', res = 800)
multiplot(Rel_byAI_c_Colour, Comp_byAI_c_Colour, cols=1) 
dev.off()

#low resolution
jpeg("Files_created_with_the_code/figures/low_resolution/Fig3_Relatedness_and_Complex_AIColLow.jpg", width = 8, height = 6, units = 'in', res = 72)
multiplot(Rel_byAI_c_Colour, Comp_byAI_c_Colour, cols=1) 
dev.off()

#regular resolution
jpeg("Files_created_with_the_code/figures/regular_resolution_ALL_FIGURES_ARE_HERE/Fig7_Relatedness_and_Complex_Morc_countriesColour.jpg", width = 8, height = 6, units = 'in', res = 300)
multiplot(Rel_byP_c_Colour, Comp_byP_c_Colour, cols=1) 
dev.off()

#high resolution
jpeg("Files_created_with_the_code/figures/high_resolution/Fig7_Relatedness_and_Complex_Morc_countriesColourHigh.jpg", width = 8, height = 6, units = 'in', res = 800)
multiplot(Rel_byP_c_Colour, Comp_byP_c_Colour, cols=1) 
dev.off()

#low resolution
jpeg("Files_created_with_the_code/figures/low_resolution/Fig7_Relatedness_and_Complex_Morc_countriesColourLow.jpg", width = 8, height = 6, units = 'in', res = 72)
multiplot(Rel_byP_c_Colour, Comp_byP_c_Colour, cols=1) 
dev.off()
