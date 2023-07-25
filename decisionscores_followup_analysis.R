# SCRIPT FOR FOLLOWUP ANALYSES OF SVM DECISION SCORES

# load libraries
library("tidyr")
library("tidyverse")
library("rstatix")
library("readxl")
library("rempsyc")
library("ggplot2")


options(scipen = 10)

# set wd
setwd("")

# read in data
decision_scores <- read.table("PATH/TO/CSV/FILE/AS/EXPORTED/FROM/NEUROMINER")
data_corr_full <- read_xlsx("/PATH/TO/ADOS/SPREADSHEET")



## 1. CORRELATION BETWEEN AGE AND DECISION SCORES -----------------------------
# checks for any residual correlation between age and SVM scores after regressing out age

data_corr_age <- data_corr_full %>%
  select(Mean_Score, Demo_Age) 

corr_age <- psych::corr.test(data_corr_age[-1], data_corr$Mean_Score, adjust="fdr")
corr_age$p

apaTables::apa.cor.table(data_corr_age, filename = NA, table.number = NA,
                         show.conf.interval = TRUE, landscape = TRUE)



## 2. CORRELATION BETWEEN ADOS AND DECISION SCORES ----------------------------

# select SVM and clinical scores
data_corr_ados <- data_corr_full %>%
  select(Mean_Score, ADOS_SA_CSS, ADOS_RRB_CSS, ADOS_Total_CSS, ADI_R_A, ADI_R_B, ADI_R_C) #note: CSS = calibrated severity score

# get correlations and respective p values
corr_clinicalscores <- psych::corr.test(data_corr_ados[-1], data_corr_ados$Mean_Score, adjust="fdr")
corr_clinicalscores$p.adj

apaTables::apa.cor.table(data_corr_ados, filename = NA, table.number = NA,
              show.conf.interval = TRUE, landscape = TRUE)

# scatter plot (faceted)
data_asdcc_long <- data_corr_full %>%
  select(Demo_Dx, Mean_Score, ADOS_SA_CSS, ADOS_RRB_CSS, ADOS_Total_CSS,ADI_R_A, ADI_R_B, ADI_R_C) %>% 
  gather(key = "variable", value = "value",
         -Mean_Score, -Demo_Dx) %>% 
  mutate(rater = case_when(str_detect(variable,"ADOS") ~ "ADOS",
                           str_detect(variable,"ADI") ~ "ADI"))

new_labels <- c("ADI_R_A" = "Reciprocal Social Interaction", 
                "ADI_R_B" = "Social Communication", 
                "ADI_R_C" = "Restricted and Repetitive Behaviors", 
                "ADOS_RRB_CSS" = "Restricted and Repetitive Behaviors",
                "ADOS_SA_CSS" = "Social Affect", 
                "ADOS_Total_CSS" = "Total Score")

pdf("corr_separately.pdf", height = 5.83, width = 8.27)
ggplot(data_asdcc_long, aes(x = value, y = Mean_Score)) + 
  geom_point(aes(color = Demo_Dx)) +
  facet_wrap(rater~variable, scales="free", labeller = labeller(variable = new_labels))+
  scale_color_manual(values = c("coral", "purple")) +
  geom_smooth(method='lm', se=FALSE, linewidth = 0.5, color = "black") +
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.line = element_line(colour = "black"),
        legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.spacing = unit(0,"points"),
        panel.spacing.y = unit(1,"cm"))  +
  ylab("Mean SVM Decision Scores") +
  scale_x_continuous(breaks = scales::breaks_pretty()) # only integer breaks
dev.off()

