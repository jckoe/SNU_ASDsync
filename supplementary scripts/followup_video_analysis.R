### SCRIPT FOR SUPPLEMENTARY ANALYSES ON VIDEO DATA

# load libraries
library("readxl")
library("lubridate")
library("tidyr")
library("dplyr")
library("ggplot2")



## 1. VIDEO LENGTHS AND NUMBER OF TASKS PER VIDEO --------------------------
# compares video lengths and number of tasks between diagnostic groups

# read in data
data <- as.data.frame(read_xlsx("/PATH/TO/SPREADSHEET/CONTAINING/VIDEO/LENGTH"))

# time descriptives
mean(data$Length)
min(data$Length)
max(data$Length)

# transform to seconds
data$Length <- lubridate::period_to_seconds(lubridate::hms(data$Length))

# t test between groups
data %>%
  describeBy("Demo_Dx")

nd_length <- data %>% #check for normal distribution
  group_by(Demo_Dx) %>% 
  shapiro_test(Version2_Length,Version2_numberoftasks) %>%
  as.data.frame() #sign. 

stats_length <- data %>%
  pivot_longer(-c(Demo_Dx),names_to = "variables", values_to = "value") %>%
  group_by(variables) %>%
  t_test(value ~ Demo_Dx, var.equal = F, p.adjust.method = "BH") %>%
  add_significance() #n.s.

effsize_length <- data %>% 
  pivot_longer(-c(Demo_Dx),names_to = "variables", values_to = "value") %>%
  group_by(variables) %>%
  cohens_d(value ~ Demo_Dx, var.equal = F)



## 2. MOTION ENERGY SYNCHRONY PER ROOM ----------------------------------------------------
# checks for differences in motion energy synchrony between rooms

features <- read.csv("/PATH/TO/FEATURE/SPREADSHEET")
features$Room <- as.factor(features$Room)

# plot
features_long <- features %>%
  select(Room, colnames(features[grep("mean",colnames(features))])) %>% 
  pivot_longer(-Room, names_to = "variables", values_to = "value")

my_legend <- paste("Room ",levels(as.factor(features$Room))," (n=",table(as.factor(features$Room)),")",sep="")
my_xlab <- paste(c("Intrapersonal", "Body", "Head"))

pdf("roomdifferences.pdf")
ggplot(features_long, aes(x=variables, y=value, fill = as.factor(Room))) + 
  geom_boxplot(aes(fill=as.factor(Room)),position=position_dodge(0.75), width=0.5, alpha=0.2,outlier.shape = NA) +
  stat_boxplot(geom = 'errorbar', position=position_dodge(0.75), width=0.25) +
  theme(axis.title.x=element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  scale_fill_discrete(breaks=c("1","2","3","4"),
                      labels=my_legend) +
  labs(y = "Mean Synchrony") +
  scale_x_discrete(labels= my_xlab)
dev.off()

# decriptives
group_by(features, Room) %>%
  summarise(
    mean = mean(intra_mean, na.rm = TRUE),
    sd = sd(intra_mean, na.rm = TRUE)
  )

features %>%
  select(Room, colnames(features[grep("mean",colnames(features))])) %>%
  describeBy(features$Room)

# ANOVAs
summary(aov(mean_bodysync ~ as.factor(Room), features))
summary(aov(mean_headsync ~ as.factor(Room), features))
summary(aov(intra_mean ~ as.factor(Room), features))
