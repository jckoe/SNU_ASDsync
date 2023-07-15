# VOCAL OUTPUT ANALYSIS SCRIPT 

# load libraries
library(tidyverse)
library(dygraphs)
library(tuneR)
library(dplyr)

## DATA FORMAT: Sound/Silence Annotation Table extracted from PRAAT (Boersma, P., & Van Heuven, V. (2001). Speak and unSpeak with PRAAT. Glot International, 5(9/10), 341-347)
## ADDITIONALLY REQUIRED: File containing ID-group allocation


## 1. READ IN DATA ---------------------------------------------------------

path = '/PATH/TO/PRAAT/FILES'

files  <- list.files(path=path, 
                         recursive=T,
                         pattern="*.txt",
                         full.names=T,
                     include.dirs=F)

myfilelist <- lapply(files, read.table, header = T)

names(myfilelist) <- tools::file_path_sans_ext(basename(list.files(path=path, 
                                recursive=T,
                                pattern="*.txt",
                                full.names=T,
                                include.dirs=F)))

# descriptives

for (i in 1:length(myfilelist)){
  
  test <- myfilelist[[i]]
  test$duration <- test$tmax-test$tmin # adds duration column for each interval
  
  speech <- subset(test, text=="1") # chooses only speech columns
  mean_length_speech <- colMeans(speech)[4] # calculates mean duration of speech
  md_length_speech <- median(speech[,4]) # calculates median duration of speech intervals
  total_length_speech <- colSums(speech)[4] # calculates total duration of speech
  silence <- subset(test, text=="0") # chooses only silence columns
  mean_length_silence <- colMeans(silence)[4] # calculates mean duration of silence
  md_length_silence <- median(silence[,4]) # calculates median duration of silence intervals
  total_length_silence <- colSums(silence)[4] # calculates total duration of silence
  prop_speech <- total_length_speech/(total_length_speech+total_length_silence)*100 #frequency speech
  prop_silence <- total_length_silence/(total_length_speech+total_length_silence)*100 #frequency silence
  total_duration <- max(test$tmax) # total duration of file
  speechsilence_ratio <- total_length_speech/total_length_silence
  descriptives <- data.frame(total_duration, total_length_speech, total_length_silence, speechsilence_ratio, prop_speech, prop_silence, mean_length_speech, md_length_speech, mean_length_silence, md_length_silence)
  rownames(descriptives) <- c() #remove row names (otherwise 'duration')
  
  # export
  names <- files[[i]]   
  nam1 <- tools::file_path_sans_ext(basename(names))
  nam2<-strsplit(nam1,"\\_")[[1]]
  descriptives$ID <- nam2[1]
  write.csv(descriptives, paste(nam2[1], ".txt", sep = ""), row.names=FALSE) 
}



## 2. CREATE OVERALL SPEECH FILE FOR FURTHER ML ANALYSIS ---------------------------

files_preproc  <- list.files(path=getwd(),
                     recursive=T,
                     pattern="*.txt",
                     full.names=T,
                     include.dirs=F)

myfilelist_preproc <- lapply(files_preproc, read.table, header = T, sep=",")


names(myfilelist_preproc) <- tools::file_path_sans_ext(basename(list.files(path=getwd(), 
                                                                   pattern="*.txt",
                                                                   full.names=F)))

vocaloutput_all <- do.call(rbind.data.frame, myfilelist_preproc) 



# 3. DESCRIPTIVE GROUP COMPARISONS ----------------------------------------

# read in data
demo <- read.csv("/PATH/TO/FILE/CONTAINING/DIAGNOSTIC/GROUP")

# merge
all <- merge(demo,vocaloutput_all,by = "ID")

# grouped boxplot
longdf <- all %>%
  select('ID','diagnosis','speechsilence_ratio', 'prop_speech', 'prop_silence','mean_length_speech','mean_length_silence', 'md_length_speech','md_length_silence')  %>%
  pivot_longer(-c(ID,diagnosis),
               names_to='vocal_output',
               values_to='values')

# violin plots of all variables
pdf("violinplots_speech.pdf")
longdf %>%
  ggplot(aes(x=diagnosis, y=values, fill=diagnosis)) +
  geom_violin(position=position_dodge(0.75), alpha=0.8, trim = T) +
  geom_boxplot(aes(fill=diagnosis),position=position_dodge(0.75), width=0.2, alpha=0.2,outlier.shape = NA) +
  stat_boxplot(geom = 'errorbar', position=position_dodge(0.75), width=0.25)+
  facet_wrap(~vocal_output, scale="free") +
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())  +
  scale_fill_manual(name="Diagnostic Group",values = c("coral", "purple")) 
dev.off()

# violin plots speech silence ratio
pdf("speechsilenceratio.pdf")
all %>% ggplot(aes(x=diagnosis, y=speechsilence_ratio, fill=diagnosis)) +
  geom_violin(position=position_dodge(0.75), alpha=0.8, trim = T) +
  geom_boxplot(aes(fill=diagnosis),position=position_dodge(0.75), width=0.2, alpha=0.2,outlier.shape = NA) +
  stat_boxplot(geom = 'errorbar', position=position_dodge(0.75), width=0.25)+
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position = "none") +
  scale_fill_manual(name="Diagnostic Group",values = c("coral", "purple"))+
  ylab("Vocalization/Silence ratio")
dev.off()
