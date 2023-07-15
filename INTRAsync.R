# SCRIPT TO CALCULATE INTRAPERSONAL SYNCHRONY FROM HEAD AND BODY MOTION ENERGY TXT OUTPUT FILES

# load libraries
library('rMEA')
library('data.table')
library('dplyr')
library('stringr')
library('moments')
library('psych')
library('tibble')

# clean workspace
rm(list=ls())

### ROI ASSIGNMENT: ROI 1 and 2 --> diagnostician head and body; ROI 3 and 4 --> patient head and body
### NAMING OF MEA .TXT OUTPUT FILES: id.txt

## 1. READ IN DATA ---------------------------------------------------
# reads in MEA txt files in compatible format

# set path to MEA txt files
path <- "/PATH/TO/MEA/TXT/FILES"

# read in patient data
mea_patient <- readMEA(path,
                    sampRate = 30, #frame rate of videos
                    skip = 30, #skips the first 1s to remove artefacts introduced by MEA
                    s1Col = c(3), s2Col = c(4), #set columns according to original MEA ROI assignment
                    s1Name = "head", s2Name = "body", #names columns according to original MEA ROI assignment
                    header = FALSE,
                    idOrder = "id",
                    idSep = "$", 
                    sep = "")

mea_patient <- setGroup(mea_patient, "patient") #add to filename to distinguish between different ROIs



## 2. VISUAL INSPECTION OF RAW DATA -------------------------------------------------------
# saves histograms of raw data according to ROI to wd

dir.create("plots")

pdf(file="plots/raw_histograms_head.pdf")
for (i in 1:length(mea_head)){
  plot(mea_head[[i]], rescale = FALSE)
}
dev.off()

pdf(file="plots/raw_histograms_body.pdf")
for (i in 1:length(mea_body)){
  plot(mea_body[[i]], rescale = FALSE)
}
dev.off()

pdf(file="plots/diagnosticplot_head.pdf")
for (i in 1:length(mea_head)){
  diagnosticPlot(mea_head[[i]], width = 30)
}
dev.off()

pdf(file="plots/diagnosticplot_body.pdf")
for (i in 1:length(mea_body)){
  diagnosticPlot(mea_body[[i]], width = 30)
}
dev.off()



## 3. SCALING AND SMOOTHING -----------------------------------------------------------------
# applies moving average smoothing and scales motion energy time series by standard deviation

smoothed_mea_patient <- MEAsmooth(mea_patient, 0.5)

scaled_mea_patient <- MEAscale(smoothed_mea_patient)



## 4. VISUAL INSPECTION OF SMOOTHED DATA -------------------------------------------------------
# saves histograms of smoothed data according to ROI to wd

pdf(file="plots/smoothed_histograms_head.pdf")
for (i in 1:length(smoothed_mea_head)){
  plot(smoothed_mea_head[[i]], rescale = FALSE)
}
dev.off()

pdf(file="plots/smoothed_histograms_body.pdf")
for (i in 1:length(smoothed_mea_body)){
  plot(smoothed_mea_body[[i]], rescale = FALSE)
}
dev.off()

pdf(file="plots/smoothed_diagnosticplot_head.pdf")
for (i in 1:length(smoothed_mea_head)){
  diagnosticPlot(smoothed_mea_head[[i]], width = 30)
}
dev.off()

pdf(file="plots/smoothed_diagnosticplot_body.pdf")
for (i in 1:length(smoothed_mea_body)){
  diagnosticPlot(smoothed_mea_body[[i]], width = 30)
}
dev.off()



## 5. TIME SERIES SYNCHRONIZATION ---------------------------------------------
# calculate intrapersonal synchrony of head and body ROI within every participant

# Run CCF analysis on scaled time series
ccf_patient <- MEAccf(smoothed_mea_patient,
                   lagSec= 5, 
                   winSec = 30, 
                   incSec=15, 
                   r2Z = TRUE, 
                   ABS = TRUE)



## 6. PSEUDOSYNCHRONY ANALYSIS ---------------------------------------------
# compares previously computed to randomized synchrony

# shuffle datasets, create 1000 random pairings per ROI
random_patient <- shuffle(ccf_patient, 1000)

# calculate ccfs
pseudo_patient <- MEAccf(random_patient, 
                      lagSec= 5, 
                      winSec = 30, 
                      incSec=15, 
                      r2Z = T, 
                      ABS = T)

# boxplots
boxplot(getCCF(ccf_patient, type="grandAver"), getCCF(pseudo_patient, type="grandAver"))

# distribution of the ccf calculations against random matched dyads
MEAdistplot(ccf_patient, contrast = pseudo_patient)
MEAlagplot(ccf_patient, contrast = pseudo_patient)

# compute descriptive statistics
describe(getCCF(ccf_patient, type="grandAver"))
describe(getCCF(pseudo_patient, type="grandAver"))

# check for normal distribution
shapiro.test(getCCF(ccf_patient, type="grandAver")) 
shapiro.test(getCCF(pseudo_patient, type="grandAver"))

# check for unequal variances
var.test(getCCF(ccf_patient, type="grandAver"), getCCF(pseudo_patient, type="grandAver")) 

# compute t-test
t.test(getCCF(ccf_patient, type="grandAver"), getCCF(pseudo_patient, type="grandAver"), var.equal = F)

# effect size
cohens_d(getCCF(ccf_patient, type="grandAver"), getCCF(pseudo_patient, type="grandAver"))



## 7. VISUAL INSPECTION OF SYNCHRONY ANALYSIS -----------------------------------------------------
# saves heatmaps of ccf matrices to disk

# headsynchrony
pdf(file="plots/smoothed_heatmaps_INTRA_patient.pdf")
for (i in 1:length(ccf_patient)){
  MEAheatmap(ccf_patient[[i]], legendSteps = 20, rescale = T)
}
dev.off()



## 8. FEATURE EXTRACTION: INTRAPERSONAL SYNCHRONY --------------------------------------------------
# computes summary statistics of synchrony values

# combine lists
mea_ccf_all <- getCCF(ccf_patient, type = "fullMatrix")

# convert data frames to matrices in order to extract summary scores
for (i in 1:length(mea_ccf_all)){
  mea_ccf_all[[i]] <- as.matrix(mea_ccf_all[[i]])
}

# extract summary statistics
mean <- sapply(mea_ccf_all,mean,na.rm=T)
min <- sapply(mea_ccf_all,min,na.rm=T)
max <- sapply(mea_ccf_all,max,na.rm=T)
sd <- sapply(mea_ccf_all,sd,na.rm=T)
md <- sapply(mea_ccf_all,median,na.rm=T)
kurtosis <- sapply(mea_ccf_all,e1071::kurtosis,na.rm=T)
skew <- sapply(mea_ccf_all,e1071::skewness,na.rm=T)

# data wrangling: create one data frame in the format ID-summary statistics
mean <- as.data.frame(t(rbind(mean)))
mean <- rownames_to_column(mean, "id")

min <- as.data.frame(t(rbind(min)))
min <- rownames_to_column(min, "id")

max <- as.data.frame(t(rbind(max)))
max <- rownames_to_column(max, "id")

sd <- as.data.frame(t(rbind(sd)))
sd <- rownames_to_column(sd, "id")

md <- as.data.frame(t(rbind(md)))
md <- rownames_to_column(md, "id")

kurtosis <- as.data.frame(t(rbind(kurtosis)))
kurtosis <- rownames_to_column(kurtosis, "id")

skew <- as.data.frame(t(rbind(skew)))
skew <- rownames_to_column(skew, "id")

sumstats <- merge(mean,min,by="id")
sumstats <- merge(sumstats,max,by="id")
sumstats <- merge(sumstats,sd,by="id")
sumstats <- merge(sumstats,md,by="id")
sumstats <- merge(sumstats,kurtosis,by="id")
sumstats <- merge(sumstats,skew,by="id")

# modify id column
sumstats$id <- str_split_fixed(sumstats$id, "_", n=3)[,2]



## 9. EXPORT DATA TO WD ----------------------------------------------------------

# intra sync
write.csv(sumstats, "intra_sumstats.csv")

# save workspace
save.image("workspace_INTRAsync.RData")
