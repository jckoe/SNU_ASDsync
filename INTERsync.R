# SCRIPT TO CALCULATE INTERPERSONAL SYNCHRONY FROM HEAD AND BODY MOTION ENERGY TXT OUTPUT FILES

# load libraries
library('rMEA')
library('data.table')
library('dplyr')
library('stringr')
library('e1071')
library('psych')
library('cowplot')

# clean workspace
rm(list=ls())

### ROI ASSIGNMENT: ROI 1 and 2 --> diagnostician head and body; ROI 3 and 4 --> patient head and body
### NAMING OF MEA .TXT OUTPUT FILES: id.txt


## 1. READ IN DATA ---------------------------------------------------
# reads in MEA txt files in compatible format

# set path to MEA txt files
path <- "/PATH/TO/MEA/TXT/FILES"

# read in head ROI data
mea_head <- readMEA(path,
                    sampRate = 30, #frame rate of videos
                    skip = 30, #skips the first 1s to remove artefacts introduced by MEA
                    s1Col = c(1), s2Col = c(3), #set columns according to original MEA ROI assignment
                    s1Name = "diagnostician", s2Name = "patient", #names columns according to original MEA ROI assignment
                    header = FALSE,
                    idOrder = "id",
                    idSep = "$", 
                    sep = "")

mea_head <- setGroup(mea_head, "head") #add to filename to distinguish between different ROIs


# read in body ROI data 
mea_body <- readMEA(path,
                    sampRate = 30, 
                    skip = 30, 
                    s1Col = c(2), s2Col = c(4), 
                    s1Name = "diagnostician", s2Name = "patient", 
                    header = FALSE,
                    idOrder = "id",
                    idSep = "$", 
                    sep = "")

mea_body <- setGroup(mea_body, "body") 



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

smoothed_mea_head <- MEAsmooth(mea_head, 0.5)
smoothed_mea_body <- MEAsmooth(mea_body, 0.5)

scaled_mea_head <- MEAscale(smoothed_mea_head)
scaled_mea_body <- MEAscale(smoothed_mea_body)



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
# calculates interpersonal movement synchrony of head and body ROI between interaction partners

# Run CCF analysis on scaled time series
ccf_head <- MEAccf(smoothed_mea_head,
                   lagSec= 5, 
                   winSec = 60, 
                   incSec=30, 
                   r2Z = TRUE, 
                   ABS = TRUE)

ccf_body <- MEAccf(smoothed_mea_body,
                   lagSec= 5, 
                   winSec = 60, 
                   incSec=30, 
                   r2Z = TRUE, 
                   ABS = TRUE)



## 6. PSEUDOSYNCHRONY ANALYSIS ---------------------------------------------
# compares previously computed to randomized synchrony

# shuffle datasets, create 500 random pairings per ROI
random_head <- shuffle(ccf_head, size = 500)
random_body <- shuffle(ccf_body, size = 500)

# calculate ccfs
pseudo_head <- MEAccf(random_head, 
                      lagSec= 5, 
                      winSec = 60, 
                      incSec=30, 
                      r2Z = T, 
                      ABS = T)

pseudo_body <- MEAccf(random_body, 
                      lagSec= 5, 
                      winSec = 60, 
                      incSec=30, 
                      r2Z = T, 
                      ABS = T)

# boxplots
boxplot(getCCF(ccf_head, type="grandAver"), getCCF(pseudo_head, type="grandAver"))
boxplot(getCCF(ccf_body, type="grandAver"), getCCF(pseudo_body, type="grandAver"))

# plots distribution of the ccf calculations against random matched dyads
distp1 <- ~MEAdistplot(ccf_head, contrast = pseudo_head, 
                       xlab = "average cross-correlation", main = "Head Synchrony")
distp2 <- ~MEAdistplot(ccf_body, contrast = pseudo_body, 
                       xlab = "average cross-correlation", main = "Body Synchrony")

tiff("distplot.tiff", width = 8, height = 5, res = 300, units = "in")
plot_grid(distp1, distp2, labels = c('A', 'B'))
dev.off()

MEAlagplot(ccf_head, contrast = pseudo_head)
MEAlagplot(ccf_body, contrast=pseudo_body)

# compute descriptive statistics
describe(getCCF(ccf_head, type="grandAver"))
describe(getCCF(pseudo_head, type="grandAver"))

describe(getCCF(ccf_body, type="grandAver"))
describe(getCCF(pseudo_body, type="grandAver"))

# check for normal distribution
shapiro.test(getCCF(ccf_head, type="grandAver")) 
shapiro.test(getCCF(pseudo_head, type="grandAver"))
shapiro.test(getCCF(ccf_body, type="grandAver")) 
shapiro.test(getCCF(pseudo_body, type="grandAver"))

# check for unequal variances
var.test(getCCF(ccf_head, type="grandAver"), getCCF(pseudo_head, type="grandAver")) 
var.test(getCCF(ccf_body, type="grandAver"), getCCF(pseudo_body, type="grandAver"))  

# compute t-test
t.test(getCCF(ccf_head, type="grandAver"), getCCF(pseudo_head, type="grandAver"), var.equal = T)
t.test(getCCF(ccf_body, type="grandAver"), getCCF(pseudo_body, type="grandAver"), var.equal = T)

# effect size
cohens_d(getCCF(ccf_head, type="grandAver"), getCCF(pseudo_head, type="grandAver"))
cohens_d(getCCF(ccf_body, type="grandAver"), getCCF(pseudo_body, type="grandAver"))



## 7. VISUAL INSPECTION OF SYNCHRONY ANALYSIS -----------------------------------------------------
# saves heatmaps of ccf matrices to wd 

# headsynchrony
pdf(file="plots/smoothed_heatmaps_IPS_head.pdf")
for (i in 1:length(ccf_head)){
  MEAheatmap(ccf_head[[i]], legendSteps = 20, rescale = T)
}
dev.off()

# bodysynchrony
pdf(file="plots/smoothed_heatmaps_IPS_body.pdf")
for (i in 1:length(ccf_body)){
  MEAheatmap(ccf_body[[i]], legendSteps = 20, rescale = T)
}
dev.off()



## 8. FEATURE EXTRACTION: HEAD AND BODY SYNCHRONY ---------------------------
# computes summary statistics of synchrony values

# combine lists
mea_ccf_all <- c(getCCF(ccf_head, type = "fullMatrix"),
                 getCCF(ccf_body, type = "fullMatrix"))

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
mean <- tibble::rownames_to_column(mean, "id")

min <- as.data.frame(t(rbind(min)))
min <- tibble::rownames_to_column(min, "id")

max <- as.data.frame(t(rbind(max)))
max <- tibble::rownames_to_column(max, "id")

sd <- as.data.frame(t(rbind(sd)))
sd <- tibble::rownames_to_column(sd, "id")

md <- as.data.frame(t(rbind(md)))
md <- tibble::rownames_to_column(md, "id")

kurtosis <- as.data.frame(t(rbind(kurtosis)))
kurtosis <- tibble::rownames_to_column(kurtosis, "id")

skew <- as.data.frame(t(rbind(skew)))
skew <- tibble::rownames_to_column(skew, "id")

sumstats <- merge(mean,min,by="id")
sumstats <- merge(sumstats,max,by="id")
sumstats <- merge(sumstats,sd,by="id")
sumstats <- merge(sumstats,md,by="id")
sumstats <- merge(sumstats,kurtosis,by="id")
sumstats <- merge(sumstats,skew,by="id")

# add ROI column
sumstats$roi <- str_split_fixed(sumstats$id, "_", n=3)[,1]
sumstats$id <- str_split_fixed(sumstats$id, "_", n=3)[,2]

  

## 9. FEATURE EXTRACTION: MOVEMENT QUANTITY ----------------------------------------------------
# extracts movement variables from MEA summary

# select columns with movement quantity values
movementquantity_head <- summary(ccf_head)[,c(1,3:5)]
movementquantity_body <- summary(ccf_body)[,c(1,3:5)]

# remove row names
rownames(movementquantity_head) <- NULL
rownames(movementquantity_body) <- NULL

# change column names
colnames(movementquantity_head) <- c("id", "roi", "diagnostician", "patient")
colnames(movementquantity_body) <- c("id", "roi", "diagnostician", "patient")

# combine in single dataframe
movementquantity_all <- rbind(movementquantity_head,movementquantity_body)



## 10. EXPORT DATA TO WD ----------------------------------------------------------

# movement sync
write.csv(sumstats, "INTERsync.csv")

# movement quantity
write.csv(movementquantity_all, "movementquantity.csv")

# save workspace
save.image("workspace_INTERsync.RData")
