##------------------------------------------------------------------------------#
# Load functions and libraries for analysis
#------------------------------------------------------------------------------#

source("functions.R") 

library(MASS)
library(mvtnorm)
library(plotrix)
library(matrixStats)
library(mgcv)
library(plyr)
library(fields)
library(gdata)
library(graphics)
library(fields)
library(dichromat)
library(grDevices)
library(viridis)
library(shape)
library(autoimage)
library(tidyverse)
library(plyr)

#------------------------------------------------------------------------------#
# Load posterior samples
#------------------------------------------------------------------------------#

samps_C = read.csv("./data/MS_SR_posteriors/Posterior_Samples_C.csv")
samps_D = read.csv("./data/MS_SR_posteriors/Posterior_Samples_D.csv")
samps_E = read.csv("./data/MS_SR_posteriors/Posterior_Samples_E.csv")

samps <- rbind(samps_C, samps_D, samps_E)

samps = as.matrix(samps)

samps = samps[,c(1:97,126:129,158:161,190:193,222:225,254:257,286:289,318:321,350:353,386:388,421:423,456:458,493:491,526:528,561:563,596:598,631:633,668,703,738,773,808,843,878,913,926:929)]
colnames(samps)[154:161] <- c("last_resid_1","last_resid_2","last_resid_3","last_resid_4","last_resid_5","last_resid_6","last_resid_7","last_resid_8")
