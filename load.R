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

samps = read.csv("./data/MS_SR_posteriors/Posterior_Samples_base_S_trunc.csv")
samps = as.matrix(samps)
