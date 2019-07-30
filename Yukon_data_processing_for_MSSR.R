########################################################################################
# Yukon_data_processing_for_MSSR.R
#
# Wrangle data into format required for multi-stock SS spawner-recruit modelling 
#
# Last updated: July 25, 2019
# Author: B. Connors (DFO)
#        
########################################################################################
library(tidyverse)

load("./data/runSize.Rdata")
agg_data <- read.csv("./data/age_and_harvest_data.csv")
agg_data <- subset(agg_data, year>1984 & year <2017)
CDN_u <- agg_data$yk_rv_har/(agg_data$spwn+agg_data$yk_rv_har)# CDN exploitation rate

# --- set values for list ----------------------------------------------------------------------------------
  ns <- 8
  
  nt <- 32
  
  na <- 4
  
  a_max <- 7
  
  C_tot_t_obs <- agg_data$total_har
    
  tau_C_obs <- round(C_tot_t_obs*0.15)#use CV of 0.15 (SE X sqrt(n)) = SD; SD^2 = var; CV = SD/mean; SD = CV*mean
    
  v <- rep(1,8)
    
  S_obs <- round(runSize$MLE*(1-rep(CDN_u,8)),0)
  
  S_obs_t <- rep(1:nt,8)
  
  S_obs_s <- sort(rep(1:8,nt))
  
  S_obs_n <- length(S_obs_s)
  
  tau_S_obs <- round(runSize$SE*(1-rep(CDN_u,8)),0)
  
  x_tas_obs <- round(agg_data[,14:17]*100); colnames(x_tas_obs) <- c("a4", "a5", "a6", "a7"); rownames(x_tas_obs) <- seq(1985,2016)
  
  ESS_ts <- matrix(NA,32,1); ESS_ts[,1] <- (rowSums(x_tas_obs)); rownames(ESS_ts) <- seq(1985,2016); colnames(ESS_ts) <- c("aggregate"); ESS_ts <- as.data.frame(ESS_ts)

  R_wish <- matrix(0,ns,ns)
  
  diag(R_wish) <- rep(1,8)
  
  df_wish <- 9
 
# --- create list ----------------------------------------------------------------------------------
Yukon_chinook_data_for_BS <- list("ns" = ns, 
                                "nt" = nt, 
                                "na" = na, 
                                "a_max" = a_max,
                                "C_tot_t_obs" = C_tot_t_obs,
                                "tau_C_obs" = tau_C_obs,
                                "v" = v,
                                "S_obs" = S_obs, 
                                "S_obs_t" = S_obs_t,
                                "S_obs_s" = S_obs_s,
                                "S_obs_n" = S_obs_n,
                                "tau_S_obs" = tau_S_obs,
                                "x_tas_obs" = x_tas_obs,
                                "ESS_ts" = ESS_ts,
                                "R_wish" = R_wish, 
                                "df_wish" = df_wish) 

saveRDS(Yukon_chinook_data_for_BS,"./outputs/Yukon_data_for_BS_July24_2019.RDS")
  