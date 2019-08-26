########################################################################################
# YT_close_loop_sims.R
#
# Closed-loop simulation of alternative harvest policies for CDN-Yukon Chinook
# July 24, 2019
# Author: B. Connors (DFO)
#        
########################################################################################

# --- Set common conditions for simulations  --------------------------------------------
num.sims <- 500 # number of Monte Carlo trials
ny <- 50 # number of years in forward simulation
pm.yr <- ny-20

US_sub <- 20000 # US subsistence target
CDN_sub <- 10000 # CDN basic needs allocation target
for_error <- 0.2 # log-normal forecast error
US_com_error <- 0.1 # log-normal outcome uncertianty in US commerical harvest
US_sub_error <- 0.05 # log-normal outcome uncertianty in US subsistence harvest
BP_error <- 0.05 # log-normal border passage measurement error
CDN_sub_error <- 0.05 # log-normal outcome uncertianty in CDN FN harvest

t_harvest_rate <- seq(0,1,length.out=40) # range of US target commercial harvest rates
BPG <- seq(1,100000,length.out=40) # range of border passage goals

# --- Create array to store outcomes ----------------------------------------------------
sim.outcomes <- array(NA,dim=c(length(BPG),10, length(t_harvest_rate),num.sims))
sim.outcomes.spw.time <- array(NA,dim=c(ny,8,length(BPG),length(t_harvest_rate),num.sims))

# --- Stationary Ricker SR dynamics ----------------------------------------------------

# set structural form of SR relationship
  SR_rel <-  "Beverton-Holt" 
  #dir.SR <- "F"
  #SR_devs <- array(1,dim=c(ny,2,13))

# run simulations
  ptm <- proc.time()
  
  for (w in 1:length(t_harvest_rate)){
    for (k in 1:length(BPG)){
      for (l in 1: num.sims){
      draw <- sample(10000,1)
        out <- process(ny = ny,
                       vcov.matrix = process.iteration(samps[draw,])$Sigma_R,
                       phi = process.iteration(samps[draw,])$phi,
                       mat = process.iteration(samps[draw,])$pis,
                       alpha = process.iteration(samps[draw,])$alpha,
                       beta = process.iteration(samps[draw,])$beta ,
                       BPG = BPG[k],
                       pm.yr = pm.yr,
                       Rec = process.iteration(samps[draw,])$R ,
                       Spw = process.iteration(samps[draw,])$S ,
                       last.resid = process.iteration(samps[draw,])$last_resid,
                       SR_rel = SR_rel, 
                       t_harvest_rate = t_harvest_rate[w])
        sim.outcomes[k,,w,l] <- out$PMs
        sim.outcomes.spw.time[,,k,w,l] <- out$S
      }
    }
  }
  
saveRDS(sim.outcomes,"./outputs/base_sims.BH")  
saveRDS(sim.outcomes.spw.time,"./outputs/base_sims_projections.BH")  

