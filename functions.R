########################################################################################
# functions.R
#
# Functions for CDN-Yukon Chinook closed loop simulations and associated analyses 
# July 24, 2019
# Author: B. Connors (DFO) 
#        
########################################################################################

#------------------------------------------------------------------------------#
# CDN-origin Yukon Chinook harvest control rule function
#------------------------------------------------------------------------------#
# run <- run size (i.e., pre-harvest recruitment) 
# BPG < - border passage goal
# for_error <- forecast error
# US_sub <- US subsistence target
# CDN_sub <- CDN First Nations harvest target (basic needs allocation) 
# US_com_U <- US commercial target harvest rate
# US_com_error <- lognormal outcome uncertainty in US commerical harvest
# US_sub_error <- lognormal outcome uncertainty in US subsistence harvest
# BP_error <- border passage lognormal measurement error
# CDN_sub_error <- lognormal outcome uncertainty in CDN subsistence harvest

hcr = function(run, BPG, US_sub, CDN_sub, US_com_U ,for_error, US_com_error, US_sub_error, BP_error, CDN_sub_error){
    run.est <- run * rlnorm(1, 0, for_error)
    TAC <- run.est - BPG
    CDN_TAC <- ifelse(TAC < 110, 0.24*TAC, ((0.24*TAC)+0.5*(TAC-110)))
    US_TAC = TAC - CDN_TAC
    pot_com_fish <- ifelse(US_TAC-US_sub > 0, US_TAC-US_sub, 0)
    US_com_catch <- US_com_U * (pot_com_fish) * rlnorm(1, 0, US_com_error)
    US_sub_catch_1 <- US_sub * rlnorm(1, 0, US_sub_error)
    US_sub_catch <- ifelse(US_sub_catch_1 > run-US_com_catch, run-US_com_catch, US_sub_catch_1)
  	border_passage = (run - US_com_catch-US_sub_catch) * rlnorm(1, 0, BP_error)
  	CDN_sub_catch_1 <- CDN_sub * rlnorm(1, 0, CDN_sub_error)
  	CDN_sub_catch <- ifelse(CDN_sub_catch_1 > border_passage, border_passage, CDN_sub_catch_1)
  	escapement <- border_passage - CDN_sub_catch
	return(c(US_com_catch, US_sub_catch, border_passage, CDN_sub_catch, escapement))
}


#------------------------------------------------------------------------------#
# Status function (to estimate whether stock is overfished or predicted to go 
#  extinct at a given harvest rate, over the long-term) 
#------------------------------------------------------------------------------#
# U <- harvest rate
# a <- productivity (NOT in log space) 
# b <- density dependence (Ricker beta parameter) 
SC.eq <- function(U,a,b){
    a <- log(a)
    S.eq <- max(0,(a-(-log(1-U)))/b)
    C.eq <- max(0,((a-(-log(1-U)))/b)*exp(a-b*((a-(-log(1-U)))/b))-((a-(-log(1-U)))/b))
    OF <- ifelse(U>0.5*a-0.07*a^2,1,0)
    EX <- ifelse(S.eq==0,1,0)
  return(c(S.eq,C.eq,OF,EX))
}

#------------------------------------------------------------------------------#
# Multi-stock simulation function with alternative structural forms
#------------------------------------------------------------------------------#
# ny <- the number of years
# vcov.matrix <- process error variance-covarance matrix
# phi <- the expected correlation through time
# mat <- maturation schedule
# alpha <- sub-stock productivity (NOT in log space)
# beta <- sub-stock density depedence 
# BPG <- border passage goal
# pm.yr <- year of simulation that pms start to be calculated over
# Rec <- estimated recruitments from last years of empirical data 
# Spw <- estimated spawers from last years of empirical data
# lst.resid <- estimated recruitment deviation from last year of empirical data
# SR_rel <- structural form of the SR relationship ("Ricker" or "Beverton-Holt")
# t_harvest_rate <- target harvest rate on US commercial catch

# BH.alpha.CV <- magnitude (amplitude) of environmental forcing on alpha if SR_rel = "Beverton-Holt"
# period <- period of enviro forcing cycle if SR_rel = "Beverton-Holt"
# alpha_scalar <- alpha scalar if using BH spawner-recruit formulation

process = function(ny,vcov.matrix,phi,mat,alpha,beta,BPG,pm.yr,Rec,Spw,lst.resid,SR_rel,t_harvest_rate){
	ns = length(alpha) #number of sub-stocks
	m.alpha <- alpha
	m.beta <- beta

	# create vectors of time varying alpha
	if (SR_rel == "Beverton-Holt"){ 
	  alpha <- alpha* alpha_scalar
	  beta.tim <- (alpha/beta)*exp(-1)
	  alpha.time <- matrix(NA,ny,length(alpha))
	  for (t in 1:ny){
	    alpha.time[t,] <- sin(2*pi*(t/period))*((alpha + (alpha * BH.alpha.CV)) - alpha) + alpha
	  }
	}
	
	#Create recruitment deviations that are correlated among stocks 
	epi = rmvnorm(ny, sigma= vcov.matrix)

	#Build time series of Spawners (S), abundance of returning spawners pre-harvest
	# (N), and the component of the residual that is correlated throught time (v)
	R = t(matrix(0,ns,ny))
	S = R * (1-0)
	v = R; v[,]=0
	R[1:3,]=Rec
	N = array(0,dim=c(ny,4,ns))
	Ntot = R; Ntot[,]=0
	H = Ntot; S = Ntot; USsub = Ntot; CDNFN = Ntot
	S[4:7,] = Spw
	predR = Ntot
	
	# populate first few years with realized states
	R[4,] = alpha[]*S[4,]*exp(-beta[]*S[4,]+(phi*lst.resid)+epi[4,])
	predR[4,] = alpha[]*S[4,]*exp(-beta[]*S[4,])
	v[4,] = log(R[4,])-log(predR[4,])
	v[v[,]=='NaN'] <- 0

	for(i in 5:7){
	  R[i,] = alpha[]*S[i,]*exp(-beta[]*S[i,]+phi*v[i-1,]+epi[i,])
	  predR[i,] = alpha[]*S[i,]*exp(-beta[]*S[i,])
	  v[i,] = log(R[i,])-log(predR[i,])
	  v[v[,]=='NaN'] <- 0	
	  }

	N[4:7,1,]=R[4:7-(3),] * mat[1]
	N[5:7,2,]=R[5:7-(4),] * mat[2]
	N[6:7,3,]=R[6:7-(5),] * mat[3]
	N[7,4,]=R[7-(6),] * mat[4]
	
	# Loop through years of simulation	
	for(i in (7+1):ny){
		N[i,1,] = R[i-(4),] * mat[1]
		N[i,2,] = R[i-(5),] * mat[2]
		N[i,3,] = R[i-(6),] * mat[3]
		N[i,4,] = R[i-(7),] * mat[4]
		Ntot[i,] = colSums(N[i,,])

		# apply harvest control rule
		run.size <- sum(Ntot[i,])
		hcr_outcomes <- hcr(run.size, BPG, US_sub, CDN_sub, t_harvest_rate ,for_error, US_com_error, US_sub_error, BP_error, CDN_sub_error )
		hcr_exploit <- sum(hcr_outcomes[c(1,2,4)])/run.size
		USsub[i] <- hcr_outcomes[2]
		CDNFN[i] <- hcr_outcomes[4]
		H[i,] =  hcr_exploit*Ntot[i,]
		S_exp = Ntot[i,]-H[i,]
		S_exp[S_exp<0] = 0
		S_exp[S_exp<50] = 0 # quasi-extinction threshold
		S[i,] = S_exp

		# predict recruitment
		if (SR_rel == "Ricker"){
			R[i,] = alpha[]*S[i,]*exp(-beta[]*S[i,]+phi*v[i-1,]+epi[i,])
			predR[i,] = alpha[]*S[i,]*exp(-beta[]*S[i,])
			v[i,] = log(R[i,])-log(predR[i,])
			v[v[,]=='NaN'] <- 0
	    }
	
		if (SR_rel == "Beverton-Holt"){
			R[i,] = alpha.time[i,]*S[i,]/(1+(alpha.time[i,]/beta.tim[])*S[i,])*exp(phi*v[i-1,]+epi[i,])
			predR[i,] = alpha.time[i,]*S[i,]/(1+(alpha.time[i,]/beta.tim[])*S[i,])
			v[i,] = log(R[i,])-log(predR[i,])
			v[v[,]=='NaN'] <- 0
		  }
	 }
	 
	# Performance measures:
	#	1: escapement
	#	2: harvest
	#	3: harvest rate
	#	4: predicted overfished
	#	5: predicted trending towards extinction
	#	6: empirical extinction
	#	7: proportion of years failed to meet US subsistence goal
	#	8: proportion of years failed to meet CDN FN harvest goal
	#	9: CV in harvest
	#	10: proportion of tributary goals met
	
	pms <- matrix(NA,1,10) 
	S[S[,]=='NaN'] <- 0
	H[H[,]=='NaN'] <- 0
	Ntot[Ntot[,]=='NaN'] <- 0
	over <- matrix(NA,length(alpha))
	ext <- matrix(NA,length(alpha))
	ext.emp <- ext
	trib.gl <- ext
	harvest_rate <- (H[pm.yr:ny,]/Ntot[pm.yr:ny,])[,1]
	harvest_rate[harvest_rate>1] <- 1
	harvest_rates <- (H[pm.yr:ny,]/Ntot[pm.yr:ny,])
	harvest_rates[harvest_rates>1] <- 1
	harvest_rate[harvest_rate=='NaN']<-1
	harvest_rates[harvest_rates=='NaN']<-1
	Smax <- round((m.alpha/m.beta)/m.alpha,digits=0)  
	ln.alpha <- log(m.alpha)
	Smsy <- round((ln.alpha*(0.5-0.07* ln.alpha))/m.beta)

	for(j in 1:length(alpha)){
		over[j] <- SC.eq(median(harvest_rates[,j]),alpha[j],beta[j])[3]
		ext[j] <- SC.eq(median(harvest_rates[,j]),alpha[j],beta[j])[4]
		ext.emp[j] <- ifelse(median(S[(ny-pm.yr):ny,j]) < ((log(alpha)/beta)*0.05)[j],1,0) # less than 5% of unfished biomass/abundance
		trib.gl[j] <- ifelse(median(S[(ny-pm.yr):ny,j]) >= (Smsy[j]),1,0)
		}

	pms[,1] <- sum(S[pm.yr:ny,])/(ny - pm.yr +1)
	pms[,2] <- sum(H[pm.yr:ny,])/(ny - pm.yr +1)
	pms[,3] <- median(harvest_rate)
	pms[,4] <- sum(over)/length(alpha)
	pms[,5] <- sum(ext)/length(alpha)
	pms[,6] <- sum(ext.emp)/length(alpha)
	pms[,7] <- sum(rowSums(USsub[pm.yr:ny,]) >= (US_sub*0.9))/(ny - pm.yr +1)
	pms[,8] <- sum(rowSums(CDNFN[pm.yr:ny,]) >= (CDN_sub*0.9))/(ny - pm.yr +1)
	pms[,9] <- sd(H[pm.yr:ny,])/mean(H[pm.yr:ny,]) 
	pms[,10] <- sum(trib.gl)/length(alpha) 

	list(S=S[,],N=Ntot[,],H=H[,],PMs=pms)
}

#------------------------------------------------------------------------------#
# Functions to process posterior samples
#------------------------------------------------------------------------------#
# Yukon stocks are in this order 
 # 1.) YC
 # 2.) YlC
 # 3.) Ym
 # 4.) YP
 # 5.) YS
 # 6.) YT
 # 7.) Yu
 # 8.) YWD

process.iteration = function(samp) {
  # 1.) extract names
  nms = names(samp)
  A = 4
  ns = 8
  # 2.) extract elements according to the names and put them into the appropriate data structure
  
  # parameters
  alpha = unname(samp[substr(nms, 1, 5) == "alpha"])
  beta = unname(samp[substr(nms, 1, 4) == "beta"])
  last_resid = unname(samp[substr(nms, 1, 10) == "last_resid"])
  phi = mean(unname(samp[substr(nms, 1, 3) == "phi"])) # for now take average across stocks
  
  Sigma_R = matrix(samp[substr(nms, 1, 7) == "Sigma_R"], ns, ns)# this is a temporary hack
  diagonal <- diag(Sigma_R)
  Sigma_R <- matrix(0.5, nrow(Sigma_R), ncol(Sigma_R))  
  diag(Sigma_R) <- diagonal
 
  pis = c(as.numeric(samp["pi_1"]), as.numeric(samp["pi_2"]), as.numeric(samp["pi_3"]), as.numeric(samp["pi_4"]))
  
  # states
  S = matrix(samp[substr(nms, 1, 2) == "S_"], A, ns)
  R = matrix(samp[substr(nms, 1, 2) == "R_"], A - 1, ns)
  
  # 3.) create output list
  output = list(
    alpha = alpha,
    beta = beta,
    phi = phi,
    last_resid = last_resid,
    Sigma_R = Sigma_R,
    S = S,
    R = R,
    pis = pis
  )

  # 4.) return output
  return(output)

}

#-------------------------------------------------------------------------------------------------------#
# Portfolio effect function (Nesbitt and Moore. JAE. 2012)
#-------------------------------------------------------------------------------------------------------#
# data <- dataset
# first_year <- first year of window over which to calcualte portfolio effects
# last_year <- last year of window over which to calcualte portfolio effects
PE_running <- function(data,first_year,last_year){
  
  rs_data <- data	
  rs <- subset(rs_data,year>= first_year & year<last_year)
  
  pop_CV <- ddply(rs,c("stock"),function(x){
    CV <-sd(x$MLE,na.rm=T)/mean(x$MLE,na.rm=T)
    abund<-sum(x$MLE,na.rm=T)
    data.frame(CV,abund)
  })
  
  
  total.esc <- ddply(rs,c("year"),function(x){
    total.esc<-sum(x$MLE,na.rm=T)
    data.frame(total.esc)
  })
  
  CV_all<-sd(total.esc$total.esc)/mean(total.esc$total.esc)
  
  PE.comp <- pop_CV[complete.cases(pop_CV),]
  
  PE_all <- sum(PE.comp$abund/sum(PE.comp$abund)* PE.comp$CV) - CV_all
  
  return(c(PE_all, CV_all) )
}
