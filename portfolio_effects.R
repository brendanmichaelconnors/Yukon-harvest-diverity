########################################################################################
# portfolio_effects.R
#
# Estimate magnitude of variance dampening in CDN-origin Yukon Chinook returns 
#
# Last updated: July 30, 2019
# Author: B. Connors (DFO)
#        
########################################################################################

# ---- Chose RR data to use -----------------------------------------------------------#
load("./data/borderPassage-mod2.Rdata")
runSize$Stock <-lapply(runSize$stock, as.character)
run_size_Carmacks <- rbind(runSize[runSize$Stock == "UpperLakesAndMainstem",],
                           runSize[runSize$Stock == "Teslin",],
                           runSize[runSize$Stock == "Carmacks",]
                          )
run_size_Teslin <- rbind(runSize[runSize$Stock == "Teslin",])
                           
# ---- Calculate running PE and CV -----------------------------------------------------#
PE_running_avg_full <- matrix(NA, 21,1)
PE_running_avg_Carmacks <- PE_running_avg_full
CV_running_avg_full <- PE_running_avg_full
CV_running_avg_Carmacks <- PE_running_avg_full
CV_running_avg_Teslin <- PE_running_avg_full

for (i in 1:21){
  PE_running_avg_full[i,] <- PE_running(runSize, 1984 + i, 1994 + i)[1]
  PE_running_avg_Carmacks[i,] <- PE_running(run_size_Carmacks, 1984 + i, 1994 + i)[1]
  CV_running_avg_full[i,] <- PE_running(runSize, 1984 + i, 1994 + i)[2]
  CV_running_avg_Carmacks[i,] <- PE_running(run_size_Carmacks, 1984 + i, 1994 + i)[2]
  CV_running_avg_Teslin[i,] <- PE_running(run_size_Teslin, 1984 + i, 1994 + i)[2]
  }

# ---- How much more stable is aggregate than individual pops?  -----------------------#
CV_ind <- matrix(NA, 8,1)

for (i in unique(runSize$stock)){
  CV_ind[i] <-PE_running(runSize[runSize$stock==i,], 1984 , 2016)[2]
}

CV_avg_all <- mean(CV_ind[9:16]); sd(CV_ind[9:16])  # mean and SD across individual pops
CV_agg <- PE_running(runSize, 1984 , 2016)[2] # realized CV integrated across all pops
CV_avg_all/CV_agg # magnitude of variance dampending

# ---- Figure of  PE and CV -----------------------------------------------------------#

jpeg("figures/Figure_12.portfolio_effects.jpeg",width=7.75, height=3.5, units="in",res=800)
  
  #dev.new(width=7.75, height=3.5,new=FALSE)
  par(mfrow=c(1,2),bty="o", mar=c(3,4,1,3),oma=c(1.5,1,1,1))#set dimensions to plots
  
  plot(x = seq(1995,2015),
       y = CV_running_avg_full,
       ylim = c(0,0.85),
       yaxt = "n",
       xaxt = "n",
       type = "l",
       ylab = "CV in run-size",
       xlab = "",
       lwd = 2)
  
  axis(1)
  axis(2,las=2)
  box(col="grey")
  
  points(x = seq(1995,2015),
         y = CV_running_avg_Carmacks,
         type = "l",
         lwd = 2,
         col = "dark grey")
  
  points(x = seq(1995,2015),
         y = CV_running_avg_Teslin,
         type = "l",
         lwd = 2,
         col = "light grey")

  text(x = 2005,
       y = c(0.38,0.55,0.8),
       labels = c("Dawson", "Carmacks","Teslin"),
       col = c("black","dark grey","light grey"),
       cex=c(0.85)) 
  
  plot(x = seq(1995,2015),
       y = PE_running_avg_full,
       ylim = c(0,0.5),
       yaxt = "n",
       xaxt = "n",
       type = "l",
       ylab = "Portfolio effect",
       xlab = "",
       lwd = 2)
  
  axis(1)
  axis(2,las=2)
  box(col="grey")
  
  points(x = seq(1995,2015),
         y = PE_running_avg_Carmacks,
         type = "l",
         lwd = 2,
         col = "dark grey")
  text(x = 2005,
       y = c(0.4,0.22),
       labels = c("Dawson", "Carmacks"),
       col = c("black","dark grey"),
       cex=0.85)  
  
  
    mtext("Year (last year of 10-year rolling window)",1,line=0.5, outer=T)

dev.off()