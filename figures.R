########################################################################################
# figures.R
#
# Code to generate figures visualizing CDN-yukon closed loop simulations 
#
# July 24, 2019
# Author: B. Connors (DFO)
#        
########################################################################################

# load simulation output
outcomes <- readRDS("outputs/base_sims.ricker")

outcomes.med <- apply(outcomes,c(1,2,3),quantile,probs=c(0.5),na.rm=T)
outcomes.upper <- apply(outcomes,c(1,2,3),quantile,probs=c(0.75),na.rm=T)
outcomes.lower <- apply(outcomes,c(1,2,3),quantile,probs=c(0.25),na.rm=T)


# --- Figures  -----------------------------------------------------------------

	#------------------------------------------------------------------------------#
	# (a) productivity vs size plot and (b) equilibrium tradeoffs
	#------------------------------------------------------------------------------#
	
	m.alpha <- apply(wide.posteriors[,,1],c(2),quantile,probs=c(0.5),na.rm=T)
	alpha.upper <- apply(wide.posteriors[,,1],c(2),quantile,probs=c(0.95),na.rm=T)
	alpha.lower<- apply(wide.posteriors[,,1],c(2),quantile,probs=c(0.05),na.rm=T)
	a <-length(m.alpha); b <- a
	size <- (log(wide.posteriors[,,1])/wide.posteriors[,,2])/1000
	
	m.size <- apply(size,c(2),quantile,probs=c(0.5),na.rm=T)
	size.lower <- apply(size,c(2),quantile,probs=c(0.05),na.rm=T)
	size.upper <- apply(size,c(2),quantile,probs=c(0.95),na.rm=T)
	
	jpeg("figures/fig_3.July2019.jpeg",width=7.75, height=3.5, units="in",res=800)
	
		#dev.new(width=7.75, height=3.5,new=FALSE)
		par(mfrow=c(1,2),bty="o", mar=c(3,3,1,3),oma=c(1,1,1,1))#set dimensions to plots
		
		# --- panel a --------------------------------------------------------------
		plot(m.size,m.alpha,yaxt="n",col="white",xlim=c(0,25),ylim=c(1,8))
		axis(2,las=2)
		
		for(i in 1:13){
			xlcl = size.lower[i]; xucl= size.upper[i]; y= m.alpha[i]; ylcl= alpha.lower[i]; yucl =alpha.upper[i];x= m.size[i]
			lines(x=c(xlcl,xucl),y=c(y,y),col=MyColour[i]);lines(x=c(x,x),y=c(ylcl,yucl),col= MyColour[i])
		}
		
		points(m.size,m.alpha, cex=1.5, pch=16,col=MyColour)
		text(m.size,m.alpha, stock_id,cex=0.5,col= MyTextColour)
		axis(2,labels=F)
		
		text(0.5,7.8,"(a)")
		mtext("Productivity ",2,line=2.75,cex=1)
		mtext("(recruits/spawner)",2,line=1.75,cex=1)
		mtext("Equilibrium size (000s)",1,line=2,cex=1)
		box(col="grey")
		# panel b
		plot(U*100, t3.upper[,2]/1000,type="l",yaxt="n",ylab="",xlab="",lwd=1,lty=2,ylim=c(0,110))
		axis(2,las=2,col="blue",col.axis="blue")
		
		polygon(c(U*100,rev(U*100)),c((t3.upper[,2]/1000),rev(t3.lower[,2]/1000)),col="#0000FF25",border=NA)
		points(U*100, (t3.median[,2]/1000),type="l",col="blue",lwd=3)
		points(U*100, (t3.upper[,2]/1000),type="l",col="blue",lwd=1,lty=2)
		points(U*100, (t3.lower[,2]/1000),type="l",col="blue",lwd=1,lty=2)
		text(3,105,"(b)")
		
		#panel b
		par(new=TRUE)
		plot(U*100, ext.med,type="l",yaxt="n",xaxt="n",ylab="",xlab="",lwd=3,col="red")
		polygon(c(U*100,rev(U*100)),c((ext.up),rev(ext.low)),col="#FF000025",border=NA)
		points(U*100, ext.med,type="l",col="red",lwd=3,lty=1)
		points(U*100, ext.up,type="l",col="red",lwd=1,lty=2)
		points(U*100, ext.low,type="l",col="red",lwd=1,lty=2)
		axis(4,las=2,labels=c(0,20,40,60,80,100),at=c(0,0.2,0.4,0.6,0.8,1),col.ticks="red",col.axis="red")
		box(col="grey")
		
		mtext("Harvest (000s)",2,line=2.25,col="blue")
		mtext("Harvest rate (%)",1,line=2.25)
		mtext("Populations extirpated (%) ",4,line=2.5,col="red")
	
	dev.off()
	
