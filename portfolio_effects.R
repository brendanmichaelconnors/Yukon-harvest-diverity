load("./data/runSize.Rdata")
rr_data <- matrix(NA,256,3)
rr_data <- runSize
data <- rr_data
first_year <- 1985
last_year <- 2015
#-------------------------------------------------------------------------------------------------------#
# Portfolio effect function
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
	
	return(c(PE_all) )
	}