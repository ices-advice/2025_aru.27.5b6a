## Prepare plots and tables for report

## Before:
## After:

library(icesTAF)
taf.library(FLCore)
taf.library(stockassessment)
taf.library(FLSAM)
library(doBy)
library(ggplot2)
suppressMessages(library(reshape))
library(tidyverse)


mkdir("report")
source('utilities_plots.R')
source("output_diagnostics.R")
load(file="model/model.RData")

terminal.year <- range(ARG)["maxyear"]

### ============================================================================
### inputs
### ============================================================================

prefix <- 'input'

##################################################
## stock weight at age
#################################################
taf.png(paste0(prefix,"_wstock_timeseries"))
zoom(timeseries(ARG,slot="stock.wt"))
dev.off()

##################################################
## catch at age
#################################################
taf.png(paste0(prefix,"_wcatch_timeseries"))
zoom(timeseries(ARG,slot="catch.wt"))
dev.off()

##################################################
## maturity at age
#################################################
taf.png(paste0(prefix,"_maturity"))
zoom(timeseries(ARG,slot="mat"))
dev.off()

##################################################
## M at age
#################################################
taf.png(paste0(prefix,"_natmort"))
zoom(timeseries(ARG,slot="m"))
dev.off()

##################################################
## prop catches at age since 2000
#################################################
taf.png(paste0(prefix,"_prop_catches"))
prop_catches <- as.data.frame(ARG@catch.n)
prop_catches$data[prop_catches$data == -1] <- NA

ggplot(subset(prop_catches,year>=(terminal.year-20)),aes(x=year,y=data))+
  geom_bar(aes(fill = as.factor(age)),stat="identity",position = "fill")+
  ylab('Proportion of Catch numbers at age')
dev.off()

##################################################
## weight in the stock by cohort
#################################################
taf.png(paste0(prefix,"_wstock_cohort"))
west.by.cohort      <- as.data.frame(FLCohort(window(ARG@stock.wt,(terminal.year-20),range(ARG)["maxyear"])))
west.by.cohort      <- subset(west.by.cohort,!is.na(west.by.cohort$data))
west.by.cohort$year <- west.by.cohort$age + west.by.cohort$cohort
zoom(xyplot(data~year,data=west.by.cohort,
            groups=cohort,
            auto.key=list(space="right",points=FALSE,lines=TRUE,type="b"),
            type="b",
            xlab="Year",ylab="Weight in the stock (kg)",
            main=paste(ARG@name,"Weight in the stock by cohort"),
            par.settings=list(superpose.symbol=list(pch=as.character(unique(west.by.cohort$cohort)%%10),cex=1.25)),
            panel=function(...) {
              panel.grid(h=-1,v=-1)
              panel.xyplot(...)
            }))
dev.off()

##################################################
## overlay survey time series
#################################################
start_year  <- terminal.year-20
end_year    <- terminal.year

for(iAge in range(ARG)["min"]:range(ARG)["max"]){
  taf.png(paste0(prefix,"_overlay_survey_series_age_", iAge))
  for(idxSurvey in 1:length(ARG.tun)){
    # make sure we don't take any non-number based surveys
    if(type(ARG.tun[[idxSurvey]])=="number"){
	    idxFilt <- which(iAge == ARG.tun[[idxSurvey]]@range[1]:ARG.tun[[idxSurvey]]@range[2])
	      
		if(idxSurvey == 1){
	    	dat <- cbind(as.data.frame(ARG.tun[[idxSurvey]]@index[idxFilt,]),
	        	         rep(ARG.tun[[idxSurvey]]@name,
	                     length(drop(ARG.tun[[idxSurvey]]@index[idxFilt,]))))
	    	colnames(dat)[dim(dat)[2]] <- 'survey'
	    	dat$data <- (dat$data-mean(dat$data[dat$data != -1],na.rm=T))/sd(dat$data[dat$data != -1],na.rm=T)
	  	}else{
	    	tempVar <- cbind(as.data.frame(ARG.tun[[idxSurvey]]@index[idxFilt,]),
	        	             rep(ARG.tun[[idxSurvey]]@name,
	                         length(drop(ARG.tun[[idxSurvey]]@index[idxFilt,]))))
	    	colnames(tempVar)[dim(tempVar)[2]] <- 'survey'
	    	tempVar$data <- (tempVar$data-mean(tempVar$data[tempVar$data != -1],na.rm=T))/sd(tempVar$data[tempVar$data != -1],na.rm=T)
	    	dat     <- rbind(dat,tempVar)
	  	}
	  }
  }
  p <- ggplot(data = dat,mapping=aes(x=year,y=data,color=survey))+
    theme_bw() +
    geom_line()+
    xlim(start_year, end_year)+
    ylab('standardized index')+xlab('year')+
    ggtitle(paste('age ',iAge))
  
  print(p)
  dev.off()
}


##################################################
## Internal consistency surveys
#################################################
taf.png(paste0(prefix,"_FSS_internal_consistency"))
zoom(plot(ARG.tun[[1]],type="internal"))
dev.off()

taf.png(paste0(prefix,"_FDWS_internal_consistency"))
zoom(plot(ARG.tun[[2]],type="internal"))
dev.off()






prefix <- 'fit'

##################################################
## assessment fit
##################################################
pdf(file.path('./report',paste0(prefix,"_",range(ARG.sam)["maxyear"],".pdf")))
residual.diagnostics(ARG.sam)
dev.off()

##################################################
## assessment fit - panel plot
##################################################
taf.png(paste0(prefix,"_residCatch"))
resids <- residuals(ARG.sam)
ggplot(subset(resids,fleet=="catch unique"),aes(x=year,y=log.mdl)) + geom_line() + geom_point(data=subset(resids,fleet=="catch unique"),aes(x=year,y=log.obs),colour="red") + theme_bw() + facet_wrap(vars(fleet,age),scales="free_y")
dev.off()

taf.png(paste0(prefix,"_residSurvey"))
ggplot(subset(resids,fleet!="catch unique"),aes(x=year,y=log.mdl)) + geom_line() + geom_point(data=subset(resids,fleet!="catch unique"),aes(x=year,y=log.obs),colour="red") + theme_bw() + facet_wrap(vars(fleet,age),scales="free_y")
dev.off()

##################################################
## stock trajectory
##################################################
taf.png(paste0(prefix,"_stock_trajectory"))
zoom(plot(ARG.sam))
dev.off()




##################################################
## Catchability at age
##################################################
taf.png(paste0(prefix,"_survey_catchability"))
catch <- catchabilities(ARG.sam)
zoom(xyplot(value+ubnd+lbnd ~ age | fleet,catch,
             scale=list(alternating=FALSE,y=list(relation="free")),as.table=TRUE,
             type="l",lwd=c(2,1,1),col=c("black","grey","grey"),
             subset=fleet %in% levels(catch$fleet)[2:3],
             main="Survey catchability parameters",ylab="Catchability",xlab="Age"))
dev.off()

##################################################
## observation variance by data source
##################################################
taf.png(paste0(prefix,"_observation_var_by_source"))
obv <- obs.var(ARG.sam)
obv$str <- paste(obv$fleet,ifelse(is.na(obv$age),"",obv$age))
obv <- obv[order(obv$value),]
bp <- barplot(obv$value,ylab="Observation Variance",
              main="Observation variances by data source",col=factor(obv$fleet))
axis(1,at=bp,labels=obv$str,las=3,lty=0,mgp=c(0,0,0))
legend("topleft",levels(obv$fleet),pch=15,col=1:nlevels(obv$fleet),pt.cex=1.5)
dev.off()

##################################################
## variance vs uncertainty for each data source
#################################################
taf.png(paste0(prefix,"_variance_vs_uncertainty"))
plot(obv$value,obv$CV,xlab="Observation variance",ylab="CV of estimate",log="x",
     pch=16,col=obv$fleet,main="Observation variance vs uncertainty")
text(obv$value,obv$CV,obv$str,pos=4,cex=0.75,xpd=NA)
dev.off()

##################################################
## Selectivity pattern per 5 years
#################################################
taf.png(paste0(prefix,"_selectivity"))
sel.pat <- merge(f(ARG.sam),fbar(ARG.sam),
                 by="year",suffixes=c(".f",".fbar"))
sel.pat$sel <- sel.pat$value.f/sel.pat$value.fbar
sel.pat$age <- as.numeric(as.character(sel.pat$age))
zoom(xyplot(sel ~ age|sprintf("%i's",floor((year)/5)*5),sel.pat,
       groups=year,type="l",as.table=TRUE,
       scale=list(alternating=FALSE),
       main="Selectivity of the Fishery by Pentad",xlab="Age",ylab="F/Fbar"))
dev.off()

##################################################
## correlation matrix model params
#################################################
taf.png(paste0(prefix,"_cor_params"))
cor.plot(ARG.sam)
dev.off()

##################################################
## Residuals by fleet per year per age
#################################################
taf.png(paste0(prefix,"_residuals"))
dat <- residuals(ARG.sam)
dat$std.res[is.na(dat$std.res)] <- 0
dat$shp <- if_else(dat$std.res<0,1,19)
ggplot(dat,aes(x=year,y=age,shape=af(shp))) + geom_point(data=dat,aes(size=abs(std.res))) + scale_shape_manual(values = c(1,19)) + theme_bw() + facet_wrap(vars(fleet)) +theme(legend.position = "none")
dev.off()
##################################################
## catch residuals per year per age
#################################################

taf.png(paste0(prefix,"_catage_residuals"))
dat <- subset(residuals(ARG.sam),fleet=="catch unique")
dat <- dat[complete.cases(dat),]
zoom(xyplot(age ~ year,data=dat,cex=dat$std.res,col="black",main="Residuals by year Catch",
             panel=function(...){
               lst <- list(...)
               panel.xyplot(lst$x,lst$y,pch=ifelse(lst$cex>0,1,19),col="black",cex=1*abs(lst$cex))
             }))
dev.off()

##################################################
## FSS residuals per year per age
#################################################
taf.png(paste0(prefix,"_FSS_residuals"))
dat <- subset(residuals(ARG.sam),fleet=="Faroese Summer    survey")
zoom(xyplot(age ~ year,data=dat,cex=dat$std.res,col="black",main="Residuals by year FSS",
             panel=function(...){
               lst <- list(...)
               panel.xyplot(lst$x,lst$y,pch=ifelse(lst$cex>0,1,19),col="black",cex=1*abs(lst$cex))
             }))
dev.off()

##################################################
## FDWS residuals per year per age
#################################################
taf.png(paste0(prefix,"_FDWS_residuals"))
dat <- subset(residuals(ARG.sam),fleet=="FaroeseDeepWaterSurvey")
zoom(xyplot(age ~ year,data=dat,cex=dat$std.res,col="black",main="Residuals by year FDWS",
             panel=function(...){
               lst <- list(...)
               panel.xyplot(lst$x,lst$y,pch=ifelse(lst$cex>0,1,19),col="black",cex=1*abs(lst$cex))
             }))
dev.off()

##################################################
## SDWS residuals per year per age
#################################################
taf.png(paste0(prefix,"_SDWS_residuals"))
dat <- subset(residuals(ARG.sam),fleet=="Scottish Deepwater survey")
zoom(xyplot(age ~ year,data=dat,cex=dat$std.res,col="black",main="Residuals by year SDWS",
             panel=function(...){
               lst <- list(...)
               panel.xyplot(lst$x,lst$y,pch=ifelse(lst$cex>0,1,19),col="black",cex=1*abs(lst$cex))
             }))
dev.off()

##################################################
## CPUE residuals per year per age
#################################################
taf.png(paste0(prefix,"_CPUE_residuals"))
dat <- subset(residuals(ARG.sam),fleet=="Combine Faroe-EU standardized CPUE")
zoom(xyplot(age ~ year,data=dat,cex=dat$std.res,col="black",main="Residuals by year CPUE",
             panel=function(...){
               lst <- list(...)
               panel.xyplot(lst$x,lst$y,pch=ifelse(lst$cex>0,1,19),col="black",cex=1*abs(lst$cex))
             }))
dev.off()

##################################################
## proc error N
#################################################
taf.png(paste0(prefix,"_procErr_N"))
procerr.plot(ARG+ARG.sam,weight="stock.wt",type="n",rel=T)
dev.off()

##################################################
## proc error M
#################################################
taf.png(paste0(prefix,"_procErr_M"))
procerr.plot(ARG+ARG.sam,weight="stock.wt",type="mort",rel=T)
dev.off()


##################################################
## F at age
#################################################
taf.png(paste0(prefix,"_fatage"))
zoom(timeseries(window(ARG,2000,range(ARG)["maxyear"]),slot="harvest"))
dev.off()

##################################################
## F/fbar at age
#################################################
ARG.temp <- ARG
ARG.temp@harvest <- sweep(ARG@harvest,2:6,fbar(ARG),'/')

taf.png(paste0(prefix,"_ffbaratage"))
zoom(timeseries(window(ARG.temp,2000,range(ARG)["maxyear"]),slot="harvest"))
dev.off()

##################################################
## retro stock trajectory
#################################################
taf.png(paste0(prefix,"_retrospective_stock"))
zoom(plot(ARG.retro))
dev.off()

##################################################
## model parameters retrospective
#################################################
taf.png(paste0(prefix,"_retrospective_SAM_parameters"))
zoom(retroParams(ARG.retro))
dev.off()

##################################################
## fishing selectivity retrospective
#################################################
taf.png(paste0(prefix,"_retrospective_SAM_parameters"))
zoom(retroSelectivity(ARG.retro,(range(ARG)["maxyear"]-5):range(ARG)["maxyear"]))
dev.off()

##################################################
## LOOI runs
#################################################
taf.png(paste0(prefix,"_looi"))
zoom(plot(ARG.loo))
dev.off()


##################################################
## otolith plot
#################################################

taf.png(paste0(prefix,"_otolith"))
otolith(ARG.sam,n=10000)
dev.off()
