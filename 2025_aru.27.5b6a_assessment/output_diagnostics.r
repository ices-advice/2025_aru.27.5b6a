#-------------------------------------------------------------------------------
# Diagnostic plots
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Do the plotting
#-------------------------------------------------------------------------------
FLSAM.diagnostics <- function(path,run,sam,tun,residuals=T,retro=NULL,loo=NULL,loi=NULL){
  pdf(file.path(path,paste0("plots_diagnostics_",run,".pdf"),sep=""))
    print(plot(sam,futureYrs=T))
    if(residuals){
      residual.diagnostics(sam)
    
      resids <- subset(residuals(sam))
      resids$std.res[which(is.na(resids$std.res))] <- 0
      print(xyplot(age ~ year | fleet,data=resids,main="Residuals by fleet",group=resids$fleet,cex=resids$std.res,
             panel=function(...){
               lst <- list(...)
               panel.xyplot(lst$x,lst$y,pch=ifelse(lst$cex[lst$subscript]>0,1,19),col="black",cex=1*abs(lst$cex[lst$subscript]))
             }))

      # print(xyplot(age ~ fleet | as.factor(year),data=resids,main="Residuals by year",group=resids$fleet,cex=resids$std.res,scales=list(x=list(rot=90)),
      #        panel=function(...){
      #          lst <- list(...)
      #          panel.xyplot(lst$x,lst$y,pch=ifelse(lst$cex[lst$subscript]>0,1,19),col="black",cex=1*abs(lst$cex[lst$subscript]))
      #        }))
    }
    # figure - catchabilities at age from HERAS
    catch <- catchabilities(sam)
    if(any(table(catch$fleet)>1)){
    print(xyplot(value+ubnd+lbnd ~ age | fleet,catch,
           scale=list(alternating=FALSE,y=list(relation="free")),as.table=TRUE,
           type="l",lwd=c(2,1,1),col=c("black","grey","grey"),
           subset=fleet %in% names(tun),
           main="Survey catchability parameters",ylab="Catchability",xlab="Age"))
    }

    # figure - f.vars
    fvar <- f.var(sam)
    print(xyplot(value+ubnd+lbnd ~ age | fleet,data=fvar,
           scale=list(alternating=FALSE,y=list(relation="free")),as.table=TRUE,
           type="l",lwd=c(2,1,1),col=c("black","grey","grey"),
           main="F random walk parameters",ylab="Variance",xlab="Age"))

    # figure - variance by data source
    obv <- obs.var(sam)
    obv$str <- paste(obv$fleet,ifelse(is.na(obv$age),"",obv$age))
    obv <- obv[order(obv$value),]
    bp <- barplot(obv$value,ylab="Observation Variance",
                  main="Observation variances by data source",col=factor(obv$fleet))
    axis(1,at=bp,labels=obv$str,las=3,lty=0,mgp=c(0,0,0))
    legend("topleft",levels(obv$fleet),pch=15,col=1:nlevels(obv$fleet),pt.cex=1.5)

    # figure - variance vs uncertainty for each data source
    plot(obv$value,obv$CV,xlab="Observation variance",ylab="CV of estimate",log="x",
         pch=16,col=obv$fleet,main="Observation variance vs uncertainty")
    text(obv$value,obv$CV,obv$str,pos=4,cex=0.75,xpd=NA)

    # figure - fishing age selectivity per year
    sel.pat <- merge(f(sam),fbar(sam),
                     by="year",suffixes=c(".f",".fbar"))
    sel.pat$sel <- sel.pat$value.f/sel.pat$value.fbar
    sel.pat$age <- as.numeric(as.character(sel.pat$age))
    print(xyplot(sel ~ age|sprintf("%i's",floor((year)/5)*5),sel.pat,
           groups=paste(year,fleet),type="l",as.table=TRUE,
           scale=list(alternating=FALSE),
           main="Selectivity of the Fishery by Pentad",xlab="Age",ylab="F/Fbar"))

    # figure - correlation matrix of model parameters
    print(cor.plot(sam))

    #Plot uncertainties as a function of time
    CV.yrs <- ssb(sam)$year
    CV.dat <- cbind(SSB=ssb(sam)$CV,
                       Fbar=fbar(sam)$CV,Rec=rec(sam)$CV)
    print(matplot(CV.yrs,CV.dat,type="l",ylim=range(pretty(c(0,CV.dat))),yaxs="i",
        xlab="Year",ylab="CV of estimate",main="Uncertainties of key parameters"))
    legend("topleft",legend=colnames(CV.dat),lty=1:5,col=1:6,bty="n")

    if(!is.null(retro))
      print(plot(retro))

    if(!is.null(retro))
      retroParams(retro)

    if(!is.null(loi))
      print(plot(loi,main="Leave one in"))
    if(!is.null(loo))
      print(plot(loo,main="Leave one out"))

  dev.off()

}