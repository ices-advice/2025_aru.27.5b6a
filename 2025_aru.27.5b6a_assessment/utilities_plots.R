##################################################
## function to overlay time series
##################################################
overlayTimeseries <- function(x,nyrs,ages,cex.symbol=0.8){
  require(doBy)
  validObject(x)
  if(class(x)!="FLQuants") stop("Object is not an FLQuants")
  if(any(is.na(names(x))==T)) stop("Each FLQuant must have a name")
  require(reshape)
  lng   <- length(x)
  dmns  <- list()
  for(i in 1:lng)
    dmns[[i]] <- dimnames(x[[i]])
  
  ags   <- unique(unlist(lapply(dmns,function(x){return(x$age)})))
  if("all" %in% ags){ x <- x[-which(ags == "all")]; dmns <- dmns[-which(ags == "all")]}
  lng   <- length(x);
  idx   <- lapply(dmns,function(x){any(x$age %in% ages)})
  if(length(idx)>0){
    x   <- x[unlist(idx)]; dmns <- dmns[unlist(idx)]}
  lng   <- length(x)
  yrs   <- range(unlist(lapply(x,function(y){dims(y)[c("minyear","maxyear")]})))
  
  stk   <- data.frame()
  for(i in 1:lng)
    stk   <- rbind(stk,cbind(as.data.frame(rescaler(trim(window(x[[i]],start=(max(an(yrs))-(nyrs-1)),end=max(an(yrs))),age=c(max(ages[1],dims(x[[i]])$min):min(rev(ages)[1],dims(x[[i]])$max))))),qname=names(x)[i]))
  stk$track <- stk$year - stk$age
  
  stk <- orderBy(~age+qname+track,data=stk)
  xyplot(data ~ track,data=stk,groups=qname,type="l",
         prepanel=function(...) {list(ylim=range(pretty(c(0,list(...)$y))))},xlab="Cohort",ylab="Standardized timeseries",
         auto.key=list(space="right",points=FALSE,lines=TRUE,type="l"),
         panel = panel.superpose,
         panel.groups = function(...) {
           res <- list(...)
           lng <- length(res$x)/nyrs
           for(i in 1:lng){
             panel.grid(v=-1,h=-1,lty=3)
             panel.xyplot(res$x[(nyrs*i-nyrs+1):(nyrs*i)],res$y[(nyrs*i-nyrs+1):(nyrs*i)],lty=i,type="l",col=res$col.line)
             panel.text(res$x[(nyrs*i-nyrs+1):(nyrs*i)],res$y[(nyrs*i-nyrs+1):(nyrs*i)],labels=stk$age[res$subscript[(nyrs*i-nyrs+1):(nyrs*i)]],col=res$col.line,cex=cex.symbol)
           }
         },
         scales=list(alternating=1,y=list(relation="free",rot=0)))
}

##################################################
## function to plot time series
##################################################

timeseries <- function(stck.,slot.,...){
  assign("stck.",stck.,envir=.GlobalEnv);assign("slot.",slot.,envir=.GlobalEnv);
  xyplot(data~year,data=slot(stck.,slot.),...,
         groups=age,
         auto.key=list(space="right",points=FALSE,lines=TRUE,type="b"),
         type="b",
         xlab="Year",ylab=paste("Time series of",slot.,ifelse(units(slot(stck.,slot.))=="NA","",paste("(",units(slot(stck.,slot.)),")",sep=""))),
         main=paste(stck.@name,"timeseries of",slot.),
         par.settings=list(superpose.symbol=list(pch=as.character(0:8),cex=1.25)))}

