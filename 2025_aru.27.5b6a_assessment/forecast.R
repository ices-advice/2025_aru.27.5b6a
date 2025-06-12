## Run analysis, write model results

## Before:
## After:

library(icesTAF)
taf.library(FLCore)
taf.library(stockassessment)
taf.library(FLSAM)
library(FLasher)

mkdir("forecast")
load(file="model/model.RData")

# data, intermediate, advice and final YEARS
ARG <- ARG+ARG.sam
dy <- dims(ARG)$maxyear
iy <- dy + 1
ay <- iy + 1
fy <- ay + 1
ry <- (dy-2):dy #recruitment years

# TAC advice current year
tac       <- FLQuant(22698, dimnames=list(age='all', year=iy), units="tonnes")
ImYC      <- FLQuant(19047, dimnames=list(age='all', year=iy), units="tonnes")
tacadvice <- FLQuant(18966, dimnames=list(age='all', year=iy), units="tonnes")
#-----------------------
# Deterministic run
#-----------------------

# Geomean recruitment from recruitment years
recdr <- exp(mean(log(rec(ARG)[,ac(ry)])))

# Generate targets from reference points
refpts=FLPar(c(0.24,82999,59730,82999,0.33),
        dimnames=list(params=c("Fmsy","Btrigger","Blim","Bpa","Fpa"),year=iy,iter=1),
        units=c(NA,"tonnes","tonnes","tonnes",NA))
targets <- Matrix::expand(as(refpts, 'FLQuant'), year=ay:fy)

# --- SETUP future
# 3 years extending, taking average of 5 years wts/selex, last year for selection
fut <- stf(ARG, nyears=3, wts.nyears=5, fbar.nyears=1)

# Set recruitment as sampled value
gmsrr <- predictModel(model=rec~a, params=FLPar(c(recdr), units="thousands",
  dimnames=list(params="a", year=seq(iy, length=3), iter=1)))

# Intermediate year assumption
fut <- fwd(fut, sr=gmsrr, catch=ImYC)

#- update to advice year
fut <- fwd(fut, sr=gmsrr, fbar=targets["Fmsy",])

# --- PROJECT catch options
# DEFINE catch options
catch_options <- list(

  # advice
  advice=FLQuants(fbar=targets["Fmsy",] *
    min((ssb(fut)[, ac(ay)] / refpts$Btrigger), 1)),

  # FMSY
  Fmsy=FLQuants(fbar=targets["Fmsy",]),

  # F0
  F0=FLQuants(fbar=FLQuant(0, dimnames=list(age='all', year=c(ay,fy)))),

  # Fpa
  Fpa=FLQuants(fbar=targets["Fpa",]),

  # Bpa
  Bpa=FLQuants(ssb_flash=targets["Bpa",]),

  # Blim
  Blim=FLQuants(ssb_flash=targets["Blim",]),

  # MSYBtrigger
  MSYBtrigger=FLQuants(ssb_flash=targets["Btrigger",]),

  # F iy
  lastF=FLQuants(fbar=Matrix::expand(fbar(fut)[, ac(iy)], year=c(ay,fy))),
                 
  # TAC iy
  rotac=FLQuants(catch=Matrix::expand(tac, year=c(ay,fy)))
)


#- Setup control for forecast
fctls <- lapply(catch_options, function(x)
  as(x, "fwdControl")
)

# RUN!
runs <- FLStocks(lapply(fctls, function(x) fwd(fut, sr=gmsrr, control=x)))

#- Create forecast table for the advice sheet
tab  <- data.frame(Basis=names(runs),
                   Catch=round(do.call(rbind,lapply(runs,function(x){catch(x)[,ac(ay)]}))),
                   Fbar=round(do.call(rbind,lapply(runs,function(x){fbar(x)[,ac(ay)]})),3),
                   SSB=round(do.call(rbind,lapply(runs,function(x){ssb(x)[,ac(fy)]}))))
tab  <- cbind(tab,SSBchange=round((tab$SSB-round(do.call(rbind,lapply(runs,function(x){ssb(x)[,ac(ay)]}))))/round(do.call(rbind,lapply(runs,function(x){ssb(x)[,ac(ay)]})))*100),
                  Catchchange=round((tab$Catch-round(do.call(rbind,lapply(runs,function(x){catch(x)[,ac(iy)]}))))/round(do.call(rbind,lapply(runs,function(x){catch(x)[,ac(iy)]})))*100))

# Project F levels (optional)
flevels <- seq(0, 0.50, 0.01)
F0 <- FLQuants(fbar=FLQuant(0, dimnames=list(age='all', year=fy)))
# BUILD control with targets as iters
control <- as(FLQuants(
  fbar=append(FLQuant(flevels,
    dimnames=list(age='all', year=ay, iter=seq(flevels))), F0[[1]])),
  "fwdControl")

# Run the F levels
fruns <- divide(fwd(fut, sr=gmsrr, control=control), names=flevels)

# Save all DeterMinistic results
save(runs, fruns, fctls, recdr, tac, tab,file="forecast/runDM.rda", compress="xz")


#-----------------------
# Stochastic run
#-----------------------

n <- 1000
#- Simulate new Ns and Fs from the fitted assessment model
ARGmc <- simulate(ARG,ARG.tun,ARG.ctrl,n=n,set.pars=data.frame(par="logN.vars",number=1,value=log(0.01)),
          set.seed=12345)

# Expand the object to have n replicates
futmc   <- propagate(ARG,n)
dmns    <- dimnames(ARG@stock.n)$year       
for(i in 1:n){
  futmc@stock.n[,dmns,,,,i] <- ARGmc[[i]]@stock.n
  futmc@harvest[,dmns,,,,i] <- ARGmc[[i]]@harvest
}
#- Extend the object to have 3 additional years for forecasting and average wts/m/mat etc
futmc <- stf(futmc,nyears=3, wts.nyears = 5, fbar.nyears=1)
            
#- Geomean recruitment per replicate
recdrmc <- exp(yearMeans(log(rec(futmc)[,ac(ry)])))

#- Convert geomean to predictModel object
gmsrrmc <- predictModel(model=rec~a, params=FLPar(c(recdrmc), units="thousands",
  dimnames=list(params="a", year=seq(iy, length=3), iter=1:n)))
gmsrrmc@params[] <- rep(c(recdrmc),each=3)

# Intermediate year assumption
futmc <- fwd(futmc,sr=gmsrrmc,catch=propagate(ImYC,n))

ssb(futmc)

#- update to advice year
futmc <- fwd(futmc, sr=gmsrrmc, fbar=targets["Fmsy",])

#- Setup control for forecast, need to do this for each replicate as 
#  advice and lastF depend on replicate specific results
catch_optionsmc <- list()
for(i in 1:n){
  catch_optionsmc[[i]] <- catch_options
  catch_optionsmc[[i]]$advice = FLQuants(fbar=targets["Fmsy",] *
    min((ssb(futmc[,ac(ay),,,,i]) / refpts$Btrigger), 1))
  catch_optionsmc[[i]]$lastF  = FLQuants(fbar=Matrix::expand(fbar(futmc[, ac(iy),,,,i]), year=c(ay,fy)))
  catch_optionsmc[[i]] <- lapply(catch_optionsmc[[i]],function(x) as(x,"fwdControl"))
}

# Run stochastic simulation, prepare the cluster
library(doParallel)
ncores <- ifelse(n<detectCores()-1,n,detectCores()-1)
cl    <- makeCluster(ncores)
clusterEvalQ(cl,library(FLasher))
clusterEvalQ(cl,library(FLCore))
registerDoParallel(cl)

#~15mins
system.time(mcruns <- foreach(i = 1:n) %dopar% FLStocks(lapply(catch_optionsmc[[i]], function(x) fwd(iter(futmc,i), sr=iter(gmsrrmc,i), control=x))))
stopCluster(cl)

#- Resuls for advice run
ssbmc     <- array(NA,dim=c(length(names(mcruns[[1]])),length(iy:fy),n),dimnames=list(run=names(mcruns[[1]]),year=ac(iy:fy),rep=1:n))
fbarmc    <- array(NA,dim=c(length(names(mcruns[[1]])),length(iy:fy),n),dimnames=list(run=names(mcruns[[1]]),year=ac(iy:fy),rep=1:n))
catchmc   <- array(NA,dim=c(length(names(mcruns[[1]])),length(iy:fy),n),dimnames=list(run=names(mcruns[[1]]),year=ac(iy:fy),rep=1:n))

for(i in 1:n){
  ssbmc[,,i]   <- do.call(rbind,lapply(mcruns[[i]],function(y) ssb(y[,ac(iy:fy)])))
  fbarmc[,,i]  <- do.call(rbind,lapply(mcruns[[i]],function(y) fbar(y[,ac(iy:fy)])))
  catchmc[,,i] <- do.call(rbind,lapply(mcruns[[i]],function(y) catch(y[,ac(iy:fy)])))
}

tabmc  <- data.frame(Basis=names(runs),
                   Catch=apply(catchmc,1:2,median)[,ac(ay)],
                   Fbar=apply(fbarmc,1:2,median)[,ac(ay)],
                   SSB=apply(ssbmc,1:2,median)[,ac(fy)])
tabmc  <- cbind(tabmc,SSBchange=round((apply(ssbmc,1:2,median)[,ac(fy)]-apply(ssbmc,1:2,median)[,ac(ay)])/apply(ssbmc,1:2,median)[,ac(ay)]*100),
                  Catchchange=round((apply(catchmc,1:2,median)[,ac(ay)]-apply(catchmc,1:2,median)[,ac(iy)])/apply(catchmc,1:2,median)[,ac(iy)]*100),
                  Advicechange=round((apply(catchmc,1:2,median)[,ac(ay)]-c(tacadvice))/c(tacadvice)*100))
write.csv(tabmc,file="./forecast/forecastSC.csv")

#- Extensive output with all the quantiles
tabmcext  <- data.frame(Basis=rep(names(mcruns[[1]]),each=3),
                   Rec=as.data.frame.table(apply(gmsrrmc@params[,drop=T],1,FUN=quantile,probs=c(0.025,0.5,0.975))),
                   Catch=as.data.frame.table(apply(catchmc,1:2,FUN=quantile,probs=c(0.025,0.5,0.975))),
                   Fbar=as.data.frame.table(apply(fbarmc,1:2,FUN=quantile,probs=c(0.025,0.5,0.975))),
                   SSB=as.data.frame.table(apply(ssbmc,1:2,FUN=quantile,probs=c(0.025,0.5,0.975))))
write.csv(tabmcext,file="./forecast/forecastSCext.csv")

# SAVE
save(mcruns, futmc, gmsrrmc, fctls,tac,tabmc, tabmcext, file="forecast/runsSTO.rda", compress="xz")