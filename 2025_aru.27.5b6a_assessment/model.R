## Run analysis, write model results

## Before:
## After:

library(icesTAF)
taf.library(FLCore)
taf.library(stockassessment)
taf.library(FLSAM)

mkdir("model")
load(file="data/data.RData")

### ============================================================================
### Construct control object
### ============================================================================

ARG.ctrl 														<- FLSAM.control(ARG,ARG.tun)
ARG.ctrl@states["catch unique",]								<- c(0:5,rep(6,11))
ARG.ctrl@catchabilities["Faroese Summer    survey",ac(5:12)]	<- c(0,1,2,3,4,5,5,5)
ARG.ctrl@catchabilities["FaroeseDeepWaterSurvey",  ac(5:14)]	<- c(0,1,2,3,4,4,4,4,4,4) + 101

ARG.ctrl@cor.obs.Flag[2:3]										<- as.factor("AR")
ARG.ctrl@cor.obs["Faroese Summer    survey",1:7]				<- 0
ARG.ctrl@cor.obs["FaroeseDeepWaterSurvey",1:9]					<- 1

ARG.ctrl@biomassTreat[4:5]										<- 5
ARG.ctrl@plus.group												<- c(1,0,0,0,0)
ARG.ctrl														<- update(ARG.ctrl)


### ============================================================================
### Run the model
### ============================================================================

ARG.sam 		<- FLSAM(ARG,ARG.tun,ARG.ctrl,
										 set.pars=data.frame(par="logN.vars",number=1,value=log(0.01)))
ARG.fit 		<- FLSAM(ARG,ARG.tun,ARG.ctrl,
										 set.pars=data.frame(par="logN.vars",number=1,value=log(0.01)),return.fit=T)
### ============================================================================
### Run retro
### ============================================================================

ARG.ctrl@residuals 	<- FALSE
ARG.retro 			<- FLSAM::retro(ARG,ARG.tun,ARG.ctrl,5,set.pars=data.frame(par="logN.vars",number=1,value=log(0.01)))

### ============================================================================
### Run leave-one-out
### ============================================================================

ARG.looi  			<- looi(ARG,ARG.tun,ARG.ctrl,type="full",set.pars=data.frame(par="logN.vars",number=1,value=log(0.01)))
ARG.loo				<- ARG.looi[c(1,grep("Drop",names(ARG.looi)))]
ARG.loi				<- ARG.looi[c(1,grep("Only",names(ARG.looi)))]

### ============================================================================
###- Save the output
### ============================================================================

save(ARG,ARG.tun,ARG.ctrl,ARG.sam,ARG.fit,ARG.retro,ARG.loo,ARG.looi,file="model/model.RData")