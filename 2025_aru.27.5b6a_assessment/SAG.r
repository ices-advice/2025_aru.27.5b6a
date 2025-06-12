# ============================================================================
# UPload Assessment Data into SAG
#
# 17/08/2021 Updated for HAWG 2021
# 30/04/2022 Updated for HAWG 2022
# ============================================================================

rm(list=ls())

library(icesTAF)

taf.library(FLCore)
taf.library(stockassessment)
taf.library(FLSAM)

library(tidyverse)
library(icesSAG)  # install.packages("icesSAG")

# Warning in install.packages :
#   package ‘icesSAG’ is not available for this version of R
# 
# A version of this package for your version of R might be available elsewhere,
# see the ideas at
# https://cran.r-project.org/doc/manuals/r-patched/R-admin.html#Installing-packages

runName <- 'model'

model.save      <- file.path(".",'model')
load(file.path(model.save,paste0(runName,'.RData')))

# use token
options(icesSAG.use_token = TRUE)

# Set years and ranges
FiY   <- dims(ARG)$minyear
DtY   <- dims(ARG)$maxyear
LaY   <- dims(ARG)$maxyear+1 #lag-years
nyrs  <- ((DtY)-(FiY))+1
nyrs2 <- ((LaY)-(FiY))+1

#- Read interim year settings
forecast <- read.csv("./forecast/forecastSCext.csv")

# Meta information
stockkeylabel  <- "aru.27.5b6a"
assessmentyear <- 2025
contactperson  <- "peturm@hav.fo"

# SSB in intermediate year
SSBintlbnd  <-subset(forecast,SSB.year==(DtY+1) & Basis == "advice" & SSB.Var1=="2.5%"  & Catch.year==(DtY+1))$SSB.Freq   # to be updated after the forecast
SSBint      <-subset(forecast,SSB.year==(DtY+1) & Basis == "advice" & SSB.Var1=="50%"   & Catch.year==(DtY+1))$SSB.Freq   # to be updated after the forecast
SSBintubnd  <-subset(forecast,SSB.year==(DtY+1) & Basis == "advice" & SSB.Var1=="97.5%" & Catch.year==(DtY+1))$SSB.Freq   # to be updated after the forecast

#- Catch in intermediate year
Catchint    <- subset(forecast,Catch.year==(DtY+1) & Basis == "advice" & Catch.Var1 == "50%" & Catch.year==(DtY+1))$Catch.Freq   # to be updated after the forecast

# Recruitment in the intermediate year
Recint      <- subset(forecast,Rec.year==(DtY+1) & Basis == "advice" & Rec.Var1=="50%" & Catch.year==(DtY+1))$Rec.Freq   # to be updated after the forecast
Recintlbnd  <- subset(forecast,Rec.year==(DtY+1) & Basis == "advice" & Rec.Var1=="2.5%" & Catch.year==(DtY+1))$Rec.Freq   # to be updated after the forecast
Recintubnd  <- subset(forecast,Rec.year==(DtY+1) & Basis == "advice" & Rec.Var1=="97.5%" & Catch.year==(DtY+1))$Rec.Freq

# F in intermediate year
Fbarintlbnd  <-subset(forecast,Fbar.year==(DtY+1) & Basis == "advice" & Fbar.Var1=="2.5%"  & Catch.year==(DtY+1))$Fbar.Freq   # to be updated after the forecast
Fbarint      <-subset(forecast,Fbar.year==(DtY+1) & Basis == "advice" & Fbar.Var1=="50%"   & Catch.year==(DtY+1))$Fbar.Freq   # to be updated after the forecast
Fbarintubnd  <-subset(forecast,Fbar.year==(DtY+1) & Basis == "advice" & Fbar.Var1=="97.5%" & Catch.year==(DtY+1))$Fbar.Freq   # to be updated after the forecast



# Create the input data for uploading  
info     <- icesSAG::stockInfo(
  StockCode = stockkeylabel, 
  AssessmentYear = assessmentyear,
  ModelType = "A",
  ContactPerson  = contactperson, 
  StockCategory = 1,
  Purpose = "Advice",
  ModelName = "SAM")

info$StockCategory             <- "1"
info$MSYBtrigger               <- 82999
info$Blim                      <- 59730
info$Bpa                       <- 82999
info$Fpa                       <- 0.33
info$FMSY                      <- 0.24
info$Fage                      <- "6-14" 
info$RecruitmentAge            <- 5
info$CatchesLandingsUnits      <- "t"
info$RecruitmentDescription    <- "age"
info$RecruitmentUnits          <- "thousand" 
info$FishingPressureDescription<- "F"
info$FishingPressureUnits      <- NA 
info$StockSizeDescription      <- "SSB"
info$StockSizeUnits            <- "t"
info$Purpose                   <- "Advice"
info$CustomSeriesName1         <- "model catch"
info$CustomSeriesName2         <- "model catch low"
info$CustomSeriesName3         <- "model catch high"
info$ModelName                 <- "SAM"
info$ModelType                 <- "A"
info$ConfidenceIntervalDefinition <- "95%"

# Create the fish data
fishdata                          <- stockFishdata(FiY:LaY)

fishdata$Catches                  <- c(an(computeCatch(ARG))[1:nyrs],Catchint)

fishdata$Low_Recruitment                <- c(rec(ARG.sam)$lbnd[1:nyrs], Recintlbnd)
fishdata$Recruitment                    <- c(rec(ARG.sam)$value[1:nyrs], Recint)
fishdata$High_Recruitment               <- c(rec(ARG.sam)$ubnd[1:nyrs], Recintubnd) 

fishdata$Low_StockSize                  <- c(ssb(ARG.sam)$lbnd[1:nyrs], SSBintlbnd)
fishdata$StockSize                      <- c(ssb(ARG.sam)$value[1:nyrs], SSBint)
fishdata$High_StockSize                 <- c(ssb(ARG.sam)$ubnd[1:nyrs], SSBintubnd)

fishdata$Low_TBiomass[1:nyrs]           <- tsb(ARG.sam)$lbnd[1:nyrs]
fishdata$TBiomass[1:nyrs]               <- tsb(ARG.sam)$value[1:nyrs]
fishdata$High_TBiomass[1:nyrs]          <- tsb(ARG.sam)$ubnd[1:nyrs]

fishdata$Low_FishingPressure            <- c(fbar(ARG.sam)$lbnd[1:nyrs],Fbarintlbnd)
fishdata$FishingPressure                <- c(fbar(ARG.sam)$value[1:nyrs],Fbarint)
fishdata$High_FishingPressure           <- c(fbar(ARG.sam)$ubnd[1:nyrs],Fbarintubnd)

fishdata$CustomSeries2[1:nyrs]        <- catch(ARG.sam)$lbnd[1:nyrs]
fishdata$CustomSeries1[1:nyrs]        <- catch(ARG.sam)$value[1:nyrs]
fishdata$CustomSeries3[1:nyrs]        <- catch(ARG.sam)$ubnd[1:nyrs]

# View(fishdata)
write.csv(fishdata,file="./report/SAGinput.csv")
# upload to SAG
key <- icesSAG::uploadStock(info, fishdata)

# Get SAG settings
# getSAGSettingsForAStock(assessmentKey=key) %>% View()
# getSAGSettingsForAStock(assessmentKey=14095) %>% View()

# Add comment to SAG settings
# setSAGSettingForAStock(assessmentKey=key, 
#                        chartKey=0,
#                        settingKey=21,
#                        settingValue="My text for the comment field",
#                        copyNextYear=FALSE) 

