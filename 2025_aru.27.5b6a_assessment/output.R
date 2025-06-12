## Extract results of interest, write TAF output tables

## Before:
## After:

library(icesTAF)
taf.library(FLCore)
taf.library(stockassessment)
taf.library(FLSAM)
library(tidyverse)

mkdir("output")
load(file="model/model.RData")


terminal.year <- range(ARG)["maxyear"]

### ============================================================================
### outputs
### ============================================================================
write.taf(flr2taf(ARG.sam@stock.n), "output/natage.csv")
write.taf(flr2taf(ARG.sam@harvest), "output/fatage.csv")


## Summary
vlu <- c("value", "lbnd", "ubnd")

summary <- data.frame(
  rec(ARG.sam)$year,
  rec(ARG.sam)[vlu],
  tsb(ARG.sam)[vlu],
  ssb(ARG.sam)[vlu],
  catch(ARG.sam)[vlu],
  fbar(ARG.sam)[vlu],
  c(catch(ARG)),
  c(sop(ARG)), row.names=NULL)

names(summary) <- c(
  "Year",
  "Rec",   "Rec_lo",   "Rec_hi",
  "TSB",   "TSB_lo",   "TSB_hi",
  "SSB",   "SSB_lo",   "SSB_hi",
  "Catch", "Catch_lo", "Catch_hi",
  "Fbar",  "Fbar_lo",  "Fbar_hi",
  "Landings", "SOP")
write.taf(summary, "output/summary_sf.csv")

df.mr <- data.frame(matrix(ncol = 3, nrow = 0))


df.mr <- rbind(df.mr,cbind(seq((terminal.year-5),terminal.year),
                           t(t(mohns.rho(ARG.retro,span=5,ref.year=terminal.year,type="ssb")$rho)),
                           rep('ssb',5,1)))
df.mr <- rbind(df.mr,cbind(seq((terminal.year-5),terminal.year),
                           t(t(mohns.rho(ARG.retro,span=5,ref.year=terminal.year,type="fbar")$rho)),rep('fbar',5,1)))
df.mr <- rbind(df.mr,cbind(seq((terminal.year-5),terminal.year),
                           t(t(mohns.rho(ARG.retro,span=5,ref.year=terminal.year,type="rec")$rho)),rep('rec',5,1)))

df.mr <- rbind(df.mr,cbind('av_5y',mean(mohns.rho(ARG.retro,span=5,ref.year=terminal.year,type="ssb")$rho),'ssb'))
df.mr <- rbind(df.mr,cbind('av_5y',mean(mohns.rho(ARG.retro,span=5,ref.year=terminal.year,type="fbar")$rho),'fbar'))
df.mr <- rbind(df.mr,cbind('av_5y',mean(mohns.rho(ARG.retro,span=5,ref.year=terminal.year,type="rec")$rho),'rec'))

colnames(df.mr) <- c('year','mohn_rho','var')

df.mr <- df.mr %>% pivot_wider(names_from = var, values_from = mohn_rho)

write.taf(df.mr, "output/mohn_rho.csv")
