---
title: "EFA_Introduction"
output: html_document
---

## Exploratory Data Analysis (EFA)

library(psych)
library(dplyr)
# data preparation
pisa<-read.csv("PISA12-AUS-motivation.csv")
pisa_efa<-pisa %>% select (ST42Q02:ST29Q08) # select items measuring math self-concept, intrinsic value and utility value
pisa_efa[,2:12] <- apply(pisa_efa[,2:12], 2, function(x) x<- 5-x) # reverse code for nagetively worded items (not necessary)

* start with unrotated EFA
#using raw data and missing data is handled by pairwise (by default)
unrotated <- fa(pisa_efa, 3, rotate="none")
unrotated[c("loadings","score.cor", "TLI", "RMSEA")]

* orthogonal rotation: the factors are constrianed to be uncorrelated (i.e., factors are oriented at 90 degrees angle in multidimensional space)
out_varimax <- fa(pisa_efa, 3, rotate="varimax")
out_varimax[c("loadings", "score.cor", "TLI", "RMSEA")]
* oblique rotation: the factors are allowed to intercorrelated (i.e., permit factor axis orientations of less than 90 degrees)
out_promax <- fa(pisa_efa, 3, rotate="promax")
out_promax[c("loadings", "score.cor", "TLI", "RMSEA")]

* Target rotation: choose “simple structure” a priori and can be applied to oblique and orthogonal rotation
Targ_key <- make.keys(12,list(f1=1:4,f2=5:8,f3=9:12))
Targ_key <- scrub(Targ_key,isvalue=1)  #fix the 0s, allow the NAs to be estimated
Targ_key <- list(Targ_key)
out_targetQ <- fa(pisa_efa,3,rotate="TargetQ",Target=Targ_key, missing = TRUE) #TargetT for orthogonal rotation
out_targetQ[c("loadings", "score.cor")]
*using correlation matrix  and missing data is handled by FIML
pisa_cor <- corFiml(pisa_efa) # convert the raw data to correlation matrix uisng FIML
unrotated <- fa(pisa_cor, 3, rotate="none", n.obs = 14481)
out_varimax <- fa(pisa_cor, 3, rotate="varimax",n.obs = 14481)
out_promax <- fa(pisa_cor, 3, rotate="promax",n.obs = 14481)
out_targetQ <- fa(pisa_cor,3,rotate="TargetQ",n.obs = 14481,Target=Targ_key) #TargetT for orthogonal rotation
