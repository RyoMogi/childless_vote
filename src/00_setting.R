# ===========================================================================================
# The codes to replicate all the analyses for the published paper: 
# "The association between childlessness and voting turnout in 38 countries" 
# in Demographic Research.

# Ryohei Mogi, rymo@sdu.dk
# with Bruno Arpino
# ===========================================================================================

library(essurvey)
library(tidyverse)
library(countrycode)
library(openxlsx)
library(foreign)
library(rms)
library(nnet)
library(stargazer)
library(ggridges)
library(tidyselect)
library(manifestoR)
library(ggpubr)
library(specr)
library(survey)
library(srvyr)
`%out%` = Negate(`%in%`)
Mycol <- c("#08306B", "#238B45", "#FD8D3C", "#D4B9DA", "#FFEDA0")