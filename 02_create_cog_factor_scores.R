################################################################################
# This is a lightly modified version of the VETSA cogntive factor score 
# pipeline. It is based on the script CFS_01_Factor Score Syntax_10_21_2024.R. #
#                                                                              #
# The input file and output name will need to be set according to whether raw  #
# or adjusted data is being used.                                              #
################################################################################

library(dplyr)
library(readr)
library(psych)
library(haven)

### Set working directory
setwd("~/netshare/M/Projects/PracEffects_GEE")

### Set input data file. This should either be the raw or PE adjusted data.
# Adjusted
data <- read.csv("data/raw_data/V1V2V3V4_cog_data_pe-adjusted_2025-05-17.csv",head=T, stringsAsFactors = F)
# # Raw
# data <- read.csv("data/raw_data/V1V2V3V4_cog_data_raw_2025-05-17.csv",head=T, stringsAsFactors = F)

### Set output file name. This should correspond to either the raw of PE adjusted data.
# Adjusted
outfile = "data/output_data/V1V2V3V4_cog_factor_scores_pe-adjusted_2025-08-20.csv"
# # Raw
# outfile = "data/output_data/V1V2V3V4_cog_factor_scores_raw_2025-08-20.csv"

# Load admin file
admin <- read_sas("~/netshare/M/NAS VETSA MASTER DATAFILES/Master Data/Admin/vetsa_admin_file_20250205.sas7bdat", NULL)

# # remove V1NE participants
# admin <- admin %>% 
#   filter(!grepl("V1NE", VGRP_procvar)) 

# Rename to uppercase
admin <- admin %>% rename_all(toupper)

#Extract Case
df_admin <- admin %>% select(VETSAID, CASE)


# Merge data
dat <- df_admin %>% inner_join(data, by="VETSAID")

dat<-arrange(dat, VETSAID)
attach(dat)


####################################################################
###########     Standarize Variables to Wave 1      ################
####################################################################

## Memory
LMITOT_V1_stndV1 = scale(LMITOT_V1p)
LMITOT_V2_stndV1 = (LMITOT_V2p-mean(LMITOT_V1p,na.rm=T))/sd(LMITOT_V1p,na.rm=T)
LMITOT_V3_stndV1 = (LMITOT_V3p-mean(LMITOT_V1p,na.rm=T))/sd(LMITOT_V1p,na.rm=T)
LMITOT_V4_stndV1 = (LMITOT_V4p-mean(LMITOT_V1p,na.rm=T))/sd(LMITOT_V1p,na.rm=T)
LMDTOT_V1_stndV1 = scale(LMDTOT_V1p)
LMDTOT_V2_stndV1 = (LMDTOT_V2p-mean(LMDTOT_V1p,na.rm=T))/sd(LMDTOT_V1p,na.rm=T)
LMDTOT_V3_stndV1 = (LMDTOT_V3p-mean(LMDTOT_V1p,na.rm=T))/sd(LMDTOT_V1p,na.rm=T)
LMDTOT_V4_stndV1 = (LMDTOT_V4p-mean(LMDTOT_V1p,na.rm=T))/sd(LMDTOT_V1p,na.rm=T)

VRITOT_V1_stndV1 = scale(VRITOT_V1p)
VRITOT_V2_stndV1 = (VRITOT_V2p-mean(VRITOT_V1p,na.rm=T))/sd(VRITOT_V1p,na.rm=T)
VRITOT_V3_stndV1 = (VRITOT_V3p-mean(VRITOT_V1p,na.rm=T))/sd(VRITOT_V1p,na.rm=T)
VRITOT_V4_stndV1 = (VRITOT_V4p-mean(VRITOT_V1p,na.rm=T))/sd(VRITOT_V1p,na.rm=T)
VRDTOT_V1_stndV1 = scale(VRDTOT_V1p)
VRDTOT_V2_stndV1 = (VRDTOT_V2p-mean(VRDTOT_V1p,na.rm=T))/sd(VRDTOT_V1p,na.rm=T)
VRDTOT_V3_stndV1 = (VRDTOT_V3p-mean(VRDTOT_V1p,na.rm=T))/sd(VRDTOT_V1p,na.rm=T)
VRDTOT_V4_stndV1 = (VRDTOT_V4p-mean(VRDTOT_V1p,na.rm=T))/sd(VRDTOT_V1p,na.rm=T)
VRCTOT_V1_stndV1 = scale(VRCTOT_V1p)
VRCTOT_V2_stndV1 = (VRCTOT_V2p-mean(VRCTOT_V1p,na.rm=T))/sd(VRCTOT_V1p,na.rm=T)
VRCTOT_V3_stndV1 = (VRCTOT_V3p-mean(VRCTOT_V1p,na.rm=T))/sd(VRCTOT_V1p,na.rm=T)
VRCTOT_V4_stndV1 = (VRCTOT_V4p-mean(VRCTOT_V1p,na.rm=T))/sd(VRCTOT_V1p,na.rm=T)

CVATOT_V1_stndV1 = scale(CVATOT_V1p)
CVATOT_V2_stndV1 = (CVATOT_V2p-mean(CVATOT_V1p,na.rm=T))/sd(CVATOT_V1p,na.rm=T)
CVATOT_V3_stndV1 = (CVATOT_V3p-mean(CVATOT_V1p,na.rm=T))/sd(CVATOT_V1p,na.rm=T)
CVATOT_V4_stndV1 = (CVATOT_V4p-mean(CVATOT_V1p,na.rm=T))/sd(CVATOT_V1p,na.rm=T)
CVSDFR_V1_stndV1 = scale(CVSDFR_V1p)
CVSDFR_V2_stndV1 = (CVSDFR_V2p-mean(CVSDFR_V1p,na.rm=T))/sd(CVSDFR_V1p,na.rm=T)
CVSDFR_V3_stndV1 = (CVSDFR_V3p-mean(CVSDFR_V1p,na.rm=T))/sd(CVSDFR_V1p,na.rm=T)
CVSDFR_V4_stndV1 = (CVSDFR_V4p-mean(CVSDFR_V1p,na.rm=T))/sd(CVSDFR_V1p,na.rm=T)
CVLDFR_V1_stndV1 = scale(CVLDFR_V1p)
CVLDFR_V2_stndV1 = (CVLDFR_V2p-mean(CVLDFR_V1p,na.rm=T))/sd(CVLDFR_V1p,na.rm=T)
CVLDFR_V3_stndV1 = (CVLDFR_V3p-mean(CVLDFR_V1p,na.rm=T))/sd(CVLDFR_V1p,na.rm=T)
CVLDFR_V4_stndV1 = (CVLDFR_V4p-mean(CVLDFR_V1p,na.rm=T))/sd(CVLDFR_V1p,na.rm=T)


## EF and Speed
StroopCW_V1_stndV1 = scale(STRCWRAW_V1p)
StroopCW_V2_stndV1=(STRCWRAW_V2p-mean(STRCWRAW_V1p,na.rm=T))/sd(STRCWRAW_V1p,na.rm=T)
StroopCW_V3_stndV1=(STRCWRAW_V3p-mean(STRCWRAW_V1p,na.rm=T))/sd(STRCWRAW_V1p,na.rm=T)
StroopCW_V4_stndV1=(STRCWRAW_V4p-mean(STRCWRAW_V1p,na.rm=T))/sd(STRCWRAW_V1p,na.rm=T)
StroopC_V1_stndV1 = scale(STRCRAW_V1p)
StroopC_V2_stndV1=(STRCRAW_V2p-mean(STRCRAW_V1p,na.rm=T))/sd(STRCRAW_V1p,na.rm=T)
StroopC_V3_stndV1=(STRCRAW_V3p-mean(STRCRAW_V1p,na.rm=T))/sd(STRCRAW_V1p,na.rm=T)
StroopC_V4_stndV1=(STRCRAW_V4p-mean(STRCRAW_V1p,na.rm=T))/sd(STRCRAW_V1p,na.rm=T)
StroopW_V1_stndV1 = scale(STRWRAW_V1p)
StroopW_V2_stndV1=(STRWRAW_V2p-mean(STRWRAW_V1p,na.rm=T))/sd(STRWRAW_V1p,na.rm=T)
StroopW_V3_stndV1=(STRWRAW_V3p-mean(STRWRAW_V1p,na.rm=T))/sd(STRWRAW_V1p,na.rm=T)
StroopW_V4_stndV1=(STRWRAW_V4p-mean(STRWRAW_V1p,na.rm=T))/sd(STRWRAW_V1p,na.rm=T)

LNseq_V1_stndV1=scale(LNTOT_V1p)
LNseq_V2_stndV1=(LNTOT_V2p-mean(LNTOT_V1p,na.rm=T))/sd(LNTOT_V1p,na.rm=T)
LNseq_V3_stndV1=(LNTOT_V3p-mean(LNTOT_V1p,na.rm=T))/sd(LNTOT_V1p,na.rm=T)
LNseq_V4_stndV1=(LNTOT_V4p-mean(LNTOT_V1p,na.rm=T))/sd(LNTOT_V1p,na.rm=T)
RSasc_V1_stndV1=scale(RSATOT_V1p)
RSasc_V2_stndV1=(RSATOT_V2p-mean(RSATOT_V1p,na.rm=T))/sd(RSATOT_V1p,na.rm=T)
RSasc_V3_stndV1=(RSATOT_V3p-mean(RSATOT_V1p,na.rm=T))/sd(RSATOT_V1p,na.rm=T)
RSasc_V4_stndV1=(RSATOT_V4p-mean(RSATOT_V1p,na.rm=T))/sd(RSATOT_V1p,na.rm=T)
DStot_V1_stndV1=scale(DSTOT_V1p)
DStot_V2_stndV1=(DSTOT_V2p-mean(DSTOT_V1p,na.rm=T))/sd(DSTOT_V1p,na.rm=T)
DStot_V3_stndV1=(DSTOT_V3p-mean(DSTOT_V1p,na.rm=T))/sd(DSTOT_V1p,na.rm=T)
DStot_V4_stndV1=(DSTOT_V4p-mean(DSTOT_V1p,na.rm=T))/sd(DSTOT_V1p,na.rm=T)

TRL2rt <- exp(TRL2TLOG_V1p)
TRL3rt <- exp(TRL3TLOG_V1p)
TRL4rt <- exp(TRL4TLOG_V1p)
TRL2rt_V2p <- exp(TRL2TLOG_V2p)
TRL3rt_V2p <- exp(TRL3TLOG_V2p)
TRL4rt_V2p <- exp(TRL4TLOG_V2p)
TRL2rt_V3p <- exp(TRL2TLOG_V3p)
TRL3rt_V3p <- exp(TRL3TLOG_V3p)
TRL4rt_V3p <- exp(TRL4TLOG_V3p)

TRL2rt_V4p <- exp(TRL2TLOG_V4p)
TRL3rt_V4p <- exp(TRL3TLOG_V4p)
TRL4rt_V4p <- exp(TRL4TLOG_V4p)

Trail4_V1_stndV1=scale(TRL4rt)
Trail4_V2_stndV1=(TRL4rt_V2p-mean(TRL4rt,na.rm=T))/sd(TRL4rt,na.rm=T)
Trail4_V3_stndV1=(TRL4rt_V3p-mean(TRL4rt,na.rm=T))/sd(TRL4rt,na.rm=T)
Trail4_V4_stndV1=(TRL4rt_V4p-mean(TRL4rt,na.rm=T))/sd(TRL4rt,na.rm=T)
Trail3_V1_stndV1=scale(TRL3rt)
Trail3_V2_stndV1=(TRL3rt_V2p-mean(TRL3rt,na.rm=T))/sd(TRL3rt,na.rm=T)
Trail3_V3_stndV1=(TRL3rt_V3p-mean(TRL3rt,na.rm=T))/sd(TRL3rt,na.rm=T)
Trail3_V4_stndV1=(TRL3rt_V4p-mean(TRL3rt,na.rm=T))/sd(TRL3rt,na.rm=T)
Trail2_V1_stndV1=scale(TRL2rt)
Trail2_V2_stndV1=(TRL2rt_V2p-mean(TRL2rt,na.rm=T))/sd(TRL2rt,na.rm=T)
Trail2_V3_stndV1=(TRL2rt_V3p-mean(TRL2rt,na.rm=T))/sd(TRL2rt,na.rm=T)
Trail2_V4_stndV1=(TRL2rt_V4p-mean(TRL2rt,na.rm=T))/sd(TRL2rt,na.rm=T)

SRTGMEANLOG_V1_stndV1 = scale(SRTGMEANLOG_V1p)
SRTGMEANLOG_V2_stndV1 = (SRTGMEANLOG_V2p-mean(SRTGMEANLOG_V1p,na.rm=T))/sd(SRTGMEANLOG_V1p,na.rm=T)
SRTGMEANLOG_V3_stndV1 = (SRTGMEANLOG_V3p-mean(SRTGMEANLOG_V1p,na.rm=T))/sd(SRTGMEANLOG_V1p,na.rm=T)
SRTGMEANLOG_V4_stndV1 = (SRTGMEANLOG_V4p-mean(SRTGMEANLOG_V1p,na.rm=T))/sd(SRTGMEANLOG_V1p,na.rm=T)
CHRTGMEANLOG_V1_stndV1 = scale(CHRTGMEANLOG_V1p)
CHRTGMEANLOG_V2_stndV1 = (CHRTGMEANLOG_V2p-mean(CHRTGMEANLOG_V1p,na.rm=T))/sd(CHRTGMEANLOG_V1p,na.rm=T)
CHRTGMEANLOG_V3_stndV1 = (CHRTGMEANLOG_V3p-mean(CHRTGMEANLOG_V1p,na.rm=T))/sd(CHRTGMEANLOG_V1p,na.rm=T)
CHRTGMEANLOG_V4_stndV1 = (CHRTGMEANLOG_V4p-mean(CHRTGMEANLOG_V1p,na.rm=T))/sd(CHRTGMEANLOG_V1p,na.rm=T)

## Fluency
LFFCOR_V1_stndV1 = scale(LFFCOR_V1p)
LFFCOR_V2_stndV1 = (LFFCOR_V2p-mean(LFFCOR_V1p,na.rm=T))/sd(LFFCOR_V1p,na.rm=T)
LFFCOR_V3_stndV1 = (LFFCOR_V3p-mean(LFFCOR_V1p,na.rm=T))/sd(LFFCOR_V1p,na.rm=T)
LFFCOR_V4_stndV1 = (LFFCOR_V4p-mean(LFFCOR_V1p,na.rm=T))/sd(LFFCOR_V1p,na.rm=T)
LFACOR_V1_stndV1 = scale(LFACOR_V1p)
LFACOR_V2_stndV1 = (LFACOR_V2p-mean(LFACOR_V1p,na.rm=T))/sd(LFACOR_V1p,na.rm=T)
LFACOR_V3_stndV1 = (LFACOR_V3p-mean(LFACOR_V1p,na.rm=T))/sd(LFACOR_V1p,na.rm=T)
LFACOR_V4_stndV1 = (LFACOR_V4p-mean(LFACOR_V1p,na.rm=T))/sd(LFACOR_V1p,na.rm=T)
LFSCOR_V1_stndV1 = scale(LFSCOR_V1p)
LFSCOR_V2_stndV1 = (LFSCOR_V2p-mean(LFSCOR_V1p,na.rm=T))/sd(LFSCOR_V1p,na.rm=T)
LFSCOR_V3_stndV1 = (LFSCOR_V3p-mean(LFSCOR_V1p,na.rm=T))/sd(LFSCOR_V1p,na.rm=T)
LFSCOR_V4_stndV1 = (LFSCOR_V4p-mean(LFSCOR_V1p,na.rm=T))/sd(LFSCOR_V1p,na.rm=T)
LFCOR_V1_stndV1 = scale(LFCOR_V1p)
LFCOR_V2_stndV1 = (LFCOR_V2p-mean(LFCOR_V1p,na.rm=T))/sd(LFCOR_V1p,na.rm=T)
LFCOR_V3_stndV1 = (LFCOR_V3p-mean(LFCOR_V1p,na.rm=T))/sd(LFCOR_V1p,na.rm=T)
LFCOR_V4_stndV1 = (LFCOR_V4p-mean(LFCOR_V1p,na.rm=T))/sd(LFCOR_V1p,na.rm=T)

CFANCOR_V1_stndV1 = scale(CFANCOR_V1p)
CFANCOR_V2_stndV1 = (CFANCOR_V2p-mean(CFANCOR_V1p,na.rm=T))/sd(CFANCOR_V1p,na.rm=T)
CFANCOR_V3_stndV1 = (CFANCOR_V3p-mean(CFANCOR_V1p,na.rm=T))/sd(CFANCOR_V1p,na.rm=T)
CFANCOR_V4_stndV1 = (CFANCOR_V4p-mean(CFANCOR_V1p,na.rm=T))/sd(CFANCOR_V1p,na.rm=T)
CFBNCOR_V1_stndV1 = scale(CFBNCOR_V1p)
CFBNCOR_V2_stndV1 = (CFBNCOR_V2p-mean(CFBNCOR_V1p,na.rm=T))/sd(CFBNCOR_V1p,na.rm=T)
CFBNCOR_V3_stndV1 = (CFBNCOR_V3p-mean(CFBNCOR_V1p,na.rm=T))/sd(CFBNCOR_V1p,na.rm=T)
CFBNCOR_V4_stndV1 = (CFBNCOR_V4p-mean(CFBNCOR_V1p,na.rm=T))/sd(CFBNCOR_V1p,na.rm=T)
CSCOR_V1_stndV1 = scale(CSCOR_V1p)
CSCOR_V2_stndV1 = (CSCOR_V2p-mean(CSCOR_V1p,na.rm=T))/sd(CSCOR_V1p,na.rm=T)
CSCOR_V3_stndV1 = (CSCOR_V3p-mean(CSCOR_V1p,na.rm=T))/sd(CSCOR_V1p,na.rm=T)
CSCOR_V4_stndV1 = (CSCOR_V4p-mean(CSCOR_V1p,na.rm=T))/sd(CSCOR_V1p,na.rm=T)
CatFluency_V1_stndV1=scale(CFCOR_V1p)
CatFluency_V2_stndV1=(CFCOR_V2p-mean(CFCOR_V1p,na.rm=T))/sd(CFCOR_V1p,na.rm=T)
CatFluency_V3_stndV1=(CFCOR_V3p-mean(CFCOR_V1p,na.rm=T))/sd(CFCOR_V1p,na.rm=T)
CatFluency_V4_stndV1=(CFCOR_V4p-mean(CFCOR_V1p,na.rm=T))/sd(CFCOR_V1p,na.rm=T)

CatSwAcc_V1_stndV1=scale(CSSACC_V1p)
CatSwAcc_V2_stndV1=(CSSACC_V2p-mean(CSSACC_V1p,na.rm=T))/sd(CSSACC_V1p,na.rm=T)
CatSwAcc_V3_stndV1=(CSSACC_V3p-mean(CSSACC_V1p,na.rm=T))/sd(CSSACC_V1p,na.rm=T)
CatSwAcc_V4_stndV1=(CSSACC_V4p-mean(CSSACC_V1p,na.rm=T))/sd(CSSACC_V1p,na.rm=T)

#Visuospatial Other
HFTOTCOR_V1_stndV1 = scale(HFTOTCOR_V1p)
HFTOTCOR_V2_stndV1 = (HFTOTCOR_V2p-mean(HFTOTCOR_V1p,na.rm=T))/sd(HFTOTCOR_V1p,na.rm=T)
HFTOTCOR_V3_stndV1 = (HFTOTCOR_V3p-mean(HFTOTCOR_V1p,na.rm=T))/sd(HFTOTCOR_V1p,na.rm=T)
HFTOTCOR_V4_stndV1 = (HFTOTCOR_V4p-mean(HFTOTCOR_V1p,na.rm=T))/sd(HFTOTCOR_V1p,na.rm=T)
MR1COR_V1_stndV1 = scale(MR1COR_V1p)
MR1COR_V2_stndV1 = (MR1COR_V2p-mean(MR1COR_V1p,na.rm=T))/sd(MR1COR_V1p,na.rm=T)
MR1COR_V3_stndV1 = (MR1COR_V3p-mean(MR1COR_V1p,na.rm=T))/sd(MR1COR_V1p,na.rm=T)
MR1COR_V4_stndV1 = (MR1COR_V4p-mean(MR1COR_V1p,na.rm=T))/sd(MR1COR_V1p,na.rm=T)
MTXRAW_V1_stndV1 = scale(MTXRAW_V1p)
MTXRAW_V2_stndV1 = (MTXRAW_V2p-mean(MTXRAW_V1p,na.rm=T))/sd(MTXRAW_V1p,na.rm=T)
MTXRAW_V3_stndV1 = (MTXRAW_V3p-mean(MTXRAW_V1p,na.rm=T))/sd(MTXRAW_V1p,na.rm=T)
MTXRAW_V4_stndV1 = (MTXRAW_V4p-mean(MTXRAW_V1p,na.rm=T))/sd(MTXRAW_V1p,na.rm=T)


## VisSpat (including AFQT Box)
MR1COR_V1_stndV1 = scale(MR1COR_V1p)
MR1COR_V2_stndV1 = (MR1COR_V2p-mean(MR1COR_V1p,na.rm=T))/sd(MR1COR_V1p,na.rm=T)
MR1COR_V3_stndV1 = (MR1COR_V3p-mean(MR1COR_V1p,na.rm=T))/sd(MR1COR_V1p,na.rm=T)
MR1COR_V4_stndV1 = (MR1COR_V4p-mean(MR1COR_V1p,na.rm=T))/sd(MR1COR_V1p,na.rm=T)

AFQTBXPCTTRAN_R_V1_stndV1 = scale(AFQTBXPCTTRAN_R_V1p)
AFQTBXPCTTRAN_R_V2_stndV1 = (AFQTBXPCTTRAN_R_V2p-mean(AFQTBXPCTTRAN_R_V1p,na.rm=T))/sd(AFQTBXPCTTRAN_R_V1p,na.rm=T)
AFQTBXPCTTRAN_R_V3_stndV1 = (AFQTBXPCTTRAN_R_V3p-mean(AFQTBXPCTTRAN_R_V1p,na.rm=T))/sd(AFQTBXPCTTRAN_R_V1p,na.rm=T)
AFQTBXPCTTRAN_R_V4_stndV1 = (AFQTBXPCTTRAN_R_V4p-mean(AFQTBXPCTTRAN_R_V1p,na.rm=T))/sd(AFQTBXPCTTRAN_R_V1p,na.rm=T)

HFTOTCOR_V1_stndV1 = scale(HFTOTCOR_V1p)
HFTOTCOR_V2_stndV1 = (HFTOTCOR_V2p-mean(HFTOTCOR_V1p,na.rm=T))/sd(HFTOTCOR_V1p,na.rm=T)
HFTOTCOR_V3_stndV1 = (HFTOTCOR_V3p-mean(HFTOTCOR_V1p,na.rm=T))/sd(HFTOTCOR_V1p,na.rm=T)
HFTOTCOR_V4_stndV1 = (HFTOTCOR_V4p-mean(HFTOTCOR_V1p,na.rm=T))/sd(HFTOTCOR_V1p,na.rm=T)

## VisMem
sVRCTOT = sqrt(VRCTOT_V1p)
sVRCTOT_V2p = sqrt(VRCTOT_V2p)
sVRCTOT_V3p = sqrt(VRCTOT_V3p)
sVRCTOT_V4p = sqrt(VRCTOT_V4p)
sVRCTOT_V1_stndV1 = scale(sVRCTOT)
sVRCTOT_V2_stndV1 = (sVRCTOT_V2p-mean(sVRCTOT,na.rm=T))/sd(sVRCTOT,na.rm=T)
sVRCTOT_V3_stndV1 = (sVRCTOT_V3p-mean(sVRCTOT,na.rm=T))/sd(sVRCTOT,na.rm=T)
sVRCTOT_V4_stndV1 = (sVRCTOT_V4p-mean(sVRCTOT,na.rm=T))/sd(sVRCTOT,na.rm=T)
VRITOT_V1_stndV1 = scale(VRITOT_V1p)
VRITOT_V2_stndV1 = (VRITOT_V2p-mean(VRITOT_V1p,na.rm=T))/sd(VRITOT_V1p,na.rm=T)
VRITOT_V3_stndV1 = (VRITOT_V3p-mean(VRITOT_V1p,na.rm=T))/sd(VRITOT_V1p,na.rm=T)
VRITOT_V4_stndV1 = (VRITOT_V4p-mean(VRITOT_V1p,na.rm=T))/sd(VRITOT_V1p,na.rm=T)
VRDTOT_V1_stndV1 = scale(VRDTOT_V1p)
VRDTOT_V2_stndV1 = (VRDTOT_V2p-mean(VRDTOT_V1p,na.rm=T))/sd(VRDTOT_V1p,na.rm=T)
VRDTOT_V3_stndV1 = (VRDTOT_V3p-mean(VRDTOT_V1p,na.rm=T))/sd(VRDTOT_V1p,na.rm=T)
VRDTOT_V4_stndV1 = (VRDTOT_V4p-mean(VRDTOT_V1p,na.rm=T))/sd(VRDTOT_V1p,na.rm=T)

############################################
### Create interference scores for EF    ###
############################################
#
library(lme4)

Stroop_V1_lm <- lmer(STRCWRAW_V1p~STRCRAW_V1p+STRWRAW_V1p + (1|CASE), na.action=na.exclude) # Regress EF condition on baseline conditions (controlling for case)
Stroop_V1 <- resid(Stroop_V1_lm)+mean(STRCWRAW_V1p, na.rm=T)                        # save residuals into new variable after adding mean back in
Stroop_V1z <- scale(Stroop_V1)

Stroop_V2_lm <- lmer(STRCWRAW_V2p~STRCRAW_V2p+STRWRAW_V2p + (1|CASE), na.action=na.exclude)
Stroop_V2 <- (resid(Stroop_V2_lm)-mean(STRCWRAW_V1p, na.rm=T)+mean(STRCWRAW_V2p, na.rm=T))/sd(Stroop_V1, na.rm=T)
Stroop_V3_lm <- lmer(STRCWRAW_V3p~STRCRAW_V3p+STRWRAW_V3p + (1|CASE), na.action=na.exclude)
Stroop_V3 <- (resid(Stroop_V3_lm)-mean(STRCWRAW_V1p, na.rm=T)+mean(STRCWRAW_V3p, na.rm=T))/sd(Stroop_V1, na.rm=T)
Stroop_V4_lm <- lmer(STRCWRAW_V4p~STRCRAW_V4p+STRWRAW_V4p + (1|CASE), na.action=na.exclude)
Stroop_V4 <- (resid(Stroop_V4_lm)-mean(STRCWRAW_V1p, na.rm=T)+mean(STRCWRAW_V4p, na.rm=T))/sd(Stroop_V1, na.rm=T)


Trail_V1_lm <- lmer(TRL4rt~TRL3rt+TRL2rt + (1|CASE), na.action=na.exclude)
Trail_V1 <- resid(Trail_V1_lm)+mean(TRL4rt, na.rm=T)
Trail_V1z <- scale(Trail_V1)

Trail_V2_lm <- lmer(TRL4rt_V2p~TRL3rt_V2p+TRL2rt_V2p + (1|CASE), na.action=na.exclude)
Trail_V2 <- (resid(Trail_V2_lm)-mean(TRL4rt, na.rm=T)+mean(TRL4rt_V2p, na.rm=T))/sd(TRL4rt, na.rm=T)
Trail_V3_lm <- lmer(TRL4rt_V3p~TRL3rt_V3p+TRL2rt_V3p + (1|CASE), na.action=na.exclude)
Trail_V3 <- (resid(Trail_V3_lm)-mean(TRL4rt, na.rm=T)+mean(TRL4rt_V3p, na.rm=T))/sd(TRL4rt, na.rm=T)
Trail_V4_lm <- lmer(TRL4rt_V4p~TRL3rt_V4p+TRL2rt_V4p + (1|CASE), na.action=na.exclude)
Trail_V4 <- (resid(Trail_V4_lm)-mean(TRL4rt, na.rm=T)+mean(TRL4rt_V4p, na.rm=T))/sd(TRL4rt, na.rm=T)


CatSw_V1_lm <- lmer(CSSACC_V1p~CFANCOR_V1p+CFBNCOR_V1p + (1|CASE), na.action=na.exclude)
CatSw_V1 <- resid(CatSw_V1_lm)+mean(TRL4rt, na.rm=T)
CatSw_V1z <- scale(CatSw_V1)

CatSw_V2_lm <- lmer(CSSACC_V2p~CFANCOR_V2p+CFBNCOR_V2p + (1|CASE), na.action=na.exclude)
CatSw_V2 <- (resid(CatSw_V2_lm)-mean(CSSACC_V1p, na.rm=T)+mean(CSSACC_V2p, na.rm=T))/sd(CSSACC_V1p, na.rm=T)
CatSw_V3_lm <- lmer(CSSACC_V3p~CFANCOR_V3p+CFBNCOR_V3p + (1|CASE), na.action=na.exclude)
CatSw_V3 <- (resid(CatSw_V3_lm)-mean(CSSACC_V1p, na.rm=T)+mean(CSSACC_V3p, na.rm=T))/sd(CSSACC_V1p, na.rm=T)
CatSw_V4_lm <- lmer(CSSACC_V4p~CFANCOR_V4p+CFBNCOR_V4p + (1|CASE), na.action=na.exclude)
CatSw_V4 <- (resid(CatSw_V4_lm)-mean(CSSACC_V1p, na.rm=T)+mean(CSSACC_V4p, na.rm=T))/sd(CSSACC_V1p, na.rm=T)

################################################
########     IMPUTE MISSING DATA       #########
####    prep domains and exclude missing    ####
################################################

set.seed(1234)

#Create Datasets for imputation (dataset for each domain and wave, removing sjs missing too many tests, create dataset with and without ID var for MICE)
EF1x <- data.frame(VETSAID, Stroop_V1z, Trail_V1z, CatSw_V1z, LNseq_V1_stndV1, RSasc_V1_stndV1, DStot_V1_stndV1)
EF2x <- data.frame(VETSAID, Stroop_V2,  Trail_V2,  CatSw_V2,  LNseq_V2_stndV1, RSasc_V2_stndV1, DStot_V2_stndV1)
EF3x <- data.frame(VETSAID, Stroop_V3,  Trail_V3,  CatSw_V3,  LNseq_V3_stndV1, RSasc_V3_stndV1, DStot_V3_stndV1)
EF4x <- data.frame(VETSAID, Stroop_V4,  Trail_V4,  CatSw_V4,  LNseq_V4_stndV1, RSasc_V4_stndV1, DStot_V4_stndV1)
EF1y <- EF1x[rowSums(is.na(EF1x))<4,] # up to 3 tests can be missing
EF2y <- EF2x[rowSums(is.na(EF2x))<4,]
EF3y <- EF3x[rowSums(is.na(EF3x))<4,]
EF4y <- EF4x[rowSums(is.na(EF4x))<4,]
EF1 <- EF1y[,2:7]
EF2 <- EF2y[,2:7]
EF3 <- EF3y[,2:7]
EF4 <- EF4y[,2:7]

FLU1x <- data.frame(VETSAID, LFFCOR_V1_stndV1, LFACOR_V1_stndV1, LFSCOR_V1_stndV1, CFANCOR_V1_stndV1, CFBNCOR_V1_stndV1, CSCOR_V1_stndV1)
FLU2x <- data.frame(VETSAID, LFFCOR_V2_stndV1, LFACOR_V2_stndV1, LFSCOR_V2_stndV1, CFANCOR_V2_stndV1, CFBNCOR_V2_stndV1, CSCOR_V2_stndV1)
FLU3x <- data.frame(VETSAID, LFFCOR_V3_stndV1, LFACOR_V3_stndV1, LFSCOR_V3_stndV1, CFANCOR_V3_stndV1, CFBNCOR_V3_stndV1, CSCOR_V3_stndV1)
FLU4x <- data.frame(VETSAID, LFFCOR_V4_stndV1, LFACOR_V4_stndV1, LFSCOR_V4_stndV1, CFANCOR_V4_stndV1, CFBNCOR_V4_stndV1, CSCOR_V4_stndV1)
FLU1y <- FLU1x[rowSums(is.na(FLU1x))<4,] # up to 3 tests can be missing
FLU2y <- FLU2x[rowSums(is.na(FLU2x))<4,]
FLU3y <- FLU3x[rowSums(is.na(FLU3x))<4,]
FLU4y <- FLU4x[rowSums(is.na(FLU4x))<4,]
FLU1 <- FLU1y[,2:7]
FLU2 <- FLU2y[,2:7]
FLU3 <- FLU3y[,2:7]
FLU4 <- FLU4y[,2:7]

MEM1x <- data.frame(VETSAID, LMITOT_V1_stndV1, LMDTOT_V1_stndV1, VRITOT_V1_stndV1, VRDTOT_V1_stndV1, CVATOT_V1_stndV1, CVSDFR_V1_stndV1, CVLDFR_V1_stndV1)
MEM2x <- data.frame(VETSAID, LMITOT_V2_stndV1, LMDTOT_V2_stndV1, VRITOT_V2_stndV1, VRDTOT_V2_stndV1, CVATOT_V2_stndV1, CVSDFR_V2_stndV1, CVLDFR_V2_stndV1)
MEM3x <- data.frame(VETSAID, LMITOT_V3_stndV1, LMDTOT_V3_stndV1, VRITOT_V3_stndV1, VRDTOT_V3_stndV1, CVATOT_V3_stndV1, CVSDFR_V3_stndV1, CVLDFR_V3_stndV1)
MEM4x <- data.frame(VETSAID, LMITOT_V4_stndV1, LMDTOT_V4_stndV1, VRITOT_V4_stndV1, VRDTOT_V4_stndV1, CVATOT_V4_stndV1, CVSDFR_V4_stndV1, CVLDFR_V4_stndV1)
MEM1y <- MEM1x[rowSums(is.na(MEM1x))<4,] # up to 3 tests can be missing
MEM2y <- MEM2x[rowSums(is.na(MEM2x))<4,]
MEM3y <- MEM3x[rowSums(is.na(MEM3x))<4,]
MEM4y <- MEM4x[rowSums(is.na(MEM4x))<4,]
MEM1 <- MEM1y[,2:8]
MEM2 <- MEM2y[,2:8]
MEM3 <- MEM3y[,2:8]
MEM4 <- MEM4y[,2:8]

SPD1x <- data.frame(VETSAID, Trail2_V1_stndV1, Trail3_V1_stndV1, StroopW_V1_stndV1, StroopC_V1_stndV1, SRTGMEANLOG_V1_stndV1, CHRTGMEANLOG_V1_stndV1) 
SPD2x <- data.frame(VETSAID, Trail2_V2_stndV1, Trail3_V2_stndV1, StroopW_V2_stndV1, StroopC_V2_stndV1, SRTGMEANLOG_V2_stndV1, CHRTGMEANLOG_V2_stndV1) 
SPD3x <- data.frame(VETSAID, Trail2_V3_stndV1, Trail3_V3_stndV1, StroopW_V3_stndV1, StroopC_V3_stndV1, SRTGMEANLOG_V3_stndV1, CHRTGMEANLOG_V3_stndV1) 
SPD4x <- data.frame(VETSAID, Trail2_V4_stndV1, Trail3_V4_stndV1, StroopW_V4_stndV1, StroopC_V4_stndV1, SRTGMEANLOG_V4_stndV1, CHRTGMEANLOG_V4_stndV1) 
SPD1y <- SPD1x[rowSums(is.na(SPD1x))<4,] # up to 3 tests can be missing
SPD2y <- SPD2x[rowSums(is.na(SPD2x))<4,]
SPD3y <- SPD3x[rowSums(is.na(SPD3x))<4,]
SPD4y <- SPD4x[rowSums(is.na(SPD4x))<4,]
SPD1 <-  SPD1y[,2:7]
SPD2 <-  SPD2y[,2:7]
SPD3 <-  SPD3y[,2:7]
SPD4 <-  SPD4y[,2:7]


WM1x <- data.frame(VETSAID, LNseq_V1_stndV1, RSasc_V1_stndV1, DStot_V1_stndV1)
WM2x <- data.frame(VETSAID, LNseq_V2_stndV1, RSasc_V2_stndV1, DStot_V2_stndV1)
WM3x <- data.frame(VETSAID, LNseq_V3_stndV1, RSasc_V3_stndV1, DStot_V3_stndV1)
WM4x <- data.frame(VETSAID, LNseq_V4_stndV1, RSasc_V4_stndV1, DStot_V4_stndV1)
WM1y <- WM1x[rowSums(is.na(WM1x))<2,] # up to 1 test can be missing
WM2y <- WM2x[rowSums(is.na(WM2x))<2,]
WM3y <- WM3x[rowSums(is.na(WM3x))<2,]
WM4y <- WM4x[rowSums(is.na(WM4x))<2,]
WM1 <-  WM1y[,2:4]
WM2 <-  WM2y[,2:4]
WM3 <-  WM3y[,2:4]
WM4 <-  WM4y[,2:4]


SF1x <- data.frame(VETSAID, CFANCOR_V1_stndV1, CFBNCOR_V1_stndV1, CSCOR_V1_stndV1)
SF2x <- data.frame(VETSAID, CFANCOR_V2_stndV1, CFBNCOR_V2_stndV1, CSCOR_V2_stndV1)
SF3x <- data.frame(VETSAID, CFANCOR_V3_stndV1, CFBNCOR_V3_stndV1, CSCOR_V3_stndV1)
SF4x <- data.frame(VETSAID, CFANCOR_V4_stndV1, CFBNCOR_V4_stndV1, CSCOR_V4_stndV1)
SF1y <- SF1x[rowSums(is.na(SF1x))<2,] # up to 1 test can be missing
SF2y <- SF2x[rowSums(is.na(SF2x))<2,]
SF3y <- SF3x[rowSums(is.na(SF3x))<2,]
SF4y <- SF4x[rowSums(is.na(SF4x))<2,]
SF1 <-  SF1y[,2:4]
SF2 <-  SF2y[,2:4]
SF3 <-  SF3y[,2:4]
SF4 <-  SF4y[,2:4]

SPA1x <- data.frame(VETSAID, MR1COR_V1_stndV1, AFQTBXPCTTRAN_R_V1_stndV1, HFTOTCOR_V1_stndV1)
SPA2x <- data.frame(VETSAID, MR1COR_V2_stndV1, AFQTBXPCTTRAN_R_V2_stndV1, HFTOTCOR_V2_stndV1)
SPA3x <- data.frame(VETSAID, MR1COR_V3_stndV1, AFQTBXPCTTRAN_R_V3_stndV1, HFTOTCOR_V3_stndV1)
SPA4x <- data.frame(VETSAID, MR1COR_V4_stndV1, AFQTBXPCTTRAN_R_V4_stndV1, HFTOTCOR_V4_stndV1)
SPA1y <- SPA1x[rowSums(is.na(SPA1x))<2,] # up to 1 tests can be missing
SPA2y <- SPA2x[rowSums(is.na(SPA2x))<2,]
SPA3y <- SPA3x[rowSums(is.na(SPA3x))<2,]
SPA4y <- SPA4x[rowSums(is.na(SPA4x))<2,]
SPA1 <- SPA1y[,2:4]
SPA2 <- SPA2y[,2:4]
SPA3 <- SPA3y[,2:4]
SPA4 <- SPA4y[,2:4]

VMEM1x <- data.frame(VETSAID, sVRCTOT_V1_stndV1, VRITOT_V1_stndV1, VRDTOT_V1_stndV1)
VMEM2x <- data.frame(VETSAID, sVRCTOT_V2_stndV1, VRITOT_V2_stndV1, VRDTOT_V2_stndV1)
VMEM3x <- data.frame(VETSAID, sVRCTOT_V3_stndV1, VRITOT_V3_stndV1, VRDTOT_V3_stndV1)
VMEM4x <- data.frame(VETSAID, sVRCTOT_V4_stndV1, VRITOT_V4_stndV1, VRDTOT_V4_stndV1)
VMEM1y <- VMEM1x[rowSums(is.na(VMEM1x))<2,] # up to 1 tests can be missing
VMEM2y <- VMEM2x[rowSums(is.na(VMEM2x))<2,]
VMEM3y <- VMEM3x[rowSums(is.na(VMEM3x))<2,]
VMEM4y <- VMEM4x[rowSums(is.na(VMEM4x))<2,]
VMEM1 <- VMEM1y[,2:4]
VMEM2 <- VMEM2y[,2:4]
VMEM3 <- VMEM3y[,2:4]
VMEM4 <- VMEM4y[,2:4]


###################################
#### Actually run imputation ######
###################################
library(mice)

FLU1_imp <- mice(FLU1,m=5) # imputing the variables, 5 times
FLU2_imp <- mice(FLU2,m=5) 
FLU3_imp <- mice(FLU3,m=5) 
FLU4_imp <- mice(FLU4,m=5) 
EF1_imp <-  mice(EF1,m=5) 
EF2_imp <-  mice(EF2,m=5) 
EF3_imp <-  mice(EF3,m=5) 
EF4_imp <-  mice(EF4,m=5) 
MEM1_imp <- mice(MEM1,m=5) 
MEM2_imp <- mice(MEM2,m=5) 
MEM3_imp <- mice(MEM3,m=5) 
MEM4_imp <- mice(MEM4,m=5) 
SPD1_imp <- mice(SPD1,m=5) 
SPD2_imp <- mice(SPD2,m=5) 
SPD3_imp <- mice(SPD3,m=5) 
SPD4_imp <- mice(SPD4,m=5) 
WM1_imp <- mice(WM1,m=5) 
WM2_imp <- mice(WM2,m=5) 
WM3_imp <- mice(WM3,m=5) 
WM4_imp <- mice(WM4,m=5) 
SF1_imp <- mice(SF1,m=5) 
SF2_imp <- mice(SF2,m=5) 
SF3_imp <- mice(SF3,m=5) 
SF4_imp <- mice(SF4,m=5) 

VMEM1_imp <- mice(VMEM1,m=5) 
VMEM2_imp <- mice(VMEM2,m=5) 
VMEM3_imp <- mice(VMEM3,m=5) 
VMEM4_imp <- mice(VMEM4,m=5) 

SPA1_imp <- mice(SPA1,m=5) # imputing the variables, 5 times
SPA2_imp <- mice(SPA2,m=5) 
SPA3_imp <- mice(SPA3,m=5) 
SPA4_imp <- mice(SPA4,m=5) 

#data2 <- complete(imp,"long") #creating complete data set from imputed variables, stacked by imputation
FLU1_data2 <- complete(FLU1_imp,"long") #creating complete data set from imputed variables, stacked by imputation
FLU2_data2 <- complete(FLU2_imp,"long") 
FLU3_data2 <- complete(FLU3_imp,"long") 
FLU4_data2 <- complete(FLU4_imp,"long") 
EF1_data2 <- complete(EF1_imp,"long") 
EF2_data2 <- complete(EF2_imp,"long") 
EF3_data2 <- complete(EF3_imp,"long") 
EF4_data2 <- complete(EF4_imp,"long") 
MEM1_data2 <- complete(MEM1_imp,"long") 
MEM2_data2 <- complete(MEM2_imp,"long") 
MEM3_data2 <- complete(MEM3_imp,"long") 
MEM4_data2 <- complete(MEM4_imp,"long") 
SPD1_data2 <- complete(SPD1_imp,"long") 
SPD2_data2 <- complete(SPD2_imp,"long") 
SPD3_data2 <- complete(SPD3_imp,"long")
SPD4_data2 <- complete(SPD4_imp,"long")  
WM1_data2 <- complete(WM1_imp,"long") 
WM2_data2 <- complete(WM2_imp,"long") 
WM3_data2 <- complete(WM3_imp,"long") 
WM4_data2 <- complete(WM4_imp,"long") 
SF1_data2 <- complete(SF1_imp,"long") 
SF2_data2 <- complete(SF2_imp,"long") 
SF3_data2 <- complete(SF3_imp,"long") 
SF4_data2 <- complete(SF4_imp,"long") 

VMEM1_data2 <- complete(VMEM1_imp,"long") 
VMEM2_data2 <- complete(VMEM2_imp,"long") 
VMEM3_data2 <- complete(VMEM3_imp,"long") 
VMEM4_data2 <- complete(VMEM4_imp,"long") 

SPA1_data2 <- complete(SPA1_imp,"long") #creating complete data set from imputed variables, stacked by imputation
SPA2_data2 <- complete(SPA2_imp,"long") 
SPA3_data2 <- complete(SPA3_imp,"long") 
SPA4_data2 <- complete(SPA4_imp,"long") 

# .id variable in EF datasets needs to be converted to int
EF1_data2$.id = as.integer(EF1_data2$.id)
EF2_data2$.id = as.integer(EF2_data2$.id)
EF3_data2$.id = as.integer(EF3_data2$.id)
EF4_data2$.id = as.integer(EF4_data2$.id)

dataimp <- FLU1                                                                       # temporary dataset with imputed data
for(idx in seq_along(unique(FLU1_data2$.id))) {
      i <- unique(FLU1_data2$.id)[idx]
      dataimp[idx, ] <- colMeans(FLU1_data2[FLU1_data2$.id == i, 1:6])
}
FLU1_Imputed <- cbind(FLU1y[,1], dataimp)
names(FLU1_Imputed)[1] <- "VETSAID"                                                   # rename first column
# Checking Summaries post imputation, there should be no missing data
#summary(FLU1_Imputed)
#names(FLU1_Imputed)
#write.csv(V3_Imp, "V3_Imp.csv")
dataimp <- FLU2
for(idx in seq_along(unique(FLU2_data2$.id))) {
      i <- unique(FLU2_data2$.id)[idx]
      dataimp[idx, ] <- colMeans(FLU2_data2[FLU2_data2$.id == i, 1:6])
}
FLU2_Imputed <- cbind(FLU2y[,1], dataimp)
names(FLU2_Imputed)[1] <- "VETSAID"

dataimp <- FLU3
for(idx in seq_along(unique(FLU3_data2$.id))) {
      i <- unique(FLU3_data2$.id)[idx]
      dataimp[idx, ] <- colMeans(FLU3_data2[FLU3_data2$.id == i, 1:6])
}
FLU3_Imputed <- cbind(FLU3y[,1], dataimp)
names(FLU3_Imputed)[1] <- "VETSAID"

dataimp <- FLU4
for(idx in seq_along(unique(FLU4_data2$.id))) {
      i <- unique(FLU4_data2$.id)[idx]
      dataimp[idx, ] <- colMeans(FLU4_data2[FLU4_data2$.id == i, 1:6])
}
FLU4_Imputed <- cbind(FLU4y[,1], dataimp)
names(FLU4_Imputed)[1] <- "VETSAID"

dataimp <- EF1
for(idx in seq_along(unique(EF1_data2$.id))) {
      i <- unique(EF1_data2$.id)[idx]
      dataimp[idx, ] <- colMeans(EF1_data2[EF1_data2$.id == i, 1:6])
}
EF1_Imputed <- cbind(EF1y[,1], dataimp)
names(EF1_Imputed)[1] <- "VETSAID"

dataimp <- EF2
for(idx in seq_along(unique(EF2_data2$.id))) {
      i <- unique(EF2_data2$.id)[idx]
      dataimp[idx, ] <- colMeans(EF2_data2[EF2_data2$.id == i, 1:6])
}
EF2_Imputed <- cbind(EF2y[,1], dataimp)
names(EF2_Imputed)[1] <- "VETSAID"

dataimp <- EF3
for(idx in seq_along(unique(EF3_data2$.id))) {
      i <- unique(EF3_data2$.id)[idx]
      dataimp[idx, ] <- colMeans(EF3_data2[EF3_data2$.id == i, 1:6])
}
EF3_Imputed <- cbind(EF3y[,1], dataimp)
names(EF3_Imputed)[1] <- "VETSAID"

dataimp <- EF4
for(idx in seq_along(unique(EF4_data2$.id))) {
      i <- unique(EF4_data2$.id)[idx]
      dataimp[idx, ] <- colMeans(EF4_data2[EF4_data2$.id == i, 1:6])
}
EF4_Imputed <- cbind(EF4y[,1], dataimp)
names(EF4_Imputed)[1] <- "VETSAID"

dataimp <- MEM1
for(idx in seq_along(unique(MEM1_data2$.id))) {
      i <- unique(MEM1_data2$.id)[idx]
      dataimp[idx, ] <- colMeans(MEM1_data2[MEM1_data2$.id == i, 1:7])
}
MEM1_Imputed <- cbind(MEM1y[,1], dataimp)
names(MEM1_Imputed)[1] <- "VETSAID"

dataimp <- MEM2
for(idx in seq_along(unique(MEM2_data2$.id))) {
      i <- unique(MEM2_data2$.id)[idx]
      dataimp[idx, ] <- colMeans(MEM2_data2[MEM2_data2$.id == i, 1:7])
}
MEM2_Imputed <- cbind(MEM2y[,1], dataimp)
names(MEM2_Imputed)[1] <- "VETSAID"

dataimp <- MEM3
for(idx in seq_along(unique(MEM3_data2$.id))) {
      i <- unique(MEM3_data2$.id)[idx]
      dataimp[idx, ] <- colMeans(MEM3_data2[MEM3_data2$.id == i, 1:7])
}
MEM3_Imputed <- cbind(MEM3y[,1], dataimp)
names(MEM3_Imputed)[1] <- "VETSAID"

dataimp <- MEM4
for(idx in seq_along(unique(MEM4_data2$.id))) {
      i <- unique(MEM4_data2$.id)[idx]
      dataimp[idx, ] <- colMeans(MEM4_data2[MEM4_data2$.id == i, 1:7])
}
MEM4_Imputed <- cbind(MEM4y[,1], dataimp)
names(MEM4_Imputed)[1] <- "VETSAID"

dataimp <- SPD1
for(idx in seq_along(unique(SPD1_data2$.id))) {
      i <- unique(SPD1_data2$.id)[idx]
      dataimp[idx, ] <- colMeans(SPD1_data2[SPD1_data2$.id == i, 1:6])
}
SPD1_Imputed <- cbind(SPD1y[,1], dataimp)
names(SPD1_Imputed)[1] <- "VETSAID"

dataimp <- SPD2
for(idx in seq_along(unique(SPD2_data2$.id))) {
      i <- unique(SPD2_data2$.id)[idx]
      dataimp[idx, ] <- colMeans(SPD2_data2[SPD2_data2$.id == i, 1:6])
}
SPD2_Imputed <- cbind(SPD2y[,1], dataimp)
names(SPD2_Imputed)[1] <- "VETSAID"

dataimp <- SPD3
for(idx in seq_along(unique(SPD3_data2$.id))) {
      i <- unique(SPD3_data2$.id)[idx]
      dataimp[idx, ] <- colMeans(SPD3_data2[SPD3_data2$.id == i, 1:6])
}
SPD3_Imputed <- cbind(SPD3y[,1], dataimp)
names(SPD3_Imputed)[1] <- "VETSAID"

dataimp <- SPD4
for(idx in seq_along(unique(SPD4_data2$.id))) {
      i <- unique(SPD4_data2$.id)[idx]
      dataimp[idx, ] <- colMeans(SPD4_data2[SPD4_data2$.id == i, 1:6])
}
SPD4_Imputed <- cbind(SPD4y[,1], dataimp)
names(SPD4_Imputed)[1] <- "VETSAID"

dataimp <- WM1
for(idx in seq_along(unique(WM1_data2$.id))) {
      i <- unique(WM1_data2$.id)[idx]
      dataimp[idx, ] <- colMeans(WM1_data2[WM1_data2$.id == i, 1:3])
}
WM1_Imputed <- cbind(WM1y[,1], dataimp)
names(WM1_Imputed) <- c("VETSAID","LN1_wm","RS1_wm","DS1_wm")

dataimp <- WM2
for(idx in seq_along(unique(WM2_data2$.id))) {
      i <- unique(WM2_data2$.id)[idx]
      dataimp[idx, ] <- colMeans(WM2_data2[WM2_data2$.id == i, 1:3])
}
WM2_Imputed <- cbind(WM2y[,1], dataimp)
names(WM2_Imputed) <- c("VETSAID","LN2_wm","RS2_wm","DS2_wm")

dataimp <- WM3
for(idx in seq_along(unique(WM3_data2$.id))) {
      i <- unique(WM3_data2$.id)[idx]
      dataimp[idx, ] <- colMeans(WM3_data2[WM3_data2$.id == i, 1:3])
}
WM3_Imputed <- cbind(WM3y[,1], dataimp)
names(WM3_Imputed) <- c("VETSAID","LN3_wm","RS3_wm","DS3_wm")

dataimp <- WM4
for(idx in seq_along(unique(WM4_data2$.id))) {
      i <- unique(WM4_data2$.id)[idx]
      dataimp[idx, ] <- colMeans(WM4_data2[WM4_data2$.id == i, 1:3])
}
WM4_Imputed <- cbind(WM4y[,1], dataimp)
names(WM4_Imputed) <- c("VETSAID","LN4_wm","RS4_wm","DS4_wm")

dataimp <- SF1
for(idx in seq_along(unique(SF1_data2$.id))) {
      i <- unique(SF1_data2$.id)[idx]
      dataimp[idx, ] <- colMeans(SF1_data2[SF1_data2$.id == i, 1:3])
}
SF1_Imputed <- cbind(SF1y[,1], dataimp)
names(SF1_Imputed) <- c("VETSAID","AN1_sf","BN1_sf","CS1_sf")

dataimp <- SF2
for(idx in seq_along(unique(SF2_data2$.id))) {
      i <- unique(SF2_data2$.id)[idx]
      dataimp[idx, ] <- colMeans(SF2_data2[SF2_data2$.id == i, 1:3])
}
SF2_Imputed <- cbind(SF2y[,1], dataimp)
names(SF2_Imputed) <- c("VETSAID","AN2_sf","BN2_sf","CS2_sf")

dataimp <- SF3
for(idx in seq_along(unique(SF3_data2$.id))) {
      i <- unique(SF3_data2$.id)[idx]
      dataimp[idx, ] <- colMeans(SF3_data2[SF3_data2$.id == i, 1:3])
}
SF3_Imputed <- cbind(SF3y[,1], dataimp)
names(SF3_Imputed) <- c("VETSAID","AN3_sf","BN3_sf","CS3_sf")

dataimp <- SF4
for(idx in seq_along(unique(SF4_data2$.id))) {
      i <- unique(SF4_data2$.id)[idx]
      dataimp[idx, ] <- colMeans(SF4_data2[SF4_data2$.id == i, 1:3])
}
SF4_Imputed <- cbind(SF4y[,1], dataimp)
names(SF4_Imputed) <- c("VETSAID","AN4_sf","BN4_sf","CS4_sf")

dataimp <- VMEM1
for(idx in seq_along(unique(VMEM1_data2$.id))) {
      i <- unique(VMEM1_data2$.id)[idx]
      dataimp[idx, ] <- colMeans(VMEM1_data2[VMEM1_data2$.id == i, 1:3])
}
VMEM1_Imputed <- cbind(VMEM1y[,1], dataimp)
names(VMEM1_Imputed)[1] <- "VETSAID"

dataimp <- VMEM2
for(idx in seq_along(unique(VMEM2_data2$.id))) {
      i <- unique(VMEM2_data2$.id)[idx]
      dataimp[idx, ] <- colMeans(VMEM2_data2[VMEM2_data2$.id == i, 1:3])
}
VMEM2_Imputed <- cbind(VMEM2y[,1], dataimp)
names(VMEM2_Imputed)[1] <- "VETSAID"

dataimp <- VMEM3
for(idx in seq_along(unique(VMEM3_data2$.id))) {
      i <- unique(VMEM3_data2$.id)[idx]
      dataimp[idx, ] <- colMeans(VMEM3_data2[VMEM3_data2$.id == i, 1:3])
}
VMEM3_Imputed <- cbind(VMEM3y[,1], dataimp)
names(VMEM3_Imputed)[1] <- "VETSAID"

dataimp <- VMEM4
for(idx in seq_along(unique(VMEM4_data2$.id))) {
      i <- unique(VMEM4_data2$.id)[idx]
      dataimp[idx, ] <- colMeans(VMEM4_data2[VMEM4_data2$.id == i, 1:3])
}
VMEM4_Imputed <- cbind(VMEM4y[,1], dataimp)
names(VMEM4_Imputed)[1] <- "VETSAID"

dataimp <- SPA1
for(idx in seq_along(unique(SPA1_data2$.id))) {
      i <- unique(SPA1_data2$.id)[idx]
      dataimp[idx, ] <- colMeans(SPA1_data2[SPA1_data2$.id == i, 1:3])
}
SPA1_Imputed <- cbind(SPA1y[,1], dataimp)
names(SPA1_Imputed)[1] <- "VETSAID"

dataimp <- SPA2
for(idx in seq_along(unique(SPA2_data2$.id))) {
      i <- unique(SPA2_data2$.id)[idx]
      dataimp[idx, ] <- colMeans(SPA2_data2[SPA2_data2$.id == i, 1:3])
}
SPA2_Imputed <- cbind(SPA2y[,1], dataimp)
names(SPA2_Imputed)[1] <- "VETSAID"

dataimp <- SPA3
for(idx in seq_along(unique(SPA3_data2$.id))) {
      i <- unique(SPA3_data2$.id)[idx]
      dataimp[idx, ] <- colMeans(SPA3_data2[SPA3_data2$.id == i, 1:3])
}
SPA3_Imputed <- cbind(SPA3y[,1], dataimp)
names(SPA3_Imputed)[1] <- "VETSAID"

dataimp <- SPA4
for(idx in seq_along(unique(SPA4_data2$.id))) {
      i <- unique(SPA4_data2$.id)[idx]
      dataimp[idx, ] <- colMeans(SPA4_data2[SPA4_data2$.id == i, 1:3])
}
SPA4_Imputed <- cbind(SPA4y[,1], dataimp)
names(SPA4_Imputed)[1] <- "VETSAID"

###########################################
#####      Create Factor Scores      ######
###########################################
##############################
##### NO IMPUTATION ##########
##############################
# Sum across standardized factor loadings from latent variable models, then re-scale all factor scores based on M/SD of wave 1 factor scores
commonEF_V1x <- .31*Stroop_V1z+ (-1)*.55*Trail_V1z+ .22*CatSw_V1z+ .49*LNseq_V1_stndV1 + .38*RSasc_V1_stndV1 + .44*DStot_V1_stndV1
commonEF_V2x <- .31*Stroop_V2 + (-1)*.55*Trail_V2 + .22*CatSw_V2 + .49*LNseq_V2_stndV1 + .38*RSasc_V2_stndV1 + .44*DStot_V2_stndV1
commonEF_V3x <- .31*Stroop_V3 + (-1)*.55*Trail_V3 + .22*CatSw_V3 + .49*LNseq_V3_stndV1 + .38*RSasc_V3_stndV1 + .44*DStot_V3_stndV1
commonEF_V4x <- .31*Stroop_V4 + (-1)*.55*Trail_V4 + .22*CatSw_V4 + .49*LNseq_V4_stndV1 + .38*RSasc_V4_stndV1 + .44*DStot_V4_stndV1
commonEF_V1 <- scale(commonEF_V1x)
commonEF_V2 <- (commonEF_V2x-mean(commonEF_V1x,na.rm=T))/sd(commonEF_V1x,na.rm=T)
commonEF_V3 <- (commonEF_V3x-mean(commonEF_V1x,na.rm=T))/sd(commonEF_V1x,na.rm=T)
commonEF_V4 <- (commonEF_V4x-mean(commonEF_V1x,na.rm=T))/sd(commonEF_V1x,na.rm=T)

WM_V1x <- .705*LNseq_V1_stndV1 + .606*RSasc_V1_stndV1 + .828*DStot_V1_stndV1
WM_V2x <- .705*LNseq_V2_stndV1 + .606*RSasc_V2_stndV1 + .828*DStot_V2_stndV1
WM_V3x <- .705*LNseq_V3_stndV1 + .606*RSasc_V3_stndV1 + .828*DStot_V3_stndV1
WM_V4x <- .705*LNseq_V4_stndV1 + .606*RSasc_V4_stndV1 + .828*DStot_V4_stndV1
work_mem_V1 <- scale(WM_V1x)
work_mem_V2 <- (WM_V2x-mean(WM_V1x,na.rm=T))/sd(WM_V1x,na.rm=T)
work_mem_V3 <- (WM_V3x-mean(WM_V1x,na.rm=T))/sd(WM_V1x,na.rm=T)
work_mem_V4 <- (WM_V4x-mean(WM_V1x,na.rm=T))/sd(WM_V1x,na.rm=T)

fluency_V1x <- .77*LFFCOR_V1_stndV1 + .78*LFACOR_V1_stndV1 + .80*LFSCOR_V1_stndV1 + .43*CFANCOR_V1_stndV1 + .54*CFBNCOR_V1_stndV1 + .36*CSCOR_V1_stndV1
fluency_V2x <- .77*LFFCOR_V2_stndV1 + .78*LFACOR_V2_stndV1 + .80*LFSCOR_V2_stndV1 + .43*CFANCOR_V2_stndV1 + .54*CFBNCOR_V2_stndV1 + .36*CSCOR_V2_stndV1
fluency_V3x <- .77*LFFCOR_V3_stndV1 + .78*LFACOR_V3_stndV1 + .80*LFSCOR_V3_stndV1 + .43*CFANCOR_V3_stndV1 + .54*CFBNCOR_V3_stndV1 + .36*CSCOR_V3_stndV1
fluency_V4x <- .77*LFFCOR_V4_stndV1 + .78*LFACOR_V4_stndV1 + .80*LFSCOR_V4_stndV1 + .43*CFANCOR_V4_stndV1 + .54*CFBNCOR_V4_stndV1 + .36*CSCOR_V4_stndV1
fluency_V1 <- scale(fluency_V1x)
fluency_V2 <- (fluency_V2x-mean(fluency_V1x,na.rm=T))/sd(fluency_V1x,na.rm=T)
fluency_V3 <- (fluency_V3x-mean(fluency_V1x,na.rm=T))/sd(fluency_V1x,na.rm=T)
fluency_V4 <- (fluency_V4x-mean(fluency_V1x,na.rm=T))/sd(fluency_V1x,na.rm=T)

semantic_flu_V1x <- .685*CFANCOR_V1_stndV1 + .623*CFBNCOR_V1_stndV1 + .516*CSCOR_V1_stndV1
semantic_flu_V2x <- .685*CFANCOR_V2_stndV1 + .623*CFBNCOR_V2_stndV1 + .516*CSCOR_V2_stndV1
semantic_flu_V3x <- .685*CFANCOR_V3_stndV1 + .623*CFBNCOR_V3_stndV1 + .516*CSCOR_V3_stndV1
semantic_flu_V4x <- .685*CFANCOR_V4_stndV1 + .623*CFBNCOR_V4_stndV1 + .516*CSCOR_V4_stndV1
semantic_flu_V1 <- scale(semantic_flu_V1x)
semantic_flu_V2 <- (semantic_flu_V2x-mean(semantic_flu_V1x,na.rm=T))/sd(semantic_flu_V1x,na.rm=T)
semantic_flu_V3 <- (semantic_flu_V3x-mean(semantic_flu_V1x,na.rm=T))/sd(semantic_flu_V1x,na.rm=T)
semantic_flu_V4 <- (semantic_flu_V4x-mean(semantic_flu_V1x,na.rm=T))/sd(semantic_flu_V1x,na.rm=T)

# Original Version without CVLT learning trials
#memory_V1x <- .87*.57*LMITOT_V1_stndV1 + .97*.57*LMDTOT_V1_stndV1 + .64*.47*VRITOT_V1_stndV1 + .88*.47*VRDTOT_V1_stndV1 + .83*.68*CVSDFR_V1_stndV1 + .96*.68*CVLDFR_V1_stndV1
#memory_V2x <- .87*.57*LMITOT_V2_stndV1 + .97*.57*LMDTOT_V2_stndV1 + .64*.47*VRITOT_V2_stndV1 + .88*.47*VRDTOT_V2_stndV1 + .83*.68*CVSDFR_V2_stndV1 + .96*.68*CVLDFR_V2_stndV1
#memory_V3x <- .87*.57*LMITOT_V3_stndV1 + .97*.57*LMDTOT_V3_stndV1 + .64*.47*VRITOT_V3_stndV1 + .88*.47*VRDTOT_V3_stndV1 + .83*.68*CVSDFR_V3_stndV1 + .96*.68*CVLDFR_V3_stndV1
#memory_V1 <- scale(memory_V1x)
#memory_V2 <- (memory_V2x-mean(memory_V1x,na.rm=T))/sd(memory_V1x,na.rm=T)
#memory_V3 <- (memory_V3x-mean(memory_V1x,na.rm=T))/sd(memory_V1x,na.rm=T)

memory_V1x <- .91*.66*LMITOT_V1_stndV1 + .94*.66*LMDTOT_V1_stndV1 + .75*.60*VRITOT_V1_stndV1 + .85*.60*VRDTOT_V1_stndV1 + .18*CVATOT_V1_stndV1 + .68*.72*CVATOT_V1_stndV1 + .90*.72*CVSDFR_V1_stndV1 + .91*.72*CVLDFR_V1_stndV1
memory_V2x <- .91*.66*LMITOT_V2_stndV1 + .94*.66*LMDTOT_V2_stndV1 + .75*.60*VRITOT_V2_stndV1 + .85*.60*VRDTOT_V2_stndV1 + .18*CVATOT_V2_stndV1 + .68*.72*CVATOT_V2_stndV1 + .90*.72*CVSDFR_V2_stndV1 + .91*.72*CVLDFR_V2_stndV1
memory_V3x <- .91*.66*LMITOT_V3_stndV1 + .94*.66*LMDTOT_V3_stndV1 + .75*.60*VRITOT_V3_stndV1 + .85*.60*VRDTOT_V3_stndV1 + .18*CVATOT_V3_stndV1 + .68*.72*CVATOT_V3_stndV1 + .90*.72*CVSDFR_V3_stndV1 + .91*.72*CVLDFR_V3_stndV1
memory_V4x <- .91*.66*LMITOT_V4_stndV1 + .94*.66*LMDTOT_V4_stndV1 + .75*.60*VRITOT_V4_stndV1 + .85*.60*VRDTOT_V4_stndV1 + .18*CVATOT_V4_stndV1 + .68*.72*CVATOT_V4_stndV1 + .90*.72*CVSDFR_V4_stndV1 + .91*.72*CVLDFR_V4_stndV1
memory_V1 <- scale(memory_V1x)
memory_V2 <- (memory_V2x-mean(memory_V1x,na.rm=T))/sd(memory_V1x,na.rm=T)
memory_V3 <- (memory_V3x-mean(memory_V1x,na.rm=T))/sd(memory_V1x,na.rm=T)
memory_V4 <- (memory_V4x-mean(memory_V1x,na.rm=T))/sd(memory_V1x,na.rm=T)

speed_V1x <- (-1)*.66*.56*Trail2_V1_stndV1 + (-1)*.84*.56*Trail3_V1_stndV1 + .74*.82*StroopW_V1_stndV1 + .83*.82*StroopC_V1_stndV1 + (-1)*.76*.54*SRTGMEANLOG_V1_stndV1 + (-1)*.86*.54*CHRTGMEANLOG_V1_stndV1 
speed_V2x <- (-1)*.66*.56*Trail2_V2_stndV1 + (-1)*.84*.56*Trail3_V2_stndV1 + .74*.82*StroopW_V2_stndV1 + .83*.82*StroopC_V2_stndV1 + (-1)*.76*.54*SRTGMEANLOG_V2_stndV1 + (-1)*.86*.54*CHRTGMEANLOG_V2_stndV1 
speed_V3x <- (-1)*.66*.56*Trail2_V3_stndV1 + (-1)*.84*.56*Trail3_V3_stndV1 + .74*.82*StroopW_V3_stndV1 + .83*.82*StroopC_V3_stndV1 + (-1)*.76*.54*SRTGMEANLOG_V3_stndV1 + (-1)*.86*.54*CHRTGMEANLOG_V3_stndV1 
speed_V4x <- (-1)*.66*.56*Trail2_V4_stndV1 + (-1)*.84*.56*Trail3_V4_stndV1 + .74*.82*StroopW_V4_stndV1 + .83*.82*StroopC_V4_stndV1 + (-1)*.76*.54*SRTGMEANLOG_V4_stndV1 + (-1)*.86*.54*CHRTGMEANLOG_V4_stndV1 
speed_V1 <- scale(speed_V1x)
speed_V2 <- (speed_V2x-mean(speed_V1x,na.rm=T))/sd(speed_V1x,na.rm=T)
speed_V3 <- (speed_V3x-mean(speed_V1x,na.rm=T))/sd(speed_V1x,na.rm=T)
speed_V4 <- (speed_V4x-mean(speed_V1x,na.rm=T))/sd(speed_V1x,na.rm=T)

VMEM_V1x <- 0.05350*sVRCTOT_V1_stndV1 + 0.80128*VRITOT_V1_stndV1 + 0.16498*VRDTOT_V1_stndV1
VMEM_V2x <- 0.05350*sVRCTOT_V2_stndV1 + 0.80128*VRITOT_V2_stndV1 + 0.16498*VRDTOT_V2_stndV1
VMEM_V3x <- 0.05350*sVRCTOT_V3_stndV1 + 0.80128*VRITOT_V3_stndV1 + 0.16498*VRDTOT_V3_stndV1
VMEM_V4x <- 0.05350*sVRCTOT_V4_stndV1 + 0.80128*VRITOT_V4_stndV1 + 0.16498*VRDTOT_V4_stndV1
VisMem_V1 <- scale(VMEM_V1x)
VisMem_V2 <- (VMEM_V2x-mean(VMEM_V1x,na.rm=T))/sd(VMEM_V1x,na.rm=T)
VisMem_V3 <- (VMEM_V3x-mean(VMEM_V1x,na.rm=T))/sd(VMEM_V1x,na.rm=T)
VisMem_V4 <- (VMEM_V4x-mean(VMEM_V1x,na.rm=T))/sd(VMEM_V1x,na.rm=T)

#Standardized Scoring Coefficients from Proc Factor n=1 priors=smc method=ML (CR)
SPA_V1x <- 0.16715*MR1COR_V1_stndV1 + 0.27901*AFQTBXPCTTRAN_R_V1_stndV1 + 0.59952*HFTOTCOR_V1_stndV1
SPA_V2x <- 0.16715*MR1COR_V2_stndV1 + 0.27901*AFQTBXPCTTRAN_R_V2_stndV1 + 0.59952*HFTOTCOR_V2_stndV1
SPA_V3x <- 0.16715*MR1COR_V3_stndV1 + 0.27901*AFQTBXPCTTRAN_R_V3_stndV1 + 0.59952*HFTOTCOR_V3_stndV1
SPA_V4x <- 0.16715*MR1COR_V4_stndV1 + 0.27901*AFQTBXPCTTRAN_R_V4_stndV1 + 0.59952*HFTOTCOR_V4_stndV1
VisSpat_V1 <- scale(SPA_V1x)
VisSpat_V2 <- (SPA_V2x-mean(SPA_V1x,na.rm=T))/sd(SPA_V1x,na.rm=T)
VisSpat_V3 <- (SPA_V3x-mean(SPA_V1x,na.rm=T))/sd(SPA_V1x,na.rm=T)
VisSpat_V4 <- (SPA_V4x-mean(SPA_V1x,na.rm=T))/sd(SPA_V1x,na.rm=T)


###################################################
#####   Factor Scores from Imputed Data  ##########
###################################################
library(dplyr)
#merge imputed datasets
Part1FLU <- merge(merge(FLU1_Imputed, FLU2_Imputed, by = "VETSAID", all = TRUE),
                  merge(FLU3_Imputed, FLU4_Imputed, by = "VETSAID", all = TRUE),
                  by = "VETSAID", all = TRUE)

Part2EF <- merge(merge(merge(Part1FLU, EF1_Imputed, by = "VETSAID", all = TRUE),
                       EF2_Imputed, by = "VETSAID", all = TRUE),
                 merge(EF3_Imputed, EF4_Imputed, by = "VETSAID", all = TRUE),
                 by = "VETSAID", all = TRUE)

Part3MEM <- merge(merge(merge(Part2EF, MEM1_Imputed, by = "VETSAID", all = TRUE),
                       MEM2_Imputed, by = "VETSAID", all = TRUE),
                 merge(MEM3_Imputed, MEM4_Imputed, by = "VETSAID", all = TRUE),
                 by = "VETSAID", all = TRUE)

Part4SPD <- merge(merge(merge(Part3MEM, SPD1_Imputed, by = "VETSAID", all = TRUE),
                       SPD2_Imputed, by = "VETSAID", all = TRUE),
                 merge(SPD3_Imputed, SPD4_Imputed, by = "VETSAID", all = TRUE),
                 by = "VETSAID", all = TRUE)

Part5WM <- merge(merge(merge(Part4SPD, WM1_Imputed, by = "VETSAID", all = TRUE),
                      WM2_Imputed, by = "VETSAID", all = TRUE),
                merge(WM3_Imputed, WM4_Imputed, by = "VETSAID", all = TRUE),
                by = "VETSAID", all = TRUE)

Part6SF <- merge(merge(merge(Part5WM, SF1_Imputed, by = "VETSAID", all = TRUE),
                      SF2_Imputed, by = "VETSAID", all = TRUE),
                merge(SF3_Imputed, SF4_Imputed, by = "VETSAID", all = TRUE),
                by = "VETSAID", all = TRUE)

Part7SPA <- merge(merge(merge(Part6SF, SPA1_Imputed, by = "VETSAID", all = TRUE),
                       SPA2_Imputed, by = "VETSAID", all = TRUE),
                 merge(SPA3_Imputed, SPA4_Imputed, by = "VETSAID", all = TRUE),
                 by = "VETSAID", all = TRUE)

Part8VMEM <- merge(merge(merge(Part7SPA, VMEM1_Imputed, by = "VETSAID", all = TRUE),
                        VMEM2_Imputed, by = "VETSAID", all = TRUE),
                  merge(VMEM3_Imputed, VMEM4_Imputed, by = "VETSAID", all = TRUE),
                  by = "VETSAID", all = TRUE)

#IDs <- data.frame(VETSAID, VETSAWAVE)
IDs <- data.frame(VETSAID)
all_Imputed <- merge(Part8VMEM, IDs, by = "VETSAID", all=T)
# Remove duplicated variables and rename
all_Imputed <- all_Imputed %>% select(-ends_with(".y"))
names(all_Imputed) = gsub(".x","",names(all_Imputed))

commonEF_V1x_imputed <- .31*all_Imputed$Stroop_V1z+ (-1)*.55*all_Imputed$Trail_V1z+ .22*all_Imputed$CatSw_V1z+ .49*all_Imputed$LNseq_V1_stndV1 + .38*all_Imputed$RSasc_V1_stndV1 + .44*all_Imputed$DStot_V1_stndV1
commonEF_V2x_imputed <- .31*all_Imputed$Stroop_V2 + (-1)*.55*all_Imputed$Trail_V2 + .22*all_Imputed$CatSw_V2 + .49*all_Imputed$LNseq_V2_stndV1 + .38*all_Imputed$RSasc_V2_stndV1 + .44*all_Imputed$DStot_V2_stndV1
commonEF_V3x_imputed <- .31*all_Imputed$Stroop_V3 + (-1)*.55*all_Imputed$Trail_V3 + .22*all_Imputed$CatSw_V3 + .49*all_Imputed$LNseq_V3_stndV1 + .38*all_Imputed$RSasc_V3_stndV1 + .44*all_Imputed$DStot_V3_stndV1
commonEF_V4x_imputed <- .31*all_Imputed$Stroop_V4 + (-1)*.55*all_Imputed$Trail_V4 + .22*all_Imputed$CatSw_V4 + .49*all_Imputed$LNseq_V4_stndV1 + .38*all_Imputed$RSasc_V4_stndV1 + .44*all_Imputed$DStot_V4_stndV1
commonEF_V1_imputed <- scale(commonEF_V1x_imputed)
commonEF_V2_imputed <- (commonEF_V2x_imputed-mean(commonEF_V1x_imputed,na.rm=T))/sd(commonEF_V1x_imputed,na.rm=T)
commonEF_V3_imputed <- (commonEF_V3x_imputed-mean(commonEF_V1x_imputed,na.rm=T))/sd(commonEF_V1x_imputed,na.rm=T)
commonEF_V4_imputed <- (commonEF_V4x_imputed-mean(commonEF_V1x_imputed,na.rm=T))/sd(commonEF_V1x_imputed,na.rm=T)

WM_V1x_imputed <- .705*all_Imputed$LN1_wm + .606*all_Imputed$RS1_wm + .828*all_Imputed$DS1_wm
WM_V2x_imputed <- .705*all_Imputed$LN2_wm + .606*all_Imputed$RS2_wm + .828*all_Imputed$DS2_wm
WM_V3x_imputed <- .705*all_Imputed$LN3_wm + .606*all_Imputed$RS3_wm + .828*all_Imputed$DS3_wm
WM_V4x_imputed <- .705*all_Imputed$LN4_wm + .606*all_Imputed$RS4_wm + .828*all_Imputed$DS4_wm
work_mem_V1_imputed <- scale(WM_V1x_imputed)
work_mem_V2_imputed <- (WM_V2x_imputed-mean(WM_V1x_imputed,na.rm=T))/sd(WM_V1x_imputed,na.rm=T)
work_mem_V3_imputed <- (WM_V3x_imputed-mean(WM_V1x_imputed,na.rm=T))/sd(WM_V1x_imputed,na.rm=T)
work_mem_V4_imputed <- (WM_V4x_imputed-mean(WM_V1x_imputed,na.rm=T))/sd(WM_V1x_imputed,na.rm=T)


fluency_V1x_imputed <- .77*all_Imputed$LFFCOR_V1_stndV1 + .78*all_Imputed$LFACOR_V1_stndV1 + .80*all_Imputed$LFSCOR_V1_stndV1 + .43*all_Imputed$CFANCOR_V1_stndV1 + .54*all_Imputed$CFBNCOR_V1_stndV1 + .36*all_Imputed$CSCOR_V1_stndV1
fluency_V2x_imputed <- .77*all_Imputed$LFFCOR_V2_stndV1 + .78*all_Imputed$LFACOR_V2_stndV1 + .80*all_Imputed$LFSCOR_V2_stndV1 + .43*all_Imputed$CFANCOR_V2_stndV1 + .54*all_Imputed$CFBNCOR_V2_stndV1 + .36*all_Imputed$CSCOR_V2_stndV1
fluency_V3x_imputed <- .77*all_Imputed$LFFCOR_V3_stndV1 + .78*all_Imputed$LFACOR_V3_stndV1 + .80*all_Imputed$LFSCOR_V3_stndV1 + .43*all_Imputed$CFANCOR_V3_stndV1 + .54*all_Imputed$CFBNCOR_V3_stndV1 + .36*all_Imputed$CSCOR_V3_stndV1
fluency_V4x_imputed <- .77*all_Imputed$LFFCOR_V4_stndV1 + .78*all_Imputed$LFACOR_V4_stndV1 + .80*all_Imputed$LFSCOR_V4_stndV1 + .43*all_Imputed$CFANCOR_V4_stndV1 + .54*all_Imputed$CFBNCOR_V4_stndV1 + .36*all_Imputed$CSCOR_V4_stndV1
fluency_V1_imputed <- scale(fluency_V1x_imputed)
fluency_V2_imputed <- (fluency_V2x_imputed-mean(fluency_V1x_imputed,na.rm=T))/sd(fluency_V1x_imputed,na.rm=T)
fluency_V3_imputed <- (fluency_V3x_imputed-mean(fluency_V1x_imputed,na.rm=T))/sd(fluency_V1x_imputed,na.rm=T)
fluency_V4_imputed <- (fluency_V4x_imputed-mean(fluency_V1x_imputed,na.rm=T))/sd(fluency_V1x_imputed,na.rm=T)

semantic_flu_V1x_imputed <- .685*all_Imputed$AN1_sf + .623*all_Imputed$BN1_sf + .516*all_Imputed$CS1_sf
semantic_flu_V2x_imputed <- .685*all_Imputed$AN2_sf + .623*all_Imputed$BN2_sf + .516*all_Imputed$CS2_sf
semantic_flu_V3x_imputed <- .685*all_Imputed$AN3_sf + .623*all_Imputed$BN3_sf + .516*all_Imputed$CS3_sf
semantic_flu_V4x_imputed <- .685*all_Imputed$AN4_sf + .623*all_Imputed$BN4_sf + .516*all_Imputed$CS4_sf
semantic_flu_V1_imputed <- scale(semantic_flu_V1x_imputed)
semantic_flu_V2_imputed <- (semantic_flu_V2x_imputed-mean(semantic_flu_V1x_imputed,na.rm=T))/sd(semantic_flu_V1x_imputed,na.rm=T)
semantic_flu_V3_imputed <- (semantic_flu_V3x_imputed-mean(semantic_flu_V1x_imputed,na.rm=T))/sd(semantic_flu_V1x_imputed,na.rm=T)
semantic_flu_V4_imputed <- (semantic_flu_V4x_imputed-mean(semantic_flu_V1x_imputed,na.rm=T))/sd(semantic_flu_V1x_imputed,na.rm=T)

memory_V1x_imputed <- .91*.66*all_Imputed$LMITOT_V1_stndV1 + .94*.66*all_Imputed$LMDTOT_V1_stndV1 + .75*.60*all_Imputed$VRITOT_V1_stndV1 + .85*.60*all_Imputed$VRDTOT_V1_stndV1 + .18*all_Imputed$CVATOT_V1_stndV1 + .68*.72*all_Imputed$CVATOT_V1_stndV1 + .90*.72*all_Imputed$CVSDFR_V1_stndV1 + .91*.72*all_Imputed$CVLDFR_V1_stndV1
memory_V2x_imputed <- .91*.66*all_Imputed$LMITOT_V2_stndV1 + .94*.66*all_Imputed$LMDTOT_V2_stndV1 + .75*.60*all_Imputed$VRITOT_V2_stndV1 + .85*.60*all_Imputed$VRDTOT_V2_stndV1 + .18*all_Imputed$CVATOT_V2_stndV1 + .68*.72*all_Imputed$CVATOT_V2_stndV1 + .90*.72*all_Imputed$CVSDFR_V2_stndV1 + .91*.72*all_Imputed$CVLDFR_V2_stndV1
memory_V3x_imputed <- .91*.66*all_Imputed$LMITOT_V3_stndV1 + .94*.66*all_Imputed$LMDTOT_V3_stndV1 + .75*.60*all_Imputed$VRITOT_V3_stndV1 + .85*.60*all_Imputed$VRDTOT_V3_stndV1 + .18*all_Imputed$CVATOT_V3_stndV1 + .68*.72*all_Imputed$CVATOT_V3_stndV1 + .90*.72*all_Imputed$CVSDFR_V3_stndV1 + .91*.72*all_Imputed$CVLDFR_V3_stndV1
memory_V4x_imputed <- .91*.66*all_Imputed$LMITOT_V4_stndV1 + .94*.66*all_Imputed$LMDTOT_V4_stndV1 + .75*.60*all_Imputed$VRITOT_V4_stndV1 + .85*.60*all_Imputed$VRDTOT_V4_stndV1 + .18*all_Imputed$CVATOT_V4_stndV1 + .68*.72*all_Imputed$CVATOT_V4_stndV1 + .90*.72*all_Imputed$CVSDFR_V4_stndV1 + .91*.72*all_Imputed$CVLDFR_V4_stndV1
memory_V1_imputed <- scale(memory_V1x_imputed)
memory_V2_imputed <- (memory_V2x_imputed-mean(memory_V1x_imputed,na.rm=T))/sd(memory_V1x_imputed,na.rm=T)
memory_V3_imputed <- (memory_V3x_imputed-mean(memory_V1x_imputed,na.rm=T))/sd(memory_V1x_imputed,na.rm=T)
memory_V4_imputed <- (memory_V4x_imputed-mean(memory_V1x_imputed,na.rm=T))/sd(memory_V1x_imputed,na.rm=T)


speed_V1x_imputed <- (-1)*.66*.56*all_Imputed$Trail2_V1_stndV1 + (-1)*.84*.56*all_Imputed$Trail3_V1_stndV1 + .74*.82*all_Imputed$StroopW_V1_stndV1 + .83*.82*all_Imputed$StroopC_V1_stndV1 + (-1)*.76*.54*all_Imputed$SRTGMEANLOG_V1_stndV1 + (-1)*.86*.54*all_Imputed$CHRTGMEANLOG_V1_stndV1 
speed_V2x_imputed <- (-1)*.66*.56*all_Imputed$Trail2_V2_stndV1 + (-1)*.84*.56*all_Imputed$Trail3_V2_stndV1 + .74*.82*all_Imputed$StroopW_V2_stndV1 + .83*.82*all_Imputed$StroopC_V2_stndV1 + (-1)*.76*.54*all_Imputed$SRTGMEANLOG_V2_stndV1 + (-1)*.86*.54*all_Imputed$CHRTGMEANLOG_V2_stndV1 
speed_V3x_imputed <- (-1)*.66*.56*all_Imputed$Trail2_V3_stndV1 + (-1)*.84*.56*all_Imputed$Trail3_V3_stndV1 + .74*.82*all_Imputed$StroopW_V3_stndV1 + .83*.82*all_Imputed$StroopC_V3_stndV1 + (-1)*.76*.54*all_Imputed$SRTGMEANLOG_V3_stndV1 + (-1)*.86*.54*all_Imputed$CHRTGMEANLOG_V3_stndV1 
speed_V4x_imputed <- (-1)*.66*.56*all_Imputed$Trail2_V4_stndV1 + (-1)*.84*.56*all_Imputed$Trail3_V4_stndV1 + .74*.82*all_Imputed$StroopW_V4_stndV1 + .83*.82*all_Imputed$StroopC_V4_stndV1 + (-1)*.76*.54*all_Imputed$SRTGMEANLOG_V4_stndV1 + (-1)*.86*.54*all_Imputed$CHRTGMEANLOG_V4_stndV1 
speed_V1_imputed <- scale(speed_V1x_imputed)
speed_V2_imputed <- (speed_V2x_imputed-mean(speed_V1x_imputed,na.rm=T))/sd(speed_V1x_imputed,na.rm=T)
speed_V3_imputed <- (speed_V3x_imputed-mean(speed_V1x_imputed,na.rm=T))/sd(speed_V1x_imputed,na.rm=T)
speed_V4_imputed <- (speed_V4x_imputed-mean(speed_V1x_imputed,na.rm=T))/sd(speed_V1x_imputed,na.rm=T)

SPA_V1x_imputed <- 0.16715*all_Imputed$MR1COR_V1_stndV1 + 0.27901*all_Imputed$AFQTBXPCTTRAN_R_V1_stndV1 + 0.59952*all_Imputed$HFTOTCOR_V1_stndV1
SPA_V2x_imputed <- 0.16715*all_Imputed$MR1COR_V2_stndV1 + 0.27901*all_Imputed$AFQTBXPCTTRAN_R_V2_stndV1 + 0.59952*all_Imputed$HFTOTCOR_V2_stndV1
SPA_V3x_imputed <- 0.16715*all_Imputed$MR1COR_V3_stndV1 + 0.27901*all_Imputed$AFQTBXPCTTRAN_R_V3_stndV1 + 0.59952*all_Imputed$HFTOTCOR_V3_stndV1
SPA_V4x_imputed <- 0.16715*all_Imputed$MR1COR_V4_stndV1 + 0.27901*all_Imputed$AFQTBXPCTTRAN_R_V4_stndV1 + 0.59952*all_Imputed$HFTOTCOR_V4_stndV1
VisSpat_V1_imputed <- scale(SPA_V1x_imputed)
VisSpat_V2_imputed <- (SPA_V2x_imputed-mean(SPA_V1x_imputed,na.rm=T))/sd(SPA_V1x_imputed,na.rm=T)
VisSpat_V3_imputed <- (SPA_V3x_imputed-mean(SPA_V1x_imputed,na.rm=T))/sd(SPA_V1x_imputed,na.rm=T)
VisSpat_V4_imputed <- (SPA_V4x_imputed-mean(SPA_V1x_imputed,na.rm=T))/sd(SPA_V1x_imputed,na.rm=T)

VMEM_V1x_imputed <- 0.05350*all_Imputed$sVRCTOT_V1_stndV1 + 0.80128*all_Imputed$VRITOT_V1_stndV1 + 0.16498*all_Imputed$VRDTOT_V1_stndV1
VMEM_V2x_imputed <- 0.05350*all_Imputed$sVRCTOT_V2_stndV1 + 0.80128*all_Imputed$VRITOT_V2_stndV1 + 0.16498*all_Imputed$VRDTOT_V2_stndV1
VMEM_V3x_imputed <- 0.05350*all_Imputed$sVRCTOT_V3_stndV1 + 0.80128*all_Imputed$VRITOT_V3_stndV1 + 0.16498*all_Imputed$VRDTOT_V3_stndV1
VMEM_V4x_imputed <- 0.05350*all_Imputed$sVRCTOT_V4_stndV1 + 0.80128*all_Imputed$VRITOT_V4_stndV1 + 0.16498*all_Imputed$VRDTOT_V4_stndV1
VisMem_V1_imputed <- scale(VMEM_V1x_imputed)
VisMem_V2_imputed <- (VMEM_V2x_imputed-mean(VMEM_V1x_imputed,na.rm=T))/sd(VMEM_V1x_imputed,na.rm=T)
VisMem_V3_imputed <- (VMEM_V3x_imputed-mean(VMEM_V1x_imputed,na.rm=T))/sd(VMEM_V1x_imputed,na.rm=T)
VisMem_V4_imputed <- (VMEM_V4x_imputed-mean(VMEM_V1x_imputed,na.rm=T))/sd(VMEM_V1x_imputed,na.rm=T)


############################
### Export Scores to csv ###
############################

# ## non imputed
# allScores <- data.frame(VETSAID, #VETSAGRP,
#                    memory_V1, memory_V2, memory_V3,  memory_V4,
#                    speed_V1, speed_V2, speed_V3, speed_V4,
#                    commonEF_V1, commonEF_V2, commonEF_V3, commonEF_V4,
#                    work_mem_V1, work_mem_V2, work_mem_V3, work_mem_V4,
#                    fluency_V1, fluency_V2, fluency_V3,  fluency_V4,
#                    semantic_flu_V1, semantic_flu_V2, semantic_flu_V3, semantic_flu_V4,
#                    VisSpat_V1, VisSpat_V2, VisSpat_V3, VisSpat_V4,
#                    VisMem_V1,VisMem_V2,VisMem_V3, VisMem_V4)
# describe(allScores)
# write.csv(allScores, "data/V1V2V3V4_CogData_FactorScores_2024_10_21.csv", row.names = FALSE)
# #head(allScores)

## imputed
allScores_imp <- data.frame(VETSAID, #VETSAWAVE,
                        memory_V1_imputed, memory_V2_imputed, memory_V3_imputed, memory_V4_imputed,
                        speed_V1_imputed, speed_V2_imputed, speed_V3_imputed, speed_V4_imputed,
                        commonEF_V1_imputed, commonEF_V2_imputed, commonEF_V3_imputed, commonEF_V4_imputed,
                        work_mem_V1_imputed, work_mem_V2_imputed, work_mem_V3_imputed, work_mem_V4_imputed,
                        fluency_V1_imputed, fluency_V2_imputed, fluency_V3_imputed, fluency_V4_imputed,
                        semantic_flu_V1_imputed, semantic_flu_V2_imputed, semantic_flu_V3_imputed, semantic_flu_V4_imputed,
                        VisSpat_V1_imputed, VisSpat_V2_imputed, VisSpat_V3_imputed, VisSpat_V4_imputed,
                        VisMem_V1_imputed,VisMem_V2_imputed,VisMem_V3_imputed, VisMem_V4_imputed)
impnames <- c("VETSAID", #"VETSAWAVE",
              "memory_V1", "memory_V2", "memory_V3", "memory_V4",
              "speed_V1", "speed_V2", "speed_V3",  "speed_V4",
              "commonEF_V1", "commonEF_V2", "commonEF_V3", "commonEF_V4",
              "work_mem_V1", "work_mem_V2", "work_mem_V3", "work_mem_V4",
              "fluency_V1", "fluency_V2", "fluency_V3", "fluency_V4",
              "semantic_flu_V1", "semantic_flu_V2", "semantic_flu_V3",  "semantic_flu_V4",
              "vis_spat_V1", "vis_spat_V2", "vis_spat_V3", "vis_spat_V4",
              "vis_mem_V1","vis_mem_V2","vis_mem_V3", "vis_mem_V4")
names(allScores_imp) <- impnames
describe(allScores_imp)
# describe(allScores)
### Exclude final WM and SS IDs that were missing too many subdomain tasks in full imputation (for EF/fluency)
# allScores_imp$WorkMem_V1[allScores_imp$VETSAID %in% WM1_exclude_IDs]<- NA
# allScores_imp$WorkMem_V2[allScores_imp$VETSAID %in% WM2_exclude_IDs]<- NA
# allScores_imp$WorkMem_V3[allScores_imp$VETSAID %in% WM3_exclude_IDs]<- NA
# allScores_imp$Semantic_Flu_V1[allScores_imp$VETSAID %in% SS1_exclude_IDs]<- NA
# allScores_imp$Semantic_Flu_V2[allScores_imp$VETSAID %in% SS2_exclude_IDs]<- NA
# allScores_imp$Semantic_Flu_V3[allScores_imp$VETSAID %in% SS3_exclude_IDs]<- NA
#describe(allScores_imp)

write.csv(allScores_imp, outfile, row.names = FALSE)

