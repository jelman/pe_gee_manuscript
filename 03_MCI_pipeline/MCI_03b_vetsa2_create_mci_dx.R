rm(list = ls())
library(dplyr)
library(psych)
library(readr)
library(haven)
library(mice) #loading mice library

#--------------------------------------------------------------#
# Overview: VETSA2 MCI Diagnosis Pipeline                      #
#                                                              #
# This script processes cognitive test data from VETSA Wave 2  #
# to create Mild Cognitive Impairment (MCI) diagnoses.         #
#                                                              #
# Processing steps:                                            #
# 1. Impute missing cognitive test scores                      #
# 2. Create composite scores for multi-test domains            #
# 3. Normalize all scores to comparable scales                 #
# 4. Calculate domain-specific impairment indicators           #
# 5. Generate MCI classifications using multiple definitions   #
#                                                              #
# Key outputs:                                                 #
# - Imputed cognitive scores                                   #
# - Domain composite scores                                    #
# - Domain impairment indicators                               #
# - MCI classifications (typical, comprehensive, conservative) #
#                                                              #
# Notes for VETSA2:                                            #
# - Includes Spatial Span but no Boston Naming Test            #
# - Missing CSSACCSC (Card Sort Accuracy) in Executive tests   #
# - All scores adjusted for Age 20 AFQT                        #
#--------------------------------------------------------------#

# -----------------------------------------------------------------------
# Read In Raw Data
# -----------------------------------------------------------------------

# Set working directory
setwd("~/netshare/M/Projects/PracEffects_GEE")

# Read in normed data
V2 <- read_csv("data/intermediate_data/MCI_02b01_vetsa2_MCI_PreImputation.csv")

# Checking Summaries
summary(V2)
dim(V2)
names(V2)

# Convert all variable names to uppercase
names(V2) <- toupper(names(V2))

# Define names of MISSING counters
missing_names <- c("TOTALMISSING", "MEMORYMISSING", "EXECMISSING", "ATTNMISSING", "VERBALMISSING", "VISUALMISSING", "PROCMISSING")

# Define names of normed scores to be imputed for wave 2
norm_scores <- c("MTXT_V2", "CVATOTSS_V2", "CVLDFSS_V2", "LM1ASS_V2", "LM1BSS_V2", "LM2ASS_V2", "LM2BSS_V2", 
                 "DSPSS_V2", "LNSC_V2", "TRL1TSC_V2", "TRL2TSC_V2", "TRL3TSC_V2", "TRL4TSC_V2", "TRL5TSC_V2", 
                 "STRWT_V2", "STRCT_V2", "STRIT_V2", "HFTOTCOR_V2", "MR1COR_V2",
                 "LFCORSC_V2", "CFCORSC_V2", "VRITOTSS_V2", "VRDTOTSS_V2", "VRCTOTSS_V2", "SSPSS_V2")

# -----------------------------------------------------------------------
# Imputing Missing Data Scores 
# -----------------------------------------------------------------------

# Select only the norm_scores variables for imputation from the full V2 dataframe
preimp_data <- V2[, norm_scores]

set.seed(547)
imp <- mice(preimp_data, m=5)
imp_long <- complete(imp, "long")

# Take the average across imputations for each variable
means_df <- imp_long %>%
  group_by(.id) %>%
  summarise(across(all_of(norm_scores), ~mean(.x, na.rm=TRUE))) %>%
  ungroup() %>%
  arrange(as.integer(.id)) %>%
  select(-.id)

# Bind VETSAID and the MISSING counters with averaged imputed data
V2_Imp <- cbind(V2[, c("VETSAID", missing_names)], means_df)

# Standardizing Mental Rotation and Hidden Figure Scores

V2_Imp$MR1CORZ_V2 <- scale(V2_Imp$MR1COR_V2)
V2_Imp$HFTOTCORZ_V2 <- scale(V2_Imp$HFTOTCOR_V2)

# Checking Summaries post imputation, there should be no missing data

summary(V2_Imp)
names(V2_Imp)

write.csv(V2_Imp, "data/intermediate_data/MCI_03b03_vetsa2_MCI_Composites.csv", row.names=FALSE)

# -----------------------------------------------------------------------
# Creating Composite Scores for Multiple Tests within Domain
# -----------------------------------------------------------------------

# Scaling CVATSS before averaging with CVLDFSS (which is already standardized to mean = 0, sd = 1)

V2_Imp$CVATOTSS_V2 <- scale(V2_Imp$CVATOTSS_V2, center=50, scale=10)

# Episodic Memory
CVLTtests <- c("CVATOTSS_V2","CVLDFSS_V2")
LMtests <- c("LM1ASS_V2","LM1BSS_V2","LM2ASS_V2","LM2BSS_V2")
VRtests <- c("VRITOTSS_V2","VRDTOTSS_V2")

# Processing Speed
TRLtests <- c("TRL2TSC_V2","TRL3TSC_V2")
STRtests <- c("STRWT_V2","STRCT_V2")

V2_Imp$CVLT <- apply(V2_Imp[,CVLTtests], 1, mean)
V2_Imp$LM <-   apply(V2_Imp[,LMtests], 1, mean)
V2_Imp$VR <-   apply(V2_Imp[,VRtests], 1, mean)
V2_Imp$TRL <-  apply(V2_Imp[,TRLtests], 1, mean)
V2_Imp$STR <-  apply(V2_Imp[,STRtests], 1, mean)

summary(V2_Imp)

names(V2_Imp)

# -----------------------------------------------------------------------
# Creating Normalized Test Scores, Based on Published Norms...
# -----------------------------------------------------------------------

# Scaled Scores, mean = 10, sd = 3
SStests <- c("DSPSS_V2", "LNSC_V2", "VRITOTSS_V2", "VRDTOTSS_V2", 
             "VRCTOTSS_V2", "TRL1TSC_V2", "TRL2TSC_V2", "TRL3TSC_V2", "TRL4TSC_V2",
             "LFCORSC_V2", "CFCORSC_V2", "LM1ASS_V2", "LM1BSS_V2",
             "LM2ASS_V2", "LM2BSS_V2", "SSPSS_V2")

# T-Scores, mean = 50, sd = 10
TStests <- c("STRWT_V2", "STRCT_V2", "STRIT_V2", "MTXT_V2")

# Z-Scores, mean = 0, sd = 1
ZStests <- c("MR1CORZ_V2", "HFTOTCORZ_V2")

# Normalizing based on means and sds above. No need to normalize those that are already Z-scores

for(i in seq_along(SStests)){V2_Imp[,SStests[i]] <- scale(V2_Imp[,SStests[i]],center=10,scale=3) }
for(i in seq_along(TStests)){V2_Imp[,TStests[i]] <- scale(V2_Imp[,TStests[i]],center=50,scale=10) }

# Normalizing composite test scores based on sd/sqrt(number of tests)

V2_Imp$CVLT <- scale(V2_Imp$CVLT, center=0, scale= 1/sqrt(2))
V2_Imp$LM <- scale(V2_Imp$LM, center=10, scale= 3/sqrt(4))
V2_Imp$VR <- scale(V2_Imp$VR, center=10, scale= 3/sqrt(2))
V2_Imp$TRL <- scale(V2_Imp$TRL, center=10, scale= 3/sqrt(2))
V2_Imp$STR <- scale(V2_Imp$STR, center=50, scale= 10/sqrt(2))

# Checking Output

summary(V2_Imp)

write.csv(V2_Imp, "MCI_04b3_V2_Imp2.csv")

# -----------------------------------------------------------------------
# CREATING MCI MEASURES 
# -----------------------------------------------------------------------

# Note: All scores already adjusted for age 20 AFQT
# Wave 2 has Spatial Span but no Boston Naming Test
V2MCI <- V2_Imp

# Define domain tests - SSPSS_V2 in ATTNtests, No BNTSS in Wave 2
MEMtests <- c("CVLT", "LM", "VR")  # 3 composites from 8 tests
EXECtests <- c("TRL4TSC_V2", "STRIT_V2", "MTXT_V2") # 3 tests - No CSSACCSC_V2
ATTNtests <- c("DSPSS_V2", "LNSC_V2", "TRL1TSC_V2", "SSPSS_V2") # 4 tests
LANGtests <- c("LFCORSC_V2", "CFCORSC_V2") # 2 tests
VStests <- c("HFTOTCORZ_V2", "MR1CORZ_V2", "VRCTOTSS_V2") # 3 tests
PStests <- c("TRL", "STR") # 2 composites from 4 tests

tests <- c(MEMtests,EXECtests,ATTNtests,LANGtests,VStests,PStests)

# -----------------------------------------------------------------------
# Defining Composite Domain Score for VETSA Standarized Measures
# -----------------------------------------------------------------------

# Summing Scores
V2MCI$MEM <- apply(V2MCI[,MEMtests], 1, mean)
V2MCI$EXEC <- apply(V2MCI[,EXECtests], 1, mean)
V2MCI$ATTN <- apply(V2MCI[,ATTNtests], 1, mean)
V2MCI$LANG <- apply(V2MCI[,LANGtests], 1, mean)
V2MCI$VS <- apply(V2MCI[,VStests], 1, mean)
V2MCI$PS <- apply(V2MCI[,PStests], 1, mean)

# Scaling Based on Number of Tests per Domain
V2MCI$MEM <- scale(V2MCI$MEM, center=0, scale=1/sqrt(length(MEMtests)))
V2MCI$EXEC <- scale(V2MCI$EXEC, center=0, scale=1/sqrt(length(EXECtests)))
V2MCI$ATTN <- scale(V2MCI$ATTN, center=0, scale=1/sqrt(length(ATTNtests)))
V2MCI$LANG <- scale(V2MCI$LANG, center=0, scale=1/sqrt(length(LANGtests)))
V2MCI$VS <- scale(V2MCI$VS, center=0, scale=1/sqrt(length(VStests)))
V2MCI$PS <- scale(V2MCI$PS, center=0, scale=1/sqrt(length(PStests)))

# -----------------------------------------------------------------------
# Creating Domain-Based MCI Classification
# -----------------------------------------------------------------------

# Define cutoff-based impairment indicators
V2data <- data.frame(matrix(ncol=0, nrow=nrow(V2MCI)))

# 1 SD Cutoff (16%)
for(j in seq_along(tests)){
    V2data[paste("imp1", tests[j], sep="")] <- as.numeric(V2MCI[,tests[j]] <= (-1))
}

# 1.5 SD Cutoff (6.7%)
for(j in seq_along(tests)){
    V2data[paste("imp2", tests[j], sep="")] <- as.numeric(V2MCI[,tests[j]] <= (-1.5))
}

# Define domain impairment counts
V2data$nMEM1 <- rowSums(V2data[,paste("imp1", MEMtests, sep="")])
V2data$nEXEC1 <- rowSums(V2data[,paste("imp1", EXECtests, sep="")])
V2data$nATTN1 <- rowSums(V2data[,paste("imp1", ATTNtests, sep="")])
V2data$nLANG1 <- rowSums(V2data[,paste("imp1", LANGtests, sep="")])
V2data$nVS1 <- rowSums(V2data[,paste("imp1", VStests, sep="")])
V2data$nPS1 <- rowSums(V2data[,paste("imp1", PStests, sep="")])

V2data$nMEM2 <- rowSums(V2data[,paste("imp2", MEMtests, sep="")])
V2data$nEXEC2 <- rowSums(V2data[,paste("imp2", EXECtests, sep="")])
V2data$nATTN2 <- rowSums(V2data[,paste("imp2", ATTNtests, sep="")])
V2data$nLANG2 <- rowSums(V2data[,paste("imp2", LANGtests, sep="")])
V2data$nVS2 <- rowSums(V2data[,paste("imp2", VStests, sep="")])
V2data$nPS2 <- rowSums(V2data[,paste("imp2", PStests, sep="")])

# Define domain impairment indicators using different criteria
# Typical - 1 test, 1.5 sd
V2data$impMEM_typ <- as.numeric(V2data$nMEM2 >= 1)
V2data$impEXEC_typ <- as.numeric(V2data$nEXEC2 >= 1)
V2data$impATTN_typ <- as.numeric(V2data$nATTN2 >= 1)
V2data$impLANG_typ <- as.numeric(V2data$nLANG2 >= 1)
V2data$impVS_typ <- as.numeric(V2data$nVS2 >= 1)
V2data$impPS_typ <- as.numeric(V2data$nPS2 >= 1)

# Comprehensive - 2 tests, 1 sd
V2data$impMEM_comp <- as.numeric(V2data$nMEM1 >= 2)
V2data$impEXEC_comp <- as.numeric(V2data$nEXEC1 >= 2)
V2data$impATTN_comp <- as.numeric(V2data$nATTN1 >= 2)
V2data$impLANG_comp <- as.numeric(V2data$nLANG1 >= 2)
V2data$impVS_comp <- as.numeric(V2data$nVS1 >= 2)
V2data$impPS_comp <- as.numeric(V2data$nPS1 >= 2)

# Conservative - 2 tests, 1.5 sd
V2data$impMEM_cons <- as.numeric(V2data$nMEM2 >= 2)
V2data$impEXEC_cons <- as.numeric(V2data$nEXEC2 >= 2)
V2data$impATTN_cons <- as.numeric(V2data$nATTN2 >= 2)
V2data$impLANG_cons <- as.numeric(V2data$nLANG2 >= 2)
V2data$impVS_cons <- as.numeric(V2data$nVS2 >= 2)
V2data$impPS_cons <- as.numeric(V2data$nPS2 >= 2)

# Quantile-based impairment (5th percentile)
V2data$impMEM_comp5 <- as.numeric(V2MCI$MEM <= quantile(V2MCI$MEM, 0.05, na.rm=TRUE))
V2data$impEXEC_comp5 <- as.numeric(V2MCI$EXEC <= quantile(V2MCI$EXEC, 0.05, na.rm=TRUE))
V2data$impATTN_comp5 <- as.numeric(V2MCI$ATTN <= quantile(V2MCI$ATTN, 0.05, na.rm=TRUE))
V2data$impLANG_comp5 <- as.numeric(V2MCI$LANG <= quantile(V2MCI$LANG, 0.05, na.rm=TRUE))
V2data$impVS_comp5 <- as.numeric(V2MCI$VS <= quantile(V2MCI$VS, 0.05, na.rm=TRUE))
V2data$impPS_comp5 <- as.numeric(V2MCI$PS <= quantile(V2MCI$PS, 0.05, na.rm=TRUE))

# Quantile-based impairment (2.5th percentile)
V2data$impMEM_comp25 <- as.numeric(V2MCI$MEM <= quantile(V2MCI$MEM, 0.025, na.rm=TRUE))
V2data$impEXEC_comp25 <- as.numeric(V2MCI$EXEC <= quantile(V2MCI$EXEC, 0.025, na.rm=TRUE))
V2data$impATTN_comp25 <- as.numeric(V2MCI$ATTN <= quantile(V2MCI$ATTN, 0.025, na.rm=TRUE))
V2data$impLANG_comp25 <- as.numeric(V2MCI$LANG <= quantile(V2MCI$LANG, 0.025, na.rm=TRUE))
V2data$impVS_comp25 <- as.numeric(V2MCI$VS <= quantile(V2MCI$VS, 0.025, na.rm=TRUE))
V2data$impPS_comp25 <- as.numeric(V2MCI$PS <= quantile(V2MCI$PS, 0.025, na.rm=TRUE))

# Create nonMEM impairment indicators
nonmemdoms <- c("EXEC", "ATTN", "LANG", "VS", "PS")
doms_typ <- paste(paste("imp", nonmemdoms, sep=""), "typ", sep="_")
doms_comp <- paste(paste("imp", nonmemdoms, sep=""), "comp", sep="_")
doms_cons <- paste(paste("imp", nonmemdoms, sep=""), "cons", sep="_")
doms_comp5 <- paste(paste("imp", nonmemdoms, sep=""), "comp5", sep="_")
doms_comp25 <- paste(paste("imp", nonmemdoms, sep=""), "comp25", sep="_")

V2data$impNONMEM_typ <- rowSums(V2data[,doms_typ])
V2data$impNONMEM_comp <- rowSums(V2data[,doms_comp])
V2data$impNONMEM_cons <- rowSums(V2data[,doms_cons])
V2data$impNONMEM_comp5 <- rowSums(V2data[,doms_comp5])
V2data$impNONMEM_comp25 <- rowSums(V2data[,doms_comp25])

# Create MCI classifications
# 0 = no impairments
# 1 = Single-Domain Non-Amnestic
# 2 = Single-Domain Amnestic
# 3 = Multi-Domain Non-Amnestic
# 4 = Multi-Domain Amnestic

V2data$rMCI_typ_V2 <- with(V2data, (0*(impMEM_typ==0 & impNONMEM_typ==0) +
                           1*(impMEM_typ==0 & impNONMEM_typ==1) +
                           2*(impMEM_typ==1 & impNONMEM_typ==0) +
                           3*(impMEM_typ==0 & impNONMEM_typ>1) +
                           4*(impMEM_typ==1 & impNONMEM_typ>=1)))

V2data$rMCI_comp_V2 <- with(V2data, (0*(impMEM_comp==0 & impNONMEM_comp==0) +
                            1*(impMEM_comp==0 & impNONMEM_comp==1) +
                            2*(impMEM_comp==1 & impNONMEM_comp==0) +
                            3*(impMEM_comp==0 & impNONMEM_comp>1) +
                            4*(impMEM_comp==1 & impNONMEM_comp>=1)))

V2data$rMCI_cons_V2 <- with(V2data, (0*(impMEM_cons==0 & impNONMEM_cons==0) +
                            1*(impMEM_cons==0 & impNONMEM_cons==1) +
                            2*(impMEM_cons==1 & impNONMEM_cons==0) +
                            3*(impMEM_cons==0 & impNONMEM_cons>1) +
                            4*(impMEM_cons==1 & impNONMEM_cons>=1)))

V2data$rMCI_p5_V2 <- with(V2data, (0*(impMEM_comp5==0 & impNONMEM_comp5==0) +
                          1*(impMEM_comp5==0 & impNONMEM_comp5==1) +
                          2*(impMEM_comp5==1 & impNONMEM_comp5==0) +
                          3*(impMEM_comp5==0 & impNONMEM_comp5>1) +
                          4*(impMEM_comp5==1 & impNONMEM_comp5>=1)))

V2data$rMCI_p25_V2 <- with(V2data, (0*(impMEM_comp25==0 & impNONMEM_comp25==0) +
                           1*(impMEM_comp25==0 & impNONMEM_comp25==1) +
                           2*(impMEM_comp25==1 & impNONMEM_comp25==0) +
                           3*(impMEM_comp25==0 & impNONMEM_comp25>1) +
                           4*(impMEM_comp25==1 & impNONMEM_comp25>=1)))

# Combine all data
V2_final <- cbind(V2MCI, V2data)

# Save final MCI measures file
write.csv(V2_final, "data/intermediate_data/MCI_03b04_vetsa2_MCI_AllData.csv", row.names=FALSE)
summary(V2_final)