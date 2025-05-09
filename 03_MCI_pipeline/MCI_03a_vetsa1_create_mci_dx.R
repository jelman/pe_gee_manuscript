rm(list = ls())
library(dplyr)
library(psych)
library(readr)
library(haven)
library(mice) #loading mice library

#--------------------------------------------------------------#
# Overview: VETSA1 MCI Diagnosis Pipeline                      #
#                                                              #
# This script processes cognitive test data from VETSA Wave 1  #
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
# Notes for VETSA1:                                            #
# - Includes Spatial Span but no Boston Naming Test            #
# - All scores adjusted for Age 20 AFQT                        #
#--------------------------------------------------------------#

# -----------------------------------------------------------------------
# Read In Raw Data
# -----------------------------------------------------------------------

# Set working directory
setwd("~/netshare/M/Projects/PracEffects_GEE")

# Read in normed data
V1 <- read_csv("data/intermediate_data/MCI_02a01_vetsa1_MCI_PreImputation.csv")

# Checking Summaries
summary(V1)
dim(V1)
names(V1)

# Convert all variable names to uppercase
names(V1) <- toupper(names(V1))

# Define names of MISSING counters
missing_names <- c("TOTALMISSING", "MEMORYMISSING", "EXECMISSING", "ATTNMISSING", "VERBALMISSING", "VISUALMISSING", "PROCMISSING")

# Define names of normed scores to be imputed for wave 1
norm_scores <- c("MTXT_V1", "CVATOTSS_V1", "CVLDFSS_V1", "LM1ASS_V1", "LM1BSS_V1", "LM2ASS_V1", "LM2BSS_V1", 
                 "DSPSS_V1", "LNSC_V1", "TRL1TSC_V1", "TRL2TSC_V1", "TRL3TSC_V1", "TRL4TSC_V1", "TRL5TSC_V1", 
                 "STRWT_V1", "STRCT_V1", "STRIT_V1", "HFTOTCOR_V1", "MR1COR_V1",
                 "LFCORSC_V1", "CFCORSC_V1", "CSCORSC_V1", "CSSACCSC_V1", "VRITOTSS_V1", "VRDTOTSS_V1", "VRCTOTSS_V1", "SSPSS_V1")

# -----------------------------------------------------------------------
# Imputing Missing Data Scores 
# -----------------------------------------------------------------------

# Select only the norm_scores variables for imputation from the full V1 dataframe
preimp_data <- V1[, norm_scores]

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
V1_Imp <- cbind(V1[, c("VETSAID", missing_names)], means_df)

# Standardizing Mental Rotation and Hidden Figure Scores
V1_Imp$MR1CORZ_V1 <- scale(V1_Imp$MR1COR_V1)
V1_Imp$HFTOTCORZ_V1 <- scale(V1_Imp$HFTOTCOR_V1)

# Checking Summaries post imputation, there should be no missing data
summary(V1_Imp)
names(V1_Imp)

write.csv(V1_Imp, "data/intermediate_data/MCI_03a02_vetsa1_MCI_PostImputation.csv", row.names=FALSE)

# -----------------------------------------------------------------------
# Creating Composite Scores for Multiple Tests within Domain
# -----------------------------------------------------------------------

# Scaling CVATSS before averaging with CVLDFSS (which is already standardized to mean = 0, sd = 1)
V1_Imp$CVATOTSS_V1 <- scale(V1_Imp$CVATOTSS_V1, center=50, scale=10)

# Episodic Memory
CVLTtests <- c("CVATOTSS_V1","CVLDFSS_V1")
LMtests <- c("LM1ASS_V1","LM1BSS_V1","LM2ASS_V1","LM2BSS_V1")
VRtests <- c("VRITOTSS_V1","VRDTOTSS_V1")

# Processing Speed
TRLtests <- c("TRL2TSC_V1","TRL3TSC_V1")
STRtests <- c("STRWT_V1","STRCT_V1")

V1_Imp$CVLT <- apply(V1_Imp[,CVLTtests], 1, mean)
V1_Imp$LM <-   apply(V1_Imp[,LMtests], 1, mean)
V1_Imp$VR <-   apply(V1_Imp[,VRtests], 1, mean)
V1_Imp$TRL <-  apply(V1_Imp[,TRLtests], 1, mean)
V1_Imp$STR <-  apply(V1_Imp[,STRtests], 1, mean)

summary(V1_Imp)
names(V1_Imp)

# -----------------------------------------------------------------------
# Creating Normalized Test Scores, Based on Published Norms...
# -----------------------------------------------------------------------

# Scaled Scores, mean = 10, sd = 3
SStests <- c("DSPSS_V1", "LNSC_V1", "VRITOTSS_V1", "VRDTOTSS_V1", 
             "VRCTOTSS_V1", "TRL1TSC_V1", "TRL2TSC_V1", "TRL3TSC_V1", "TRL4TSC_V1",
             "LFCORSC_V1", "CFCORSC_V1", "CSSACCSC_V1", "LM1ASS_V1", "LM1BSS_V1",
             "LM2ASS_V1", "LM2BSS_V1", "SSPSS_V1")

# T-Scores, mean = 50, sd = 10 , CVATSS already scaled previously
TStests <- c("STRWT_V1", "STRCT_V1", "STRIT_V1", "MTXT_V1")

# Z-Scores, mean = 0, sd = 1 
ZStests <- c("MR1CORZ_V1", "HFTOTCORZ_V1")

# Normalizing based on means and sds above. No need to normalize those that are already Z-scores
for(i in seq_along(SStests)){V1_Imp[,SStests[i]] <- scale(V1_Imp[,SStests[i]],center=10,scale=3) }
for(i in seq_along(TStests)){V1_Imp[,TStests[i]] <- scale(V1_Imp[,TStests[i]],center=50,scale=10) }

# Normalizing composite test scores based on sd/sqrt(number of tests)
V1_Imp$CVLT <- scale(V1_Imp$CVLT, center=0, scale= 1/sqrt(2))
V1_Imp$LM <- scale(V1_Imp$LM, center=10, scale= 3/sqrt(4))
V1_Imp$VR <- scale(V1_Imp$VR, center=10, scale= 3/sqrt(2))
V1_Imp$TRL <- scale(V1_Imp$TRL, center=10, scale= 3/sqrt(2))
V1_Imp$STR <- scale(V1_Imp$STR, center=50, scale= 10/sqrt(2))

# Checking Output
summary(V1_Imp)
write.csv(V1_Imp, "data/intermediate_data/MCI_03a03_vetsa1_MCI_Composites.csv", row.names=FALSE)

# -----------------------------------------------------------------------
# CREATING MCI MEASURES 
# -----------------------------------------------------------------------

# Note: All scores already adjusted for age 20 AFQT
# Note: Wave 1 has Boston Naming Test but no Spatial Span
V1MCI <- V1_Imp

# Define domain tests - NO SSPSS_V1 in ATTNtests, LANGtests INCLUDES BNTSS in Wave 1
MEMtests <- c("CVLT", "LM", "VR")  # 3 composites from 8 tests
EXECtests <- c("TRL4TSC_V1", "CSSACCSC_V1", "STRIT_V1", "MTXT_V1") # 4 tests
ATTNtests <- c("DSPSS_V1", "LNSC_V1", "TRL1TSC_V1", "SSPSS_V1") # 3 tests
LANGtests <- c("LFCORSC_V1", "CFCORSC_V1") # 2 tests - BNTSS not available in dataset
VStests <- c("HFTOTCORZ_V1", "MR1CORZ_V1", "VRCTOTSS_V1") # 3 tests
PStests <- c("TRL", "STR") # 2 composites from 4 tests

tests <- c(MEMtests, EXECtests, ATTNtests, LANGtests, VStests, PStests)

# -----------------------------------------------------------------------
# Defining Composite Domain Score for VETSA Standarized Measures
# -----------------------------------------------------------------------

# Summing Scores
V1MCI$MEM <- apply(V1MCI[,MEMtests], 1, mean)
V1MCI$EXEC <- apply(V1MCI[,EXECtests], 1, mean)
V1MCI$ATTN <- apply(V1MCI[,ATTNtests], 1, mean)
V1MCI$LANG <- apply(V1MCI[,LANGtests], 1, mean)
V1MCI$VS <- apply(V1MCI[,VStests], 1, mean)
V1MCI$PS <- apply(V1MCI[,PStests], 1, mean)

# Scaling Based on Number of Tests per Domain
V1MCI$MEM <- scale(V1MCI$MEM, center=0, scale=1/sqrt(length(MEMtests)))
V1MCI$EXEC <- scale(V1MCI$EXEC, center=0, scale=1/sqrt(length(EXECtests)))
V1MCI$ATTN <- scale(V1MCI$ATTN, center=0, scale=1/sqrt(length(ATTNtests)))
V1MCI$LANG <- scale(V1MCI$LANG, center=0, scale=1/sqrt(length(LANGtests)))
V1MCI$VS <- scale(V1MCI$VS, center=0, scale=1/sqrt(length(VStests)))
V1MCI$PS <- scale(V1MCI$PS, center=0, scale=1/sqrt(length(PStests)))

# -----------------------------------------------------------------------
# Creating Domain-Based MCI Classification
# -----------------------------------------------------------------------

# Define cutoff-based impairment indicators
V1data <- data.frame(matrix(ncol=0, nrow=nrow(V1MCI)))

# 1 SD Cutoff (16%)
for(j in seq_along(tests)){
    V1data[paste("imp1", tests[j], sep="")] <- as.numeric(V1MCI[,tests[j]] <= (-1))
}

# 1.5 SD Cutoff (6.7%)
for(j in seq_along(tests)){
    V1data[paste("imp2", tests[j], sep="")] <- as.numeric(V1MCI[,tests[j]] <= (-1.5))
}

# Define domain impairment counts
V1data$nMEM1 <- rowSums(V1data[,paste("imp1", MEMtests, sep="")])
V1data$nEXEC1 <- rowSums(V1data[,paste("imp1", EXECtests, sep="")])
V1data$nATTN1 <- rowSums(V1data[,paste("imp1", ATTNtests, sep="")])
V1data$nLANG1 <- rowSums(V1data[,paste("imp1", LANGtests, sep="")])
V1data$nVS1 <- rowSums(V1data[,paste("imp1", VStests, sep="")])
V1data$nPS1 <- rowSums(V1data[,paste("imp1", PStests, sep="")])

V1data$nMEM2 <- rowSums(V1data[,paste("imp2", MEMtests, sep="")])
V1data$nEXEC2 <- rowSums(V1data[,paste("imp2", EXECtests, sep="")])
V1data$nATTN2 <- rowSums(V1data[,paste("imp2", ATTNtests, sep="")])
V1data$nLANG2 <- rowSums(V1data[,paste("imp2", LANGtests, sep="")])
V1data$nVS2 <- rowSums(V1data[,paste("imp2", VStests, sep="")])
V1data$nPS2 <- rowSums(V1data[,paste("imp2", PStests, sep="")])

# Define domain impairment indicators using different criteria
# Typical - 1 test, 1.5 sd
V1data$impMEM_typ <- as.numeric(V1data$nMEM2 >= 1)
V1data$impEXEC_typ <- as.numeric(V1data$nEXEC2 >= 1)
V1data$impATTN_typ <- as.numeric(V1data$nATTN2 >= 1)
V1data$impLANG_typ <- as.numeric(V1data$nLANG2 >= 1)
V1data$impVS_typ <- as.numeric(V1data$nVS2 >= 1)
V1data$impPS_typ <- as.numeric(V1data$nPS2 >= 1)

# Comprehensive - 2 tests, 1 sd
V1data$impMEM_comp <- as.numeric(V1data$nMEM1 >= 2)
V1data$impEXEC_comp <- as.numeric(V1data$nEXEC1 >= 2)
V1data$impATTN_comp <- as.numeric(V1data$nATTN1 >= 2)
V1data$impLANG_comp <- as.numeric(V1data$nLANG1 >= 2)
V1data$impVS_comp <- as.numeric(V1data$nVS1 >= 2)
V1data$impPS_comp <- as.numeric(V1data$nPS1 >= 2)

# Conservative - 2 tests, 1.5 sd
V1data$impMEM_cons <- as.numeric(V1data$nMEM2 >= 2)
V1data$impEXEC_cons <- as.numeric(V1data$nEXEC2 >= 2)
V1data$impATTN_cons <- as.numeric(V1data$nATTN2 >= 2)
V1data$impLANG_cons <- as.numeric(V1data$nLANG2 >= 2)
V1data$impVS_cons <- as.numeric(V1data$nVS2 >= 2)
V1data$impPS_cons <- as.numeric(V1data$nPS2 >= 2)

# Quantile-based impairment (5th percentile)
V1data$impMEM_comp5 <- as.numeric(V1MCI$MEM <= quantile(V1MCI$MEM, 0.05, na.rm=TRUE))
V1data$impEXEC_comp5 <- as.numeric(V1MCI$EXEC <= quantile(V1MCI$EXEC, 0.05, na.rm=TRUE))
V1data$impATTN_comp5 <- as.numeric(V1MCI$ATTN <= quantile(V1MCI$ATTN, 0.05, na.rm=TRUE))
V1data$impLANG_comp5 <- as.numeric(V1MCI$LANG <= quantile(V1MCI$LANG, 0.05, na.rm=TRUE))
V1data$impVS_comp5 <- as.numeric(V1MCI$VS <= quantile(V1MCI$VS, 0.05, na.rm=TRUE))
V1data$impPS_comp5 <- as.numeric(V1MCI$PS <= quantile(V1MCI$PS, 0.05, na.rm=TRUE))

# Quantile-based impairment (2.5th percentile)
V1data$impMEM_comp25 <- as.numeric(V1MCI$MEM <= quantile(V1MCI$MEM, 0.025, na.rm=TRUE))
V1data$impEXEC_comp25 <- as.numeric(V1MCI$EXEC <= quantile(V1MCI$EXEC, 0.025, na.rm=TRUE))
V1data$impATTN_comp25 <- as.numeric(V1MCI$ATTN <= quantile(V1MCI$ATTN, 0.025, na.rm=TRUE))
V1data$impLANG_comp25 <- as.numeric(V1MCI$LANG <= quantile(V1MCI$LANG, 0.025, na.rm=TRUE))
V1data$impVS_comp25 <- as.numeric(V1MCI$VS <= quantile(V1MCI$VS, 0.025, na.rm=TRUE))
V1data$impPS_comp25 <- as.numeric(V1MCI$PS <= quantile(V1MCI$PS, 0.025, na.rm=TRUE))

# Create nonMEM impairment indicators
nonmemdoms <- c("EXEC", "ATTN", "LANG", "VS", "PS")
doms_typ <- paste(paste("imp", nonmemdoms, sep=""), "typ", sep="_")
doms_comp <- paste(paste("imp", nonmemdoms, sep=""), "comp", sep="_")
doms_cons <- paste(paste("imp", nonmemdoms, sep=""), "cons", sep="_")
doms_comp5 <- paste(paste("imp", nonmemdoms, sep=""), "comp5", sep="_")
doms_comp25 <- paste(paste("imp", nonmemdoms, sep=""), "comp25", sep="_")

V1data$impNONMEM_typ <- rowSums(V1data[,doms_typ])
V1data$impNONMEM_comp <- rowSums(V1data[,doms_comp])
V1data$impNONMEM_cons <- rowSums(V1data[,doms_cons])
V1data$impNONMEM_comp5 <- rowSums(V1data[,doms_comp5])
V1data$impNONMEM_comp25 <- rowSums(V1data[,doms_comp25])

# Create MCI classifications
# 0 = no impairments
# 1 = Single-Domain Non-Amnestic
# 2 = Single-Domain Amnestic
# 3 = Multi-Domain Non-Amnestic
# 4 = Multi-Domain Amnestic

V1data$rMCI_typ_V1 <- with(V1data, (0*(impMEM_typ==0 & impNONMEM_typ==0) +
                           1*(impMEM_typ==0 & impNONMEM_typ==1) +
                           2*(impMEM_typ==1 & impNONMEM_typ==0) +
                           3*(impMEM_typ==0 & impNONMEM_typ>1) +
                           4*(impMEM_typ==1 & impNONMEM_typ>=1)))

V1data$rMCI_comp_V1 <- with(V1data, (0*(impMEM_comp==0 & impNONMEM_comp==0) +
                            1*(impMEM_comp==0 & impNONMEM_comp==1) +
                            2*(impMEM_comp==1 & impNONMEM_comp==0) +
                            3*(impMEM_comp==0 & impNONMEM_comp>1) +
                            4*(impMEM_comp==1 & impNONMEM_comp>=1)))

V1data$rMCI_cons_V1 <- with(V1data, (0*(impMEM_cons==0 & impNONMEM_cons==0) +
                            1*(impMEM_cons==0 & impNONMEM_cons==1) +
                            2*(impMEM_cons==1 & impNONMEM_cons==0) +
                            3*(impMEM_cons==0 & impNONMEM_cons>1) +
                            4*(impMEM_cons==1 & impNONMEM_cons>=1)))

V1data$rMCI_p5_V1 <- with(V1data, (0*(impMEM_comp5==0 & impNONMEM_comp5==0) +
                          1*(impMEM_comp5==0 & impNONMEM_comp5==1) +
                          2*(impMEM_comp5==1 & impNONMEM_comp5==0) +
                          3*(impMEM_comp5==0 & impNONMEM_comp5>1) +
                          4*(impMEM_comp5==1 & impNONMEM_comp5>=1)))

V1data$rMCI_p25_V1 <- with(V1data, (0*(impMEM_comp25==0 & impNONMEM_comp25==0) +
                           1*(impMEM_comp25==0 & impNONMEM_comp25==1) +
                           2*(impMEM_comp25==1 & impNONMEM_comp25==0) +
                           3*(impMEM_comp25==0 & impNONMEM_comp25>1) +
                           4*(impMEM_comp25==1 & impNONMEM_comp25>=1)))

# Combine all data
V1_final <- cbind(V1MCI, V1data)

# Save final MCI measures file
write.csv(V1_final, "data/intermediate_data/MCI_03a04_vetsa1_MCI_AllData.csv", row.names=FALSE)
summary(V1_final)
