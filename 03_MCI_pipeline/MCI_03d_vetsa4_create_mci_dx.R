rm(list = ls())
library(dplyr)
library(psych)
library(readr)
library(haven)
library(mice) #loading mice library

#--------------------------------------------------------------#
# Overview: VETSA4 MCI Diagnosis Pipeline                      #
#                                                              #
# This script processes cognitive test data from VETSA Wave 4  #
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
# Notes for VETSA4:                                            #
# - Includes Boston Naming Test but no Spatial Span            #
# - Missing counters for number of missing tests per domain    #
# - All scores adjusted for Age 20 AFQT                        #
#--------------------------------------------------------------#

# -----------------------------------------------------------------------
# Read In Raw Data
# -----------------------------------------------------------------------

# Set working directory
setwd("~/netshare/M/Projects/PracEffects_GEE")

# Read in normed data
V4 <- read_csv("data/intermediate_data/MCI_02d01_vetsa4_MCI_PreImputation.csv")

# Checking Summaries
summary(V4)
dim(V4)
names(V4)

# Convert all variable names to uppercase
names(V4) <- toupper(names(V4))

# Define names of MISSING counters
missing_names <- c("TOTALMISSING", "MEMORYMISSING", "EXECMISSING", "ATTNMISSING", "VERBALMISSING", "VISUALMISSING", "PROCMISSING")

# Define names of normed scores to be imputed for wave 4
norm_scores <- c("MTXT_V4", "CVATOTSS_V4", "CVLDFSS_V4", "LM1ASS_V4", "LM1BSS_V4", "LM2ASS_V4", "LM2BSS_V4", 
                 "DSPSS_V4", "LNSC_V4", "TRL1TSC_V4", "TRL2TSC_V4", "TRL3TSC_V4", "TRL4TSC_V4", "TRL5TSC_V4", 
                 "STRWT_V4", "STRCT_V4", "STRIT_V4", "HFTOTCOR_V4", "MR1COR_V4",
                 "LFCORSC_V4", "CFCORSC_V4", "CSCORSC_V4", "CSSACCSC_V4", "VRITOTSS_V4", "VRDTOTSS_V4", "VRCTOTSS_V4", "BNTSS_V4")

# -----------------------------------------------------------------------
# Imputing Missing Data Scores 
# -----------------------------------------------------------------------

# Select only the norm_scores variables for imputation from the full V4 dataframe
preimp_data <- V4[, norm_scores]

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
V4_Imp <- cbind(V4[, c("VETSAID", missing_names)], means_df)

# Standardizing Mental Rotation and Hidden Figure Scores
V4_Imp$MR1CORZ_V4 <- scale(V4_Imp$MR1COR_V4)
V4_Imp$HFTOTCORZ_V4 <- scale(V4_Imp$HFTOTCOR_V4)

# Checking Summaries post imputation, there should be no missing data

summary(V4_Imp)
names(V4_Imp)
write.csv(V4_Imp, "data/intermediate_data/MCI_03d02_vetsa4_MCI_PostImputation.csv", row.names=FALSE)

# -----------------------------------------------------------------------
# Creating Composite Scores for Multiple Tests within Domain
# -----------------------------------------------------------------------

# Scaling CVATSS before averaging with CVLDFSS (which is already standardized to mean = 0, sd = 1)

V4_Imp$CVATOTSS_V4 <- scale(V4_Imp$CVATOTSS_V4, center=50, scale=10)

# Episodic Memory
CVLTtests <- c("CVATOTSS_V4","CVLDFSS_V4")
LMtests <- c("LM1ASS_V4","LM1BSS_V4","LM2ASS_V4","LM2BSS_V4")
VRtests <- c("VRITOTSS_V4","VRDTOTSS_V4")

# Processing Speed
TRLtests <- c("TRL2TSC_V4","TRL3TSC_V4")
STRtests <- c("STRWT_V4","STRCT_V4")

V4_Imp$CVLT <- apply(V4_Imp[,CVLTtests], 1, mean)
V4_Imp$LM <-   apply(V4_Imp[,LMtests], 1, mean)
V4_Imp$VR <-   apply(V4_Imp[,VRtests], 1, mean)
V4_Imp$TRL <-  apply(V4_Imp[,TRLtests], 1, mean)
V4_Imp$STR <-  apply(V4_Imp[,STRtests], 1, mean)

summary(V4_Imp)

names(V4_Imp)


# -----------------------------------------------------------------------
# Creating Normalized Test Scores, Based on Published Norms...
# -----------------------------------------------------------------------

# Scaled Scores, mean = 10, sd = 3

# Because SSPSS_V4 is full of NAs, we do not include it here in SStests
SStests <- c("DSPSS_V4", "LNSC_V4", "VRITOTSS_V4", "VRDTOTSS_V4", 
             "VRCTOTSS_V4", "TRL1TSC_V4", "TRL2TSC_V4", "TRL3TSC_V4", "TRL4TSC_V4",
             "LFCORSC_V4", "CFCORSC_V4", "CSSACCSC_V4", "LM1ASS_V4", "LM1BSS_V4",
             "LM2ASS_V4", "LM2BSS_V4", "BNTSS_V4")

# T-Scores, mean = 50, sd = 10 , CVATSS already scaled previously
 
TStests <- c("STRWT_V4", "STRCT_V4", "STRIT_V4", "MTXT_V4")

# Z-Scores, mean = 0, sd = 1 

ZStests <- c("MR1CORZ_V4", "HFTOTCORZ_V4")

# Normalizing based on means and sds above. No need to normalize those that are already Z-scores

for(i in seq_along(SStests)){V4_Imp[,SStests[i]] <- scale(V4_Imp[,SStests[i]],center=10,scale=3) }
for(i in seq_along(TStests)){V4_Imp[,TStests[i]] <- scale(V4_Imp[,TStests[i]],center=50,scale=10) }


# Normalizing composite test scores based on sd/sqrt(number of tests)

V4_Imp$CVLT <- scale(V4_Imp$CVLT, center=0, scale= 1/sqrt(2))

V4_Imp$LM <- scale(V4_Imp$LM, center=10, scale= 3/sqrt(4))

V4_Imp$VR <- scale(V4_Imp$VR, center=10, scale= 3/sqrt(2))

V4_Imp$TRL <- scale(V4_Imp$TRL, center=10, scale= 3/sqrt(2))

V4_Imp$STR <- scale(V4_Imp$STR, center=50, scale= 10/sqrt(2))

# Checking Output

summary(V4_Imp)
write.csv(V4_Imp, "data/intermediate_data/MCI_03d03_vetsa4_MCI_Composites.csv", row.names=FALSE)

# -----------------------------------------------------------------------
# CREATING MCI MEASURES 
# -----------------------------------------------------------------------

# Note: All scores already adjusted for age 20 AFQT
# Note: Wave 4 has Boston Naming Test but no Spatial Span
V4MCI <- V4_Imp

# Define domain tests - NO SSPSS_V4 in ATTNtests, INCLUDES BNTSS in LANGtests for Wave 4
MEMtests <- c("CVLT", "LM", "VR")  # 3 composites from 8 tests
EXECtests <- c("TRL4TSC_V4", "CSSACCSC_V4", "STRIT_V4", "MTXT_V4") # 4 tests
ATTNtests <- c("DSPSS_V4", "LNSC_V4", "TRL1TSC_V4") # 3 tests - No SSPSS in Wave 4
LANGtests <- c("LFCORSC_V4", "CFCORSC_V4", "BNTSS_V4") # 3 tests - Includes BNTSS in Wave 4
VStests <- c("HFTOTCORZ_V4", "MR1CORZ_V4", "VRCTOTSS_V4") # 3 tests
PStests <- c("TRL", "STR") # 2 composites from 4 tests

tests <- c(MEMtests, EXECtests, ATTNtests, LANGtests, VStests, PStests)

# -----------------------------------------------------------------------
# Defining Composite Domain Score for VETSA Standarized Measures
# -----------------------------------------------------------------------

# Summing Scores
V4MCI$MEM <- apply(V4MCI[,MEMtests], 1, mean)
V4MCI$EXEC <- apply(V4MCI[,EXECtests], 1, mean)
V4MCI$ATTN <- apply(V4MCI[,ATTNtests], 1, mean)
V4MCI$LANG <- apply(V4MCI[,LANGtests], 1, mean)
V4MCI$VS <- apply(V4MCI[,VStests], 1, mean)
V4MCI$PS <- apply(V4MCI[,PStests], 1, mean)

#Scaling Based on Number of Tests per Domain
V4MCI$MEM <- scale(V4MCI$MEM, center=0, scale=1/sqrt(length(MEMtests)))
V4MCI$EXEC <- scale(V4MCI$EXEC, center=0, scale=1/sqrt(length(EXECtests)))
V4MCI$ATTN <- scale(V4MCI$ATTN, center=0, scale=1/sqrt(length(ATTNtests)))
V4MCI$LANG <- scale(V4MCI$LANG, center=0, scale=1/sqrt(length(LANGtests)))
V4MCI$VS <- scale(V4MCI$VS, center=0, scale=1/sqrt(length(VStests)))
V4MCI$PS <- scale(V4MCI$PS, center=0, scale=1/sqrt(length(PStests)))

# -----------------------------------------------------------------------
# Creating Domain-Based MCI Classification
# -----------------------------------------------------------------------

# Define cutoff-based impairment indicators
V4data <- data.frame(matrix(ncol=0, nrow=nrow(V4MCI)))

# 1 SD Cutoff (16%)
for(j in seq_along(tests)){
    V4data[paste("imp1", tests[j], sep="")] <- as.numeric(V4MCI[,tests[j]] <= (-1))
}

# 1.5 SD Cutoff (6.7%)
for(j in seq_along(tests)){
    V4data[paste("imp2", tests[j], sep="")] <- as.numeric(V4MCI[,tests[j]] <= (-1.5))
}

# Define domain impairment counts
V4data$nMEM1 <- rowSums(V4data[,paste("imp1", MEMtests, sep="")])
V4data$nEXEC1 <- rowSums(V4data[,paste("imp1", EXECtests, sep="")])
V4data$nATTN1 <- rowSums(V4data[,paste("imp1", ATTNtests, sep="")])
V4data$nLANG1 <- rowSums(V4data[,paste("imp1", LANGtests, sep="")])
V4data$nVS1 <- rowSums(V4data[,paste("imp1", VStests, sep="")])
V4data$nPS1 <- rowSums(V4data[,paste("imp1", PStests, sep="")])

V4data$nMEM2 <- rowSums(V4data[,paste("imp2", MEMtests, sep="")])
V4data$nEXEC2 <- rowSums(V4data[,paste("imp2", EXECtests, sep="")])
V4data$nATTN2 <- rowSums(V4data[,paste("imp2", ATTNtests, sep="")])
V4data$nLANG2 <- rowSums(V4data[,paste("imp2", LANGtests, sep="")])
V4data$nVS2 <- rowSums(V4data[,paste("imp2", VStests, sep="")])
V4data$nPS2 <- rowSums(V4data[,paste("imp2", PStests, sep="")])

# Define domain impairment indicators using different criteria
# Typical - 1 test, 1.5 sd
V4data$impMEM_typ <- as.numeric(V4data$nMEM2 >= 1)
V4data$impEXEC_typ <- as.numeric(V4data$nEXEC2 >= 1)
V4data$impATTN_typ <- as.numeric(V4data$nATTN2 >= 1)
V4data$impLANG_typ <- as.numeric(V4data$nLANG2 >= 1)
V4data$impVS_typ <- as.numeric(V4data$nVS2 >= 1)
V4data$impPS_typ <- as.numeric(V4data$nPS2 >= 1)

# Comprehensive - 2 tests, 1 sd
V4data$impMEM_comp <- as.numeric(V4data$nMEM1 >= 2)
V4data$impEXEC_comp <- as.numeric(V4data$nEXEC1 >= 2)
V4data$impATTN_comp <- as.numeric(V4data$nATTN1 >= 2)
V4data$impLANG_comp <- as.numeric(V4data$nLANG1 >= 2)
V4data$impVS_comp <- as.numeric(V4data$nVS1 >= 2)
V4data$impPS_comp <- as.numeric(V4data$nPS1 >= 2)

# Conservative - 2 tests, 1.5 sd
V4data$impMEM_cons <- as.numeric(V4data$nMEM2 >= 2)
V4data$impEXEC_cons <- as.numeric(V4data$nEXEC2 >= 2)
V4data$impATTN_cons <- as.numeric(V4data$nATTN2 >= 2)
V4data$impLANG_cons <- as.numeric(V4data$nLANG2 >= 2)
V4data$impVS_cons <- as.numeric(V4data$nVS2 >= 2)
V4data$impPS_cons <- as.numeric(V4data$nPS2 >= 2)

# Quantile-based impairment (5th percentile)
V4data$impMEM_comp5 <- as.numeric(V4MCI$MEM <= quantile(V4MCI$MEM, 0.05, na.rm=TRUE))
V4data$impEXEC_comp5 <- as.numeric(V4MCI$EXEC <= quantile(V4MCI$EXEC, 0.05, na.rm=TRUE))
V4data$impATTN_comp5 <- as.numeric(V4MCI$ATTN <= quantile(V4MCI$ATTN, 0.05, na.rm=TRUE))
V4data$impLANG_comp5 <- as.numeric(V4MCI$LANG <= quantile(V4MCI$LANG, 0.05, na.rm=TRUE))
V4data$impVS_comp5 <- as.numeric(V4MCI$VS <= quantile(V4MCI$VS, 0.05, na.rm=TRUE))
V4data$impPS_comp5 <- as.numeric(V4MCI$PS <= quantile(V4MCI$PS, 0.05, na.rm=TRUE))

# Quantile-based impairment (2.5th percentile)
V4data$impMEM_comp25 <- as.numeric(V4MCI$MEM <= quantile(V4MCI$MEM, 0.025, na.rm=TRUE))
V4data$impEXEC_comp25 <- as.numeric(V4MCI$EXEC <= quantile(V4MCI$EXEC, 0.025, na.rm=TRUE))
V4data$impATTN_comp25 <- as.numeric(V4MCI$ATTN <= quantile(V4MCI$ATTN, 0.025, na.rm=TRUE))
V4data$impLANG_comp25 <- as.numeric(V4MCI$LANG <= quantile(V4MCI$LANG, 0.025, na.rm=TRUE))
V4data$impVS_comp25 <- as.numeric(V4MCI$VS <= quantile(V4MCI$VS, 0.025, na.rm=TRUE))
V4data$impPS_comp25 <- as.numeric(V4MCI$PS <= quantile(V4MCI$PS, 0.025, na.rm=TRUE))

# Create nonMEM impairment indicators
nonmemdoms <- c("EXEC", "ATTN", "LANG", "VS", "PS")
doms_typ <- paste(paste("imp", nonmemdoms, sep=""), "typ", sep="_")
doms_comp <- paste(paste("imp", nonmemdoms, sep=""), "comp", sep="_")
doms_cons <- paste(paste("imp", nonmemdoms, sep=""), "cons", sep="_")
doms_comp5 <- paste(paste("imp", nonmemdoms, sep=""), "comp5", sep="_")
doms_comp25 <- paste(paste("imp", nonmemdoms, sep=""), "comp25", sep="_")

V4data$impNONMEM_typ <- rowSums(V4data[,doms_typ])
V4data$impNONMEM_comp <- rowSums(V4data[,doms_comp])
V4data$impNONMEM_cons <- rowSums(V4data[,doms_cons])
V4data$impNONMEM_comp5 <- rowSums(V4data[,doms_comp5])
V4data$impNONMEM_comp25 <- rowSums(V4data[,doms_comp25])

# Create MCI classifications
# 0 = no impairments
# 1 = Single-Domain Non-Amnestic
# 2 = Single-Domain Amnestic
# 3 = Multi-Domain Non-Amnestic
# 4 = Multi-Domain Amnestic

V4data$rMCI_typ <- with(V4data, (0*(impMEM_typ==0 & impNONMEM_typ==0) +
                           1*(impMEM_typ==0 & impNONMEM_typ==1) +
                           2*(impMEM_typ==1 & impNONMEM_typ==0) +
                           3*(impMEM_typ==0 & impNONMEM_typ>1) +
                           4*(impMEM_typ==1 & impNONMEM_typ>=1)))

V4data$rMCI_comp <- with(V4data, (0*(impMEM_comp==0 & impNONMEM_comp==0) +
                            1*(impMEM_comp==0 & impNONMEM_comp==1) +
                            2*(impMEM_comp==1 & impNONMEM_comp==0) +
                            3*(impMEM_comp==0 & impNONMEM_comp>1) +
                            4*(impMEM_comp==1 & impNONMEM_comp>=1)))

V4data$rMCI_cons <- with(V4data, (0*(impMEM_cons==0 & impNONMEM_cons==0) +
                            1*(impMEM_cons==0 & impNONMEM_cons==1) +
                            2*(impMEM_cons==1 & impNONMEM_cons==0) +
                            3*(impMEM_cons==0 & impNONMEM_cons>1) +
                            4*(impMEM_cons==1 & impNONMEM_cons>=1)))

V4data$rMCI_p5 <- with(V4data, (0*(impMEM_comp5==0 & impNONMEM_comp5==0) +
                          1*(impMEM_comp5==0 & impNONMEM_comp5==1) +
                          2*(impMEM_comp5==1 & impNONMEM_comp5==0) +
                          3*(impMEM_comp5==0 & impNONMEM_comp5>1) +
                          4*(impMEM_comp5==1 & impNONMEM_comp5>=1)))

V4data$rMCI_p25 <- with(V4data, (0*(impMEM_comp25==0 & impNONMEM_comp25==0) +
                           1*(impMEM_comp25==0 & impNONMEM_comp25==1) +
                           2*(impMEM_comp25==1 & impNONMEM_comp25==0) +
                           3*(impMEM_comp25==0 & impNONMEM_comp25>1) +
                           4*(impMEM_comp25==1 & impNONMEM_comp25>=1)))

# Combine all data
V4_final <- cbind(V4MCI, V4data)

# Save final MCI measures file
write.csv(V4_final, "data/intermediate_data/MCI_03d04_vetsa4_MCI_AllData.csv", row.names=FALSE)
summary(V4_final)
