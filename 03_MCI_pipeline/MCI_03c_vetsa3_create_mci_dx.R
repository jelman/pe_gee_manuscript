rm(list = ls())
library(dplyr)
library(psych)
library(readr)
library(haven)
library(mice) #loading mice library

#--------------------------------------------------------------#
# Overview: VETSA3 MCI Diagnosis Pipeline                      #
#                                                              #
# This script processes cognitive test data from VETSA Wave 3  #
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
# Notes for VETSA3:                                            #
# - Includes both Spatial Span and Boston Naming Test          #
# - All scores adjusted for Age 20 AFQT                        #
#--------------------------------------------------------------#

# -----------------------------------------------------------------------
# Read In Raw Data
# -----------------------------------------------------------------------

# Set working directory
setwd("~/netshare/M/Projects/PracEffects_GEE")

# Read in normed data
V3 <- read_csv("data/intermediate_data/MCI_02c01_vetsa3_MCI_PreImputation.csv")

# Checking Summaries
summary(V3)
dim(V3)
names(V3)

# Convert all variable names to uppercase
names(V3) <- toupper(names(V3))

# Define names of MISSING counters
missing_names <- c("TOTALMISSING", "MEMORYMISSING", "EXECMISSING", "ATTNMISSING", "VERBALMISSING", "VISUALMISSING", "PROCMISSING")


# Define names of normed scores to be imputed for wave 3
norm_scores <- c("MTXT_V3", "CVATOTSS_V3", "CVLDFSS_V3", "LM1ASS_V3", "LM1BSS_V3", "LM2ASS_V3", "LM2BSS_V3", 
                 "DSPSS_V3", "LNSC_V3", "TRL1TSC_V3", "TRL2TSC_V3", "TRL3TSC_V3", "TRL4TSC_V3", "TRL5TSC_V3", 
                 "STRWT_V3", "STRCT_V3", "STRIT_V3", "HFTOTCOR_V3", "MR1COR_V3",
                 "LFCORSC_V3", "CFCORSC_V3", "CSCORSC_V3", "CSSACCSC_V3", "VRITOTSS_V3", "VRDTOTSS_V3", "VRCTOTSS_V3", "SSPSS_V3", "BNTSS_V3")

# -----------------------------------------------------------------------
# Imputing Missing Data Scores 
# -----------------------------------------------------------------------

# Select only the norm_scores variables for imputation from the full V3 dataframe
preimp_data <- V3[, norm_scores]

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
V3_Imp <- cbind(V3[, c("VETSAID", missing_names)], means_df)

# Standardizing Mental Rotation and Hidden Figure Scores

V3_Imp$MR1CORZ_V3 <- scale(V3_Imp$MR1COR_V3)
V3_Imp$HFTOTCORZ_V3 <- scale(V3_Imp$HFTOTCOR_V3)

# Checking Summaries post imputation, there should be no missing data

summary(V3_Imp)
names(V3_Imp)

write.csv(V3_Imp, "data/intermediate_data/MCI_03c03_vetsa3_MCI_Composites.csv", row.names=FALSE)

# -----------------------------------------------------------------------
# Creating Composite Scores for Multiple Tests within Domain
# -----------------------------------------------------------------------

# Scaling CVATSS before averaging with CVLDFSS (which is already standardized to mean = 0, sd = 1)

V3_Imp$CVATOTSS_V3 <- scale(V3_Imp$CVATOTSS_V3, center=50, scale=10)

# Episodic Memory
CVLTtests <- c("CVATOTSS_V3","CVLDFSS_V3")
LMtests <- c("LM1ASS_V3","LM1BSS_V3","LM2ASS_V3","LM2BSS_V3")
VRtests <- c("VRITOTSS_V3","VRDTOTSS_V3")

# Processing Speed
TRLtests <- c("TRL2TSC_V3","TRL3TSC_V3")
STRtests <- c("STRWT_V3","STRCT_V3")

V3_Imp$CVLT <- apply(V3_Imp[,CVLTtests], 1, mean)
V3_Imp$LM <-   apply(V3_Imp[,LMtests], 1, mean)
V3_Imp$VR <-   apply(V3_Imp[,VRtests], 1, mean)
V3_Imp$TRL <-  apply(V3_Imp[,TRLtests], 1, mean)
V3_Imp$STR <-  apply(V3_Imp[,STRtests], 1, mean)

summary(V3_Imp)

names(V3_Imp)

# -----------------------------------------------------------------------
# Creating Normalized Test Scores, Based on Published Norms...
# -----------------------------------------------------------------------

# Scaled Scores, mean = 10, sd = 3
SStests <- c("DSPSS_V3", "LNSC_V3", "VRITOTSS_V3", "VRDTOTSS_V3", 
             "VRCTOTSS_V3", "TRL1TSC_V3", "TRL2TSC_V3", "TRL3TSC_V3", "TRL4TSC_V3",
             "LFCORSC_V3", "CFCORSC_V3", "CSSACCSC_V3", "LM1ASS_V3", "LM1BSS_V3",
             "LM2ASS_V3", "LM2BSS_V3", "SSPSS_V3", "BNTSS_V3")

# T-Scores, mean = 50, sd = 10
TStests <- c("STRWT_V3", "STRCT_V3", "STRIT_V3", "MTXT_V3")

# Z-Scores, mean = 0, sd = 1
ZStests <- c("MR1CORZ_V3", "HFTOTCORZ_V3")

# Normalizing based on means and sds above. No need to normalize those that are already Z-scores

for(i in seq_along(SStests)){V3_Imp[,SStests[i]] <- scale(V3_Imp[,SStests[i]],center=10,scale=3) }
for(i in seq_along(TStests)){V3_Imp[,TStests[i]] <- scale(V3_Imp[,TStests[i]],center=50,scale=10) }

# Normalizing composite test scores based on sd/sqrt(number of tests)

V3_Imp$CVLT <- scale(V3_Imp$CVLT, center=0, scale= 1/sqrt(2))
V3_Imp$LM <- scale(V3_Imp$LM, center=10, scale= 3/sqrt(4))
V3_Imp$VR <- scale(V3_Imp$VR, center=10, scale= 3/sqrt(2))
V3_Imp$TRL <- scale(V3_Imp$TRL, center=10, scale= 3/sqrt(2))
V3_Imp$STR <- scale(V3_Imp$STR, center=50, scale= 10/sqrt(2))

# Checking Output

summary(V3_Imp)

write.csv(V3_Imp, "MCI_04c3_V3_Imp2.csv")

# -----------------------------------------------------------------------
# CREATING MCI MEASURES 
# -----------------------------------------------------------------------

# Note: All scores already adjusted for age 20 AFQT
# Note: Wave 3 has Spatial Span but no Boston Naming Test
V3MCI <- V3_Imp

# Define domain tests - INCLUDES SSPSS_V3 in ATTNtests, NO BNTSS in LANGtests for Wave 3
MEMtests <- c("CVLT", "LM", "VR")  # 3 composites from 8 tests
EXECtests <- c("TRL4TSC_V3", "CSSACCSC_V3", "STRIT_V3", "MTXT_V3") # 4 tests
ATTNtests <- c("DSPSS_V3", "LNSC_V3", "TRL1TSC_V3", "SSPSS_V3") # 4 tests
LANGtests <- c("LFCORSC_V3", "CFCORSC_V3", "BNTSS_V3") # 3 tests
VStests <- c("HFTOTCORZ_V3", "MR1CORZ_V3", "VRCTOTSS_V3") # 3 tests
PStests <- c("TRL", "STR") # 2 composites from 4 tests

tests <- c(MEMtests,EXECtests,ATTNtests,LANGtests,VStests,PStests)

# -----------------------------------------------------------------------
# Defining Composite Domain Score for VETSA Standarized Measures
# -----------------------------------------------------------------------

# Summing Scores
V3MCI$MEM <- apply(V3MCI[,MEMtests], 1, mean)
V3MCI$EXEC <- apply(V3MCI[,EXECtests], 1, mean)
V3MCI$ATTN <- apply(V3MCI[,ATTNtests], 1, mean)
V3MCI$LANG <- apply(V3MCI[,LANGtests], 1, mean)
V3MCI$VS <- apply(V3MCI[,VStests], 1, mean)
V3MCI$PS <- apply(V3MCI[,PStests], 1, mean)

# Scaling Based on Number of Tests per Domain
V3MCI$MEM <- scale(V3MCI$MEM, center=0, scale=1/sqrt(length(MEMtests)))
V3MCI$EXEC <- scale(V3MCI$EXEC, center=0, scale=1/sqrt(length(EXECtests)))
V3MCI$ATTN <- scale(V3MCI$ATTN, center=0, scale=1/sqrt(length(ATTNtests)))
V3MCI$LANG <- scale(V3MCI$LANG, center=0, scale=1/sqrt(length(LANGtests)))
V3MCI$VS <- scale(V3MCI$VS, center=0, scale=1/sqrt(length(VStests)))
V3MCI$PS <- scale(V3MCI$PS, center=0, scale=1/sqrt(length(PStests)))

# -----------------------------------------------------------------------
# Creating Domain-Based MCI Classification
# -----------------------------------------------------------------------

# Define cutoff-based impairment indicators
V3data <- data.frame(matrix(ncol=0, nrow=nrow(V3MCI)))

# 1 SD Cutoff (16%)
for(j in seq_along(tests)){
    V3data[paste("imp1", tests[j], sep="")] <- as.numeric(V3MCI[,tests[j]] <= (-1))
}

# 1.5 SD Cutoff (6.7%)
for(j in seq_along(tests)){
    V3data[paste("imp2", tests[j], sep="")] <- as.numeric(V3MCI[,tests[j]] <= (-1.5))
}

# Define domain impairment counts
V3data$nMEM1 <- rowSums(V3data[,paste("imp1", MEMtests, sep="")])
V3data$nEXEC1 <- rowSums(V3data[,paste("imp1", EXECtests, sep="")])
V3data$nATTN1 <- rowSums(V3data[,paste("imp1", ATTNtests, sep="")])
V3data$nLANG1 <- rowSums(V3data[,paste("imp1", LANGtests, sep="")])
V3data$nVS1 <- rowSums(V3data[,paste("imp1", VStests, sep="")])
V3data$nPS1 <- rowSums(V3data[,paste("imp1", PStests, sep="")])

V3data$nMEM2 <- rowSums(V3data[,paste("imp2", MEMtests, sep="")])
V3data$nEXEC2 <- rowSums(V3data[,paste("imp2", EXECtests, sep="")])
V3data$nATTN2 <- rowSums(V3data[,paste("imp2", ATTNtests, sep="")])
V3data$nLANG2 <- rowSums(V3data[,paste("imp2", LANGtests, sep="")])
V3data$nVS2 <- rowSums(V3data[,paste("imp2", VStests, sep="")])
V3data$nPS2 <- rowSums(V3data[,paste("imp2", PStests, sep="")])

# Define domain impairment indicators using different criteria
# Typical - 1 test, 1.5 sd
V3data$impMEM_typ <- as.numeric(V3data$nMEM2 >= 1)
V3data$impEXEC_typ <- as.numeric(V3data$nEXEC2 >= 1)
V3data$impATTN_typ <- as.numeric(V3data$nATTN2 >= 1)
V3data$impLANG_typ <- as.numeric(V3data$nLANG2 >= 1)
V3data$impVS_typ <- as.numeric(V3data$nVS2 >= 1)
V3data$impPS_typ <- as.numeric(V3data$nPS2 >= 1)

# Comprehensive - 2 tests, 1 sd
V3data$impMEM_comp <- as.numeric(V3data$nMEM1 >= 2)
V3data$impEXEC_comp <- as.numeric(V3data$nEXEC1 >= 2)
V3data$impATTN_comp <- as.numeric(V3data$nATTN1 >= 2)
V3data$impLANG_comp <- as.numeric(V3data$nLANG1 >= 2)
V3data$impVS_comp <- as.numeric(V3data$nVS1 >= 2)
V3data$impPS_comp <- as.numeric(V3data$nPS1 >= 2)

# Conservative - 2 tests, 1.5 sd
V3data$impMEM_cons <- as.numeric(V3data$nMEM2 >= 2)
V3data$impEXEC_cons <- as.numeric(V3data$nEXEC2 >= 2)
V3data$impATTN_cons <- as.numeric(V3data$nATTN2 >= 2)
V3data$impLANG_cons <- as.numeric(V3data$nLANG2 >= 2)
V3data$impVS_cons <- as.numeric(V3data$nVS2 >= 2)
V3data$impPS_cons <- as.numeric(V3data$nPS2 >= 2)

# Quantile-based impairment (5th percentile)
V3data$impMEM_comp5 <- as.numeric(V3MCI$MEM <= quantile(V3MCI$MEM, 0.05, na.rm=TRUE))
V3data$impEXEC_comp5 <- as.numeric(V3MCI$EXEC <= quantile(V3MCI$EXEC, 0.05, na.rm=TRUE))
V3data$impATTN_comp5 <- as.numeric(V3MCI$ATTN <= quantile(V3MCI$ATTN, 0.05, na.rm=TRUE))
V3data$impLANG_comp5 <- as.numeric(V3MCI$LANG <= quantile(V3MCI$LANG, 0.05, na.rm=TRUE))
V3data$impVS_comp5 <- as.numeric(V3MCI$VS <= quantile(V3MCI$VS, 0.05, na.rm=TRUE))
V3data$impPS_comp5 <- as.numeric(V3MCI$PS <= quantile(V3MCI$PS, 0.05, na.rm=TRUE))

# Quantile-based impairment (2.5th percentile)
V3data$impMEM_comp25 <- as.numeric(V3MCI$MEM <= quantile(V3MCI$MEM, 0.025, na.rm=TRUE))
V3data$impEXEC_comp25 <- as.numeric(V3MCI$EXEC <= quantile(V3MCI$EXEC, 0.025, na.rm=TRUE))
V3data$impATTN_comp25 <- as.numeric(V3MCI$ATTN <= quantile(V3MCI$ATTN, 0.025, na.rm=TRUE))
V3data$impLANG_comp25 <- as.numeric(V3MCI$LANG <= quantile(V3MCI$LANG, 0.025, na.rm=TRUE))
V3data$impVS_comp25 <- as.numeric(V3MCI$VS <= quantile(V3MCI$VS, 0.025, na.rm=TRUE))
V3data$impPS_comp25 <- as.numeric(V3MCI$PS <= quantile(V3MCI$PS, 0.025, na.rm=TRUE))

# Create nonMEM impairment indicators
nonmemdoms <- c("EXEC", "ATTN", "LANG", "VS", "PS")
doms_typ <- paste(paste("imp", nonmemdoms, sep=""), "typ", sep="_")
doms_comp <- paste(paste("imp", nonmemdoms, sep=""), "comp", sep="_")
doms_cons <- paste(paste("imp", nonmemdoms, sep=""), "cons", sep="_")
doms_comp5 <- paste(paste("imp", nonmemdoms, sep=""), "comp5", sep="_")
doms_comp25 <- paste(paste("imp", nonmemdoms, sep=""), "comp25", sep="_")

V3data$impNONMEM_typ <- rowSums(V3data[,doms_typ])
V3data$impNONMEM_comp <- rowSums(V3data[,doms_comp])
V3data$impNONMEM_cons <- rowSums(V3data[,doms_cons])
V3data$impNONMEM_comp5 <- rowSums(V3data[,doms_comp5])
V3data$impNONMEM_comp25 <- rowSums(V3data[,doms_comp25])

# Create MCI classifications
# 0 = no impairments
# 1 = Single-Domain Non-Amnestic
# 2 = Single-Domain Amnestic
# 3 = Multi-Domain Non-Amnestic
# 4 = Multi-Domain Amnestic

V3data$rMCI_typ_V3 <- with(V3data, (0*(impMEM_typ==0 & impNONMEM_typ==0) +
                           1*(impMEM_typ==0 & impNONMEM_typ==1) +
                           2*(impMEM_typ==1 & impNONMEM_typ==0) +
                           3*(impMEM_typ==0 & impNONMEM_typ>1) +
                           4*(impMEM_typ==1 & impNONMEM_typ>=1)))

V3data$rMCI_comp_V3 <- with(V3data, (0*(impMEM_comp==0 & impNONMEM_comp==0) +
                            1*(impMEM_comp==0 & impNONMEM_comp==1) +
                            2*(impMEM_comp==1 & impNONMEM_comp==0) +
                            3*(impMEM_comp==0 & impNONMEM_comp>1) +
                            4*(impMEM_comp==1 & impNONMEM_comp>=1)))

V3data$rMCI_cons_V3 <- with(V3data, (0*(impMEM_cons==0 & impNONMEM_cons==0) +
                            1*(impMEM_cons==0 & impNONMEM_cons==1) +
                            2*(impMEM_cons==1 & impNONMEM_cons==0) +
                            3*(impMEM_cons==0 & impNONMEM_cons>1) +
                            4*(impMEM_cons==1 & impNONMEM_cons>=1)))

V3data$rMCI_p5_V3 <- with(V3data, (0*(impMEM_comp5==0 & impNONMEM_comp5==0) +
                          1*(impMEM_comp5==0 & impNONMEM_comp5==1) +
                          2*(impMEM_comp5==1 & impNONMEM_comp5==0) +
                          3*(impMEM_comp5==0 & impNONMEM_comp5>1) +
                          4*(impMEM_comp5==1 & impNONMEM_comp5>=1)))

V3data$rMCI_p25_V3 <- with(V3data, (0*(impMEM_comp25==0 & impNONMEM_comp25==0) +
                           1*(impMEM_comp25==0 & impNONMEM_comp25==1) +
                           2*(impMEM_comp25==1 & impNONMEM_comp25==0) +
                           3*(impMEM_comp25==0 & impNONMEM_comp25>1) +
                           4*(impMEM_comp25==1 & impNONMEM_comp25>=1)))

# Combine all data
V3_final <- cbind(V3MCI, V3data)

# Save final MCI measures file
write.csv(V3_final, "data/intermediate_data/MCI_03c04_vetsa3_MCI_AllData.csv", row.names=FALSE)
summary(V3_final)