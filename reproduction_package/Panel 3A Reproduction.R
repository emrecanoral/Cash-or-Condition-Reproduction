
#Clear the memory
rm(list = ls()) 

#install.packages("pacman")

#load libraries that we might use:
library(pacman)

#Basics
p_load(tictoc,tidyverse,dplyr,broom,ggplot2,
       data.table,MASS,
       stargazer,DT,furrr, haven, texreg)

#Estimation packages
p_load(estimatr,boot,plm,lmtest)


set.seed(1337)

# Load the data
dir <- "C:/Users/Emre Can Oral/Desktop/reproduction_package/"
CorC= read_dta(paste0(dir,'CorC_Public_Data_FINAL_DEIDENTIFIED.dta'))

setwd(dir)


RHS_controls <- paste("highest_grade_baseline", "asset_index_baseline", "never_had_sex_baseline",
                "`_Iage_R1_14`", "`_Iage_R1_15`", "`_Iage_R1_16`", "`_Iage_R1_17`", "`_Iage_R1_18`", 
                "`_Iage_R1_19`","`_Iage_R1_20`", "stratum1", "stratum2", sep=" + ")




##########################################################################
  # TABLE III PANEL A: SCHOOL ENROLLMENT & ATTENDANCE (SELF-REPORTED) #
##########################################################################

#Reg1
fmla_reg1 <- as.formula(paste("term1_r2 ~ T2a + T2b", RHS_controls, sep="+"))
reg1 <- lm_robust(formula = fmla_reg1, data = CorC, subset = sample_SG==1 & panel ==1 & round==2
                 , weights = wgt, clusters = eaid)

#Reg2
fmla_reg2 <- as.formula(paste("term2_r2 ~ T2a + T2b", RHS_controls, sep="+"))
reg2 <- lm_robust(formula = fmla_reg2, data = CorC, subset = sample_SG==1 & panel ==1 & round==2
                  , weights = wgt, clusters = eaid)

#Reg3
fmla_reg3 <- as.formula(paste("term3_r2 ~ T2a + T2b", RHS_controls, sep="+"))
reg3 <- lm_robust(formula = fmla_reg3, data = CorC, subset = sample_SG==1 & panel ==1 & round==2
                  , weights = wgt, clusters = eaid)

#Reg4
fmla_reg4 <- as.formula(paste("inschool_term1_2009 ~ T2a + T2b", RHS_controls, sep="+"))
reg4 <- lm_robust(formula = fmla_reg4, data = CorC, subset = sample_SG==1 & panel ==1 & round==3
                  , weights = wgt, clusters = eaid)

#Reg5
fmla_reg5 <- as.formula(paste("inschool_term2_2009 ~ T2a + T2b", RHS_controls, sep="+"))
reg5 <- lm_robust(formula = fmla_reg5, data = CorC, subset = sample_SG==1 & panel ==1 & round==3
                  , weights = wgt, clusters = eaid)

#Reg6
fmla_reg6 <- as.formula(paste("inschool_term3_2009 ~ T2a + T2b", RHS_controls, sep="+"))
reg6 <- lm_robust(formula = fmla_reg6, data = CorC, subset = sample_SG==1 & panel ==1 & round==3
                  , weights = wgt, clusters = eaid)

#Reg7
fmla_reg7 <- as.formula(paste("num_terms_enrolled ~ T2a + T2b", RHS_controls, sep="+"))
reg7 <- lm_robust(formula = fmla_reg7, data = CorC, subset = sample_SG==1 & panel ==1 & round==3
                  , weights = wgt, clusters = eaid)
#Reg8
fmla_reg8 <- as.formula(paste("inschool_term1_2010 ~ T2a + T2b", RHS_controls, sep="+"))
reg8 <- lm_robust(formula = fmla_reg8, data = CorC, subset = sample_SG==1 & panel ==1 & round==3
                  , weights = wgt, clusters = eaid)




texreg(list(reg1,reg2,reg3,reg4,reg5,reg6,reg7,reg8),
       booktabs = TRUE, include.ci = FALSE, digits =3, file="Table3_Panel_A")


