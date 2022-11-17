# Purpose: Create visualizations for the accuracy of different   #
#          poverty identification strategies                     #
#                                                                #
# Created: October 11, 2022                                      #
# Depends on:                                                    #
#   Author: Manuel Cardona                                       # 
#   e-mail: mcardona@poverty-action.org                          #
#                                                                #

rm(list = ls()) # to clean the workspace

# **************************************************************
#### 1. Load_packages ####
# **************************************************************

library(dplyr)
library(ggplot2)
library(grid)
library(scales)
library(dampack)
library(extrafont)
library(gridExtra)
library(paletteer) 
library(hrbrthemes)
library(haven)
library(pROC)


# ***************************************************************
#### 2. Load_data ####
# ***************************************************************

#----2.0. Survey Features----
survey_1 <- read_dta("../02 Data/02 Clean/LSMS/MASTER_ESS3_Ethiopia.dta")
survey_1 <- survey_1[, c("hh_id", "rural", "region")]
survey_2 <- read_dta("../02 Data/02 Clean/LSMS/Full datasets/ESS3_2016.dta")
survey_2 <- survey_2[, c("household_id2", "urban", "region", "poor_npl1")]
survey_2$household_id2 <- as.double(survey_2$household_id2)

#----2.1. Model 1: EN - LSMS----
pred_mod1_test <- read.csv("../08 Results/LSMS/Full Datasets/01 LSMS/Short DB/Predicted_scores_10q_NPL1_test.csv")
pred_mod1_test <- merge(pred_mod1_test, survey_1, by="hh_id")
pred_mod1_test<-pred_mod1_test%>%
  mutate(urban=case_when(urban==0 ~ "Rural",
                         urban==1 ~ "Urban"),
         region=case_when(region==1 ~ "Amhara",
                          region==2 ~ "Oromiya",
                          region==3 ~ "SNNP",
                          region==4 ~ "Tigray",
                          region==5 ~ "Other regions"),
         urban_poor=case_when(urban=="Rural" & poor_npl1==0 ~ "Rural, Non-Poor",
                              urban=="Rural" & poor_npl1==1 ~ "Rural, Poor",
                              urban=="Urban" & poor_npl1==0 ~ "Urban, Non-Poor",
                              urban=="Urban" & poor_npl1==1 ~ "Urban, Poor"))
prob_cutoff <- seq(0, 1, 0.05)
pred_mod1_test$higher_00 <- pred_mod1_test$pred_score_pr_test>0.00 
pred_mod1_test$higher_05 <- pred_mod1_test$pred_score_pr_test>0.05 
pred_mod1_test$higher_10 <- pred_mod1_test$pred_score_pr_test>0.10 
pred_mod1_test$higher_15 <- pred_mod1_test$pred_score_pr_test>0.15 
pred_mod1_test$higher_20 <- pred_mod1_test$pred_score_pr_test>0.20 
pred_mod1_test$higher_25 <- pred_mod1_test$pred_score_pr_test>0.25 
pred_mod1_test$higher_30 <- pred_mod1_test$pred_score_pr_test>0.30 
pred_mod1_test$higher_35 <- pred_mod1_test$pred_score_pr_test>0.35 
pred_mod1_test$higher_40 <- pred_mod1_test$pred_score_pr_test>0.40 
pred_mod1_test$higher_45 <- pred_mod1_test$pred_score_pr_test>0.45 
pred_mod1_test$higher_50 <- pred_mod1_test$pred_score_pr_test>0.50 
pred_mod1_test$higher_55 <- pred_mod1_test$pred_score_pr_test>0.55 
pred_mod1_test$higher_60 <- pred_mod1_test$pred_score_pr_test>0.60 
pred_mod1_test$higher_65 <- pred_mod1_test$pred_score_pr_test>0.65 
pred_mod1_test$higher_70 <- pred_mod1_test$pred_score_pr_test>0.70 
pred_mod1_test$higher_75 <- pred_mod1_test$pred_score_pr_test>0.75 
pred_mod1_test$higher_80 <- pred_mod1_test$pred_score_pr_test>0.80 
pred_mod1_test$higher_85 <- pred_mod1_test$pred_score_pr_test>0.85 
pred_mod1_test$higher_90 <- pred_mod1_test$pred_score_pr_test>0.90 
pred_mod1_test$higher_95 <- pred_mod1_test$pred_score_pr_test>0.95 
pred_mod1_test$higher_100 <- pred_mod1_test$pred_score_pr_test>1

pred_mod1_test$inc_error_00 <- pred_mod1_test$higher_00==TRUE & pred_mod1_test$poor_npl1==0
pred_mod1_test$inc_error_05 <- pred_mod1_test$higher_05==TRUE & pred_mod1_test$poor_npl1==0
pred_mod1_test$inc_error_10 <- pred_mod1_test$higher_10==TRUE & pred_mod1_test$poor_npl1==0
pred_mod1_test$inc_error_15 <- pred_mod1_test$higher_15==TRUE & pred_mod1_test$poor_npl1==0
pred_mod1_test$inc_error_20 <- pred_mod1_test$higher_20==TRUE & pred_mod1_test$poor_npl1==0
pred_mod1_test$inc_error_25 <- pred_mod1_test$higher_25==TRUE & pred_mod1_test$poor_npl1==0
pred_mod1_test$inc_error_30 <- pred_mod1_test$higher_30==TRUE & pred_mod1_test$poor_npl1==0
pred_mod1_test$inc_error_35 <- pred_mod1_test$higher_35==TRUE & pred_mod1_test$poor_npl1==0
pred_mod1_test$inc_error_40 <- pred_mod1_test$higher_40==TRUE & pred_mod1_test$poor_npl1==0
pred_mod1_test$inc_error_45 <- pred_mod1_test$higher_45==TRUE & pred_mod1_test$poor_npl1==0
pred_mod1_test$inc_error_50 <- pred_mod1_test$higher_50==TRUE & pred_mod1_test$poor_npl1==0
pred_mod1_test$inc_error_55 <- pred_mod1_test$higher_55==TRUE & pred_mod1_test$poor_npl1==0
pred_mod1_test$inc_error_60 <- pred_mod1_test$higher_60==TRUE & pred_mod1_test$poor_npl1==0
pred_mod1_test$inc_error_65 <- pred_mod1_test$higher_65==TRUE & pred_mod1_test$poor_npl1==0
pred_mod1_test$inc_error_70 <- pred_mod1_test$higher_70==TRUE & pred_mod1_test$poor_npl1==0
pred_mod1_test$inc_error_75 <- pred_mod1_test$higher_75==TRUE & pred_mod1_test$poor_npl1==0
pred_mod1_test$inc_error_80 <- pred_mod1_test$higher_80==TRUE & pred_mod1_test$poor_npl1==0
pred_mod1_test$inc_error_85 <- pred_mod1_test$higher_85==TRUE & pred_mod1_test$poor_npl1==0
pred_mod1_test$inc_error_90 <- pred_mod1_test$higher_90==TRUE & pred_mod1_test$poor_npl1==0
pred_mod1_test$inc_error_95 <- pred_mod1_test$higher_95==TRUE & pred_mod1_test$poor_npl1==0
pred_mod1_test$inc_error_100 <- pred_mod1_test$higher_100==TRUE & pred_mod1_test$poor_npl1==0

pred_mod1_inc <- c(length(which(pred_mod1_test$inc_error_00==TRUE))/length(which(pred_mod1_test$poor_npl1==0)),
                   length(which(pred_mod1_test$inc_error_05==TRUE))/length(which(pred_mod1_test$poor_npl1==0)),
                   length(which(pred_mod1_test$inc_error_10==TRUE))/length(which(pred_mod1_test$poor_npl1==0)),
                   length(which(pred_mod1_test$inc_error_15==TRUE))/length(which(pred_mod1_test$poor_npl1==0)),
                   length(which(pred_mod1_test$inc_error_20==TRUE))/length(which(pred_mod1_test$poor_npl1==0)),
                   length(which(pred_mod1_test$inc_error_25==TRUE))/length(which(pred_mod1_test$poor_npl1==0)),
                   length(which(pred_mod1_test$inc_error_30==TRUE))/length(which(pred_mod1_test$poor_npl1==0)),
                   length(which(pred_mod1_test$inc_error_35==TRUE))/length(which(pred_mod1_test$poor_npl1==0)),
                   length(which(pred_mod1_test$inc_error_40==TRUE))/length(which(pred_mod1_test$poor_npl1==0)),
                   length(which(pred_mod1_test$inc_error_45==TRUE))/length(which(pred_mod1_test$poor_npl1==0)),
                   length(which(pred_mod1_test$inc_error_50==TRUE))/length(which(pred_mod1_test$poor_npl1==0)),
                   length(which(pred_mod1_test$inc_error_55==TRUE))/length(which(pred_mod1_test$poor_npl1==0)),
                   length(which(pred_mod1_test$inc_error_60==TRUE))/length(which(pred_mod1_test$poor_npl1==0)),
                   length(which(pred_mod1_test$inc_error_65==TRUE))/length(which(pred_mod1_test$poor_npl1==0)),
                   length(which(pred_mod1_test$inc_error_70==TRUE))/length(which(pred_mod1_test$poor_npl1==0)),
                   length(which(pred_mod1_test$inc_error_75==TRUE))/length(which(pred_mod1_test$poor_npl1==0)),
                   length(which(pred_mod1_test$inc_error_80==TRUE))/length(which(pred_mod1_test$poor_npl1==0)),
                   length(which(pred_mod1_test$inc_error_85==TRUE))/length(which(pred_mod1_test$poor_npl1==0)),
                   length(which(pred_mod1_test$inc_error_90==TRUE))/length(which(pred_mod1_test$poor_npl1==0)),
                   length(which(pred_mod1_test$inc_error_95==TRUE))/length(which(pred_mod1_test$poor_npl1==0)),
                   length(which(pred_mod1_test$inc_error_100==TRUE))/length(which(pred_mod1_test$poor_npl1==0)))

pred_mod1_test$exc_error_00 <- pred_mod1_test$higher_00==FALSE & pred_mod1_test$poor_npl1==1
pred_mod1_test$exc_error_05 <- pred_mod1_test$higher_05==FALSE & pred_mod1_test$poor_npl1==1
pred_mod1_test$exc_error_10 <- pred_mod1_test$higher_10==FALSE & pred_mod1_test$poor_npl1==1
pred_mod1_test$exc_error_15 <- pred_mod1_test$higher_15==FALSE & pred_mod1_test$poor_npl1==1
pred_mod1_test$exc_error_20 <- pred_mod1_test$higher_20==FALSE & pred_mod1_test$poor_npl1==1
pred_mod1_test$exc_error_25 <- pred_mod1_test$higher_25==FALSE & pred_mod1_test$poor_npl1==1
pred_mod1_test$exc_error_30 <- pred_mod1_test$higher_30==FALSE & pred_mod1_test$poor_npl1==1
pred_mod1_test$exc_error_35 <- pred_mod1_test$higher_35==FALSE & pred_mod1_test$poor_npl1==1
pred_mod1_test$exc_error_40 <- pred_mod1_test$higher_40==FALSE & pred_mod1_test$poor_npl1==1
pred_mod1_test$exc_error_45 <- pred_mod1_test$higher_45==FALSE & pred_mod1_test$poor_npl1==1
pred_mod1_test$exc_error_50 <- pred_mod1_test$higher_50==FALSE & pred_mod1_test$poor_npl1==1
pred_mod1_test$exc_error_55 <- pred_mod1_test$higher_55==FALSE & pred_mod1_test$poor_npl1==1
pred_mod1_test$exc_error_60 <- pred_mod1_test$higher_60==FALSE & pred_mod1_test$poor_npl1==1
pred_mod1_test$exc_error_65 <- pred_mod1_test$higher_65==FALSE & pred_mod1_test$poor_npl1==1
pred_mod1_test$exc_error_70 <- pred_mod1_test$higher_70==FALSE & pred_mod1_test$poor_npl1==1
pred_mod1_test$exc_error_75 <- pred_mod1_test$higher_75==FALSE & pred_mod1_test$poor_npl1==1
pred_mod1_test$exc_error_80 <- pred_mod1_test$higher_80==FALSE & pred_mod1_test$poor_npl1==1
pred_mod1_test$exc_error_85 <- pred_mod1_test$higher_85==FALSE & pred_mod1_test$poor_npl1==1
pred_mod1_test$exc_error_90 <- pred_mod1_test$higher_90==FALSE & pred_mod1_test$poor_npl1==1
pred_mod1_test$exc_error_95 <- pred_mod1_test$higher_95==FALSE & pred_mod1_test$poor_npl1==1
pred_mod1_test$exc_error_100 <- pred_mod1_test$higher_100==FALSE & pred_mod1_test$poor_npl1==1

pred_mod1_exc <- c(length(which(pred_mod1_test$exc_error_00==TRUE))/length(which(pred_mod1_test$poor_npl1==1)),
                   length(which(pred_mod1_test$exc_error_05==TRUE))/length(which(pred_mod1_test$poor_npl1==1)),
                   length(which(pred_mod1_test$exc_error_10==TRUE))/length(which(pred_mod1_test$poor_npl1==1)),
                   length(which(pred_mod1_test$exc_error_15==TRUE))/length(which(pred_mod1_test$poor_npl1==1)),
                   length(which(pred_mod1_test$exc_error_20==TRUE))/length(which(pred_mod1_test$poor_npl1==1)),
                   length(which(pred_mod1_test$exc_error_25==TRUE))/length(which(pred_mod1_test$poor_npl1==1)),
                   length(which(pred_mod1_test$exc_error_30==TRUE))/length(which(pred_mod1_test$poor_npl1==1)),
                   length(which(pred_mod1_test$exc_error_35==TRUE))/length(which(pred_mod1_test$poor_npl1==1)),
                   length(which(pred_mod1_test$exc_error_40==TRUE))/length(which(pred_mod1_test$poor_npl1==1)),
                   length(which(pred_mod1_test$exc_error_45==TRUE))/length(which(pred_mod1_test$poor_npl1==1)),
                   length(which(pred_mod1_test$exc_error_50==TRUE))/length(which(pred_mod1_test$poor_npl1==1)),
                   length(which(pred_mod1_test$exc_error_55==TRUE))/length(which(pred_mod1_test$poor_npl1==1)),
                   length(which(pred_mod1_test$exc_error_60==TRUE))/length(which(pred_mod1_test$poor_npl1==1)),
                   length(which(pred_mod1_test$exc_error_65==TRUE))/length(which(pred_mod1_test$poor_npl1==1)),
                   length(which(pred_mod1_test$exc_error_70==TRUE))/length(which(pred_mod1_test$poor_npl1==1)),
                   length(which(pred_mod1_test$exc_error_75==TRUE))/length(which(pred_mod1_test$poor_npl1==1)),
                   length(which(pred_mod1_test$exc_error_80==TRUE))/length(which(pred_mod1_test$poor_npl1==1)),
                   length(which(pred_mod1_test$exc_error_85==TRUE))/length(which(pred_mod1_test$poor_npl1==1)),
                   length(which(pred_mod1_test$exc_error_90==TRUE))/length(which(pred_mod1_test$poor_npl1==1)),
                   length(which(pred_mod1_test$exc_error_95==TRUE))/length(which(pred_mod1_test$poor_npl1==1)),
                   length(which(pred_mod1_test$exc_error_100==TRUE))/length(which(pred_mod1_test$poor_npl1==1)))

pred_mod1_error <- cbind(prob_cutoff, pred_mod1_inc, pred_mod1_exc)

#----2.2. Model 2: EN - LSMS+RWI----
pred_mod2_test <- read.csv("../08 Results/LSMS/Full Datasets/02 LSMS+RWI/Predicted_scores_11q_NPL1_test.csv")
pred_mod2_test <- merge(pred_mod2_test, survey_1, by="hh_id")
pred_mod2_test<-pred_mod2_test%>%
  mutate(urban=case_when(urban==0 ~ "Rural",
                         urban==1 ~ "Urban"),
         region=case_when(region==1 ~ "Amhara",
                          region==2 ~ "Oromiya",
                          region==3 ~ "SNNP",
                          region==4 ~ "Tigray",
                          region==5 ~ "Other regions"),
         urban_poor=case_when(urban=="Rural" & poor_npl1==0 ~ "Rural, Non-Poor",
                              urban=="Rural" & poor_npl1==1 ~ "Rural, Poor",
                              urban=="Urban" & poor_npl1==0 ~ "Urban, Non-Poor",
                              urban=="Urban" & poor_npl1==1 ~ "Urban, Poor"))

pred_mod2_test$higher_00 <- pred_mod2_test$pred_score_pr_test>0.00 
pred_mod2_test$higher_05 <- pred_mod2_test$pred_score_pr_test>0.05 
pred_mod2_test$higher_10 <- pred_mod2_test$pred_score_pr_test>0.10 
pred_mod2_test$higher_15 <- pred_mod2_test$pred_score_pr_test>0.15 
pred_mod2_test$higher_20 <- pred_mod2_test$pred_score_pr_test>0.20 
pred_mod2_test$higher_25 <- pred_mod2_test$pred_score_pr_test>0.25 
pred_mod2_test$higher_30 <- pred_mod2_test$pred_score_pr_test>0.30 
pred_mod2_test$higher_35 <- pred_mod2_test$pred_score_pr_test>0.35 
pred_mod2_test$higher_40 <- pred_mod2_test$pred_score_pr_test>0.40 
pred_mod2_test$higher_45 <- pred_mod2_test$pred_score_pr_test>0.45 
pred_mod2_test$higher_50 <- pred_mod2_test$pred_score_pr_test>0.50 
pred_mod2_test$higher_55 <- pred_mod2_test$pred_score_pr_test>0.55 
pred_mod2_test$higher_60 <- pred_mod2_test$pred_score_pr_test>0.60 
pred_mod2_test$higher_65 <- pred_mod2_test$pred_score_pr_test>0.65 
pred_mod2_test$higher_70 <- pred_mod2_test$pred_score_pr_test>0.70 
pred_mod2_test$higher_75 <- pred_mod2_test$pred_score_pr_test>0.75 
pred_mod2_test$higher_80 <- pred_mod2_test$pred_score_pr_test>0.80 
pred_mod2_test$higher_85 <- pred_mod2_test$pred_score_pr_test>0.85 
pred_mod2_test$higher_90 <- pred_mod2_test$pred_score_pr_test>0.90 
pred_mod2_test$higher_95 <- pred_mod2_test$pred_score_pr_test>0.95 
pred_mod2_test$higher_100 <- pred_mod2_test$pred_score_pr_test>1

pred_mod2_test$inc_error_00 <- pred_mod2_test$higher_00==TRUE & pred_mod2_test$poor_npl1==0
pred_mod2_test$inc_error_05 <- pred_mod2_test$higher_05==TRUE & pred_mod2_test$poor_npl1==0
pred_mod2_test$inc_error_10 <- pred_mod2_test$higher_10==TRUE & pred_mod2_test$poor_npl1==0
pred_mod2_test$inc_error_15 <- pred_mod2_test$higher_15==TRUE & pred_mod2_test$poor_npl1==0
pred_mod2_test$inc_error_20 <- pred_mod2_test$higher_20==TRUE & pred_mod2_test$poor_npl1==0
pred_mod2_test$inc_error_25 <- pred_mod2_test$higher_25==TRUE & pred_mod2_test$poor_npl1==0
pred_mod2_test$inc_error_30 <- pred_mod2_test$higher_30==TRUE & pred_mod2_test$poor_npl1==0
pred_mod2_test$inc_error_35 <- pred_mod2_test$higher_35==TRUE & pred_mod2_test$poor_npl1==0
pred_mod2_test$inc_error_40 <- pred_mod2_test$higher_40==TRUE & pred_mod2_test$poor_npl1==0
pred_mod2_test$inc_error_45 <- pred_mod2_test$higher_45==TRUE & pred_mod2_test$poor_npl1==0
pred_mod2_test$inc_error_50 <- pred_mod2_test$higher_50==TRUE & pred_mod2_test$poor_npl1==0
pred_mod2_test$inc_error_55 <- pred_mod2_test$higher_55==TRUE & pred_mod2_test$poor_npl1==0
pred_mod2_test$inc_error_60 <- pred_mod2_test$higher_60==TRUE & pred_mod2_test$poor_npl1==0
pred_mod2_test$inc_error_65 <- pred_mod2_test$higher_65==TRUE & pred_mod2_test$poor_npl1==0
pred_mod2_test$inc_error_70 <- pred_mod2_test$higher_70==TRUE & pred_mod2_test$poor_npl1==0
pred_mod2_test$inc_error_75 <- pred_mod2_test$higher_75==TRUE & pred_mod2_test$poor_npl1==0
pred_mod2_test$inc_error_80 <- pred_mod2_test$higher_80==TRUE & pred_mod2_test$poor_npl1==0
pred_mod2_test$inc_error_85 <- pred_mod2_test$higher_85==TRUE & pred_mod2_test$poor_npl1==0
pred_mod2_test$inc_error_90 <- pred_mod2_test$higher_90==TRUE & pred_mod2_test$poor_npl1==0
pred_mod2_test$inc_error_95 <- pred_mod2_test$higher_95==TRUE & pred_mod2_test$poor_npl1==0
pred_mod2_test$inc_error_100 <- pred_mod2_test$higher_100==TRUE & pred_mod2_test$poor_npl1==0

pred_mod2_inc <- c(length(which(pred_mod2_test$inc_error_00==TRUE))/length(which(pred_mod2_test$poor_npl1==0)),
                   length(which(pred_mod2_test$inc_error_05==TRUE))/length(which(pred_mod2_test$poor_npl1==0)),
                   length(which(pred_mod2_test$inc_error_10==TRUE))/length(which(pred_mod2_test$poor_npl1==0)),
                   length(which(pred_mod2_test$inc_error_15==TRUE))/length(which(pred_mod2_test$poor_npl1==0)),
                   length(which(pred_mod2_test$inc_error_20==TRUE))/length(which(pred_mod2_test$poor_npl1==0)),
                   length(which(pred_mod2_test$inc_error_25==TRUE))/length(which(pred_mod2_test$poor_npl1==0)),
                   length(which(pred_mod2_test$inc_error_30==TRUE))/length(which(pred_mod2_test$poor_npl1==0)),
                   length(which(pred_mod2_test$inc_error_35==TRUE))/length(which(pred_mod2_test$poor_npl1==0)),
                   length(which(pred_mod2_test$inc_error_40==TRUE))/length(which(pred_mod2_test$poor_npl1==0)),
                   length(which(pred_mod2_test$inc_error_45==TRUE))/length(which(pred_mod2_test$poor_npl1==0)),
                   length(which(pred_mod2_test$inc_error_50==TRUE))/length(which(pred_mod2_test$poor_npl1==0)),
                   length(which(pred_mod2_test$inc_error_55==TRUE))/length(which(pred_mod2_test$poor_npl1==0)),
                   length(which(pred_mod2_test$inc_error_60==TRUE))/length(which(pred_mod2_test$poor_npl1==0)),
                   length(which(pred_mod2_test$inc_error_65==TRUE))/length(which(pred_mod2_test$poor_npl1==0)),
                   length(which(pred_mod2_test$inc_error_70==TRUE))/length(which(pred_mod2_test$poor_npl1==0)),
                   length(which(pred_mod2_test$inc_error_75==TRUE))/length(which(pred_mod2_test$poor_npl1==0)),
                   length(which(pred_mod2_test$inc_error_80==TRUE))/length(which(pred_mod2_test$poor_npl1==0)),
                   length(which(pred_mod2_test$inc_error_85==TRUE))/length(which(pred_mod2_test$poor_npl1==0)),
                   length(which(pred_mod2_test$inc_error_90==TRUE))/length(which(pred_mod2_test$poor_npl1==0)),
                   length(which(pred_mod2_test$inc_error_95==TRUE))/length(which(pred_mod2_test$poor_npl1==0)),
                   length(which(pred_mod2_test$inc_error_100==TRUE))/length(which(pred_mod2_test$poor_npl1==0)))

pred_mod2_test$exc_error_00 <- pred_mod2_test$higher_00==FALSE & pred_mod2_test$poor_npl1==1
pred_mod2_test$exc_error_05 <- pred_mod2_test$higher_05==FALSE & pred_mod2_test$poor_npl1==1
pred_mod2_test$exc_error_10 <- pred_mod2_test$higher_10==FALSE & pred_mod2_test$poor_npl1==1
pred_mod2_test$exc_error_15 <- pred_mod2_test$higher_15==FALSE & pred_mod2_test$poor_npl1==1
pred_mod2_test$exc_error_20 <- pred_mod2_test$higher_20==FALSE & pred_mod2_test$poor_npl1==1
pred_mod2_test$exc_error_25 <- pred_mod2_test$higher_25==FALSE & pred_mod2_test$poor_npl1==1
pred_mod2_test$exc_error_30 <- pred_mod2_test$higher_30==FALSE & pred_mod2_test$poor_npl1==1
pred_mod2_test$exc_error_35 <- pred_mod2_test$higher_35==FALSE & pred_mod2_test$poor_npl1==1
pred_mod2_test$exc_error_40 <- pred_mod2_test$higher_40==FALSE & pred_mod2_test$poor_npl1==1
pred_mod2_test$exc_error_45 <- pred_mod2_test$higher_45==FALSE & pred_mod2_test$poor_npl1==1
pred_mod2_test$exc_error_50 <- pred_mod2_test$higher_50==FALSE & pred_mod2_test$poor_npl1==1
pred_mod2_test$exc_error_55 <- pred_mod2_test$higher_55==FALSE & pred_mod2_test$poor_npl1==1
pred_mod2_test$exc_error_60 <- pred_mod2_test$higher_60==FALSE & pred_mod2_test$poor_npl1==1
pred_mod2_test$exc_error_65 <- pred_mod2_test$higher_65==FALSE & pred_mod2_test$poor_npl1==1
pred_mod2_test$exc_error_70 <- pred_mod2_test$higher_70==FALSE & pred_mod2_test$poor_npl1==1
pred_mod2_test$exc_error_75 <- pred_mod2_test$higher_75==FALSE & pred_mod2_test$poor_npl1==1
pred_mod2_test$exc_error_80 <- pred_mod2_test$higher_80==FALSE & pred_mod2_test$poor_npl1==1
pred_mod2_test$exc_error_85 <- pred_mod2_test$higher_85==FALSE & pred_mod2_test$poor_npl1==1
pred_mod2_test$exc_error_90 <- pred_mod2_test$higher_90==FALSE & pred_mod2_test$poor_npl1==1
pred_mod2_test$exc_error_95 <- pred_mod2_test$higher_95==FALSE & pred_mod2_test$poor_npl1==1
pred_mod2_test$exc_error_100 <- pred_mod2_test$higher_100==FALSE & pred_mod2_test$poor_npl1==1

pred_mod2_exc <- c(length(which(pred_mod2_test$exc_error_00==TRUE))/length(which(pred_mod2_test$poor_npl1==1)),
                   length(which(pred_mod2_test$exc_error_05==TRUE))/length(which(pred_mod2_test$poor_npl1==1)),
                   length(which(pred_mod2_test$exc_error_10==TRUE))/length(which(pred_mod2_test$poor_npl1==1)),
                   length(which(pred_mod2_test$exc_error_15==TRUE))/length(which(pred_mod2_test$poor_npl1==1)),
                   length(which(pred_mod2_test$exc_error_20==TRUE))/length(which(pred_mod2_test$poor_npl1==1)),
                   length(which(pred_mod2_test$exc_error_25==TRUE))/length(which(pred_mod2_test$poor_npl1==1)),
                   length(which(pred_mod2_test$exc_error_30==TRUE))/length(which(pred_mod2_test$poor_npl1==1)),
                   length(which(pred_mod2_test$exc_error_35==TRUE))/length(which(pred_mod2_test$poor_npl1==1)),
                   length(which(pred_mod2_test$exc_error_40==TRUE))/length(which(pred_mod2_test$poor_npl1==1)),
                   length(which(pred_mod2_test$exc_error_45==TRUE))/length(which(pred_mod2_test$poor_npl1==1)),
                   length(which(pred_mod2_test$exc_error_50==TRUE))/length(which(pred_mod2_test$poor_npl1==1)),
                   length(which(pred_mod2_test$exc_error_55==TRUE))/length(which(pred_mod2_test$poor_npl1==1)),
                   length(which(pred_mod2_test$exc_error_60==TRUE))/length(which(pred_mod2_test$poor_npl1==1)),
                   length(which(pred_mod2_test$exc_error_65==TRUE))/length(which(pred_mod2_test$poor_npl1==1)),
                   length(which(pred_mod2_test$exc_error_70==TRUE))/length(which(pred_mod2_test$poor_npl1==1)),
                   length(which(pred_mod2_test$exc_error_75==TRUE))/length(which(pred_mod2_test$poor_npl1==1)),
                   length(which(pred_mod2_test$exc_error_80==TRUE))/length(which(pred_mod2_test$poor_npl1==1)),
                   length(which(pred_mod2_test$exc_error_85==TRUE))/length(which(pred_mod2_test$poor_npl1==1)),
                   length(which(pred_mod2_test$exc_error_90==TRUE))/length(which(pred_mod2_test$poor_npl1==1)),
                   length(which(pred_mod2_test$exc_error_95==TRUE))/length(which(pred_mod2_test$poor_npl1==1)),
                   length(which(pred_mod2_test$exc_error_100==TRUE))/length(which(pred_mod2_test$poor_npl1==1)))

pred_mod2_error <- cbind(prob_cutoff, pred_mod2_inc, pred_mod2_exc)

#----2.3. Model 3: EBM - LSMS----
pred_mod3_test <- read.csv("../08 Results/LSMS/Full Datasets/03 EBM LSMS/Test_predicted_probability_surveyimageguided_False_surveyonly_True_interactions_False.csv")
pred_mod3_test <- merge(pred_mod3_test, survey_2, by="household_id2")
pred_mod3_test<-pred_mod3_test%>%
  mutate(urban=case_when(urban==0 ~ "Rural",
                         urban==1 ~ "Urban"),
         region=case_when(region==1 ~ "Amhara",
                          region==2 ~ "Oromiya",
                          region==3 ~ "SNNP",
                          region==4 ~ "Tigray",
                          region==5 ~ "Other regions"),
         urban_poor=case_when(urban=="Rural" & poor_npl1==0 ~ "Rural, Non-Poor",
                              urban=="Rural" & poor_npl1==1 ~ "Rural, Poor",
                              urban=="Urban" & poor_npl1==0 ~ "Urban, Non-Poor",
                              urban=="Urban" & poor_npl1==1 ~ "Urban, Poor"))

pred_mod3_test$higher_00 <- pred_mod3_test$predicted_probs>0.00 
pred_mod3_test$higher_05 <- pred_mod3_test$predicted_probs>0.05 
pred_mod3_test$higher_10 <- pred_mod3_test$predicted_probs>0.10 
pred_mod3_test$higher_15 <- pred_mod3_test$predicted_probs>0.15 
pred_mod3_test$higher_20 <- pred_mod3_test$predicted_probs>0.20 
pred_mod3_test$higher_25 <- pred_mod3_test$predicted_probs>0.25 
pred_mod3_test$higher_30 <- pred_mod3_test$predicted_probs>0.30 
pred_mod3_test$higher_35 <- pred_mod3_test$predicted_probs>0.35 
pred_mod3_test$higher_40 <- pred_mod3_test$predicted_probs>0.40 
pred_mod3_test$higher_45 <- pred_mod3_test$predicted_probs>0.45 
pred_mod3_test$higher_50 <- pred_mod3_test$predicted_probs>0.50 
pred_mod3_test$higher_55 <- pred_mod3_test$predicted_probs>0.55 
pred_mod3_test$higher_60 <- pred_mod3_test$predicted_probs>0.60 
pred_mod3_test$higher_65 <- pred_mod3_test$predicted_probs>0.65 
pred_mod3_test$higher_70 <- pred_mod3_test$predicted_probs>0.70 
pred_mod3_test$higher_75 <- pred_mod3_test$predicted_probs>0.75 
pred_mod3_test$higher_80 <- pred_mod3_test$predicted_probs>0.80 
pred_mod3_test$higher_85 <- pred_mod3_test$predicted_probs>0.85 
pred_mod3_test$higher_90 <- pred_mod3_test$predicted_probs>0.90 
pred_mod3_test$higher_95 <- pred_mod3_test$predicted_probs>0.95 
pred_mod3_test$higher_100 <- pred_mod3_test$predicted_probs>1

pred_mod3_test$inc_error_00 <- pred_mod3_test$higher_00==TRUE & pred_mod3_test$poor_npl1==0
pred_mod3_test$inc_error_05 <- pred_mod3_test$higher_05==TRUE & pred_mod3_test$poor_npl1==0
pred_mod3_test$inc_error_10 <- pred_mod3_test$higher_10==TRUE & pred_mod3_test$poor_npl1==0
pred_mod3_test$inc_error_15 <- pred_mod3_test$higher_15==TRUE & pred_mod3_test$poor_npl1==0
pred_mod3_test$inc_error_20 <- pred_mod3_test$higher_20==TRUE & pred_mod3_test$poor_npl1==0
pred_mod3_test$inc_error_25 <- pred_mod3_test$higher_25==TRUE & pred_mod3_test$poor_npl1==0
pred_mod3_test$inc_error_30 <- pred_mod3_test$higher_30==TRUE & pred_mod3_test$poor_npl1==0
pred_mod3_test$inc_error_35 <- pred_mod3_test$higher_35==TRUE & pred_mod3_test$poor_npl1==0
pred_mod3_test$inc_error_40 <- pred_mod3_test$higher_40==TRUE & pred_mod3_test$poor_npl1==0
pred_mod3_test$inc_error_45 <- pred_mod3_test$higher_45==TRUE & pred_mod3_test$poor_npl1==0
pred_mod3_test$inc_error_50 <- pred_mod3_test$higher_50==TRUE & pred_mod3_test$poor_npl1==0
pred_mod3_test$inc_error_55 <- pred_mod3_test$higher_55==TRUE & pred_mod3_test$poor_npl1==0
pred_mod3_test$inc_error_60 <- pred_mod3_test$higher_60==TRUE & pred_mod3_test$poor_npl1==0
pred_mod3_test$inc_error_65 <- pred_mod3_test$higher_65==TRUE & pred_mod3_test$poor_npl1==0
pred_mod3_test$inc_error_70 <- pred_mod3_test$higher_70==TRUE & pred_mod3_test$poor_npl1==0
pred_mod3_test$inc_error_75 <- pred_mod3_test$higher_75==TRUE & pred_mod3_test$poor_npl1==0
pred_mod3_test$inc_error_80 <- pred_mod3_test$higher_80==TRUE & pred_mod3_test$poor_npl1==0
pred_mod3_test$inc_error_85 <- pred_mod3_test$higher_85==TRUE & pred_mod3_test$poor_npl1==0
pred_mod3_test$inc_error_90 <- pred_mod3_test$higher_90==TRUE & pred_mod3_test$poor_npl1==0
pred_mod3_test$inc_error_95 <- pred_mod3_test$higher_95==TRUE & pred_mod3_test$poor_npl1==0
pred_mod3_test$inc_error_100 <- pred_mod3_test$higher_100==TRUE & pred_mod3_test$poor_npl1==0

pred_mod3_inc <- c(length(which(pred_mod3_test$inc_error_00==TRUE))/length(which(pred_mod3_test$poor_npl1==0)),
                   length(which(pred_mod3_test$inc_error_05==TRUE))/length(which(pred_mod3_test$poor_npl1==0)),
                   length(which(pred_mod3_test$inc_error_10==TRUE))/length(which(pred_mod3_test$poor_npl1==0)),
                   length(which(pred_mod3_test$inc_error_15==TRUE))/length(which(pred_mod3_test$poor_npl1==0)),
                   length(which(pred_mod3_test$inc_error_20==TRUE))/length(which(pred_mod3_test$poor_npl1==0)),
                   length(which(pred_mod3_test$inc_error_25==TRUE))/length(which(pred_mod3_test$poor_npl1==0)),
                   length(which(pred_mod3_test$inc_error_30==TRUE))/length(which(pred_mod3_test$poor_npl1==0)),
                   length(which(pred_mod3_test$inc_error_35==TRUE))/length(which(pred_mod3_test$poor_npl1==0)),
                   length(which(pred_mod3_test$inc_error_40==TRUE))/length(which(pred_mod3_test$poor_npl1==0)),
                   length(which(pred_mod3_test$inc_error_45==TRUE))/length(which(pred_mod3_test$poor_npl1==0)),
                   length(which(pred_mod3_test$inc_error_50==TRUE))/length(which(pred_mod3_test$poor_npl1==0)),
                   length(which(pred_mod3_test$inc_error_55==TRUE))/length(which(pred_mod3_test$poor_npl1==0)),
                   length(which(pred_mod3_test$inc_error_60==TRUE))/length(which(pred_mod3_test$poor_npl1==0)),
                   length(which(pred_mod3_test$inc_error_65==TRUE))/length(which(pred_mod3_test$poor_npl1==0)),
                   length(which(pred_mod3_test$inc_error_70==TRUE))/length(which(pred_mod3_test$poor_npl1==0)),
                   length(which(pred_mod3_test$inc_error_75==TRUE))/length(which(pred_mod3_test$poor_npl1==0)),
                   length(which(pred_mod3_test$inc_error_80==TRUE))/length(which(pred_mod3_test$poor_npl1==0)),
                   length(which(pred_mod3_test$inc_error_85==TRUE))/length(which(pred_mod3_test$poor_npl1==0)),
                   length(which(pred_mod3_test$inc_error_90==TRUE))/length(which(pred_mod3_test$poor_npl1==0)),
                   length(which(pred_mod3_test$inc_error_95==TRUE))/length(which(pred_mod3_test$poor_npl1==0)),
                   length(which(pred_mod3_test$inc_error_100==TRUE))/length(which(pred_mod3_test$poor_npl1==0)))

pred_mod3_test$exc_error_00 <- pred_mod3_test$higher_00==FALSE & pred_mod3_test$poor_npl1==1
pred_mod3_test$exc_error_05 <- pred_mod3_test$higher_05==FALSE & pred_mod3_test$poor_npl1==1
pred_mod3_test$exc_error_10 <- pred_mod3_test$higher_10==FALSE & pred_mod3_test$poor_npl1==1
pred_mod3_test$exc_error_15 <- pred_mod3_test$higher_15==FALSE & pred_mod3_test$poor_npl1==1
pred_mod3_test$exc_error_20 <- pred_mod3_test$higher_20==FALSE & pred_mod3_test$poor_npl1==1
pred_mod3_test$exc_error_25 <- pred_mod3_test$higher_25==FALSE & pred_mod3_test$poor_npl1==1
pred_mod3_test$exc_error_30 <- pred_mod3_test$higher_30==FALSE & pred_mod3_test$poor_npl1==1
pred_mod3_test$exc_error_35 <- pred_mod3_test$higher_35==FALSE & pred_mod3_test$poor_npl1==1
pred_mod3_test$exc_error_40 <- pred_mod3_test$higher_40==FALSE & pred_mod3_test$poor_npl1==1
pred_mod3_test$exc_error_45 <- pred_mod3_test$higher_45==FALSE & pred_mod3_test$poor_npl1==1
pred_mod3_test$exc_error_50 <- pred_mod3_test$higher_50==FALSE & pred_mod3_test$poor_npl1==1
pred_mod3_test$exc_error_55 <- pred_mod3_test$higher_55==FALSE & pred_mod3_test$poor_npl1==1
pred_mod3_test$exc_error_60 <- pred_mod3_test$higher_60==FALSE & pred_mod3_test$poor_npl1==1
pred_mod3_test$exc_error_65 <- pred_mod3_test$higher_65==FALSE & pred_mod3_test$poor_npl1==1
pred_mod3_test$exc_error_70 <- pred_mod3_test$higher_70==FALSE & pred_mod3_test$poor_npl1==1
pred_mod3_test$exc_error_75 <- pred_mod3_test$higher_75==FALSE & pred_mod3_test$poor_npl1==1
pred_mod3_test$exc_error_80 <- pred_mod3_test$higher_80==FALSE & pred_mod3_test$poor_npl1==1
pred_mod3_test$exc_error_85 <- pred_mod3_test$higher_85==FALSE & pred_mod3_test$poor_npl1==1
pred_mod3_test$exc_error_90 <- pred_mod3_test$higher_90==FALSE & pred_mod3_test$poor_npl1==1
pred_mod3_test$exc_error_95 <- pred_mod3_test$higher_95==FALSE & pred_mod3_test$poor_npl1==1
pred_mod3_test$exc_error_100 <- pred_mod3_test$higher_100==FALSE & pred_mod3_test$poor_npl1==1

pred_mod3_exc <- c(length(which(pred_mod3_test$exc_error_00==TRUE))/length(which(pred_mod3_test$poor_npl1==1)),
                   length(which(pred_mod3_test$exc_error_05==TRUE))/length(which(pred_mod3_test$poor_npl1==1)),
                   length(which(pred_mod3_test$exc_error_10==TRUE))/length(which(pred_mod3_test$poor_npl1==1)),
                   length(which(pred_mod3_test$exc_error_15==TRUE))/length(which(pred_mod3_test$poor_npl1==1)),
                   length(which(pred_mod3_test$exc_error_20==TRUE))/length(which(pred_mod3_test$poor_npl1==1)),
                   length(which(pred_mod3_test$exc_error_25==TRUE))/length(which(pred_mod3_test$poor_npl1==1)),
                   length(which(pred_mod3_test$exc_error_30==TRUE))/length(which(pred_mod3_test$poor_npl1==1)),
                   length(which(pred_mod3_test$exc_error_35==TRUE))/length(which(pred_mod3_test$poor_npl1==1)),
                   length(which(pred_mod3_test$exc_error_40==TRUE))/length(which(pred_mod3_test$poor_npl1==1)),
                   length(which(pred_mod3_test$exc_error_45==TRUE))/length(which(pred_mod3_test$poor_npl1==1)),
                   length(which(pred_mod3_test$exc_error_50==TRUE))/length(which(pred_mod3_test$poor_npl1==1)),
                   length(which(pred_mod3_test$exc_error_55==TRUE))/length(which(pred_mod3_test$poor_npl1==1)),
                   length(which(pred_mod3_test$exc_error_60==TRUE))/length(which(pred_mod3_test$poor_npl1==1)),
                   length(which(pred_mod3_test$exc_error_65==TRUE))/length(which(pred_mod3_test$poor_npl1==1)),
                   length(which(pred_mod3_test$exc_error_70==TRUE))/length(which(pred_mod3_test$poor_npl1==1)),
                   length(which(pred_mod3_test$exc_error_75==TRUE))/length(which(pred_mod3_test$poor_npl1==1)),
                   length(which(pred_mod3_test$exc_error_80==TRUE))/length(which(pred_mod3_test$poor_npl1==1)),
                   length(which(pred_mod3_test$exc_error_85==TRUE))/length(which(pred_mod3_test$poor_npl1==1)),
                   length(which(pred_mod3_test$exc_error_90==TRUE))/length(which(pred_mod3_test$poor_npl1==1)),
                   length(which(pred_mod3_test$exc_error_95==TRUE))/length(which(pred_mod3_test$poor_npl1==1)),
                   length(which(pred_mod3_test$exc_error_100==TRUE))/length(which(pred_mod3_test$poor_npl1==1)))

pred_mod3_error <- cbind(prob_cutoff, pred_mod3_inc, pred_mod3_exc)

#----2.4. Model 4: EBM - MOSAIKS----
pred_mod4_test <- read.csv("../08 Results/LSMS/Full Datasets/04 EBM MOSAIKS/Test_predicted_probability_mosaiks128_buffer_1000.csv")
pred_mod4_test <- merge(pred_mod4_test, survey_2, by="household_id2")
pred_mod4_test<-pred_mod4_test%>%
  mutate(urban=case_when(urban==0 ~ "Rural",
                         urban==1 ~ "Urban"),
         region=case_when(region==1 ~ "Amhara",
                          region==2 ~ "Oromiya",
                          region==3 ~ "SNNP",
                          region==4 ~ "Tigray",
                          region==5 ~ "Other regions"),
         urban_poor=case_when(urban=="Rural" & poor_npl1==0 ~ "Rural, Non-Poor",
                              urban=="Rural" & poor_npl1==1 ~ "Rural, Poor",
                              urban=="Urban" & poor_npl1==0 ~ "Urban, Non-Poor",
                              urban=="Urban" & poor_npl1==1 ~ "Urban, Poor"))


pred_mod4_test$higher_00 <- pred_mod4_test$predicted_probs>0.00 
pred_mod4_test$higher_05 <- pred_mod4_test$predicted_probs>0.05 
pred_mod4_test$higher_10 <- pred_mod4_test$predicted_probs>0.10 
pred_mod4_test$higher_15 <- pred_mod4_test$predicted_probs>0.15 
pred_mod4_test$higher_20 <- pred_mod4_test$predicted_probs>0.20 
pred_mod4_test$higher_25 <- pred_mod4_test$predicted_probs>0.25 
pred_mod4_test$higher_30 <- pred_mod4_test$predicted_probs>0.30 
pred_mod4_test$higher_35 <- pred_mod4_test$predicted_probs>0.35 
pred_mod4_test$higher_40 <- pred_mod4_test$predicted_probs>0.40 
pred_mod4_test$higher_45 <- pred_mod4_test$predicted_probs>0.45 
pred_mod4_test$higher_50 <- pred_mod4_test$predicted_probs>0.50 
pred_mod4_test$higher_55 <- pred_mod4_test$predicted_probs>0.55 
pred_mod4_test$higher_60 <- pred_mod4_test$predicted_probs>0.60 
pred_mod4_test$higher_65 <- pred_mod4_test$predicted_probs>0.65 
pred_mod4_test$higher_70 <- pred_mod4_test$predicted_probs>0.70 
pred_mod4_test$higher_75 <- pred_mod4_test$predicted_probs>0.75 
pred_mod4_test$higher_80 <- pred_mod4_test$predicted_probs>0.80 
pred_mod4_test$higher_85 <- pred_mod4_test$predicted_probs>0.85 
pred_mod4_test$higher_90 <- pred_mod4_test$predicted_probs>0.90 
pred_mod4_test$higher_95 <- pred_mod4_test$predicted_probs>0.95 
pred_mod4_test$higher_100 <- pred_mod4_test$predicted_probs>1

pred_mod4_test$inc_error_00 <- pred_mod4_test$higher_00==TRUE & pred_mod4_test$poor_npl1==0
pred_mod4_test$inc_error_05 <- pred_mod4_test$higher_05==TRUE & pred_mod4_test$poor_npl1==0
pred_mod4_test$inc_error_10 <- pred_mod4_test$higher_10==TRUE & pred_mod4_test$poor_npl1==0
pred_mod4_test$inc_error_15 <- pred_mod4_test$higher_15==TRUE & pred_mod4_test$poor_npl1==0
pred_mod4_test$inc_error_20 <- pred_mod4_test$higher_20==TRUE & pred_mod4_test$poor_npl1==0
pred_mod4_test$inc_error_25 <- pred_mod4_test$higher_25==TRUE & pred_mod4_test$poor_npl1==0
pred_mod4_test$inc_error_30 <- pred_mod4_test$higher_30==TRUE & pred_mod4_test$poor_npl1==0
pred_mod4_test$inc_error_35 <- pred_mod4_test$higher_35==TRUE & pred_mod4_test$poor_npl1==0
pred_mod4_test$inc_error_40 <- pred_mod4_test$higher_40==TRUE & pred_mod4_test$poor_npl1==0
pred_mod4_test$inc_error_45 <- pred_mod4_test$higher_45==TRUE & pred_mod4_test$poor_npl1==0
pred_mod4_test$inc_error_50 <- pred_mod4_test$higher_50==TRUE & pred_mod4_test$poor_npl1==0
pred_mod4_test$inc_error_55 <- pred_mod4_test$higher_55==TRUE & pred_mod4_test$poor_npl1==0
pred_mod4_test$inc_error_60 <- pred_mod4_test$higher_60==TRUE & pred_mod4_test$poor_npl1==0
pred_mod4_test$inc_error_65 <- pred_mod4_test$higher_65==TRUE & pred_mod4_test$poor_npl1==0
pred_mod4_test$inc_error_70 <- pred_mod4_test$higher_70==TRUE & pred_mod4_test$poor_npl1==0
pred_mod4_test$inc_error_75 <- pred_mod4_test$higher_75==TRUE & pred_mod4_test$poor_npl1==0
pred_mod4_test$inc_error_80 <- pred_mod4_test$higher_80==TRUE & pred_mod4_test$poor_npl1==0
pred_mod4_test$inc_error_85 <- pred_mod4_test$higher_85==TRUE & pred_mod4_test$poor_npl1==0
pred_mod4_test$inc_error_90 <- pred_mod4_test$higher_90==TRUE & pred_mod4_test$poor_npl1==0
pred_mod4_test$inc_error_95 <- pred_mod4_test$higher_95==TRUE & pred_mod4_test$poor_npl1==0
pred_mod4_test$inc_error_100 <- pred_mod4_test$higher_100==TRUE & pred_mod4_test$poor_npl1==0

pred_mod4_inc <- c(length(which(pred_mod4_test$inc_error_00==TRUE))/length(which(pred_mod4_test$poor_npl1==0)),
                   length(which(pred_mod4_test$inc_error_05==TRUE))/length(which(pred_mod4_test$poor_npl1==0)),
                   length(which(pred_mod4_test$inc_error_10==TRUE))/length(which(pred_mod4_test$poor_npl1==0)),
                   length(which(pred_mod4_test$inc_error_15==TRUE))/length(which(pred_mod4_test$poor_npl1==0)),
                   length(which(pred_mod4_test$inc_error_20==TRUE))/length(which(pred_mod4_test$poor_npl1==0)),
                   length(which(pred_mod4_test$inc_error_25==TRUE))/length(which(pred_mod4_test$poor_npl1==0)),
                   length(which(pred_mod4_test$inc_error_30==TRUE))/length(which(pred_mod4_test$poor_npl1==0)),
                   length(which(pred_mod4_test$inc_error_35==TRUE))/length(which(pred_mod4_test$poor_npl1==0)),
                   length(which(pred_mod4_test$inc_error_40==TRUE))/length(which(pred_mod4_test$poor_npl1==0)),
                   length(which(pred_mod4_test$inc_error_45==TRUE))/length(which(pred_mod4_test$poor_npl1==0)),
                   length(which(pred_mod4_test$inc_error_50==TRUE))/length(which(pred_mod4_test$poor_npl1==0)),
                   length(which(pred_mod4_test$inc_error_55==TRUE))/length(which(pred_mod4_test$poor_npl1==0)),
                   length(which(pred_mod4_test$inc_error_60==TRUE))/length(which(pred_mod4_test$poor_npl1==0)),
                   length(which(pred_mod4_test$inc_error_65==TRUE))/length(which(pred_mod4_test$poor_npl1==0)),
                   length(which(pred_mod4_test$inc_error_70==TRUE))/length(which(pred_mod4_test$poor_npl1==0)),
                   length(which(pred_mod4_test$inc_error_75==TRUE))/length(which(pred_mod4_test$poor_npl1==0)),
                   length(which(pred_mod4_test$inc_error_80==TRUE))/length(which(pred_mod4_test$poor_npl1==0)),
                   length(which(pred_mod4_test$inc_error_85==TRUE))/length(which(pred_mod4_test$poor_npl1==0)),
                   length(which(pred_mod4_test$inc_error_90==TRUE))/length(which(pred_mod4_test$poor_npl1==0)),
                   length(which(pred_mod4_test$inc_error_95==TRUE))/length(which(pred_mod4_test$poor_npl1==0)),
                   length(which(pred_mod4_test$inc_error_100==TRUE))/length(which(pred_mod4_test$poor_npl1==0)))

pred_mod4_test$exc_error_00 <- pred_mod4_test$higher_00==FALSE & pred_mod4_test$poor_npl1==1
pred_mod4_test$exc_error_05 <- pred_mod4_test$higher_05==FALSE & pred_mod4_test$poor_npl1==1
pred_mod4_test$exc_error_10 <- pred_mod4_test$higher_10==FALSE & pred_mod4_test$poor_npl1==1
pred_mod4_test$exc_error_15 <- pred_mod4_test$higher_15==FALSE & pred_mod4_test$poor_npl1==1
pred_mod4_test$exc_error_20 <- pred_mod4_test$higher_20==FALSE & pred_mod4_test$poor_npl1==1
pred_mod4_test$exc_error_25 <- pred_mod4_test$higher_25==FALSE & pred_mod4_test$poor_npl1==1
pred_mod4_test$exc_error_30 <- pred_mod4_test$higher_30==FALSE & pred_mod4_test$poor_npl1==1
pred_mod4_test$exc_error_35 <- pred_mod4_test$higher_35==FALSE & pred_mod4_test$poor_npl1==1
pred_mod4_test$exc_error_40 <- pred_mod4_test$higher_40==FALSE & pred_mod4_test$poor_npl1==1
pred_mod4_test$exc_error_45 <- pred_mod4_test$higher_45==FALSE & pred_mod4_test$poor_npl1==1
pred_mod4_test$exc_error_50 <- pred_mod4_test$higher_50==FALSE & pred_mod4_test$poor_npl1==1
pred_mod4_test$exc_error_55 <- pred_mod4_test$higher_55==FALSE & pred_mod4_test$poor_npl1==1
pred_mod4_test$exc_error_60 <- pred_mod4_test$higher_60==FALSE & pred_mod4_test$poor_npl1==1
pred_mod4_test$exc_error_65 <- pred_mod4_test$higher_65==FALSE & pred_mod4_test$poor_npl1==1
pred_mod4_test$exc_error_70 <- pred_mod4_test$higher_70==FALSE & pred_mod4_test$poor_npl1==1
pred_mod4_test$exc_error_75 <- pred_mod4_test$higher_75==FALSE & pred_mod4_test$poor_npl1==1
pred_mod4_test$exc_error_80 <- pred_mod4_test$higher_80==FALSE & pred_mod4_test$poor_npl1==1
pred_mod4_test$exc_error_85 <- pred_mod4_test$higher_85==FALSE & pred_mod4_test$poor_npl1==1
pred_mod4_test$exc_error_90 <- pred_mod4_test$higher_90==FALSE & pred_mod4_test$poor_npl1==1
pred_mod4_test$exc_error_95 <- pred_mod4_test$higher_95==FALSE & pred_mod4_test$poor_npl1==1
pred_mod4_test$exc_error_100 <- pred_mod4_test$higher_100==FALSE & pred_mod4_test$poor_npl1==1

pred_mod4_exc <- c(length(which(pred_mod4_test$exc_error_00==TRUE))/length(which(pred_mod4_test$poor_npl1==1)),
                   length(which(pred_mod4_test$exc_error_05==TRUE))/length(which(pred_mod4_test$poor_npl1==1)),
                   length(which(pred_mod4_test$exc_error_10==TRUE))/length(which(pred_mod4_test$poor_npl1==1)),
                   length(which(pred_mod4_test$exc_error_15==TRUE))/length(which(pred_mod4_test$poor_npl1==1)),
                   length(which(pred_mod4_test$exc_error_20==TRUE))/length(which(pred_mod4_test$poor_npl1==1)),
                   length(which(pred_mod4_test$exc_error_25==TRUE))/length(which(pred_mod4_test$poor_npl1==1)),
                   length(which(pred_mod4_test$exc_error_30==TRUE))/length(which(pred_mod4_test$poor_npl1==1)),
                   length(which(pred_mod4_test$exc_error_35==TRUE))/length(which(pred_mod4_test$poor_npl1==1)),
                   length(which(pred_mod4_test$exc_error_40==TRUE))/length(which(pred_mod4_test$poor_npl1==1)),
                   length(which(pred_mod4_test$exc_error_45==TRUE))/length(which(pred_mod4_test$poor_npl1==1)),
                   length(which(pred_mod4_test$exc_error_50==TRUE))/length(which(pred_mod4_test$poor_npl1==1)),
                   length(which(pred_mod4_test$exc_error_55==TRUE))/length(which(pred_mod4_test$poor_npl1==1)),
                   length(which(pred_mod4_test$exc_error_60==TRUE))/length(which(pred_mod4_test$poor_npl1==1)),
                   length(which(pred_mod4_test$exc_error_65==TRUE))/length(which(pred_mod4_test$poor_npl1==1)),
                   length(which(pred_mod4_test$exc_error_70==TRUE))/length(which(pred_mod4_test$poor_npl1==1)),
                   length(which(pred_mod4_test$exc_error_75==TRUE))/length(which(pred_mod4_test$poor_npl1==1)),
                   length(which(pred_mod4_test$exc_error_80==TRUE))/length(which(pred_mod4_test$poor_npl1==1)),
                   length(which(pred_mod4_test$exc_error_85==TRUE))/length(which(pred_mod4_test$poor_npl1==1)),
                   length(which(pred_mod4_test$exc_error_90==TRUE))/length(which(pred_mod4_test$poor_npl1==1)),
                   length(which(pred_mod4_test$exc_error_95==TRUE))/length(which(pred_mod4_test$poor_npl1==1)),
                   length(which(pred_mod4_test$exc_error_100==TRUE))/length(which(pred_mod4_test$poor_npl1==1)))

pred_mod4_error <- cbind(prob_cutoff, pred_mod4_inc, pred_mod4_exc)

#----2.5. Model 5: EBM - LSMS+MOSAIKS----
pred_mod5_test <- read.csv("../08 Results/LSMS/Full Datasets/05 EBM LSMS+MOSAIKS/Test_predicted_probability_surveyimageguided_True_surveyonly_False_interactions_True.csv")
pred_mod5_test <- merge(pred_mod5_test, survey_2, by="household_id2")
pred_mod5_test<-pred_mod5_test%>%
  mutate(urban=case_when(urban==0 ~ "Rural",
                         urban==1 ~ "Urban"),
         region=case_when(region==1 ~ "Amhara",
                          region==2 ~ "Oromiya",
                          region==3 ~ "SNNP",
                          region==4 ~ "Tigray",
                          region==5 ~ "Other regions"),
         urban_poor=case_when(urban=="Rural" & poor_npl1==0 ~ "Rural, Non-Poor",
                              urban=="Rural" & poor_npl1==1 ~ "Rural, Poor",
                              urban=="Urban" & poor_npl1==0 ~ "Urban, Non-Poor",
                              urban=="Urban" & poor_npl1==1 ~ "Urban, Poor"))

pred_mod5_test$higher_00 <- pred_mod5_test$predicted_probs>0.00 
pred_mod5_test$higher_05 <- pred_mod5_test$predicted_probs>0.05 
pred_mod5_test$higher_10 <- pred_mod5_test$predicted_probs>0.10 
pred_mod5_test$higher_15 <- pred_mod5_test$predicted_probs>0.15 
pred_mod5_test$higher_20 <- pred_mod5_test$predicted_probs>0.20 
pred_mod5_test$higher_25 <- pred_mod5_test$predicted_probs>0.25 
pred_mod5_test$higher_30 <- pred_mod5_test$predicted_probs>0.30 
pred_mod5_test$higher_35 <- pred_mod5_test$predicted_probs>0.35 
pred_mod5_test$higher_40 <- pred_mod5_test$predicted_probs>0.40 
pred_mod5_test$higher_45 <- pred_mod5_test$predicted_probs>0.45 
pred_mod5_test$higher_50 <- pred_mod5_test$predicted_probs>0.50 
pred_mod5_test$higher_55 <- pred_mod5_test$predicted_probs>0.55 
pred_mod5_test$higher_60 <- pred_mod5_test$predicted_probs>0.60 
pred_mod5_test$higher_65 <- pred_mod5_test$predicted_probs>0.65 
pred_mod5_test$higher_70 <- pred_mod5_test$predicted_probs>0.70 
pred_mod5_test$higher_75 <- pred_mod5_test$predicted_probs>0.75 
pred_mod5_test$higher_80 <- pred_mod5_test$predicted_probs>0.80 
pred_mod5_test$higher_85 <- pred_mod5_test$predicted_probs>0.85 
pred_mod5_test$higher_90 <- pred_mod5_test$predicted_probs>0.90 
pred_mod5_test$higher_95 <- pred_mod5_test$predicted_probs>0.95 
pred_mod5_test$higher_100 <- pred_mod5_test$predicted_probs>1

pred_mod5_test$inc_error_00 <- pred_mod5_test$higher_00==TRUE & pred_mod5_test$poor_npl1==0
pred_mod5_test$inc_error_05 <- pred_mod5_test$higher_05==TRUE & pred_mod5_test$poor_npl1==0
pred_mod5_test$inc_error_10 <- pred_mod5_test$higher_10==TRUE & pred_mod5_test$poor_npl1==0
pred_mod5_test$inc_error_15 <- pred_mod5_test$higher_15==TRUE & pred_mod5_test$poor_npl1==0
pred_mod5_test$inc_error_20 <- pred_mod5_test$higher_20==TRUE & pred_mod5_test$poor_npl1==0
pred_mod5_test$inc_error_25 <- pred_mod5_test$higher_25==TRUE & pred_mod5_test$poor_npl1==0
pred_mod5_test$inc_error_30 <- pred_mod5_test$higher_30==TRUE & pred_mod5_test$poor_npl1==0
pred_mod5_test$inc_error_35 <- pred_mod5_test$higher_35==TRUE & pred_mod5_test$poor_npl1==0
pred_mod5_test$inc_error_40 <- pred_mod5_test$higher_40==TRUE & pred_mod5_test$poor_npl1==0
pred_mod5_test$inc_error_45 <- pred_mod5_test$higher_45==TRUE & pred_mod5_test$poor_npl1==0
pred_mod5_test$inc_error_50 <- pred_mod5_test$higher_50==TRUE & pred_mod5_test$poor_npl1==0
pred_mod5_test$inc_error_55 <- pred_mod5_test$higher_55==TRUE & pred_mod5_test$poor_npl1==0
pred_mod5_test$inc_error_60 <- pred_mod5_test$higher_60==TRUE & pred_mod5_test$poor_npl1==0
pred_mod5_test$inc_error_65 <- pred_mod5_test$higher_65==TRUE & pred_mod5_test$poor_npl1==0
pred_mod5_test$inc_error_70 <- pred_mod5_test$higher_70==TRUE & pred_mod5_test$poor_npl1==0
pred_mod5_test$inc_error_75 <- pred_mod5_test$higher_75==TRUE & pred_mod5_test$poor_npl1==0
pred_mod5_test$inc_error_80 <- pred_mod5_test$higher_80==TRUE & pred_mod5_test$poor_npl1==0
pred_mod5_test$inc_error_85 <- pred_mod5_test$higher_85==TRUE & pred_mod5_test$poor_npl1==0
pred_mod5_test$inc_error_90 <- pred_mod5_test$higher_90==TRUE & pred_mod5_test$poor_npl1==0
pred_mod5_test$inc_error_95 <- pred_mod5_test$higher_95==TRUE & pred_mod5_test$poor_npl1==0
pred_mod5_test$inc_error_100 <- pred_mod5_test$higher_100==TRUE & pred_mod5_test$poor_npl1==0

pred_mod5_inc <- c(length(which(pred_mod5_test$inc_error_00==TRUE))/length(which(pred_mod5_test$poor_npl1==0)),
                   length(which(pred_mod5_test$inc_error_05==TRUE))/length(which(pred_mod5_test$poor_npl1==0)),
                   length(which(pred_mod5_test$inc_error_10==TRUE))/length(which(pred_mod5_test$poor_npl1==0)),
                   length(which(pred_mod5_test$inc_error_15==TRUE))/length(which(pred_mod5_test$poor_npl1==0)),
                   length(which(pred_mod5_test$inc_error_20==TRUE))/length(which(pred_mod5_test$poor_npl1==0)),
                   length(which(pred_mod5_test$inc_error_25==TRUE))/length(which(pred_mod5_test$poor_npl1==0)),
                   length(which(pred_mod5_test$inc_error_30==TRUE))/length(which(pred_mod5_test$poor_npl1==0)),
                   length(which(pred_mod5_test$inc_error_35==TRUE))/length(which(pred_mod5_test$poor_npl1==0)),
                   length(which(pred_mod5_test$inc_error_40==TRUE))/length(which(pred_mod5_test$poor_npl1==0)),
                   length(which(pred_mod5_test$inc_error_45==TRUE))/length(which(pred_mod5_test$poor_npl1==0)),
                   length(which(pred_mod5_test$inc_error_50==TRUE))/length(which(pred_mod5_test$poor_npl1==0)),
                   length(which(pred_mod5_test$inc_error_55==TRUE))/length(which(pred_mod5_test$poor_npl1==0)),
                   length(which(pred_mod5_test$inc_error_60==TRUE))/length(which(pred_mod5_test$poor_npl1==0)),
                   length(which(pred_mod5_test$inc_error_65==TRUE))/length(which(pred_mod5_test$poor_npl1==0)),
                   length(which(pred_mod5_test$inc_error_70==TRUE))/length(which(pred_mod5_test$poor_npl1==0)),
                   length(which(pred_mod5_test$inc_error_75==TRUE))/length(which(pred_mod5_test$poor_npl1==0)),
                   length(which(pred_mod5_test$inc_error_80==TRUE))/length(which(pred_mod5_test$poor_npl1==0)),
                   length(which(pred_mod5_test$inc_error_85==TRUE))/length(which(pred_mod5_test$poor_npl1==0)),
                   length(which(pred_mod5_test$inc_error_90==TRUE))/length(which(pred_mod5_test$poor_npl1==0)),
                   length(which(pred_mod5_test$inc_error_95==TRUE))/length(which(pred_mod5_test$poor_npl1==0)),
                   length(which(pred_mod5_test$inc_error_100==TRUE))/length(which(pred_mod5_test$poor_npl1==0)))

pred_mod5_test$exc_error_00 <- pred_mod5_test$higher_00==FALSE & pred_mod5_test$poor_npl1==1
pred_mod5_test$exc_error_05 <- pred_mod5_test$higher_05==FALSE & pred_mod5_test$poor_npl1==1
pred_mod5_test$exc_error_10 <- pred_mod5_test$higher_10==FALSE & pred_mod5_test$poor_npl1==1
pred_mod5_test$exc_error_15 <- pred_mod5_test$higher_15==FALSE & pred_mod5_test$poor_npl1==1
pred_mod5_test$exc_error_20 <- pred_mod5_test$higher_20==FALSE & pred_mod5_test$poor_npl1==1
pred_mod5_test$exc_error_25 <- pred_mod5_test$higher_25==FALSE & pred_mod5_test$poor_npl1==1
pred_mod5_test$exc_error_30 <- pred_mod5_test$higher_30==FALSE & pred_mod5_test$poor_npl1==1
pred_mod5_test$exc_error_35 <- pred_mod5_test$higher_35==FALSE & pred_mod5_test$poor_npl1==1
pred_mod5_test$exc_error_40 <- pred_mod5_test$higher_40==FALSE & pred_mod5_test$poor_npl1==1
pred_mod5_test$exc_error_45 <- pred_mod5_test$higher_45==FALSE & pred_mod5_test$poor_npl1==1
pred_mod5_test$exc_error_50 <- pred_mod5_test$higher_50==FALSE & pred_mod5_test$poor_npl1==1
pred_mod5_test$exc_error_55 <- pred_mod5_test$higher_55==FALSE & pred_mod5_test$poor_npl1==1
pred_mod5_test$exc_error_60 <- pred_mod5_test$higher_60==FALSE & pred_mod5_test$poor_npl1==1
pred_mod5_test$exc_error_65 <- pred_mod5_test$higher_65==FALSE & pred_mod5_test$poor_npl1==1
pred_mod5_test$exc_error_70 <- pred_mod5_test$higher_70==FALSE & pred_mod5_test$poor_npl1==1
pred_mod5_test$exc_error_75 <- pred_mod5_test$higher_75==FALSE & pred_mod5_test$poor_npl1==1
pred_mod5_test$exc_error_80 <- pred_mod5_test$higher_80==FALSE & pred_mod5_test$poor_npl1==1
pred_mod5_test$exc_error_85 <- pred_mod5_test$higher_85==FALSE & pred_mod5_test$poor_npl1==1
pred_mod5_test$exc_error_90 <- pred_mod5_test$higher_90==FALSE & pred_mod5_test$poor_npl1==1
pred_mod5_test$exc_error_95 <- pred_mod5_test$higher_95==FALSE & pred_mod5_test$poor_npl1==1
pred_mod5_test$exc_error_100 <- pred_mod5_test$higher_100==FALSE & pred_mod5_test$poor_npl1==1

pred_mod5_exc <- c(length(which(pred_mod5_test$exc_error_00==TRUE))/length(which(pred_mod5_test$poor_npl1==1)),
                   length(which(pred_mod5_test$exc_error_05==TRUE))/length(which(pred_mod5_test$poor_npl1==1)),
                   length(which(pred_mod5_test$exc_error_10==TRUE))/length(which(pred_mod5_test$poor_npl1==1)),
                   length(which(pred_mod5_test$exc_error_15==TRUE))/length(which(pred_mod5_test$poor_npl1==1)),
                   length(which(pred_mod5_test$exc_error_20==TRUE))/length(which(pred_mod5_test$poor_npl1==1)),
                   length(which(pred_mod5_test$exc_error_25==TRUE))/length(which(pred_mod5_test$poor_npl1==1)),
                   length(which(pred_mod5_test$exc_error_30==TRUE))/length(which(pred_mod5_test$poor_npl1==1)),
                   length(which(pred_mod5_test$exc_error_35==TRUE))/length(which(pred_mod5_test$poor_npl1==1)),
                   length(which(pred_mod5_test$exc_error_40==TRUE))/length(which(pred_mod5_test$poor_npl1==1)),
                   length(which(pred_mod5_test$exc_error_45==TRUE))/length(which(pred_mod5_test$poor_npl1==1)),
                   length(which(pred_mod5_test$exc_error_50==TRUE))/length(which(pred_mod5_test$poor_npl1==1)),
                   length(which(pred_mod5_test$exc_error_55==TRUE))/length(which(pred_mod5_test$poor_npl1==1)),
                   length(which(pred_mod5_test$exc_error_60==TRUE))/length(which(pred_mod5_test$poor_npl1==1)),
                   length(which(pred_mod5_test$exc_error_65==TRUE))/length(which(pred_mod5_test$poor_npl1==1)),
                   length(which(pred_mod5_test$exc_error_70==TRUE))/length(which(pred_mod5_test$poor_npl1==1)),
                   length(which(pred_mod5_test$exc_error_75==TRUE))/length(which(pred_mod5_test$poor_npl1==1)),
                   length(which(pred_mod5_test$exc_error_80==TRUE))/length(which(pred_mod5_test$poor_npl1==1)),
                   length(which(pred_mod5_test$exc_error_85==TRUE))/length(which(pred_mod5_test$poor_npl1==1)),
                   length(which(pred_mod5_test$exc_error_90==TRUE))/length(which(pred_mod5_test$poor_npl1==1)),
                   length(which(pred_mod5_test$exc_error_95==TRUE))/length(which(pred_mod5_test$poor_npl1==1)),
                   length(which(pred_mod5_test$exc_error_100==TRUE))/length(which(pred_mod5_test$poor_npl1==1)))

pred_mod5_error <- cbind(prob_cutoff, pred_mod5_inc, pred_mod5_exc)


pred_mod1_error <- as.data.frame(pred_mod1_error)
pred_mod2_error <- as.data.frame(pred_mod2_error)
pred_mod3_error <- as.data.frame(pred_mod3_error)
pred_mod4_error <- as.data.frame(pred_mod4_error)
pred_mod5_error <- as.data.frame(pred_mod5_error)
pred_mod1_error$model <- "EN - LSMS"
pred_mod2_error$model <- "EN - LSMS+RWI"
pred_mod3_error$model <- "EBM - LSMS"
pred_mod4_error$model <- "EBM - MOSAIKS"
pred_mod5_error$model <- "EBM - LSMS+MOSAIKS"

pred_mod1_error <- rename(pred_mod1_error, inclusion_error = pred_mod1_inc)
pred_mod1_error <- rename(pred_mod1_error, exclusion_error = pred_mod1_exc)
pred_mod2_error <- rename(pred_mod2_error, inclusion_error = pred_mod2_inc)
pred_mod2_error <- rename(pred_mod2_error, exclusion_error = pred_mod2_exc)
pred_mod3_error <- rename(pred_mod3_error, inclusion_error = pred_mod3_inc)
pred_mod3_error <- rename(pred_mod3_error, exclusion_error = pred_mod3_exc)
pred_mod4_error <- rename(pred_mod4_error, inclusion_error = pred_mod4_inc)
pred_mod4_error <- rename(pred_mod4_error, exclusion_error = pred_mod4_exc)
pred_mod5_error <- rename(pred_mod5_error, inclusion_error = pred_mod5_inc)
pred_mod5_error <- rename(pred_mod5_error, exclusion_error = pred_mod5_exc)

pred_errors <- rbind(pred_mod1_error,
                     pred_mod2_error, 
                     pred_mod3_error, 
                     pred_mod4_error,
                     pred_mod5_error)
pred_errors$inclusion_error[is.na(pred_errors$inclusion_error)] <- 0
pred_errors$exclusion_error[is.na(pred_errors$exclusion_error)] <- 0

pred_errors$inclusion_error <- pred_errors$inclusion_error*100
pred_errors$exclusion_error <- pred_errors$exclusion_error*100

pred_errors$model <- factor(pred_errors$model, levels = c("EN - LSMS",
                                                          "EN - LSMS+RWI",
                                                          "EBM - LSMS",
                                                          "EBM - MOSAIKS",
                                                          "EBM - LSMS+MOSAIKS"))

# *****************************************************************************
#### 3. PLOTS ####
# *****************************************************************************

#Import font. You may need to download the relevant fonts from:
#https://fonts.google.com/specimen/Open+Sans?query=open+sans

#font_import(paths = "C:/Users/Manuel Cardona Arias/Downloads/Open_Sans/",prompt = F) 

#Color palette: Tableau 20
palette<-paletteer::paletteer_d("ggthemes::Tableau_20")

#----Target error (Full sample)----
target<-ggplot(pred_errors, aes(x=prob_cutoff, color=model)) + 
  geom_line(aes(y=inclusion_error), size=0.5) +
  geom_line(aes(y=exclusion_error), size=0.5) +
  geom_point(aes(x = 0.17, y = 23.5), size=0.8) +
  geom_vline(xintercept = 0.17, size=0.5, linetype="dotted") + 
  geom_hline(yintercept = 23.5, size=0.5, linetype="dotted") + 
  scale_y_continuous(breaks = number_ticks(10)) +
  scale_x_continuous(breaks = number_ticks(10)) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold",
                                  size = 20,
                                  family = "Arial"),
        plot.subtitle = element_text(size = 12,
                                     face = "plain", 
                                     family = "Arial"),
        plot.caption = element_text(hjust = 0, 
                                    face = "plain", 
                                    family = "Arial",
                                    size = 8,
                                    colour = "#777777"),
        panel.background = element_rect(fill = "white", 
                                        colour = "white", 
                                        size = 0.15, 
                                        linetype = "solid"),
        panel.grid.major = element_line(size = 0.15, 
                                        linetype = 'solid',
                                        colour = "gray90"), 
        panel.grid.minor = element_line(size = 0.15, 
                                        linetype = 'solid',
                                        colour = "gray90"), 
        axis.title.x = (element_text(size = 16,
                                     family = "Arial")),
        axis.title.y = (element_text(size =16,
                                     family = "Arial")),
        element_line(linetype = "dotted",
                     colour = "gray99",
                     size = .1),
        axis.text.x = element_text(angle = 0,
                                   hjust = 0.5,
                                   size = 12, 
                                   family = ""),
        axis.text.y = element_text(size = 12,
                                   family = ""),
        legend.text = element_text(size = 12,
                                   family = ""),
        legend.title = element_blank()) +
  labs(title = "Targeting accuracy by type of model",
       subtitle = "National Poverty Line - Ethiopia 2015-16 (Full sample)",
       x = "Score cutoff",
       y = "% misclassified",
       caption = paste(" ")) +
  ggsave(paste0("../06 Figures/04 Accuracy/", "01", "_", "target_error_full_allmodels_2", ".jpeg"), 
         width = 10, height = 6)

#----Target error - Inclusion (Full sample)----
target<-ggplot(pred_errors, aes(x=prob_cutoff, color=model)) + 
  geom_line(aes(y=inclusion_error), size=0.5) +
  scale_y_continuous(breaks = number_ticks(10)) +
  scale_x_continuous(breaks = number_ticks(10)) +
  geom_vline(xintercept = 0.2, size=0.5, linetype="dotted") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold",
                                  size = 20,
                                  family = "Arial"),
        plot.subtitle = element_text(size = 12,
                                     face = "plain", 
                                     family = "Arial"),
        plot.caption = element_text(hjust = 0, 
                                    face = "plain", 
                                    family = "Arial",
                                    size = 8,
                                    colour = "#777777"),
        panel.background = element_rect(fill = "white", 
                                        colour = "white", 
                                        size = 0.15, 
                                        linetype = "solid"),
        panel.grid.major = element_line(size = 0.15, 
                                        linetype = 'solid',
                                        colour = "gray90"), 
        panel.grid.minor = element_line(size = 0.15, 
                                        linetype = 'solid',
                                        colour = "gray90"), 
        axis.title.x = (element_text(size = 16,
                                     family = "Arial")),
        axis.title.y = (element_text(size =16,
                                     family = "Arial")),
        element_line(linetype = "dotted",
                     colour = "gray99",
                     size = .1),
        axis.text.x = element_text(angle = 0,
                                   hjust = 0.5,
                                   size = 12, 
                                   family = ""),
        axis.text.y = element_text(size = 12,
                                   family = ""),
        legend.text = element_text(size = 12,
                                   family = ""),
        legend.title = element_blank()) +
  labs(title = "Inclusion error by type of model",
       subtitle = "National Poverty Line - Ethiopia 2015-16 (Full sample)",
       x = "Score cutoff",
       y = "% misclassified",
       caption = paste(" ")) +
  ggsave(paste0("../06 Figures/04 Accuracy/", "01", "_", "inclusion_error_full_allmodels", ".jpeg"), 
         width = 10, height = 6)

#----Target error - Inclusion (Full sample)----
target<-ggplot(pred_errors, aes(x=prob_cutoff, color=model)) + 
  geom_line(aes(y=exclusion_error), size=0.5) +
  scale_y_continuous(breaks = number_ticks(10)) +
  scale_x_continuous(breaks = number_ticks(10)) +
  geom_vline(xintercept = 0.2, size=0.5, linetype="dotted") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold",
                                  size = 20,
                                  family = "Arial"),
        plot.subtitle = element_text(size = 12,
                                     face = "plain", 
                                     family = "Arial"),
        plot.caption = element_text(hjust = 0, 
                                    face = "plain", 
                                    family = "Arial",
                                    size = 8,
                                    colour = "#777777"),
        panel.background = element_rect(fill = "white", 
                                        colour = "white", 
                                        size = 0.15, 
                                        linetype = "solid"),
        panel.grid.major = element_line(size = 0.15, 
                                        linetype = 'solid',
                                        colour = "gray90"), 
        panel.grid.minor = element_line(size = 0.15, 
                                        linetype = 'solid',
                                        colour = "gray90"), 
        axis.title.x = (element_text(size = 16,
                                     family = "Arial")),
        axis.title.y = (element_text(size =16,
                                     family = "Arial")),
        element_line(linetype = "dotted",
                     colour = "gray99",
                     size = .1),
        axis.text.x = element_text(angle = 0,
                                   hjust = 0.5,
                                   size = 12, 
                                   family = ""),
        axis.text.y = element_text(size = 12,
                                   family = ""),
        legend.text = element_text(size = 12,
                                   family = ""),
        legend.title = element_blank()) +
  labs(title = "Exclusion error by type of model",
       subtitle = "National Poverty Line - Ethiopia 2015-16 (Full sample)",
       x = "Score cutoff",
       y = "% misclassified",
       caption = paste(" ")) +
  ggsave(paste0("../06 Figures/04 Accuracy/", "01", "_", "exclusion_error_full_allmodels", ".jpeg"), 
         width = 10, height = 6)
#----ROC - Model 1----
roc_mod1 <- roc(pred_mod1_test$poor_npl1, pred_mod1_test$pred_score_pr_test)
auc_mod1 <- round(auc(pred_mod1_test$poor_npl1, pred_mod1_test$pred_score_pr_test), 4)
ggroc(roc_mod1, colour = '#E15759FF', size = 1) +
  ggtitle(paste0('ROC Curve - EN-LSMS', '(AUC = ', auc_mod1, ')')) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold",
                                  size = 16,
                                  family = "Arial"),
        plot.subtitle = element_text(size = 12,
                                     face = "plain", 
                                     family = "Arial"),
        plot.caption = element_text(hjust = 0, 
                                    face = "plain", 
                                    family = "Arial",
                                    size = 8,
                                    colour = "#777777"),
        panel.background = element_rect(fill = "white", 
                                        colour = "white", 
                                        size = 0.15, 
                                        linetype = "solid"),
        panel.grid.major = element_line(size = 0.15, 
                                        linetype = 'solid',
                                        colour = "gray40"), 
        panel.grid.minor = element_line(size = 0.15, 
                                        linetype = 'solid',
                                        colour = "gray40"), 
        axis.title.x = (element_text(size = 16,
                                     family = "Arial")),
        axis.title.y = (element_text(size =16,
                                     family = "Arial")),
        element_line(linetype = "dotted",
                     colour = "gray99",
                     size = .1),
        axis.text.x = element_text(angle = 0,
                                   hjust = 0.5,
                                   size = 12, 
                                   family = ""),
        axis.text.y = element_text(size = 12,
                                   family = ""),
        legend.text = element_text(size = 12,
                                   family = ""),
        legend.title = element_blank()) +
  labs(subtitle = "National Poverty Line - Ethiopia 2015-16 (Full sample)",
       x = "Specificity (True negative rate)",
       y = "Sensitivity (True positive rate)",
       caption = paste(" ")) +
  ggsave(paste0("../06 Figures/04 Accuracy/", "02", "_", "roc_full_mod1", ".jpeg"), 
         width = 10, height = 6)
#----ROC - Model 2----
roc_mod2 <- roc(pred_mod2_test$poor_npl1, pred_mod2_test$pred_score_pr_test)
auc_mod2 <- round(auc(pred_mod2_test$poor_npl1, pred_mod2_test$pred_score_pr_test), 4)
ggroc(roc_mod2, colour = '#8CD17DFF', size = 1) +
  ggtitle(paste0('ROC Curve - EN-LSMS+RWI', '(AUC = ', auc_mod2, ')')) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold",
                                  size = 16,
                                  family = "Arial"),
        plot.subtitle = element_text(size = 12,
                                     face = "plain", 
                                     family = "Arial"),
        plot.caption = element_text(hjust = 0, 
                                    face = "plain", 
                                    family = "Arial",
                                    size = 8,
                                    colour = "#777777"),
        panel.background = element_rect(fill = "white", 
                                        colour = "white", 
                                        size = 0.15, 
                                        linetype = "solid"),
        panel.grid.major = element_line(size = 0.15, 
                                        linetype = 'solid',
                                        colour = "gray40"), 
        panel.grid.minor = element_line(size = 0.15, 
                                        linetype = 'solid',
                                        colour = "gray40"), 
        axis.title.x = (element_text(size = 16,
                                     family = "Arial")),
        axis.title.y = (element_text(size =16,
                                     family = "Arial")),
        element_line(linetype = "dotted",
                     colour = "gray99",
                     size = .1),
        axis.text.x = element_text(angle = 0,
                                   hjust = 0.5,
                                   size = 12, 
                                   family = ""),
        axis.text.y = element_text(size = 12,
                                   family = ""),
        legend.text = element_text(size = 12,
                                   family = ""),
        legend.title = element_blank()) +
  labs(subtitle = "National Poverty Line - Ethiopia 2015-16 (Full sample)",
       x = "Specificity (True negative rate)",
       y = "Sensitivity (True positive rate)",
       caption = paste(" ")) +
  ggsave(paste0("../06 Figures/04 Accuracy/", "02", "_", "roc_full_mod2", ".jpeg"), 
         width = 10, height = 6)
#----ROC - Model 3----
roc_mod3 <- roc(pred_mod3_test$poor_npl1, pred_mod3_test$predicted_probs)
auc_mod3 <- round(auc(pred_mod3_test$poor_npl1, pred_mod3_test$predicted_probs), 4)
ggroc(roc_mod3, colour = '#499894FF', size = 1) +
  ggtitle(paste0('ROC Curve - EBM-LSMS', '(AUC = ', auc_mod3, ')')) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold",
                                  size = 16,
                                  family = "Arial"),
        plot.subtitle = element_text(size = 12,
                                     face = "plain", 
                                     family = "Arial"),
        plot.caption = element_text(hjust = 0, 
                                    face = "plain", 
                                    family = "Arial",
                                    size = 8,
                                    colour = "#777777"),
        panel.background = element_rect(fill = "white", 
                                        colour = "white", 
                                        size = 0.15, 
                                        linetype = "solid"),
        panel.grid.major = element_line(size = 0.15, 
                                        linetype = 'solid',
                                        colour = "gray40"), 
        panel.grid.minor = element_line(size = 0.15, 
                                        linetype = 'solid',
                                        colour = "gray40"), 
        axis.title.x = (element_text(size = 16,
                                     family = "Arial")),
        axis.title.y = (element_text(size =16,
                                     family = "Arial")),
        element_line(linetype = "dotted",
                     colour = "gray99",
                     size = .1),
        axis.text.x = element_text(angle = 0,
                                   hjust = 0.5,
                                   size = 12, 
                                   family = ""),
        axis.text.y = element_text(size = 12,
                                   family = ""),
        legend.text = element_text(size = 12,
                                   family = ""),
        legend.title = element_blank()) +
  labs(subtitle = "National Poverty Line - Ethiopia 2015-16 (Full sample)",
       x = "Specificity (True negative rate)",
       y = "Sensitivity (True positive rate)",
       caption = paste(" ")) +
  ggsave(paste0("../06 Figures/04 Accuracy/", "02", "_", "roc_full_mod3", ".jpeg"), 
         width = 10, height = 6)

#----ROC - Model 4----
roc_mod4 <- roc(pred_mod4_test$poor_npl1, pred_mod4_test$predicted_probs)
auc_mod4 <- round(auc(pred_mod4_test$poor_npl1, pred_mod4_test$predicted_probs), 4)
ggroc(roc_mod4, colour = '#A0CBE8FF', size = 1) +
  ggtitle(paste0('ROC Curve - EBM-MOSAIKS', '(AUC = ', auc_mod4, ')')) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold",
                                  size = 16,
                                  family = "Arial"),
        plot.subtitle = element_text(size = 12,
                                     face = "plain", 
                                     family = "Arial"),
        plot.caption = element_text(hjust = 0, 
                                    face = "plain", 
                                    family = "Arial",
                                    size = 8,
                                    colour = "#777777"),
        panel.background = element_rect(fill = "white", 
                                        colour = "white", 
                                        size = 0.15, 
                                        linetype = "solid"),
        panel.grid.major = element_line(size = 0.15, 
                                        linetype = 'solid',
                                        colour = "gray40"), 
        panel.grid.minor = element_line(size = 0.15, 
                                        linetype = 'solid',
                                        colour = "gray40"), 
        axis.title.x = (element_text(size = 16,
                                     family = "Arial")),
        axis.title.y = (element_text(size =16,
                                     family = "Arial")),
        element_line(linetype = "dotted",
                     colour = "gray99",
                     size = .1),
        axis.text.x = element_text(angle = 0,
                                   hjust = 0.5,
                                   size = 12, 
                                   family = ""),
        axis.text.y = element_text(size = 12,
                                   family = ""),
        legend.text = element_text(size = 12,
                                   family = ""),
        legend.title = element_blank()) +
  labs(subtitle = "National Poverty Line - Ethiopia 2015-16 (Full sample)",
       x = "Specificity (True negative rate)",
       y = "Sensitivity (True positive rate)",
       caption = paste(" ")) +
  ggsave(paste0("../06 Figures/04 Accuracy/", "02", "_", "roc_full_mod4", ".jpeg"), 
         width = 10, height = 6)

#----ROC - Model 5----
roc_mod5 <- roc(pred_mod5_test$poor_npl1, pred_mod5_test$predicted_probs)
auc_mod5 <- round(auc(pred_mod5_test$poor_npl1, pred_mod5_test$predicted_probs), 4)
ggroc(roc_mod5, colour = '#D37295FF', size = 1) +
  ggtitle(paste0('ROC Curve - EBM-LSMS+MOSAIKS', '(AUC = ', auc_mod5, ')')) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold",
                                  size = 16,
                                  family = "Arial"),
        plot.subtitle = element_text(size = 12,
                                     face = "plain", 
                                     family = "Arial"),
        plot.caption = element_text(hjust = 0, 
                                    face = "plain", 
                                    family = "Arial",
                                    size = 8,
                                    colour = "#777777"),
        panel.background = element_rect(fill = "white", 
                                        colour = "white", 
                                        size = 0.15, 
                                        linetype = "solid"),
        panel.grid.major = element_line(size = 0.15, 
                                        linetype = 'solid',
                                        colour = "gray40"), 
        panel.grid.minor = element_line(size = 0.15, 
                                        linetype = 'solid',
                                        colour = "gray40"), 
        axis.title.x = (element_text(size = 16,
                                     family = "Arial")),
        axis.title.y = (element_text(size =16,
                                     family = "Arial")),
        element_line(linetype = "dotted",
                     colour = "gray99",
                     size = .1),
        axis.text.x = element_text(angle = 0,
                                   hjust = 0.5,
                                   size = 12, 
                                   family = ""),
        axis.text.y = element_text(size = 12,
                                   family = ""),
        legend.text = element_text(size = 12,
                                   family = ""),
        legend.title = element_blank()) +
  labs(subtitle = "National Poverty Line - Ethiopia 2015-16 (Full sample)",
       x = "Specificity (True negative rate)",
       y = "Sensitivity (True positive rate)",
       caption = paste(" ")) +
  ggsave(paste0("../06 Figures/04 Accuracy/", "02", "_", "roc_full_mod5", ".jpeg"), 
         width = 10, height = 6)

#----ROC - Compare models----
ggroc(list(call_roc_name_1 = roc_mod1, call_roc_name_2 = roc_mod2,
           call_roc_name_3 = roc_mod3, call_roc_name_4 = roc_mod4,
           call_roc_name_5 = roc_mod5)) +
  theme_minimal() +
  scale_color_manual(labels = c("EN - LSMS (0.76)", "EN - LSMS+RWI (0.75)",
                                "EBM - LSMS (0.79)", "EBM - MOSAIKS (0.76)",
                                "EBM - LSMS+MOSAIKS (0.84)"),
                     values = c("#E15759FF", "#8CD17DFF",
                                "#499894FF", "#A0CBE8FF", 
                                "#D37295FF")) +
  theme(plot.title = element_text(face = "bold",
                                  size = 16,
                                  family = "Arial"),
        plot.subtitle = element_text(size = 12,
                                     face = "plain", 
                                     family = "Arial"),
        plot.caption = element_text(hjust = 0, 
                                    face = "plain", 
                                    family = "Arial",
                                    size = 8,
                                    colour = "#777777"),
        panel.background = element_rect(fill = "white", 
                                        colour = "white", 
                                        size = 0.15, 
                                        linetype = "solid"),
        panel.grid.major = element_line(size = 0.15, 
                                        linetype = 'solid',
                                        colour = "gray40"), 
        panel.grid.minor = element_line(size = 0.15, 
                                        linetype = 'solid',
                                        colour = "gray40"), 
        axis.title.x = (element_text(size = 16,
                                     family = "Arial")),
        axis.title.y = (element_text(size =16,
                                     family = "Arial")),
        element_line(linetype = "dotted",
                     colour = "gray99",
                     size = .1),
        axis.text.x = element_text(angle = 0,
                                   hjust = 0.5,
                                   size = 12, 
                                   family = ""),
        axis.text.y = element_text(size = 12,
                                   family = ""),
        legend.text = element_text(size = 12,
                                   family = ""),
        legend.title = element_blank()) +
  labs(subtitle = "National Poverty Line - Ethiopia 2015-16 (Full sample)",
       title = "ROC curves by type of model (AUC*)",
       x = "Specificity (True negative rate)",
       y = "Sensitivity (True positive rate)",
       caption = paste(" ")) +
  ggsave(paste0("../06 Figures/04 Accuracy/", "02", "_", "roc_full_allmodels", ".jpeg"), 
         width = 10, height = 6)