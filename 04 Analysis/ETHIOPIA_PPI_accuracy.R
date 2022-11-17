################################################################## 
# Purpose: Create visualizations for the accuracy of different   #
#          versions of the PPI
#                                                                #
# Created: August 24, 2021                                       #
# Depends on:                                                    #
#   Author: Manuel Cardona                                       # 
#   e-mail: mcardona@poverty-action.org                          #
#                                                                #
################################################################## 

rm(list = ls()) # to clean the workspace

# *****************************************************************************
#### 01_Load_packages ####
# *****************************************************************************

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


# *****************************************************************************
#### 02_Load_data ####
# *****************************************************************************
  setwd("/Users/manuelarias/Documents/GitHub/satellite_to_poverty_IPA/")

  #----Survey data----
  survey_data<-read_dta("02 Data/02 Clean/LSMS/MASTER_ESS3_Ethiopia.dta")
  
  #----No radiance scores----
  mod0<-read.csv("08 Results/LSMS/PovRate_Errors_10q_ALLpovlines.csv")
  tod0<-read.csv("08 Results/LSMS/Target_Errors_10q_ALLpovlines.csv")
  pred0<-read.csv("08 Results/LSMS/Predicted_scores_10q_NPL1.csv")
  
  #----Regions and radiance unpenalized----
  mod1<-read.csv("08 Results/Luminosity/Regions and radiance unpenalized/PovRate_Errors_10q_ALLpovlines.csv")
  tod1<-read.csv("08 Results/Luminosity/Regions and radiance unpenalized/Target_Errors_10q_ALLpovlines.csv")
  pred1<-read.csv("08 Results/Luminosity/Predicted_scores_10q_NPL1.csv")
  
  #----Radiance penalized, but calibration unpenalized----
  mod2<-read.csv("08 Results/Luminosity/Regions unpenalized 2/PovRate_Errors_10q_ALLpovlines.csv")
  tod2<-read.csv("08 Results/Luminosity/Regions unpenalized 2/Target_Errors_10q_ALLpovlines.csv")
  pred2<-read.csv("08 Results/RWI/Predicted_scores_10q_NPL1.csv")
  
  #----Predicted Scored by Type of Model----
  pred_ur_10<-read.csv("08 Results/LSMS/01 UR 10Q/Predicted_scores_10q_NPL1.csv")
  pred_ur_20<-read.csv("08 Results/LSMS/02 UR 20Q/Predicted_scores_20q_NPL1.csv")
  pred_nur_10<-read.csv("08 Results/LSMS/03 No UR 10Q/Predicted_scores_10q_NPL1.csv")
  pred_nur_20<-read.csv("08 Results/LSMS/04 No UR 20Q/Predicted_scores_20q_NPL1.csv")
  
  
# *****************************************************************************
#### 03_Arrange_data####
# *****************************************************************************
  mod0<-subset(mod0, povline=="poor_npl1" & obs==500)
  mod0$model<-"Original"
  
  mod1<-subset(mod1, povline=="poor_npl1" & obs==500)
  mod1$model<-"Unpenalized"
  
  mod2<-subset(mod2, povline=="poor_npl1" & obs==500)
  mod2$model<-"Penalized for calibration"
  
  mods<-rbind(mod0, mod1, mod2)
  mods<-mods%>%
    mutate(zone=case_when(zone=="rural" ~ "Rural",
                          zone=="urban" ~ "Urban",
                          zone=="full" ~ "Full sample"),
           error=paste0(round(mean_error_model,2), "%"))
  
  tod0_full<-subset(tod0, povline=="poor_npl1" & obs==500 & zone=="full")
  tod0_full$model<-"Original"
  
  tod1_full<-subset(tod1, povline=="poor_npl1" & obs==500 & zone=="full")
  tod1_full$model<-"Unpenalized"
  
  tod2_full<-subset(tod2, povline=="poor_npl1" & obs==500 & zone=="full")
  tod2_full$model<-"Penalized for calibration"
  
  tods_full<-rbind(tod0_full, tod1_full, tod2_full)
  
  
  tods_full<-tods_full%>%
    select(cutoff, X50._excl, X50._incl, obs, zone, povline, model)%>%
    mutate(zone=case_when(zone=="rural" ~ "Rural",
                          zone=="urban" ~ "Urban",
                          zone=="full" ~ "Full sample"),
           exclusion=round((X50._excl*100),2),
           inclusion=round((X50._incl*100),2))
  
  tods_full_excl<-tods_full%>%
    select(cutoff, model, exclusion)%>%
    rename(error=exclusion)%>%
    mutate(type="Exclusion error")
  
  tods_full_incl<-tods_full%>%
    select(cutoff, model, inclusion)%>%
    rename(error=inclusion)%>%
    mutate(type="Inclusion error")
  
  tods_full<-rbind(tods_full_excl, tods_full_incl)
  
  
  
  tod0_rur<-subset(tod0, povline=="poor_npl1" & obs==500 & zone=="rural")
  tod0_rur$model<-"Original"
  
  tod1_rur<-subset(tod1, povline=="poor_npl1" & obs==500 & zone=="rural")
  tod1_rur$model<-"Unpenalized"
  
  tod2_rur<-subset(tod2, povline=="poor_npl1" & obs==500 & zone=="rural")
  tod2_rur$model<-"Penalized for calibration"
  
  tods_rur<-rbind(tod0_rur, tod1_rur, tod2_rur)
  
  
  tods_rur<-tods_rur%>%
    select(cutoff, X50._excl, X50._incl, obs, zone, povline, model)%>%
    mutate(zone=case_when(zone=="rural" ~ "Rural",
                          zone=="urban" ~ "Urban",
                          zone=="full" ~ "Full sample"),
           exclusion=round((X50._excl*100),2),
           inclusion=round((X50._incl*100),2))
  
  tods_rur_excl<-tods_rur%>%
    select(cutoff, model, exclusion)%>%
    rename(error=exclusion)%>%
    mutate(type="Exclusion error")
  
  tods_rur_incl<-tods_rur%>%
    select(cutoff, model, inclusion)%>%
    rename(error=inclusion)%>%
    mutate(type="Inclusion error")
  
  tods_rur<-rbind(tods_rur_excl, tods_rur_incl)
  
  tod0_urb<-subset(tod0, povline=="poor_npl1" & obs==500 & zone=="urban")
  tod0_urb$model<-"Original"
  
  tod1_urb<-subset(tod1, povline=="poor_npl1" & obs==500 & zone=="urban")
  tod1_urb$model<-"Unpenalized"
  
  tod2_urb<-subset(tod2, povline=="poor_npl1" & obs==500 & zone=="urban")
  tod2_urb$model<-"Penalized for calibration"
  
  tods_urb<-rbind(tod0_urb, tod1_urb, tod2_urb)
  
  
  tods_urb<-tods_urb%>%
    select(cutoff, X50._excl, X50._incl, obs, zone, povline, model)%>%
    mutate(zone=case_when(zone=="rural" ~ "Rural",
                          zone=="urban" ~ "Urban",
                          zone=="full" ~ "Full sample"),
           exclusion=round((X50._excl*100),2),
           inclusion=round((X50._incl*100),2))
  
  tods_urb_excl<-tods_urb%>%
    select(cutoff, model, exclusion)%>%
    rename(error=exclusion)%>%
    mutate(type="Exclusion error")
  
  tods_urb_incl<-tods_urb%>%
    select(cutoff, model, inclusion)%>%
    rename(error=inclusion)%>%
    mutate(type="Inclusion error")
  
  tods_urb<-rbind(tods_urb_excl, tods_urb_incl)
  
  #----Predicted probabilities----
  regions<-survey_data[, c("hh_id", "region")]
  pred0<-merge(pred0, regions, by="hh_id")
  pred0<-pred0%>%
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
                                urban=="Urban" & poor_npl1==1 ~ "Urban, Poor"),
           region_poor=case_when(region=="Amhara" & poor_npl1==0 ~ "Amhara, Non-Poor",
                                 region=="Amhara" & poor_npl1==1 ~ "Amhara, Poor",
                                 region=="Oromiya" & poor_npl1==0 ~ "Oromiya, Non-Poor",
                                 region=="Oromiya" & poor_npl1==1 ~ "Oromiya, Poor",
                                 region=="SNNP" & poor_npl1==0 ~ "SNNP, Non-Poor",
                                 region=="SNNP" & poor_npl1==1 ~ "SNNP, Poor",
                                 region=="Tigray" & poor_npl1==0 ~ "Tigray, Non-Poor",
                                 region=="Tigray" & poor_npl1==1 ~ "Tigray, Poor",
                                 region=="Other regions" & poor_npl1==0 ~ "Other regions, Non-Poor",
                                 region=="Other regions" & poor_npl1==1 ~ "Other regions, Poor"))

  #----Predicted probabilities----
  regions<-survey_data[, c("hh_id", "region")]
  pred1<-merge(pred1, regions, by="hh_id")
  pred1<-pred1%>%
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
                                urban=="Urban" & poor_npl1==1 ~ "Urban, Poor"),
           region_poor=case_when(region=="Amhara" & poor_npl1==0 ~ "Amhara, Non-Poor",
                                 region=="Amhara" & poor_npl1==1 ~ "Amhara, Poor",
                                 region=="Oromiya" & poor_npl1==0 ~ "Oromiya, Non-Poor",
                                 region=="Oromiya" & poor_npl1==1 ~ "Oromiya, Poor",
                                 region=="SNNP" & poor_npl1==0 ~ "SNNP, Non-Poor",
                                 region=="SNNP" & poor_npl1==1 ~ "SNNP, Poor",
                                 region=="Tigray" & poor_npl1==0 ~ "Tigray, Non-Poor",
                                 region=="Tigray" & poor_npl1==1 ~ "Tigray, Poor",
                                 region=="Other regions" & poor_npl1==0 ~ "Other regions, Non-Poor",
                                 region=="Other regions" & poor_npl1==1 ~ "Other regions, Poor"))

  #----Predicted probabilities----
  regions<-survey_data[, c("hh_id", "region")]
  pred2<-merge(pred2, regions, by="hh_id")
  pred2<-pred2%>%
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
                                urban=="Urban" & poor_npl1==1 ~ "Urban, Poor"),
           region_poor=case_when(region=="Amhara" & poor_npl1==0 ~ "Amhara, Non-Poor",
                                 region=="Amhara" & poor_npl1==1 ~ "Amhara, Poor",
                                 region=="Oromiya" & poor_npl1==0 ~ "Oromiya, Non-Poor",
                                 region=="Oromiya" & poor_npl1==1 ~ "Oromiya, Poor",
                                 region=="SNNP" & poor_npl1==0 ~ "SNNP, Non-Poor",
                                 region=="SNNP" & poor_npl1==1 ~ "SNNP, Poor",
                                 region=="Tigray" & poor_npl1==0 ~ "Tigray, Non-Poor",
                                 region=="Tigray" & poor_npl1==1 ~ "Tigray, Poor",
                                 region=="Other regions" & poor_npl1==0 ~ "Other regions, Non-Poor",
                                 region=="Other regions" & poor_npl1==1 ~ "Other regions, Poor"))
  
  pred0$features <- "LSMS features"
  pred1$features <- "LSMS + Radiance"
  pred2$features <- "LSMS + RWI"
  
  pred <- rbind(pred0, pred1, pred2)
  
  pred<-pred%>%
    mutate(feature_poor = case_when(features=="LSMS features" & poor_npl1==0 ~ "LSMS, Non-Poor",
                                    features=="LSMS features" & poor_npl1==1 ~ "LSMS, Poor",
                                    features=="LSMS + Radiance" & poor_npl1==0 ~ "Radiance, Non-Poor",
                                    features=="LSMS + Radiance" & poor_npl1==1 ~ "Radiance, Poor",
                                    features=="LSMS + RWI" & poor_npl1==0 ~ "RWI, Non-Poor",
                                    features=="LSMS + RWI" & poor_npl1==1 ~ "RWI, Poor"))
  
  
  #----Predicted probabilities----
  regions<-survey_data[, c("hh_id", "region")]
  pred_ur_10<-merge(pred_ur_10, regions, by="hh_id")
  pred_ur_10<-pred_ur_10%>%
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
                                urban=="Urban" & poor_npl1==1 ~ "Urban, Poor"),
           region_poor=case_when(region=="Amhara" & poor_npl1==0 ~ "Amhara, Non-Poor",
                                 region=="Amhara" & poor_npl1==1 ~ "Amhara, Poor",
                                 region=="Oromiya" & poor_npl1==0 ~ "Oromiya, Non-Poor",
                                 region=="Oromiya" & poor_npl1==1 ~ "Oromiya, Poor",
                                 region=="SNNP" & poor_npl1==0 ~ "SNNP, Non-Poor",
                                 region=="SNNP" & poor_npl1==1 ~ "SNNP, Poor",
                                 region=="Tigray" & poor_npl1==0 ~ "Tigray, Non-Poor",
                                 region=="Tigray" & poor_npl1==1 ~ "Tigray, Poor",
                                 region=="Other regions" & poor_npl1==0 ~ "Other regions, Non-Poor",
                                 region=="Other regions" & poor_npl1==1 ~ "Other regions, Poor"))
  
  pred_ur_20<-merge(pred_ur_20, regions, by="hh_id")
  pred_ur_20<-pred_ur_20%>%
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
                                urban=="Urban" & poor_npl1==1 ~ "Urban, Poor"),
           region_poor=case_when(region=="Amhara" & poor_npl1==0 ~ "Amhara, Non-Poor",
                                 region=="Amhara" & poor_npl1==1 ~ "Amhara, Poor",
                                 region=="Oromiya" & poor_npl1==0 ~ "Oromiya, Non-Poor",
                                 region=="Oromiya" & poor_npl1==1 ~ "Oromiya, Poor",
                                 region=="SNNP" & poor_npl1==0 ~ "SNNP, Non-Poor",
                                 region=="SNNP" & poor_npl1==1 ~ "SNNP, Poor",
                                 region=="Tigray" & poor_npl1==0 ~ "Tigray, Non-Poor",
                                 region=="Tigray" & poor_npl1==1 ~ "Tigray, Poor",
                                 region=="Other regions" & poor_npl1==0 ~ "Other regions, Non-Poor",
                                 region=="Other regions" & poor_npl1==1 ~ "Other regions, Poor"))
  pred_nur_10<-merge(pred_nur_10, regions, by="hh_id")
  pred_nur_10<-pred_nur_10%>%
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
                                urban=="Urban" & poor_npl1==1 ~ "Urban, Poor"),
           region_poor=case_when(region=="Amhara" & poor_npl1==0 ~ "Amhara, Non-Poor",
                                 region=="Amhara" & poor_npl1==1 ~ "Amhara, Poor",
                                 region=="Oromiya" & poor_npl1==0 ~ "Oromiya, Non-Poor",
                                 region=="Oromiya" & poor_npl1==1 ~ "Oromiya, Poor",
                                 region=="SNNP" & poor_npl1==0 ~ "SNNP, Non-Poor",
                                 region=="SNNP" & poor_npl1==1 ~ "SNNP, Poor",
                                 region=="Tigray" & poor_npl1==0 ~ "Tigray, Non-Poor",
                                 region=="Tigray" & poor_npl1==1 ~ "Tigray, Poor",
                                 region=="Other regions" & poor_npl1==0 ~ "Other regions, Non-Poor",
                                 region=="Other regions" & poor_npl1==1 ~ "Other regions, Poor"))
  pred_nur_20<-merge(pred_nur_20, regions, by="hh_id")
  pred_nur_20<-pred_nur_20%>%
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
                                urban=="Urban" & poor_npl1==1 ~ "Urban, Poor"),
           region_poor=case_when(region=="Amhara" & poor_npl1==0 ~ "Amhara, Non-Poor",
                                 region=="Amhara" & poor_npl1==1 ~ "Amhara, Poor",
                                 region=="Oromiya" & poor_npl1==0 ~ "Oromiya, Non-Poor",
                                 region=="Oromiya" & poor_npl1==1 ~ "Oromiya, Poor",
                                 region=="SNNP" & poor_npl1==0 ~ "SNNP, Non-Poor",
                                 region=="SNNP" & poor_npl1==1 ~ "SNNP, Poor",
                                 region=="Tigray" & poor_npl1==0 ~ "Tigray, Non-Poor",
                                 region=="Tigray" & poor_npl1==1 ~ "Tigray, Poor",
                                 region=="Other regions" & poor_npl1==0 ~ "Other regions, Non-Poor",
                                 region=="Other regions" & poor_npl1==1 ~ "Other regions, Poor"))
  
  
  pred_ur_10$features <- "UR (10Q)"
  pred_ur_20$features <- "UR (20Q)"
  pred_nur_10$features <- "No UR (10Q)"
  pred_nur_20$features <- "No UR (20Q)"
  
  pred_nur <- rbind(pred_ur_10, pred_ur_20, pred_nur_10, pred_nur_20)
  
  pred_nur<-pred_nur%>%
    mutate(feature_poor = case_when(features=="UR (10Q)" & poor_npl1==0 ~ "UR (10Q), Non-Poor",
                                    features=="UR (10Q)" & poor_npl1==1 ~ "UR (10Q), Poor",
                                    features=="UR (20Q)" & poor_npl1==0 ~ "UR (20Q), Non-Poor",
                                    features=="UR (20Q)" & poor_npl1==1 ~ "UR (20Q), Poor",
                                    features=="No UR (10Q)" & poor_npl1==0 ~ "No UR (10Q), Non-Poor",
                                    features=="No UR (10Q)" & poor_npl1==1 ~ "No UR (10Q), Poor",
                                    features=="No UR (20Q)" & poor_npl1==0 ~ "No UR (20Q), Non-Poor",
                                    features=="No UR (20Q)" & poor_npl1==1 ~ "No UR (20Q), Poor"))
  
  
  
# *****************************************************************************
#### 04_PLOTS ####
# *****************************************************************************
  
  #Import font. You may need to download the relevant fonts from:
  #https://fonts.google.com/specimen/Open+Sans?query=open+sans
  
  #font_import(paths = "C:/Users/Manuel Cardona Arias/Downloads/Open_Sans/",prompt = F) 
  
  #Color palette: Tableau 20
  palette<-paletteer::paletteer_d("ggthemes::Tableau_20")
  
  #----Poverty rate error----
      rate_error<-ggplot(mods, aes(x = zone, y = mean_error_model, fill= model)) +
        geom_col(position = "dodge", colour = "white") +
        geom_text(aes(label = error), vjust = 1.4, colour = "white", size = 3, position = position_dodge(0.9)) +
        #coord_flip()+
        scale_fill_manual(values=c(palette[1], palette[5], palette[15]))+
        scale_y_continuous(breaks = dampack::number_ticks(10)) +
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
        labs(title = "Poverty rate accuracy by type of PPI",
             subtitle = "National Poverty Line - Ethiopia 2015-16",
             x = " ",
             y = "Errors in poverty rate estimation (%)",
             caption = paste(" ")) +
          ggsave(paste0("06 Figures/02 Luminosity/", "01", "_", "poverty_rate_error", ".jpeg"), 
                 width = 10, height = 6)
        
      
  #----Target error (Full sample)----
  target<-ggplot(tods_full, aes(x=cutoff, y=error, shape=model, color=type)) + 
    geom_point(size=3) +
    geom_line(size=0.8) +
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
    labs(title = "Targeting accuracy by type of PPI",
         subtitle = "National Poverty Line - Ethiopia 2015-16 (Full sample)",
         x = "Score cutoff",
         y = " ",
         caption = paste(" ")) +
    ggsave(paste0("06 Figures/02 Luminosity/", "01", "_", "target_error_full", ".jpeg"), 
           width = 10, height = 6)
    
  #----Target error (Rural sample)----
  target<-ggplot(tods_rur, aes(x=cutoff, y=error, shape=model, color=type)) + 
    geom_point(size=3) +
    geom_line(size=0.8) +
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
    labs(title = "Targeting accuracy by type of PPI",
         subtitle = "National Poverty Line - Ethiopia 2015-16 (Rural sample)",
         x = "Score cutoff",
         y = " ",
         caption = paste(" ")) +
    ggsave(paste0("06 Figures/02 Luminosity/", "01", "_", "target_error_rural", ".jpeg"), 
           width = 10, height = 6)
  
    #----Target error (Urban sample)----
    target<-ggplot(tods_urb, aes(x=cutoff, y=error, shape=model, color=type)) + 
      geom_point(size=3) +
      geom_line(size=0.8) +
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
      labs(title = "Targeting accuracy by type of PPI",
           subtitle = "National Poverty Line - Ethiopia 2015-16 (Urban sample)",
           x = "Score cutoff",
           y = " ",
           caption = paste(" ")) +
      ggsave(paste0("06 Figures/02 Luminosity/", "01", "_", "target_error_urban", ".jpeg"), 
             width = 10, height = 6)
    
  #----Predicted probabilities Box Plot----
  pred_region <- pred0 %>%
    ggplot( aes(x=region_poor, y=pred_score_pr_full, fill=region_poor)) +
    geom_boxplot() +
    scale_fill_manual(values=c("#A0CBE8FF", "#8CD17DFF", "#A0CBE8FF", "#8CD17DFF", "#A0CBE8FF", "#8CD17DFF", "#A0CBE8FF", "#8CD17DFF", "#A0CBE8FF", "#8CD17DFF")) +
    scale_x_discrete(labels=c("     Amhara", " ", "     Oromiya", " ", "      SNNP", " ", "     Tigray", " ", "  Other regions", " ")) +
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
          legend.position = "none",
          legend.title = element_blank()) +
    labs(title = "Predicted probability of poverty across regions and poverty status",
         subtitle = "National Poverty Line - Ethiopia 2015-16 (Full sample)",
         x = " ",
         y = "Probability of Poverty",
         caption = paste(" ")) +
    ggsave(paste0("06 Figures/01 LSMS/", "01", "_", "pred_prob_region", ".jpeg"), 
           width = 10, height = 6)
  
  pred_urban <- pred0 %>%
    ggplot( aes(x=urban_poor, y=pred_score_pr_full, fill=urban_poor)) +
    geom_boxplot() +
    scale_fill_manual(values=c("#A0CBE8FF", "#8CD17DFF", "#A0CBE8FF", "#8CD17DFF")) +
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
          legend.position = "none",
          legend.title = element_blank()) +
    labs(title = "Predicted probability of poverty across regions and poverty status",
         subtitle = "National Poverty Line - Ethiopia 2015-16 (Full sample)",
         x = " ",
         y = "Probability of Poverty",
         caption = paste(" ")) +
    ggsave(paste0("06 Figures/01 LSMS/", "01", "_", "pred_prob_region", ".jpeg"), 
           width = 10, height = 6)
  
  #----Predicted probabilities Box Plot----
  pred_region_lum <- pred1 %>%
    ggplot( aes(x=region_poor, y=pred_score_pr_full, fill=region_poor)) +
    geom_boxplot() +
    scale_fill_manual(values=c("#A0CBE8FF", "#8CD17DFF", "#A0CBE8FF", "#8CD17DFF", "#A0CBE8FF", "#8CD17DFF", "#A0CBE8FF", "#8CD17DFF", "#A0CBE8FF", "#8CD17DFF")) +
    scale_x_discrete(labels=c("     Amhara", " ", "     Oromiya", " ", "      SNNP", " ", "     Tigray", " ", "  Other regions", " ")) +
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
          legend.position = "none",
          legend.title = element_blank()) +
    labs(title = "Predicted probability of poverty across regions and poverty status",
         subtitle = "National Poverty Line - Ethiopia 2015-16 (Full sample)",
         x = " ",
         y = "Probability of Poverty",
         caption = paste(" ")) +
    ggsave(paste0("06 Figures/02 Luminosity/", "01", "_", "pred_prob_region", ".jpeg"), 
           width = 10, height = 6)
  
  pred_urban_lum <- pred1 %>%
    ggplot( aes(x=urban_poor, y=pred_score_pr_full, fill=urban_poor)) +
    geom_boxplot() +
    scale_fill_manual(values=c("#A0CBE8FF", "#8CD17DFF", "#A0CBE8FF", "#8CD17DFF")) +
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
          legend.position = "none",
          legend.title = element_blank()) +
    labs(title = "Predicted probability of poverty across regions and poverty status",
         subtitle = "National Poverty Line - Ethiopia 2015-16 (Full sample)",
         x = " ",
         y = "Probability of Poverty",
         caption = paste(" ")) +
    ggsave(paste0("06 Figures/02 Luminosity/", "01", "_", "pred_prob_region", ".jpeg"), 
           width = 10, height = 6)
  
  
  #----Predicted probabilities Box Plot----
  pred_region_rwi <- pred2 %>%
    ggplot( aes(x=region_poor, y=pred_score_pr_full, fill=region_poor)) +
    geom_boxplot() +
    scale_fill_manual(values=c("#A0CBE8FF", "#8CD17DFF", "#A0CBE8FF", "#8CD17DFF", "#A0CBE8FF", "#8CD17DFF", "#A0CBE8FF", "#8CD17DFF", "#A0CBE8FF", "#8CD17DFF")) +
    scale_x_discrete(labels=c("     Amhara", " ", "     Oromiya", " ", "      SNNP", " ", "     Tigray", " ", "  Other regions", " ")) +
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
          legend.position = "none",
          legend.title = element_blank()) +
    labs(title = "Predicted probability of poverty across regions and poverty status",
         subtitle = "National Poverty Line - Ethiopia 2015-16 (Full sample)",
         x = " ",
         y = "Probability of Poverty",
         caption = paste(" ")) +
    ggsave(paste0("06 Figures/03 RWI/", "01", "_", "pred_prob_region", ".jpeg"), 
           width = 10, height = 6)
  
  pred_urban_rwi <- pred2 %>%
    ggplot( aes(x=urban_poor, y=pred_score_pr_full, fill=urban_poor)) +
    geom_boxplot() +
    scale_fill_manual(values=c("#A0CBE8FF", "#8CD17DFF", "#A0CBE8FF", "#8CD17DFF")) +
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
          legend.position = "none",
          legend.title = element_blank()) +
    labs(title = "Predicted probability of poverty across regions and poverty status",
         subtitle = "National Poverty Line - Ethiopia 2015-16 (Full sample)",
         x = " ",
         y = "Probability of Poverty",
         caption = paste(" ")) +
    ggsave(paste0("06 Figures/03 RWI/", "01", "_", "pred_prob_region", ".jpeg"), 
           width = 10, height = 6)
  
  #----Predicted probabilities Box Plot----
  pred_full <- pred %>%
    ggplot( aes(x=feature_poor, y=pred_score_pr_full, fill=feature_poor)) +
    geom_boxplot() +
    scale_fill_manual(values=c("#A0CBE8FF", "#8CD17DFF", "#A0CBE8FF", "#8CD17DFF", "#A0CBE8FF", "#8CD17DFF")) +
    scale_x_discrete(labels=c("     LSMS features", " ", "      LSMS + Radiance", " ", "       LSMS + RWI", " ")) +
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
          legend.position = "none",
          legend.title = element_blank()) +
    labs(title = "Predicted probability of poverty across regions and poverty status",
         subtitle = "National Poverty Line - Ethiopia 2015-16 (Full sample)",
         x = " ",
         y = "Probability of Poverty",
         caption = paste(" ")) +
    ggsave(paste0("06 Figures/03 RWI/", "01", "_", "pred_prob_region", ".jpeg"), 
           width = 10, height = 6)
  
  
  
  #----Predicted probabilities Box Plot----
  pred_full_nur <- pred_nur %>%
    ggplot( aes(x=feature_poor, y=pred_score_pr_full, fill=feature_poor)) +
    geom_boxplot() +
    scale_fill_manual(values=c("#A0CBE8FF", "#8CD17DFF", "#A0CBE8FF", "#8CD17DFF", "#A0CBE8FF", "#8CD17DFF", "#A0CBE8FF", "#8CD17DFF")) +
    scale_x_discrete(labels=c("              UR (10Q)", " ", "              UR (20Q)", " ", "               No UR (10Q)", " ", "               No UR (20Q)", " ")) +
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
          legend.position = "none",
          legend.title = element_blank()) +
    labs(title = "Predicted probability of poverty across regions and poverty status",
         subtitle = "National Poverty Line - Ethiopia 2015-16 (Full sample)",
         x = " ",
         y = "Probability of Poverty",
         caption = paste(" ")) +
    ggsave(paste0("06 Figures/", "02", "_", "pred_prob_byUR", ".jpeg"), 
           width = 10, height = 6)