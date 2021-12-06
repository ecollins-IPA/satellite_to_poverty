#****************************************************************# 
# Purpose: Create visualizations for the accuracy of different   #
#          versions of the PPI
#                                                                #
# Created: August 24, 2021                                       #
# Depends on:                                                    #
#   Author: Manuel Cardona                                       # 
#   e-mail: mcardona@poverty-action.org                          #
#                                                                #
#****************************************************************# 

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


# *****************************************************************************
#### 02_Load_data ####
# *****************************************************************************
  setwd("~/../Box Sync/IPA_Programs_PPI/07 PPI Development/ETHIOPIA/Ethiopia - Socioeconomic Survey 2015-2016/")
  
  #----No radiance scores----
  mod0<-read.csv("08 Results/LSMS/PovRate_Errors_10q_ALLpovlines.csv")
  tod0<-read.csv("08 Results/LSMS/Target_Errors_10q_ALLpovlines.csv")
  
  #----Regions and radiance unpenalized----
  mod1<-read.csv("08 Results/Luminosity/Regions and radiance unpenalized/PovRate_Errors_10q_ALLpovlines.csv")
  tod1<-read.csv("08 Results/Luminosity/Regions and radiance unpenalized/Target_Errors_10q_ALLpovlines.csv")
  
  #----Radiance penalized, but calibration unpenalized----
  mod2<-read.csv("08 Results/Luminosity/Regions unpenalized 2/PovRate_Errors_10q_ALLpovlines.csv")
  tod2<-read.csv("08 Results/Luminosity/Regions unpenalized 2/Target_Errors_10q_ALLpovlines.csv")
  
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
  
# *****************************************************************************
#### 04_PLOTS ####
# *****************************************************************************
  
  #Import font. You may need to download the relevant fonts from:
  #https://fonts.google.com/specimen/Open+Sans?query=open+sans
  
  font_import(paths = "C:/Users/Manuel Cardona Arias/Downloads/Open_Sans/",prompt = F) 
  
  #Color palette: Tableau 20
  palette<-paletteer_d("ggthemes::Tableau_20")
  
      #----Poverty rate error----
      rate_error<-ggplot(mods, aes(x = zone, y = mean_error_model, fill= model)) +
        geom_col(position = "dodge", colour = "white") +
        geom_text(aes(label = error), vjust = 1.4, colour = "white", size = 3, position = position_dodge(0.9)) +
        #coord_flip()+
        scale_fill_manual(values=c(palette[1], palette[5], palette[15]))+
        scale_y_continuous(breaks = number_ticks(10)) +
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
    
    