
library(tidyverse)
library(dplyr)
library(ggplot2)

dat2015 <- read_csv("may data_clean_2015.csv",guess_max = 2000) %>%
  filter(complete.cases(data.frame(chla_ugl,phyco_ugl))) %>%
  filter(chla_ugl > 0) %>% 
  filter(phyco_ugl > 0.1) %>%
  arrange(desc(chla_ugl), desc(phyco_ugl)) %>%
  filter(dilution == "1:1") %>%
  filter(analysis_rep == "Primary") %>%
  filter(fluorometer_type == "Beagle") %>%
  filter(sample_temp_c > 20 & sample_temp_c < 24)%>%
  filter(sample_method==("Intergrated Sampler"))
         
   #####################################################################
  ########################################################################
  
  #  extract the varilables by state
  
  phyco_state<- dat2015 %>%
    group_by (state,phyco_ugl,sample_date)
  
  ########################################################################## 
  summary(dat2015)
  ############################################################################
  chla_gg <- ggplot(data=dat2015, mapping = aes(x=sample_date,y=chla_ugl)) + 
    geom_point() +
    facet_wrap(~ state) +
    geom_smooth(method = "lm")
  chla_gg
  ###########################################################################
  
  Chla_VT<-filter(dat2015,state=="VT")%>%
    select(chla_ugl,sample_date)
  
  plotchla_VT <- ggplot(data = Chla_VT,
                        aes(x=sample_date,y=chla_ugl))+
    geom_point()
  plotchla_VT
  
  
  phyco_VT<-filter(dat2015,state=="VT")%>%
    select(phyco_ugl,sample_date)
  
  plotphyco_VT <- ggplot(data = phyco_VT,
                         aes(x=sample_date,y=phyco_ugl))+
    geom_point()
  
  plotphyco_VT 
  ################################################################################
  phyco_RI<-filter(dat2015,state=="RI")%>%
    select(phyco_ugl,sample_date)
  
  plotphyco_RI <- ggplot(data = phyco_RI,
                         aes(x=sample_date,y=phyco_ugl))+
    geom_point()+
    geom_smooth(method = "lm")
  
  plotphyco_RI 
  
  Chla_RI<-filter(dat2015,state=="RI")%>%
    select(chla_ugl,sample_date)
  
  plotchla_RI <- ggplot(data = Chla_RI,
                        aes(x=sample_date,y=chla_ugl))+
    geom_point()+
    geom_smooth(method = "lm")
  plotchla_RI
  ##################################################################################
  phyco_CT<-filter(dat2015,state=="CT")%>%
    select(phyco_ugl,sample_date)
  
  plotphyco_CT <- ggplot(data = phyco_CT,
                         aes(x=sample_date,y=phyco_ugl))+
    geom_point()+
    geom_smooth(method = "lm")
  
  plotphyco_CT 
  
  Chla_CT<-filter(dat2015,state=="CT")%>%
    select(chla_ugl,sample_date)
  
  plotchla_CT <- ggplot(data = Chla_RI,
                        aes(x=sample_date,y=chla_ugl))+
    geom_point()+
    geom_smooth(method = "lm")
  plotchla_CT
  ##################################################################################
  phyco_NH<-filter(dat2015,state=="NH")%>%
    select(phyco_ugl,sample_date)
  
  plotphyco_NH <- ggplot(data = phyco_NH,
                         aes(x=sample_date,y=phyco_ugl))+
    geom_point()+
    geom_smooth(method = "lm")
  
  plotphyco_NH 
  
  Chla_NH<-filter(dat2015,state=="NH")%>%
    select(chla_ugl,sample_date)
  
  plotchla_NH <- ggplot(data = Chla_NH,
                        aes(x=sample_date,y=chla_ugl))+
    geom_point()+
    geom_smooth(method = "lm")
  plotchla_NH
###################################################################
  phyco_MA<-filter(dat2015,state=="MA")%>%
    select(phyco_ugl,sample_date)
  
  plotphyco_MA <- ggplot(data = phyco_MA,
                         aes(x=sample_date,y=phyco_ugl))+
    geom_point()+
    geom_smooth(method = "lm")
  
  plotphyco_MA 
  
  Chla_MA<-filter(dat2015,state=="MA")%>%
    select(chla_ugl,sample_date)
  
  plotchla_MA <- ggplot(data = Chla_MA,
                        aes(x=sample_date,y=chla_ugl))+
    geom_point()+
    geom_smooth(method = "lm")
  plotchla_MA
  