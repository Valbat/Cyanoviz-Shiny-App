library(shiny)
library(tidyverse)
library(dplyr)
library(ggplot2)


dat2015 <- read_csv("may data_clean_2015.csv",guess_max = 2000) %>%
  filter(complete.cases(data.frame(chla_ugl,phyco_ugl))) %>%
  arrange(desc(chla_ugl), desc(phyco_ugl)) %>%
  filter(fluorometer_type == "Beagle") %>%
  filter(sample_method==("Intergrated Sampler"))

#################################
##checkbox list of QA Filters
fluidPage(
  titlePanel("2015 New England Cyano Monitoring Quality Control Filters"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      #Input:Select state
      selectInput("State",
                  label = "View a State",
                  choices = unique(dat2015$state),
                  selected = "VT",
                  multiple = TRUE),
      
   
      # Input: Select the QA filter ----
      
      checkboxGroupInput("dataset", label=h4("Range of QA Filters"),
                         choices=
                           list(" Samples taken within QA Detection Limit" = dat2015$chla_ugl>0& dat2015$phyco_ugl>0.1,
                                       "Samples taken with no Dilution" = dat2015$dilution=="1:1",
                                       "Analysis not repeated" =  dat2015$analysis_rep=="Primary",
                                       "Samples Analyzed within QAPP Temp Requirements" = dat2015$sample_temp_c== "> 20 & < 24"))
      ), 
              
           # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(
        id ="Data",
        type="tabs",
        tabPanel("Chlorophyll",value = "chla_ugl",plotOutput("plot1")),
        tabPanel("Phycocyanin",value = "phyco_ugl",plotOutput("plot2"))
      )
    )))
       
    