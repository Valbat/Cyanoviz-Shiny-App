
library(shiny)
library(tidyverse)
library(dplyr)
library(ggplot2)

#############################################
      fluidPage(
        titlePanel("New England Cyano Monitoring 2015 Data"),
        sidebarLayout(
          sidebarPanel(
            helpText(strong("Pick a State or States to view the Chlorophyll or Phycocyanin 2015 Cyano 
                            Monitoring Data ")),
            
            selectInput("State",
                        label = "Select a State",
                        choices = unique(dat2015$state),
                        selected = "VT",
                        multiple = TRUE),
            
            selectInput("Data",
                        label = "Data Explorer",
                        choices = c("Chlorophyll"= "chla_ugl",
                                    "Phycocyanin"= "phyco_ugl"),
                        selected = "chla_ugl",
                        multiple = FALSE)
           
          ),
            
          mainPanel(
            plotOutput("Plot1")
            )
          ))
        






